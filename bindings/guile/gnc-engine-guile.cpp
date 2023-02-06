/********************************************************************\
 * gnc-engine-guile.cpp -- engine helper functions for guile          *
 * Copyright (C) 2000 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2001 Linux Developers Group, Inc.                  *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include <config.h>

#include "swig-runtime.h"
#include <libguile.h>
#include <cstring>

#include "Account.h"
#include "engine-helpers.h"
#include "gnc-engine-guile.h"
#include "gnc-date.h"
#include "gnc-engine.h"
#include "gnc-session.h"
#include "guile-mappings.h"
#include "gnc-guile-utils.h"
#include <qof.h>
#include <qofbookslots.h>

#ifndef HAVE_STRPTIME
#    include "strptime.h"
#endif

/** \todo Code dependent on the private query headers
qofquery-p.h and qofquerycore-p.h may need to be modified.
These files are temporarily exported for QOF 0.6.0 but
cannot be considered "standard" or public parts of QOF. */
#include "qofquery-p.h"
#include "qofquerycore-p.h"

#define FUNC_NAME G_STRFUNC

static QofLogModule log_module = GNC_MOD_ENGINE;


GDate gnc_time64_to_GDate(SCM x)
{
    time64 time = scm_to_int64 (x);
    return time64_to_gdate(time);
}

SCM
gnc_guid2scm(GncGUID guid)
{
    char string[GUID_ENCODING_LENGTH + 1];

    if (!guid_to_string_buff(&guid, string))
        return SCM_BOOL_F;

    return scm_from_utf8_string(string);
}

GncGUID
gnc_scm2guid(SCM guid_scm)
{
    GncGUID guid;
    gchar * str;

    if (!scm_is_string(guid_scm)
            || (GUID_ENCODING_LENGTH != scm_c_string_length (guid_scm)))
    {
        return *guid_null();
    }
    str = gnc_scm_to_utf8_string (guid_scm);
    string_to_guid(str, &guid);
    g_free (str);
    return guid;
}

int
gnc_guid_p(SCM guid_scm)
{
    GncGUID guid;
    gchar * str;
    int return_int;

    if (!scm_is_string(guid_scm))
        return FALSE;

    if (GUID_ENCODING_LENGTH != scm_c_string_length (guid_scm))
    {
        return FALSE;
    }
    str = gnc_scm_to_utf8_string (guid_scm);
    return_int = string_to_guid(str, &guid);
    g_free (str);
    return return_int;
}


/********************************************************************
 * type converters for query API
 ********************************************************************/

/* The query scm representation is a list of pairs, where the
 * car of each pair is one of the following symbols:
 *
 *   Symbol                cdr
 *   'terms                list of OR terms
 *   'primary-sort         scm rep of sort_type_t
 *   'secondary-sort       scm rep of sort_type_t
 *   'tertiary-sort        scm rep of sort_type_t
 *   'primary-increasing   boolean
 *   'secondary-increasing boolean
 *   'tertiary-increasing  boolean
 *   'max-splits           integer
 *
 *   Each OR term is a list of AND terms.
 *   Each AND term is a list of one of the following forms:
 *
 *   ('pd-amount pr-type sense-bool amt-match-how amt-match-sign amount)
 *   ('pd-account pr-type sense-bool acct-match-how list-of-account-guids)
 *   ('pd-string pr-type sense-bool case-sense-bool use-regexp-bool string)
 *   ('pd-cleared pr-type sense-bool cleared-field)
 *   ('pd-balance pr-type sense-bool balance-field)
 */

typedef enum
{
    gnc_QUERY_v1 = 1,
    gnc_QUERY_v2
} query_version_t;

/* QofCompareFunc */

static QofQueryCompare
gnc_query_scm2compare (SCM how_scm)
{
    return static_cast<QofQueryCompare>(scm_to_int(how_scm));
}

/* QofStringMatch */
static QofStringMatch
gnc_query_scm2string (SCM how_scm)
{
    return static_cast<QofStringMatch>(scm_to_int(how_scm));
}

/* QofDateMatch */
static QofDateMatch
gnc_query_scm2date (SCM how_scm)
{
    return static_cast<QofDateMatch>(scm_to_int(how_scm));
}

/* QofNumericMatch */
static QofNumericMatch
gnc_query_scm2numericop (SCM how_scm)
{
    return static_cast<QofNumericMatch>(scm_to_int(how_scm));
}

/* QofGuidMatch */
static QofGuidMatch
gnc_query_scm2guid (SCM how_scm)
{
    return static_cast<QofGuidMatch>(scm_to_int(how_scm));
}

/* QofCharMatch */
static QofCharMatch
gnc_query_scm2char (SCM how_scm)
{
    return static_cast<QofCharMatch>(scm_to_int(how_scm));
}

static QofGuidMatch
gnc_scm2acct_match_how (SCM how_scm)
{
    QofGuidMatch res;
    gchar *how = gnc_scm_symbol_to_locale_string (how_scm);

    if (!g_strcmp0 (how, "acct-match-all"))
        res = QOF_GUID_MATCH_ALL;
    else if (!g_strcmp0 (how, "acct-match-any"))
        res = QOF_GUID_MATCH_ANY;
    else if (!g_strcmp0 (how, "acct-match-none"))
        res = QOF_GUID_MATCH_NONE;
    else
    {
        PINFO ("invalid account match: %s", how);
        res = QOF_GUID_MATCH_NULL;
    }

    g_free (how);
    return res;
}

static QofQueryCompare
gnc_scm2amt_match_how (SCM how_scm)
{
    QofQueryCompare res;
    gchar *how = gnc_scm_symbol_to_locale_string (how_scm);

    if (!g_strcmp0 (how, "amt-match-atleast"))
        res = QOF_COMPARE_GTE;
    else if (!g_strcmp0 (how, "amt-match-atmost"))
        res = QOF_COMPARE_LTE;
    else if (!g_strcmp0 (how, "amt-match-exactly"))
        res = QOF_COMPARE_EQUAL;
    else
    {
        PINFO ("invalid amount match: %s", how);
        res = QOF_COMPARE_EQUAL;
    }

    g_free (how);
    return res;
}

static int
gnc_scm2bitfield (SCM field_scm)
{
    int field = 0;

    if (!scm_is_list (field_scm))
        return 0;

    while (!scm_is_null (field_scm))
    {
        SCM scm;
        int bit;

        scm = SCM_CAR (field_scm);
        field_scm = SCM_CDR (field_scm);

        bit = scm_to_int(scm);
        field |= bit;
    }

    return field;
}

static cleared_match_t
gnc_scm2cleared_match_how (SCM how_scm)
{
    return static_cast<cleared_match_t>(gnc_scm2bitfield (how_scm));
}

static gboolean
gnc_scm2balance_match_how (SCM how_scm, gboolean *resp)
{
    gchar *how;

    if (!scm_is_list (how_scm))
        return FALSE;

    if (scm_is_null (how_scm))
        return FALSE;

    /* Only allow a single-entry list */
    if (!scm_is_null (SCM_CDR (how_scm)))
        return FALSE;

    how = gnc_scm_symbol_to_locale_string (SCM_CAR(how_scm));

    if (!g_strcmp0 (how, "balance-match-balanced"))
        *resp = TRUE;
    else
        *resp = FALSE;

    g_free (how);
    return TRUE;
}

static SCM
gnc_guid_glist2scm (const GList *account_guids)
{
    SCM guids = SCM_EOL;
    const GList *node;

    for (node = account_guids; node; node = node->next)
    {
        auto guid = static_cast<GncGUID*>(node->data);

        if (guid)
            guids = scm_cons (gnc_guid2scm (*guid), guids);
    }

    return scm_reverse (guids);
}

static GList *
gnc_scm2guid_glist (SCM guids_scm)
{
    GList *guids = nullptr;

    if (!scm_is_list (guids_scm))
        return nullptr;

    while (!scm_is_null (guids_scm))
    {
        SCM guid_scm = SCM_CAR (guids_scm);
        GncGUID *guid = nullptr;

        if (guid_scm != SCM_BOOL_F)
        {
            guid = guid_malloc ();
            *guid = gnc_scm2guid (guid_scm);
        }

        guids = g_list_prepend (guids, guid);

        guids_scm = SCM_CDR (guids_scm);
    }

    return g_list_reverse (guids);
}

static inline void
gnc_guid_glist_free (GList *guids)
{
    g_list_free_full (guids, (GDestroyNotify)guid_free);
}

static SCM
gnc_query_numeric2scm (gnc_numeric val)
{
    return scm_cons (scm_from_int64 (val.num),
                     scm_from_int64 (val.denom));
}

static gboolean
gnc_query_numeric_p (SCM pair)
{
    return (scm_is_pair (pair));
}

static gnc_numeric
gnc_query_scm2numeric (SCM pair)
{
    SCM denom;
    SCM num;

    num = SCM_CAR (pair);
    denom = SCM_CDR (pair);

    return gnc_numeric_create (scm_to_int64 (num),
                               scm_to_int64 (denom));
}

static SCM
gnc_query_path2scm (const GSList *path)
{
    SCM path_scm = SCM_EOL;
    const GSList *node;

    for (node = path; node; node = node->next)
    {
        auto key = static_cast<const char *>(node->data);

        if (key)
            path_scm = scm_cons (scm_from_utf8_string (key), path_scm);
    }

    return scm_reverse (path_scm);
}

GSList *
gnc_query_scm2path (SCM path_scm)
{
    GSList *path = nullptr;

    if (!scm_is_list (path_scm))
        return nullptr;

    while (!scm_is_null (path_scm))
    {
        SCM key_scm = SCM_CAR (path_scm);
        char *key;

        if (!scm_is_string (key_scm))
            break;

        key = gnc_scm_to_utf8_string(key_scm);
        path = g_slist_prepend (path, key);
        path_scm = SCM_CDR (path_scm);
    }

    return g_slist_reverse (path);
}

static void
gnc_query_path_free (GSList *path)
{
    GSList *node;

    for (node = path; node; node = node->next)
        g_free (node->data);

    g_slist_free (path);
}


static SCM
gnc_queryterm2scm (const QofQueryTerm *qt)
{
    SCM qt_scm = SCM_EOL;
    QofQueryPredData *pd = nullptr;

    qt_scm = scm_cons (gnc_query_path2scm (qof_query_term_get_param_path (qt)),
                       qt_scm);
    qt_scm = scm_cons (SCM_BOOL (qof_query_term_is_inverted (qt)), qt_scm);

    pd = qof_query_term_get_pred_data (qt);
    qt_scm = scm_cons (scm_from_locale_symbol (pd->type_name), qt_scm);
    qt_scm = scm_cons (scm_from_long  (pd->how), qt_scm);

    if (!g_strcmp0 (pd->type_name, QOF_TYPE_STRING))
    {
        auto pdata = (query_string_t) pd;

        qt_scm = scm_cons (scm_from_long  (pdata->options), qt_scm);
        qt_scm = scm_cons (SCM_BOOL (pdata->is_regex), qt_scm);
        qt_scm = scm_cons (pdata->matchstring ? scm_from_utf8_string (pdata->matchstring) : SCM_BOOL_F, qt_scm);

    }
    else if (!g_strcmp0 (pd->type_name, QOF_TYPE_DATE))
    {
        auto pdata = (query_date_t) pd;

        qt_scm = scm_cons (scm_from_long  (pdata->options), qt_scm);
        qt_scm = scm_cons (scm_from_int64 (pdata->date), qt_scm);

    }
    else if (!g_strcmp0 (pd->type_name, QOF_TYPE_NUMERIC))
    {
        auto pdata = (query_numeric_t) pd;

        qt_scm = scm_cons (scm_from_long  (pdata->options), qt_scm);
        qt_scm = scm_cons (gnc_query_numeric2scm (pdata->amount), qt_scm);

    }
    else if (!g_strcmp0 (pd->type_name, QOF_TYPE_GUID))
    {
        auto pdata = (query_guid_t) pd;

        qt_scm = scm_cons (scm_from_long  (pdata->options), qt_scm);
        qt_scm = scm_cons (gnc_guid_glist2scm (pdata->guids), qt_scm);

    }
    else if (!g_strcmp0 (pd->type_name, QOF_TYPE_INT64))
    {
        auto pdata = (query_int64_t) pd;

        qt_scm = scm_cons (scm_from_int64 (pdata->val), qt_scm);

    }
    else if (!g_strcmp0 (pd->type_name, QOF_TYPE_DOUBLE))
    {
        auto pdata = (query_double_t) pd;

        qt_scm = scm_cons (scm_from_double  (pdata->val), qt_scm);

    }
    else if (!g_strcmp0 (pd->type_name, QOF_TYPE_BOOLEAN))
    {
        auto pdata = (query_boolean_t) pd;

        qt_scm = scm_cons (SCM_BOOL (pdata->val), qt_scm);

    }
    else if (!g_strcmp0 (pd->type_name, QOF_TYPE_CHAR))
    {
        auto pdata = (query_char_t) pd;

        qt_scm = scm_cons (scm_from_long  (pdata->options), qt_scm);
        qt_scm = scm_cons (pdata->char_list ? scm_from_utf8_string (pdata->char_list) : SCM_BOOL_F, qt_scm);

    }
    else
    {
        PWARN ("query core type %s not supported", pd->type_name);
        return SCM_BOOL_F;
    }

    return scm_reverse (qt_scm);
}

static QofQuery *
gnc_scm2query_term_query_v2 (SCM qt_scm)
{
    QofQuery *q = nullptr;
    QofQueryPredData *pd = nullptr;
    SCM scm;
    gchar *type = nullptr;
    GSList *path = nullptr;
    gboolean inverted = FALSE;
    QofQueryCompare compare_how;

    if (!scm_is_list (qt_scm) || scm_is_null (qt_scm))
        return nullptr;

    do
    {
        /* param path */
        scm = SCM_CAR (qt_scm);
        qt_scm = SCM_CDR (qt_scm);
        if (!scm_is_list (scm))
            break;
        path = gnc_query_scm2path (scm);

        /* inverted */
        scm = SCM_CAR (qt_scm);
        qt_scm = SCM_CDR (qt_scm);
        if (!scm_is_bool (scm))
            break;
        inverted = scm_is_true (scm);

        /* type */
        scm = SCM_CAR (qt_scm);
        qt_scm = SCM_CDR (qt_scm);
        if (!scm_is_symbol (scm))
            break;
        type = gnc_scm_symbol_to_locale_string (scm);

        /* QofCompareFunc */
        scm = SCM_CAR (qt_scm);
        qt_scm = SCM_CDR (qt_scm);
        if (scm_is_null (scm))
            break;
        compare_how = gnc_query_scm2compare (scm);

        /* Now compute the predicate */

        if (!g_strcmp0 (type, QOF_TYPE_STRING))
        {
            QofStringMatch options;
            gboolean is_regex;
            gchar *matchstring;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm)) break;
            options = gnc_query_scm2string (scm);

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!scm_is_bool (scm)) break;
            is_regex = scm_is_true (scm);

            scm = SCM_CAR (qt_scm);
            if (!scm_is_string (scm)) break;

            matchstring = gnc_scm_to_utf8_string (scm);

            pd = qof_query_string_predicate (compare_how, matchstring,
                                             options, is_regex);
            g_free (matchstring);
        }
        else if (!g_strcmp0 (type, QOF_TYPE_DATE))
        {
            QofDateMatch options;
            time64 date;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            options = gnc_query_scm2date (scm);

            scm = SCM_CAR (qt_scm);
            if (scm_is_null (scm))
                break;
            date = scm_to_int64 (scm);

            pd = qof_query_date_predicate (compare_how, options, date);

        }
        else if (!g_strcmp0 (type, QOF_TYPE_NUMERIC))
        {
            QofNumericMatch options;
            gnc_numeric val;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            options = gnc_query_scm2numericop (scm);

            scm = SCM_CAR (qt_scm);
            if (!gnc_query_numeric_p (scm))
                break;
            val = gnc_query_scm2numeric (scm);

            pd = qof_query_numeric_predicate (compare_how, options, val);

        }
        else if (!g_strcmp0 (type, QOF_TYPE_GUID))
        {
            QofGuidMatch options;
            GList *guids;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            options = gnc_query_scm2guid (scm);

            scm = SCM_CAR (qt_scm);
            if (!scm_is_list (scm))
                break;
            guids = gnc_scm2guid_glist (scm);

            pd = qof_query_guid_predicate (options, guids);

            gnc_guid_glist_free (guids);

        }
        else if (!g_strcmp0 (type, QOF_TYPE_INT64))
        {
            gint64 val;

            scm = SCM_CAR (qt_scm);
            if (scm_is_null (scm))
                break;
            val = scm_to_int64 (scm);

            pd = qof_query_int64_predicate (compare_how, val);

        }
        else if (!g_strcmp0 (type, QOF_TYPE_DOUBLE))
        {
            double val;

            scm = SCM_CAR (qt_scm);
            if (!scm_is_number (scm))
                break;
            val = scm_to_double (scm);

            pd = qof_query_double_predicate (compare_how, val);

        }
        else if (!g_strcmp0 (type, QOF_TYPE_BOOLEAN))
        {
            gboolean val;

            scm = SCM_CAR (qt_scm);
            if (!scm_is_bool (scm))
                break;
            val = scm_is_true (scm);

            pd = qof_query_boolean_predicate (compare_how, val);

        }
        else if (!g_strcmp0 (type, QOF_TYPE_CHAR))
        {
            QofCharMatch options;
            gchar *char_list;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            options = gnc_query_scm2char (scm);

            scm = SCM_CAR (qt_scm);
            if (!scm_is_string (scm))
                break;
            char_list = gnc_scm_to_utf8_string (scm);

            pd = qof_query_char_predicate (options, char_list);
            g_free (char_list);
        }
        else
        {
            PWARN ("query core type %s not supported", type);
            break;
        }

        g_free (type);

    }
    while (FALSE);

    if (pd)
    {
        q = qof_query_create ();
        qof_query_add_term (q, path, pd, QOF_QUERY_OR);
        if (inverted)
        {
            QofQuery *outq = qof_query_invert (q);
            qof_query_destroy (q);
            q = outq;
        }
    }
    else
    {
        gnc_query_path_free (path);
    }

    return q;
}

static QofQuery *
gnc_scm2query_term_query_v1 (SCM query_term_scm)
{
    gboolean ok = FALSE;
    gchar * pd_type = nullptr;
    gchar * pr_type = nullptr;
    gboolean sense = FALSE;
    QofQuery *q = nullptr;
    SCM scm;

    if (!scm_is_list (query_term_scm) ||
            scm_is_null (query_term_scm))
    {
        PINFO ("null term");
        return nullptr;
    }

    do
    {
        /* pd_type */
        scm = SCM_CAR (query_term_scm);
        query_term_scm = SCM_CDR (query_term_scm);
        pd_type = gnc_scm_symbol_to_locale_string (scm);

        /* pr_type */
        if (scm_is_null (query_term_scm))
        {
            PINFO ("null pr_type");
            break;
        }
        scm = SCM_CAR (query_term_scm);
        query_term_scm = SCM_CDR (query_term_scm);
        pr_type = gnc_scm_symbol_to_locale_string (scm);

        /* sense */
        if (scm_is_null (query_term_scm))
        {
            PINFO ("null sense");
            break;
        }
        scm = SCM_CAR (query_term_scm);
        query_term_scm = SCM_CDR (query_term_scm);
        sense = scm_is_true (scm);

        q = qof_query_create_for(GNC_ID_SPLIT);

        if (!g_strcmp0 (pd_type, "pd-date"))
        {
            gboolean use_start;
            gboolean use_end;
            time64 start;
            time64 end;

            /* use_start */
            if (scm_is_null (query_term_scm))
            {
                PINFO ("null use_start");
                break;
            }

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            use_start = scm_is_true (scm);

            /* start */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            start = scm_to_int64 (scm);

            /* use_end */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            use_end = scm_is_true (scm);

            /* end */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            end = scm_to_int64 (scm);

            xaccQueryAddDateMatchTT (q, use_start, start, use_end, end, QOF_QUERY_OR);

            ok = TRUE;

        }
        else if (!g_strcmp0 (pd_type, "pd-amount"))
        {
            QofQueryCompare how;
            QofNumericMatch amt_sgn;
            gnc_numeric val;

            /* how */
            if (scm_is_null (query_term_scm))
                break;
            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            how = gnc_scm2amt_match_how (scm);

            /* amt_sgn */
            if (scm_is_null (query_term_scm))
                break;
            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            amt_sgn = gnc_query_scm2numericop (scm);

            /* amount */
            if (scm_is_null (query_term_scm))
                break;
            scm = SCM_CAR (query_term_scm);
            val = gnc_numeric_create (scm_to_int64(scm_numerator(scm)),
                                      scm_to_int64(scm_denominator(scm)));

            if (!g_strcmp0 (pr_type, "pr-price"))
            {
                xaccQueryAddSharePriceMatch (q, val, how, QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!g_strcmp0 (pr_type, "pr-shares"))
            {
                xaccQueryAddSharesMatch (q, val, how, QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!g_strcmp0 (pr_type, "pr-value"))
            {
                xaccQueryAddValueMatch (q, val, amt_sgn, how, QOF_QUERY_OR);
                ok = TRUE;

            }
            else
            {
                PINFO ("unknown amount predicate: %s", pr_type);
            }

        }
        else if (!g_strcmp0 (pd_type, "pd-account"))
        {
            QofGuidMatch how;
            GList *account_guids;

            /* how */
            if (scm_is_null (query_term_scm))
            {
                PINFO ("pd-account: null how");
                break;
            }

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            how = gnc_scm2acct_match_how (scm);

            /* account guids */
            if (scm_is_null (query_term_scm))
            {
                PINFO ("pd-account: null guids");
                break;
            }

            scm = SCM_CAR (query_term_scm);

            account_guids = gnc_scm2guid_glist (scm);

            xaccQueryAddAccountGUIDMatch (q, account_guids, how, QOF_QUERY_OR);

            gnc_guid_glist_free (account_guids);

            ok = TRUE;

        }
        else if (!g_strcmp0 (pd_type, "pd-string"))
        {
            gboolean case_sens;
            gboolean use_regexp;
            gchar *matchstring;

            /* case_sens */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            case_sens = scm_is_true (scm);

            /* use_regexp */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            use_regexp = scm_is_true (scm);

            /* matchstring */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            matchstring = gnc_scm_to_utf8_string (scm);

            if (!g_strcmp0 (pr_type, "pr-action"))
            {
                xaccQueryAddActionMatch (q, matchstring, case_sens, use_regexp,
                                         QOF_COMPARE_CONTAINS, QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!g_strcmp0 (pr_type, "pr-desc"))
            {
                xaccQueryAddDescriptionMatch (q, matchstring, case_sens,
                                              use_regexp, QOF_COMPARE_CONTAINS, QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!g_strcmp0 (pr_type, "pr-memo"))
            {
                xaccQueryAddMemoMatch (q, matchstring, case_sens, use_regexp,
                                       QOF_COMPARE_CONTAINS, QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!g_strcmp0 (pr_type, "pr-num"))
            {
                xaccQueryAddNumberMatch (q, matchstring, case_sens, use_regexp,
                                         QOF_COMPARE_CONTAINS, QOF_QUERY_OR);
                ok = TRUE;

            }
            else
            {
                PINFO ("Unknown string predicate: %s", pr_type);
            }
            g_free (matchstring);

        }
        else if (!g_strcmp0 (pd_type, "pd-cleared"))
        {
            cleared_match_t how;

            /* how */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            how = gnc_scm2cleared_match_how (scm);

            xaccQueryAddClearedMatch (q, how, QOF_QUERY_OR);
            ok = TRUE;

        }
        else if (!g_strcmp0 (pd_type, "pd-balance"))
        {
            gboolean how;

            /* how */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            if (gnc_scm2balance_match_how (scm, &how) == FALSE)
                break;

            xaccQueryAddBalanceMatch (q, static_cast<QofQueryCompare>(how), QOF_QUERY_OR);
            ok = TRUE;

        }
        else if (!g_strcmp0 (pd_type, "pd-guid"))
        {
            GncGUID guid;
            QofIdType id_type;

            /* guid */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            guid = gnc_scm2guid (scm);

            /* id type */
            scm = SCM_CAR (query_term_scm);
            id_type = (QofIdType) gnc_scm_to_utf8_string (scm);

            xaccQueryAddGUIDMatch (q, &guid, id_type, QOF_QUERY_OR);
            g_free ((void *) id_type);
            ok = TRUE;

        }
        else
        {
            PINFO ("Unknown Predicate: %s", pd_type);
        }

        g_free (pd_type);
        g_free (pr_type);

    }
    while (FALSE);

    if (ok)
    {
        QofQuery *out_q;

        if (sense)
            out_q = q;
        else
        {
            out_q = qof_query_invert (q);
            qof_query_destroy (q);
        }

        return out_q;
    }

    qof_query_destroy (q);
    return nullptr;
}

static QofQuery *
gnc_scm2query_term_query (SCM query_term_scm, query_version_t vers)
{
    switch (vers)
    {
    case gnc_QUERY_v1:
        return gnc_scm2query_term_query_v1 (query_term_scm);
    case gnc_QUERY_v2:
        return gnc_scm2query_term_query_v2 (query_term_scm);
    default:
        return nullptr;
    }
}

static SCM
gnc_query_terms2scm (const GList *terms)
{
    SCM or_terms = SCM_EOL;
    const GList *or_node;

    for (or_node = terms; or_node; or_node = or_node->next)
    {
        SCM and_terms = SCM_EOL;
        GList *and_node;

        for (and_node = static_cast<GList*>(or_node->data); and_node; and_node = and_node->next)
        {
            auto qt = static_cast<QofQueryTerm*>(and_node->data);
            SCM qt_scm;

            qt_scm = gnc_queryterm2scm (qt);

            and_terms = scm_cons (qt_scm, and_terms);
        }

        and_terms = scm_reverse (and_terms);

        or_terms = scm_cons (and_terms, or_terms);
    }

    return scm_reverse (or_terms);
}

static QofQuery *
gnc_scm2query_and_terms (SCM and_terms, query_version_t vers)
{
    QofQuery *q = nullptr;

    if (!scm_is_list (and_terms))
        return nullptr;

    while (!scm_is_null (and_terms))
    {
        SCM term;

        term = SCM_CAR (and_terms);
        and_terms = SCM_CDR (and_terms);

        if (!q)
            q = gnc_scm2query_term_query (term, vers);
        else
        {
            QofQuery *q_and;
            QofQuery *q_new;

            q_and = gnc_scm2query_term_query (term, vers);

            if (q_and)
            {
                q_new = qof_query_merge (q, q_and, QOF_QUERY_AND);
                qof_query_destroy (q_and);

                if (q_new)
                {
                    qof_query_destroy (q);
                    q = q_new;
                }
            }
        }
    }

    return q;
}

static QofQuery *
gnc_scm2query_or_terms (SCM or_terms, query_version_t vers)
{
    QofQuery *q = nullptr;

    if (!scm_is_list (or_terms))
        return nullptr;

    q = qof_query_create_for(GNC_ID_SPLIT);

    while (!scm_is_null (or_terms))
    {
        SCM and_terms;

        and_terms = SCM_CAR (or_terms);
        or_terms = SCM_CDR (or_terms);

        if (!q)
            q = gnc_scm2query_and_terms (and_terms, vers);
        else
        {
            QofQuery *q_or;
            QofQuery *q_new;

            q_or = gnc_scm2query_and_terms (and_terms, vers);

            if (q_or)
            {
                q_new = qof_query_merge (q, q_or, QOF_QUERY_OR);
                qof_query_destroy (q_or);

                if (q_new)
                {
                    qof_query_destroy (q);
                    q = q_new;
                }
            }
        }
    }

    return q;
}

static SCM
gnc_query_sort2scm (const QofQuerySort *qs)
{
    SCM sort_scm = SCM_EOL;
    GSList *path;

    path = qof_query_sort_get_param_path (qs);
    if (path == nullptr)
        return SCM_BOOL_F;

    sort_scm = scm_cons (gnc_query_path2scm (path), sort_scm);
    sort_scm = scm_cons (scm_from_int  (qof_query_sort_get_sort_options (qs)), sort_scm);
    sort_scm = scm_cons (SCM_BOOL (qof_query_sort_get_increasing (qs)), sort_scm);

    return scm_reverse (sort_scm);
}

static gboolean
gnc_query_scm2sort (SCM sort_scm, GSList **path, gint *options, gboolean *inc)
{
    SCM val;
    GSList *p;
    gint o;
    gboolean i;

    g_return_val_if_fail (path && options && inc, FALSE);
    g_return_val_if_fail (*path == nullptr, FALSE);

    /* This is ok -- it means we have an empty sort.  Don't do anything */
    if (scm_is_bool (sort_scm))
        return TRUE;

    /* Ok, this had better be a list */
    if (!scm_is_list (sort_scm))
        return FALSE;

    /* Parse the path, options, and increasing */
    val = SCM_CAR (sort_scm);
    sort_scm = SCM_CDR (sort_scm);
    if (!scm_is_list (val))
        return FALSE;
    p = gnc_query_scm2path (val);

    /* options */
    val = SCM_CAR (sort_scm);
    sort_scm = SCM_CDR (sort_scm);
    if (!scm_is_number (val))
    {
        gnc_query_path_free (p);
        return FALSE;
    }
    o = scm_to_int (val);

    /* increasing */
    val = SCM_CAR (sort_scm);
    sort_scm = SCM_CDR (sort_scm);
    if (!scm_is_bool (val))
    {
        gnc_query_path_free (p);
        return FALSE;
    }
    i = scm_is_true (val);

    /* EOL */
    if (!scm_is_null (sort_scm))
    {
        gnc_query_path_free (p);
        return FALSE;
    }
    *path = p;
    *options = o;
    *inc = i;

    return TRUE;
}

SCM
gnc_query2scm (QofQuery *q)
{
    SCM query_scm = SCM_EOL;
    SCM pair;
    QofQuerySort *s1, *s2, *s3;

    if (!q) return SCM_BOOL_F;

    /* terms */
    pair = scm_cons (gnc_query_terms2scm (qof_query_get_terms (q)), SCM_EOL);
    pair = scm_cons (scm_from_locale_symbol ("terms"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* search-for */
    pair = scm_cons (scm_from_locale_symbol (qof_query_get_search_for (q)), SCM_EOL);
    pair = scm_cons (scm_from_locale_symbol ("search-for"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* sorts... */
    qof_query_get_sorts (q, &s1, &s2, &s3);

    /* primary-sort */
    pair = scm_cons (gnc_query_sort2scm (s1), SCM_EOL);
    pair = scm_cons (scm_from_locale_symbol ("primary-sort"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* secondary-sort */
    pair = scm_cons (gnc_query_sort2scm (s2), SCM_EOL);
    pair = scm_cons (scm_from_locale_symbol ("secondary-sort"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* tertiary-sort */
    pair = scm_cons (gnc_query_sort2scm (s3), SCM_EOL);
    pair = scm_cons (scm_from_locale_symbol ("tertiary-sort"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* max results */
    pair = scm_cons (scm_from_int  (qof_query_get_max_results (q)), SCM_EOL);
    pair = scm_cons (scm_from_locale_symbol ("max-results"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* Reverse this list; tag it as 'query-v2' */
    pair = scm_reverse (query_scm);
    return scm_cons (scm_from_locale_symbol ("query-v2"), pair);
}

static GSList *
gnc_query_sort_to_list (const gchar * symbol)
{
    GSList *path = nullptr;

    if (!symbol)
        return nullptr;

    if (!g_strcmp0 (symbol, "by-none"))
    {
        path = nullptr;
    }
    else if (!g_strcmp0 (symbol, "by-standard"))
    {
        path = g_slist_prepend (path, (gpointer) QUERY_DEFAULT_SORT);

    }
    else if (!g_strcmp0 (symbol, "by-date") ||
             !g_strcmp0 (symbol, "by-date-rounded"))
    {
        path = g_slist_prepend (path, (gpointer) TRANS_DATE_POSTED);
        path = g_slist_prepend (path, (gpointer) SPLIT_TRANS);

    }
    else if (!g_strcmp0 (symbol, "by-date-entered") ||
             !g_strcmp0 (symbol, "by-date-entered-rounded"))
    {
        path = g_slist_prepend (path, (gpointer) TRANS_DATE_ENTERED);
        path = g_slist_prepend (path, (gpointer) SPLIT_TRANS);

    }
    else if (!g_strcmp0 (symbol, "by-date-reconciled") ||
             !g_strcmp0 (symbol, "by-date-reconciled-rounded"))
    {
        path = g_slist_prepend (path, (gpointer) SPLIT_DATE_RECONCILED);

    }
    else if (!g_strcmp0 (symbol, "by-num"))
    {
        path = g_slist_prepend (path, (gpointer) TRANS_NUM);
        path = g_slist_prepend (path, (gpointer) SPLIT_TRANS);

    }
    else if (!g_strcmp0 (symbol, "by-amount"))
    {
        path = g_slist_prepend (path, (gpointer) SPLIT_VALUE);

    }
    else if (!g_strcmp0 (symbol, "by-memo"))
    {
        path = g_slist_prepend (path, (gpointer) SPLIT_MEMO);

    }
    else if (!g_strcmp0 (symbol, "by-desc"))
    {
        path = g_slist_prepend (path, (gpointer) TRANS_DESCRIPTION);
        path = g_slist_prepend (path, (gpointer) SPLIT_TRANS);

    }
    else if (!g_strcmp0 (symbol, "by-reconcile"))
    {
        path = g_slist_prepend (path, (gpointer) SPLIT_RECONCILE);

    }
    else if (!g_strcmp0 (symbol, "by-account-full-name"))
    {
        path = g_slist_prepend (path, (gpointer) SPLIT_ACCT_FULLNAME);

    }
    else if (!g_strcmp0 (symbol, "by-account-code"))
    {
        path = g_slist_prepend (path, (gpointer) ACCOUNT_CODE_);
        path = g_slist_prepend (path, (gpointer) SPLIT_ACCOUNT);

    }
    else if (!g_strcmp0 (symbol, "by-corr-account-full-name"))
    {
        path = g_slist_prepend (path, (gpointer) SPLIT_CORR_ACCT_NAME);

    }
    else if (!g_strcmp0 (symbol, "by-corr-account-code"))
    {
        path = g_slist_prepend (path, (gpointer) SPLIT_CORR_ACCT_CODE);

    }
    else
    {
        PERR ("Unknown sort-type, %s", symbol);
    }

    return path;
}

static QofQuery *
gnc_scm2query_v1 (SCM query_scm)
{
    QofQuery *q = nullptr;
    gboolean ok = TRUE;
    gchar * primary_sort = nullptr;
    gchar * secondary_sort = nullptr;
    gchar * tertiary_sort = nullptr;
    gboolean primary_increasing = TRUE;
    gboolean secondary_increasing = TRUE;
    gboolean tertiary_increasing = TRUE;
    int max_splits = -1;

    while (!scm_is_null (query_scm))
    {
        gchar *symbol;
        SCM sym_scm;
        SCM value;
        SCM pair;

        pair = SCM_CAR (query_scm);
        query_scm = SCM_CDR (query_scm);

        if (!scm_is_pair (pair))
        {
            PERR ("Not a Pair");
            ok = FALSE;
            break;
        }

        sym_scm = SCM_CAR (pair);
        value = SCM_CADR (pair);

        if (!scm_is_symbol (sym_scm))
        {
            PERR ("Not a symbol");
            ok = FALSE;
            break;
        }

        symbol = gnc_scm_symbol_to_locale_string (sym_scm);
        if (!symbol)
        {
            PERR ("No string found");
            ok = FALSE;
            break;
        }

        if (g_strcmp0 ("terms", symbol) == 0)
        {
            if (q)
                qof_query_destroy (q);

            q = gnc_scm2query_or_terms (value, gnc_QUERY_v1);
            if (!q)
            {
                PINFO ("invalid terms");
                ok = FALSE;
                break;
            }

        }
        else if (g_strcmp0 ("primary-sort", symbol) == 0)
        {
            if (!scm_is_symbol (value))
            {
                PINFO ("Invalid primary sort");
                ok = FALSE;
                break;
            }

            primary_sort = gnc_scm_symbol_to_locale_string (value);

        }
        else if (g_strcmp0 ("secondary-sort", symbol) == 0)
        {
            if (!scm_is_symbol (value))
            {
                PINFO ("Invalid secondary sort");
                ok = FALSE;
                break;
            }

            secondary_sort = gnc_scm_symbol_to_locale_string (value);

        }
        else if (g_strcmp0 ("tertiary-sort", symbol) == 0)
        {
            if (!scm_is_symbol (value))
            {
                PINFO ("Invalid tertiary sort");
                ok = FALSE;
                break;
            }

            tertiary_sort = gnc_scm_symbol_to_locale_string (value);

        }
        else if (g_strcmp0 ("primary-increasing", symbol) == 0)
        {
            primary_increasing = scm_is_true (value);

        }
        else if (g_strcmp0 ("secondary-increasing", symbol) == 0)
        {
            secondary_increasing = scm_is_true (value);

        }
        else if (g_strcmp0 ("tertiary-increasing", symbol) == 0)
        {
            tertiary_increasing = scm_is_true (value);

        }
        else if (g_strcmp0 ("max-splits", symbol) == 0)
        {
            if (!scm_is_number (value))
            {
                PERR ("invalid max-splits");
                ok = FALSE;
                break;
            }

            max_splits = scm_to_int (value);

        }
        else
        {
            PERR ("Unknown symbol: %s", symbol);
            ok = FALSE;
            break;
        }

        g_free (symbol);
    }

    if (ok)
    {
        GSList *s1, *s2, *s3;
        s1 = gnc_query_sort_to_list (primary_sort);
        s2 = gnc_query_sort_to_list (secondary_sort);
        s3 = gnc_query_sort_to_list (tertiary_sort);

        qof_query_set_sort_order (q, s1, s2, s3);
        qof_query_set_sort_increasing (q, primary_increasing, secondary_increasing,
                                       tertiary_increasing);
        qof_query_set_max_results (q, max_splits);
    }
    else
    {
        qof_query_destroy (q);
        q = nullptr;
    }

    g_free (primary_sort);
    g_free (secondary_sort);
    g_free (tertiary_sort);

    return q;
}

static QofQuery *
gnc_scm2query_v2 (SCM query_scm)
{
    QofQuery *q = nullptr;
    gboolean ok = TRUE;
    gchar * search_for = nullptr;
    GSList *sp1 = nullptr, *sp2 = nullptr, *sp3 = nullptr;
    gint so1 = 0, so2 = 0, so3 = 0;
    gboolean si1 = TRUE, si2 = TRUE, si3 = TRUE;
    int max_results = -1;

    while (!scm_is_null (query_scm))
    {
        gchar *symbol;
        SCM sym_scm;
        SCM value;
        SCM pair;

        pair = SCM_CAR (query_scm);
        query_scm = SCM_CDR (query_scm);

        if (!scm_is_pair (pair))
        {
            ok = FALSE;
            break;
        }

        sym_scm = SCM_CAR (pair);
        value = SCM_CADR (pair);

        if (!scm_is_symbol (sym_scm))
        {
            ok = FALSE;
            break;
        }

        symbol = gnc_scm_symbol_to_locale_string (sym_scm);
        if (!symbol)
        {
            ok = FALSE;
            break;
        }

        if (!g_strcmp0 ("terms", symbol))
        {
            if (q)
                qof_query_destroy (q);

            q = gnc_scm2query_or_terms (value, gnc_QUERY_v2);
            if (!q)
            {
                ok = FALSE;
                break;
            }

        }
        else if (!g_strcmp0 ("search-for", symbol))
        {
            if (!scm_is_symbol (value))
            {
                ok = FALSE;
                break;
            }
            search_for = gnc_scm_symbol_to_locale_string (value);

        }
        else if (g_strcmp0 ("primary-sort", symbol) == 0)
        {
            if (! gnc_query_scm2sort (value, &sp1, &so1, &si1))
            {
                ok = FALSE;
                break;
            }

        }
        else if (!g_strcmp0 ("secondary-sort", symbol))
        {
            if (! gnc_query_scm2sort (value, &sp2, &so2, &si2))
            {
                ok = FALSE;
                break;
            }

        }
        else if (!g_strcmp0 ("tertiary-sort", symbol))
        {
            if (! gnc_query_scm2sort (value, &sp3, &so3, &si3))
            {
                ok = FALSE;
                break;
            }

        }
        else if (!g_strcmp0 ("max-results", symbol))
        {
            if (!scm_is_number (value))
            {
                ok = FALSE;
                break;
            }

            max_results = scm_to_int (value);

        }
        else
        {
            ok = FALSE;
            break;
        }

        g_free (symbol);
    }

    if (ok && search_for)
    {
        qof_query_search_for (q, search_for);
        qof_query_set_sort_order (q, sp1, sp2, sp3);
        qof_query_set_sort_options (q, so1, so2, so3);
        qof_query_set_sort_increasing (q, si1, si2, si3);
        qof_query_set_max_results (q, max_results);
    }
    else
    {
        qof_query_destroy (q);
        q = nullptr;
    }

    g_free (search_for);

    return q;
}

QofQuery *
gnc_scm2query (SCM query_scm)
{
    SCM q_type;
    gchar *type;
    QofQuery *q = nullptr;

    /* Not a list or nullptr?  No need to go further */
    if (!scm_is_list (query_scm) || scm_is_null (query_scm))
        return nullptr;

    /* Grab the 'type' (for v2 and above) */
    q_type = SCM_CAR (query_scm);

    if (!scm_is_symbol (q_type))
    {
        if (scm_is_pair (q_type))
        {
            /* Version-1 queries are just a list */
            return gnc_scm2query_v1 (query_scm);
        }
        else
        {
            return nullptr;
        }
    }

    /* Ok, the LHS is the version and the RHS is the actual query list */
    type = gnc_scm_symbol_to_locale_string (q_type);
    if (!type)
        return nullptr;

    if (!g_strcmp0 (type, "query-v2"))
        q = gnc_scm2query_v2 (SCM_CDR (query_scm));

    g_free (type);
    return q;
}

gnc_numeric
gnc_scm_to_numeric(SCM gncnum)
{
    SCM num, denom;

    /* Not a number. */
    if (!scm_is_number (gncnum))
        return gnc_numeric_error (GNC_ERROR_ARG);

    num = scm_numerator (gncnum);
    denom = scm_denominator (gncnum);

    /* scm overflows 64-bit numbers */
    if (!scm_is_signed_integer (num, INT64_MIN, INT64_MAX) ||
        !scm_is_signed_integer (denom, INT64_MIN, INT64_MAX))
        return gnc_numeric_error (GNC_ERROR_OVERFLOW);

    return gnc_numeric_create (scm_to_int64 (num), scm_to_int64 (denom));
}

SCM
gnc_numeric_to_scm(gnc_numeric arg)
{
    return gnc_numeric_check (arg) ? SCM_BOOL_F :
        scm_divide (scm_from_int64 (arg.num), scm_from_int64 (arg.denom));
}

static SCM
gnc_generic_to_scm(const void *cx, const gchar *type_str)
{
    swig_type_info * stype = nullptr;
    void *x = (void*) cx;

    if (!x) return SCM_BOOL_F;
    stype = SWIG_TypeQuery(type_str);

    if (!stype)
    {
        PERR("Unknown SWIG Type: %s ", type_str);
        return SCM_BOOL_F;
    }

    return SWIG_NewPointerObj(x, stype, 0);
}

static void *
gnc_scm_to_generic(SCM scm, const gchar *type_str)
{
    swig_type_info * stype = nullptr;

    stype = SWIG_TypeQuery(type_str);
    if (!stype)
    {
        PERR("Unknown SWIG Type: %s ", type_str);
        return nullptr;
    }

    if (!SWIG_IsPointerOfType(scm, stype))
        return nullptr;

    return SWIG_MustGetPtr(scm, stype, 1, 0);
}

gnc_commodity *
gnc_scm_to_commodity(SCM scm)
{
    return static_cast<gnc_commodity*>(gnc_scm_to_generic(scm, "_p_gnc_commodity"));
}

SCM
gnc_commodity_to_scm (const gnc_commodity *commodity)
{
    return gnc_generic_to_scm(commodity, "_p_gnc_commodity");
}

SCM
gnc_book_to_scm (const QofBook *book)
{
    return gnc_generic_to_scm(book, "_p_QofBook");
}

static swig_type_info *
get_acct_type ()
{
    static swig_type_info * account_type = nullptr;

    if (!account_type)
        account_type = SWIG_TypeQuery("_p_Account");

    return account_type;
}

GncAccountValue * gnc_scm_to_account_value_ptr (SCM valuearg)
{
    GncAccountValue *res;
    Account *acc = nullptr;
    gnc_numeric value;
    swig_type_info * account_type = get_acct_type();
    SCM val;

    /* Get the account */
    val = SCM_CAR (valuearg);
    if (!SWIG_IsPointerOfType (val, account_type))
        return nullptr;

    acc = static_cast<Account*>(SWIG_MustGetPtr(val, account_type, 1, 0));

    /* Get the value */
    val = SCM_CDR (valuearg);
    value = gnc_scm_to_numeric (val);

    /* Build and return the object */
    res = g_new0 (GncAccountValue, 1);
    res->account = acc;
    res->value = value;
    return res;
}

SCM gnc_account_value_ptr_to_scm (GncAccountValue *av)
{
    swig_type_info * account_type = get_acct_type();
    gnc_commodity * com;
    gnc_numeric val;

    if (!av) return SCM_BOOL_F;

    com = xaccAccountGetCommodity (av->account);
    val = gnc_numeric_convert (av->value, gnc_commodity_get_fraction (com),
                               GNC_HOW_RND_ROUND_HALF_UP);

    return scm_cons (SWIG_NewPointerObj(av->account, account_type, 0),
                     gnc_numeric_to_scm (val));
}

typedef struct
{
    SCM proc;
    int num_args;
} GncScmDangler;


static void
delete_scm_hook (gpointer data)
{
    auto scm = static_cast<GncScmDangler*>(data);
    scm_gc_unprotect_object(scm->proc);
    g_free(scm);
}

static void
scm_hook_cb (gpointer data, GncScmDangler *scm)
{
    ENTER("data %p, cbarg %p", data, scm);

    if (scm->num_args == 0)
        scm_call_0 (scm->proc);
    else
    {
        // XXX: FIXME: We really should make sure this is a session!!! */
        scm_call_1 (scm->proc,
            SWIG_NewPointerObj(data, SWIG_TypeQuery("_p_QofSession"), 0));
    }

    LEAVE("");
}

void
gnc_hook_add_scm_dangler (const gchar *name, SCM proc)
{
    GncScmDangler *scm;
    int num_args;

    ENTER("list %s, proc ???", name);
    num_args = gnc_hook_num_args(name);
    g_return_if_fail(num_args >= 0);
    scm = g_new0(GncScmDangler, 1);
    scm_gc_protect_object(proc);
    scm->proc = proc;
    scm->num_args = num_args;
    gnc_hook_add_dangler(name, (GFunc)scm_hook_cb,
                         (GDestroyNotify) delete_scm_hook, scm);
    LEAVE("");
}

time64
gnc_parse_time_to_time64 (const gchar *s, const gchar *format)
{
    struct tm tm{};

    g_return_val_if_fail(s && format, -1);

    if (!strptime(s, format, &tm))
        return -1;

    return gnc_mktime(&tm);
}

