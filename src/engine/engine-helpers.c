/********************************************************************\
 * engine-helpers.c -- gnucash engine helper functions              *
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

#include "config.h"

#include "swig-runtime.h"
#include <libguile.h>
#include <string.h>

#include "Account.h"
#include "engine-helpers.h"
#include "glib-helpers.h"
#include "gnc-date.h"
#include "gnc-engine.h"
#include "guile-mappings.h"
#include "qof.h"
/** \todo Code dependent on the private query headers
qofquery-p.h and qofquerycore-p.h may need to be modified.
These files are temporarily exported for QOF 0.6.0 but
cannot be considered "standard" or public parts of QOF. */
#include "qofquery-p.h"
#include "qofquerycore-p.h"

#define FUNC_NAME G_STRFUNC

static QofLogModule log_module = GNC_MOD_ENGINE;

Timespec
gnc_transaction_get_date_posted(const Transaction *t)
{
    Timespec result;
    xaccTransGetDatePostedTS(t, &result);
    return(result);
}

Timespec
gnc_transaction_get_date_entered(const Transaction *t)
{
    Timespec result;
    xaccTransGetDateEnteredTS(t, &result);
    return(result);
}

Timespec
gnc_split_get_date_reconciled(const Split *s)
{
    Timespec result;
    xaccSplitGetDateReconciledTS(s, &result);
    return(result);
}

void
gnc_transaction_set_date_posted(Transaction *t, const Timespec d)
{
    xaccTransSetDatePostedTS(t, &d);
}

void
gnc_transaction_set_date_entered(Transaction *t, const Timespec d)
{
    xaccTransSetDateEnteredTS(t, &d);
}

void
gnc_transaction_set_date(Transaction *t, Timespec ts)
{
    xaccTransSetDatePostedTS(t, &ts);
}

SCM
gnc_timespec2timepair(Timespec t)
{
    SCM secs;
    SCM nsecs;

    secs = gnc_gint64_to_scm(t.tv_sec);
    nsecs = scm_long2num(t.tv_nsec);
    return(scm_cons(secs, nsecs));
}

Timespec
gnc_timepair2timespec(SCM x)
{
    Timespec result = {0, 0};
    if (gnc_timepair_p (x))
    {
        result.tv_sec = gnc_scm_to_gint64(SCM_CAR(x));
        result.tv_nsec = scm_num2long(SCM_CDR(x), SCM_ARG1, G_STRFUNC);
    }
    return(result);
}

int
gnc_timepair_p(SCM x)
{
    return(scm_is_pair(x) &&
           gnc_gh_gint64_p(SCM_CAR(x)) &&
           gnc_gh_gint64_p(SCM_CDR(x)));
}

SCM
gnc_guid2scm(GUID guid)
{
    char string[GUID_ENCODING_LENGTH + 1];

    if (!guid_to_string_buff(&guid, string))
        return SCM_BOOL_F;

    return scm_makfrom0str(string);
}

GUID
gnc_scm2guid(SCM guid_scm)
{
    GUID guid;
    const gchar * str;

    if (GUID_ENCODING_LENGTH != scm_i_string_length (guid_scm))
    {
        return *guid_null();
    }
    str = scm_to_locale_string (guid_scm);
    string_to_guid(str, &guid);
    return guid;
}

int
gnc_guid_p(SCM guid_scm)
{
    GUID guid;
    const gchar * str;

    if (!scm_is_string(guid_scm))
        return FALSE;

    if (GUID_ENCODING_LENGTH != scm_i_string_length (guid_scm))
    {
        return FALSE;
    }
    str = scm_to_locale_string (guid_scm);

    return string_to_guid(str, &guid);
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
 *   ('pd-date pr-type sense-bool use-start-bool start-timepair
 *             use-end-bool use-end-timepair)
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
    return scm_num2int(how_scm, SCM_ARG1, G_STRFUNC);
}

/* QofStringMatch */
static QofStringMatch
gnc_query_scm2string (SCM how_scm)
{
    return scm_num2int(how_scm, SCM_ARG1, G_STRFUNC);
}

/* QofDateMatch */
static QofDateMatch
gnc_query_scm2date (SCM how_scm)
{
    return scm_num2int(how_scm, SCM_ARG1, G_STRFUNC);
}

/* QofNumericMatch */
static QofNumericMatch
gnc_query_scm2numericop (SCM how_scm)
{
    return scm_num2int(how_scm, SCM_ARG1, G_STRFUNC);
}

/* QofGuidMatch */
static QofGuidMatch
gnc_query_scm2guid (SCM how_scm)
{
    return scm_num2int(how_scm, SCM_ARG1, G_STRFUNC);
}

/* QofCharMatch */
static QofCharMatch
gnc_query_scm2char (SCM how_scm)
{
    return scm_num2int(how_scm, SCM_ARG1, G_STRFUNC);
}

static QofGuidMatch
gnc_scm2acct_match_how (SCM how_scm)
{
    QofGuidMatch res;
    const gchar *how = SCM_SYMBOL_CHARS (how_scm);

    if (!safe_strcmp (how, "acct-match-all"))
        res = QOF_GUID_MATCH_ALL;
    else if (!safe_strcmp (how, "acct-match-any"))
        res = QOF_GUID_MATCH_ANY;
    else if (!safe_strcmp (how, "acct-match-none"))
        res = QOF_GUID_MATCH_NONE;
    else
    {
        PINFO ("invalid account match: %s", how);
        res = QOF_GUID_MATCH_NULL;
    }
    return res;
}

static QofQueryCompare
gnc_scm2amt_match_how (SCM how_scm)
{
    QofQueryCompare res;
    const gchar *how = SCM_SYMBOL_CHARS (how_scm);

    if (!safe_strcmp (how, "amt-match-atleast"))
        res = QOF_COMPARE_GTE;
    else if (!safe_strcmp (how, "amt-match-atmost"))
        res = QOF_COMPARE_LTE;
    else if (!safe_strcmp (how, "amt-match-exactly"))
        res = QOF_COMPARE_EQUAL;
    else
    {
        PINFO ("invalid amount match: %s", how);
        res = QOF_COMPARE_EQUAL;
    }

    return res;
}

static QofQueryCompare
gnc_scm2kvp_match_how (SCM how_scm)
{
    QofQueryCompare res;
    const gchar *how = SCM_SYMBOL_CHARS (how_scm);

    if (!safe_strcmp (how, "kvp-match-lt"))
        res = QOF_COMPARE_LT;
    else if (!safe_strcmp (how, "kvp-match-lte"))
        res = QOF_COMPARE_LTE;
    else if (!safe_strcmp (how, "kvp-match-eq"))
        res = QOF_COMPARE_EQUAL;
    else if (!safe_strcmp (how, "kvp-match-gte"))
        res = QOF_COMPARE_GTE;
    else if (!safe_strcmp (how, "kvp-match-gt"))
        res = QOF_COMPARE_GT;
    else
    {
        PINFO ("invalid kvp match: %s", how);
        res = QOF_COMPARE_EQUAL;
    }
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

        bit = scm_num2int(scm, SCM_ARG2, G_STRFUNC);
        field |= bit;
    }

    return field;
}

static cleared_match_t
gnc_scm2cleared_match_how (SCM how_scm)
{
    return gnc_scm2bitfield (how_scm);
}

static gboolean
gnc_scm2balance_match_how (SCM how_scm, gboolean *resp)
{
    const gchar *how;

    if (!scm_is_list (how_scm))
        return FALSE;

    if (scm_is_null (how_scm))
        return FALSE;

    /* Only allow a single-entry list */
    if (!scm_is_null (SCM_CDR (how_scm)))
        return FALSE;

    how = SCM_SYMBOL_CHARS (SCM_CAR(how_scm));

    if (!safe_strcmp (how, "balance-match-balanced"))
        *resp = TRUE;
    else
        *resp = FALSE;

    return TRUE;
}

static QofIdType
gnc_scm2kvp_match_where (SCM where_scm)
{
    QofIdType res;
    const gchar *where;

    if (!scm_is_list (where_scm))
        return NULL;

    where = SCM_SYMBOL_CHARS (SCM_CAR(where_scm));

    if (!safe_strcmp (where, "kvp-match-split"))
        res = GNC_ID_SPLIT;
    else if (!safe_strcmp (where, "kvp-match-trans"))
        res = GNC_ID_TRANS;
    else if (!safe_strcmp (where, "kvp-match-account"))
        res = GNC_ID_ACCOUNT;
    else
    {
        PINFO ("Unknown kvp-match-where: %s", where);
        res = NULL;
    }
    return res;
}

static SCM
gnc_guid_glist2scm (const GList *account_guids)
{
    SCM guids = SCM_EOL;
    const GList *node;

    for (node = account_guids; node; node = node->next)
    {
        GUID *guid = node->data;

        if (guid)
            guids = scm_cons (gnc_guid2scm (*guid), guids);
    }

    return scm_reverse (guids);
}

static GList *
gnc_scm2guid_glist (SCM guids_scm)
{
    GList *guids = NULL;

    if (!scm_is_list (guids_scm))
        return NULL;

    while (!scm_is_null (guids_scm))
    {
        SCM guid_scm = SCM_CAR (guids_scm);
        GUID *guid = NULL;

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

static void
gnc_guid_glist_free (GList *guids)
{
    GList *node;

    for (node = guids; node; node = node->next)
        guid_free (node->data);

    g_list_free (guids);
}

static SCM
gnc_query_numeric2scm (gnc_numeric val)
{
    return scm_cons (gnc_gint64_to_scm (val.num),
                     gnc_gint64_to_scm (val.denom));
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

    return gnc_numeric_create (gnc_scm_to_gint64 (num),
                               gnc_scm_to_gint64 (denom));
}

static SCM
gnc_query_path2scm (const GSList *path)
{
    SCM path_scm = SCM_EOL;
    const GSList *node;

    for (node = path; node; node = node->next)
    {
        const char *key = node->data;

        if (key)
            path_scm = scm_cons (scm_makfrom0str (key), path_scm);
    }

    return scm_reverse (path_scm);
}

GSList *
gnc_query_scm2path (SCM path_scm)
{
    GSList *path = NULL;

    if (!scm_is_list (path_scm))
        return NULL;

    while (!scm_is_null (path_scm))
    {
        SCM key_scm = SCM_CAR (path_scm);
        char *key;

        if (!scm_is_string (key_scm))
            break;

        key = g_strdup (scm_to_locale_string (key_scm));

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

static KvpValueType
gnc_scm2KvpValueTypeype (SCM value_type_scm)
{
    return scm_num2int(value_type_scm, SCM_ARG1, G_STRFUNC);
}

static SCM gnc_kvp_frame2scm (KvpFrame *frame);

static SCM
gnc_kvp_value2scm (const KvpValue *value)
{
    SCM value_scm = SCM_EOL;
    KvpValueType value_t;
    SCM scm;

    if (!value) return SCM_BOOL_F;

    value_t = kvp_value_get_type (value);

    value_scm = scm_cons (scm_long2num (value_t), value_scm);

    switch (value_t)
    {
    case KVP_TYPE_GINT64:
        scm = gnc_gint64_to_scm (kvp_value_get_gint64 (value));
        break;

    case KVP_TYPE_DOUBLE:
        scm = scm_make_real (kvp_value_get_double (value));
        break;

    case KVP_TYPE_STRING:
        scm = scm_makfrom0str (kvp_value_get_string (value));
        break;

    case KVP_TYPE_GUID:
        scm = gnc_guid2scm (*kvp_value_get_guid (value));
        break;

    case KVP_TYPE_TIMESPEC:
        scm = gnc_timespec2timepair (kvp_value_get_timespec (value));
        break;

    case KVP_TYPE_BINARY:
        scm = SCM_BOOL_F;
        break;

    case KVP_TYPE_NUMERIC:
    {
        gnc_numeric n = kvp_value_get_numeric (value);
        scm = gnc_query_numeric2scm (n);
        break;
    }

    case KVP_TYPE_GLIST:
    {
        GList *node;

        scm = SCM_EOL;
        for (node = kvp_value_get_glist (value); node; node = node->next)
            scm = scm_cons (gnc_kvp_value2scm (node->data), scm);
        scm = scm_reverse (scm);
        break;
    }

    case KVP_TYPE_FRAME:
        scm = gnc_kvp_frame2scm (kvp_value_get_frame (value));
        break;

    default:
        scm = SCM_BOOL_F;
        break;
    }

    value_scm = scm_cons (scm, value_scm);

    return scm_reverse (value_scm);
}

typedef struct
{
    SCM scm;
} KVPSCMData;

static void
kvp_frame_slot2scm (const char *key, KvpValue *value, gpointer data)
{
    KVPSCMData *ksd = data;
    SCM value_scm;
    SCM key_scm;
    SCM pair;

    key_scm = scm_makfrom0str (key);
    value_scm = gnc_kvp_value2scm (value);
    pair = scm_cons (key_scm, value_scm);

    ksd->scm = scm_cons (pair, ksd->scm);
}

static SCM
gnc_kvp_frame2scm (KvpFrame *frame)
{
    KVPSCMData ksd;

    if (!frame) return SCM_BOOL_F;

    ksd.scm = SCM_EOL;

    kvp_frame_for_each_slot (frame, kvp_frame_slot2scm, &ksd);

    return ksd.scm;
}

static KvpFrame * gnc_scm2KvpFrame (SCM frame_scm);

static KvpValue *
gnc_scm2KvpValue (SCM value_scm)
{
    KvpValueType value_t;
    KvpValue *value = NULL;
    SCM type_scm;
    SCM val_scm;

    if (!scm_is_list (value_scm) || scm_is_null (value_scm))
        return NULL;

    type_scm = SCM_CAR (value_scm);
    value_t = gnc_scm2KvpValueTypeype (type_scm);

    value_scm = SCM_CDR (value_scm);
    if (!scm_is_list (value_scm) || scm_is_null (value_scm))
        return NULL;

    val_scm = SCM_CAR (value_scm);

    switch (value_t)
    {
    case KVP_TYPE_GINT64:
        value = kvp_value_new_gint64 (gnc_scm_to_gint64 (val_scm));
        break;

    case KVP_TYPE_DOUBLE:
        value = kvp_value_new_double (scm_num2dbl (val_scm, G_STRFUNC));
        break;

    case KVP_TYPE_STRING:
    {
        const gchar * str = scm_to_locale_string (val_scm);
        value = kvp_value_new_string (str);
        break;
    }

    case KVP_TYPE_GUID:
    {
        if (val_scm != SCM_BOOL_F)
        {
            GUID guid = gnc_scm2guid (val_scm);
            value = kvp_value_new_guid (&guid);
        }
        else
            value = NULL;
        break;
    }

    case KVP_TYPE_TIMESPEC:
    {
        Timespec ts = gnc_timepair2timespec (val_scm);
        value = kvp_value_new_timespec(ts);
        break;
    }

    case KVP_TYPE_GDATE:
    {
        Timespec ts = gnc_timepair2timespec (val_scm);
        value = kvp_value_new_gdate(timespec_to_gdate(ts));
        break;
    }

    case KVP_TYPE_BINARY:
        return NULL;
        break;

    case KVP_TYPE_NUMERIC:
    {
        gnc_numeric n;

        if (!gnc_query_numeric_p (val_scm))
            return NULL;

        n = gnc_query_scm2numeric (val_scm);

        value = kvp_value_new_gnc_numeric (n);
        break;
    }

    case KVP_TYPE_GLIST:
    {
        GList *list = NULL;
        GList *node;

        for (; scm_is_list (val_scm) && !scm_is_null (val_scm);
                val_scm = SCM_CDR (val_scm))
        {
            SCM scm = SCM_CAR (val_scm);

            list = g_list_prepend (list, gnc_scm2KvpValue (scm));
        }

        list = g_list_reverse (list);

        value = kvp_value_new_glist (list);

        for (node = list; node; node = node->next)
            kvp_value_delete (node->data);
        g_list_free (list);
        break;
    }

    case KVP_TYPE_FRAME:
    {
        KvpFrame *frame;

        frame = gnc_scm2KvpFrame (val_scm);
        value = kvp_value_new_frame (frame);
        kvp_frame_delete (frame);
        break;
    }

    }

    return value;
}

static KvpFrame *
gnc_scm2KvpFrame (SCM frame_scm)
{
    KvpFrame * frame;

    if (!scm_is_list (frame_scm)) return NULL;

    frame = kvp_frame_new ();

    for (; scm_is_list (frame_scm) && !scm_is_null (frame_scm);
            frame_scm = SCM_CDR (frame_scm))
    {
        SCM pair = SCM_CAR (frame_scm);
        KvpValue *value;
        SCM key_scm;
        SCM val_scm;
        const gchar *key;

        if (!scm_is_pair (pair))
            continue;

        key_scm = SCM_CAR (pair);
        val_scm = SCM_CDR (pair);

        if (!scm_is_string (key_scm))
            continue;

        key = scm_to_locale_string (key_scm);
        if (!key)
            continue;

        value = gnc_scm2KvpValue (val_scm);
        if (!value) continue;

        kvp_frame_set_slot_nc (frame, key, value);
    }

    return frame;
}

static SCM
gnc_queryterm2scm (const QofQueryTerm *qt)
{
    SCM qt_scm = SCM_EOL;
    QofQueryPredData *pd = NULL;

    qt_scm = scm_cons (gnc_query_path2scm (qof_query_term_get_param_path (qt)),
                       qt_scm);
    qt_scm = scm_cons (SCM_BOOL (qof_query_term_is_inverted (qt)), qt_scm);

    pd = qof_query_term_get_pred_data (qt);
    qt_scm = scm_cons (scm_str2symbol (pd->type_name), qt_scm);
    qt_scm = scm_cons (scm_long2num (pd->how), qt_scm);

    if (!safe_strcmp (pd->type_name, QOF_TYPE_STRING))
    {
        query_string_t pdata = (query_string_t) pd;

        qt_scm = scm_cons (scm_long2num (pdata->options), qt_scm);
        qt_scm = scm_cons (SCM_BOOL (pdata->is_regex), qt_scm);
        qt_scm = scm_cons (scm_makfrom0str (pdata->matchstring), qt_scm);

    }
    else if (!safe_strcmp (pd->type_name, QOF_TYPE_DATE))
    {
        query_date_t pdata = (query_date_t) pd;

        qt_scm = scm_cons (scm_long2num (pdata->options), qt_scm);
        qt_scm = scm_cons (gnc_timespec2timepair (pdata->date), qt_scm);

    }
    else if (!safe_strcmp (pd->type_name, QOF_TYPE_NUMERIC))
    {
        query_numeric_t pdata = (query_numeric_t) pd;

        qt_scm = scm_cons (scm_long2num (pdata->options), qt_scm);
        qt_scm = scm_cons (gnc_query_numeric2scm (pdata->amount), qt_scm);

    }
    else if (!safe_strcmp (pd->type_name, QOF_TYPE_GUID))
    {
        query_guid_t pdata = (query_guid_t) pd;

        qt_scm = scm_cons (scm_long2num (pdata->options), qt_scm);
        qt_scm = scm_cons (gnc_guid_glist2scm (pdata->guids), qt_scm);

    }
    else if (!safe_strcmp (pd->type_name, QOF_TYPE_INT64))
    {
        query_int64_t pdata = (query_int64_t) pd;

        qt_scm = scm_cons (gnc_gint64_to_scm (pdata->val), qt_scm);

    }
    else if (!safe_strcmp (pd->type_name, QOF_TYPE_DOUBLE))
    {
        query_double_t pdata = (query_double_t) pd;

        qt_scm = scm_cons (scm_make_real (pdata->val), qt_scm);

    }
    else if (!safe_strcmp (pd->type_name, QOF_TYPE_BOOLEAN))
    {
        query_boolean_t pdata = (query_boolean_t) pd;

        qt_scm = scm_cons (SCM_BOOL (pdata->val), qt_scm);

    }
    else if (!safe_strcmp (pd->type_name, QOF_TYPE_CHAR))
    {
        query_char_t pdata = (query_char_t) pd;

        qt_scm = scm_cons (scm_long2num (pdata->options), qt_scm);
        qt_scm = scm_cons (scm_makfrom0str (pdata->char_list), qt_scm);

    }
    else if (!safe_strcmp (pd->type_name, QOF_TYPE_KVP))
    {
        query_kvp_t pdata = (query_kvp_t) pd;

        qt_scm = scm_cons (gnc_query_path2scm (pdata->path), qt_scm);
        qt_scm = scm_cons (gnc_kvp_value2scm (pdata->value), qt_scm);

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
    QofQuery *q = NULL;
    QofQueryPredData *pd = NULL;
    SCM scm;
    const gchar *type = NULL;
    GSList *path = NULL;
    gboolean inverted = FALSE;
    QofQueryCompare compare_how;

    if (!scm_is_list (qt_scm) || scm_is_null (qt_scm))
        return NULL;

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
        type = SCM_SYMBOL_CHARS (scm);

        /* QofCompareFunc */
        scm = SCM_CAR (qt_scm);
        qt_scm = SCM_CDR (qt_scm);
        if (scm_is_null (scm))
            break;
        compare_how = gnc_query_scm2compare (scm);

        /* Now compute the predicate */

        if (!safe_strcmp (type, QOF_TYPE_STRING))
        {
            QofStringMatch options;
            gboolean is_regex;
            const gchar *matchstring;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm)) break;
            options = gnc_query_scm2string (scm);

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!scm_is_bool (scm)) break;
            is_regex = scm_is_true (scm);

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!scm_is_string (scm)) break;

            matchstring = scm_to_locale_string (scm);

            pd = qof_query_string_predicate (compare_how, matchstring,
                                             options, is_regex);
        }
        else if (!safe_strcmp (type, QOF_TYPE_DATE))
        {
            QofDateMatch options;
            Timespec date;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            options = gnc_query_scm2date (scm);

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            date = gnc_timepair2timespec (scm);

            pd = qof_query_date_predicate (compare_how, options, date);

        }
        else if (!safe_strcmp (type, QOF_TYPE_NUMERIC))
        {
            QofNumericMatch options;
            gnc_numeric val;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            options = gnc_query_scm2numericop (scm);

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!gnc_query_numeric_p (scm))
                break;
            val = gnc_query_scm2numeric (scm);

            pd = qof_query_numeric_predicate (compare_how, options, val);

        }
        else if (!safe_strcmp (type, QOF_TYPE_GUID))
        {
            QofGuidMatch options;
            GList *guids;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            options = gnc_query_scm2guid (scm);

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!scm_is_list (scm))
                break;
            guids = gnc_scm2guid_glist (scm);

            pd = qof_query_guid_predicate (options, guids);

            gnc_guid_glist_free (guids);

        }
        else if (!safe_strcmp (type, QOF_TYPE_INT64))
        {
            gint64 val;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            val = gnc_scm_to_gint64 (scm);

            pd = qof_query_int64_predicate (compare_how, val);

        }
        else if (!safe_strcmp (type, QOF_TYPE_DOUBLE))
        {
            double val;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!scm_is_number (scm))
                break;
            val = scm_num2dbl (scm, G_STRFUNC);

            pd = qof_query_double_predicate (compare_how, val);

        }
        else if (!safe_strcmp (type, QOF_TYPE_BOOLEAN))
        {
            gboolean val;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!scm_is_bool (scm))
                break;
            val = scm_is_true (scm);

            pd = qof_query_boolean_predicate (compare_how, val);

        }
        else if (!safe_strcmp (type, QOF_TYPE_CHAR))
        {
            QofCharMatch options;
            const gchar *char_list;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
                break;
            options = gnc_query_scm2char (scm);

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!scm_is_string (scm))
                break;
            char_list = scm_to_locale_string (scm);

            pd = qof_query_char_predicate (options, char_list);
        }
        else if (!safe_strcmp (type, QOF_TYPE_KVP))
        {
            GSList *kvp_path;
            KvpValue *value;

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (!scm_is_list (scm))
                break;
            kvp_path = gnc_query_scm2path (scm);

            scm = SCM_CAR (qt_scm);
            qt_scm = SCM_CDR (qt_scm);
            if (scm_is_null (scm))
            {
                gnc_query_path_free (kvp_path);
                break;
            }
            value = gnc_scm2KvpValue (scm);

            pd = qof_query_kvp_predicate (compare_how, kvp_path, value);
            gnc_query_path_free (kvp_path);
            kvp_value_delete (value);

        }
        else
        {
            PWARN ("query core type %s not supported", type);
            break;
        }

    }
    while (FALSE);

    if (pd)
    {
        q = qof_query_create ();
        qof_query_add_term (q, path, pd, QOF_QUERY_OR);
        if (inverted)
        {
            Query *outq = qof_query_invert (q);
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
    const gchar * pd_type = NULL;
    const gchar * pr_type = NULL;
    gboolean sense = FALSE;
    QofQuery *q = NULL;
    SCM scm;

    if (!scm_is_list (query_term_scm) ||
            scm_is_null (query_term_scm))
    {
        PINFO ("null term");
        return NULL;
    }

    do
    {
        /* pd_type */
        scm = SCM_CAR (query_term_scm);
        query_term_scm = SCM_CDR (query_term_scm);
        pd_type = SCM_SYMBOL_CHARS (scm);

        /* pr_type */
        if (scm_is_null (query_term_scm))
        {
            PINFO ("null pr_type");
            break;
        }
        scm = SCM_CAR (query_term_scm);
        query_term_scm = SCM_CDR (query_term_scm);
        pr_type = SCM_SYMBOL_CHARS (scm);

        /* sense */
        if (scm_is_null (query_term_scm))
        {
            PINFO ("null sense");
            break;
        }
        scm = SCM_CAR (query_term_scm);
        query_term_scm = SCM_CDR (query_term_scm);
        sense = scm_is_true (scm);

        q = xaccMallocQuery ();

        if (!safe_strcmp (pd_type, "pd-date"))
        {
            gboolean use_start;
            gboolean use_end;
            Timespec start;
            Timespec end;

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
            start = gnc_timepair2timespec (scm);

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
            query_term_scm = SCM_CDR (query_term_scm);
            end = gnc_timepair2timespec (scm);

            xaccQueryAddDateMatchTS (q, use_start, start, use_end, end, QOF_QUERY_OR);

            ok = TRUE;

        }
        else if (!safe_strcmp (pd_type, "pd-amount"))
        {
            QofQueryCompare how;
            QofNumericMatch amt_sgn;
            double amount;
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
            query_term_scm = SCM_CDR (query_term_scm);
            amount = scm_num2dbl (scm, G_STRFUNC);

            val = double_to_gnc_numeric (amount, GNC_DENOM_AUTO,
                                         GNC_HOW_DENOM_SIGFIGS(6) | GNC_HOW_RND_ROUND);

            if (!safe_strcmp (pr_type, "pr-price"))
            {
                xaccQueryAddSharePriceMatch (q, val, how, QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!safe_strcmp (pr_type, "pr-shares"))
            {
                xaccQueryAddSharesMatch (q, val, how, QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!safe_strcmp (pr_type, "pr-value"))
            {
                xaccQueryAddValueMatch (q, val, amt_sgn, how, QOF_QUERY_OR);
                ok = TRUE;

            }
            else
            {
                PINFO ("unknown amount predicate: %s", pr_type);
            }

        }
        else if (!safe_strcmp (pd_type, "pd-account"))
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
            query_term_scm = SCM_CDR (query_term_scm);

            account_guids = gnc_scm2guid_glist (scm);

            xaccQueryAddAccountGUIDMatch (q, account_guids, how, QOF_QUERY_OR);

            gnc_guid_glist_free (account_guids);

            ok = TRUE;

        }
        else if (!safe_strcmp (pd_type, "pd-string"))
        {
            gboolean case_sens;
            gboolean use_regexp;
            const gchar *matchstring;

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
            query_term_scm = SCM_CDR (query_term_scm);
            matchstring = scm_to_locale_string (scm);

            if (!safe_strcmp (pr_type, "pr-action"))
            {
                xaccQueryAddActionMatch (q, matchstring, case_sens, use_regexp,
                                         QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!safe_strcmp (pr_type, "pr-desc"))
            {
                xaccQueryAddDescriptionMatch (q, matchstring, case_sens,
                                              use_regexp, QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!safe_strcmp (pr_type, "pr-memo"))
            {
                xaccQueryAddMemoMatch (q, matchstring, case_sens, use_regexp,
                                       QOF_QUERY_OR);
                ok = TRUE;

            }
            else if (!safe_strcmp (pr_type, "pr-num"))
            {
                xaccQueryAddNumberMatch (q, matchstring, case_sens, use_regexp,
                                         QOF_QUERY_OR);
                ok = TRUE;

            }
            else
            {
                PINFO ("Unknown string predicate: %s", pr_type);
            }

        }
        else if (!safe_strcmp (pd_type, "pd-cleared"))
        {
            cleared_match_t how;

            /* how */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            how = gnc_scm2cleared_match_how (scm);

            xaccQueryAddClearedMatch (q, how, QOF_QUERY_OR);
            ok = TRUE;

        }
        else if (!safe_strcmp (pd_type, "pd-balance"))
        {
            gboolean how;

            /* how */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            if (gnc_scm2balance_match_how (scm, &how) == FALSE)
                break;

            xaccQueryAddBalanceMatch (q, how, QOF_QUERY_OR);
            ok = TRUE;

        }
        else if (!safe_strcmp (pd_type, "pd-guid"))
        {
            GUID guid;
            QofIdType id_type;

            /* guid */
            if (scm_is_null (query_term_scm))
                break;

            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            guid = gnc_scm2guid (scm);

            /* id type */
            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            id_type = g_strdup (scm_to_locale_string (scm));

            xaccQueryAddGUIDMatch (q, &guid, id_type, QOF_QUERY_OR);
            ok = TRUE;

        }
        else if (!safe_strcmp (pd_type, "pd-kvp"))
        {
            GSList *path;
            KvpValue *value;
            QofQueryCompare how;
            QofIdType where;

            /* how */
            if (scm_is_null (query_term_scm))
                break;
            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            how = gnc_scm2kvp_match_how (scm);

            /* where */
            if (scm_is_null (query_term_scm))
                break;
            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            where = gnc_scm2kvp_match_where (scm);

            /* path */
            if (scm_is_null (query_term_scm))
                break;
            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            path = gnc_query_scm2path (scm);

            /* value */
            if (scm_is_null (query_term_scm))
                break;
            scm = SCM_CAR (query_term_scm);
            query_term_scm = SCM_CDR (query_term_scm);
            value = gnc_scm2KvpValue (scm);

            xaccQueryAddKVPMatch (q, path, value, how, where, QOF_QUERY_OR);

            gnc_query_path_free (path);
            kvp_value_delete (value);
            ok = TRUE;

        }
        else
        {
            PINFO ("Unknown Predicate: %s", pd_type);
        }

    }
    while (FALSE);

    if (ok)
    {
        Query *out_q;

        if (sense)
            out_q = q;
        else
        {
            out_q = xaccQueryInvert (q);
            xaccFreeQuery (q);
        }

        return out_q;
    }

    xaccFreeQuery (q);
    return NULL;
}

static Query *
gnc_scm2query_term_query (SCM query_term_scm, query_version_t vers)
{
    switch (vers)
    {
    case gnc_QUERY_v1:
        return gnc_scm2query_term_query_v1 (query_term_scm);
    case gnc_QUERY_v2:
        return gnc_scm2query_term_query_v2 (query_term_scm);
    default:
        return NULL;
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

        for (and_node = or_node->data; and_node; and_node = and_node->next)
        {
            QofQueryTerm *qt = and_node->data;
            SCM qt_scm;

            qt_scm = gnc_queryterm2scm (qt);

            and_terms = scm_cons (qt_scm, and_terms);
        }

        and_terms = scm_reverse (and_terms);

        or_terms = scm_cons (and_terms, or_terms);
    }

    return scm_reverse (or_terms);
}

static Query *
gnc_scm2query_and_terms (SCM and_terms, query_version_t vers)
{
    Query *q = NULL;

    if (!scm_is_list (and_terms))
        return NULL;

    while (!scm_is_null (and_terms))
    {
        SCM term;

        term = SCM_CAR (and_terms);
        and_terms = SCM_CDR (and_terms);

        if (!q)
            q = gnc_scm2query_term_query (term, vers);
        else
        {
            Query *q_and;
            Query *q_new;

            q_and = gnc_scm2query_term_query (term, vers);

            if (q_and)
            {
                q_new = xaccQueryMerge (q, q_and, QOF_QUERY_AND);

                if (q_new)
                {
                    xaccFreeQuery (q);
                    q = q_new;
                }
            }
        }
    }

    return q;
}

static Query *
gnc_scm2query_or_terms (SCM or_terms, query_version_t vers)
{
    Query *q = NULL;

    if (!scm_is_list (or_terms))
        return NULL;

    q = xaccMallocQuery ();

    while (!scm_is_null (or_terms))
    {
        SCM and_terms;

        and_terms = SCM_CAR (or_terms);
        or_terms = SCM_CDR (or_terms);

        if (!q)
            q = gnc_scm2query_and_terms (and_terms, vers);
        else
        {
            Query *q_or;
            Query *q_new;

            q_or = gnc_scm2query_and_terms (and_terms, vers);

            if (q_or)
            {
                q_new = xaccQueryMerge (q, q_or, QOF_QUERY_OR);

                if (q_new)
                {
                    xaccFreeQuery (q);
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
    if (path == NULL)
        return SCM_BOOL_F;

    sort_scm = scm_cons (gnc_query_path2scm (path), sort_scm);
    sort_scm = scm_cons (scm_int2num (qof_query_sort_get_sort_options (qs)), sort_scm);
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
    g_return_val_if_fail (*path == NULL, FALSE);

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
    o = scm_num2int (val, SCM_ARG1, G_STRFUNC);

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

#ifndef HAVE_GUILE18
    ++scm_block_gc;
#endif

    /* terms */
    pair = scm_cons (gnc_query_terms2scm (qof_query_get_terms (q)), SCM_EOL);
    pair = scm_cons (scm_str2symbol ("terms"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* search-for */
    pair = scm_cons (scm_str2symbol (qof_query_get_search_for (q)), SCM_EOL);
    pair = scm_cons (scm_str2symbol ("search-for"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* sorts... */
    qof_query_get_sorts (q, &s1, &s2, &s3);

    /* primary-sort */
    pair = scm_cons (gnc_query_sort2scm (s1), SCM_EOL);
    pair = scm_cons (scm_str2symbol ("primary-sort"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* secondary-sort */
    pair = scm_cons (gnc_query_sort2scm (s2), SCM_EOL);
    pair = scm_cons (scm_str2symbol ("secondary-sort"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* tertiary-sort */
    pair = scm_cons (gnc_query_sort2scm (s3), SCM_EOL);
    pair = scm_cons (scm_str2symbol ("tertiary-sort"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* max results */
    pair = scm_cons (scm_int2num (qof_query_get_max_results (q)), SCM_EOL);
    pair = scm_cons (scm_str2symbol ("max-results"), pair);
    query_scm = scm_cons (pair, query_scm);

    /* Reverse this list; tag it as 'query-v2' */
    pair = scm_reverse (query_scm);
#ifndef HAVE_GUILE18
    --scm_block_gc;
#endif
    return scm_cons (scm_str2symbol ("query-v2"), pair);
}

static GSList *
gnc_query_sort_to_list (const gchar * symbol)
{
    GSList *path = NULL;

    if (!symbol)
        return NULL;

    if (!safe_strcmp (symbol, "by-none"))
    {
        path = NULL;
    }
    else if (!safe_strcmp (symbol, "by-standard"))
    {
        path = g_slist_prepend (path, QUERY_DEFAULT_SORT);

    }
    else if (!safe_strcmp (symbol, "by-date") ||
             !safe_strcmp (symbol, "by-date-rounded"))
    {
        path = g_slist_prepend (path, TRANS_DATE_POSTED);
        path = g_slist_prepend (path, SPLIT_TRANS);

    }
    else if (!safe_strcmp (symbol, "by-date-entered") ||
             !safe_strcmp (symbol, "by-date-entered-rounded"))
    {
        path = g_slist_prepend (path, TRANS_DATE_ENTERED);
        path = g_slist_prepend (path, SPLIT_TRANS);

    }
    else if (!safe_strcmp (symbol, "by-date-reconciled") ||
             !safe_strcmp (symbol, "by-date-reconciled-rounded"))
    {
        path = g_slist_prepend (path, SPLIT_DATE_RECONCILED);

    }
    else if (!safe_strcmp (symbol, "by-num"))
    {
        path = g_slist_prepend (path, TRANS_NUM);
        path = g_slist_prepend (path, SPLIT_TRANS);

    }
    else if (!safe_strcmp (symbol, "by-amount"))
    {
        path = g_slist_prepend (path, SPLIT_VALUE);

    }
    else if (!safe_strcmp (symbol, "by-memo"))
    {
        path = g_slist_prepend (path, SPLIT_MEMO);

    }
    else if (!safe_strcmp (symbol, "by-desc"))
    {
        path = g_slist_prepend (path, TRANS_DESCRIPTION);
        path = g_slist_prepend (path, SPLIT_TRANS);

    }
    else if (!safe_strcmp (symbol, "by-reconcile"))
    {
        path = g_slist_prepend (path, SPLIT_RECONCILE);

    }
    else if (!safe_strcmp (symbol, "by-account-full-name"))
    {
        path = g_slist_prepend (path, SPLIT_ACCT_FULLNAME);

    }
    else if (!safe_strcmp (symbol, "by-account-code"))
    {
        path = g_slist_prepend (path, ACCOUNT_CODE_);
        path = g_slist_prepend (path, SPLIT_ACCOUNT);

    }
    else if (!safe_strcmp (symbol, "by-corr-account-full-name"))
    {
        path = g_slist_prepend (path, SPLIT_CORR_ACCT_NAME);

    }
    else if (!safe_strcmp (symbol, "by-corr-account-code"))
    {
        path = g_slist_prepend (path, SPLIT_CORR_ACCT_CODE);

    }
    else
    {
        PERR ("Unknown sort-type, %s", symbol);
    }

    return path;
}

static Query *
gnc_scm2query_v1 (SCM query_scm)
{
    Query *q = NULL;
    gboolean ok = TRUE;
    const gchar * primary_sort = NULL;
    const gchar * secondary_sort = NULL;
    const gchar * tertiary_sort = NULL;
    gboolean primary_increasing = TRUE;
    gboolean secondary_increasing = TRUE;
    gboolean tertiary_increasing = TRUE;
    int max_splits = -1;

    while (!scm_is_null (query_scm))
    {
        const gchar *symbol;
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

        symbol = SCM_SYMBOL_CHARS (sym_scm);
        if (!symbol)
        {
            PERR ("No string found");
            ok = FALSE;
            break;
        }

        if (safe_strcmp ("terms", symbol) == 0)
        {
            if (q)
                xaccFreeQuery (q);

            q = gnc_scm2query_or_terms (value, gnc_QUERY_v1);
            if (!q)
            {
                PINFO ("invalid terms");
                ok = FALSE;
                break;
            }

        }
        else if (safe_strcmp ("primary-sort", symbol) == 0)
        {
            if (!scm_is_symbol (value))
            {
                PINFO ("Invalid primary sort");
                ok = FALSE;
                break;
            }

            primary_sort = SCM_SYMBOL_CHARS (value);

        }
        else if (safe_strcmp ("secondary-sort", symbol) == 0)
        {
            if (!scm_is_symbol (value))
            {
                PINFO ("Invalid secondary sort");
                ok = FALSE;
                break;
            }

            secondary_sort = SCM_SYMBOL_CHARS (value);

        }
        else if (safe_strcmp ("tertiary-sort", symbol) == 0)
        {
            if (!scm_is_symbol (value))
            {
                PINFO ("Invalid tertiary sort");
                ok = FALSE;
                break;
            }

            tertiary_sort = SCM_SYMBOL_CHARS (value);

        }
        else if (safe_strcmp ("primary-increasing", symbol) == 0)
        {
            primary_increasing = scm_is_true (value);

        }
        else if (safe_strcmp ("secondary-increasing", symbol) == 0)
        {
            secondary_increasing = scm_is_true (value);

        }
        else if (safe_strcmp ("tertiary-increasing", symbol) == 0)
        {
            tertiary_increasing = scm_is_true (value);

        }
        else if (safe_strcmp ("max-splits", symbol) == 0)
        {
            if (!scm_is_number (value))
            {
                PERR ("invalid max-splits");
                ok = FALSE;
                break;
            }

            max_splits = scm_num2int (value, SCM_ARG1, G_STRFUNC);

        }
        else
        {
            PERR ("Unknown symbol: %s", symbol);
            ok = FALSE;
            break;
        }
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
        xaccQuerySetMaxSplits (q, max_splits);

        return q;
    }

    xaccFreeQuery (q);
    return NULL;
}

static Query *
gnc_scm2query_v2 (SCM query_scm)
{
    Query *q = NULL;
    gboolean ok = TRUE;
    const gchar * search_for = NULL;
    GSList *sp1 = NULL, *sp2 = NULL, *sp3 = NULL;
    gint so1 = 0, so2 = 0, so3 = 0;
    gboolean si1 = TRUE, si2 = TRUE, si3 = TRUE;
    int max_results = -1;

#ifndef HAVE_GUILE18
    ++scm_block_gc;
#endif

    while (!scm_is_null (query_scm))
    {
        const gchar *symbol;
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

        symbol = SCM_SYMBOL_CHARS (sym_scm);
        if (!symbol)
        {
            ok = FALSE;
            break;
        }

        if (!safe_strcmp ("terms", symbol))
        {
            if (q)
                xaccFreeQuery (q);

            q = gnc_scm2query_or_terms (value, gnc_QUERY_v2);
            if (!q)
            {
                ok = FALSE;
                break;
            }

        }
        else if (!safe_strcmp ("search-for", symbol))
        {
            if (!scm_is_symbol (value))
            {
                ok = FALSE;
                break;
            }
            search_for = SCM_SYMBOL_CHARS (value);

        }
        else if (safe_strcmp ("primary-sort", symbol) == 0)
        {
            if (! gnc_query_scm2sort (value, &sp1, &so1, &si1))
            {
                ok = FALSE;
                break;
            }

        }
        else if (!safe_strcmp ("secondary-sort", symbol))
        {
            if (! gnc_query_scm2sort (value, &sp2, &so2, &si2))
            {
                ok = FALSE;
                break;
            }

        }
        else if (!safe_strcmp ("tertiary-sort", symbol))
        {
            if (! gnc_query_scm2sort (value, &sp3, &so3, &si3))
            {
                ok = FALSE;
                break;
            }

        }
        else if (!safe_strcmp ("max-results", symbol))
        {
            if (!scm_is_number (value))
            {
                ok = FALSE;
                break;
            }

            max_results = scm_num2int (value, SCM_ARG1, G_STRFUNC);

        }
        else
        {
            ok = FALSE;
            break;
        }
    }

#ifndef HAVE_GUILE18
    --scm_block_gc;
#endif

    if (ok && search_for)
    {
        qof_query_search_for (q, search_for);
        qof_query_set_sort_order (q, sp1, sp2, sp3);
        qof_query_set_sort_options (q, so1, so2, so3);
        qof_query_set_sort_increasing (q, si1, si2, si3);
        qof_query_set_max_results (q, max_results);

        return q;
    }

    xaccFreeQuery (q);
    return NULL;
}

Query *
gnc_scm2query (SCM query_scm)
{
    SCM q_type;
    const gchar *type;
    Query *q = NULL;

    /* Not a list or NULL?  No need to go further */
    if (!scm_is_list (query_scm) || scm_is_null (query_scm))
        return NULL;

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
            return NULL;
        }
    }

    /* Ok, the LHS is the version and the RHS is the actual query list */
    type = SCM_SYMBOL_CHARS (q_type);
    if (!type)
        return NULL;

    if (!safe_strcmp (type, "query-v2"))
        q = gnc_scm2query_v2 (SCM_CDR (query_scm));

    return q;
}

SCM
gnc_gint64_to_scm(const gint64 x)
{
#if GUILE_LONG_LONG_OK
    return scm_long_long2num(x);
#else
    const gchar negative_p = (x < 0);
    const guint64 magnitude = negative_p ? -x : x;
    const guint32 lower_half = (guint32) (magnitude & 0xFFFFFFFF);
    const guint32 upper_half = (guint32) (magnitude >> 32);
    SCM result;

    result = scm_sum(scm_ash(scm_ulong2num(upper_half), SCM_MAKINUM(32)),
                     scm_ulong2num(lower_half));

    if (negative_p)
    {
        return scm_difference(SCM_INUM0, result);
    }
    else
    {
        return result;
    }
#endif
}

gint64
gnc_scm_to_gint64(SCM num)
{
#if GUILE_LONG_LONG_OK
#ifdef SCM_MINOR_VERSION
    /* Guile 1.6 and later have the SCM_XXX_VERSION macro */
    return scm_num2long_long(num, SCM_ARG1, "gnc_scm_to_gint64");
#else
    return scm_num2long_long(num, (char *) SCM_ARG1, "gnc_scm_to_gint64");
#endif
#else
    static SCM bits00to15_mask = SCM_BOOL_F;
    SCM magnitude  = scm_abs(num);
    SCM bits;
    unsigned long c_bits;
    long long     c_result = 0;
    int                i;

    /* This doesn't work -- atm (bit-extract 4000 0 32) proves it */
    /*
    SCM lower = scm_bit_extract(magnitude, SCM_MAKINUM(0), SCM_MAKINUM(32));
    */

    if (bits00to15_mask == SCM_BOOL_F)
    {
        bits00to15_mask = scm_ulong2num(0xFFFF);
        scm_gc_protect_object (bits00to15_mask);
    }

    /*
     * This isn't very complicated (IMHO).  We work from the "top" of
     * the number downwards.  We assume this is no more than a 64-bit
     * number, otherwise it will fail right away.  Anyways, we keep
     * taking the top 16 bits of the number and move it to c_result.
     * Then we 'remove' those bits from the original number and continue
     * with the next 16 bits down, and so on.  -- warlord@mit.edu
     * 2001/02/13
     */
    for (i = 48; i >= 0; i -= 16)
    {
        bits = scm_ash(magnitude, SCM_MAKINUM(-i));
        c_bits = scm_num2ulong(scm_logand(bits, bits00to15_mask), SCM_ARG1, G_STRFUNC);
        c_result += ((long long)c_bits << i);
        magnitude = scm_difference(magnitude, scm_ash(bits, SCM_MAKINUM(i)));
    }

    if (scm_negative_p(num) != SCM_BOOL_F)
    {
        return(- c_result);
    }
    else
    {
        return(c_result);
    }
#endif
}

int
gnc_gh_gint64_p(SCM num)
{
    static int initialized = 0;
    static SCM maxval;
    static SCM minval;

    if (!initialized)
    {
        /* to be super safe, we have to build these manually because
           though we know that we have gint64's here, we *don't* know how
           to portably specify a 64bit constant to the compiler (i.e. like
           0x7FFFFFFFFFFFFFFF). */
        gint64 tmp;

        tmp = 0x7FFFFFFF;
        tmp <<= 32;
        tmp |= 0xFFFFFFFF;
        maxval = gnc_gint64_to_scm(tmp);

        tmp = 0x80000000;
        tmp <<= 32;
        minval = gnc_gint64_to_scm(tmp);

        scm_gc_protect_object(maxval);
        scm_gc_protect_object(minval);
        initialized = 1;
    }

    return (scm_is_exact(num) &&
            (scm_geq_p(num, minval) != SCM_BOOL_F) &&
            (scm_leq_p(num, maxval) != SCM_BOOL_F));
}

gnc_numeric
gnc_scm_to_numeric(SCM gncnum)
{
    static SCM get_num   = SCM_BOOL_F;
    static SCM get_denom = SCM_BOOL_F;

    if (get_num == SCM_BOOL_F)
    {
        get_num = scm_c_eval_string("gnc:gnc-numeric-num");
    }
    if (get_denom == SCM_BOOL_F)
    {
        get_denom = scm_c_eval_string("gnc:gnc-numeric-denom");
    }

    return gnc_numeric_create(gnc_scm_to_gint64(scm_call_1(get_num, gncnum)),
                              gnc_scm_to_gint64(scm_call_1(get_denom, gncnum)));
}

SCM
gnc_numeric_to_scm(gnc_numeric arg)
{
    static SCM maker = SCM_BOOL_F;

    if (maker == SCM_BOOL_F)
    {
        maker = scm_c_eval_string("gnc:make-gnc-numeric");
    }

    return scm_call_2(maker, gnc_gint64_to_scm(gnc_numeric_num(arg)),
                      gnc_gint64_to_scm(gnc_numeric_denom(arg)));
}

int
gnc_numeric_p(SCM arg)
{
    static SCM type_p = SCM_BOOL_F;
    SCM        ret    = SCM_BOOL_F;

    if (type_p == SCM_BOOL_F)
    {
        type_p = scm_c_eval_string("gnc:gnc-numeric?");
    }
    ret = scm_call_1(type_p, arg);

    if (ret == SCM_BOOL_F)
    {
        return FALSE;
    }
    else
    {
        return TRUE;
    }
}


static SCM
gnc_generic_to_scm(const void *cx, const gchar *type_str)
{
    swig_type_info * stype = NULL;
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
    swig_type_info * stype = NULL;

    stype = SWIG_TypeQuery(type_str);
    if (!stype)
    {
        PERR("Unknown SWIG Type: %s ", type_str);
        return NULL;
    }

    if (!SWIG_IsPointerOfType(scm, stype))
        return NULL;

    return SWIG_MustGetPtr(scm, stype, 1, 0);
}

gnc_commodity *
gnc_scm_to_commodity(SCM scm)
{
    return gnc_scm_to_generic(scm, "_p_gnc_commodity");
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

SCM
qof_session_to_scm (const QofSession *session)
{
    return gnc_generic_to_scm(session, "_p_QofSession");
}
