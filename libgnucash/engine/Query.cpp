/********************************************************************\
 * Query.c : api for finding transactions                           *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2002 Linas Vepstas <linas@linas.org>               *
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
\********************************************************************/

#include <config.h>

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <ctype.h>
#include <glib.h>
#include <math.h>
#include <string.h>
#include <sys/types.h>

#include <regex.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include "gnc-lot.h"
#include "Account.h"
#include "Query.h"
#include "Transaction.h"
#include "TransactionP.hpp"

static QofLogModule log_module = GNC_MOD_QUERY;

static GSList *
build_param_list_internal (const char *first, va_list rest)
{
    GSList *list = nullptr;
    char const *param;

    for (param = first; param; param = va_arg (rest, const char *))
        list = g_slist_prepend (list, (gpointer)param);

    return (g_slist_reverse (list));
}

/********************************************************************
 * xaccQueryGetSplitsUniqueTrans
 * Get splits but no more than one from a given transaction.
 ********************************************************************/

SplitList *
xaccQueryGetSplitsUniqueTrans(QofQuery *q)
{
    GList       * splits = qof_query_run(q);
    GList       * current;
    GList       * result = nullptr;
    GHashTable  * trans_hash = g_hash_table_new(g_direct_hash, g_direct_equal);

    for (current = splits; current; current = current->next)
    {
        Split *split = GNC_SPLIT (current->data);
        Transaction *trans = xaccSplitGetParent (split);

        if (!g_hash_table_lookup (trans_hash, trans))
        {
            g_hash_table_insert (trans_hash, trans, trans);
            result = g_list_prepend (result, split);
        }
    }

    g_hash_table_destroy (trans_hash);

    return g_list_reverse (result);
}

/********************************************************************
 * xaccQueryGetTransactions
 * Get transactions matching the query terms, specifying whether
 * we require some or all splits to match
 ********************************************************************/

static void
query_match_all_filter_func(gpointer key, gpointer value, gpointer user_data)
{
    Transaction * t = GNC_TRANSACTION(key);
    int         num_matches = GPOINTER_TO_INT(value);
    auto matches = static_cast<GList**>(user_data);

    if (num_matches == xaccTransCountSplits(t))
    {
        *matches = g_list_prepend(*matches, t);
    }
}

static void
query_match_any_filter_func(gpointer key, gpointer value, gpointer user_data)
{
    Transaction * t = GNC_TRANSACTION(key);
    auto matches = static_cast<GList**>(user_data);
    *matches = g_list_prepend(*matches, t);
}

TransList *
xaccQueryGetTransactions (QofQuery * q, query_txn_match_t runtype)
{
    GList       * splits = qof_query_run(q);
    GList       * current = nullptr;
    GList       * retval = nullptr;
    GHashTable  * trans_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
    Transaction * trans = nullptr;
    gpointer    val = nullptr;
    int         count = 0;

    /* iterate over matching splits, incrementing a match-count in
     * the hash table */
    for (current = splits; current; current = current->next)
    {
        trans = xaccSplitGetParent(GNC_SPLIT(current->data));

        /* don't waste time looking up unless we need the count
         * information */
        if (runtype == QUERY_TXN_MATCH_ALL)
        {
            val   = g_hash_table_lookup(trans_hash, trans);
            count = GPOINTER_TO_INT(val);
        }
        g_hash_table_insert(trans_hash, trans, GINT_TO_POINTER(count + 1));
    }

    /* now pick out the transactions that match */
    if (runtype == QUERY_TXN_MATCH_ALL)
    {
        g_hash_table_foreach(trans_hash, query_match_all_filter_func,
                             &retval);
    }
    else
    {
        g_hash_table_foreach(trans_hash, query_match_any_filter_func,
                             &retval);
    }

    g_hash_table_destroy(trans_hash);

    return retval;
}

/********************************************************************
 * xaccQueryGetLots
 * Get lots matching the query terms, specifying whether
 * we require some or all splits to match
 ********************************************************************/

static void
query_match_all_lot_filter_func(gpointer key, gpointer value, gpointer user_data)
{
    GNCLot *	l = GNC_LOT(key);
    int		num_matches = GPOINTER_TO_INT(value);
    auto matches = static_cast<GList**>(user_data);

    if (num_matches == gnc_lot_count_splits(l))
    {
        *matches = g_list_prepend(*matches, l);
    }
}

static void
query_match_any_lot_filter_func(gpointer key, gpointer value, gpointer user_data)
{
    GNCLot *	t = GNC_LOT(key);
    auto matches = static_cast<GList**>(user_data);
    *matches = g_list_prepend(*matches, t);
}

LotList *
xaccQueryGetLots (QofQuery * q, query_txn_match_t runtype)
{
    GList       * splits = qof_query_run(q);
    GList       * current = nullptr;
    GList       * retval = nullptr;
    GHashTable  * lot_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
    GNCLot      * lot = nullptr;
    gpointer    val = nullptr;
    int         count = 0;

    /* iterate over matching splits, incrementing a match-count in
     * the hash table */
    for (current = splits; current; current = current->next)
    {
        lot = xaccSplitGetLot((GNC_SPLIT (current->data)));

        /* don't waste time looking up unless we need the count
         * information */
        if (runtype == QUERY_TXN_MATCH_ALL)
        {
            val   = g_hash_table_lookup(lot_hash, lot);
            count = GPOINTER_TO_INT(val);
        }
        g_hash_table_insert(lot_hash, lot, GINT_TO_POINTER(count + 1));
    }

    /* now pick out the transactions that match */
    if (runtype == QUERY_TXN_MATCH_ALL)
    {
        g_hash_table_foreach(lot_hash, query_match_all_lot_filter_func,
                             &retval);
    }
    else
    {
        g_hash_table_foreach(lot_hash, query_match_any_lot_filter_func,
                             &retval);
    }

    g_hash_table_destroy(lot_hash);

    return retval;
}

/*******************************************************************
 *  match-adding API
 *******************************************************************/

void
xaccQueryAddAccountMatch(QofQuery *q, AccountList *acct_list,
                         QofGuidMatch how, QofQueryOp op)
{
    GList *list = nullptr;

    if (!q) return;
    for (; acct_list; acct_list = acct_list->next)
    {
        Account *acc = GNC_ACCOUNT (acct_list->data);
        const GncGUID *guid;

        if (!acc)
        {
            PWARN ("acct_list has nullptr account");
            continue;
        }

        guid = qof_entity_get_guid (QOF_INSTANCE(acc));
        if (!guid)
        {
            PWARN ("acct returns nullptr GncGUID");
            continue;
        }

        list = g_list_prepend (list, (gpointer)guid);
    }
    xaccQueryAddAccountGUIDMatch (q, list, how, op);
    g_list_free (list);
}

void
xaccQueryAddAccountGUIDMatch(QofQuery *q, AccountGUIDList *guid_list,
                             QofGuidMatch how, QofQueryOp op)
{
    QofQueryPredData *pred_data;
    GSList *param_list = nullptr;

    if (!q) return;

    if (!guid_list && how != QOF_GUID_MATCH_NULL)
    {
        g_warning("Got a nullptr guid_list but the QofGuidMatch is not MATCH_nullptr (but instead %d). In other words, the list of GUID matches is empty but it must contain something non-empty.", how);
        /* qof_query_guid_predicate() would trigger a g_warning as well */
        return;
    }
    pred_data = qof_query_guid_predicate (how, guid_list);
    if (!pred_data)
        return;

    switch (how)
    {
    case QOF_GUID_MATCH_ANY:
    case QOF_GUID_MATCH_NONE:
        param_list = qof_query_build_param_list (SPLIT_ACCOUNT, QOF_PARAM_GUID, nullptr);
        break;
    case QOF_GUID_MATCH_ALL:
        param_list = qof_query_build_param_list (SPLIT_TRANS, TRANS_SPLITLIST,
                     SPLIT_ACCOUNT_GUID, nullptr);
        break;
    default:
        PERR ("Invalid match type: %d", how);
        break;
    }

    qof_query_add_term (q, param_list, pred_data, op);
}

void
xaccQueryAddSingleAccountMatch(QofQuery *q, Account *acc, QofQueryOp op)
{
    GList *list;
    const GncGUID *guid;

    if (!q || !acc)
        return;

    guid = qof_entity_get_guid (QOF_INSTANCE(acc));
    g_return_if_fail (guid);

    list = g_list_prepend (nullptr, (gpointer)guid);
    xaccQueryAddAccountGUIDMatch (q, list, QOF_GUID_MATCH_ANY, op);
    g_list_free (list);
}

void
xaccQueryAddStringMatch (QofQuery* q, const char *matchstring,
                         gboolean case_sens, gboolean use_regexp,
                         QofQueryCompare how, QofQueryOp op,
                         const char * path, ...)
{
    QofQueryPredData *pred_data;
    GSList *param_list;
    va_list ap;

    if (!path || !q)
        return;

    pred_data = qof_query_string_predicate (how, (char *)matchstring,
                                            (case_sens ? QOF_STRING_MATCH_NORMAL :
                                                    QOF_STRING_MATCH_CASEINSENSITIVE),
                                            use_regexp);
    if (!pred_data)
        return;

    va_start (ap, path);
    param_list = build_param_list_internal (path, ap);
    va_end (ap);

    qof_query_add_term (q, param_list, pred_data, op);
}

void
xaccQueryAddNumericMatch (QofQuery *q, gnc_numeric amount, QofNumericMatch sign,
                          QofQueryCompare how, QofQueryOp op,
                          const char * path, ...)
{
    QofQueryPredData *pred_data;
    GSList *param_list;
    va_list ap;

    if (!q || !path)
        return;

    pred_data = qof_query_numeric_predicate (how, sign, amount);
    if (!pred_data)
        return;

    va_start (ap, path);
    param_list = build_param_list_internal (path, ap);
    va_end (ap);

    qof_query_add_term (q, param_list, pred_data, op);
}

/* The DateMatch queries match transactions whose posted date
 *    is in a date range.  If use_start is TRUE, then a matching
 *    posted date will be greater than the start date.   If
 *    use_end is TRUE, then a match occurs for posted dates earlier
 *    than the end date.  If both flags are set, then *both*
 *    conditions must hold ('and').  If neither flag is set, then
 *    all transactions are matched.
 */

void
xaccQueryAddDateMatchTT (QofQuery * q,
                         gboolean use_start, time64 stt,
                         gboolean use_end, time64 ett,
                         QofQueryOp op)
{
    QofQuery *tmp_q = nullptr;
    QofQueryPredData *pred_data;
    GSList *param_list;

    if (!q || (!use_start && !use_end))
        return;

    tmp_q = qof_query_create ();

    if (use_start)
    {
        pred_data = qof_query_date_predicate (QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, stt);
        if (!pred_data)
        {
            qof_query_destroy (tmp_q);
            return;
        }

        param_list = qof_query_build_param_list (SPLIT_TRANS, TRANS_DATE_POSTED, nullptr);
        qof_query_add_term (tmp_q, param_list, pred_data, QOF_QUERY_AND);
    }

    if (use_end)
    {
        pred_data = qof_query_date_predicate (QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, ett);
        if (!pred_data)
        {
            qof_query_destroy (tmp_q);
            return;
        }

        param_list = qof_query_build_param_list (SPLIT_TRANS, TRANS_DATE_POSTED, nullptr);
        qof_query_add_term (tmp_q, param_list, pred_data, QOF_QUERY_AND);
    }

    qof_query_merge_in_place (q, tmp_q, op);
    qof_query_destroy (tmp_q);
}

void
xaccQueryGetDateMatchTT (QofQuery * q,
                         time64 * stt,
                         time64 * ett)
{
    QofQueryPredData *term_data;
    GSList *param_list;
    GSList *terms, *tmp;

    *stt = 0;
    *ett = 0;

    param_list = qof_query_build_param_list (SPLIT_TRANS, TRANS_DATE_POSTED, nullptr);
    terms = qof_query_get_term_type (q, param_list);
    g_slist_free(param_list);

    for (tmp = terms; tmp; tmp = g_slist_next(tmp))
    {
        term_data = static_cast<QofQueryPredData*>(tmp->data);
        if (term_data->how == QOF_COMPARE_GTE)
            qof_query_date_predicate_get_date(term_data, stt);
        if (term_data->how == QOF_COMPARE_LTE)
            qof_query_date_predicate_get_date(term_data, ett);
    }
    g_slist_free(terms);
}

/********************************************************************
 * xaccQueryAddDateMatch
 * Add a date filter to an existing query.
 ********************************************************************/

void
xaccQueryAddDateMatch(QofQuery * q,
                      gboolean use_start, int sday, int smonth, int syear,
                      gboolean use_end, int eday, int emonth, int eyear,
                      QofQueryOp op)
{
    /* gcc -O3 will auto-inline this function, avoiding a call overhead */
    xaccQueryAddDateMatchTT (q, use_start,
                             gnc_dmy2time64(sday, smonth, syear),
                             use_end,
                             gnc_dmy2time64_end(eday, emonth, eyear),
                             op);
}

void
xaccQueryAddClearedMatch(QofQuery * q, cleared_match_t how, QofQueryOp op)
{
    QofQueryPredData *pred_data;
    GSList *param_list;
    char chars[6];
    int i = 0;

    if (!q)
        return;

    if (how & CLEARED_CLEARED)
        chars[i++] = CREC;
    if (how & CLEARED_RECONCILED)
        chars[i++] = YREC;
    if (how & CLEARED_FROZEN)
        chars[i++] = FREC;
    if (how & CLEARED_NO)
        chars[i++] = NREC;
    if (how & CLEARED_VOIDED)
        chars[i++] = VREC;
    chars[i] = '\0';

    pred_data = qof_query_char_predicate (QOF_CHAR_MATCH_ANY, chars);
    if (!pred_data)
        return;

    param_list = qof_query_build_param_list (SPLIT_RECONCILE, nullptr);

    qof_query_add_term (q, param_list, pred_data, op);
}

cleared_match_t
xaccQueryGetClearedMatch(QofQuery * q)
{
    QofQueryPredData *term_data;
    cleared_match_t cleared_match = CLEARED_ALL;
    GSList *param_list;
    GSList *terms, *tmp;
    char *chars = nullptr;

    param_list = qof_query_build_param_list (SPLIT_RECONCILE, nullptr);
    terms = qof_query_get_term_type (q, param_list);
    g_slist_free (param_list);

    for (tmp = terms; tmp; tmp = g_slist_next (tmp))
    {
        term_data = static_cast<QofQueryPredData*>(tmp->data);

        if (qof_query_char_predicate_get_char (term_data, &chars))
        {
             cleared_match = CLEARED_NONE;

            if (strstr (chars, "c"))
                cleared_match = (cleared_match_t)(cleared_match | CLEARED_CLEARED);
            if (strstr (chars, "y"))
                cleared_match = (cleared_match_t)(cleared_match | CLEARED_RECONCILED);
            if (strstr (chars, "f"))
                cleared_match = (cleared_match_t)(cleared_match | CLEARED_FROZEN);
            if (strstr (chars, "n"))
                cleared_match = (cleared_match_t)(cleared_match | CLEARED_NO);
            if (strstr (chars, "v"))
                cleared_match = (cleared_match_t)(cleared_match | CLEARED_VOIDED);
        }
    }
    g_slist_free (terms);

    return cleared_match;
}

void
xaccQueryAddGUIDMatch(QofQuery * q, const GncGUID *guid,
                      QofIdType id_type, QofQueryOp op)
{
    GSList *param_list = nullptr;

    if (!q || !guid || !id_type)
        return;

    if (!g_strcmp0 (id_type, GNC_ID_SPLIT))
        param_list = qof_query_build_param_list (QOF_PARAM_GUID, nullptr);
    else if (!g_strcmp0 (id_type, GNC_ID_TRANS))
        param_list = qof_query_build_param_list (SPLIT_TRANS, QOF_PARAM_GUID, nullptr);
    else if (!g_strcmp0 (id_type, GNC_ID_ACCOUNT))
        param_list = qof_query_build_param_list (SPLIT_ACCOUNT, QOF_PARAM_GUID, nullptr);
    else
        PERR ("Invalid match type: %s", id_type);

    qof_query_add_guid_match (q, param_list, guid, op);
}


/********************************************************************
 * xaccQueryAddClosingTransMatch
 * Add a filter that matches book closing entries to an existing query.
 ********************************************************************/

void
xaccQueryAddClosingTransMatch(QofQuery *q, gboolean value, QofQueryOp op)
{
    GSList *param_list;

    param_list = qof_query_build_param_list(SPLIT_TRANS, TRANS_IS_CLOSING, nullptr);
    qof_query_add_boolean_match(q, param_list, value, op);
}

/*******************************************************************
 *  xaccQueryGetEarliestDateFound
 *******************************************************************/

time64
xaccQueryGetEarliestDateFound(QofQuery * q)
{
    GList * spl;
    Split * sp;
    time64 earliest;

    if (!q) return 0;
    spl = qof_query_last_run (q);
    if (!spl) return 0;

    /* Safe until 2038 on archs where time64 is 32bit */
    sp = GNC_SPLIT (spl->data);
    earliest = sp->parent->date_posted;
    for (; spl; spl = spl->next)
    {
        sp = GNC_SPLIT (spl->data);
        if (sp->parent->date_posted < earliest)
        {
            earliest = sp->parent->date_posted;
        }
    }
    return earliest;
}

/*******************************************************************
 *  xaccQueryGetLatestDateFound
 *******************************************************************/

time64
xaccQueryGetLatestDateFound(QofQuery * q)
{
    Split  * sp;
    GList  * spl;
    time64 latest = 0;

    if (!q) return 0;
    spl = qof_query_last_run (q);
    if (!spl) return 0;

    for (; spl; spl = spl->next)
    {
        sp = GNC_SPLIT (spl->data);
        if (sp->parent->date_posted > latest)
        {
            latest = sp->parent->date_posted;
        }
    }
    return latest;
}

void
xaccQueryAddDescriptionMatch(QofQuery *q, const char *m, gboolean c, gboolean r,
                             QofQueryCompare h, QofQueryOp o)
{
    xaccQueryAddStringMatch ((q), (m), (c), (r), (h), (o), SPLIT_TRANS,
                             TRANS_DESCRIPTION, nullptr);
}

void
xaccQueryAddNotesMatch(QofQuery *q, const char *m, gboolean c, gboolean r,
                             QofQueryCompare h, QofQueryOp o)
{
    xaccQueryAddStringMatch ((q), (m), (c), (r), (h), (o), SPLIT_TRANS,
                             TRANS_NOTES, nullptr);
}

void
xaccQueryAddNumberMatch(QofQuery *q, const char *m, gboolean c, gboolean r,
                        QofQueryCompare h, QofQueryOp o)
{
    xaccQueryAddStringMatch ((q), (m), (c), (r), (h), (o), SPLIT_TRANS,
                             TRANS_NUM, nullptr);
}

void
xaccQueryAddActionMatch(QofQuery *q, const char *m, gboolean c, gboolean r,
                        QofQueryCompare h, QofQueryOp o)
{
    xaccQueryAddStringMatch ((q), (m), (c), (r), (h), (o), SPLIT_ACTION, nullptr);
}

void
xaccQueryAddMemoMatch(QofQuery *q, const char *m, gboolean c, gboolean r,
                      QofQueryCompare h, QofQueryOp o)
{
    xaccQueryAddStringMatch ((q), (m), (c), (r), (h), (o), SPLIT_MEMO, nullptr);
}

void
xaccQueryAddValueMatch(QofQuery *q, gnc_numeric amt, QofNumericMatch sgn,
                       QofQueryCompare how, QofQueryOp op)
{
    xaccQueryAddNumericMatch ((q), (amt), (sgn), (how), (op),
                              SPLIT_VALUE, nullptr);
}

void
xaccQueryAddSharePriceMatch(QofQuery *q, gnc_numeric amt, QofQueryCompare how,
                            QofQueryOp op)
{
    xaccQueryAddNumericMatch ((q), (amt), QOF_NUMERIC_MATCH_ANY, (how), (op),
                              SPLIT_SHARE_PRICE, nullptr);
}

void
xaccQueryAddSharesMatch(QofQuery *q, gnc_numeric amt, QofQueryCompare how,
                        QofQueryOp op)
{
    xaccQueryAddNumericMatch ((q), (amt), QOF_NUMERIC_MATCH_ANY, (how), (op),
                              SPLIT_AMOUNT, nullptr);
}

void
xaccQueryAddBalanceMatch(QofQuery *q, QofQueryCompare bal, QofQueryOp op)
{
    xaccQueryAddNumericMatch(
        (q), gnc_numeric_zero(), QOF_NUMERIC_MATCH_ANY,
        ((bal) ? QOF_COMPARE_EQUAL : QOF_COMPARE_NEQ), (op),
        SPLIT_TRANS, TRANS_IMBALANCE, nullptr);
}


/* ======================== END OF FILE ======================= */
