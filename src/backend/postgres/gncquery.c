/********************************************************************\
 * gncquery.c : Convert gnucash engine Query into an SQL Query      *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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

/*
 * FILE:
 * gncquery.c
 *
 * FUNCTION:
 * Convert gnucash engine Query (a la Query.h) into an SQL Query
 *
 * The gnc engine query consists of doubly nested list of
 * query terms.  The inner list consists of terms that need to be
 * AND'ed together; the outer list OR's together the inner lists.
 *
 * HISTORY:
 * Linas Vepstas January 2001
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "Account.h"
#include "Transaction.h"
#include "gnc-engine.h"
/** \todo Code dependent on the private query headers
qofquery-p.h and qofquerycore-p.h may need to be modified.
These files are temporarily exported for QOF 0.6.0 but
cannot be considered "standard" or public parts of QOF. */
#include "qofquery-p.h"
#include "qofquerycore-p.h"

#include "gncquery.h"
#include "builder.h"
#include "escape.h"

static QofLogModule log_module = GNC_MOD_BACKEND;


struct _gnc_query
{
    char *q_base;
    char *pq;
    size_t buflen;
    sqlEscape *escape;
};


/* =========================================================== */

#define INITIAL_BUFSZ 32000

sqlQuery *
sqlQuery_new(void)
{
    sqlQuery *sq = g_new(sqlQuery, 1);

    sq->q_base = g_malloc(INITIAL_BUFSZ);
    sq->buflen = INITIAL_BUFSZ;

    sq->pq = sq->q_base;
    sq->escape = sqlEscape_new();
    return sq;
}

void
sql_Query_destroy(sqlQuery * sq)
{
    ENTER(" ");
    if (!sq)
    {
        LEAVE("sq = (null)");
        return;
    }
    g_free(sq->q_base);
    sqlEscape_destroy(sq->escape);
    g_free(sq);
    LEAVE(" ");
}

/*
 * non-default sort-type.  This combines most of the logic of
 * sort_order and sort_distinct into one place
 */
static char *
sql_sort_get_type(char *p, GSList * path)
{
    if (!safe_strcmp(path->data, SPLIT_TRANS))
    {
        path = path->next;
        if (!path)
            PERR("AIEE -- sorting on the Transaction???");

        if (!safe_strcmp(path->data, TRANS_DATE_POSTED))
        {
            p = stpcpy(p, "gncTransaction.date_posted");
        }
        else if (!safe_strcmp(path->data, TRANS_DATE_ENTERED))
        {
            p = stpcpy(p, "gncTransaction.date_entered");
        }
        else if (!safe_strcmp(path->data, TRANS_NUM))
        {
            p = stpcpy(p, "gncTransaction.num");
        }
        else if (!safe_strcmp(path->data, TRANS_DESCRIPTION))
        {
            p = stpcpy(p, "gncTransaction.description");
        }
        else
        {
            PERR("Unknown Transaction parameter: %s", (char *)(path->data));
        }

    }
    else if (!safe_strcmp(path->data, SPLIT_ACCOUNT))
    {
        path = path->next;
        if (!path)
            PERR("AIEE -- sorting on the Account??");

        if (!safe_strcmp(path->data, ACCOUNT_CODE_))
        {
            /* XXX hack alert FIXME implement this */
            PERR("BY_ACCOUNT_CODE badly implemented");
            p = stpcpy(p, "gncAccount.accountCode");
        }
        else
        {
            PERR("Unknown Account parameter: %s", (char *)(path->data));
        }

    }
    else if (!safe_strcmp(path->data, SPLIT_DATE_RECONCILED))
    {
        p = stpcpy(p, "gncSplit.date_reconciled");
    }
    else if (!safe_strcmp(path->data, SPLIT_MEMO))
    {
        p = stpcpy(p, "gncSplit.memo");
    }
    else if (!safe_strcmp(path->data, SPLIT_RECONCILE))
    {
        p = stpcpy(p, "gncSplit.reconciled");
    }
    else if (!safe_strcmp(path->data, SPLIT_VALUE))
    {
        p = stpcpy(p, "gncSplit.amount");
    }
    else if (!safe_strcmp(path->data, SPLIT_ACCT_FULLNAME))
    {
        /* XXX hack alert FIXME implement this */
        PERR("BY_ACCOUNT_FULL_NAME badly implemented");
        p = stpcpy(p, "gncAccount.accountName");
    }
    else if (!safe_strcmp(path->data, SPLIT_CORR_ACCT_NAME))
    {
        /* XXX hack alert FIXME implement this */
        PERR("BY_CORR_ACCOUNT_FULL_NAME not implemented");
        p = stpcpy(p, "gncAccount.accountName");
    }
    else if (!safe_strcmp(path->data, SPLIT_CORR_ACCT_CODE))
    {
        /* XXX hack alert FIXME implement this */
        PERR("BY_CORR_ACCOUNT_CODE not implemented");
        p = stpcpy(p, "gncAccount.accountCode");

    }
    else
    {
        PERR("Unknown Split parameter: %s", (char *)(path->data));
    }

    return p;
}

/* =========================================================== */
/* sorting order */
/* We use DESC for increasing and ASC for decreasing,
 * since the MaxSplits parameter refers to the *last*
 * splits in the sort order. The real sorting will be
 * done by the Query C code. */

static char *
sql_sort_order(char *p, QofQuerySort *sort)
{
    GSList *path;
    gboolean increasing;

    increasing = qof_query_sort_get_increasing(sort);

    ENTER("incr=%d", increasing);

    path = qof_query_sort_get_param_path(sort);

    if (path == NULL)
    {
        /* Ok, we're not sorting on anything here. */
        ;
    }
    else if (!safe_strcmp(path->data, QUERY_DEFAULT_SORT))
    {
        if (TRUE == increasing)
        {
            p = stpcpy(p,
                       "gncTransaction.date_posted DESC, "
                       "gncTransaction.num DESC, "
                       "gncTransaction.date_entered DESC, "
                       "gncTransaction.description");
        }
        else
        {
            p = stpcpy(p,
                       "gncTransaction.date_posted ASC, "
                       "gncTransaction.num ASC, "
                       "gncTransaction.date_entered ASC, "
                       "gncTransaction.description");
        }

    }
    else
    {
        p = sql_sort_get_type(p, path);
    }

    if (TRUE == increasing)
    {
        p = stpcpy(p, " DESC ");
    }
    else
    {
        p = stpcpy(p, " ASC ");
    }

    return p;
}

/* =========================================================== */
/* distinct clauses */

static char *
sql_sort_distinct(char *p, QofQuerySort *sort)
{
    GSList *path;
    ENTER(".");

    path = qof_query_sort_get_param_path(sort);

    if (NULL != path)
    {
        p = stpcpy(p, ", ");
    }

    if (path == NULL)
    {
        ;

    }
    else if (!safe_strcmp(path->data, QUERY_DEFAULT_SORT))
    {
        p = stpcpy(p, "gncTransaction.date_posted, gncTransaction.num, "
                   "gncTransaction.date_entered, gncTransaction.description");
    }
    else
    {
        p = sql_sort_get_type(p, path);
    }

    return p;
}


/* =========================================================== */
/* does sorting require a reference to this particular table? */

static gboolean
sql_sort_sort_need_account(QofQuerySort *sort)
{
    gboolean need_account = FALSE;
    GSList *path;
    ENTER(".");

    path = qof_query_sort_get_param_path(sort);

    if (path)
        if (!safe_strcmp(path->data, SPLIT_ACCT_FULLNAME) ||
                !safe_strcmp(path->data, SPLIT_CORR_ACCT_NAME) ||
                !safe_strcmp(path->data, SPLIT_CORR_ACCT_CODE) ||
                !safe_strcmp(path->data, SPLIT_ACCOUNT))
            need_account = TRUE;

    return need_account;
}

static gboolean
sql_sort_need_account(QofQuery * q)
{
    gboolean need_account = FALSE;
    QofQuerySort *s1, *s2, *s3;

    qof_query_get_sorts(q, &s1, &s2, &s3);

    need_account = sql_sort_sort_need_account(s1) ||
                   sql_sort_sort_need_account(s2) || sql_sort_sort_need_account(s3);

    return need_account;
}

/* =========================================================== */
/* does sorting require a reference to this particular table? */

static gboolean
sql_sort_sort_need_entry(QofQuerySort *sort)
{
    gboolean need_entry = FALSE;
    GSList *path;
    ENTER(".");

    path = qof_query_sort_get_param_path(sort);

    if (path)
        if (!safe_strcmp(path->data, SPLIT_VALUE) ||
                !safe_strcmp(path->data, SPLIT_DATE_RECONCILED) ||
                !safe_strcmp(path->data, SPLIT_MEMO) ||
                !safe_strcmp(path->data, SPLIT_RECONCILE))
            need_entry = TRUE;

    return need_entry;
}

static gboolean
sql_sort_need_entry(QofQuery * q)
{
    gboolean need_entry = FALSE;
    QofQuerySort *s1, *s2, *s3;

    qof_query_get_sorts(q, &s1, &s2, &s3);

    need_entry = sql_sort_sort_need_entry(s1) ||
                 sql_sort_sort_need_entry(s2) || sql_sort_sort_need_entry(s3);

    return need_entry;
}

/* =========================================================== */
/* Macro for QOF_TYPE_STRING query types
 * Note that postgres supports both case-sensitive and
 * case-insensitve string searches, and it also supports
 * regex!  yahooo!
 */

#define STRING_TERM(fieldname)                                \
{                                                             \
   const char *tmp;                                           \
                                                              \
   if (invert)                                                \
      sq->pq = stpcpy (sq->pq, "NOT (");                      \
   if (pd->how == QOF_COMPARE_NEQ)                            \
      sq->pq = stpcpy (sq->pq, "NOT (");                      \
   if (pdata->is_regex || pdata->options == QOF_STRING_MATCH_CASEINSENSITIVE) \
     sq->pq = stpcpy(sq->pq, fieldname " ~");                 \
   else                                                       \
     sq->pq = stpcpy(sq->pq, fieldname " =");                 \
   if (pdata->options == QOF_STRING_MATCH_CASEINSENSITIVE) {  \
      sq->pq = stpcpy(sq->pq, "*");                           \
   }                                                          \
   sq->pq = stpcpy(sq->pq, " '");                             \
   tmp = sqlEscapeString (sq->escape, pdata->matchstring);    \
   sq->pq = stpcpy(sq->pq, tmp);                              \
   sq->pq = stpcpy(sq->pq, "'");                              \
   if (pd->how == QOF_COMPARE_NEQ)                            \
      sq->pq = stpcpy (sq->pq, ") ");                         \
   if (invert)                                                \
      sq->pq = stpcpy (sq->pq, ") ");                         \
}

/* =========================================================== */
/* Macro for QOF_TYPE_NUMERIC type terms.  The logic used here in the
 * SQL exactly matches that used in the qofquery.c code.  If
 * that code is incorrect or has changed, then the code below is
 * broken as well.
 */

#define AMOUNT_TERM(signcheck,fieldname,comtable)                    \
{                                                                    \
   double amt = gnc_numeric_to_double (pdata->amount);               \
                                                                     \
   if (invert)                                                       \
      sq->pq = stpcpy (sq->pq, "NOT (");                             \
                                                                     \
   switch(pdata->options)                                            \
   {                                                                 \
      case QOF_NUMERIC_MATCH_CREDIT:                                 \
         sq->pq = stpcpy(sq->pq, signcheck " <= 0 AND ");            \
         break;                                                      \
      case QOF_NUMERIC_MATCH_DEBIT:                                  \
         sq->pq = stpcpy(sq->pq, signcheck " >= 0 AND ");            \
         break;                                                      \
      default:                                                       \
         break;                                                      \
   }                                                                 \
   switch(pd->how)                                                   \
   {                                                                 \
      case QOF_COMPARE_GTE:                                          \
         sq->pq = stpcpy(sq->pq,                                     \
            fieldname " >= "comtable" * float8");                    \
         sq->pq += sprintf (sq->pq, "(%22.18g)", amt);               \
         break;                                                      \
      case QOF_COMPARE_GT:                                           \
         sq->pq = stpcpy(sq->pq,                                     \
            fieldname " > "comtable" * float8");                     \
         sq->pq += sprintf (sq->pq, "(%22.18g)", amt);               \
         break;                                                      \
      case QOF_COMPARE_LTE:                                          \
         sq->pq = stpcpy(sq->pq,                                     \
            fieldname " <= "comtable" * float8");                    \
         sq->pq += sprintf (sq->pq, "(%22.18g)", amt);               \
         break;                                                      \
      case QOF_COMPARE_LT:                                           \
         sq->pq = stpcpy(sq->pq,                                     \
            fieldname " < "comtable" * float8");                     \
         sq->pq += sprintf (sq->pq, "(%22.18g)", amt);               \
         break;                                                      \
      case QOF_COMPARE_EQUAL:                                        \
         sq->pq = stpcpy(sq->pq,                                     \
            "abs(" fieldname " - abs("comtable" * float8");          \
         sq->pq += sprintf (sq->pq, "(%22.18g)", amt);               \
         sq->pq = stpcpy(sq->pq, ")) < 1");                          \
         break;                                                      \
      case QOF_COMPARE_NEQ:                                          \
         sq->pq = stpcpy(sq->pq,                                     \
            "abs(" fieldname " - abs("comtable" * float8");          \
         sq->pq += sprintf (sq->pq, "(%22.18g)", amt);               \
         sq->pq = stpcpy(sq->pq, ")) > 1");                          \
         break;                                                      \
   }                                                                 \
   if (invert)                                                       \
      sq->pq = stpcpy (sq->pq, ") ");                                \
                                                                     \
}

/* =========================================================== */
/* Macro for PR_CLEARED term */

#define CLR_TERM(flagchar)                                        \
{                                                                 \
      if (got_one)                                                \
         sq->pq = stpcpy(sq->pq, "OR ");                          \
                                                                  \
      sq->pq = stpcpy(sq->pq, "gncSplit.reconciled = '");         \
      *(sq->pq) = flagchar;  (sq->pq) ++;                         \
      sq->pq = stpcpy(sq->pq, "' ");                              \
      got_one++;                                                  \
}

/* =========================================================== */
static const char *
kvp_table_name(KvpValueType value_t)
{
    switch (value_t)
    {
    case KVP_TYPE_GINT64:
        return "gnckvpvalue_int64";

    case KVP_TYPE_DOUBLE:
        return "gnckvpvalue_dbl";

    case KVP_TYPE_NUMERIC:
        return "gnckvpvalue_numeric";

    case KVP_TYPE_STRING:
        return "gnckvpvalue_str";

    case KVP_TYPE_GUID:
        return "gnckvpvalue_guid";

    case KVP_TYPE_TIMESPEC:
        return "gnckvpvalue_timespec";

    default:
        PWARN("kvp value not supported");
        return NULL;
    }
}

static char *
kvp_path_name(GSList * path)
{
    GString *s = g_string_new(NULL);
    char *name;

    for (; path; path = path->next)
    {
        g_string_append_c(s, '/');
        g_string_append(s, path->data);
    }

    name = s->str;
    g_string_free(s, FALSE);

    return name;
}

static const char *
compare_op_name(QofQueryCompare how)
{
    switch (how)
    {
    case QOF_COMPARE_LT:
        return " < ";

    case QOF_COMPARE_LTE:
        return " <= ";

    case QOF_COMPARE_EQUAL:
        return " = ";

    case QOF_COMPARE_GTE:
        return " >= ";

    case QOF_COMPARE_GT:
        return " > ";

    case QOF_COMPARE_NEQ:
        return " != ";

    default:
        return NULL;
    }
}

static char *
kvp_left_operand(KvpValue * value)
{
    KvpValueType value_t;
    const char *kvptable;

    g_return_val_if_fail(value, NULL);

    value_t = kvp_value_get_type(value);

    kvptable = kvp_table_name(value_t);

    switch (value_t)
    {
    case KVP_TYPE_GINT64:
    case KVP_TYPE_DOUBLE:
    case KVP_TYPE_GUID:
    case KVP_TYPE_TIMESPEC:
    case KVP_TYPE_STRING:
        return g_strdup_printf("%s.data", kvptable);

    case KVP_TYPE_NUMERIC:
    {
        gnc_numeric n = kvp_value_get_numeric(value);
        return g_strdup_printf("(%" G_GINT64_FORMAT "::int8 * %s.num::int8)",
                               n.denom, kvptable);
    }

    default:
        return NULL;
    }
}

static char *
kvp_right_operand(sqlQuery * sq, KvpValue * value)
{
    KvpValueType value_t;
    const char *kvptable;

    g_return_val_if_fail(value, NULL);

    value_t = kvp_value_get_type(value);

    kvptable = kvp_table_name(value_t);

    switch (value_t)
    {
    case KVP_TYPE_GINT64:
        return g_strdup_printf("%" G_GINT64_FORMAT,
                               kvp_value_get_gint64(value));

    case KVP_TYPE_DOUBLE:
        return g_strdup_printf(SQL_DBL_FMT, kvp_value_get_double(value));

    case KVP_TYPE_GUID:
    {
        /* THREAD-UNSAFE */
        const char *guid = guid_to_string(kvp_value_get_guid(value));
        char *s = g_strdup_printf("'%s'", guid);
        return s;
    }

    case KVP_TYPE_TIMESPEC:
    {
        char s[80];
        gnc_timespec_to_iso8601_buff(kvp_value_get_timespec(value), s);
        return g_strdup_printf("'%s'", s);
    }

    case KVP_TYPE_STRING:
    {
        const char *s = sqlEscapeString(sq->escape,
                                        kvp_value_get_string(value));
        return g_strdup_printf("'%s'", s);
    }

    case KVP_TYPE_NUMERIC:
    {
        gnc_numeric n = kvp_value_get_numeric(value);
        return g_strdup_printf("(%" G_GINT64_FORMAT "::int8 * %s.denom::int8)",
                               n.num, kvptable);
    }

    default:
        return NULL;
    }
}

static void
add_kvp_clause(sqlQuery * sq, const char *kvptable, const char *entity_table,
               const char *left, const char *op, const char *right)
{
    sq->pq = stpcpy(sq->pq, " ( ");

    sq->pq = stpcpy(sq->pq, "gncPathCache.ipath = ");
    sq->pq = stpcpy(sq->pq, kvptable);
    sq->pq = stpcpy(sq->pq, ".ipath");

    sq->pq = stpcpy(sq->pq, " AND ");

    sq->pq = stpcpy(sq->pq, entity_table);
    sq->pq = stpcpy(sq->pq, ".iguid");
    sq->pq = stpcpy(sq->pq, " = ");
    sq->pq = stpcpy(sq->pq, kvptable);
    sq->pq = stpcpy(sq->pq, ".iguid");

    sq->pq = stpcpy(sq->pq, " AND ");

    sq->pq = stpcpy(sq->pq, left);
    sq->pq = stpcpy(sq->pq, op);
    sq->pq = stpcpy(sq->pq, right);

    sq->pq = stpcpy(sq->pq, " ) ");
}

static void
sqlQuery_kvp_build(sqlQuery * sq, GSList * sort_path, QofQueryCompare how,
                   gboolean invert, query_kvp_t kpd)
{
    KvpValueType value_t;
    const char *kvptable;
    const char *op;
    GList *list;
    GList *node;
    char *right;
    char *left;
    char *path;

    g_return_if_fail(sq && kpd && kpd->path && kpd->value);

    if (safe_strcmp(sort_path->data, SPLIT_KVP) &&
            ((safe_strcmp(sort_path->data, SPLIT_ACCOUNT) &&
              safe_strcmp(sort_path->data, SPLIT_TRANS)) ||
             safe_strcmp(sort_path->next->data, SPLIT_KVP)))
        return;

    value_t = kvp_value_get_type(kpd->value);

    if (value_t == KVP_TYPE_GUID && how != QOF_COMPARE_EQUAL)
    {
        PWARN("guid non-equality comparison not supported");
        return;
    }

    kvptable = kvp_table_name(value_t);
    if (!kvptable)
        return;

    path = kvp_path_name(kpd->path);
    op = compare_op_name(how);
    left = kvp_left_operand(kpd->value);
    right = kvp_right_operand(sq, kpd->value);

    list = NULL;
    if (!safe_strcmp(sort_path->data, SPLIT_KVP))
        list = g_list_prepend(list, "gncSplit");
    else if (!safe_strcmp(sort_path->data, SPLIT_TRANS))
        list = g_list_prepend(list, "gncTransaction");
    else
        list = g_list_prepend(list, "gncAccount");

    if (invert)
        sq->pq = stpcpy(sq->pq, "NOT ");

    sq->pq = stpcpy(sq->pq, " EXISTS ( SELECT true " "          WHERE ");

    sq->pq = stpcpy(sq->pq, "gncPathCache.path = '");
    sq->pq = stpcpy(sq->pq, sqlEscapeString(sq->escape, path));
    sq->pq = stpcpy(sq->pq, "'");

    for (node = list; node; node = node->next)
    {
        sq->pq = stpcpy(sq->pq, " AND ");
        add_kvp_clause(sq, kvptable, node->data, left, op, right);
    }

    sq->pq = stpcpy(sq->pq, " ) ");

    g_free(path);
    g_free(left);
    g_free(right);
    g_list_free(list);
}

/* =========================================================== */
/* Design Note:
 * The simplest way of doing a query in SQL would be to simply
 * list all of the tables that might or might not be referenced:
 * SELECT * FROM gncAccount, gnEntry, ... WHERE ...
 * This is what this code originally used to do.  However, during
 * performance tuning, I discovered that gratuitously listing a
 * table that is not referenced (e.g. gncAccount when the query
 * doesn't involve accounts) is not gratuitous: it can hurt
 * performance by factors of ten(!!!).  Thus, there is a *lot*
 * (hundreds of lines of code) of extra complexity here to make
 * sure that only the needed tables get listed, and no more.
 *
 * Yes, I agree that this should have been idempotent code, and
 * that a good SQL optimizer would have weeded away and discarded
 * the tables that are not referenced.  But that is not the case
 * as of Postgres 7.1. This code is needed: it delivers a big
 * performance boost.
 *
 * (Tech Note: Postgres (and other databases ??) consider it
 * to be a join with all rows matching when a table is listed
 * in the FROM clause but not used in the WHERE clause: thus,
 * the number of rows returned is sizeof(unrefed-table) times
 * larger than otherwise.)
 *
 * Specifically, I am refering to the 'need_account' and the
 * 'need_entry' booleans.  These serve no functional purpose:
 * You'd get exactly the same results if you just set these to
 * TRUE.  But they affect performance tremendously, and there
 * are hundreds of lines of extra logic to compute these for
 * the sole reason of optimizing performance.
 */

const char *
sqlQuery_build(sqlQuery * sq, Query * q)
{
    GList *il, *jl, *qterms, *andterms;
    GList *tables = NULL;
    GSList *path;
    QofQueryTerm *qt;
    QofQueryPredData *pd;
    QofQuerySort *s1, *s2, *s3;
    int more_or = 0;
    int more_and = 0;
    int max_rows;
    gboolean invert;
    gboolean need_account_commodity = FALSE;
    gboolean need_trans_commodity = FALSE;
    gboolean need_account = FALSE;
    gboolean need_entry = FALSE;

    if (!sq || !q)
        return NULL;

    /* Only Split searches are allowed */
    if (safe_strcmp(qof_query_get_search_for(q), GNC_ID_SPLIT))
    {
        PERR("Only SPLITs are supported, not %s", qof_query_get_search_for(q));
        return NULL;
    }

    /* Determine whether the query will need to reference certain
     * tables. See note above for details. */
    qterms = qof_query_get_terms(q);

    for (il = qterms; il; il = il->next)
    {
        /* andterms is GList of query terms that must be anded */
        andterms = il->data;

        for (jl = andterms; jl; jl = jl->next)
        {
            qt = (QofQueryTerm *) jl->data;
            pd = qof_query_term_get_pred_data(qt);
            path = qof_query_term_get_param_path(qt);

            g_assert(path);

            if (!safe_strcmp(path->data, SPLIT_ACTION) ||
                    !safe_strcmp(path->data, SPLIT_RECONCILE) ||
                    !safe_strcmp(path->data, SPLIT_MEMO) ||
                    !safe_strcmp(path->data, SPLIT_SHARE_PRICE))
            {
                need_entry = TRUE;
            }
            else if (!safe_strcmp(path->data, SPLIT_VALUE))
            {
                need_entry = TRUE;
                need_trans_commodity = TRUE;
            }
            else if (!safe_strcmp(pd->type_name, QOF_TYPE_GUID))
            {
                if (!safe_strcmp(path->data, QOF_PARAM_GUID))
                    need_entry = TRUE;
                else if (!safe_strcmp(path->data, SPLIT_ACCOUNT))
                {
                    need_account = TRUE;
                }
            }
            else if (!safe_strcmp(pd->type_name, QOF_TYPE_KVP))
            {
                if (!safe_strcmp(path->data, SPLIT_KVP))
                    need_entry = TRUE;
                else if (!safe_strcmp(path->data, SPLIT_ACCOUNT))
                    need_account = TRUE;
            }
            else if (!safe_strcmp(path->data, SPLIT_AMOUNT))
            {
                need_entry = TRUE;
                need_account_commodity = TRUE;
                need_account = TRUE;
            }
        }
    }

    /* determine whether the requested sort order needs these tables */
    need_entry = need_entry || sql_sort_need_entry(q);
    need_account = need_account || sql_sort_need_account(q);

    /* reset the buffer pointers */
    sq->pq = sq->q_base;
    sq->pq = stpcpy(sq->pq, "SELECT DISTINCT gncTransaction.* ");

    qof_query_get_sorts(q, &s1, &s2, &s3);

    /* For SELECT DISTINCT, ORDER BY expressions must appear in target list */
    sq->pq = sql_sort_distinct(sq->pq, s1);
    sq->pq = sql_sort_distinct(sq->pq, s2);
    sq->pq = sql_sort_distinct(sq->pq, s3);

    /* add needed explicit tables. postgres can figure out the rest. */
    if (need_account_commodity)
        tables = g_list_prepend(tables, "gncCommodity account_com");

    if (need_trans_commodity)
        tables = g_list_prepend(tables, "gncCommodity trans_com");

    if (tables)
    {
        GList *node;

        sq->pq = stpcpy(sq->pq, " FROM ");

        for (node = tables; node; node = node->next)
        {
            sq->pq = stpcpy(sq->pq, node->data);
            if (node->next)
                sq->pq = stpcpy(sq->pq, ", ");
        }

        g_list_free(tables);
    }

    sq->pq = stpcpy(sq->pq, " WHERE ");

    if (need_entry || need_account)
    {
        sq->pq = stpcpy(sq->pq,
                        " gncSplit.transGuid = gncTransaction.transGuid AND ");
    }

    if (need_account)
    {
        sq->pq = stpcpy(sq->pq,
                        " gncSplit.accountGuid = gncAccount.accountGuid AND ");
    }

    sq->pq = stpcpy(sq->pq, "  ( ");

    /* qterms is a list of lists: outer list is a set of terms
     * that must be OR'ed together, inner lists are a set of terms
     * that must be anded.  Out strategy is to build the sql query
     * of the AND terms first, and OR these together ...
     */
    for (il = qterms; il; il = il->next)
    {
        /* andterms is GList of query terms that must be anded */
        andterms = il->data;

        /* if there are andterms, open a brace */
        if (andterms)
        {
            /* concatenate additional OR terms */
            if (more_or)
                sq->pq = stpcpy(sq->pq, " OR ");
            more_or = 1;
            sq->pq = stpcpy(sq->pq, "(");
        }

        more_and = 0;
        for (jl = andterms; jl; jl = jl->next)
        {
            /* concatencate more terms together */
            if (more_and)
            {
                sq->pq = stpcpy(sq->pq, " AND ");
            }
            more_and = 1;

            qt = (QofQueryTerm *) jl->data;
            pd = qof_query_term_get_pred_data(qt);
            path = qof_query_term_get_param_path(qt);
            invert = qof_query_term_is_inverted(qt);

            if (!safe_strcmp(pd->type_name, QOF_TYPE_GUID))
            {
                query_guid_t pdata = (query_guid_t) pd;
                GList *node;
                char *field = NULL;

                PINFO("term is QOF_TYPE_GUID");

                if (!safe_strcmp(path->data, QOF_PARAM_GUID))
                {
                    field = "gncSplit.splitGuid";
                    g_assert(pdata->options != QOF_GUID_MATCH_ALL);

                }
                else if (!safe_strcmp(path->data, SPLIT_TRANS) &&
                         !safe_strcmp(path->next->data, QOF_PARAM_GUID))
                {
                    field = "gncSplit.transGUID";
                    g_assert(pdata->options != QOF_GUID_MATCH_ALL);

                }
                else if (!safe_strcmp(path->data, SPLIT_ACCOUNT) &&
                         !safe_strcmp(path->next->data, QOF_PARAM_GUID))
                {
                    field = "gncSplit.accountGUID";
                    g_assert(pdata->options != QOF_GUID_MATCH_ALL);

                }
                else if (!safe_strcmp(path->data, SPLIT_TRANS) &&
                         !safe_strcmp(path->next->data, TRANS_SPLITLIST) &&
                         !safe_strcmp(path->next->next->data,
                                      SPLIT_ACCOUNT_GUID))
                {
                    field = "gncSplit.accountGUID";
                    g_assert(pdata->options == QOF_GUID_MATCH_ALL);

                }
                else if (!safe_strcmp(path->data, QOF_PARAM_BOOK) &&
                         !safe_strcmp(path->next->data, QOF_PARAM_GUID))
                {
                    /* XXX: Need to support the Book GUID? (gncAccount.bookGUID) */
                    field = "gncAccount.bookGUID";
                    g_assert(pdata->options != QOF_GUID_MATCH_ALL);
                }
                else
                {
                    PINFO("Unknown GUID parameter, %s", (char *)path->data);
                }

                if (field != NULL)
                {
                    if (invert)
                        sq->pq = stpcpy(sq->pq, "NOT ");

                    sq->pq = stpcpy(sq->pq, "(");

                    for (node = pdata->guids; node; node = node->next)
                    {

                        switch (pdata->options)
                        {
                        case QOF_GUID_MATCH_NONE:
                            sq->pq = stpcpy(sq->pq, "NOT ");
                            /* fall through */

                        case QOF_GUID_MATCH_ANY:
                            sq->pq = stpcpy(sq->pq, field);
                            sq->pq = stpcpy(sq->pq, "='");
                            sq->pq =
                                guid_to_string_buff((GUID *) node->data,
                                                    sq->pq);
                            sq->pq = stpcpy(sq->pq, "'");
                            break;

                        case QOF_GUID_MATCH_ALL:
                            sq->pq = stpcpy(sq->pq,
                                            " EXISTS ( SELECT true FROM gncSplit e"
                                            "          WHERE "
                                            "e.transGuid = gncTransaction.transGuid"
                                            " AND " "e.accountGuid='");
                            sq->pq =
                                guid_to_string_buff((GUID *) node->data,
                                                    sq->pq);
                            sq->pq = stpcpy(sq->pq, "')");

                            break;

                        default:
                            PERR("unexpected guid match type: %d",
                                 pdata->options);
                            break;
                        }

                        if (node->next)
                        {
                            switch (pdata->options)
                            {
                            case QOF_GUID_MATCH_ANY:
                                sq->pq = stpcpy(sq->pq, " OR ");
                                break;

                            case QOF_GUID_MATCH_ALL:
                            case QOF_GUID_MATCH_NONE:
                                sq->pq = stpcpy(sq->pq, " AND ");
                                break;

                            default:
                                PERR("unexpected account match type: %d",
                                     pdata->options);
                                break;
                            }
                        }
                    }

                    sq->pq = stpcpy(sq->pq, ")");

                }
                else
                {
                    /* Empty field -- nothing to "AND" in... */
                    more_and = 0;
                }

            }
            else if (!safe_strcmp(pd->type_name, QOF_TYPE_STRING))
            {
                query_string_t pdata = (query_string_t) pd;

                if (!safe_strcmp(path->data, SPLIT_ACTION))
                {
                    PINFO("term is PR_ACTION");
                    STRING_TERM("gncSplit.action");

                }
                else if (!safe_strcmp(path->data, SPLIT_MEMO))
                {
                    PINFO("term is PR_MEMO");
                    STRING_TERM("gncSplit.memo");

                }
                else if (!safe_strcmp(path->data, SPLIT_TRANS))
                {
                    path = path->next;

                    if (!path)
                    {
                        PINFO("invalid transaction parameter");

                    }
                    else if (!safe_strcmp(path->data, TRANS_DESCRIPTION))
                    {
                        PINFO("term is PR_DESC");
                        STRING_TERM("gncTransaction.description");

                    }
                    else if (!safe_strcmp(path->data, TRANS_NUM))
                    {
                        PINFO("term is PR_NUM");
                        STRING_TERM("gncTransaction.num");
                    }
                    else
                        PINFO("unknown string (transaction) parameter: %s",
                              (char *)(path->data));
                }
                else
                    PINFO("unknown string (split) parameter: %s",
                          (char *)(path->data));


            }
            else if (!safe_strcmp(pd->type_name, QOF_TYPE_NUMERIC))
            {
                query_numeric_t pdata = (query_numeric_t) pd;


                if (!safe_strcmp(path->data, SPLIT_AMOUNT))
                {
                    PINFO("term is PR_SHRS");
                    sq->pq = stpcpy(sq->pq,
                                    "gncAccount.commodity = account_com.commodity AND ");
                    AMOUNT_TERM("gncSplit.amount", "abs(gncSplit.amount)",
                                "account_com.fraction");

                }
                else if (!safe_strcmp(path->data, SPLIT_VALUE))
                {

                    PINFO("term is PR_VALUE");
                    sq->pq = stpcpy(sq->pq,
                                    "gncTransaction.currency = trans_com.commodity AND ");
                    AMOUNT_TERM("gncSplit.value", "abs(gncSplit.value)",
                                "trans_com.fraction");

                }
                else if (!safe_strcmp(path->data, SPLIT_SHARE_PRICE))
                {

                    PINFO("term is PR_PRICE");

                    AMOUNT_TERM("gncSplit.value / gncSplit.amount",
                                "gncHelperPrVal(gncSplit)",
                                "gncHelperPrAmt(gncSplit)");
                }
                else
                {

                    PINFO("Unknown NUMERIC: %s", (char *)(path->data));
                }

            }
            else if (!safe_strcmp(pd->type_name, QOF_TYPE_DATE))
            {
                query_date_t pdata = (query_date_t) pd;
                char *field = NULL;
                const char *op = NULL;

                PINFO("term is PR_DATE");
                if (invert)
                    sq->pq = stpcpy(sq->pq, "NOT (");

                if (!safe_strcmp(path->data, SPLIT_TRANS) &&
                        !safe_strcmp(path->next->data, TRANS_DATE_POSTED))
                    field = "gncTransaction.date_posted";

                op = compare_op_name(pd->how);

                if (field)
                {
                    sq->pq = stpcpy(sq->pq, field);
                    sq->pq = stpcpy(sq->pq, op);
                    sq->pq = stpcpy(sq->pq, "'");
                    sq->pq =
                        gnc_timespec_to_iso8601_buff(pdata->date, sq->pq);
                    sq->pq = stpcpy(sq->pq, "' ");
                }

                if (invert)
                    sq->pq = stpcpy(sq->pq, ") ");

            }
            else if (!safe_strcmp(pd->type_name, QOF_TYPE_CHAR))
            {
                query_char_t pdata = (query_char_t) pd;
                int got_one = 0;

                if (!safe_strcmp(path->data, SPLIT_RECONCILE))
                {

                    PINFO("term is CHAR (Reconcile)");

                    if (invert)
                        sq->pq = stpcpy(sq->pq, "NOT (");

                    if (pdata->options == QOF_CHAR_MATCH_NONE)
                        sq->pq = stpcpy(sq->pq, "NOT ");

                    sq->pq = stpcpy(sq->pq, "(");

                    while (pdata->char_list[got_one] != '\0')
                        CLR_TERM(pdata->char_list[got_one]);

                    sq->pq = stpcpy(sq->pq, ") ");

                    if (invert)
                        sq->pq = stpcpy(sq->pq, ") ");

                }
                else
                    PINFO("Unknown CHAR type, %s", (char *)(path->data));

            }
            else if (!safe_strcmp(pd->type_name, QOF_TYPE_KVP))
            {
                query_kvp_t pdata = (query_kvp_t) pd;

                PINFO("term is a KVP");
                sqlQuery_kvp_build(sq, path, pd->how, invert, pdata);

            }
            else
            {
                PINFO("Unsupported Query Type: %s", pd->type_name);
            }
        }

        /* if there were and terms, close the brace */
        if (il->data)
        {
            sq->pq = stpcpy(sq->pq, ")");
        }
        else
        {
            more_and = 0;
        }
    }

    sq->pq = stpcpy(sq->pq, ") ");

    /* ---------------------------------------------------- */
    /* implement sorting order as well; bad sorts lead to bad data
     * if the limit is set to a finite number of rows.
     */

    if (qof_query_sort_get_param_path(s1) != NULL)
    {
        sq->pq = stpcpy(sq->pq, "ORDER BY ");
        sq->pq = sql_sort_order(sq->pq, s1);

        if (qof_query_sort_get_param_path(s2) != NULL)
        {
            sq->pq = stpcpy(sq->pq, ", ");
            sq->pq = sql_sort_order(sq->pq, s2);

            if (qof_query_sort_get_param_path(s3) != NULL)
            {
                sq->pq = stpcpy(sq->pq, ", ");
                sq->pq = sql_sort_order(sq->pq, s3);
            }
        }
    }

    /* ---------------------------------------------------- */
    /* limit the query result to a finite number of rows */
    max_rows = qof_query_get_max_results(q);
    if (0 <= max_rows)
    {
        sq->pq = stpcpy(sq->pq, " LIMIT ");
        sq->pq += snprintf(sq->pq, 30, "%d", max_rows);
    }

    sq->pq = stpcpy(sq->pq, ";");

    return sq->q_base;
}


/* ========================== END OF FILE ==================== */
