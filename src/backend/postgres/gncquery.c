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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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

#define _GNU_SOURCE
#include "config.h"

#include <glib.h>
#include <string.h>

#include "builder.h"
#include "escape.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gncquery.h"
#include "Transaction.h"

static short module = MOD_BACKEND;


struct _gnc_query {
   char * q_base;
   char * pq;
   size_t buflen;
   sqlEscape *escape;
};


/* =========================================================== */

#define INITIAL_BUFSZ 16300

sqlQuery *
sqlQuery_new(void)
{
   sqlQuery *sq = g_new (sqlQuery, 1);

   sq->q_base = g_malloc (INITIAL_BUFSZ);
   sq->buflen = INITIAL_BUFSZ;

   sq->pq = sq->q_base;
   sq->escape = sqlEscape_new ();
   return sq;
}

void 
sql_Query_destroy (sqlQuery *sq)
{
   if (!sq) return;
   g_free(sq->q_base);
   sqlEscape_destroy (sq->escape);
   g_free(sq);
}

/* =========================================================== */
/* sorting order */

static char *
sql_sort_order (char *p, sort_type_t sort_type, gboolean increasing)
{
   ENTER ("sort type=%d, incr=%d", sort_type, increasing);

   switch (sort_type)
   {
      case BY_STANDARD:
         if (TRUE == increasing)
         {
            p = stpcpy (p, "gncTransaction.date_posted DESC, gncTransaction.num DESC, "
                           "gncTransaction.date_entered DESC, gncTransaction.description");
         } else {
            p = stpcpy (p, "gncTransaction.date_posted ASC, gncTransaction.num ASC, "
                           "gncTransaction.date_entered ASC, gncTransaction.description");
         }
         break;
      case BY_DATE:
         p = stpcpy (p, "gncTransaction.date_posted");
         break;
      case BY_DATE_ROUNDED:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_DATE_ROUNDED badly implemented");
         p = stpcpy (p, "gncTransaction.date_posted");
         break;
      case BY_DATE_ENTERED:
         p = stpcpy (p, "gncTransaction.date_entered");
         break;
      case BY_DATE_ENTERED_ROUNDED:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_DATE_ENTERED_ROUNDED badly implemented");
         p = stpcpy (p, "gncTransaction.date_entered");
         break;
      case BY_DATE_RECONCILED:
         p = stpcpy (p, "gncEntry.date_reconciled");
         break;
      case BY_DATE_RECONCILED_ROUNDED:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_DATE_RECONCILED_ROUNDED badly implemented");
         p = stpcpy (p, "gncEntry.date_reconciled");
         break;
      case BY_NUM:
         p = stpcpy (p, "gncTransaction.num");
         break;
      case BY_AMOUNT:
         p = stpcpy (p, "gncEntry.amount");
         break;
      case BY_MEMO:
         p = stpcpy (p, "gncEntry.memo");
         break;
      case BY_DESC:
         p = stpcpy (p, "gncTransaction.description");
         break;
      case BY_RECONCILE:
         p = stpcpy (p, "gncEntry.reconciled");
         break;
      case BY_ACCOUNT_FULL_NAME:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_ACCOUNT_FULL_NAME badly implemented");
         p = stpcpy (p, "gncAccount.accountName");
         break;
      case BY_ACCOUNT_CODE:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_ACCOUNT_CODE badly implemented");
         p = stpcpy (p, "gncAccount.accountCode");
         break;
      case BY_CORR_ACCOUNT_FULL_NAME:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_CORR_ACCOUNT_FULL_NAME not implemented");
         p = stpcpy (p, "gncAccount.accountName");
         break;
      case BY_CORR_ACCOUNT_CODE:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_CORR_ACCOUNT_CODE not implemented");
         p = stpcpy (p, "gncAccount.accountCode");
         break;
      case BY_NONE:
         break;
      default:
         PERR ("unknown sort type %d", sort_type);
         break;
   }

   if (TRUE == increasing)
   {
      p = stpcpy (p, " DESC ");
   }
   else 
   {
      p = stpcpy (p, " ASC ");
   }

   return p;
}
 
/* =========================================================== */
/* distinct clauses */

static char *
sql_sort_distinct (char *p, sort_type_t sort_type)
{
   ENTER ("sort type=%d", sort_type);

   if (BY_NONE != sort_type)
   {
      p = stpcpy (p, ", ");
   }

   switch (sort_type)
   {
      case BY_STANDARD:
         p = stpcpy (p, "gncTransaction.date_posted, gncTransaction.num, "
                        "gncTransaction.date_entered, gncTransaction.description");
         break;
      case BY_DATE:
      case BY_DATE_ROUNDED:
         p = stpcpy (p, "gncTransaction.date_posted");
         break;
      case BY_DATE_ENTERED:
      case BY_DATE_ENTERED_ROUNDED:
         p = stpcpy (p, "gncTransaction.date_entered");
         break;
      case BY_DATE_RECONCILED:
      case BY_DATE_RECONCILED_ROUNDED:
         p = stpcpy (p, "gncEntry.date_reconciled");
         break;
      case BY_NUM:
         p = stpcpy (p, "gncTransaction.num");
         break;
      case BY_AMOUNT:
         p = stpcpy (p, "gncEntry.amount");
         break;
      case BY_MEMO:
         p = stpcpy (p, "gncEntry.memo");
         break;
      case BY_DESC:
         p = stpcpy (p, "gncTransaction.description");
         break;
      case BY_RECONCILE:
         p = stpcpy (p, "gncEntry.reconciled");
         break;
      case BY_ACCOUNT_FULL_NAME:
         p = stpcpy (p, "gncAccount.accountName");
         break;
      case BY_ACCOUNT_CODE:
         p = stpcpy (p, "gncAccount.accountCode");
         break;
      case BY_CORR_ACCOUNT_FULL_NAME:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_CORR_ACCOUNT_FULL_NAME not implemented");
         p = stpcpy (p, "gncAccount.accountName");
         break;
      case BY_CORR_ACCOUNT_CODE:
         /* XXX hack alert FIXME implement this */
         PERR ("BY_CORR_ACCOUNT_CODE not implemented");
         p = stpcpy (p, "gncAccount.accountCode");
         break;
      case BY_NONE:
         break;
      default:
         PERR ("unknown sort type %d", sort_type);
         break;
   }

   return p;
}
 

/* =========================================================== */
/* does sorting require a reference to this particular table? */

static gboolean 
sql_sort_sort_need_account (sort_type_t sort_type)
{
   gboolean need_account = FALSE;
   ENTER ("sort type=%d", sort_type);

   switch (sort_type)
   {
      case BY_STANDARD:
      case BY_AMOUNT:
      case BY_DATE:
      case BY_DATE_ROUNDED:
      case BY_DATE_ENTERED:
      case BY_DATE_ENTERED_ROUNDED:
      case BY_DATE_RECONCILED:
      case BY_DATE_RECONCILED_ROUNDED:
      case BY_DESC:
      case BY_MEMO:
      case BY_NUM:
      case BY_RECONCILE:
      case BY_NONE:
         break;
      case BY_ACCOUNT_FULL_NAME:
      case BY_ACCOUNT_CODE:
      case BY_CORR_ACCOUNT_FULL_NAME:
      case BY_CORR_ACCOUNT_CODE:
         need_account = TRUE;
         break;
      default:
         PERR ("unknown sort type %d", sort_type);
         break;
   }

   return need_account;
}
 
static gboolean 
sql_sort_need_account (Query *q)
{
   gboolean need_account = FALSE;

   sort_type_t sorter;
   sorter = xaccQueryGetPrimarySortOrder(q);
   if (BY_NONE != sorter)
   {
      need_account = sql_sort_sort_need_account(sorter);
      sorter = xaccQueryGetSecondarySortOrder(q);
      if (BY_NONE != sorter)
      {
         need_account = need_account || sql_sort_sort_need_account(sorter);
         sorter = xaccQueryGetTertiarySortOrder(q);
         if (BY_NONE != sorter)
         {   
            need_account = need_account || sql_sort_sort_need_account(sorter);
         }
      }
   }
   return need_account;
}

/* =========================================================== */
/* does sorting require a reference to this particular table? */

static gboolean 
sql_sort_sort_need_entry (sort_type_t sort_type)
{
   gboolean need_entry = FALSE;
   ENTER ("sort type=%d", sort_type);

   switch (sort_type)
   {
      case BY_STANDARD:
      case BY_ACCOUNT_FULL_NAME:
      case BY_ACCOUNT_CODE:
      case BY_CORR_ACCOUNT_FULL_NAME:
      case BY_CORR_ACCOUNT_CODE:
      case BY_DATE:
      case BY_DATE_ROUNDED:
      case BY_DATE_ENTERED:
      case BY_DATE_ENTERED_ROUNDED:
      case BY_DESC:
      case BY_NUM:
      case BY_NONE:
         break;
      case BY_AMOUNT:
      case BY_DATE_RECONCILED:
      case BY_DATE_RECONCILED_ROUNDED:
      case BY_MEMO:
      case BY_RECONCILE:
         need_entry = TRUE;
         break;
      default:
         PERR ("unknown sort type %d", sort_type);
         break;
   }

   return need_entry;
}
 
static gboolean 
sql_sort_need_entry (Query *q)
{
   gboolean need_entry = FALSE;

   sort_type_t sorter;
   sorter = xaccQueryGetPrimarySortOrder(q);
   if (BY_NONE != sorter)
   {
      need_entry = sql_sort_sort_need_entry(sorter);
      sorter = xaccQueryGetSecondarySortOrder(q);
      if (BY_NONE != sorter)
      {
         need_entry = need_entry || sql_sort_sort_need_entry(sorter);
         sorter = xaccQueryGetTertiarySortOrder(q);
         if (BY_NONE != sorter)
         {   
            need_entry = need_entry || sql_sort_sort_need_entry(sorter);
         }
      }
   }
   return need_entry;
}

/* =========================================================== */
/* Macro for PD_STRING query types
 * Note that postgres supports both case-sensitive and 
 * case-insensitve string searches, and it also supports 
 * regex!  yahooo! 
 */

#define STRING_TERM(fieldname)                                \
{                                                             \
   const char *tmp;                                           \
                                                              \
   if (0 == pd->str.sense)                                    \
   {                                                          \
      sq->pq = stpcpy (sq->pq, "NOT (");                      \
   }                                                          \
   if (pd->str.use_regexp || !pd->str.case_sens)              \
     sq->pq = stpcpy(sq->pq, fieldname " ~");                 \
   else                                                       \
     sq->pq = stpcpy(sq->pq, fieldname " =");                 \
   if (0 == pd->str.case_sens) {                              \
      sq->pq = stpcpy(sq->pq, "*");                           \
   }                                                          \
   sq->pq = stpcpy(sq->pq, " '");                             \
   tmp = sqlEscapeString (sq->escape, pd->str.matchstring);   \
   sq->pq = stpcpy(sq->pq, tmp);                              \
   sq->pq = stpcpy(sq->pq, "'");                              \
   if (0 == pd->str.sense)                                    \
   {                                                          \
      sq->pq = stpcpy (sq->pq, ") ");                         \
   }                                                          \
}

/* =========================================================== */
/* Macro for PD_AMOUNT type terms.  The logic used here in the
 * SQL exactly matches that used in the Query.c code.  If
 * that code is incorrect or has changed, then the code below is 
 * broken as well. 
 */

#define AMOUNT_TERM(fieldname,comtable)                              \
{                                                                    \
   if (0 == pd->amount.sense)                                        \
   {                                                                 \
      sq->pq = stpcpy (sq->pq, "NOT (");                             \
   }                                                                 \
   switch(pd->amount.amt_sgn)                                        \
   {                                                                 \
      case AMT_SGN_MATCH_CREDIT:                                     \
         sq->pq = stpcpy(sq->pq, fieldname " <= 0 AND ");            \
         break;                                                      \
      case AMT_SGN_MATCH_DEBIT:                                      \
         sq->pq = stpcpy(sq->pq, fieldname " >= 0 AND ");            \
         break;                                                      \
      default:                                                       \
         break;                                                      \
   }                                                                 \
   switch(pd->amount.how)                                            \
   {                                                                 \
      case AMT_MATCH_ATLEAST:                                        \
         sq->pq = stpcpy(sq->pq,                                     \
            "abs(" fieldname ") >= "comtable".fraction * float8");   \
         sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); \
         break;                                                      \
      case AMT_MATCH_ATMOST:                                         \
         sq->pq = stpcpy(sq->pq,                                     \
            "abs(" fieldname ") <= "comtable".fraction * float8");   \
         sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); \
         break;                                                      \
      case AMT_MATCH_EXACTLY:                                        \
         sq->pq = stpcpy(sq->pq,                                     \
            "abs(abs(" fieldname ") - abs("comtable".fraction * float8"); \
         sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); \
         sq->pq = stpcpy(sq->pq, ")) < 1");                          \
         break;                                                      \
   }                                                                 \
   if (0 == pd->amount.sense)                                        \
   {                                                                 \
      sq->pq = stpcpy (sq->pq, ") ");                                \
   }                                                                 \
}

/* =========================================================== */
/* Macro for PR_CLEARED term */

#define CLR_TERM(howie,flagchar)                                  \
{                                                                 \
   if (pd->cleared.how & howie)                                   \
   {                                                              \
      if (got_one)                                                \
      {                                                           \
         sq->pq = stpcpy(sq->pq, "OR ");                          \
      }                                                           \
      sq->pq = stpcpy(sq->pq, "gncEntry.reconciled = '");         \
      *(sq->pq) = flagchar;  (sq->pq) ++;                         \
      sq->pq = stpcpy(sq->pq, "' ");                              \
      got_one = 1;                                                \
   }                                                              \
}

/* =========================================================== */
static const char *
kvp_table_name (kvp_value_t value_t)
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
      PWARN ("kvp value not supported");
      return NULL;
  }
}

static char *
kvp_path_name (GSList *path)
{
  GString *s = g_string_new (NULL);
  char *name;

  for ( ; path; path = path->next)
  {
    g_string_append_c (s, '/');
    g_string_append (s, path->data);
  }

  name = s->str;
  g_string_free (s, FALSE);

  return name;
}

static const char *
kvp_op_name (kvp_match_t how)
{
  switch (how)
  {
    case KVP_MATCH_LT:
      return " < ";

    case KVP_MATCH_LTE:
      return " <= ";

    case KVP_MATCH_EQ:
      return " = ";

    case KVP_MATCH_GTE:
      return " >= ";

    case KVP_MATCH_GT:
      return " > ";

    default:
      return NULL;
  }
}

static char *
kvp_left_operand (kvp_value *value)
{
  kvp_value_t value_t;
  const char *kvptable;

  g_return_val_if_fail (value, NULL);

  value_t = kvp_value_get_type (value);

  kvptable = kvp_table_name (value_t);

  switch (value_t)
  {
    case KVP_TYPE_GINT64:
    case KVP_TYPE_DOUBLE:
    case KVP_TYPE_GUID:
    case KVP_TYPE_TIMESPEC:
    case KVP_TYPE_STRING:
      return g_strdup_printf ("%s.data", kvptable);

    case KVP_TYPE_NUMERIC: {
      gnc_numeric n = kvp_value_get_numeric (value);
      return g_strdup_printf ("(%lld::int8 * %s.num::int8)",
                              (long long int) n.denom, kvptable);
    }

    default:
      return NULL;
  }
}

static char *
kvp_right_operand (sqlQuery *sq, kvp_value *value)
{
  kvp_value_t value_t;
  const char *kvptable;

  g_return_val_if_fail (value, NULL);

  value_t = kvp_value_get_type (value);

  kvptable = kvp_table_name (value_t);

  switch (value_t)
  {
    case KVP_TYPE_GINT64:
      return g_strdup_printf ("%lld",
                              (long long int) kvp_value_get_gint64 (value));

    case KVP_TYPE_DOUBLE:
      return g_strdup_printf (SQL_DBL_FMT, kvp_value_get_double (value));

    case KVP_TYPE_GUID: {
      char *guid = guid_to_string (kvp_value_get_guid (value));
      char *s = g_strdup_printf ("'%s'", guid);
      g_free (guid);
      return s;
    }

    case KVP_TYPE_TIMESPEC: {
      char s[80];
      gnc_timespec_to_iso8601_buff (kvp_value_get_timespec (value), s);
      return g_strdup_printf ("'%s'", s);
    }

    case KVP_TYPE_STRING: {
      const char *s = sqlEscapeString (sq->escape,
                                       kvp_value_get_string (value));
      return g_strdup_printf ("'%s'", s);
    }

    case KVP_TYPE_NUMERIC: {
      gnc_numeric n = kvp_value_get_numeric (value);
      return g_strdup_printf ("(%lld::int8 * %s.denom::int8)",
                              (long long int) n.num, kvptable);
    }

    default:
      return NULL;
  }
}

static void
add_kvp_clause (sqlQuery *sq, const char *kvptable, const char *entity_table,
                const char *left, const char *op, const char *right)
{
  sq->pq = stpcpy (sq->pq, " ( ");

  sq->pq = stpcpy (sq->pq, "gncPathCache.ipath = ");
  sq->pq = stpcpy (sq->pq, kvptable);
  sq->pq = stpcpy (sq->pq, ".ipath");

  sq->pq = stpcpy (sq->pq, " AND ");

  sq->pq = stpcpy (sq->pq, entity_table);
  sq->pq = stpcpy (sq->pq, ".iguid");
  sq->pq = stpcpy (sq->pq, " = ");
  sq->pq = stpcpy (sq->pq, kvptable);
  sq->pq = stpcpy (sq->pq, ".iguid");

  sq->pq = stpcpy (sq->pq, " AND ");

  sq->pq = stpcpy (sq->pq, left);
  sq->pq = stpcpy (sq->pq, op);
  sq->pq = stpcpy (sq->pq, right);

  sq->pq = stpcpy (sq->pq, " ) ");
}

static void
sqlQuery_kvp_build (sqlQuery *sq, KVPPredicateData *kpd)
{
  kvp_value_t value_t;
  const char *kvptable;
  const char *op;
  GList *list;
  GList *node;
  char *right;
  char *left;
  char *path;

  g_return_if_fail (sq && kpd && kpd->path && kpd->value);

  if (!(kpd->where & (KVP_MATCH_SPLIT | KVP_MATCH_TRANS | KVP_MATCH_ACCOUNT)))
    return;

  value_t = kvp_value_get_type (kpd->value);

  if (value_t == KVP_TYPE_GUID && kpd->how != KVP_MATCH_EQ)
  {
    PWARN ("guid non-equality comparison not supported");
    return;
  }

  kvptable = kvp_table_name (value_t);
  if (!kvptable)
    return;

  path = kvp_path_name (kpd->path);
  op = kvp_op_name (kpd->how);
  left = kvp_left_operand (kpd->value);
  right = kvp_right_operand (sq, kpd->value);

  list = NULL;
  if (kpd->where & KVP_MATCH_SPLIT)
    list = g_list_prepend (list, "gncEntry");
  if (kpd->where & KVP_MATCH_TRANS)
    list = g_list_prepend (list, "gncTransaction");
  if (kpd->where & KVP_MATCH_ACCOUNT)
    list = g_list_prepend (list, "gncAccount");

  if (!kpd->sense)
    sq->pq = stpcpy (sq->pq, "NOT ");

  sq->pq = stpcpy (sq->pq,
                   " EXISTS ( SELECT true "
                   "          WHERE ");

  sq->pq = stpcpy (sq->pq, "gncPathCache.path = '");
  sq->pq = stpcpy (sq->pq, sqlEscapeString (sq->escape, path));
  sq->pq = stpcpy (sq->pq, "'");

  for (node = list; node; node = node->next)
  {
    sq->pq = stpcpy (sq->pq, " AND ");
    add_kvp_clause (sq, kvptable, node->data, left, op, right);
  }

  sq->pq = stpcpy (sq->pq, " ) ");

  g_free (path);
  g_free (left);
  g_free (right);
  g_list_free (list);
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
sqlQuery_build (sqlQuery *sq, Query *q)
{
   GList *il, *jl, *qterms, *andterms;
   QueryTerm *qt;
   PredicateData *pd;
   GList *tables = NULL;
   int more_or = 0;
   int more_and = 0;
   int max_rows;
   gboolean need_account_commodity = FALSE;
   gboolean need_trans_commodity = FALSE;
   gboolean need_account = FALSE;
   gboolean need_entry = FALSE;
   sort_type_t sorter;

   if (!sq || !q) return NULL;

   /* Determine whether the query will need to reference certain
    * tables. See note above for details. */
   qterms = xaccQueryGetTerms (q);

   for (il=qterms; il; il=il->next)
   {
      /* andterms is GList of query terms that must be anded */
      andterms = il->data;

      for (jl=andterms; jl; jl=jl->next)
      {
         qt = (QueryTerm *)jl->data;
         pd = &qt->data;

         switch (pd->base.term_type) 
         {
            case PR_BALANCE:
            case PR_DATE:
            case PR_DESC:
            case PR_MISC:
            case PR_NUM:
               break;

            case PR_ACCOUNT: 
            case PR_ACTION:
            case PR_CLEARED:
            case PR_MEMO:
            case PR_PRICE: 
               need_entry = TRUE;
               break;

            case PR_BOOK: 
               /* hack alert FIXME this sucks, since *all* queries will 
                * have a PR_BOOK term in them, which is going to cause 
                * this big old join, which is going to kill performance.
                * Maybe we need to put the book in the transaction, for 
                * performance ?? 
                */
               need_account = TRUE;
               break;

            case PR_VALUE:
               need_entry = TRUE;
               need_trans_commodity = TRUE;
               break;

            case PR_GUID:
               if (!safe_strcmp (pd->guid.id_type, GNC_ID_ACCOUNT))
               {
                  need_account = TRUE;
               }
               else if (!safe_strcmp (pd->guid.id_type, GNC_ID_SPLIT))
               {
                  need_entry = TRUE;
               }
               break;

            case PR_KVP:
               if (pd->kvp.where & KVP_MATCH_SPLIT)
               {
                  need_entry = TRUE;
               }
               if (pd->kvp.where & KVP_MATCH_ACCOUNT)
               {
                  need_account = TRUE;
               }
               break;

            case PR_SHRS: 
               need_entry = TRUE;
               need_account_commodity = TRUE;
               need_account = TRUE;
               break;

            default:
               break;
         }
      }
   }

   /* determine whether the requested sort order needs these tables */
   need_entry = need_entry || sql_sort_need_entry (q);
   need_account = need_account || sql_sort_need_account (q);

   /* reset the buffer pointers */
   sq->pq = sq->q_base;
   sq->pq = stpcpy(sq->pq,
                   "SELECT DISTINCT gncTransaction.* ");

   /* For SELECT DISTINCT, ORDER BY expressions must appear in target list */
   sq->pq = sql_sort_distinct (sq->pq, xaccQueryGetPrimarySortOrder(q));
   sq->pq = sql_sort_distinct (sq->pq, xaccQueryGetSecondarySortOrder(q));
   sq->pq = sql_sort_distinct (sq->pq, xaccQueryGetTertiarySortOrder(q));

   /* add needed explicit tables. postgres can figure out the rest. */
   if (need_account_commodity)
     tables = g_list_prepend (tables, "gncCommodity account_com");

   if (need_trans_commodity)
     tables = g_list_prepend (tables, "gncCommodity trans_com");

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

     g_list_free (tables);
   }

   sq->pq = stpcpy(sq->pq, " WHERE ");

   if (need_entry || need_account)
   {
      sq->pq = stpcpy(sq->pq, 
                      " gncEntry.transGuid = gncTransaction.transGuid AND ");
   }

   if (need_account)
   {
      sq->pq = stpcpy(sq->pq, 
                      " gncEntry.accountGuid = gncAccount.accountGuid AND ");
   }

   sq->pq = stpcpy(sq->pq, "  ( ");

   /* qterms is a list of lists: outer list is a set of terms 
    * that must be OR'ed together, inner lists are a set of terms 
    * that must be anded.  Out strategy is to build the sql query 
    * of the AND terms first, and OR these together ...
    */
   for (il=qterms; il; il=il->next)
   {
      /* andterms is GList of query terms that must be anded */
      andterms = il->data;

      /* if there are andterms, open a brace */
      if (andterms) 
      {
         /* concatenate additional OR terms */
         if (more_or) sq->pq = stpcpy (sq->pq, " OR ");
         more_or = 1;
         sq->pq = stpcpy(sq->pq, "(");
      }

      more_and = 0;
      for (jl=andterms; jl; jl=jl->next)
      {
         /* concatencate more terms together */
         if(more_and) sq->pq = stpcpy(sq->pq, " AND ");
         more_and = 1;

         qt = (QueryTerm *)jl->data;
         pd = &qt->data;
         switch (pd->base.term_type) 
         {
            case PR_ACCOUNT: 
            {
               AccountList *acct;

               PINFO("term is PR_ACCOUNT");

               if (!pd->acct.sense)
               {
                 sq->pq = stpcpy (sq->pq, "NOT ");
               }

               sq->pq = stpcpy(sq->pq, "(");

               for (acct = pd->acct.account_guids; acct; acct=acct->next)
               {
                 switch (pd->acct.how)
                 {
                   case ACCT_MATCH_NONE:
                     sq->pq = stpcpy (sq->pq, "NOT ");
                     /* fall through */

                   case ACCT_MATCH_ANY:
                     sq->pq = stpcpy(sq->pq, "gncEntry.accountGuid='");
                     sq->pq = guid_to_string_buff ((GUID*) acct->data, sq->pq);
                     sq->pq = stpcpy(sq->pq, "'");
                     break;

                   case ACCT_MATCH_ALL:
                     sq->pq = stpcpy (sq->pq,
                                      " EXISTS ( SELECT true FROM gncEntry e"
                                      "          WHERE "
                                      "e.transGuid = gncTransaction.transGuid"
                                      " AND "
                                      "e.accountGuid='");
                     sq->pq = guid_to_string_buff ((GUID*) acct->data, sq->pq);
                     sq->pq = stpcpy(sq->pq, "')");

                     break;

                   default:
                     PERR ("unexpected account match type: %d",pd->acct.how);
                     break;
                 }

                 if (acct->next)
                 {
                   switch (pd->acct.how)
                   {
                     case ACCT_MATCH_ANY:
                       sq->pq = stpcpy(sq->pq, " OR ");
                       break;

                     case ACCT_MATCH_ALL:
                     case ACCT_MATCH_NONE:
                       sq->pq = stpcpy(sq->pq, " AND ");
                       break;

                     default:
                       PERR ("unexpected account match type: %d",pd->acct.how);
                       break;
                   }
                 }
               }

               sq->pq = stpcpy(sq->pq, ")");

               break;
            }

            case PR_ACTION:
               PINFO("term is PR_ACTION");
               STRING_TERM ("gncEntry.action");
               break;

            case PR_BALANCE:
            {
               PINFO("term is PR_BALANCE");
               PWARN("PR_BALANCE query term not properly implemented");
               if (0 == pd->balance.sense)
               {
                  sq->pq = stpcpy (sq->pq, "NOT (");
               }
               if (pd->balance.how & BALANCE_BALANCED) 
               {
                  sq->pq = stpcpy(sq->pq, "TRUE ");
               }
               else 
               {
                  sq->pq = stpcpy(sq->pq, "FALSE ");
               }
               if (0 == pd->balance.sense)
               {
                  sq->pq = stpcpy (sq->pq, ") ");
               }
               break;
            }

            case PR_BOOK: 
            {
               BookList *book;

               PINFO("term is PR_BOOK");

               if (!pd->book.sense)
               {
                 sq->pq = stpcpy (sq->pq, "NOT ");
               }

               sq->pq = stpcpy(sq->pq, "(");

               for (book = pd->book.book_guids; book; book=book->next)
               {
                 switch (pd->book.how)
                 {
                   case BOOK_MATCH_NONE:
                     sq->pq = stpcpy (sq->pq, "NOT ");
                     /* fall through */

                   case BOOK_MATCH_ANY:
                     sq->pq = stpcpy(sq->pq, "gncAccount.bookGuid='");
                     sq->pq = guid_to_string_buff ((GUID*) book->data, sq->pq);
                     sq->pq = stpcpy(sq->pq, "'");
                     break;

                   default:
                     PERR ("unexpected book match type: %d",pd->book.how);
                     break;
                 }

                 if (book->next)
                 {
                   switch (pd->book.how)
                   {
                     case BOOK_MATCH_ANY:
                       sq->pq = stpcpy(sq->pq, " OR ");
                       break;

                     case BOOK_MATCH_NONE:
                       sq->pq = stpcpy(sq->pq, " AND ");
                       break;

                     default:
                       PERR ("unexpected book match type: %d",pd->book.how);
                       break;
                   }
                 }
               }

               sq->pq = stpcpy(sq->pq, ")");

               break;
            }

            case PR_CLEARED:
            {
               int got_one = 0;
               PINFO("term is PR_CLEARED");
               if (0 == pd->cleared.sense)
               {
                  sq->pq = stpcpy (sq->pq, "NOT ");
               }
               sq->pq = stpcpy (sq->pq, "(");

               CLR_TERM (CLEARED_NO, NREC);
               CLR_TERM (CLEARED_CLEARED, CREC);
               CLR_TERM (CLEARED_RECONCILED, YREC);
               CLR_TERM (CLEARED_FROZEN, FREC);
               CLR_TERM (CLEARED_VOIDED, VREC);

               sq->pq = stpcpy (sq->pq, ") ");
               break;
            }

            case PR_DATE:
            {
               PINFO("term is PR_DATE");
               if (0 == pd->date.sense)
               {
                  sq->pq = stpcpy (sq->pq, "NOT (");
               }
               if (pd->date.use_start)
               {
                  sq->pq = stpcpy(sq->pq, "gncTransaction.date_posted >= '");
                  sq->pq = gnc_timespec_to_iso8601_buff (pd->date.start,
                                                         sq->pq);
                  sq->pq = stpcpy(sq->pq, "' ");
               }
               if (pd->date.use_end)
               {
                  if (pd->date.use_start) 
                  {
                     sq->pq = stpcpy(sq->pq, "AND ");
                  }
                  sq->pq = stpcpy(sq->pq, "gncTransaction.date_posted <= '");
                  sq->pq = gnc_timespec_to_iso8601_buff (pd->date.end, sq->pq);
                  sq->pq = stpcpy(sq->pq, "' ");
               }
               if (!pd->date.use_start && !pd->date.use_end) 
               {
                  sq->pq = stpcpy(sq->pq, "TRUE ");
               }
               if (0 == pd->date.sense)
               {
                  sq->pq = stpcpy (sq->pq, ") ");
               }
               break;
            }

            case PR_DESC:
               PINFO("term is PR_DESC");
               STRING_TERM ("gncTransaction.description");
               break;

            case PR_GUID:
            {
               PINFO("term is PR_GUID");
               if (0 == pd->guid.sense)
               {
                  sq->pq = stpcpy (sq->pq, "NOT ");
               }

               sq->pq = stpcpy (sq->pq, " (");

               if (pd->guid.id_type == GNC_ID_NONE ||
                   !safe_strcmp (pd->guid.id_type, GNC_ID_NULL))
               {
                   sq->pq = stpcpy(sq->pq, "FALSE ");
               }
               else if (!safe_strcmp (pd->guid.id_type, GNC_ID_ACCOUNT))
               {
                   sq->pq = stpcpy(sq->pq, "gncAccount.accountGuid = '");
                   sq->pq = guid_to_string_buff (&pd->guid.guid, sq->pq);
                   sq->pq = stpcpy(sq->pq, "' ");
               }
               else if (!safe_strcmp (pd->guid.id_type, GNC_ID_TRANS))
               {
                   sq->pq = stpcpy(sq->pq, "gncTransaction.transGuid = '");
                   sq->pq = guid_to_string_buff (&pd->guid.guid, sq->pq);
                   sq->pq = stpcpy(sq->pq, "' ");
               }
               else if (!safe_strcmp (pd->guid.id_type, GNC_ID_SPLIT))
               {
                   sq->pq = stpcpy(sq->pq, "gncEntry.entryGuid = '");
                   sq->pq = guid_to_string_buff (&pd->guid.guid, sq->pq);
                   sq->pq = stpcpy(sq->pq, "' ");
               }

               sq->pq = stpcpy (sq->pq, ") ");

               break;
            }

            case PR_KVP:
               PINFO("term is PR_KVP");
               sqlQuery_kvp_build (sq, &pd->kvp);
               break;

            case PR_MEMO:
               PINFO("term is PR_MEMO");
               STRING_TERM ("gncEntry.memo");
               break;

            case PR_MISC:
               PINFO("term is PR_MISC");
               sq->pq = stpcpy(sq->pq, "TRUE ");
               break;

            case PR_NUM:
               PINFO("term is PR_NUM");
               STRING_TERM ("gncTransaction.num");
               break;

            case PR_PRICE: {
               PINFO("term is PR_PRICE");

               if (0 == pd->amount.sense)
               {
                  sq->pq = stpcpy (sq->pq, "NOT (");
               }
               switch(pd->amount.amt_sgn) 
               {
                  case AMT_SGN_MATCH_CREDIT:
                     sq->pq = stpcpy(sq->pq, "gncEntry.value / gncEntry.amount <= 0 AND "); 
                     break;
                  case AMT_SGN_MATCH_DEBIT:
                     sq->pq = stpcpy(sq->pq, "gncEntry.value / gncEntry.amount >= 0 AND "); 
                     break;
                  default:
                     break;
               }
               switch(pd->amount.how) 
               {
                  case AMT_MATCH_ATLEAST:
                     sq->pq = stpcpy(sq->pq, 
                        "gncHelperPrVal(gncEntry) >= gncHelperPrAmt(gncEntry) * float8"); 
                     sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); 
                     break;
                  case AMT_MATCH_ATMOST:
                     sq->pq = stpcpy(sq->pq, 
                        "gncHelperPrVal(gncEntry) <= gncHelperPrAmt(gncEntry) * float8"); 
                     sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); 
                     break;
                  case AMT_MATCH_EXACTLY:
                     sq->pq = stpcpy(sq->pq, 
                        "abs(gncHelperPrVal(gncEntry) -  gncHelperPrAmt(gncEntry) * float8"); 
                     sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); 
                     sq->pq = stpcpy(sq->pq, ") < 1");
                     break;
               }
               if (0 == pd->amount.sense)
               {
                  sq->pq = stpcpy (sq->pq, ") ");
               }
               break;
            }

            case PR_SHRS: {
               PINFO("term is PR_SHRS");
               sq->pq = stpcpy(sq->pq, 
                     "gncAccount.commodity = account_com.commodity AND ");
               AMOUNT_TERM ("gncEntry.amount","account_com");
               break;
            }

            case PR_VALUE:
            {
               PINFO("term is PR_VALUE");
               sq->pq = stpcpy(sq->pq, 
                     "gncTransaction.currency = trans_com.commodity AND ");
               AMOUNT_TERM ("gncEntry.value","trans_com");
               break;
            }

            default:
               PERR ("unknown query term type %d", pd->base.term_type);
               more_and = 0;
               break;
         }
      }

      /* if there were and terms, close the brace */
      if (il->data) sq->pq = stpcpy(sq->pq, ")");
   }

   sq->pq = stpcpy(sq->pq, ") ");

   /* ---------------------------------------------------- */
   /* implement sorting order as well; bad sorts lead to bad data
    * if the limit is set to a finite number of rows. 
    */
   sorter = xaccQueryGetPrimarySortOrder(q);
   if (BY_NONE != sorter)
   {
      sq->pq = stpcpy(sq->pq, "ORDER BY ");
      sq->pq = sql_sort_order (sq->pq, sorter,
                               xaccQueryGetSortPrimaryIncreasing (q));

      sorter = xaccQueryGetSecondarySortOrder(q);
      if (BY_NONE != sorter)
      {
         sq->pq = stpcpy(sq->pq, ", ");
         sq->pq = sql_sort_order (sq->pq, sorter,
                                  xaccQueryGetSortSecondaryIncreasing (q));

         sorter = xaccQueryGetTertiarySortOrder(q);
         if (BY_NONE != sorter)
         {   
            sq->pq = stpcpy(sq->pq, ", ");
            sq->pq = sql_sort_order (sq->pq, sorter,
                                     xaccQueryGetSortTertiaryIncreasing (q));
         }
      }
   }

   /* ---------------------------------------------------- */
   /* limit the query result to a finite number of rows */
   max_rows = xaccQueryGetMaxSplits (q);
   if (0 <= max_rows)
   {
      sq->pq = stpcpy(sq->pq, " LIMIT ");
      sq->pq += snprintf (sq->pq, 30, "%d", max_rows);
   }

   sq->pq = stpcpy(sq->pq, ";");
   
   return sq->q_base;
}


/* ========================== END OF FILE ==================== */
