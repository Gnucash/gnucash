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
#include <glib.h>
#include <string.h>

#include "escape.h"
#include "gnc-engine-util.h"
#include "gncquery.h"

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
/* Macro for PD_STRING query types
 * Note that postgres supports both case-sensitive and 
 * case-insensitve string searches, and it also supports 
 * regex!  yahooo! 
 */

#define STRING_TERM(fieldname)				\
{							\
   const char *tmp;					\
							\
   if (0 == pd->str.sense)				\
   {							\
      sq->pq = stpcpy (sq->pq, "NOT (");		\
   }							\
   sq->pq = stpcpy(sq->pq, fieldname " ~");		\
   if (0 == pd->str.case_sens) {			\
      sq->pq = stpcpy(sq->pq, "*");			\
   }							\
   sq->pq = stpcpy(sq->pq, " '");			\
   tmp = sqlEscapeString (sq->escape, pd->str.matchstring); \
   sq->pq = stpcpy(sq->pq, tmp);			\
   sq->pq = stpcpy(sq->pq, "'");			\
   if (0 == pd->str.sense)				\
   {							\
      sq->pq = stpcpy (sq->pq, ") ");			\
   }							\
}

/* =========================================================== */
/* Macro for PD_AMOUNT type terms.  The logic used here in the
 * SQL exactly matches that used in the Query.c code.  If
 * that code is incorrect or has changed, then the code below is 
 * broken as well. 
 */

#define AMOUNT_TERM(fieldname)					\
{								\
   if (0 == pd->amount.sense)					\
   {								\
      sq->pq = stpcpy (sq->pq, "NOT (");			\
   }								\
   switch(pd->amount.amt_sgn) 					\
   {								\
      case AMT_SGN_MATCH_CREDIT:				\
         sq->pq = stpcpy(sq->pq, fieldname " <= 0 AND "); 	\
         break;							\
      case AMT_SGN_MATCH_DEBIT:					\
         sq->pq = stpcpy(sq->pq, fieldname " >= 0 AND "); 	\
         break;							\
      default:							\
         break;							\
   }								\
   switch(pd->amount.how) 					\
   {								\
      case AMT_MATCH_ATLEAST:					\
         sq->pq = stpcpy(sq->pq, 				\
            "abs(" fieldname ") >= gncCommodity.fraction * float8"); \
         sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); \
         break;							\
      case AMT_MATCH_ATMOST:					\
         sq->pq = stpcpy(sq->pq, 				\
            "abs(" fieldname ") <= gncCommodity.fraction * float8"); \
         sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); \
         break;							\
      case AMT_MATCH_EXACTLY:					\
         sq->pq = stpcpy(sq->pq, 				\
            "abs(abs(" fieldname ") - gncCommodity.fraction * float8"); \
         sq->pq += sprintf (sq->pq, "(%22.18g)", pd->amount.amount); \
         sq->pq = stpcpy(sq->pq, ") < 1");			\
         break;							\
   }								\
   if (0 == pd->amount.sense)					\
   {								\
      sq->pq = stpcpy (sq->pq, ") ");				\
   }								\
}

/* =========================================================== */
/* Macro for PR_CLEARED term */

#define CLR_TERM(howie,flagchar)				\
{								\
   if (pd->cleared.how & howie)					\
   {								\
      if (got_one)						\
      {								\
         sq->pq = stpcpy(sq->pq, "OR ");			\
      }								\
      sq->pq = stpcpy(sq->pq, "gncEntry.reconciled = '");	\
      *(sq->pq) = flagchar;  (sq->pq) ++;			\
      sq->pq = stpcpy(sq->pq, "' ");				\
      got_one = 1;						\
   }								\
}

/* =========================================================== */

const char *
sqlQuery_build (sqlQuery *sq, Query *q)
{
   GList *il, *jl, *qterms, *andterms;
   QueryTerm *qt;
   PredicateData *pd;
   int more_or = 0;
   int more_and = 0;
   int max_rows;
   gboolean need_account = FALSE;
   gboolean need_commodity = FALSE;
   sort_type_t sorter;

   if (!sq || !q) return NULL;

   /* determine whther the query will need to reference the account
    * or commodity tables.  If it doesn't need them, then we can gain
    * a significant performance improvement by not specifying them.
    * The exact reason why this affect performance is a bit of a 
    * mystery to me ... */
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
            case PR_ACCOUNT: 
            case PR_ACTION:
            case PR_BALANCE:
            case PR_CLEARED:
            case PR_DATE:
            case PR_DESC:
            case PR_MEMO:
            case PR_MISC:
            case PR_NUM:
            case PR_PRICE: 
               break;
            case PR_AMOUNT:
               need_commodity = TRUE;
               break;
            case PR_GUID:
               switch (xaccGUIDType (&pd->guid.guid))
               {
                  case GNC_ID_ACCOUNT:
                     need_account = TRUE;
                     break;
                  case GNC_ID_NONE:
                  case GNC_ID_NULL:
                  case GNC_ID_TRANS:
                  case GNC_ID_SPLIT:
                  default:
                     break;
               }
               break;
            case PR_SHRS: 
               need_commodity = TRUE;
               need_account = TRUE;
               break;
            default:
               break;
         }
      }
   }
   
   /* reset the buffer pointers */
   sq->pq = sq->q_base;
   sq->pq = stpcpy(sq->pq, 
               "SELECT DISTINCT gncEntry.transGuid ");

   /* For SELECT DISTINCT, ORDER BY expressions must appear in target list */
   sq->pq = sql_sort_distinct (sq->pq, xaccQueryGetPrimarySortOrder(q));
   sq->pq = sql_sort_distinct (sq->pq, xaccQueryGetSecondarySortOrder(q));
   sq->pq = sql_sort_distinct (sq->pq, xaccQueryGetTertiarySortOrder(q));

   sq->pq = stpcpy(sq->pq, "  FROM gncEntry, gncTransaction");

   /* add additional search tables, as needed for performance */
   if (need_account)
   {
      sq->pq = stpcpy(sq->pq, ", gncAccount");
   }
   if (need_commodity)
   {
      sq->pq = stpcpy(sq->pq, ", gncCommodity");
   }
   sq->pq = stpcpy(sq->pq, 
           "  WHERE gncEntry.transGuid = gncTransaction.transGuid AND ( ");


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
               int got_more = 0;
               GList *acct;

               PINFO("term is PR_ACCOUNT");

               for (acct = pd->acct.account_guids; acct; acct=acct->next)
               {
                  if (got_more) sq->pq = stpcpy(sq->pq, " AND ");
                  got_more = 1;

                  if ((0 == pd->acct.sense && ACCT_MATCH_NONE != pd->acct.how) ||
                      (1 == pd->acct.sense && ACCT_MATCH_NONE == pd->acct.how))
                  {
                     sq->pq = stpcpy (sq->pq, "NOT ");
                  }
                  sq->pq = stpcpy(sq->pq, "gncEntry.accountGuid='");
                  sq->pq = guid_to_string_buff ((GUID*) acct->data, sq->pq);
                  sq->pq = stpcpy(sq->pq, "'");
               }
               break;
            }

            case PR_ACTION:
               PINFO("term is PR_ACTION");
               STRING_TERM ("gncEntry.action");
               break;

            case PR_AMOUNT:
            {
               PINFO("term is PR_AMOUNT");
               sq->pq = stpcpy(sq->pq, 
                     "gncTransaction.currency = gncCommodity.commodity AND ");
               AMOUNT_TERM ("gncEntry.value");
               break;
            }

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
                  sq->pq = gnc_timespec_to_iso8601_buff (pd->date.start, sq->pq);
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
                  sq->pq = stpcpy (sq->pq, "NOT (");
               }
               switch (xaccGUIDType (&pd->guid.guid))
               {
                  case GNC_ID_NONE:
                  case GNC_ID_NULL:
                  default:
                     sq->pq = stpcpy(sq->pq, "FALSE ");
                     break;
              
                  case GNC_ID_ACCOUNT:
                     sq->pq = stpcpy(sq->pq, "gncAccount.accountGuid = '");
                     sq->pq = guid_to_string_buff (&pd->guid.guid, sq->pq);
                     sq->pq = stpcpy(sq->pq, "' ");
                     break;
              
                  case GNC_ID_TRANS:
                     sq->pq = stpcpy(sq->pq, "gncTransaction.transGuid = '");
                     sq->pq = guid_to_string_buff (&pd->guid.guid, sq->pq);
                     sq->pq = stpcpy(sq->pq, "' ");
                     break;
              
                  case GNC_ID_SPLIT:
                     sq->pq = stpcpy(sq->pq, "gncEntry.entryGuid = '");
                     sq->pq = guid_to_string_buff (&pd->guid.guid, sq->pq);
                     sq->pq = stpcpy(sq->pq, "' ");
                     break;
               }

               if (0 == pd->guid.sense)
               {
                  sq->pq = stpcpy (sq->pq, ") ");
               }
               break;
            }

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
                     "gncEntry.accountGuid = gncAccount.accountGuid AND "
                     "gncAccount.commodity = gncCommodity.commodity AND ");
               AMOUNT_TERM ("gncEntry.amount");
               break;
            }

            default:
               PERR ("unkown query term type %d", pd->base.term_type);
         }
      }

      /* if there were and terms, close the brace */
      if (il->data) sq->pq = stpcpy(sq->pq, ")");
   }

   sq->pq = stpcpy(sq->pq, ")");

   /* ---------------------------------------------------- */
   /* implement sorting order as well; bad sorts lead to bad data
    * if the limit is set to a finite number of rows. 
    */
   sorter = xaccQueryGetPrimarySortOrder(q);
   if (BY_NONE != sorter)
   {
      sq->pq = stpcpy(sq->pq, "ORDER BY ");
      sq->pq = sql_sort_order (sq->pq, sorter, xaccQueryGetSortPrimaryIncreasing (q));

      sorter = xaccQueryGetSecondarySortOrder(q);
      if (BY_NONE != sorter)
      {
         sq->pq = stpcpy(sq->pq, ", ");
         sq->pq = sql_sort_order (sq->pq, sorter, xaccQueryGetSortSecondaryIncreasing (q));

         sorter = xaccQueryGetTertiarySortOrder(q);
         if (BY_NONE != sorter)
         {   
            sq->pq = stpcpy(sq->pq, ", ");
            sq->pq = sql_sort_order (sq->pq, sorter, xaccQueryGetSortTertiaryIncreasing (q));
         }
      }
   }

   /* ---------------------------------------------------- */
   /* limit the query result to a finite numbe of rows */
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

