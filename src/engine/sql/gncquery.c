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
/* Note that postgres supports both case-sensitive and 
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

const char *
sqlQuery_build (sqlQuery*sq, Query *q)
{
   GList *il, *jl, *qterms, *andterms;
   QueryTerm *qt;
   PredicateData *pd;
   int more_or = 0;
   int more_and = 0;
   int max_rows;

   if (!sq || !q) return NULL;

   /* reset the buffer pointers */
   sq->pq = sq->q_base;
   sq->pq = stpcpy(sq->pq, 
               "SELECT gncEntry.transGuid FROM gncEntry, gncTransaction "
               "  WHERE gncEntry.transGuid = gncTransaction.transGuid AND ( ");

   qterms = xaccQueryGetTerms (q);

   /* qterms is a list of lists: outer list is a set of terms 
    * that must be OR'ed together, inner lists are a set of terms 
    * that must be anded.  Out strategy is to build the sql query 
    * of the AND terms first, and OR these together ...
    */
   for (il=qterms; il; il=il->next)
   {
      /* andterms is GList of query terms that mustbe anded */
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
               PERR ("not implemented");
               break;
            }

            case PR_BALANCE:
               PINFO("term is PR_BALANCE");
               PERR ("not implemented");
               break;
            case PR_CLEARED:
               PINFO("term is PR_CLEARED");
               PERR ("not implemented");
               break;

            case PR_DATE:
            {
               PINFO("term is PR_DATE");
               if (0 == pd->acct.sense)
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
               if (0 == pd->acct.sense)
               {
                  sq->pq = stpcpy (sq->pq, ") ");
               }
               break;
            }

            case PR_DESC:
               PINFO("term is PR_DESC");
               STRING_TERM ("gncTransaction.description");
               break;

            case PR_MEMO:
               PINFO("term is PR_MEMO");
               STRING_TERM ("gncEntry.memo");
               break;

            case PR_MISC:
               PINFO("term is PR_MISC");
               PERR ("not implemented");
               break;

            case PR_NUM:
               PINFO("term is PR_NUM");
               STRING_TERM ("gncTransaction.num");
               break;

            case PR_PRICE:
               PINFO("term is PR_PRICE");
               PERR ("not implemented");
               break;

            case PR_SHRS:
               PINFO("term is PR_SHRS");
               PERR ("not implemented");
               break;

            default:
               PERR ("unkown query term type");
         }
      }

      /* if there were and terms, close the brace */
      if (il->data) sq->pq = stpcpy(sq->pq, ")");
   }

   sq->pq = stpcpy(sq->pq, ")");

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

