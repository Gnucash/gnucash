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

#include "gnc-engine-util.h"
#include "gncquery.h"

static short module = MOD_BACKEND;


struct _gnc_query {
   char * q_base;
   char * pq;
   size_t buflen;
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
   return sq;
}

void 
sql_Query_destroy (sqlQuery *sq)
{
   if (!sq) return;
   g_free(sq->q_base);
   g_free(sq);
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
   sq->pq = stpcpy(sq->pq, "SELECT transGuid FROM gncEntry WHERE ");

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
               char guid_str[GUID_ENCODING_LENGTH+1];

               PINFO("term is PR_ACCOUNT");

               /* hack alert --  the following code is ignoring 
                * both the 'how' and the 'sense' fields */
               for (acct = pd->acct.account_guids; acct; acct=acct->next)
               {
                  if (got_more) sq->pq = stpcpy(sq->pq, " AND ");
                  got_more = 1;

                  guid_to_string_buff ((GUID*) acct->data, guid_str);
                  sq->pq = stpcpy(sq->pq, "accountguid='");
                  sq->pq = stpcpy(sq->pq, guid_str);
                  sq->pq = stpcpy(sq->pq, "'");
               }
               break;
            }
            case PR_ACTION:
               PINFO("term is PR_ACTION");
               break;
            case PR_AMOUNT:
               PINFO("term is PR_AMOUNT");
               break;
            case PR_BALANCE:
               PINFO("term is PR_BALANCE");
               break;
            case PR_CLEARED:
               PINFO("term is PR_CLEARED");
               break;
            case PR_DATE:
               PINFO("term is PR_DATE");
               break;
            case PR_DESC:
               PINFO("term is PR_DESC");
               break;
            case PR_MEMO:
               PINFO("term is PR_MEMO");
               break;
            case PR_MISC:
               PINFO("term is PR_MISC");
               break;
            case PR_NUM:
               PINFO("term is PR_NUM");
               break;
            case PR_PRICE:
               PINFO("term is PR_PRICE");
               break;
            case PR_SHRS:
               PINFO("term is PR_SHRS");
               break;
            default:
               PERR ("unkown query term type");
         }
      }

      /* if there were and terms, close the brace */
      if (il->data) sq->pq = stpcpy(sq->pq, ")");
   }


   /* limit the query result to a finite numbe of rows */
   max_rows = xaccQueryGetMaxSplits (q);
   if (0 <= max_rows)
   {
      char buff[30];
      snprintf (buff, 30, "%d", max_rows);
      sq->pq = stpcpy(sq->pq, " LIMIT ");
      sq->pq = stpcpy(sq->pq, buff);
   }

   sq->pq = stpcpy(sq->pq, ";");
   
   return sq->q_base;
}


/* ========================== END OF FILE ==================== */

