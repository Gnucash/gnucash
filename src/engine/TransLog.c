/********************************************************************\
 * TransLog.c -- the transaction logger                             *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
 *                                                                  *
\********************************************************************/

#define _GNU_SOURCE
#include "config.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <glib.h>

#include "Account.h"
#include "AccountP.h"
#include "DateUtils.h"
#include "date.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "gnc-engine-util.h"

/*
 * The logfiles are useful for tracing, journalling, error recovery.
 * Note that the current support for journalling is at best 
 * embryonic, at worst, is dangerous by setting the wrong expectations.
 */

/* 
 * Some design philosphy that I think would be good to keep in mind:
 * (0) Simplicity and foolproofness are the over-riding design points.
 *     This is supposed to be a fail-safe safety net.   We don't want
 *     our safety net to fail because of some whiz-bang shenanigans.
 *
 * (1) Try to keep the code simple.  Want to make it simple and obvious
 *     that we are recording everything that we need to record.
 *
 * (2) Keep the printed format human readable, for the same reasons.
 * (2.a) Keep the format, simple, flat, more or less unstructured,
 *       record oriented.  This will help parsing by perl scripts.
 *       No, using a perl script to analyze a file that's supposed to
 *       be human readable is not a contradication in terms -- that's 
 *       exactly the point.
 * (2.b) Use tabs as a human freindly field separator; its also a 
 *       character that does not (should not) appear naturally anywhere 
 *       in the data, as it serves no formatting purpose in the current 
 *       GUI design.  (hack alert -- this is not currently tested for 
 *       or enforced, so this is a very unsafe assumption. Maybe 
 *       urlencoding should be used.)
 * (2.c) Don't print redundant information in a single record. This 
 *       would just confuse any potential user of this file.
 * (2.d) Saving space, being compact is not a priority, I don't think.
 *       
 * (3) There are no compatibility requirements from release to release.
 *     Sounds OK to me to change the format of the output when needed.
 *
 * (-) print transaction start and end delimiters
 * (-) print a unique transaction id as a handy label for anyone 
 *     who actually examines these logs.  
 *     The C address pointer to the transaction struct should be fine, 
 *     as it is simple and unique until the transaction is deleted ...
 *     and we log deletions, so that's OK.  Just note that the id
 *     for a deleted transaction might be recycled.
 * (-) print the current timestamp, so that if it is known that a bug 
 *     occurred at a certain time, it can be located.
 * (-) hack alert -- something better than just the account name 
 *     is needed for identifying the account.
 */
/* ------------------------------------------------------------------ */
/*
 * The engine currently uses the log mechanism with flag char set as
 * follows:
 * 
 * 'B' for 'begin edit' (followed by the transaction as it looks 
 *     before any changes, i.e. the 'old value')
 * 'D' for delete (i.e. delete the previous B; echoes the data in the 
 *     'old B')
 * 'C' for commit (i.e. accept a previous B; data that follows is the
 *     'new value')
 * 'R' for rollback (i.e. revert to previous B; data that follows should
 *     be identical to old B)
 */


static int gen_logs = 1;
static FILE * trans_log = NULL;
static char * log_base_name = NULL;

/********************************************************************\
\********************************************************************/

void xaccLogDisable (void) { gen_logs = 0; }
void xaccLogEnable  (void) { gen_logs = 1; }

/********************************************************************\
\********************************************************************/

void 
xaccLogSetBaseName (const char *basepath)
{
   if (!basepath) return;

   g_free (log_base_name);
   log_base_name = g_strdup (basepath);

   if (trans_log) {
      xaccCloseLog();
      xaccOpenLog();
   }
}

/********************************************************************\
\********************************************************************/

void
xaccOpenLog (void)
{
   char * filename;
   char * timestamp;

   if (!gen_logs) return;
   if (trans_log) return;

   if (!log_base_name) log_base_name = g_strdup ("translog");

   /* tag each filename with a timestamp */
   timestamp = xaccDateUtilGetStampNow ();

   filename = g_strconcat (log_base_name, ".", timestamp, ".log", NULL);

   trans_log = fopen (filename, "a");
   if (!trans_log) {
      int norr = errno;
      printf ("Error: xaccOpenLog(): cannot open journal \n"
              "\t %d %s\n", norr, strerror (norr));

      g_free (filename);
      g_free (timestamp);
      return;
   }

   g_free (filename);
   g_free (timestamp);

   /* use tab-separated fields */
   fprintf (trans_log, "mod	id	time_now	" \
                       "date_entered	date_posted	" \
                       "account	num	description	" \
                       "memo	action	reconciled	" \
                       "amount	price date_reconciled\n");
   fprintf (trans_log, "-----------------\n");
}

/********************************************************************\
\********************************************************************/

void
xaccCloseLog (void)
{
   if (!trans_log) return;
   fflush (trans_log);
   fclose (trans_log);
   trans_log = NULL;
}

/********************************************************************\
\********************************************************************/

void
xaccTransWriteLog (Transaction *trans, char flag)
{
   GList *node;
   char *dnow, *dent, *dpost, *drecn; 

   if (!gen_logs) return;
   if (!trans_log) return;

   dnow = xaccDateUtilGetStampNow ();
   dent = xaccDateUtilGetStamp (trans->date_entered.tv_sec);
   dpost = xaccDateUtilGetStamp (trans->date_posted.tv_sec);

   fprintf (trans_log, "===== START\n");

   for (node = trans->splits; node; node = node->next) {
      Split *split = node->data;
      const char * accname = "";

      if (xaccSplitGetAccount(split))
        accname = xaccAccountGetName (xaccSplitGetAccount(split));

      drecn = xaccDateUtilGetStamp (split->date_reconciled.tv_sec);

      /* use tab-separated fields */
      fprintf (trans_log,
               "%c\t%p/%p\t%s\t%s\t%s\t%s\t%s\t"
               "%s\t%s\t%s\t%c\t%lld/%lld\t%lld/%lld\t%s\n",
               flag,
               trans, split,  /* trans+split make up unique id */
               dnow ? dnow : "",
               dent ? dent : "", 
               dpost ? dpost : "", 
               accname ? accname : "",
               trans->num ? trans->num : "", 
               trans->description ? trans->description : "",
               split->memo ? split->memo : "",
               split->action ? split->action : "",
               split->reconciled,
               gnc_numeric_num(split->damount), 
               gnc_numeric_denom(split->damount),
               gnc_numeric_num(split->value), 
               gnc_numeric_denom(split->value),
               drecn ? drecn : "");

      g_free (drecn);
   }

   fprintf (trans_log, "===== END\n");

   g_free (dnow);
   g_free (dent);
   g_free (dpost);

   /* get data out to the disk */
   fflush (trans_log);
}

/********************************************************************\
\********************************************************************/

#if 0
/* open_memstream seems to give various distros fits
 * this has resulted in warfare on the mailing list.
 * I think the truce called required changing this to asprintf
 * this code is not currently used ...  so its ifdef out
 */

char *
xaccSplitAsString(Split *split, const char prefix[]) {
  char *result = NULL;
  size_t result_size;
  FILE *stream = open_memstream(&result, &result_size); 
  const char *split_memo = xaccSplitGetMemo(split);
  const double split_value = DxaccSplitGetValue(split);
  Account *split_dest = xaccSplitGetAccount(split);
  const char *dest_name =
    split_dest ? xaccAccountGetName(split_dest) : NULL;

  assert(stream);

  fputc('\n', stream);
  fputs(prefix, stream);
  fprintf(stream, "  %10.2f | %15s | %s",
          split_value,
          dest_name ? dest_name : "<no-account-name>",
          split_memo ? split_memo : "<no-split-memo>");
  fclose(stream); 
  return(result);
}

static char *
xaccTransGetDateStr (Transaction *trans)
{
   char buf [MAX_DATE_LENGTH];
   struct tm *date;
   time_t secs;

   secs = xaccTransGetDate (trans);

   date = localtime (&secs);

   printDate(buf, date->tm_mday, date->tm_mon+1, date->tm_year +1900);

   return g_strdup (buf);
}

char *
xaccTransAsString(Transaction *txn, const char prefix[]) {
  char *result = NULL;
  size_t result_size;
  FILE *stream = open_memstream(&result, &result_size); 
  time_t date = xaccTransGetDate(txn);
  const char *num = xaccTransGetNum(txn);
  const char *desc = xaccTransGetDescription(txn);
  const char *memo = xaccSplitGetMemo(xaccTransGetSplit(txn, 0));
  const double total = DxaccSplitGetValue(xaccTransGetSplit(txn, 0));
  
  assert(stream);

  fputs(prefix, stream);
  if(date) {
    char *datestr = xaccTransGetDateStr(txn);
    fprintf(stream, "%s", datestr);
    free(datestr);
  } else {
    fprintf(stream, "<no-date>");
  }
  fputc(' ', stream); 
  if(num) {
    fputs(num, stream);
  } else {
    fprintf(stream, "<no-num>");
  }

  fputc('\n', stream);
  fputs(prefix, stream);
  if(desc) {
    fputs("  ", stream);
    fputs(desc, stream);
  } else {
    fprintf(stream, "<no-description>");
  }
  
  fputc('\n', stream);
  fputs(prefix, stream);
  if(memo) {
    fputs("  ", stream);
    fputs(memo, stream);
  } else {
    fprintf(stream, "<no-transaction-memo>");
  }
  
  {
    int split_count = xaccTransCountSplits(txn);
    int i;
    for(i = 1; i < split_count; i++) {
      Split *split = xaccTransGetSplit(txn, i);
      char *split_text = xaccSplitAsString(split, prefix);
      fputs(split_text, stream);
      free(split_text);
    }
  }
  fputc('\n', stream);

  fputs(prefix, stream);
  fprintf(stream, "  %10.2f -- Transaction total\n", total);
  fclose(stream); 

  return(result);
}

#endif

/************************ END OF ************************************\
\************************* FILE *************************************/
