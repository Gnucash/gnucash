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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#define _GNU_SOURCE
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "DateUtils.h"
#include "date.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "util.h"

/*
 * The logfiles are useful for tracing, journalling.
 * Note that the current support for journalling is at best 
 * embryonic, at worst, sets the wrong expectations.
 */
int gen_logs = 1;
FILE * trans_log = 0x0;
FILE * split_log = 0x0;


/********************************************************************\
\********************************************************************/

void
xaccOpenLog (void)
{
   char * timestamp;

   if (!gen_logs) return;
   if (trans_log && split_log) return;

   /* tag each filename with a timestamp */
   timestamp = xaccDateUtilGetStampNow ();

   if (!trans_log) {
      char filename[1000];

      strcpy (filename, "translog.");
      strcat (filename, timestamp);
      strcat (filename, ".log");

      trans_log = fopen (filename, "a");

      /* use tab-separated fields, to be /rdb compatible */
      fprintf (trans_log, "num	description\n");
      fprintf (trans_log, "-----------------\n");
   }

   if (!split_log) {
      char filename[1000];

      strcpy (filename, "splitlog.");
      strcat (filename, timestamp);
      strcat (filename, ".log");

      split_log = fopen (filename, "a");

      /* use tab-separated fields, to be /rdb compatible */
      fprintf (split_log, "num	memo	action	reconciled	amount	price\n");
      fprintf (split_log, "-----------------\n");
   }
   free (timestamp);
}

/********************************************************************\
\********************************************************************/

void
xaccTransWriteLog (Transaction *trans)
{
   Split *split;
   int i = 0;

   if (!gen_logs) return;
   if (!trans_log || !split_log) return;

   /* use tab-separated fields, to be /rdb compatible */
   fprintf (trans_log, "%s	%s\n", trans->num, trans->description);

   split = trans->splits[0];
   while (split) {
      fprintf (split_log, "%s	%s	%s	%c	%10.6f	%10.6f\n",
               trans->num,
               split->memo,
               split->action,
               split->reconciled,
               split->damount,
               split->share_price
               );
      i++;
      split = trans->splits[i];
   }

   /* get data out to the disk */
   fflush (trans_log);
   fflush (split_log);
}

/********************************************************************\
\********************************************************************/


char *
xaccSplitAsString(Split *split, const char prefix[]) {
  char *result = NULL;
  size_t result_size;
  FILE *stream = open_memstream(&result, &result_size); 
  const char *split_memo = xaccSplitGetMemo(split);
  const double split_value = xaccSplitGetValue(split);
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

char *
xaccTransAsString(Transaction *txn, const char prefix[]) {
  char *result = NULL;
  size_t result_size;
  FILE *stream = open_memstream(&result, &result_size); 
  time_t date = xaccTransGetDate(txn);
  const char *num = xaccTransGetNum(txn);
  const char *desc = xaccTransGetDescription(txn);
  const char *memo = xaccSplitGetMemo(xaccTransGetSplit(txn, 0));
  const double total = xaccSplitGetValue(xaccTransGetSplit(txn, 0));
  
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

/************************ END OF ************************************\
\************************* FILE *************************************/
