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
      fprintf (split_log, "%s	%s	%s	%c	%g	%g\n",
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
}

/************************ END OF ************************************\
\************************* FILE *************************************/
