/*
 * FILE:
 * Queue.c
 *
 * DESCRIPTION:
 * Provide simple FIFO/LIFO cost-basis accounting support.
 *
 * HISTORY:
 * created by Linas Vepstas January 1999
 * Copyright (c) 1999 Linas Vepstas
 */
/********************************************************************\
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
\********************************************************************/

#ifndef __XACC_QUEUE_H__
#define __XACC_QUEUE_H__

void xaccQueuePushHead (Split *s);

#endif /* __XACC_QUEUE_H__ */

#include <limits.h>
#include <stdlib.h>
#include <strings.h>

#include "config.h"

#include "Transaction.h"
#include "TransactionP.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

/* ================================================== */

struct _Queue {
   Split **split_list;

   int head_split;
   int tail_split;
   int list_len;

   double head_amount;
   double head_price;
   double tail_amount;
   double tail_price;

};

/* ================================================== */

Queue *
xaccMallocQueue (void)
{
   Queue * ret;
   ret =  (Queue *) _malloc (sizeof (Queue));
   xaccInitQueue (ret);
   return ret;
}

/* ================================================== */
#define INITIAL_LENGTH 100

void 
xaccInitQueue (Queue *q)
{
   if (!q) return;

   q->split_list = (Split **) malloc (INITIAL_LENGTH * sizeof (Split *));
   q->list_len = INITIAL_LENGTH;
   q->head_split = -1;
   q->tail_split = -1;
   q->head_amount = 0.0;
   q->tail_amount = 0.0;
   q->head_price = 0.0;
   q->tail_price = 0.0;
}

/* ================================================== */

void 
xaccFreeQueue (Queue *q)
{
   if (!q) return;

   if (q->split_list) _free (q->split_list);
   q->split_list = 0x0;
   q->list_len = -1;
   q->head_split = -1;
   q->tail_split = -1;
   q->head_amount = 0.0;
   q->tail_amount = 0.0;
   q->head_price = 0.0;
   q->tail_price = 0.0;

   _free (q);
}

/* ================================================== */

void 
xaccQueuePushHead (Split *s)
{
}

/* ================ END OF FILE  ==================== */
