/* XXXXXXXXXXXXXXXXXXXXXXXXXXX
 Caution

As of March 2003, this file is under construction, 
it is in the process of being modernized
in order to allow the implementation of Lots to proceed forward.
This is needed, in turn, to complete the implementation of books.

this file does not currently compile
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

/*
 * FILE:
 * Queue.c
 *
 * DESCRIPTION:
 * Provide simple FIFO/LIFO cost-basis accounting support.
 *
 * RESTRICTIONS:
 * -- Does not support use with mixed currencies.
 * -- Does not check for or warn mixed currency use.
 * -- Does not allow pushhead after a pophead has occured.
 *
 * HISTORY:
 * Created by Linas Vepstas January 1999
 * Copyright (c) 1999, 2000, 2003 Linas Vepstas <linas@linas.org>
 */

#include <limits.h>
#include <stdlib.h>
#include <strings.h>

#include "config.h"

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "GNCQueue.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

/* ================================================== */

struct queue_s 
{
   GUID guid;  /* globally unique id */
   GNCBook *book;

   /* List of Lots ?? */

   /* XXX commodity ??? */
   Split **split_list;

   int head_split;
   int tail_split;
   int list_len;

   gnc_numeric head_amount;
   gnc_numeric head_value;
   Timespec head_date;

   gnc_numeric tail_amount;
   gnc_numeric tail_value;
   Timespec tail_date;

  /* kvp_data is a key-value pair database for storing simple
   * "extra" information in splits, transactions, and accounts.
   * it's NULL until accessed. */
  kvp_frame * kvp_data;
};

/* ================================================== */

GNCQueue *
xaccMallocQueue (GNCBook *book)
{
   GNCQueue * ret;
   ret =  (GNCQueue *) g_new (GNCQueue, 1);
   xaccInitGNCQueue (ret, book);
   return ret;
}

/* ================================================== */
#define INITIAL_LENGTH 100

void 
xaccInitQueue (GNCQueue *q, GNCBook *book)
{
   if (!q) return;

   q->split_list = (Split **) malloc (INITIAL_LENGTH * sizeof (Split *));
   q->list_len = INITIAL_LENGTH;
   q->head_split = -1;
   q->tail_split = 0;
   q->head_amount = gnc_numeric_zero();
   q->tail_amount = gnc_numeric_zero();
   q->head_price = gnc_numeric_zero();
   q->tail_price = gnc_numeric_zero();

   q->head_date.tv_sec = 0;
   q->head_date.tv_nsec = 0;
   q->tail_date.tv_sec = 0;
   q->tail_date.tv_nsec = 0;

   q->kvp_data =  kvp_frame_new();
	q->book = book;
   xaccGUIDNew (&q->guid, book);
   xaccStoreEntity(book->entity_table, q, &q->guid, GNC_ID_QUEUE);
}

/* ================================================== */

void 
xaccFreeQueue (GNCQueue *q)
{
   if (!q) return;

   xaccRemoveEntity (q->book->entity_table, &q->guid);

   if (q->split_list) _free (q->split_list);
   q->split_list = 0x0;
   q->list_len = -1;
   q->head_split = -1;
   q->tail_split = 0;
   q->head_amount = gnc_numeric_zero();
   q->tail_amount = gnc_numeric_zero();
   q->head_price = gnc_numeric_zero();
   q->tail_price = gnc_numeric_zero();

   g_free (q);
}

/* ================================================== */

void 
xaccQueuePushHead (GNCQueue *q, Split *s)
{
  if (!q || !s) return;

  /* I'm too lazy to code up a more complex feature that no one will use ... */
  /* If you are reading this, you are invited to do so yourself :-) */
  if ( !DEQ (q->head_amount, 0.0)) {
    PERR ("The current implementation does not\n"
          "\tsupport pushing onto a queue that has been popped \n");
    return;
  }

  /* don't queue a split that has no value */
  if (DEQ (s->damount, 0.0)) return;

  q->head_split ++;
  q->split_list [ q->head_split ] = s;
}

/* ================================================== */

double 
xaccQueuePopTailShares (GNCQueue *q, double shrs)
{
   int tp, hp;
   Split **list;
   double rshrs = 0.0;

   if (!q) return 0.0;
   if (0.0 >= shrs) return 0.0;

   /* the tail holds enough to do it in one go. */
   if (q->tail_amount > shrs) {
      q->tail_amount -= shrs;
      return shrs;
   }

   /* use up the tail shares first ... */
   shrs -= q->tail_amount;
   rshrs += q->tail_amount;
   q->tail_amount = 0.0;
   q->tail_price = 0.0;
   q->tail_date.tv_sec = 0;
   q->tail_date.tv_nsec = 0;

   /* start poping */
   tp = q->tail_split;
   hp = q->head_split;
   list = q->split_list;
   while (tp <= hp)
   {
      /* the tail holds enough to do it in one go. */
      if ((list[tp]->damount) > shrs) {
         q->tail_amount = list[tp]->damount - shrs;
         q->tail_price = list[tp]->share_price;
         assert (list[tp]->parent);
         q->tail_date = list[tp]->parent->date_posted;
         rshrs += shrs;
         tp++;
         q->tail_split = tp;
         return rshrs;
      }

      /* well, well need use up this entire split ... */
      shrs -= (list[tp]->damount);
      rshrs += (list[tp]->damount);
      tp++;
   }

   /* oops, if we got to here, we've used up all of the splits.
    * give em whatever we've got on the head, and then we're outta here.
    */
   q->tail_split = 0;
   q->head_split = -1;

   /* the head holds enough to do it in one go. */
   if (q->head_amount > shrs) {
      q->head_amount -= shrs;
      rshrs += shrs;
      return shrs;
   }

   /* use up whats left of the head shares */
   shrs -= q->head_amount;
   rshrs += q->head_amount;
   q->head_amount = 0.0;
   q->head_price = 0.0;
   q->head_date.tv_sec = 0;
   q->head_date.tv_nsec = 0;

   return rshrs;
}

/* ================================================== */

double 
xaccQueuePopTailValue (GNCQueue *q, double val)
{
   int tp, hp;
   Split **list;
   double rval = 0.0;

   if (!q) return 0.0;
   if (0.0 >= val) return 0.0;

   /* the tail holds enough to do it in one go. */
   if (q->tail_amount * q->tail_price > val) {
      q->tail_amount -= val / q->tail_price;
      return val;
   }

   /* use up the tail shares first ... */
   val -= q->tail_amount * q->tail_price;
   rval += q->tail_amount * q->tail_price;
   q->tail_amount = 0.0;
   q->tail_price = 0.0;
   q->tail_date.tv_sec = 0;
   q->tail_date.tv_nsec = 0;

   /* start poping */
   tp = q->tail_split;
   hp = q->head_split;
   list = q->split_list;
   while (tp <= hp)
   {
      /* the tail holds enough to do it in one go. */
      if ((list[tp]->damount)*(list[tp]->share_price) > val) {
         q->tail_amount = list[tp]->damount - (val/(list[tp]->share_price));
         q->tail_price = list[tp]->share_price;
         assert (list[tp]->parent);
         q->tail_date = list[tp]->parent->date_posted;
         rval += val;
         tp++;
         q->tail_split = tp;
         return rval;
      }

      /* well, well need use up this entire split ... */
      val -= (list[tp]->damount) * (list[tp]->share_price);
      rval += (list[tp]->damount) * (list[tp]->share_price);
      tp++;
   }

   /* oops, if we got to here, we've used up all of the splits.
    * give em whatever we've got on the head, and then we're outta here.
    */
   q->tail_split = 0;
   q->head_split = -1;

   /* the head holds enough to do it in one go. */
   if (q->head_amount * q->head_price > val) {
      q->head_amount -= val / (q->head_price);
      rval += val;
      return val;
   }

   /* use up whats left of the head shares */
   val -= q->head_amount * q->head_price; 
   rval += q->head_amount * q->head_price;
   q->head_amount = 0.0;
   q->head_price = 0.0;
   q->head_date.tv_sec = 0;
   q->head_date.tv_nsec = 0;

   return rval;
}

/* ================================================== */
/* these routines are same as above, but everything is reversed... */

double 
xaccQueuePopHeadValue (GNCQueue *q, double val)
{
   PERR("not implemented\n");
   return 0.0;
}

double 
xaccQueuePopHeadShares (GNCQueue *q, double val)
{
   PERR("not implemented\n");
   return 0.0;
}

/* ================================================== */

double
xaccQueueGetShares (GNCQueue *q)
{
   Split **list;
   double shrs = 0.0;
   int i;

   if (!q) return 0.0;

   shrs += q->head_amount;
   shrs += q->tail_amount;

   list = q->split_list;

   for (i = q->tail_split; i <= q->head_split; i++)
      shrs += list[i]->damount;

   return shrs;
}

double 
xaccQueueGetValue (GNCQueue *q)
{
   Split **list;
   double val = 0.0;
   int i;

   if (!q) return 0.0;

   val += q->head_amount * q->head_price;
   val += q->tail_amount * q->tail_price;

   list = q->split_list;

   for (i = q->tail_split; i <= q->head_split; i++)
      val += list[i]->damount * list[i]->share_price;

   return val;
}

/* ================ END OF FILE  ==================== */
