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
 * Queue.h
 *
 * DESCRIPTION:
 * Provides simple FIFO/LIFO cost-basis accounting support.
 *
 * The routines in this class implement a simple FIFO that
 * holds a series of splits.  Splits can be pushed onto the head
 * of the fifo.  The number of shares contained in the fifo is 
 * then simply the sum of all of the shares in the splits.
 * The "value" of the fifo is the cost-basis value of all of 
 * the splits, i.e. the total of number of shares times prices.
 *
 * When shares are sold, they are dequed from the queue.  If the 
 * oldest shares are sold first, they are dequed or "popped" from
 * the tail, and the queue acts as a FIFO.  If the newest shares
 * are sold first, then the queue acts as a LIFO, and shares are
 * poped off the head.
 *
 * Either shares or a monetary value can be dequeued.  The amount
 * dequeued does not have to exactly equal the amount/value of
 * any given split; fractional amounts can be dequeued.  This queue
 * automatically tracks fractional amounts on both the head (LIFO) 
 * and tail (FIFO).  Indeed, there are *no* routines to explicitly
 * dequeue a split; *only* amounts and values can be dequed. 
 * Conversely, *only* splits can be pushed on, as this queue does 
 * not provide any internal or hidden elements.
 *
 * In addition to the queue value, and the number of shares queued,
 * the average age of the shares can also be computed, as well as
 * other statistics involving a date and price.  The queue automatically
 * tracks the posted date of fractional splits at the head and tail.
 *
 * RESTRICTIONS:
 * -- Does not support use with mixed currencies.
 * -- Does not check for or warn mixed currency use.
 * -- Does not allow push-head after a pop-head has occured.
 * -- Push-tail not implemented
 *
 * HISTORY:
 * created by Linas Vepstas January 1999
 * Copyright (c) 1999, 2000 Linas Vepstas
 */

#ifndef XACC_QUEUE_H
#define XACC_QUEUE_H

#include "Transaction.h"

typedef struct _Queue Queue;

/*
 * The xaccQueuePushHead() routine pushes a split onto the head 
 *    of the queue.
 *
 * The xaccQueuePopTailShares() routine removes the indicated
 *    number of shares from the bottom of the queue.  If the queue
 *    does not contain that many shares, it dequeues all of the 
 *    shares. This routine returns the actual number of shares dequeued.
 *    Use this routine for FIFO accounting.
 *
 * The xaccQueuePopTailValue() routine removes the indicated
 *    value from the bottom of the queue.  If the value of the queue is less
 *    than this amount, the value of the queue is set to zero, and the
 *    actual value poped is returned.
 *    Use this routine for FIFO accounting.
 *
 * The xaccQueuePopHeadShares() and xaccQueuePopHeadValue() routines
 *    behave in the same way as the *Tail* versions, except that they act
 *    on the head of the queue.  Use these routines for LIFO accounting.
 *
 * The xaccQueueGetValue() routine returns the value of the queue.
 * The xaccQueueGetShares() routine returns the number of shares in the queue.
 */

Queue * xaccMallocQueue (void);
void xaccInitQueue (Queue *q);
void xaccFreeQueue (Queue *q);

void xaccQueuePushHead (Queue *q, Split *s);

double xaccQueuePopHeadShares (Queue *, double);
double xaccQueuePopHeadValue (Queue *, double);
double xaccQueuePopTailShares (Queue *, double);
double xaccQueuePopTailValue (Queue *, double);

double xaccQueueGetValue (Queue *);
double xaccQueueGetShares (Queue *);

#endif /* XACC_QUEUE_H */
