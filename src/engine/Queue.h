/*
 * FILE:
 * Queue.h
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

#endif /* __XACC_QUEUE_H__ */
