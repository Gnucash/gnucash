/********************************************************************\
 * QuickFill.h -- the quickfill tree data structure                 *
 * Copyright (C) 1997 Robin D. Clark                                *
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
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __XACC_QUICKFILL_H__
#define __XACC_QUICKFILL_H__

#include "config.h"

/** STRUCTS *********************************************************/

/* The way quickfill works is this: the decscription field of a transaction
 * is used to insert a pointer to itself into the quickfill tree.  The
 * QuickFill struct is a node in the tree, and the qf array is an array
 * of pointers to children of the node.  (NULL, if no corresponding child.)
 * The index of the array is determined by the next letter in the string
 * that is the description field.  The trans field is a pointer to the last
 * inserted child that made it to this node.  At the root of the tree is
 * a QuickFill struct, with trans == NULL, and the array of pointers to
 * children.  When a transaction is inserted, the first letter of the
 * description field determines which child the transaction goes into.  
 * If the child is NULL, a new QuickFill node is created.  Otherwise,
 * the trans field of the child is changed to point to the new transaction.
 * Then, recursively, the process is repeated, with the next character in
 * the description field as the index to the array.  The recursion stops
 * when the end of the descriptions string is reached.
 */

#define QFNUM 27     /* 26+1 letters in the alphabet */

typedef struct _quickfill {
  char * text;                     /* the first matching text string     */
  struct _quickfill *qf[QFNUM];    /* array of children in the tree      */
} QuickFill;

/** PROTOTYPES ******************************************************/

QuickFill *xaccMallocQuickFill( void );
void       xaccFreeQuickFill( QuickFill *qf );
QuickFill *xaccGetQuickFill( QuickFill *qf, char c );
void       xaccQFInsertText( QuickFill *qf, const char *text );

/** GLOBALS *********************************************************/

#endif /* __XACC_QUICKFILL_H__ */
