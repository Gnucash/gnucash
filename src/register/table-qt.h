/*
 * FILE:
 * table-qt.h
 *
 * FUNCTION:
 * This file defines the QT-specific GUI portions of the 
 * Table object.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 1998 Rob Browning <rlb@cs.utexas.edu>
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


#ifndef __XACC_TABLE_QT_H__
#define __XACC_TABLE_QT_H__

#define TABLE_PRIVATE_DATA						 \
  void *table_widget;

#define TABLE_PRIVATE_DATA_INIT(table)  {}

#define TABLE_PRIVATE_DATA_DESTROY(table) {}

/* nothing to resize */
#define TABLE_PRIVATE_DATA_RESIZE(a,b,c,d,e)

typedef struct _Table Table;

/* create the QT Widget */
void *xaccCreateTable (Table *, void *parent);
void        xaccNextTabGroup (Table *, void*);

/* redraw the table GUI */
void        xaccRefreshTableGUI (Table *);

#endif __XACC_TABLE_QT_H__
/* ================== end of file ======================= */
