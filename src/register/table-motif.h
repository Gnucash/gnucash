/*
 * FILE:
 * table-motif.h
 *
 * FUNCTION:
 * This header defines the Motif-GUI specific functions
 * associated with the Table GUI.  
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
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


#ifndef __XACC_TABLE_MOTIF_H__
#define __XACC_TABLE_MOTIF_H__

#include <Xm/Xm.h>

/* We use C not C++ in this project, but we none-the-less need 
 * the general mechanism of inheritance.  The three #defines
 * below implement that.
 *
 * the TABLE_PRIVATE_DATA declaration should be thought of as a
 * "derived class" of which Table is the base class.  This
 * define is included as a part of the definition of the Table
 * structure in table-allgui.h
 *
 * The TABLE_PRIVATE_DATA_INIT and DESTROY are the constructors
 * and destructors, respectively, for this derived class.
 * These are included in the xaccTableInit() and the xaccTableDestroy()
 * routines in the file table-allgui.c, where they are called, 
 * respectively, last, and first, just as "real" constructors &
 * destructors would be
 */

#define TABLE_PRIVATE_DATA					\
  /* Motif specific private data */				\
  Widget table_widget;          /* the XbaeMatrix */		\
  Widget next_tab_group;        /* where to traverse in the end */ 


#define TABLE_PRIVATE_DATA_INIT(table) {		\
   table->table_widget = 0;				\
   table->next_tab_group = 0;				\
}

/* hack alert -- shouldn't destroy get rid of the widget? */
#define TABLE_PRIVATE_DATA_DESTROY(table) 

typedef struct _Table Table;

/* create the widget */
Widget      xaccCreateTable (Table *, Widget parent, char * name);
void        xaccNextTabGroup (Table *, Widget);

/* redraw the table GUI */
void        xaccRefreshTableGUI (Table *);

#endif __XACC_TABLE_MOTIF_H__
/* ================== end of file ======================= */
