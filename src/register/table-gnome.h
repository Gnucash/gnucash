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

/*
 * FILE:
 * table-gnome.h
 *
 * FUNCTION:
 * This file defines the GNOME-specific GUI portions of the Table object.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 1998 Rob Browning <rlb@cs.utexas.edu>
 */


#ifndef __XACC_TABLE_GNOME_H__
#define __XACC_TABLE_GNOME_H__

#include "config.h"

#include <gnome.h>

#include "gnc-common.h"

   
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

 TODO:

 - Still need prev_entry_text?

 */


#define TABLE_PRIVATE_DATA						\
  /* Gtk-only private table members  */					\
  GtkWidget *table_widget;          /* the Sheet */			\
  gint insert_signal_tag;                                               \
  gint delete_signal_tag;                                               \
  gint entry_needs_reconnect;



#define TABLE_PRIVATE_DATA_INIT(table) {				\
   table->table_widget = NULL;						\
   table->insert_signal_tag = -1;                                       \
   table->delete_signal_tag = -1;                                       \
   table->entry_needs_reconnect = FALSE;                                \
}



#define TABLE_PRIVATE_DATA_DESTROY(table) {				\
									\
   /* Let GTK know we're finished with this */				\
   if(table->table_widget) gtk_widget_unref(table->table_widget);	\
   table->table_widget = NULL;						\
									\
}

/* nothing to resize */
#define TABLE_PRIVATE_DATA_RESIZE(a,b,c,d,e)

typedef struct _Table Table;


void xaccCreateTable (GtkWidget *, void *);
void doRefreshCursorGUI (Table *, CellBlock *, int, int, gncBoolean);
void xaccRefreshTableGUI (Table *);

#endif __XACC_TABLE_GNOME_H__

/* ================== end of file ======================= */
