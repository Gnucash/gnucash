/*
 * FILE:
 * table-gtk.h
 *
 * FUNCTION:
 * This file defines the GTK-specific GUI portions of the 
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


#ifndef __XACC_TABLE_GTK_H__
#define __XACC_TABLE_GTK_H__

#include <gtk/gtk.h>
   
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


#define TABLE_PRIVATE_DATA						\
  /* Gtk-only private table members  */					\
  GtkWidget *table_widget;          /* the CList */			\
  GtkWidget *entry_frame;           /* the editing widget frame */	\
  GtkWidget *entry_widget;          /* the current cell editing widget */\
									\
  /* Current editing cell */						\
  int current_col;							\
  int current_row;							\
									\
  /* snapshot of entry text -- used to detect changes in callback */	\
  char *prev_entry_text;						\
									\
  GtkWidget *next_tab_group;        /* where to traverse in the end */	\



#define TABLE_PRIVATE_DATA_INIT(table) {				\
   table->table_widget = NULL;						\
   table->entry_frame = NULL;						\
   table->entry_widget = NULL;						\
									\
   table->current_col = -1;  /* coords ignoring header lines */		\
   table->current_row = -1;						\
   table->prev_entry_text = NULL;					\
   									\
   table->next_tab_group = 0;						\
}



#define TABLE_PRIVATE_DATA_DESTROY(table) {				\
									\
   /* Let GTK know we're finished with this */				\
   if(table->table_widget) gtk_widget_unref(table->table_widget);	\
   if(table->entry_frame) gtk_widget_unref(table->entry_frame);		\
   if(table->entry_widget) gtk_widget_unref(table->entry_widget);	\
   table->table_widget = NULL;						\
   table->entry_frame = NULL;						\
   table->entry_widget = NULL;						\
									\
   g_free(table->prev_entry_text); table->prev_entry_text = NULL;	\
}

typedef struct _Table Table;

/* create the GtkWidget */
GtkWidget      *xaccCreateTable (Table *, GtkWidget *parent);
void        xaccNextTabGroup (Table *, GtkWidget*);

/* redraw the table GUI */
void        xaccRefreshTableGUI (Table *);

#endif __XACC_TABLE_GTK_H__
/* ================== end of file ======================= */
