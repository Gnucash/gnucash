/********************************************************************\
 * reconcile-listP.h -- private GNOME reconcile list functions      *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
\********************************************************************/

#ifndef __RECONCILE_LISTP_H__
#define __RECONCILE_LISTP_H__

#include "reconcile-list.h"


enum
{
  TOGGLE_RECONCILED,
  LAST_SIGNAL
};


static void gnc_reconcile_list_init(GNCReconcileList *list);

static void gnc_reconcile_list_class_init(GNCReconcileListClass *klass);

static void gnc_reconcile_list_select_row(GtkCList *clist, gint row,
					  gint column, GdkEvent *event);

static void gnc_reconcile_list_unselect_row(GtkCList *clist, gint row,
					    gint column, GdkEvent *event);

static void gnc_reconcile_list_destroy(GtkObject *object);

static void gnc_reconcile_list_fill(GNCReconcileList *list);


#endif
