/********************************************************************\
 * reconcile-list.h -- GNOME reconcile list functions               *
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

#ifndef __GNC_RECONCILE_LIST_H__
#define __GNC_RECONCILE_LIST_H__

#include <gtk/gtkclist.h>

#ifdef __cplusplus
extern "C" {
#endif				/* __cplusplus */

#define GTK_TYPE_GNC_RECONCILE_LIST (gnc_reconcile_list_get_type ())
#define GNC_RECONCILE_LIST(obj) (GTK_CHECK_CAST ((obj), GTK_TYPE_GNC_RECONCILE_LIST, GNCReconcileList))
#define GNC_RECONCILE_LIST_CLASS(klass) (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_GNC_RECONCILE_LIST, GNCReconcileListClass))
#define GTK_IS_GNC_RECONCILE_LIST(obj) (GTK_CHECK_TYPE ((obj), GTK_TYPE_GNC_RECONCILE_LIST))
#define GTK_IS_GNC_RECONCILE_LIST_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_GNC_RECONCILE_LIST))

typedef struct _GNCReconcileList      GNCReconcileList;
typedef struct _GNCReconcileListClass GNCReconcileListClass;

typedef enum
{
  RECLIST_DEBIT,
  RECLIST_CREDIT
} GNCReconcileListType;

struct _GNCReconcileList
{
  GtkCList clist;

  gint list_type; /* DEBIT or CREDIT */

  gint num_splits;
  gint num_columns;

  gint current_row;

  GHashTable *reconciled;

  GtkStyle *reconciled_style;
  GtkStyle *normal_style;

  Account *account;
};

struct _GNCReconcileListClass
{
  GtkCListClass parent_class;

  void (*toggle_reconciled) (GNCReconcileList *list,
			     Split            *split);
};

/***********************************************************
 *                public functions                         *
 ***********************************************************/

GtkType gnc_reconcile_list_get_type (void);

GtkWidget * gnc_reconcile_list_new (Account * account,
				    GNCReconcileListType type);

gint gnc_reconcile_list_get_row_height(GNCReconcileList *list);

gint gnc_reconcile_list_get_num_splits(GNCReconcileList *list);

void gnc_reconcile_list_refresh (GNCReconcileList *list);

double gnc_reconcile_list_reconciled_balance(GNCReconcileList *list);

void gnc_reconcile_list_commit(GNCReconcileList *list);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __RECONCILE_LIST_H__ */
