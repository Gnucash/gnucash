/********************************************************************\
 * reconcile-list.h -- GNOME reconcile list functions               *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
 * Copyright (C) 2003 Derek Atkins <derek@ihtfp.com>                *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_RECONCILE_LIST_H
#define GNC_RECONCILE_LIST_H

#include "gnc-query-list.h"
#include "qof.h"

G_BEGIN_DECLS

#define GNC_TYPE_RECONCILE_LIST		(gnc_reconcile_list_get_type ())
#define GNC_RECONCILE_LIST(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_RECONCILE_LIST, GNCReconcileList))
#define GNC_RECONCILE_LIST_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_RECONCILE_LIST, GNCReconcileListClass))
#define GNC_IS_RECONCILE_LIST(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_RECONCILE_LIST))
#define GNC_IS_RECONCILE_LIST_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_RECONCILE_LIST))

typedef struct GNCReconcileList GNCReconcileList;

typedef enum {
  RECLIST_DEBIT,
  RECLIST_CREDIT
} GNCReconcileListType;

struct GNCReconcileList {
  GNCQueryList qlist;

  GHashTable *reconciled;
  Account *account;
  GList *column_list;

  time_t statement_date;

  GNCReconcileList *sibling;
  GNCReconcileListType list_type;
  gboolean no_toggle;
};

typedef struct {
  GtkCListClass parent_class;

  void (*toggle_reconciled)  (GNCReconcileList *list,
			      Split            *split);
  void (*double_click_split) (GNCReconcileList *list,
			      Split            *split);
} GNCReconcileListClass;

#define GCONF_RECONCILE_SECTION "dialogs/reconcile"

/***********************************************************
 *                public functions                         *
 ***********************************************************/

GType gnc_reconcile_list_get_type (void);

GtkWidget * gnc_reconcile_list_new (Account * account,
				    GNCReconcileListType type,
                                    time_t date);

gint gnc_reconcile_list_get_needed_height(GNCReconcileList *list,
                                          gint num_rows);

gint gnc_reconcile_list_get_num_splits(GNCReconcileList *list);

Split * gnc_reconcile_list_get_current_split(GNCReconcileList *list);

void gnc_reconcile_list_refresh (GNCReconcileList *list);

gnc_numeric gnc_reconcile_list_reconciled_balance(GNCReconcileList *list);

void gnc_reconcile_list_commit (GNCReconcileList *list, time_t date);

void gnc_reconcile_list_postpone (GNCReconcileList *list);

void gnc_reconcile_list_unselect_all(GNCReconcileList *list);

gboolean gnc_reconcile_list_changed(GNCReconcileList *list);

G_END_DECLS

#endif /* RECONCILE_LIST_H */
