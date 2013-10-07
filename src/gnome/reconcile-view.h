/********************************************************************\
 * reconcile-view.h -- GNOME reconcile view functions               *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
 * Copyright (C) 2003 Derek Atkins <derek@ihtfp.com>                *
 * Copyright (C) 2012 Robert Fewell                                 *
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

#ifndef GNC_RECONCILE_VIEW_H
#define GNC_RECONCILE_VIEW_H

#include "gnc-query-view.h"
#include "qof.h"

G_BEGIN_DECLS

#define GNC_TYPE_RECONCILE_VIEW		(gnc_reconcile_view_get_type ())
#define GNC_RECONCILE_VIEW(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_RECONCILE_VIEW, GNCReconcileView))
#define GNC_RECONCILE_VIEW_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_RECONCILE_VIEW, GNCReconcileViewClass))
#define GNC_IS_RECONCILE_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_RECONCILE_VIEW))
#define GNC_IS_RECONCILE_VIEW_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_RECONCILE_VIEW))

typedef struct GNCReconcileView GNCReconcileView;

typedef enum
{
    RECLIST_DEBIT,
    RECLIST_CREDIT
} GNCReconcileViewType;

struct GNCReconcileView
{
    GNCQueryView         qview;

    GHashTable          *reconciled;
    Account             *account;
    GList               *column_list;

    time64               statement_date;

    GNCReconcileView    *sibling;
    GNCReconcileViewType view_type;
    gboolean             no_toggle;
};

typedef struct
{
    GtkTreeViewClass parent_class;

    void (*toggle_reconciled)  (GNCReconcileView *view, Split *split);
    void (*line_selected)      (GNCReconcileView *view, gpointer item);
    void (*double_click_split) (GNCReconcileView *view, Split *split);
} GNCReconcileViewClass;

#define GNC_PREFS_GROUP_RECONCILE "dialogs.reconcile"

/***********************************************************
 *                public functions                         *
 ***********************************************************/

GType gnc_reconcile_view_get_type (void);

GtkWidget * gnc_reconcile_view_new (Account * account,
                                    GNCReconcileViewType type,
                                    time64 date);

gint gnc_reconcile_view_get_num_splits (GNCReconcileView *view);

gint gnc_reconcile_view_num_selected (GNCReconcileView *view );

Split * gnc_reconcile_view_get_current_split (GNCReconcileView *view);

void gnc_reconcile_view_set_list (GNCReconcileView *view, gboolean reconcile);

void gnc_reconcile_view_refresh (GNCReconcileView *view);

gnc_numeric gnc_reconcile_view_reconciled_balance (GNCReconcileView *view);

void gnc_reconcile_view_commit (GNCReconcileView *view, time64 date);

void gnc_reconcile_view_postpone (GNCReconcileView *view);

void gnc_reconcile_view_unselect_all (GNCReconcileView *view);

gboolean gnc_reconcile_view_changed (GNCReconcileView *view);

G_END_DECLS

#endif /* GNC_RECONCILE_VIEW_H */
