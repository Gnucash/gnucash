/********************************************************************\
 * gnc-tree-view-account.c -- GtkTreeView implementation to display *
 *                            accounts in a GtkTreeView.            *
 * Copyright (C) 2003 David Hampton <hampton@employees.org>         *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>

#include "gnc-tree-model-account.h"
#include "gnc-tree-view-account.h"

#include "Account.h"
#include "Group.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-icons.h"
#include "gnc-ui-util.h"
#include "messages.h"

#define TREE_VIEW_ACCOUNT_CM_CLASS "tree-view-account"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static void gnc_tree_view_account_class_init (GncTreeViewAccountClass *klass);
static void gnc_tree_view_account_init (GncTreeViewAccount *view);
static void gnc_tree_view_account_finalize (GObject *object);
static void gnc_tree_view_account_destroy (GtkObject *object);

#if 0
static void gnc_tree_model_account_refresh_handler (GHashTable *changes,
						    gpointer data);
static void gnc_tree_model_account_refresh (GncTreeModelAccount *model);
#endif

struct GncTreeViewAccountPrivate
{
  AccountViewInfo avi;
  gboolean has_filter;
};

typedef struct _gnc_tree_view_account_default {
  GncTreeModelAccountColumn column;
  GncTreeModelAccountColumn color_column;
  gboolean auto_resize;
  gfloat x_alignment;
  const char *pref_name;
  const char *field_name;
} gnc_tree_view_account_default;
static gnc_tree_view_account_default gnc_tree_view_account_defaults[] = {
  {GNC_TREE_MODEL_ACCOUNT_COL_NAME,              0, TRUE,  0.0, "name",              N_("Account Name")},
  {GNC_TREE_MODEL_ACCOUNT_COL_TYPE,              0, FALSE, 0.0, "type",              N_("Type")},
  {GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY,         0, FALSE, 0.0, "commodity",         N_("Commodity")},
  {GNC_TREE_MODEL_ACCOUNT_COL_CODE,              0, FALSE, 0.0, "code",              N_("Account Code")},
  {GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,       0, TRUE,  0.0, "description",       N_("Description")},
  {GNC_TREE_MODEL_ACCOUNT_COL_PRESENT,           GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,    TRUE,  1.0, "present",           N_("Present")},
  {GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT,    GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,    TRUE,  1.0, "present_report",    N_("Present (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_BALANCE,           GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,    TRUE,  1.0, "balance",           N_("Balance")},
  {GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT,    GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,    TRUE,  1.0, "balance_report",    N_("Balance (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_CLEARED,           GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,    TRUE,  1.0, "cleared",           N_("Cleared")},
  {GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT,    GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,    TRUE,  1.0, "cleared_report",    N_("Cleared (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED,        GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED, TRUE,  1.0, "reconciled",        N_("Reconciled")},
  {GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT, GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED, TRUE,  1.0, "reconciled_report", N_("Reconciled (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN,        GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN, TRUE,  1.0, "future_min",        N_("Future Minimum")},
  {GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT, GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN, TRUE,  1.0, "future_min_report", N_("Future Minimum (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_TOTAL,             GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,      TRUE,  1.0, "total",             N_("Total")},
  {GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT,      GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,      TRUE,  1.0, "total_report",      N_("Total (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_NOTES,             0, FALSE, 0.0, "notes",             N_("Notes")},
  {GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO,          0, FALSE, 0.0, "tax-info",          N_("Tax Info")},
};


static GObjectClass *parent_class = NULL;

GType
gnc_tree_view_account_get_type (void)
{
	static GType gnc_tree_view_account_type = 0;

	if (gnc_tree_view_account_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeViewAccountClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_tree_view_account_class_init,
			NULL,
			NULL,
			sizeof (GncTreeViewAccount),
			0,
			(GInstanceInitFunc) gnc_tree_view_account_init
		};
		
		gnc_tree_view_account_type = g_type_register_static (GTK_TYPE_TREE_VIEW,
								     "GncTreeViewAccount",
								     &our_info, 0);
	}

	return gnc_tree_view_account_type;
}

static void
gnc_tree_view_account_class_init (GncTreeViewAccountClass *klass)
{
	GObjectClass *o_class;
	GtkObjectClass *object_class;

	parent_class = g_type_class_peek_parent (klass);

	o_class = G_OBJECT_CLASS (klass);
	object_class = GTK_OBJECT_CLASS (klass);

	/* GObject signals */
	o_class->finalize = gnc_tree_view_account_finalize;

	/* GtkObject signals */
	object_class->destroy = gnc_tree_view_account_destroy;
}

static void
gnc_tree_view_account_init (GncTreeViewAccount *view)
{
  ENTER(" ");
  view->priv = g_new0 (GncTreeViewAccountPrivate, 1);

  gnc_init_account_view_info(&view->priv->avi);
  LEAVE(" ");
}

static void
gnc_tree_view_account_finalize (GObject *object)
{
  GncTreeViewAccount *account_view;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (object));

  ENTER("view %p", object);
  account_view = GNC_TREE_VIEW_ACCOUNT (object);
  g_free (account_view->priv);

  if (G_OBJECT_CLASS (parent_class)->finalize)
    (* G_OBJECT_CLASS (parent_class)->finalize) (object);
  LEAVE(" ");
}

static void
gnc_tree_view_account_destroy (GtkObject *object)
{
  GncTreeViewAccount *account_view;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (object));

  ENTER("view %p", object);
  account_view = GNC_TREE_VIEW_ACCOUNT (object);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
  LEAVE(" ");
}

static void
gnc_tree_view_update_column_visibility_internal (GncTreeViewAccount *account_view)
{
  GtkTreeView *tree_view;
  GtkTreeViewColumn *column;
  gint i;

  ENTER(" ");
  tree_view = GTK_TREE_VIEW (account_view);

  for (i = 0; i < NUM_ACCOUNT_FIELDS; i++) {
    DEBUG("setting column %d to %s", i, account_view->priv->avi.show_field[i] ? "visible" : "invisible");
    column = gtk_tree_view_get_column (tree_view, i);
    gtk_tree_view_column_set_visible (column, account_view->priv->avi.show_field[i]);
  }
  LEAVE(" ");
}

static GtkTreeView *
gnc_tree_view_account_new_internal (gboolean filterable)
{
  GncTreeViewAccount *account_view;
  GtkTreeView *tree_view;
  GtkTreeModel *model, *filter_model;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  gint i;

  ENTER(" ");
  account_view = g_object_new (GNC_TYPE_TREE_VIEW_ACCOUNT, NULL);
  tree_view = GTK_TREE_VIEW (account_view);

  model = gnc_tree_model_account_new (gnc_book_get_group (gnc_get_current_book ()));
  if (filterable) {
    filter_model = egg_tree_model_filter_new (model, NULL);
    gtk_tree_view_set_model (tree_view, filter_model);
  } else {
    gtk_tree_view_set_model (tree_view, model);
  }
  gtk_object_sink(GTK_OBJECT(model));
  account_view->priv->has_filter = filterable;

  /* Set column visibilities */
  gnc_tree_view_init_default_visibility(&account_view->priv->avi);

  /* Account name */
  column = gtk_tree_view_column_new ();
  gtk_tree_view_column_set_title (column, gettext(gnc_tree_view_account_defaults[0].field_name));
  renderer = gtk_cell_renderer_pixbuf_new ();
  g_object_set (renderer, "stock-id", GNC_STOCK_ACCOUNT, NULL);
  gtk_tree_view_column_pack_start (column, renderer, FALSE);
  renderer = gtk_cell_renderer_text_new ();
  gtk_tree_view_column_pack_start (column, renderer, FALSE);
  gtk_tree_view_column_add_attribute (column,
				      renderer,
				      "text", gnc_tree_view_account_defaults[0].column);
  gtk_tree_view_append_column (tree_view, column);
  gtk_tree_view_column_set_resizable (column, TRUE);
  gtk_tree_view_set_expander_column (tree_view, column);

  /* All other columns */
  for (i = 1; i < NUM_ACCOUNT_FIELDS; i++) {
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (gettext(gnc_tree_view_account_defaults[i].field_name),
						       renderer,
						       "text", gnc_tree_view_account_defaults[i].column,
						       NULL);
    if (gnc_tree_view_account_defaults[i].color_column)
      gtk_tree_view_column_add_attribute (column, renderer,
					  "foreground", gnc_tree_view_account_defaults[i].color_column);
    if (gnc_tree_view_account_defaults[i].x_alignment)
      gtk_tree_view_column_add_attribute (column, renderer,
					  "xalign", GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT);

    gtk_tree_view_append_column (tree_view, column);
    gtk_tree_view_column_set_sizing (column,
				     gnc_tree_view_account_defaults[i].auto_resize
				     ? GTK_TREE_VIEW_COLUMN_AUTOSIZE
				     : GTK_TREE_VIEW_COLUMN_FIXED);
    gtk_tree_view_column_set_visible (column, account_view->priv->avi.show_field[i]);
    gtk_tree_view_column_set_alignment (column, gnc_tree_view_account_defaults[i].x_alignment);
    gtk_tree_view_column_set_min_width (column, 50 /* DRH - Should be based on title width */);
    gtk_tree_view_column_set_resizable (column, TRUE);
  }

#if 0
  priv->component_id = gnc_register_gui_component (TREE_MODEL_ACCOUNT_CM_CLASS,
  						 gnc_tree_model_account_refresh_handler,
  						 NULL,
  						 model);
  
  gnc_gui_component_watch_entity_type (priv->component_id,
  				     GNC_ID_ACCOUNT,
  				     GNC_EVENT_CREATE | GNC_EVENT_DESTROY);
#endif

  LEAVE("%p", tree_view);
  return tree_view;
}

GtkTreeView *
gnc_tree_view_account_new (void)
{
  return gnc_tree_view_account_new_internal (FALSE);
}

GtkTreeView *
gnc_tree_view_account_new_filterable (void)
{
  return gnc_tree_view_account_new_internal (TRUE);
}

/********************************************************************\
 * gnc_init_account_view_info                                       *
 *   initialize an account view info structure with default values  *
 *                                                                  *
 * Args: avi - structure to initialize                              *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_tree_view_init_default_visibility(AccountViewInfo *avi)
{
  int i;

  for (i = 0; i < NUM_ACCOUNT_FIELDS; i++)
    avi->show_field[i] = FALSE;

  avi->show_field[ACCOUNT_NAME] = TRUE;
  avi->show_field[ACCOUNT_DESCRIPTION] = TRUE;
  avi->show_field[ACCOUNT_TOTAL] = TRUE;
}

void
gnc_tree_view_update_column_visibility (GncTreeViewAccount *account_view,
					AccountViewInfo *avi)
{
  GncTreeViewAccountPrivate *priv;

  ENTER("%p", account_view);
  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(account_view));
  priv = account_view->priv;

  priv->avi = *avi;
  
  gnc_tree_view_update_column_visibility_internal (account_view);
  LEAVE(" ");
}

static gint
gnc_tree_view_account_pref_name_to_field (const char *pref_name)
{
  gint i;
  g_return_val_if_fail ((pref_name != NULL), GNC_TREE_MODEL_ACCOUNT_COL_NAME);

  for (i = 0; i < NUM_ACCOUNT_FIELDS; i++)
    if (safe_strcmp(gnc_tree_view_account_defaults[i].pref_name, pref_name) == 0)
      return i;
  return(GNC_TREE_MODEL_ACCOUNT_COL_NAME);
}

void
gnc_tree_view_account_configure_columns (GncTreeViewAccount *account_view, GSList *list)
{
  AccountViewInfo new_avi;
  AccountFieldCode field;
  GSList *node;

  ENTER(" ");
  memset (&new_avi, 0, sizeof(new_avi));

  for (node = list; node != NULL; node = node->next)
  {
    field = gnc_tree_view_account_pref_name_to_field(node->data);
    if (field < NUM_ACCOUNT_FIELDS)
      new_avi.show_field[field] = TRUE;
  }

  new_avi.show_field[ACCOUNT_NAME] = TRUE;

  gnc_tree_view_update_column_visibility (account_view, &new_avi);
  LEAVE(" ");
}

void
gnc_tree_view_account_set_filter (GncTreeViewAccount *account_view, 
				  EggTreeModelFilterVisibleFunc  func,
				  gpointer                       data,
				  GtkDestroyNotify               destroy)
{
  GtkTreeModel *filter_model;

  g_return_if_fail (account_view->priv->has_filter == TRUE);

  filter_model = gtk_tree_view_get_model (GTK_TREE_VIEW (account_view));
  egg_tree_model_filter_set_visible_func (EGG_TREE_MODEL_FILTER (filter_model),
					  func, data, destroy);
}

gboolean
gnc_tree_view_account_get_selected_account (GncTreeViewAccount *view,
					    Account **account)
{
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter, child_iter;

    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), FALSE);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
      return FALSE;

    if (!view->priv->has_filter) {
      *account = iter.user_data;
      return TRUE;
    }

    egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER (model),
						      &child_iter, &iter);
    account = child_iter.user_data;
    return TRUE;
}

#if 0
static void
gnc_tree_view_account_refresh_handler (GHashTable *changes, gpointer user_data)
{
	ENTER("changes %p, view %p", changes, user_data);
	g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (user_data));

	gnc_tree_view_account_refresh (GNC_TREE_VIEW_ACCOUNT (user_data));
	LEAVE(" ");
}

static void
gnc_tree_view_account_refresh (GncTreeViewAccount *view)
{
	GtkTreePath *path;
	gint i;
	
	ENTER("view %p", view);
	if (view->priv->root == NULL) {
		return;
	}

	path = gtk_tree_path_new_first ();
	if (view->priv->toplevel != NULL) {
		gtk_tree_path_append_index (path, 0);
	}
	for (i = 0; i < xaccGroupGetNumAccounts (view->priv->root); i++) {
		gtk_tree_view_row_deleted (GTK_TREE_VIEW (view), path);
	}
	gtk_tree_path_free (path);

	xaccGroupForEachAccount (view->priv->root, account_row_inserted, view, TRUE);
	LEAVE(" ");
}
#endif

