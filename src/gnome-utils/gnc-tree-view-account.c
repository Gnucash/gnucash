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


/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_view_account_class_init (GncTreeViewAccountClass *klass);
static void gnc_tree_view_account_init (GncTreeViewAccount *view);
static void gnc_tree_view_account_finalize (GObject *object);
static void gnc_tree_view_account_destroy (GtkObject *object);


struct GncTreeViewAccountPrivate
{
  AccountViewInfo avi;
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
  {GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,       0, FALSE, 0.0, "placeholder",       N_("Placeholder")},
};


/************************************************************/
/*               g_object required functions                */
/************************************************************/

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
  view->priv = g_new0 (GncTreeViewAccountPrivate, 1);

  gnc_init_account_view_info(&view->priv->avi);
}

static void
gnc_tree_view_account_finalize (GObject *object)
{
  GncTreeViewAccount *account_view;

  ENTER("view %p", object);
  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (object));

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

  ENTER("view %p", object);
  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (object));

  account_view = GNC_TREE_VIEW_ACCOUNT (object);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
  LEAVE(" ");
}


/************************************************************
 *                        Callbacks                         *
 ************************************************************/
static void
gnc_tree_view_account_placeholder_toggled (GtkCellRendererToggle *cell,
					   gchar *path_str,
					   GncTreeViewAccount *tree_view)
{
	GtkTreePath *path;
	Account *account;
	gboolean placeholder;

	/* Change the requested account */
	path = gtk_tree_path_new_from_string (path_str);
	account = gnc_tree_view_account_get_account_from_path (tree_view, path);
	if (account) {
	  placeholder = !gtk_cell_renderer_toggle_get_active (cell); // hasn't changed yet.
	  xaccAccountSetPlaceholder (account, placeholder);
	}

	/* Clean up */
	gtk_tree_path_free (path);
}

/************************************************************/
/*                    New View Creation                     */
/************************************************************/

/*
 * Create a new account tree view with (optional) top level root node.
 * This view will be based on a model that is common to all view of
 * the same set of books, but will have its own private filter on that
 * model.
 */
GtkTreeView *
gnc_tree_view_account_new_with_group (AccountGroup *group, gboolean show_root)
{
  GncTreeViewAccount *account_view;
  GtkTreeView *tree_view;
  GtkTreeModel *model, *filter_model;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreePath *virtual_root_path = NULL;
  gint i;

  ENTER(" ");
  /* Create our view */
  account_view = g_object_new (GNC_TYPE_TREE_VIEW_ACCOUNT, NULL);
  tree_view = GTK_TREE_VIEW (account_view);

  /* Create/get a pointer to the existing model for this set of books. */
  model = gnc_tree_model_account_new (group);

  /* Set up the view private filter on the common model. */
  if (!show_root)
    virtual_root_path = gtk_tree_path_new_first ();
  filter_model = egg_tree_model_filter_new (model, virtual_root_path);
  gtk_object_sink(GTK_OBJECT(model));
  gtk_tree_view_set_model (tree_view, filter_model);
  g_object_unref(G_OBJECT(filter_model));
  if (virtual_root_path)
    gtk_tree_path_free(virtual_root_path);

  /* Set default visibilities */
  gtk_tree_view_set_headers_visible (tree_view, FALSE);
  //  gtk_tree_view_set_rules_hint (tree_view, TRUE);
  gnc_tree_view_account_init_view_info(&account_view->priv->avi);

  /* Set up the "account name" column */
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


  /* Set up all other columns */
  for (i = 1; i < GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER; i++) {
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


  /* Setup Placeholder column */
  renderer = gtk_cell_renderer_toggle_new ();
  column = gtk_tree_view_column_new_with_attributes (_("Placeholder"),
						     renderer,
						     "active", GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,
						     NULL);
  g_signal_connect (G_OBJECT (renderer), "toggled",
		    G_CALLBACK (gnc_tree_view_account_placeholder_toggled),
		    tree_view);
  gtk_tree_view_append_column (tree_view, column);
  gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
  gtk_tree_view_column_set_resizable (column, TRUE);
  gtk_tree_view_column_set_visible (column, FALSE);
  gtk_tree_view_column_set_min_width (column, 20 /* DRH - Should be based on title width */);


  LEAVE("%p", tree_view);
  return tree_view;
}

/*
 * Create a new account tree view with (optional) top level root node.
 * This view will be based on a model that is common to all view of
 * the same set of books, but will have its own private filter on that
 * model.
 */
GtkTreeView *
gnc_tree_view_account_new (gboolean show_root)
{
  AccountGroup *group;

  group = gnc_book_get_group (gnc_get_current_book ());
  return gnc_tree_view_account_new_with_group (group, show_root);
}

/************************************************************/
/*            Account Tree View Filter Functions            */
/************************************************************/

/*
 * Convert a column name to a numeric identifier.  This is a
 * helper routine for the following function.
 */
static gint
gnc_tree_view_account_pref_name_to_field (const char *pref_name)
{
  gint i;
  g_return_val_if_fail ((pref_name != NULL), GNC_TREE_MODEL_ACCOUNT_COL_NAME);

  for (i = 0; i < GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM; i++)
    if (safe_strcmp(gnc_tree_view_account_defaults[i].pref_name, pref_name) == 0)
      return i;
  return(GNC_TREE_MODEL_ACCOUNT_COL_NAME);
}

/*
 * Set the list of columns that will be visible in an account tree view.
 */
void
gnc_tree_view_account_configure_columns (GncTreeViewAccount *account_view,
					 GSList *column_names)
{
  AccountViewInfo new_avi;
  AccountFieldCode field;
  GSList *node;

  ENTER(" ");
  memset (&new_avi, 0, sizeof(new_avi));

  for (node = column_names; node != NULL; node = node->next)
  {
    field = gnc_tree_view_account_pref_name_to_field(node->data);
    if (field < GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM)
      new_avi.show_field[field] = TRUE;
  }

  new_avi.show_field[ACCOUNT_NAME] = TRUE;

  gnc_tree_view_account_set_view_info (account_view, &new_avi);
  LEAVE(" ");
}

/*
 * Initialize an account view info structure with default values.
 */
void
gnc_tree_view_account_init_view_info(AccountViewInfo *avi)
{
  int i;

  for (i = 0; i < GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM; i++)
    avi->show_field[i] = FALSE;

  avi->show_field[ACCOUNT_NAME] = TRUE;
}

/*
 * Get a copy of the account view info structure in use by the
 * specified tree.
 */
void
gnc_tree_view_account_get_view_info (GncTreeViewAccount *account_view,
				     AccountViewInfo *avi)
{
  GncTreeViewAccountPrivate *priv;

  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(account_view));
  priv = account_view->priv;

  *avi = priv->avi;
}

/*
 * Set the account view info data in use by the specified tree to
 * match the callers request.
 *
 * DRH - COMPATABILITY WARNING
 *
 * This function does not do anything with the 'include_type' field.
 * Should there be a automatic filter for backward compatability
 * that uses these flags, or should all uses of this be converted to
 * a eggtreemodelfilter?
 */
void
gnc_tree_view_account_set_view_info (GncTreeViewAccount *account_view,
				     AccountViewInfo *avi)
{
  GtkTreeViewColumn *column;
  gint i;

  ENTER("%p", account_view);
  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(account_view));

  account_view->priv->avi = *avi;
  
  for (i = 0; i < GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM; i++) {
    column = gtk_tree_view_get_column (GTK_TREE_VIEW(account_view), i);
    gtk_tree_view_column_set_visible (column, avi->show_field[i]);
  }

  LEAVE(" ");
}

/*
 * Set an eggtreemodel visible filter on this account.  This filter will be
 * called for each account that the tree is about to show, and the
 * account will be passed to the callback function.
 */
void
gnc_tree_view_account_set_filter (GncTreeViewAccount *account_view, 
				  EggTreeModelFilterVisibleFunc  func,
				  gpointer                       data,
				  GtkDestroyNotify               destroy)
{
  GtkTreeModel *filter_model;

  ENTER("view %p, filter func %p, data %p, destroy %p",
	account_view, func, data, destroy);
  filter_model = gtk_tree_view_get_model (GTK_TREE_VIEW (account_view));
  egg_tree_model_filter_set_visible_func (EGG_TREE_MODEL_FILTER (filter_model),
					  func, data, destroy);
  LEAVE(" ");
}

/*
 * Forces the entire account tree to be re-evaluated for visibility.
 */
void
gnc_tree_view_account_refilter (GncTreeViewAccount *view)
{
  GtkTreeModel *filter_model;

  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(view));

  filter_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
  egg_tree_model_filter_refilter (EGG_TREE_MODEL_FILTER (filter_model));
}


/************************************************************/
/*           Account Tree View Get/Set Functions            */
/************************************************************/

/*
 * Return the account associated with the top level pseudo-account for
 * the tree.
 */
Account *
gnc_tree_view_account_get_top_level (GncTreeViewAccount *view)
{
  GtkTreeModel *model, *filter_model;

  filter_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  model = egg_tree_model_filter_get_model (EGG_TREE_MODEL_FILTER (filter_model));

  return gnc_tree_model_account_get_toplevel (GNC_TREE_MODEL_ACCOUNT(model));
}

/*
 * Retrieve the selected account from an account tree view.  The
 * account tree must be in single selection mode.
 */
Account *
gnc_tree_view_account_get_account_from_path (GncTreeViewAccount *view,
					     GtkTreePath *path)
{
    GtkTreeModel *model;
    GtkTreeIter iter, child_iter;
    Account *account;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);

    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    if (!gtk_tree_model_get_iter (model, &iter, path)) {
      LEAVE("no iter");
      return NULL;
    }

    egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER (model),
						      &child_iter, &iter);
    account = child_iter.user_data;
    LEAVE("account %p (%s)", account, xaccAccountGetName (account));
    return account;
}

/*
 * Retrieve the selected account from an account tree view.  The
 * account tree must be in single selection mode.
 */
Account *
gnc_tree_view_account_get_selected_account (GncTreeViewAccount *view)
{
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter, child_iter;
    Account *account;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    if (!gtk_tree_selection_get_selected (selection, &model, &iter)) {
      LEAVE("no account, get_selected failed");
      return FALSE;
    }

    egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER (model),
						      &child_iter, &iter);
    account = child_iter.user_data;
    LEAVE("account %p (%s)", account, xaccAccountGetName (account));
    return account;
}

/*
 * Selects a single account in the account tree view.  The account
 * tree must be in single selection mode.
 */
void
gnc_tree_view_account_set_selected_account (GncTreeViewAccount *view,
					    Account *account)
{
  GtkTreeModel *model, *filter_model;
  GtkTreeSelection *selection;
  GtkTreePath *path, *filter_path, *parent_path;

  ENTER("view %p, account %p (%s)", view,
	account, xaccAccountGetName (account));

  /* Clear any existing selection. */
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  gtk_tree_selection_unselect_all (selection);

  filter_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  model = egg_tree_model_filter_get_model (EGG_TREE_MODEL_FILTER (filter_model));

  path = gnc_tree_model_account_get_path_from_account (GNC_TREE_MODEL_ACCOUNT(model), account);
  if (path == NULL) {
    LEAVE("get_path_from_account failed");
    return;
  }
  {
    gchar *path_string = gtk_tree_path_to_string(path);
    DEBUG("tree path %s", path_string);
    g_free(path_string);
  }
  filter_path =
    egg_tree_model_filter_convert_child_path_to_path (EGG_TREE_MODEL_FILTER (filter_model),
						      path);
  gtk_tree_path_free(path);
  if (filter_path == NULL) {
    LEAVE("convert_child_path_to_path failed");
    return;
  }

  /* gtk_tree_view requires that a row be visible before it can be selected */
  parent_path = gtk_tree_path_copy (filter_path);
  if (gtk_tree_path_up (parent_path)) {
    /* This function is misnamed.  It expands the actual item
     * specified, not the path to the item specified. I.E. It expands
     * one level too many, thus the get of the parent. */
    gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), parent_path);
  }
  gtk_tree_path_free(parent_path);

  gtk_tree_selection_select_path (selection, filter_path);
  gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), filter_path, NULL, FALSE, 0.0, 0.0);
  {
    gchar *path_string = gtk_tree_path_to_string(filter_path);
    LEAVE("filter path %s", path_string);
    g_free(path_string);
  }
  gtk_tree_path_free(filter_path);
}

/*
 * This helper function is called once for each row in the tree view
 * that is currently selected.  Its task is to an the corresponding
 * account to the end of a glist.
 */
static void
get_selected_accounts_helper (GtkTreeModel *model,
			      GtkTreePath *path,
			      GtkTreeIter *iter,
			      gpointer data)
{
  GList **return_list = data;
  Account *account;

  account = iter->user_data;
  *return_list = g_list_append(*return_list, account);
}

/*
 * Given an account tree view, return a list of the selected accounts. The
 * account tree must be in multiple selection mode.
 *
 * Note: It is the responsibility of the caller to free the returned
 * list.
 */
GList *
gnc_tree_view_account_get_selected_accounts (GncTreeViewAccount *view)
{
  GtkTreeSelection *selection;
  GList *return_list = NULL;

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
  gtk_tree_selection_selected_foreach(selection, get_selected_accounts_helper, &return_list);
  return return_list;
}

/*
 * Given an account tree view and a list of accounts, select those
 * accounts in the tree view.
 */
void
gnc_tree_view_account_set_selected_accounts (GncTreeViewAccount *view,
					     GList *account_list,
					     gboolean show_last)
{
  GtkTreeModel *model;
  GtkTreePath *path, *parent_path;
  GtkTreeSelection *selection;
  GList *element;
  Account *account;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));

  /* Clear any existing selection. */
  gtk_tree_selection_unselect_all (selection);
  gtk_tree_view_collapse_all (GTK_TREE_VIEW(view));

  /* Now go select what the user requested. */
  for (element = account_list; element; ) {
    account = element->data;
    element = g_list_next(element);

    path = gnc_tree_model_account_get_path_from_account (GNC_TREE_MODEL_ACCOUNT(model), account);
    if (path == NULL) {
      /*
       * Oops.  Someone must have deleted this account and not cleaned
       * up all references to it.
       */
      continue;
    }

    /* gtk_tree_view requires that a row be visible before it can be selected */
    parent_path = gtk_tree_path_copy (path);
    if (gtk_tree_path_up (parent_path)) {
      /* This function is misnamed.  It expands the actual item
       * specified, not the path to the item specified. I.E. It
       * expands one level too many, thus the get of the parent. */
      gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), parent_path);
    }
    gtk_tree_path_free(parent_path);

    gtk_tree_selection_select_path (selection, path);
    if (show_last && (element == NULL))
      gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), path, NULL, FALSE, 0.0, 0.0);
    gtk_tree_path_free(path);
  }
}
