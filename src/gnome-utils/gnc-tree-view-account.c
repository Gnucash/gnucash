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
#include "gnc-tree-view-common.h"

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
static GList *active_views = NULL;

/** Declarations *********************************************************/
static void gnc_tree_view_account_class_init (GncTreeViewAccountClass *klass);
static void gnc_tree_view_account_init (GncTreeViewAccount *view);
static void gnc_tree_view_account_finalize (GObject *object);
static void gnc_tree_view_account_destroy (GtkObject *object);

struct GncTreeViewAccountPrivate
{
  AccountViewInfo avi;
};

/* Defined at the end of the file */
static gnc_view_column gnc_tree_view_account_defaults[];


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

#if DEBUG_REFERENCE_COUNTING
static void
dump_view (GncTreeViewAccount *view, gpointer dummy)
{
    g_warning("GncTreeViewAccount %p still exists.", view);
}

static gint
gnc_tree_view_account_report_references (void)
{
  g_list_foreach(active_views, (GFunc)dump_view, NULL);
  return 0;
}
#endif

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

#if DEBUG_REFERENCE_COUNTING
	gtk_quit_add (0,
		      (GtkFunction)gnc_tree_view_account_report_references,
		      NULL);
#endif
}

/********************************************************************\
 * gnc_init_account_view_info                                       *
 *   initialize an account view info structure with default values  *
 *                                                                  *
 * Args: avi - structure to initialize                              *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_init_account_view_info(AccountViewInfo *avi)
{
  int i;

  for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
    avi->include_type[i] = TRUE;

  for (i = 0; i < NUM_ACCOUNT_FIELDS; i++)
    avi->show_field[i] = FALSE;

  avi->show_field[ACCOUNT_NAME] = TRUE;
  avi->show_field[ACCOUNT_DESCRIPTION] = TRUE;
  avi->show_field[ACCOUNT_TOTAL] = TRUE;
}

static void
gnc_tree_view_account_init (GncTreeViewAccount *view)
{
  view->priv = g_new0 (GncTreeViewAccountPrivate, 1);

  gnc_init_account_view_info(&view->priv->avi);

  active_views = g_list_append (active_views, view);
}

static void
gnc_tree_view_account_finalize (GObject *object)
{
  GncTreeViewAccount *account_view;

  ENTER("view %p", object);
  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (object));

  account_view = GNC_TREE_VIEW_ACCOUNT (object);
  active_views = g_list_remove (active_views, account_view);

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
					   const gchar *s_path_str,
					   gpointer user_data)
{
	GncTreeViewAccount *tree_view;
	GtkTreePath *s_path;
	Account *account;
	gboolean placeholder;

	/* Change the requested account */
	tree_view = user_data;
	s_path = gtk_tree_path_new_from_string (s_path_str);
	account = gnc_tree_view_account_get_account_from_path (tree_view, s_path);
	if (account) {
	  placeholder = !gtk_cell_renderer_toggle_get_active (cell); // hasn't changed yet.
	  xaccAccountSetPlaceholder (account, placeholder);
	}

	/* Clean up */
	gtk_tree_path_free (s_path);
}


/************************************************************/
/*                      sort functions                      */
/************************************************************/

static gint
sort_by_xxx_value (xaccGetBalanceInCurrencyFn fn,
		   gboolean recurse,
		   GtkTreeModel *f_model,
		   GtkTreeIter *f_iter_a,
		   GtkTreeIter *f_iter_b,
		   gpointer user_data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  Account *account;
  gnc_numeric balance_a, balance_b;

  model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));

  /* Get balance 1 */
  egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER(f_model),
						    &iter,
						    f_iter_a);
  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
  balance_a = gnc_ui_account_get_balance_full(fn, account, recurse, NULL, NULL);

  /* Get balance 2 */
  egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER(f_model),
						    &iter,
						    f_iter_b);
  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
  balance_b = gnc_ui_account_get_balance_full(fn, account, recurse, NULL, NULL);

  return gnc_numeric_compare(balance_a, balance_b);
}

static gint
sort_by_present_value (GtkTreeModel *f_model,
		       GtkTreeIter *f_iter_a,
		       GtkTreeIter *f_iter_b,
		       gpointer user_data)
{
  return sort_by_xxx_value (xaccAccountGetPresentBalanceInCurrency, FALSE,
			    f_model, f_iter_a, f_iter_b, user_data);
}

static gint
sort_by_balance_value (GtkTreeModel *f_model,
		       GtkTreeIter *f_iter_a,
		       GtkTreeIter *f_iter_b,
		       gpointer user_data)
{
  return sort_by_xxx_value (xaccAccountGetBalanceInCurrency, FALSE,
			    f_model, f_iter_a, f_iter_b, user_data);
}

static gint
sort_by_cleared_value (GtkTreeModel *f_model,
		       GtkTreeIter *f_iter_a,
		       GtkTreeIter *f_iter_b,
		       gpointer user_data)
{
  return sort_by_xxx_value (xaccAccountGetClearedBalanceInCurrency, FALSE,
			    f_model, f_iter_a, f_iter_b, user_data);
}

static gint
sort_by_reconciled_value (GtkTreeModel *f_model,
			  GtkTreeIter *f_iter_a,
			  GtkTreeIter *f_iter_b,
			  gpointer user_data)
{
  return sort_by_xxx_value (xaccAccountGetReconciledBalanceInCurrency, FALSE,
			    f_model, f_iter_a, f_iter_b, user_data);
}

static gint
sort_by_future_min_value (GtkTreeModel *f_model,
			  GtkTreeIter *f_iter_a,
			  GtkTreeIter *f_iter_b,
			  gpointer user_data)
{
  return sort_by_xxx_value (xaccAccountGetProjectedMinimumBalanceInCurrency, FALSE,
			    f_model, f_iter_a, f_iter_b, user_data);
}

static gint
sort_by_total_value (GtkTreeModel *f_model,
		     GtkTreeIter *f_iter_a,
		     GtkTreeIter *f_iter_b,
		     gpointer user_data)
{
  return sort_by_xxx_value (xaccAccountGetBalanceInCurrency, TRUE,
			    f_model, f_iter_a, f_iter_b, user_data);
}

static gint
sort_by_placeholder (GtkTreeModel *f_model,
		     GtkTreeIter *f_iter_a,
		     GtkTreeIter *f_iter_b,
		     gpointer user_data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  Account *account;
  gboolean flag_a, flag_b;

  model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));

  /* Get balance 1 */
  egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER(f_model),
						    &iter,
						    f_iter_a);
  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
  flag_a = xaccAccountGetPlaceholder(account);

  /* Get balance 2 */
  egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER(f_model),
						    &iter,
						    f_iter_b);
  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
  flag_b = xaccAccountGetPlaceholder(account);

  if (flag_a < flag_b)
    return -1;
  else if (flag_a == flag_b)
    return 0;
  return 1;
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
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *virtual_root_path = NULL;

  ENTER(" ");
  /* Create our view */
  account_view = g_object_new (GNC_TYPE_TREE_VIEW_ACCOUNT, NULL);
  tree_view = GTK_TREE_VIEW (account_view);

  /* Create/get a pointer to the existing model for this set of books. */
  model = gnc_tree_model_account_new (group);

  /* Set up the view private filter layer on the common model. */
  if (!show_root)
    virtual_root_path = gtk_tree_path_new_first ();
  f_model = egg_tree_model_filter_new (model, virtual_root_path);
  gtk_object_sink(GTK_OBJECT(model));
  if (virtual_root_path)
    gtk_tree_path_free(virtual_root_path);

  /* Set up the view private sort layer on the common model. */
  s_model = gtk_tree_model_sort_new_with_model(f_model);
  gtk_tree_view_set_model (tree_view, s_model);
  g_object_unref(G_OBJECT(f_model));

  /* Set default visibilities */
  gtk_tree_view_set_headers_visible (tree_view, FALSE);
  //  gtk_tree_view_set_rules_hint (tree_view, TRUE);
  gnc_tree_view_account_init_view_info(&account_view->priv->avi);

  gnc_tree_view_common_create_columns (tree_view, "Accounts", GNC_STOCK_ACCOUNT,
				       gnc_tree_view_account_defaults);

  gtk_widget_show(GTK_WIDGET(tree_view));
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
/*                   Auxiliary Functions                    */
/************************************************************/

#define debug_path(fn, path) {				\
    gchar *path_string = gtk_tree_path_to_string(path); \
    fn("tree path %s", path_string);			\
    g_free(path_string);				\
  }

#if 0
static GtkTreePath *
gnc_tree_view_account_get_path_from_account (GncTreeViewAccount *view,
					     Account *account)
{
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *path, *f_path, *s_path;

  ENTER("view %p, account %p (%s)", view, account, xaccAccountGetName(account));

  if (account == NULL) {
    LEAVE("no account");
    return NULL;
  }

  /* Reach down to the real model and get a path for this account */
  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));
  path = gnc_tree_model_account_get_path_from_account (GNC_TREE_MODEL_ACCOUNT(model), account);
  if (path == NULL) {
    LEAVE("no path");
    return NULL;
  }

  /* convert back to a filtered path */
  f_path = egg_tree_model_filter_convert_child_path_to_path (EGG_TREE_MODEL_FILTER (f_model), path);
  gtk_tree_path_free(path);
  if (!f_path) {
    LEAVE("no filter path");
    return NULL;
  }

  /* convert back to a sorted path */
  s_path = egg_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model), f_path);
  gtk_tree_path_free(f_path);
  debug_path(LEAVE, s_path);
  return s_path;
}
#endif

static gboolean
gnc_tree_view_account_get_iter_from_account (GncTreeViewAccount *view,
					     Account *account,
					     GtkTreeIter *s_iter)
{
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreeIter iter, f_iter;

  g_return_val_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(view), FALSE);
  g_return_val_if_fail(account != NULL, FALSE);
  g_return_val_if_fail(s_iter != NULL, FALSE);
  
  ENTER("view %p, account %p (%s)", view, account, xaccAccountGetName(account));

  /* Reach down to the real model and get an iter for this account */
  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));
  if (!gnc_tree_model_account_get_iter_from_account (GNC_TREE_MODEL_ACCOUNT(model), account, &iter)) {
    LEAVE("model_get_iter_from_account failed");
    return FALSE;
  }

  /* convert back to a sort iter */
  egg_tree_model_filter_convert_child_iter_to_iter (EGG_TREE_MODEL_FILTER(f_model),
						    &f_iter, &iter);
  gtk_tree_model_sort_convert_child_iter_to_iter (GTK_TREE_MODEL_SORT(s_model),
						  s_iter, &f_iter);
  LEAVE(" ");
  return TRUE;
}

gint
gnc_tree_view_account_count_children (GncTreeViewAccount *view,
				      Account *account)
{
  GtkTreeModel *s_model;
  GtkTreeIter s_iter;
  gint num_children;

  ENTER("view %p, account %p (%s)", view, account, xaccAccountGetName(account));

  if (account == NULL) {
    LEAVE("no account");
    return 0;
  }

  if (!gnc_tree_view_account_get_iter_from_account (view, account, &s_iter)) {
    LEAVE("view_get_iter_from_account failed");
    return 0;
  }

  /* Any children? */
  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  num_children = gtk_tree_model_iter_n_children(s_model, &s_iter);
  LEAVE("%d children", num_children);
  return num_children;
}


Account *
gnc_tree_view_account_get_account_from_column (GtkTreeViewColumn *column,
					       GtkTreeModel *s_model,
					       GtkTreeIter  *s_iter)
{
  GtkTreeModel *model, *f_model;
  GtkTreeIter iter, f_iter;
  Account *account;

  g_return_val_if_fail (GTK_IS_TREE_VIEW_COLUMN(column), NULL);
  g_return_val_if_fail (GTK_IS_TREE_MODEL_SORT(s_model), NULL);
  g_return_val_if_fail (s_iter != NULL, NULL);

  ENTER("column %p, s_model %p, s_iter %p", column, s_model, s_iter);
  gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT(s_model),
						  &f_iter,
						  s_iter);
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER(f_model),
						    &iter,
						    &f_iter);
  model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));
  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
  LEAVE("account %p (%s)", account, xaccAccountGetName (account));
  return account;
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

  for (i = 0; i <= GNC_TREE_MODEL_ACCOUNT_COL_LAST_VISIBLE; i++)
    if (safe_strcmp(gnc_tree_view_account_defaults[i].pref_name, pref_name) == 0)
      return i;
  return(GNC_TREE_MODEL_ACCOUNT_COL_NAME);
}

const char *
gnc_tree_view_account_get_field_name (AccountFieldCode field)
{
  g_return_val_if_fail ((field >= 0) && (field <= GNC_TREE_MODEL_ACCOUNT_COL_LAST_VISIBLE), NULL);

  return(gettext(gnc_tree_view_account_defaults[field].field_name));
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

  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(account_view));

  ENTER(" ");
  memset (&new_avi, 0, sizeof(new_avi));

  for (node = column_names; node != NULL; node = node->next)
  {
    field = gnc_tree_view_account_pref_name_to_field(node->data);
    if (field <= GNC_TREE_MODEL_ACCOUNT_COL_LAST_VISIBLE)
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

  for (i = 0; i <= GNC_TREE_MODEL_ACCOUNT_COL_LAST_VISIBLE; i++)
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
  g_return_if_fail(avi != NULL);

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
  g_return_if_fail(avi != NULL);

  account_view->priv->avi = *avi;
  
  for (i = 0; i <= GNC_TREE_MODEL_ACCOUNT_COL_LAST_VISIBLE; i++) {
    column = gtk_tree_view_get_column (GTK_TREE_VIEW(account_view), i);
    gtk_tree_view_column_set_visible (column, avi->show_field[i]);
  }

  LEAVE(" ");
}

typedef struct {
  gnc_tree_view_account_filter_func user_fn;
  gpointer                          user_data;
  GtkDestroyNotify                  user_destroy;
} filter_user_data;

static void
gnc_tree_view_account_filter_destroy (gpointer data)
{
  filter_user_data *fd = data;

  if (fd->user_destroy)
    fd->user_destroy(fd->user_data);
  g_free(fd);
}

static gboolean
gnc_tree_view_account_filter_helper (GtkTreeModel *model,
				     GtkTreeIter *iter,
				     gpointer data)
{
  Account *account;
  filter_user_data *fd = data;

  g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), FALSE);
  g_return_val_if_fail (iter != NULL, FALSE);

  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), iter);
  return fd->user_fn(account, fd->user_data);
}

/*
 * Set an eggtreemodel visible filter on this account.  This filter will be
 * called for each account that the tree is about to show, and the
 * account will be passed to the callback function.
 */
void
gnc_tree_view_account_set_filter (GncTreeViewAccount *view,
				  gnc_tree_view_account_filter_func func,
				  gpointer data,
				  GtkDestroyNotify destroy)
{
  GtkTreeModel *f_model, *s_model;
  filter_user_data *fd = data;

  ENTER("view %p, filter func %p, data %p, destroy %p",
	view, func, data, destroy);

  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(view));
  g_return_if_fail(func != NULL);

  fd = g_malloc(sizeof(filter_user_data));
  fd->user_fn      = func;
  fd->user_data    = data;
  fd->user_destroy = destroy;

  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  egg_tree_model_filter_set_visible_func (EGG_TREE_MODEL_FILTER (f_model),
					  gnc_tree_view_account_filter_helper,
					  fd,
					  gnc_tree_view_account_filter_destroy);

  /* Whack any existing levels. The top two levels have been created
   * before this routine can be called. */
  egg_tree_model_filter_refilter (EGG_TREE_MODEL_FILTER (f_model));
  LEAVE(" ");
}

/*
 * Forces the entire account tree to be re-evaluated for visibility.
 */
void
gnc_tree_view_account_refilter (GncTreeViewAccount *view)
{
  GtkTreeModel *f_model, *s_model;

  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(view));

  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  egg_tree_model_filter_refilter (EGG_TREE_MODEL_FILTER (f_model));
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
  GtkTreeModel *model, *f_model, *s_model;

  g_return_val_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(view), NULL);

  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));

  return gnc_tree_model_account_get_toplevel (GNC_TREE_MODEL_ACCOUNT(model));
}

/*
 * Retrieve the selected account from an account tree view.  The
 * account tree must be in single selection mode.
 */
Account *
gnc_tree_view_account_get_account_from_path (GncTreeViewAccount *view,
					     GtkTreePath *s_path)
{
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreePath *path, *f_path;
    GtkTreeIter iter;
    Account *account;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);
    g_return_val_if_fail (s_path != NULL, NULL);
    
    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_path = gtk_tree_model_sort_convert_path_to_child_path (GTK_TREE_MODEL_SORT (s_model), s_path);
    if (!f_path) {
      LEAVE("no filter path");
      return NULL;
    }

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    path = egg_tree_model_filter_convert_path_to_child_path (EGG_TREE_MODEL_FILTER (f_model), f_path);
    gtk_tree_path_free(f_path);
    if (!path) {
      LEAVE("no path");
      return NULL;
    }

    model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));
    if (!gtk_tree_model_get_iter (model, &iter, path)) {
      LEAVE("no iter");
      return NULL;
    }

    account = iter.user_data;
    gtk_tree_path_free(path);
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
    GtkTreeModel *f_model, *s_model;
    GtkTreeIter iter, f_iter, s_iter;
    Account *account;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    if (!gtk_tree_selection_get_selected (selection, &s_model, &s_iter)) {
      LEAVE("no account, get_selected failed");
      return FALSE;
    }

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
						    &f_iter, &s_iter);

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER (f_model),
						      &iter, &f_iter);

    account = iter.user_data;
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
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *path, *f_path, *s_path, *parent_path;
  GtkTreeSelection *selection;

  ENTER("view %p, account %p (%s)", view,
	account, xaccAccountGetName (account));
  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));

  /* Clear any existing selection. */
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  gtk_tree_selection_unselect_all (selection);

  if (account == NULL)
    return;

  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));

  path = gnc_tree_model_account_get_path_from_account (GNC_TREE_MODEL_ACCOUNT(model), account);
  if (path == NULL) {
    LEAVE("no path");
    return;
  }
  debug_path(DEBUG, path);

  f_path = egg_tree_model_filter_convert_child_path_to_path (EGG_TREE_MODEL_FILTER (f_model),
							     path);
  gtk_tree_path_free(path);
  if (f_path == NULL) {
    LEAVE("no filter path");
    return;
  }
  debug_path(DEBUG, f_path);

  s_path = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model),
							   f_path);
  gtk_tree_path_free(f_path);
  if (s_path == NULL) {
    LEAVE("no sort path");
    return;
  }

  /* gtk_tree_view requires that a row be visible before it can be selected */
  parent_path = gtk_tree_path_copy (s_path);
  if (gtk_tree_path_up (parent_path)) {
    /* This function is misnamed.  It expands the actual item
     * specified, not the path to the item specified. I.E. It expands
     * one level too many, thus the get of the parent. */
    gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), parent_path);
  }
  gtk_tree_path_free(parent_path);

  gtk_tree_selection_select_path (selection, s_path);
  gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), s_path, NULL, FALSE, 0.0, 0.0);
  debug_path(LEAVE, s_path);
  gtk_tree_path_free(s_path);
}

/*
 * This helper function is called once for each row in the tree view
 * that is currently selected.  Its task is to an the corresponding
 * account to the end of a glist.
 */
static void
get_selected_accounts_helper (GtkTreeModel *s_model,
			      GtkTreePath *s_path,
			      GtkTreeIter *s_iter,
			      gpointer data)
{
  GList **return_list = data;
  GtkTreeModel *f_model;
  GtkTreeIter iter, f_iter;
  Account *account;

  gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
						  &f_iter, s_iter);

  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER (f_model),
						    &iter, &f_iter);
  account = iter.user_data;
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

  g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);

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
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *path, *f_path, *s_path, *parent_path;
  GtkTreeSelection *selection;
  GList *element;
  Account *account;

  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));

  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));

  /* Clear any existing selection. */
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
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

    f_path = egg_tree_model_filter_convert_child_path_to_path (EGG_TREE_MODEL_FILTER (f_model),
							       path);
    gtk_tree_path_free(path);
    if (f_path == NULL)
      continue;

    s_path = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model),
							     f_path);
    gtk_tree_path_free(f_path);
    if (s_path == NULL)
      continue;

    /* gtk_tree_view requires that a row be visible before it can be selected */
    parent_path = gtk_tree_path_copy (s_path);
    if (gtk_tree_path_up (parent_path)) {
      /* This function is misnamed.  It expands the actual item
       * specified, not the path to the item specified. I.E. It
       * expands one level too many, thus the get of the parent. */
      gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), parent_path);
    }
    gtk_tree_path_free(parent_path);

    gtk_tree_selection_select_path (selection, s_path);
    if (show_last && (element == NULL))
      gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), s_path, NULL, FALSE, 0.0, 0.0);
    gtk_tree_path_free(s_path);
  }
}

/*
 * Selects all sub-accounts of an acccount.
 */
void
gnc_tree_view_account_select_subaccounts (GncTreeViewAccount *view,
					  Account *account)
{
  GtkTreeModel *s_model;
  GtkTreeSelection *selection;
  GtkTreePath *sp_account, *sp_start, *sp_end;
  GtkTreeIter si_account, si_start, si_end;
  gint num_children;

  ENTER("view %p, account %p (%s)", view, account, xaccAccountGetName(account));

  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));

  if (account == NULL) {
    LEAVE("no account");
    return;
  }

  if (!gnc_tree_view_account_get_iter_from_account (view, account, &si_account)) {
    LEAVE("view_get_iter_from_account failed");
    return;
  }

  /* Any children? */
  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  num_children = gtk_tree_model_iter_n_children(s_model, &si_account);
  if (num_children == 0) {
    LEAVE("no children");
    return;
  }

  /* Expand the tree.  Required for selection to work */
  sp_account = gtk_tree_model_get_path (s_model, &si_account);
  gtk_tree_view_expand_row (GTK_TREE_VIEW(view), sp_account, TRUE);

  /* compute start/end paths */
  gtk_tree_model_iter_nth_child (s_model, &si_start, &si_account, 0);
  gtk_tree_model_iter_nth_child (s_model, &si_end, &si_account, num_children - 1);
  sp_start = gtk_tree_model_get_path (s_model, &si_start);
  sp_end = gtk_tree_model_get_path (s_model, &si_end);

  /* select everything between */
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  gtk_tree_selection_select_range (selection, sp_start, sp_end);

  /* clean up */
  gtk_tree_path_free(sp_start);
  gtk_tree_path_free(sp_end);
  gtk_tree_path_free(sp_account);
  LEAVE(" ");
  return;
}

/*
 * Retrieve the account currently under the cursor.
 */
Account *
gnc_tree_view_account_get_cursor_account (GncTreeViewAccount *view)
{
    GtkTreeModel *s_model;
    GtkTreePath *s_path;
    Account *account;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    gtk_tree_view_get_cursor (GTK_TREE_VIEW(view), &s_path, NULL);
    if (!s_path) {
      LEAVE("no account");
      return NULL;
    }

    account = gnc_tree_view_account_get_account_from_path (view, s_path);
    gtk_tree_path_free(s_path);
    LEAVE("account %p (%s)", account, xaccAccountGetName (account));
    return account;
}


/************************************************************/
/*         Account Tree View Add Column Functions           */
/************************************************************/

static void
account_cell_kvp_data_func (GtkTreeViewColumn *tree_column,
			    GtkCellRenderer *cell,
			    GtkTreeModel *s_model,
			    GtkTreeIter *s_iter,
			    gpointer key)
{
	GtkTreeModel *model, *f_model;
	GtkTreeIter iter, f_iter;
	Account *account;
	kvp_frame * frame;

	g_return_if_fail (GTK_IS_TREE_MODEL_SORT (s_model));

	gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
							&f_iter, s_iter);

	f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
	egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER(f_model),
							  &iter,
							  &f_iter);

	model = egg_tree_model_filter_get_model(EGG_TREE_MODEL_FILTER(f_model));
	account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
	frame = xaccAccountGetSlots(account);

	g_object_set (G_OBJECT (cell),
		      "text", kvp_frame_get_string(frame, (gchar *)key),
		      "xalign", 0.0,
		      NULL);
}


void
gnc_tree_view_account_add_kvp_column (GncTreeViewAccount *view,
				      const gchar *column_title,
				      const gchar *kvp_key)
{
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
  g_return_if_fail (kvp_key != NULL);

  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 1.0, NULL);
  column = gtk_tree_view_column_new_with_attributes (column_title,
						     renderer,
						     NULL);
  gtk_tree_view_column_set_cell_data_func (column, renderer, 
					   account_cell_kvp_data_func,
					   g_strdup(kvp_key), g_free);
  gtk_tree_view_append_column (GTK_TREE_VIEW(view), column);
}


/************************************************************/
/*                    Column Definitions                    */
/************************************************************/

static gnc_view_column gnc_tree_view_account_defaults[] = {
  {GNC_TREE_MODEL_ACCOUNT_COL_NAME,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   0.0, GNC_TREE_VIEW_COLUMN_ALIGN_NONE,
   NULL,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "name",
   N_("Account Name")},
  {GNC_TREE_MODEL_ACCOUNT_COL_TYPE,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   0.0, GNC_TREE_VIEW_COLUMN_ALIGN_NONE,
   NULL,
   GTK_TREE_VIEW_COLUMN_FIXED,
   FALSE, NULL,
   "type",
   N_("Type")},
  {GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   0.0, GNC_TREE_VIEW_COLUMN_ALIGN_NONE,
   NULL,
   GTK_TREE_VIEW_COLUMN_FIXED,
   FALSE, NULL,
   "commodity",
   N_("Commodity")},
  {GNC_TREE_MODEL_ACCOUNT_COL_CODE,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   0.0, GNC_TREE_VIEW_COLUMN_ALIGN_NONE,
   NULL,
   GTK_TREE_VIEW_COLUMN_FIXED,
   FALSE, NULL,
   "code",
   N_("Account Code")},
  {GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   0.0, GNC_TREE_VIEW_COLUMN_ALIGN_NONE,
   NULL,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "description",
   N_("Description")},
  {GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   NULL,
   FALSE,
   FALSE, NULL,
   "lastnum",
   N_("Last Num")},
  {GNC_TREE_MODEL_ACCOUNT_COL_PRESENT,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_present_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "present",
   N_("Present")},
  {GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_present_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "present_report",
   N_("Present (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_BALANCE,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_balance_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "balance",
   N_("Balance")},
  {GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_balance_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "balance_report",
   N_("Balance (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_CLEARED,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_cleared_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "cleared",
   N_("Cleared")},
  {GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_cleared_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "cleared_report",
   N_("Cleared (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_reconciled_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "reconciled",
   N_("Reconciled")},
  {GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_reconciled_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "reconciled_report",
   N_("Reconciled (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_future_min_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "future_min",
   N_("Future Minimum")},
  {GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_future_min_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "future_min_report",
   N_("Future Minimum (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_TOTAL,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_total_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "total",
   N_("Total")},
  {GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,
   1.0, GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
   sort_by_total_value,
   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
   FALSE, NULL,
   "total_report",
   N_("Total (Report)")},
  {GNC_TREE_MODEL_ACCOUNT_COL_NOTES,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   0.0, GNC_TREE_VIEW_COLUMN_ALIGN_NONE,
   NULL,
   FALSE,
   FALSE, NULL,
   "notes",
   N_("Notes")},
  {GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   0.0, GNC_TREE_VIEW_COLUMN_ALIGN_NONE,
   NULL,
   FALSE,
   FALSE, NULL,
   "tax-info",
   N_("Tax Info")},
  {GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,
   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
   0.0, GNC_TREE_VIEW_COLUMN_ALIGN_NONE,
   sort_by_placeholder,
   FALSE,
   TRUE, gnc_tree_view_account_placeholder_toggled,
   "placeholder",
   N_("Placeholder")},
  { 0 }
};
