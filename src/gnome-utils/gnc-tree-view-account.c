/********************************************************************\
 * gnc-tree-view-account.c -- GtkTreeView implementation to display *
 *                            accounts in a GtkTreeView.            *
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>    *
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

#include "gnc-tree-view.h"
#include "gnc-tree-model-account.h"
#include "gnc-tree-view-account.h"

#include "Account.h"
#include "Group.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-icons.h"
#include "gnc-ui-util.h"
#include "messages.h"


#define SAMPLE_ACCOUNT_VALUE "$1,000,000.00"

/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_view_account_class_init (GncTreeViewAccountClass *klass);
static void gnc_tree_view_account_init (GncTreeViewAccount *view);
static void gnc_tree_view_account_finalize (GObject *object);
static void gnc_tree_view_account_dispose (GObject *object);

static gboolean gnc_tree_view_account_filter_helper (GtkTreeModel *model,
                                                     GtkTreeIter *iter,
                                                     gpointer data);

typedef struct GncTreeViewAccountPrivate
{
    AccountViewInfo avi;

    gnc_tree_view_account_filter_func filter_fn;
    gpointer                          filter_data;
    GtkDestroyNotify                  filter_destroy;
} GncTreeViewAccountPrivate;

#define GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccountPrivate))


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
		
		gnc_tree_view_account_type = g_type_register_static (
                    GNC_TYPE_TREE_VIEW, "GncTreeViewAccount", &our_info, 0);
	}

	return gnc_tree_view_account_type;
}

static void
gnc_tree_view_account_class_init (GncTreeViewAccountClass *klass)
{
	GObjectClass *o_class;

	parent_class = g_type_class_peek_parent (klass);

	/* GObject signals */
	o_class = G_OBJECT_CLASS (klass);
	o_class->finalize = gnc_tree_view_account_finalize;
        o_class->dispose = gnc_tree_view_account_dispose;

	g_type_class_add_private(klass, sizeof(GncTreeViewAccountPrivate));
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
}

static void
gnc_tree_view_account_init (GncTreeViewAccount *view)
{
  GncTreeViewAccountPrivate *priv;

  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(view);
  gnc_init_account_view_info(&priv->avi);
}

static void
gnc_tree_view_account_finalize (GObject *object)
{
  GncTreeViewAccount *account_view;
  GncTreeViewAccountPrivate *priv;

  ENTER("view %p", object);
  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (object));

  account_view = GNC_TREE_VIEW_ACCOUNT (object);

  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(account_view);
  if (priv->filter_destroy) {
      priv->filter_destroy(priv->filter_data);
      priv->filter_destroy = NULL;
  }
  priv->filter_fn = NULL;

  if (G_OBJECT_CLASS (parent_class)->finalize)
    (* G_OBJECT_CLASS (parent_class)->finalize) (object);
  LEAVE(" ");
}

static void
gnc_tree_view_account_dispose (GObject *object)
{
  GncTreeViewAccount *view;

  ENTER("view %p", object);
  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (object));

  view = GNC_TREE_VIEW_ACCOUNT (object);
  gnc_tree_view_set_model(GNC_TREE_VIEW(view), NULL);

  if (G_OBJECT_CLASS (parent_class)->dispose)
    (* G_OBJECT_CLASS (parent_class)->dispose) (object);
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

  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));

  /* Get balance 1 */
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    &iter,
						    f_iter_a);
  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
  balance_a = gnc_ui_account_get_balance_full(fn, account, recurse, NULL, NULL);

  /* Get balance 2 */
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
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

  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));

  /* Get balance 1 */
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    &iter,
						    f_iter_a);
  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
  flag_a = xaccAccountGetPlaceholder(account);

  /* Get balance 2 */
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    &iter,
						    f_iter_b);
  account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), &iter);
  flag_b = xaccAccountGetPlaceholder(account);

  if (flag_a > flag_b)
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
  GncTreeView *view;
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *virtual_root_path = NULL;
  const gchar *sample_type, *sample_commodity;

  ENTER(" ");
  /* Create our view */
  view = g_object_new (GNC_TYPE_TREE_VIEW_ACCOUNT,
                       "name", "account_tree", NULL);

  /* Create/get a pointer to the existing model for this set of books. */
  model = gnc_tree_model_account_new (group);

  /* Set up the view private filter layer on the common model. */
  if (!show_root)
    virtual_root_path = gtk_tree_path_new_first ();
  f_model = gtk_tree_model_filter_new (model, virtual_root_path);
  /* A GncTreeModelAccount is based on a GncTreeModel, which is a
   * GtkObject that provides a GtkTreeModel interface.  This
   * underlying model should probably be converted to a GObject at
   * some point, eliminating the need for the call to sink().*/
  gtk_object_sink(GTK_OBJECT(model));
  if (virtual_root_path)
    gtk_tree_path_free(virtual_root_path);

  /* Set up the view private sort layer on the common model. */
  s_model = gtk_tree_model_sort_new_with_model(f_model);
  g_object_unref(G_OBJECT(f_model));
  gnc_tree_view_set_model (view, s_model);
  g_object_unref(G_OBJECT(s_model));

  /* Set default visibilities */
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(view), FALSE);

  sample_type = xaccAccountGetTypeStr(CREDIT);
  sample_commodity = gnc_commodity_get_fullname(gnc_default_currency());

  gnc_tree_view_add_text_column(view, N_("Account Name"), "name",
				GNC_STOCK_ACCOUNT, "Expenses:Entertainment",
				GNC_TREE_MODEL_ACCOUNT_COL_NAME,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				NULL);
  gnc_tree_view_add_text_column(view, N_("Type"), "type", NULL, sample_type,
				GNC_TREE_MODEL_ACCOUNT_COL_TYPE,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				NULL);
  gnc_tree_view_add_text_column(view, N_("Commodity"), "commodity", NULL,
				sample_commodity,
				GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				NULL);
  gnc_tree_view_add_text_column(view, N_("Account Code"), "account-code", NULL,
				"1-123-1234",
				GNC_TREE_MODEL_ACCOUNT_COL_CODE,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				NULL);
  gnc_tree_view_add_text_column(view, N_("Description"), "description", NULL,
				"Sample account description.",
				GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				NULL);
  gnc_tree_view_add_numeric_column(view, N_("Last Num"), "lastnum", "12345",
				   GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM,
				   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   NULL);
  gnc_tree_view_add_numeric_column(view, N_("Present"), "present",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_PRESENT,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_present_value);
  gnc_tree_view_add_numeric_column(view, N_("Present (Report)"), "present_report",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_present_value);
  gnc_tree_view_add_numeric_column(view, N_("Balance"), "balance",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_BALANCE,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_balance_value);
  gnc_tree_view_add_numeric_column(view, N_("Balance (Report)"), "balance_report",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_balance_value);
  gnc_tree_view_add_numeric_column(view, N_("Cleared"), "cleared",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_CLEARED,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_cleared_value);
  gnc_tree_view_add_numeric_column(view, N_("Cleared (Report)"), "cleared_report",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_cleared_value);
  gnc_tree_view_add_numeric_column(view, N_("Reconciled"), "reconciled",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_reconciled_value);
  gnc_tree_view_add_numeric_column(view, N_("Reconciled (Report)"), "reconciled_report",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_reconciled_value);
  gnc_tree_view_add_numeric_column(view, N_("Future Minimum"), "future_min",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_future_min_value);
  gnc_tree_view_add_numeric_column(view, N_("Future Minimum (Report)"), "future_min_report",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_future_min_value);
  gnc_tree_view_add_numeric_column(view, N_("Total"), "total",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_TOTAL,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_total_value);
  gnc_tree_view_add_numeric_column(view, N_("Total (Report)"), "total_report",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_total_value);
  gnc_tree_view_add_text_column(view, N_("Notes"), "notes", NULL,
				"Sample account notes.",
				GNC_TREE_MODEL_ACCOUNT_COL_NOTES,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				NULL);
  gnc_tree_view_add_text_column(view, N_("Tax Info"), "tax-info", NULL,
				"Sample tax info.",
				GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				NULL);
  gnc_tree_view_add_toggle_column(view, N_("Placeholder"), "P", "placeholder",
				  GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,
				  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				  sort_by_placeholder,
				  gnc_tree_view_account_placeholder_toggled);

  gnc_tree_view_configure_columns(view, "description", "total", NULL);
  gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER (f_model),
					  gnc_tree_view_account_filter_helper,
					  view,
					  NULL);

  gtk_widget_show(GTK_WIDGET(view));
  LEAVE("%p", view);
  return GTK_TREE_VIEW(view);
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
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  path = gnc_tree_model_account_get_path_from_account (GNC_TREE_MODEL_ACCOUNT(model), account);
  if (path == NULL) {
    LEAVE("no path");
    return NULL;
  }

  /* convert back to a filtered path */
  f_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_FILTER (f_model), path);
  gtk_tree_path_free(path);
  if (!f_path) {
    LEAVE("no filter path");
    return NULL;
  }

  /* convert back to a sorted path */
  s_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model), f_path);
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
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  if (!gnc_tree_model_account_get_iter_from_account (
          GNC_TREE_MODEL_ACCOUNT(model), account, &iter)) {
    LEAVE("model_get_iter_from_account failed");
    return FALSE;
  }

  /* convert back to a sort iter */
  gtk_tree_model_filter_convert_child_iter_to_iter (
      GTK_TREE_MODEL_FILTER(f_model), &f_iter, &iter);
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


/************************************************************/
/*            Account Tree View Filter Functions            */
/************************************************************/

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

  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(account_view);

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
 * a GtkTreeModelFilter?
 *
 * CAS - For now, I'll try the automatic filter approach by making
 * this function use GtkTreeModelFilter.
 */
void
gnc_tree_view_account_set_view_info (GncTreeViewAccount *account_view,
				     AccountViewInfo *avi)
{
  GncTreeViewAccountPrivate *priv;
  gint i;
  guint sel_bits = 0;

  ENTER("%p", account_view);
  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(account_view));
  g_return_if_fail(avi != NULL);

  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(account_view);
  priv->avi = *avi;
  
  for (i = 0; i < NUM_ACCOUNT_TYPES; i++) {
      sel_bits |= avi->include_type[i] ? (1 << i): 0;
  }

  /* FIXME: if we want to allow a truly empty bitfield, we'll have to fix
     the callers who don't set the include_type fields. */
  if (sel_bits) {
      gnc_tree_view_account_set_filter(
          account_view, gnc_tree_view_account_filter_by_type_selection, 
          GUINT_TO_POINTER(sel_bits), NULL);
  }

  LEAVE(" ");
}

static gboolean
gnc_tree_view_account_filter_helper (GtkTreeModel *model,
				     GtkTreeIter *iter,
				     gpointer data)
{
  Account *account;
  GncTreeViewAccount *view = data;
  GncTreeViewAccountPrivate *priv;

  g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), FALSE);
  g_return_val_if_fail (iter != NULL, FALSE);

  account = gnc_tree_model_account_get_account (
      GNC_TREE_MODEL_ACCOUNT(model), iter);
  
  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(view);
  if (priv->filter_fn)
      return priv->filter_fn(account, priv->filter_data);
  else return TRUE;
}

/*
 * Set an GtkTreeModel visible filter on this account.  This filter will be
 * called for each account that the tree is about to show, and the
 * account will be passed to the callback function.
 *
 * Use NULL as func to remove filter. 
 */
void
gnc_tree_view_account_set_filter (GncTreeViewAccount *view,
				  gnc_tree_view_account_filter_func func,
				  gpointer data,
				  GtkDestroyNotify destroy)
{
  GncTreeViewAccountPrivate *priv;

  ENTER("view %p, filter func %p, data %p, destroy %p",
	view, func, data, destroy);

  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(view));

  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(view);
  if (priv->filter_destroy) {
      priv->filter_destroy(priv->filter_data);
  }
  priv->filter_destroy = destroy;
  priv->filter_data = data;
  priv->filter_fn = func;  

  gnc_tree_view_account_refilter(view);
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
  gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (f_model));
}

gboolean 
gnc_tree_view_account_filter_by_type_selection(Account* acct, gpointer data)
{
    GNCAccountType acct_type;
    guint sel_bits = GPOINTER_TO_UINT(data);

    g_return_val_if_fail(GNC_IS_ACCOUNT(acct), FALSE);
    acct_type = xaccAccountGetType(acct);

    /* Because of some silly '== TRUE' comparisons in treemodelfilter,
       we have to return exactly TRUE */
    return (sel_bits & (1 << acct_type)) ? TRUE : FALSE;
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
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));

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
    f_path = gtk_tree_model_sort_convert_path_to_child_path (
        GTK_TREE_MODEL_SORT (s_model), s_path);
    if (!f_path) {
      LEAVE("no filter path");
      return NULL;
    }

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    path = gtk_tree_model_filter_convert_path_to_child_path (
        GTK_TREE_MODEL_FILTER (f_model), f_path);
    gtk_tree_path_free(f_path);
    if (!path) {
      LEAVE("no path");
      return NULL;
    }

    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    if (!gtk_tree_model_get_iter (model, &iter, path)) {
      LEAVE("no iter");
      return NULL;
    }

    account = iter.user_data;
    gtk_tree_path_free(path);
    LEAVE("account %p (%s)", account, xaccAccountGetName (account));
    return account;
}


Account *
gnc_tree_view_account_get_account_from_iter (GtkTreeModel *s_model,
					     GtkTreeIter  *s_iter)
{
  GtkTreeModel *model, *f_model;
  GtkTreeIter iter, f_iter;
  Account *account;

  g_return_val_if_fail (GTK_IS_TREE_MODEL_SORT(s_model), NULL);
  g_return_val_if_fail (s_iter != NULL, NULL);

  gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT(s_model),
						  &f_iter,
						  s_iter);
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  gtk_tree_model_filter_convert_iter_to_child_iter (
      GTK_TREE_MODEL_FILTER(f_model), &iter, &f_iter);
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  account = gnc_tree_model_account_get_account (
      GNC_TREE_MODEL_ACCOUNT(model), &iter);
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
    gtk_tree_model_filter_convert_iter_to_child_iter (
        GTK_TREE_MODEL_FILTER (f_model), &iter, &f_iter);

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
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));

  path = gnc_tree_model_account_get_path_from_account (
      GNC_TREE_MODEL_ACCOUNT(model), account);
  if (path == NULL) {
    LEAVE("no path");
    return;
  }
  debug_path(DEBUG, path);

  f_path = gtk_tree_model_filter_convert_child_path_to_path (
      GTK_TREE_MODEL_FILTER (f_model), path);
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
 * that is currently selected.  Its task is to append the corresponding
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
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (f_model),
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
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));

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

    f_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_FILTER (f_model),
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

/* This function implements a custom mapping between an account's KVP
 * and the cell renderer's 'text' property. */
static void
account_cell_kvp_data_func (GtkTreeViewColumn *tree_column,
			    GtkCellRenderer *cell,
			    GtkTreeModel *s_model,
			    GtkTreeIter *s_iter,
			    gpointer key)
{
    Account *account;
    kvp_frame * frame;
    
    g_return_if_fail (GTK_IS_TREE_MODEL_SORT (s_model));
    account = gnc_tree_view_account_get_account_from_iter(s_model, s_iter);
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
    GList *list;

    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    g_return_if_fail (kvp_key != NULL);

    column = gnc_tree_view_add_text_column(GNC_TREE_VIEW(view), column_title,
					   kvp_key, NULL, "Sample text",
					   -1, -1, NULL);

    /* This new kvp column has only had one renderer added to it so
     * far.  Find that renderer. */
    list = gtk_tree_view_column_get_cell_renderers(column);
    renderer = list->data;
    g_list_free(list);
    g_object_set (G_OBJECT (renderer), "xalign", 1.0, NULL);

    gtk_tree_view_column_set_cell_data_func (column, renderer, 
					     account_cell_kvp_data_func,
					     g_strdup(kvp_key), g_free);
}

static void col_edited_helper(GtkCellRendererText *cell, gchar *path_string, 
                              gchar *new_text, gpointer _s_model)
{
    Account *account;
    GtkTreeModel *s_model;
    GtkTreeIter s_iter;
    GncTreeViewAccountColumnTextEdited col_edited_cb;
    GtkTreeViewColumn *col;

    col_edited_cb = g_object_get_data(G_OBJECT(cell), 
                                      "column_edited_callback");
    col = GTK_TREE_VIEW_COLUMN(g_object_get_data(G_OBJECT(cell), 
                                                 "column_view"));
    s_model = GTK_TREE_MODEL(_s_model);

    if (!gtk_tree_model_get_iter_from_string(s_model, &s_iter, path_string))
        return;
        
    account = gnc_tree_view_account_get_account_from_iter(s_model, &s_iter);
    col_edited_cb(account, col, new_text);
}

static void col_source_helper(GtkTreeViewColumn *col, GtkCellRenderer *cell,
                              GtkTreeModel *s_model, GtkTreeIter *s_iter,
                              gpointer _col_source_cb)
{
    Account *account;
    gchar *text;
    GncTreeViewAccountColumnSource col_source_cb;
    
    g_return_if_fail (GTK_IS_TREE_MODEL_SORT (s_model));
    col_source_cb = (GncTreeViewAccountColumnSource) _col_source_cb;
    account = gnc_tree_view_account_get_account_from_iter(s_model, s_iter);
    text = col_source_cb(account, col, cell);
    g_object_set (G_OBJECT (cell), "text", text, "xalign", 1.0, NULL);
    g_free(text);
}

 
GtkTreeViewColumn *
gnc_tree_view_account_add_custom_column(GncTreeViewAccount *account_view,
                                        const gchar *column_title,
                                        GncTreeViewAccountColumnSource
                                        col_source_cb,
                                        GncTreeViewAccountColumnTextEdited
                                        col_edited_cb)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeModel *s_model;
    
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (account_view), NULL);
    
    renderer = gtk_cell_renderer_text_new ();
    g_object_set (G_OBJECT (renderer), "xalign", 1.0, NULL);
    
    column = gtk_tree_view_column_new_with_attributes (column_title,
                                                       renderer, NULL);
    if (col_edited_cb) {
        g_object_set(G_OBJECT(renderer), "editable", TRUE, NULL);
        g_object_set_data(G_OBJECT(renderer), "column_edited_callback",
                          col_edited_cb);
        s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(account_view));
        g_signal_connect(G_OBJECT(renderer), "edited", 
                         (GCallback) col_edited_helper, s_model);
        g_object_set_data(G_OBJECT(renderer), "column_view", column);
    }
    gtk_tree_view_column_set_cell_data_func (column, renderer, 
                                             col_source_helper,
                                             col_source_cb, NULL);
    gnc_tree_view_append_column (GNC_TREE_VIEW(account_view), column);
    return column;
}
