/********************************************************************\
 * gnc-tree-view-account.c -- GtkTreeView implementation to display *
 *                            accounts in a GtkTreeView.            *
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org> *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-view.h"
#include "gnc-tree-model-account.h"
#include "gnc-tree-model-account-types.h"
#include "gnc-tree-view-account.h"

#include "Account.h"
#include "gnc-accounting-period.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-glib-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-hooks.h"
#include "gnc-session.h"
#include "gnc-icons.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"
#include "window-main-summarybar.h"

#define SAMPLE_ACCOUNT_VALUE "$1,000,000.00"

/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_view_account_class_init (GncTreeViewAccountClass *klass);
static void gnc_tree_view_account_init (GncTreeViewAccount *view);
static void gnc_tree_view_account_finalize (GObject *object);

static void gtva_update_column_names (GncTreeView *view);
static void gtva_currency_changed_cb (void);

static gboolean gnc_tree_view_account_filter_helper (GtkTreeModel *model,
                                                     GtkTreeIter *iter,
                                                     gpointer data);

static void gtva_setup_column_renderer_edited_cb(GncTreeViewAccount *account_view,
                                                 GtkTreeViewColumn *column,
                                                 GtkCellRenderer *renderer,
                                                 GncTreeViewAccountColumnTextEdited col_edited_cb);

typedef struct GncTreeViewAccountPrivate
{
    AccountViewInfo avi;

    gnc_tree_view_account_filter_func filter_fn;
    gpointer                          filter_data;
    GtkDestroyNotify                  filter_destroy;

  GtkTreeViewColumn *name_column;
  GtkTreeViewColumn *code_column;
  GtkTreeViewColumn *desc_column;
  GtkTreeViewColumn *present_report_column;
  GtkTreeViewColumn *balance_report_column;
  GtkTreeViewColumn *cleared_report_column;
  GtkTreeViewColumn *reconciled_report_column;
  GtkTreeViewColumn *future_min_report_column;
  GtkTreeViewColumn *total_report_column;
  GtkTreeViewColumn *notes_column;
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
                    GNC_TYPE_TREE_VIEW, GNC_TREE_VIEW_ACCOUNT_NAME,
		    &our_info, 0);
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

	g_type_class_add_private(klass, sizeof(GncTreeViewAccountPrivate));

	gnc_hook_add_dangler(HOOK_CURRENCY_CHANGED,
			     (GFunc)gtva_currency_changed_cb, NULL);
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

static GtkTreeModel *
sort_cb_setup_w_iters (GtkTreeModel *f_model,
		       GtkTreeIter *f_iter_a,
		       GtkTreeIter *f_iter_b,
		       GtkTreeIter *iter_a,
		       GtkTreeIter *iter_b,
		       const Account **account_a,
		       const Account **account_b)
{
  GtkTreeModel *model;

  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    iter_a,
						    f_iter_a);
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    iter_b,
						    f_iter_b);
  *account_a = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), iter_a);
  *account_b = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT(model), iter_b);
  return model;
}

static void
sort_cb_setup (GtkTreeModel *f_model,
	       GtkTreeIter *f_iter_a,
	       GtkTreeIter *f_iter_b,
	       const Account **account_a,
	       const Account **account_b)
{
  GtkTreeIter iter_a, iter_b;

  sort_cb_setup_w_iters (f_model, f_iter_a, f_iter_b,
			 &iter_a, &iter_b, account_a, account_b);
}

static gint
sort_by_string (GtkTreeModel *f_model,
		GtkTreeIter *f_iter1,
		GtkTreeIter *f_iter2,
		gpointer user_data)
{
  GtkTreeModel *model;
  GtkTreeIter iter1, iter2;
  const Account *account1, *account2;
  gchar *str1, *str2;
  gint column = GPOINTER_TO_INT(user_data);
  gint result;

  model = sort_cb_setup_w_iters(f_model, f_iter1, f_iter2, &iter1, &iter2, &account1, &account2);

  /* Get the strings. */
  gtk_tree_model_get(GTK_TREE_MODEL(model), &iter1,  column, &str1, -1);
  gtk_tree_model_get(GTK_TREE_MODEL(model), &iter2,  column, &str2, -1);

  result = safe_utf8_collate(str1, str2);
  g_free(str1);
  g_free(str2);
  if (result != 0)
    return result;
  return xaccAccountOrder(account1, account2);
}

static gint
sort_by_code (GtkTreeModel *f_model,
	      GtkTreeIter *f_iter_a,
	      GtkTreeIter *f_iter_b,
	      gpointer user_data)
{
  const Account *account_a, *account_b;

  sort_cb_setup (f_model, f_iter_a, f_iter_b, &account_a, &account_b);

  /* Default ordering uses this column first. */
  return xaccAccountOrder(account_a, account_b);
}

static gint
sort_by_xxx_value (xaccGetBalanceInCurrencyFn fn,
		   gboolean recurse,
		   GtkTreeModel *f_model,
		   GtkTreeIter *f_iter_a,
		   GtkTreeIter *f_iter_b,
		   gpointer user_data)
{
  const Account *account_a, *account_b;
  gnc_numeric balance_a, balance_b;
  gint result;

  /* Find the accounts */
  sort_cb_setup (f_model, f_iter_a, f_iter_b, &account_a, &account_b);

  /* Get balances */
  balance_a = gnc_ui_account_get_balance_full(fn, account_a, recurse, NULL, NULL);
  balance_b = gnc_ui_account_get_balance_full(fn, account_b, recurse, NULL, NULL);

  result = gnc_numeric_compare(balance_a, balance_b);
  if (result != 0)
    return result;
  return xaccAccountOrder(account_a, account_b);
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
  const Account *account_a, *account_b;
  gboolean flag_a, flag_b;

  /* Find the accounts */
  sort_cb_setup (f_model, f_iter_a, f_iter_b, &account_a, &account_b);

  /* Get the placeholder flags. */
  flag_a = xaccAccountGetPlaceholder(account_a);
  flag_b = xaccAccountGetPlaceholder(account_b);

  if (flag_a > flag_b)
    return -1;
  else if (flag_a < flag_b)
    return 1;
  return xaccAccountOrder(account_a, account_b);
}

static gint
sort_by_xxx_period_value (GtkTreeModel *f_model,
			  GtkTreeIter *f_iter_a,
			  GtkTreeIter *f_iter_b,
			  gboolean recurse)
{
  Account *acct1, *acct2;
  time_t t1, t2;
  gnc_numeric b1, b2;
  gint result;

  sort_cb_setup (f_model, f_iter_a, f_iter_b,
		 (const Account **)&acct1, (const Account **)&acct2);

  t1 = gnc_accounting_period_fiscal_start();
  t2 = gnc_accounting_period_fiscal_end();

  b1 = xaccAccountGetBalanceChangeForPeriod(acct1, t1, t2, recurse);
  b2 = xaccAccountGetBalanceChangeForPeriod(acct2, t1, t2, recurse);

  result = gnc_numeric_compare(b1, b2);
  if (result != 0)
    return result;
  return xaccAccountOrder(acct1, acct2);
}

static gint
sort_by_balance_period_value (GtkTreeModel *f_model,
			     GtkTreeIter *f_iter_a,
			     GtkTreeIter *f_iter_b,
			     gpointer user_data)
{
  return sort_by_xxx_period_value (f_model, f_iter_a, f_iter_b, FALSE);
}

static gint
sort_by_total_period_value (GtkTreeModel *f_model,
			     GtkTreeIter *f_iter_a,
			     GtkTreeIter *f_iter_b,
			     gpointer user_data)
{
  return sort_by_xxx_period_value (f_model, f_iter_a, f_iter_b, TRUE);
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
gnc_tree_view_account_new_with_root (Account *root, gboolean show_root)
{
  GncTreeView *view;
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *virtual_root_path = NULL;
  const gchar *sample_type, *sample_commodity;
  GncTreeViewAccountPrivate *priv;

  ENTER(" ");
  /* Create our view */
  view = g_object_new (GNC_TYPE_TREE_VIEW_ACCOUNT,
                       "name", "account_tree", NULL);

  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(GNC_TREE_VIEW_ACCOUNT (view));

  /* Create/get a pointer to the existing model for this set of books. */
  model = gnc_tree_model_account_new (root);

  /* Set up the view private filter layer on the common model. */
  if (!show_root)
    virtual_root_path = gtk_tree_path_new_first ();
  f_model = gtk_tree_model_filter_new (model, virtual_root_path);
  /* A GncTreeModelAccount is based on a GncTreeModel, which is a
   * GObject that provides a GtkTreeModel interface. */
  g_object_unref(G_OBJECT(model));
  if (virtual_root_path)
    gtk_tree_path_free(virtual_root_path);

  /* Set up the view private sort layer on the common model. */
  s_model = gtk_tree_model_sort_new_with_model(f_model);
  g_object_unref(G_OBJECT(f_model));
  gnc_tree_view_set_model (view, s_model);
  g_object_unref(G_OBJECT(s_model));

  /* Set default visibilities */
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(view), FALSE);

  sample_type = xaccAccountGetTypeStr(ACCT_TYPE_CREDIT);
  sample_commodity = gnc_commodity_get_fullname(gnc_default_currency());

  priv->name_column 
    = gnc_tree_view_add_text_column(view, _("Account Name"), "name",
                                    GNC_STOCK_ACCOUNT, "Expenses:Entertainment",
                                    GNC_TREE_MODEL_ACCOUNT_COL_NAME,
                                    GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                    sort_by_string);
  gnc_tree_view_add_text_column(view, _("Type"), "type", NULL, sample_type,
				GNC_TREE_MODEL_ACCOUNT_COL_TYPE,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				sort_by_string);
  gnc_tree_view_add_text_column(view, _("Commodity"), "commodity", NULL,
				sample_commodity,
				GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				sort_by_string);
  priv->code_column
    = gnc_tree_view_add_text_column(view, _("Account Code"), "account-code", NULL,
                                    "1-123-1234",
                                    GNC_TREE_MODEL_ACCOUNT_COL_CODE,
                                    GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                    sort_by_code);
  priv->desc_column
    = gnc_tree_view_add_text_column(view, _("Description"), "description", NULL,
                                    "Sample account description.",
                                    GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,
                                    GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                    sort_by_string);
  gnc_tree_view_add_numeric_column(view, _("Last Num"), "lastnum", "12345",
				   GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM,
				   GNC_TREE_VIEW_COLUMN_COLOR_NONE,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_string);
  gnc_tree_view_add_numeric_column(view, _("Present"), "present",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_PRESENT,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_present_value);
  priv->present_report_column
    = gnc_tree_view_add_numeric_column(view, _("Present (Report)"), "present_report",
				       SAMPLE_ACCOUNT_VALUE,
				       GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT,
				       GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,
				       GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				       sort_by_present_value);
  gnc_tree_view_add_numeric_column(view, _("Balance"), "balance",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_BALANCE,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_balance_value);
  priv->balance_report_column
    = gnc_tree_view_add_numeric_column(view, _("Balance (Report)"), "balance_report",
				       SAMPLE_ACCOUNT_VALUE,
				       GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT,
				       GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,
				       GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				       sort_by_balance_value);

  gnc_tree_view_add_numeric_column(view, _("Balance (Period)"), "balance-period",
				   SAMPLE_ACCOUNT_VALUE, 
				   GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_PERIOD,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE_PERIOD,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_balance_period_value);
  gnc_tree_view_add_numeric_column(view, _("Cleared"), "cleared",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_CLEARED,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_cleared_value);
  priv->cleared_report_column
    = gnc_tree_view_add_numeric_column(view, _("Cleared (Report)"), "cleared_report",
				       SAMPLE_ACCOUNT_VALUE,
				       GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT,
				       GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,
				       GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				       sort_by_cleared_value);
  gnc_tree_view_add_numeric_column(view, _("Reconciled"), "reconciled",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_reconciled_value);
  priv->reconciled_report_column
    = gnc_tree_view_add_numeric_column(view, _("Reconciled (Report)"), "reconciled_report",
				       SAMPLE_ACCOUNT_VALUE,
				       GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT,
				       GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED,
				       GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				       sort_by_reconciled_value);
  gnc_tree_view_add_numeric_column(view, _("Future Minimum"), "future_min",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_future_min_value);
  priv->future_min_report_column
    =  gnc_tree_view_add_numeric_column(view, _("Future Minimum (Report)"), "future_min_report",
					SAMPLE_ACCOUNT_VALUE,
					GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT,
					GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN,
					GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
					sort_by_future_min_value);
  gnc_tree_view_add_numeric_column(view, _("Total"), "total",
				   SAMPLE_ACCOUNT_VALUE,
				   GNC_TREE_MODEL_ACCOUNT_COL_TOTAL,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_total_value);
  priv->total_report_column
    = gnc_tree_view_add_numeric_column(view, _("Total (Report)"), "total_report",
				       SAMPLE_ACCOUNT_VALUE,
				       GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT,
				       GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,
				       GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				       sort_by_total_value);
  gnc_tree_view_add_numeric_column(view, _("Total (Period)"), "total-period",
				   SAMPLE_ACCOUNT_VALUE, 
				   GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_PERIOD,
				   GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL_PERIOD,
				   GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				   sort_by_total_period_value);
  priv->notes_column
    = gnc_tree_view_add_text_column(view, _("Notes"), "notes", NULL,
                                    "Sample account notes.",
                                    GNC_TREE_MODEL_ACCOUNT_COL_NOTES,
                                    GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                    sort_by_string);
  gnc_tree_view_add_text_column(view, _("Tax Info"), "tax-info", NULL,
				"Sample tax info.",
				GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO,
				GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
				sort_by_string);
  gnc_tree_view_add_toggle_column(view, _("Placeholder"),
     /* Translators: This string has a context prefix; the translation
	must only contain the part after the | character. */
     Q_("Column letter for 'Placeholder'|P"),
     "placeholder",
     GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     sort_by_placeholder,
     gnc_tree_view_account_placeholder_toggled);

  /* Update column titles to use the curreny name. */
  gtva_update_column_names(view);

  /* By default only the first column is visible. */
  gnc_tree_view_configure_columns(view);
  gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER (f_model),
					  gnc_tree_view_account_filter_helper,
					  view,
					  NULL);

  /* Default the sorting to account name */
  gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(s_model),
				       GNC_TREE_MODEL_ACCOUNT_COL_NAME,
				       GTK_SORT_ASCENDING);

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
  Account *root;

  root = gnc_book_get_root_account (gnc_get_current_book ());
  return gnc_tree_view_account_new_with_root (root, show_root);
}

/************************************************************/
/*                   Auxiliary Functions                    */
/************************************************************/

#define debug_path(fn, path) {				\
    gchar *path_string = gtk_tree_path_to_string(path); \
    fn("tree path %s", path_string);			\
    g_free(path_string);				\
  }

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
  s_path = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model), f_path);
  gtk_tree_path_free(f_path);
  debug_path(LEAVE, s_path);
  return s_path;
}

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

  ENTER("model %p, iter %p", s_model, s_iter);

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
    GtkSelectionMode mode;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    mode = gtk_tree_selection_get_mode(selection);
    if ((mode != GTK_SELECTION_SINGLE) && (mode != GTK_SELECTION_BROWSE)) {
      return NULL;
    }
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

  /* give gtk+ a chance to resize the tree view first by handling pending
   * configure events */
  while (gtk_events_pending ())
    gtk_main_iteration ();
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
  gboolean have_start, have_end;
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
  have_start = gtk_tree_model_iter_nth_child(s_model, &si_start, &si_account, 0);
  si_end = si_account;
  while (num_children) {
    GtkTreeIter tmp_iter = si_end;
    have_end = gtk_tree_model_iter_nth_child(s_model, &si_end, &tmp_iter,
                                             num_children - 1);
    if (have_end)
      num_children = gtk_tree_model_iter_n_children(s_model, &si_end);
    else
      num_children = 0;
  }

  if (have_start && have_end) {
    sp_start = gtk_tree_model_get_path (s_model, &si_start);
    sp_end = gtk_tree_model_get_path (s_model, &si_end);

    /* select everything between */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
    gtk_tree_selection_select_range (selection, sp_start, sp_end);

    /* clean up */
    gtk_tree_path_free(sp_start);
    gtk_tree_path_free(sp_end);
  }
  gtk_tree_path_free(sp_account);
  LEAVE(" ");
  return;
}

void
gnc_tree_view_account_expand_to_account (GncTreeViewAccount *view,
					 Account *account)
{
  GtkTreePath *path;

  g_return_if_fail(view != NULL);
  g_return_if_fail(GNC_IS_TREE_VIEW_ACCOUNT(view));
  ENTER("view %p, account %p", view, account);

  path = gnc_tree_view_account_get_path_from_account(view, account);
  if (path) {
    gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), path);
    gtk_tree_path_free(path);
  }
  LEAVE(" ");
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
gtva_update_column_name (GtkTreeViewColumn *column,
			 const gchar *fmt,
			 const gchar *mnemonic)
{
  gchar *name;

  g_return_if_fail(column);

  name = g_strdup_printf(fmt, mnemonic);
  gtk_tree_view_column_set_title(column, name);
  g_free(name);
}


static void
gtva_update_column_names (GncTreeView *view)
{
  GncTreeViewAccountPrivate *priv;
  const gchar *mnemonic;

  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(view);
  mnemonic = gnc_commodity_get_mnemonic(gnc_default_report_currency());

  gtva_update_column_name(priv->present_report_column,
			  /* Translators: %s is a currency mnemonic.*/
			  _("Present (%s)"), mnemonic);
  gtva_update_column_name(priv->balance_report_column,
			  /* Translators: %s is a currency mnemonic.*/
			  _("Balance (%s)"), mnemonic);
  gtva_update_column_name(priv->cleared_report_column,
			  /* Translators: %s is a currency mnemonic.*/
			  _("Cleared (%s)"), mnemonic);
  gtva_update_column_name(priv->reconciled_report_column,
			  /* Translators: %s is a currency mnemonic.*/
			  _("Reconciled (%s)"), mnemonic);
  gtva_update_column_name(priv->future_min_report_column,
			  /* Translators: %s is a currency mnemonic.*/
			  _("Future Minimum (%s)"), mnemonic);
  gtva_update_column_name(priv->total_report_column,
			  /* Translators: %s is a currency mnemonic.*/
			  _("Total (%s)"), mnemonic);
  gnc_tree_view_set_show_column_menu(view, FALSE);
  gnc_tree_view_set_show_column_menu(view, TRUE);
}


static void
gtva_currency_changed_cb (void)
{
  const GList *views, *ptr;

  views = gnc_gobject_tracking_get_list (GNC_TREE_VIEW_ACCOUNT_NAME);
  for (ptr = views; ptr; ptr = g_list_next(ptr)) {
    gtva_update_column_names (ptr->data);
  }
}
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


GtkTreeViewColumn *
gnc_tree_view_account_add_kvp_column (GncTreeViewAccount *view,
				      const gchar *column_title,
				      const gchar *kvp_key)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);
    g_return_val_if_fail (kvp_key != NULL, NULL);

    column = gnc_tree_view_add_text_column(GNC_TREE_VIEW(view), column_title,
					   kvp_key, NULL, "Sample text",
					   -1, -1, NULL);

    /* This new kvp column has only had one renderer added to it so
     * far.  Find that renderer. */
    renderer = gnc_tree_view_column_get_renderer(column);
    g_object_set (G_OBJECT (renderer), "xalign", 1.0, NULL);

    gtk_tree_view_column_set_cell_data_func (column, renderer, 
					     account_cell_kvp_data_func,
					     g_strdup(kvp_key), g_free);
    return column;
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

/**
 * If col_edited_cb is null, the editing callback (helper) will be
 * effectively disconnected.
 **/
void
gtva_setup_column_renderer_edited_cb(GncTreeViewAccount *account_view,
                                     GtkTreeViewColumn *column,
                                     GtkCellRenderer *renderer,
                                     GncTreeViewAccountColumnTextEdited col_edited_cb)
{
  GtkTreeModel *s_model;

  if (col_edited_cb == NULL)
  {
    g_object_set(G_OBJECT(renderer), "editable", FALSE, NULL);
    g_object_set_data(G_OBJECT(renderer), "column_edited_callback", col_edited_cb);
    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(account_view));
    g_signal_handlers_disconnect_by_func(G_OBJECT(renderer), col_edited_cb, s_model);
    g_object_set_data(G_OBJECT(renderer), "column_view", column);
  }
  else
  {
    g_object_set(G_OBJECT(renderer), "editable", TRUE, NULL);
    g_object_set_data(G_OBJECT(renderer), "column_edited_callback",
                      col_edited_cb);
    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(account_view));
    g_signal_connect(G_OBJECT(renderer), "edited", 
                     (GCallback) col_edited_helper, s_model);
    g_object_set_data(G_OBJECT(renderer), "column_view", column);
  }
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
    
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (account_view), NULL);
    
    renderer = gtk_cell_renderer_text_new ();
    g_object_set (G_OBJECT (renderer), "xalign", 1.0, NULL);
    
    column = gtk_tree_view_column_new_with_attributes (column_title,
                                                       renderer, NULL);
    if (col_edited_cb) {
        gtva_setup_column_renderer_edited_cb(account_view, column, 
                                             renderer, col_edited_cb);
    }
    gtk_tree_view_column_set_cell_data_func (column, renderer, 
                                             col_source_helper,
                                             col_source_cb, NULL);
    gnc_tree_view_append_column (GNC_TREE_VIEW(account_view), column);
    return column;
}


/* BEGIN FILTER FUNCTIONS */
#define FILTER_TREE_VIEW "types_tree_view"

/** This function tells the account tree view whether or not to filter
 *  out a particular account.  Accounts may be filtered if the user
 *  has decided not to display that particular account type, or if the
 *  user has requested that accounts with a zero total not be shown.
 *
 *  @param account The account that was toggled.
 *
 *  @param user_data A pointer to the AccountFilterDialog struct.
 *
 *  @return TRUE if the account should be visible.  FALSE if the
 *  account should be hidden. */
gboolean
gnc_plugin_page_account_tree_filter_accounts (Account *account, 
                                              gpointer user_data)
{
  AccountFilterDialog *fd = user_data;
  GNCAccountType acct_type;
  gnc_numeric total;
  gboolean result;

  ENTER("account %p:%s", account, xaccAccountGetName(account));

  if (!fd->show_hidden && xaccAccountIsHidden (account)) {
    LEAVE(" hide: hidden");
    return FALSE;
  }
  
  if (!fd->show_zero_total) {
    total = xaccAccountGetBalanceInCurrency (account, NULL, TRUE);
    if (gnc_numeric_zero_p(total)) {
      LEAVE(" hide: zero balance");
      return FALSE;
    }
  }
  
  acct_type = xaccAccountGetType(account);
  result = (fd->visible_types & (1 << acct_type)) ? TRUE : FALSE;
  LEAVE(" %s", result ? "show" : "hide");
  return result;
}

/** The "show hidden" button in the Filter dialog changed state.
 *  Update the page to reflect these changes.
 *
 *  @param button The GtkCheckButton that was toggled.
 *
 *  @param fd A pointer to the account filter dialog struct. */
void
gppat_filter_show_hidden_toggled_cb (GtkToggleButton *button,
				     AccountFilterDialog *fd)
{
  g_return_if_fail(GTK_IS_TOGGLE_BUTTON(button));

  ENTER("button %p", button);
  fd->show_hidden = gtk_toggle_button_get_active(button);
  gnc_tree_view_account_refilter(fd->tree_view);
  LEAVE("show_hidden %d", fd->show_hidden);
}

/** The "show zero totals" button in the Filter dialog changed state.
 *  Update the page to reflect these changes.
 *
 *  @param button The GtkCheckButton that was toggled.
 *
 *  @param fd A pointer to the account filter dialog struct. */
void
gppat_filter_show_zero_toggled_cb (GtkToggleButton *button,
				   AccountFilterDialog *fd)
{
  g_return_if_fail(GTK_IS_TOGGLE_BUTTON(button));

  ENTER("button %p", button);
  fd->show_zero_total = gtk_toggle_button_get_active(button);
  gnc_tree_view_account_refilter(fd->tree_view);
  LEAVE("show_zero %d", fd->show_zero_total);
}

/** The "clear all account types" button in the Filter dialog was
 *  clicked.  Clear all account types shown, and update the visible
 *  page.
 *
 *  @param button The button that was clicked.
 *
 *  @param fd A pointer to the account filter dialog struct. */
void
gppat_filter_clear_all_cb (GtkWidget *button,
			   AccountFilterDialog *fd)
{
  g_return_if_fail(GTK_IS_BUTTON(button));

  ENTER("button %p", button);
  fd->visible_types = 0;
  gtk_tree_model_filter_refilter(GTK_TREE_MODEL_FILTER(fd->model));
  gnc_tree_view_account_refilter(fd->tree_view);
  LEAVE("types 0x%x", fd->visible_types);
}

/** The "select all account types" button in the Filter dialog was
 *  clicked.  Make all account types visible, and update the page.
 *
 *  @param button The button that was clicked.
 *
 *  @param fd A pointer to the account filter dialog struct. */
void
gppat_filter_select_all_cb (GtkWidget *button,
			    AccountFilterDialog *fd)
{
  g_return_if_fail(GTK_IS_BUTTON(button));

  ENTER("button %p", button);
  fd->visible_types = -1;
  gtk_tree_model_filter_refilter(GTK_TREE_MODEL_FILTER(fd->model));
  gnc_tree_view_account_refilter(fd->tree_view);
  LEAVE("types 0x%x", fd->visible_types);
}

/** The "select default account types" button in the Filter dialog was
 *  clicked.  Set all account types to their default visibility (which
 *  happens to be visible for all of them), and update the page.
 *
 *  @param button The button that was clicked.
 *
 *  @param page A pointer to the account tree page to update. */
void
gppat_filter_select_default_cb (GtkWidget *button,
				AccountFilterDialog *fd)
{
  ENTER("button %p", button);
  gppat_filter_select_all_cb(button, fd);
  LEAVE(" ");
}

/** Set the renderer's properties.
 *
 *  @param column A GtkTreeColumn
 *
 *  @param renderer The GtkCellRendererToggle being rendered by @column
 *
 *  @param model The GtkTreeModel being rendered
 *
 *  @param iter A GtkTreeIter of the current row rendered
 *
 *  @param data A pointer to the account filter dialog struct. */
static void
gppat_filter_visible_set_func (GtkTreeViewColumn *column,
                               GtkCellRenderer *renderer,
                               GtkTreeModel *model,
                               GtkTreeIter *iter,
                               gpointer data)
{
  AccountFilterDialog *fd = data;
  GNCAccountType type;
  gboolean active;

  gtk_tree_model_get(model, iter, GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE, &type, -1);

  active = (fd->visible_types & (1 << type)) ? TRUE : FALSE;
  g_object_set (G_OBJECT (renderer), "active", active, NULL);
}

/** A check box in the tree view was toggled.
 *
 *  @param renderer The GtkCellRendererToggle being toggled.
 *
 *  @param fd A pointer to the account filter dialog struct. */
static void
gppat_filter_visible_toggled_cb (GtkCellRendererToggle *renderer,
                                 gchar *path_str,
                                 AccountFilterDialog *fd)
{
  GtkTreeModel *model = fd->model;
  GtkTreeIter iter;
  GtkTreePath *path;
  GNCAccountType type;

  ENTER("toggled %p", path_str);
  path = gtk_tree_path_new_from_string(path_str);

  if (gtk_tree_model_get_iter(model, &iter, path)) {
    gtk_tree_model_get(model, &iter, GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE, &type, -1);
    fd->visible_types ^= (1 << type);
    gnc_tree_view_account_refilter(fd->tree_view);
  }
  gtk_tree_path_free(path);
  LEAVE("types 0x%x", fd->visible_types);
}

/** The Filter dialog was closed.  Check to see if this was done via
 *  the OK button.  If so, make the changes permanent.  If not, revert
 *  any changes.
 *
 *  @param dialog A pointer to the "Filter By" dialog.
 *
 *  @param response The response code from closing the dialog.
 *
 *  @param fd A pointer to the account filter dialog struct. */
void
gppat_filter_response_cb (GtkWidget *dialog,
			  gint       response,
			  AccountFilterDialog *fd)
{
  GtkWidget *view;
  gpointer gptemp;

  g_return_if_fail(GTK_IS_DIALOG(dialog));

  ENTER("dialog %p, response %d", dialog, response);
  view = gnc_glade_lookup_widget(dialog, FILTER_TREE_VIEW);

  if (response != GTK_RESPONSE_OK) {
    fd->visible_types = fd->original_visible_types;
    fd->show_hidden = fd->original_show_hidden;
    fd->show_zero_total = fd->original_show_zero_total;
    gnc_tree_view_account_refilter(fd->tree_view);
  }

  /* Clean up and delete dialog */
  gptemp = (gpointer *)fd->dialog;
  g_atomic_pointer_compare_and_exchange(&gptemp,
					dialog, NULL);
  fd->dialog = gptemp;
  gtk_widget_destroy(dialog);
  LEAVE("types 0x%x", fd->visible_types);
}

void
account_filter_dialog_create(AccountFilterDialog *fd, GncPluginPage *page)
{
  GtkWidget *dialog, *button;
  GtkTreeView *view;
  GtkCellRenderer *renderer;
  GladeXML *xml;
  gchar *title;

  ENTER("(fd %p, page %p)", fd, page);

  if (fd->dialog) {
    gtk_window_present(GTK_WINDOW(fd->dialog));
    LEAVE("existing dialog");
    return;
  }

  /* Create the dialog */
  xml = gnc_glade_xml_new ("account.glade", "Filter By");
  dialog = glade_xml_get_widget (xml, "Filter By");
  fd->dialog = dialog;
  gtk_window_set_transient_for(GTK_WINDOW(dialog),
			       GTK_WINDOW(GNC_PLUGIN_PAGE(page)->window));
  /* Translators: The %s is the name of the plugin page */
  title = g_strdup_printf(_("Filter %s by..."),
			  gnc_plugin_page_get_page_name(GNC_PLUGIN_PAGE(page)));
  gtk_window_set_title(GTK_WINDOW(dialog), title);
  g_free(title);

  /* Remember current state */
  fd->original_visible_types = fd->visible_types;
  fd->original_show_hidden = fd->show_hidden;
  fd->original_show_zero_total = fd->show_zero_total;

  /* Update the dialog widgets for the current state */
  button = glade_xml_get_widget (xml, "show_hidden");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button),
			       fd->show_hidden);
  button = glade_xml_get_widget (xml, "show_zero");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button),
			       fd->show_zero_total);

  /* Set up the tree view and model */
  view = GTK_TREE_VIEW(glade_xml_get_widget (xml, FILTER_TREE_VIEW));

  fd->model = gnc_tree_model_account_types_filter_using_mask
    (~(1 << ACCT_TYPE_ROOT));
  gtk_tree_view_set_model(view, fd->model);
  g_object_unref (fd->model);

  renderer = gtk_cell_renderer_toggle_new();

  g_signal_connect(renderer, "toggled",
                   G_CALLBACK(gppat_filter_visible_toggled_cb), fd);

  gtk_tree_view_insert_column_with_data_func
    (view,
     -1, NULL, renderer,
     gppat_filter_visible_set_func, fd, NULL);

  gtk_tree_view_insert_column_with_attributes
    (view,
     -1, _("Account Types"), gtk_cell_renderer_text_new(),
     "text", GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME, NULL);

  /* Wire up the rest of the callbacks */
  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, 
                                    fd);

  /* Show it */
  gtk_widget_show_all(dialog);
  LEAVE(" ");
}

#define ACCT_COUNT    "NumberOfOpenAccounts"
#define ACCT_OPEN     "OpenAccount%d"
#define ACCT_SELECTED "SelectedAccount"
#define SHOW_HIDDEN   "ShowHidden"
#define SHOW_ZERO     "ShowZeroTotal"
#define ACCT_TYPES    "AccountTypes"

typedef struct foo {
  GKeyFile *key_file;
  const gchar *group_name;
  int count;
} bar_t;

/** Save information about an expanded row.  This function is called
 *  via a gtk_tree_view_map_expanded_rows, which calls it once per
 *  expanded row.  Its job is to write the full account name of the
 *  row out to the state file.
 *
 *  @param view A pointer to the GncTreeViewAccount
 *
 *  @param path A pointer to a particular entry in the tree.
 *
 *  @param data A pointer to a data structure holding the information
 *  related to the state file. */
static void
tree_save_expanded_row (GncTreeViewAccount *view,
			GtkTreePath *path,
			gpointer user_data)
{
    Account *account;
    bar_t *bar = user_data;
    gchar *key;
    gchar *account_name;
    
    account = gnc_tree_view_account_get_account_from_path (view, path);
    if (account == NULL)
        return;
    
    account_name = xaccAccountGetFullName(account);
    if (account_name == NULL)
        return;

    key = g_strdup_printf(ACCT_OPEN, ++bar->count);
    g_key_file_set_string(bar->key_file, bar->group_name, key, account_name);
    g_free(key);
    g_free(account_name);
}


/** Save information about the selected row.  Its job is to write the
 *  full account name of the row out to the state file.
 *
 *  @param view A pointer to the GtkTreeView embedded in an
 *  account tree page.
 *
 *  @param path A pointer to a particular entry in the tree.
 *
 *  @param data A pointer to a data structure holding the information
 *  related to the state file. */
static void
tree_save_selected_row (GncTreeViewAccount *view,
			gpointer user_data)
{
    Account *account;
    bar_t *bar = user_data;
    gchar *account_name;

    account = gnc_tree_view_account_get_selected_account(view);
    if (account == NULL)
        return;

    account_name = xaccAccountGetFullName (account);
    if (account_name == NULL)
        return;

    g_key_file_set_string(bar->key_file, bar->group_name, ACCT_SELECTED, 
                          account_name);
    g_free(account_name);
}

void
gnc_tree_view_account_save(GncTreeViewAccount *view, 
                           AccountFilterDialog *fd, 
                           GKeyFile *key_file, const gchar *group_name)
{
    bar_t bar;
    
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);
    
    ENTER("view %p, key_file %p, group_name %s", view, key_file,
          group_name);
    
    g_key_file_set_integer(key_file, group_name, ACCT_TYPES, 
                           fd->visible_types);
    g_key_file_set_boolean(key_file, group_name, SHOW_HIDDEN, 
                           fd->show_hidden);
    g_key_file_set_boolean(key_file, group_name, SHOW_ZERO, 
                           fd->show_zero_total);
	
    bar.key_file = key_file;
    bar.group_name = group_name;
    bar.count = 0;
    tree_save_selected_row(view, &bar);
    gtk_tree_view_map_expanded_rows(
        GTK_TREE_VIEW(view), (GtkTreeViewMappingFunc) tree_save_expanded_row, 
        &bar);
    g_key_file_set_integer(key_file, group_name, ACCT_COUNT, bar.count);
    LEAVE(" ");
    
}

/** Expand a row in the tree that was expanded when the user last quit
 *  gnucash.  Its job is to map from account name to tree row and
 *  expand the row.
 *
 *  @param view A pointer to the GncTreeViewAccount.
 *
 *  @param account_name A pointer to the full account name. */
static void
tree_restore_expanded_row (GncTreeViewAccount *view,
			   const gchar *account_name)
{
  Account *account;
  QofBook *book;

  book = qof_session_get_book(gnc_get_current_session());
  account = gnc_account_lookup_by_full_name(gnc_book_get_root_account(book),
                                            account_name);
  if (account)
    gnc_tree_view_account_expand_to_account(view, account);
}


/** Select the row in the tree that was selected when the user last
 *  quit gnucash.  Its job is to map from account name to tree row and
 *  select the row.
 *
 *  @param tree A pointer to the GncTreeViewAccount embedded.
 *
 *  @param account_name A pointer to the full account name. */
static void
tree_restore_selected_row (GncTreeViewAccount *view,
			   const gchar *account_name)
{
  Account *account;
  QofBook *book;

  book = qof_session_get_book(gnc_get_current_session());
  account = gnc_account_lookup_by_full_name(gnc_book_get_root_account(book),
					    account_name);
  if (account)
      gnc_tree_view_account_set_selected_account(view, account);
}

void
gnc_tree_view_account_restore(GncTreeViewAccount *view, 
                              AccountFilterDialog *fd, 
                              GKeyFile *key_file, const gchar *group_name)
{
    GError *error = NULL;
    gchar *key, *value;
    gint i, count;
    gboolean show;	

    /* Filter information. Ignore missing keys. */
    show = g_key_file_get_boolean(key_file, group_name, SHOW_HIDDEN, &error);
    if (error) {
        g_warning("error reading group %s key %s: %s",
                  group_name, SHOW_HIDDEN, error->message);
        g_error_free(error);
        error = NULL;
        show = TRUE;
    }
    fd->show_hidden = show;
    
    show = g_key_file_get_boolean(key_file, group_name, SHOW_ZERO, &error);
    if (error) {
        g_warning("error reading group %s key %s: %s",
                  group_name, SHOW_ZERO, error->message);
        g_error_free(error);
        error = NULL;
        show = TRUE;
    }
    fd->show_zero_total = show;
    
    i = g_key_file_get_integer(key_file, group_name, ACCT_TYPES, &error);
    if (error) {
        g_warning("error reading group %s key %s: %s",
                  group_name, ACCT_TYPES, error->message);
        g_error_free(error);
        error = NULL;
        i = -1;
    }
    fd->visible_types = i;
    
    /* Expanded accounts. Skip if count key missing. */
    count = g_key_file_get_integer(key_file, group_name, ACCT_COUNT, &error);
    if (error == NULL) {
        for (i = 1; i <= count; i++) {
	    key = g_strdup_printf(ACCT_OPEN, i);
	    value = g_key_file_get_string(key_file, group_name, key, &error);
	    if (error) {
                g_warning("error reading group %s key %s: %s",
                          group_name, key, error->message);
                g_error_free(error);
                error = NULL;
	    } else {
                tree_restore_expanded_row(view, value);
                g_free(value);
	    }
            g_free(key);
        }
    } else {
        g_warning("error reading group %s key %s: %s",
                  group_name, ACCT_COUNT, error->message);
        g_error_free(error);
    }
    
    /* Selected account (if any) */
    value = g_key_file_get_string(key_file, group_name, ACCT_SELECTED, NULL);
    if (value) {
        tree_restore_selected_row(view, value);
        g_free(value);
    }

    /* Update tree view for any changes */
    gnc_tree_view_account_refilter(view);
}

// @@fixme -- factor this app-not-gui-specific-logic out.
void
gnc_tree_view_account_name_edited_cb(Account *account, GtkTreeViewColumn *col, const gchar *new_name)
{
  // check for accounts with the same name among our parent's children.
  // should probably factor this consistency check out to the account
  // itself....
  {
    Account *parent = gnc_account_get_parent(account);
    Account *existing = gnc_account_lookup_by_name(parent, new_name);
    if (existing != NULL && existing != account)
    {
      PERR("account with the same name [%s] already exists.", new_name);
      return;
    }
  }
  xaccAccountSetName(account, new_name);
}

void
gnc_tree_view_account_code_edited_cb(Account *account, GtkTreeViewColumn *col, const gchar *new_code)
{
  xaccAccountSetCode(account, new_code);
}

void
gnc_tree_view_account_description_edited_cb(Account *account, GtkTreeViewColumn *col, const gchar *new_desc)
{
  xaccAccountSetDescription(account, new_desc);
}

void
gnc_tree_view_account_notes_edited_cb(Account *account, GtkTreeViewColumn *col, const gchar *new_notes)
{
  xaccAccountSetNotes(account, new_notes);
}

static void
gtva_set_column_editor(GncTreeViewAccount *view,
                       GtkTreeViewColumn *column,
                       GncTreeViewAccountColumnTextEdited edited_cb)
{
  GList *renderers_orig, *renderers;
  GtkCellRenderer *renderer;
  
  // look for the first text-renderer; on the 0th column of the account tree,
  // there are two renderers: pixbuf and text.  So find the text one.
  for (renderers_orig = renderers = gtk_tree_view_column_get_cell_renderers(column);
       renderers && !GTK_IS_CELL_RENDERER_TEXT(renderers->data);
       renderers = renderers->next);
  renderer = GTK_CELL_RENDERER(renderers->data);
  g_list_free(renderers_orig);
  g_return_if_fail(renderer != NULL);
  gtva_setup_column_renderer_edited_cb(GNC_TREE_VIEW_ACCOUNT(view), column, renderer, edited_cb);
}

void
gnc_tree_view_account_set_name_edited(GncTreeViewAccount *view,
                                      GncTreeViewAccountColumnTextEdited edited_cb)
{
  GncTreeViewAccountPrivate *priv;
  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(view);
  gtva_set_column_editor(view, priv->name_column, edited_cb);
}

void
gnc_tree_view_account_set_code_edited(GncTreeViewAccount *view,
                                      GncTreeViewAccountColumnTextEdited edited_cb)
{
  GncTreeViewAccountPrivate *priv;
  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(view);
  gtva_set_column_editor(view, priv->code_column, edited_cb);
}

void
gnc_tree_view_account_set_description_edited(GncTreeViewAccount *view,
                                             GncTreeViewAccountColumnTextEdited edited_cb)
{
  GncTreeViewAccountPrivate *priv;
  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(view);
  gtva_set_column_editor(view, priv->desc_column, edited_cb);
}

void
gnc_tree_view_account_set_notes_edited(GncTreeViewAccount *view,
                                       GncTreeViewAccountColumnTextEdited edited_cb)
{
  GncTreeViewAccountPrivate *priv;
  priv = GNC_TREE_VIEW_ACCOUNT_GET_PRIVATE(view);
  gtva_set_column_editor(view, priv->notes_column, edited_cb);
}
