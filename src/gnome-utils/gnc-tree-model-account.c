/* 
 * gnc-tree-model-account.c -- GtkTreeModel implementation to
 *	display accounts in a GtkTreeView.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-model-account.h"
#include "gnc-component-manager.h"
#include "Account.h"
#include "Group.h"
#include "gnc-accounting-period.h"
#include "gnc-commodity.h"
#include "gnc-gconf-utils.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-gobject-utils.h"
#include "gnc-ui-util.h"

#define TREE_MODEL_ACCOUNT_CM_CLASS "tree-model-account"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_model_account_class_init (GncTreeModelAccountClass *klass);
static void gnc_tree_model_account_init (GncTreeModelAccount *model);
static void gnc_tree_model_account_finalize (GObject *object);
static void gnc_tree_model_account_dispose (GObject *object);

/** Implementation of GtkTreeModel  **************************************/
static void gnc_tree_model_account_tree_model_init (GtkTreeModelIface *iface);
static guint gnc_tree_model_account_get_flags (GtkTreeModel *tree_model);
static int gnc_tree_model_account_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_tree_model_account_get_column_type (GtkTreeModel *tree_model,
						     int index);
static gboolean gnc_tree_model_account_get_iter (GtkTreeModel *tree_model,
						 GtkTreeIter *iter,
						 GtkTreePath *path);
static GtkTreePath *gnc_tree_model_account_get_path (GtkTreeModel *tree_model,
						     GtkTreeIter *iter);
static void gnc_tree_model_account_get_value (GtkTreeModel *tree_model,
					      GtkTreeIter *iter,
					      int column,
					      GValue *value);
static gboolean	gnc_tree_model_account_iter_next (GtkTreeModel *tree_model,
						  GtkTreeIter *iter);
static gboolean	gnc_tree_model_account_iter_children (GtkTreeModel *tree_model,
						      GtkTreeIter *iter,
						      GtkTreeIter *parent);
static gboolean	gnc_tree_model_account_iter_has_child (GtkTreeModel *tree_model,
						       GtkTreeIter *iter);
static int gnc_tree_model_account_iter_n_children (GtkTreeModel *tree_model,
						   GtkTreeIter *iter);
static gboolean	gnc_tree_model_account_iter_nth_child (GtkTreeModel *tree_model,
						       GtkTreeIter *iter,
						       GtkTreeIter *parent,
						       int n);
static gboolean	gnc_tree_model_account_iter_parent (GtkTreeModel *tree_model,
						    GtkTreeIter *iter,
    						    GtkTreeIter *child);

/** Helper Functions ****************************************************/
static void gnc_tree_model_account_set_toplevel (GncTreeModelAccount *model,
						 Account *toplevel);

/** Component Manager Callback ******************************************/
static void gnc_tree_model_account_event_handler (QofEntity *entity,
						  QofEventId event_type,
						  GncTreeModelAccount *model,
						  GncEventData *ed);

/** The instance private data for a account tree model. */
typedef struct GncTreeModelAccountPrivate
{
	QofBook *book;
	AccountGroup *root;
	Account *toplevel;
	gint event_handler_id;
	const gchar *negative_color;
} GncTreeModelAccountPrivate;

#define GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_MODEL_ACCOUNT, GncTreeModelAccountPrivate))


/************************************************************/
/*           Account Tree Model - Misc Functions            */
/************************************************************/

/** Tell the GncTreeModelAccount code to update the color that it will
 *  use for negative numbers.  This function will iterate over all
 *  existing models and update their setting from gconf.
 *
 *  @internal
 */
static void
gnc_tree_model_account_update_color (GConfEntry *entry, gpointer user_data)
{
	GncTreeModelAccountPrivate *priv;
	GncTreeModelAccount *model;
	GConfValue *value;
	gboolean use_red;

	g_return_if_fail(GNC_IS_TREE_MODEL_ACCOUNT(user_data));
	model = user_data;
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	value = gconf_entry_get_value(entry);
	use_red = gconf_value_get_bool(value);
	priv->negative_color = use_red ? "red" : "black";
}
/************************************************************/
/*               g_object required functions                */
/************************************************************/

/** A pointer to the parent class of a account tree model. */
static GtkObjectClass *parent_class = NULL;

GType
gnc_tree_model_account_get_type (void)
{
	static GType gnc_tree_model_account_type = 0;

	if (gnc_tree_model_account_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeModelAccountClass), /* class_size */
			NULL,   			   /* base_init */
			NULL,				   /* base_finalize */
			(GClassInitFunc) gnc_tree_model_account_class_init,
			NULL,				   /* class_finalize */
			NULL,				   /* class_data */
			sizeof (GncTreeModelAccount),	   /* */
			0,				   /* n_preallocs */
			(GInstanceInitFunc) gnc_tree_model_account_init
		};
		
		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) gnc_tree_model_account_tree_model_init,
			NULL,
			NULL
		};

		gnc_tree_model_account_type = g_type_register_static (GNC_TYPE_TREE_MODEL,
								      GNC_TREE_MODEL_ACCOUNT_NAME,
								      &our_info, 0);
		
		g_type_add_interface_static (gnc_tree_model_account_type,
					     GTK_TYPE_TREE_MODEL,
					     &tree_model_info);
	}

	return gnc_tree_model_account_type;
}

static void
gnc_tree_model_account_class_init (GncTreeModelAccountClass *klass)
{
	GObjectClass *o_class;

	parent_class = g_type_class_peek_parent (klass);

	o_class = G_OBJECT_CLASS (klass);

	/* GObject signals */
	o_class->finalize = gnc_tree_model_account_finalize;
	o_class->dispose = gnc_tree_model_account_dispose;

	g_type_class_add_private(klass, sizeof(GncTreeModelAccountPrivate));
}

static void
gnc_tree_model_account_init (GncTreeModelAccount *model)
{
	GncTreeModelAccountPrivate *priv;
	gboolean red;

	ENTER("model %p", model);
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	red = gnc_gconf_get_bool(GCONF_GENERAL, KEY_NEGATIVE_IN_RED, NULL);

	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	priv->book = NULL;
	priv->root = NULL;
	priv->toplevel = NULL;
	priv->negative_color = red ? "red" : "black";

	gnc_gconf_general_register_cb(KEY_NEGATIVE_IN_RED,
				      gnc_tree_model_account_update_color,
				      model);
	
	LEAVE(" ");
}

static void
gnc_tree_model_account_finalize (GObject *object)
{
	GncTreeModelAccountPrivate *priv;
	GncTreeModelAccount *model;

	ENTER("model %p", object);
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (object));

	model = GNC_TREE_MODEL_ACCOUNT (object);
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

	gnc_gconf_general_remove_cb(KEY_NEGATIVE_IN_RED,
				    gnc_tree_model_account_update_color,
				    model);

	priv->book = NULL;

	if (G_OBJECT_CLASS (parent_class)->finalize)
            G_OBJECT_CLASS(parent_class)->finalize (object);
	LEAVE(" ");
}

static void
gnc_tree_model_account_dispose (GObject *object)
{
	GncTreeModelAccountPrivate *priv;
	GncTreeModelAccount *model;

	ENTER("model %p", object);
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (object));

	model = GNC_TREE_MODEL_ACCOUNT (object);
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

	if (priv->event_handler_id) {
	  qof_event_unregister_handler (priv->event_handler_id);
	  priv->event_handler_id = 0;
	}

	gnc_gconf_general_remove_cb(KEY_NEGATIVE_IN_RED,
                                    gnc_tree_model_account_update_color,
                                    model);

	if (G_OBJECT_CLASS (parent_class)->dispose)
            G_OBJECT_CLASS (parent_class)->dispose (object);
	LEAVE(" ");
}


/************************************************************/
/*                   New Model Creation                     */
/************************************************************/

GtkTreeModel *
gnc_tree_model_account_new (AccountGroup *group)
{
	GncTreeModelAccount *model;
	GncTreeModelAccountPrivate *priv;
	const GList *item;
	
	ENTER("group %p", group);
	item = gnc_gobject_tracking_get_list(GNC_TREE_MODEL_ACCOUNT_NAME);
	for ( ; item; item = g_list_next(item)) {
		model = (GncTreeModelAccount *)item->data;
		priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
		if (priv->root == group) {
			g_object_ref(G_OBJECT(model));
			LEAVE("returning existing model %p", model);
			return GTK_TREE_MODEL(model);
		}
	}

	model = g_object_new (GNC_TYPE_TREE_MODEL_ACCOUNT,
			      NULL);

	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	priv->book = gnc_get_current_book();
	priv->root = group;

	{
	  Account *account;

	  account = xaccMallocAccount(priv->book);
	  gnc_tree_model_account_set_toplevel (model, account);
	}

	priv->event_handler_id = qof_event_register_handler
	  ((QofEventHandler)gnc_tree_model_account_event_handler, model);

	LEAVE("model %p", model);
	return GTK_TREE_MODEL (model);
}


/************************************************************/
/*        Gnc Tree Model Debugging Utility Function         */
/************************************************************/

#define ITER_STRING_LEN 128

static const gchar *
iter_to_string (GtkTreeIter *iter)
{
#ifdef G_THREADS_ENABLED
  static GStaticPrivate gtmits_buffer_key = G_STATIC_PRIVATE_INIT;
  gchar *string;

  string = g_static_private_get (&gtmits_buffer_key);
  if (string == NULL) {
    string = g_malloc(ITER_STRING_LEN + 1);
    g_static_private_set (&gtmits_buffer_key, string, g_free);
  }
#else
  static char string[ITER_STRING_LEN + 1];
#endif

  if (iter)
    snprintf(string, ITER_STRING_LEN,
	     "[stamp:%x data:%p (%s), %p, %d]",
	     iter->stamp, iter->user_data,
	     xaccAccountGetName ((Account *) iter->user_data),
	     iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
  else
    strcpy(string, "(null)");
  return string;
}


/************************************************************/
/*       Gtk Tree Model Required Interface Functions        */
/************************************************************/

static void
gnc_tree_model_account_tree_model_init (GtkTreeModelIface *iface)
{
	iface->get_flags       = gnc_tree_model_account_get_flags;
	iface->get_n_columns   = gnc_tree_model_account_get_n_columns;
	iface->get_column_type = gnc_tree_model_account_get_column_type;
	iface->get_iter        = gnc_tree_model_account_get_iter;
	iface->get_path        = gnc_tree_model_account_get_path;
	iface->get_value       = gnc_tree_model_account_get_value;
	iface->iter_next       = gnc_tree_model_account_iter_next;
	iface->iter_children   = gnc_tree_model_account_iter_children;
	iface->iter_has_child  = gnc_tree_model_account_iter_has_child;
	iface->iter_n_children = gnc_tree_model_account_iter_n_children;
	iface->iter_nth_child  = gnc_tree_model_account_iter_nth_child;
	iface->iter_parent     = gnc_tree_model_account_iter_parent;
}

static guint
gnc_tree_model_account_get_flags (GtkTreeModel *tree_model)
{
	return 0;
}

static int
gnc_tree_model_account_get_n_columns (GtkTreeModel *tree_model)
{
	g_return_val_if_fail(GNC_IS_TREE_MODEL_ACCOUNT(tree_model), -1);
    
	return GNC_TREE_MODEL_ACCOUNT_NUM_COLUMNS;
}

static GType
gnc_tree_model_account_get_column_type (GtkTreeModel *tree_model,
					int index)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), G_TYPE_INVALID);
	g_return_val_if_fail ((index < GNC_TREE_MODEL_ACCOUNT_NUM_COLUMNS) && (index >= 0), G_TYPE_INVALID);

	switch (index) {
		case GNC_TREE_MODEL_ACCOUNT_COL_NAME:
		case GNC_TREE_MODEL_ACCOUNT_COL_TYPE:
		case GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY:
		case GNC_TREE_MODEL_ACCOUNT_COL_CODE:
		case GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION:
		case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT:
		case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE:
		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_PERIOD:
		case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED:
		case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED:
		case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN:
		case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL:
		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_PERIOD:
		case GNC_TREE_MODEL_ACCOUNT_COL_NOTES:
		case GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO:
		case GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM:

		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE_PERIOD:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL_PERIOD:
			return G_TYPE_STRING;

		case GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER:
			return G_TYPE_BOOLEAN;

		default:
			g_assert_not_reached ();
			return G_TYPE_INVALID;
	}
}

static gboolean
gnc_tree_model_account_get_iter (GtkTreeModel *tree_model,
				 GtkTreeIter *iter,
				 GtkTreePath *path)
{
	GncTreeModelAccountPrivate *priv;
	GncTreeModelAccount *model;
	Account *account = NULL;
	AccountGroup *group = NULL, *children;
	gint i = 0, *indices;

	{
	  gchar *path_string = gtk_tree_path_to_string(path);
	  ENTER("model %p, iter %p, path %s", tree_model, iter, path_string);
	  g_free(path_string);
	}
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	if (priv->toplevel != NULL) {
		if (gtk_tree_path_get_depth (path) > 1) {
			i++;
		} else {

			iter->user_data = priv->toplevel;
			iter->user_data2 = NULL;
			iter->user_data3 = GINT_TO_POINTER (0);
			iter->stamp = model->stamp;

			LEAVE("iter (1) %s", iter_to_string(iter));
			return TRUE;
		}
	}

	if (priv->root == NULL) {
		LEAVE("failed (2)");
		return FALSE;
	}

	children = priv->root;

	indices = gtk_tree_path_get_indices (path);
	for (; i < gtk_tree_path_get_depth (path); i++) {
		group = children;
		if (indices[i] >= xaccGroupGetNumAccounts (group)) {
			iter->stamp = 0;

			LEAVE("failed (3)");
			return FALSE;
		}

		account = xaccGroupGetAccount (group, indices[i]);
		children = xaccAccountGetChildren (account);
	}

	if (account == NULL || group == NULL) {
		iter->stamp = 0;

		LEAVE("failed (4)");
		return FALSE;
	}

	iter->stamp = model->stamp;
	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (indices[i - 1]);

	LEAVE("iter (5) %s", iter_to_string(iter));
	return TRUE;
}

static GtkTreePath *
gnc_tree_model_account_get_path (GtkTreeModel *tree_model,
				 GtkTreeIter *iter)
{
	GncTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	GncTreeModelAccountPrivate *priv;
	Account *account;
	AccountGroup *group;
	GtkTreePath *path;
	gint i;
	gboolean found, finished = FALSE;

	ENTER("model %p, iter %s", model, iter_to_string(iter));
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	if (priv->root == NULL) {
		LEAVE("failed (1)");
		return NULL;
	}

	account = (Account *) iter->user_data;
	group = (AccountGroup *) iter->user_data2;

	path = gtk_tree_path_new ();

	if (priv->toplevel != NULL) {
		if (account == priv->toplevel) {
			gtk_tree_path_append_index (path, 0);

			{
			  gchar *path_string = gtk_tree_path_to_string(path);
			  LEAVE("path (2) %s", path_string);
			  g_free(path_string);
			}
			return path;
		}
	}

	do {
		found = FALSE;
		for (i = 0; i < xaccGroupGetNumAccounts (group); i++) {
			if (xaccGroupGetAccount (group, i) == account) {
				found = TRUE;
				if (group == priv->root)
					finished = TRUE;
				break;
			}
		}

		if (!found) {
			gtk_tree_path_free (path);
			LEAVE("failed (3)");
			return NULL;
		}

		gtk_tree_path_prepend_index (path, i);

		account = xaccAccountGetParentAccount (account);
		group = xaccAccountGetParent (account);
	} while (!finished);

	if (priv->toplevel != NULL) {
		gtk_tree_path_prepend_index (path, 0);
	}

	{
	  gchar *path_string = gtk_tree_path_to_string(path);
	  LEAVE("path (4) %s", path_string);
	  g_free(path_string);
	}
	return path;
}

static void
gnc_tree_model_account_set_color(GncTreeModelAccount *model,
				 gboolean negative,
				 GValue *value)
{
	GncTreeModelAccountPrivate *priv;

	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	if (negative)
	  g_value_set_static_string (value, priv->negative_color);
	else 
	  g_value_set_static_string (value, "black");
}

static gchar *
gnc_tree_model_account_compute_period_balance(GncTreeModelAccount *model,
					      Account *acct,
					      gboolean recurse,
					      gboolean *negative)
{
  GncTreeModelAccountPrivate *priv;
  time_t t1, t2;
  gnc_numeric b3;  

  priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
  if (acct == priv->toplevel)
    return g_strdup("");

  t1 = gnc_accounting_period_fiscal_start();
  t2 = gnc_accounting_period_fiscal_end();
    
  if (t1 > t2)
    return g_strdup("");

  b3 = xaccAccountGetBalanceChangeForPeriod(acct, t1, t2, recurse);
  if (gnc_reverse_balance (acct))
    b3 = gnc_numeric_neg (b3);

  if (negative)
    *negative = gnc_numeric_negative_p(b3);

  return g_strdup(xaccPrintAmount(b3, gnc_account_print_info(acct, TRUE)));
}

static void
gnc_tree_model_account_get_value (GtkTreeModel *tree_model,
				  GtkTreeIter *iter,
				  int column,
				  GValue *value)
{
	GncTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	GncTreeModelAccountPrivate *priv;
	Account *account;
	gboolean negative; /* used to set "deficit style" aka red numbers */
	gchar *string;

	ENTER("model %p, iter %s, col %d", tree_model,
              iter_to_string(iter), column);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

	account = (Account *) iter->user_data;
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

	switch (column) {
		case GNC_TREE_MODEL_ACCOUNT_COL_NAME:
			g_value_init (value, G_TYPE_STRING);
			if (account == priv->toplevel)
			  g_value_set_string (value, _("New top level account"));
			else
			  g_value_set_string (value, xaccAccountGetName (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_TYPE:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, 
					    xaccAccountGetTypeStr (xaccAccountGetType (account)));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_CODE:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetCode (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value,
					    gnc_commodity_get_fullname(xaccAccountGetCommodity (account)));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetDescription (account));
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetPresentBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetPresentBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetPresentBalanceInCurrency,
								  account, FALSE, &negative);
			gnc_tree_model_account_set_color(model, negative, value);
			g_free(string);
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetBalanceInCurrency,
								  account, FALSE, &negative);
			gnc_tree_model_account_set_color(model, negative, value);
			g_free(string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_PERIOD:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_tree_model_account_compute_period_balance(model, account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE_PERIOD:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_tree_model_account_compute_period_balance(model, account, FALSE, &negative);
			gnc_tree_model_account_set_color(model, negative, value);
			g_free (string);
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetClearedBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetClearedBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetClearedBalanceInCurrency,
								  account, FALSE, &negative);
			gnc_tree_model_account_set_color(model, negative, value);
			g_free(string);
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetReconciledBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetReconciledBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetReconciledBalanceInCurrency,
								  account, FALSE, &negative);
			gnc_tree_model_account_set_color(model, negative, value);
			g_free (string);
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetProjectedMinimumBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetProjectedMinimumBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetProjectedMinimumBalanceInCurrency,
								  account, FALSE, &negative);
			gnc_tree_model_account_set_color(model, negative, value);
			g_free (string);
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetBalanceInCurrency,
								  account, TRUE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetBalanceInCurrency,
									 account, TRUE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetBalanceInCurrency,
								  account, TRUE, &negative);
			gnc_tree_model_account_set_color(model, negative, value);
			g_free (string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_PERIOD:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_tree_model_account_compute_period_balance(model, account, TRUE, &negative);
			g_value_take_string (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL_PERIOD:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_tree_model_account_compute_period_balance(model, account, TRUE, &negative);
			gnc_tree_model_account_set_color(model, negative, value);
			g_free (string);
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_NOTES:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetNotes (account));
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO:
			g_value_init (value, G_TYPE_STRING);
			g_value_take_string (value, gnc_ui_account_get_tax_info_string (account));
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetLastNum (account));
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER:
			g_value_init (value, G_TYPE_BOOLEAN);
			g_value_set_boolean (value, xaccAccountGetPlaceholder (account));
			break;

		default:
			g_assert_not_reached ();
	}
	LEAVE(" ");
}

static gboolean
gnc_tree_model_account_iter_next (GtkTreeModel *tree_model,
				  GtkTreeIter *iter)
{
	GncTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	GncTreeModelAccountPrivate *priv;
	Account *account;
	AccountGroup *group;
	gint i;

	ENTER("model %p, iter %s", tree_model, iter_to_string(iter));
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	if (iter->user_data == priv->toplevel) {
		iter->stamp = 0;
		LEAVE("failed (1)");
		return FALSE;
	}

	group = (AccountGroup *) iter->user_data2;
	i = GPOINTER_TO_INT (iter->user_data3);

	if (i > xaccGroupGetNumAccounts (group) - 2) {
		iter->stamp = 0;
		LEAVE("failed (2)");
		return FALSE;
	}

	account = xaccGroupGetAccount (group, i + 1);

	if (account == NULL) {
		iter->stamp = 0;
		LEAVE("failed (3)");
		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (i + 1);

	LEAVE("iter %s", iter_to_string(iter));
	return TRUE;
}

static gboolean
gnc_tree_model_account_iter_children (GtkTreeModel *tree_model,
				      GtkTreeIter *iter,
				      GtkTreeIter *parent)
{
	GncTreeModelAccountPrivate *priv;
	GncTreeModelAccount *model;
	Account *account;
	AccountGroup *group;

	if (parent) {
	  ENTER("model %p, iter %p (to be filed in), parent %s",
		tree_model, iter, iter_to_string(parent));
	} else {
	  ENTER("model %p, iter %p (to be filed in), parent (null)", tree_model, iter);
	}
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

	if (priv->toplevel != NULL) {
		if (parent == NULL) {
			iter->user_data = priv->toplevel;
			iter->user_data2 = NULL;
			iter->user_data3 = GINT_TO_POINTER (0);
			iter->stamp = model->stamp;
			LEAVE("iter (1) %s", iter_to_string(iter));
			return TRUE;
		} else if (parent->user_data == priv->toplevel) {
			parent = NULL;
		}
	}

	if (priv->root == NULL || 
            xaccGroupGetNumAccounts (priv->root) == 0) {
		iter->stamp = 0;
		LEAVE("failed (either no group or group has no accounts)");
		return FALSE;
	}

	if (parent == NULL) {
		account = xaccGroupGetAccount (priv->root, 0);
		
		if (account == NULL) {
			iter->stamp = 0;
			LEAVE("failed (couldn't get account from group)");
			return FALSE;
		}

		iter->user_data = account;
		iter->user_data2 = priv->root;
		iter->user_data3 = GINT_TO_POINTER (0);
		iter->stamp = model->stamp;
		LEAVE("iter (2) %s", iter_to_string(iter));
		return TRUE;	
	}

	g_return_val_if_fail (parent != NULL, FALSE);
	g_return_val_if_fail (parent->user_data != NULL, FALSE);
	g_return_val_if_fail (parent->stamp == model->stamp, FALSE);	

	group = xaccAccountGetChildren ((Account *) parent->user_data);

	if (group == NULL || xaccGroupGetNumAccounts (group) == 0) {
		iter->stamp = 0;
		LEAVE("failed (children group was %s)", 
                      group ? "empty" : "null");
		return FALSE;
	}

	account = xaccGroupGetAccount (group, 0);
	
	if (account == NULL) {
		iter->stamp = 0;
		LEAVE("failed (group's account is null)");
		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (0);
	iter->stamp = model->stamp;
	LEAVE("iter (3) %s", iter_to_string(iter));
	return TRUE;
}

static gboolean
gnc_tree_model_account_iter_has_child (GtkTreeModel *tree_model,
				       GtkTreeIter *iter)
{
	GncTreeModelAccount *model;
	GncTreeModelAccountPrivate *priv;
	AccountGroup *group;

	ENTER("model %p, iter %s", tree_model, iter_to_string(iter));
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (iter->user_data == priv->toplevel) {
		group = priv->root;
	} else {
		group = xaccAccountGetChildren ((Account *) iter->user_data);
	}

	if (group == NULL || xaccGroupGetNumAccounts (group) == 0) {
		LEAVE("no");
		return FALSE;
	}

	LEAVE("yes");
	return TRUE;
}

static int
gnc_tree_model_account_iter_n_children (GtkTreeModel *tree_model,
					GtkTreeIter *iter)
{
	GncTreeModelAccount *model;
	GncTreeModelAccountPrivate *priv;
	AccountGroup *group;

	ENTER("model %p, iter %s", tree_model, iter_to_string(iter));
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

	if (iter == NULL) {
		if (priv->toplevel != NULL) {
			LEAVE("count is 1");
			return 1;
		} else {
			LEAVE("count is %d", xaccGroupGetNumAccounts (priv->root));
			return xaccGroupGetNumAccounts (priv->root);
		}
	}

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (priv->toplevel == iter->user_data) {
		group = priv->root;
	} else {
		group = xaccAccountGetChildren ((Account *) iter->user_data);
	}

	LEAVE("count is %d", xaccGroupGetNumAccounts (group));
	return xaccGroupGetNumAccounts (group);
}

static gboolean
gnc_tree_model_account_iter_nth_child (GtkTreeModel *tree_model,
				       GtkTreeIter *iter,
				       GtkTreeIter *parent,
				       int n)
{
	GncTreeModelAccount *model;
	GncTreeModelAccountPrivate *priv;
	Account *account;
	AccountGroup *group;

	if (parent) {
	  gchar *parent_string;

	  parent_string = strdup(iter_to_string(parent));
	  ENTER("model %p, iter %s, parent %s, n %d",
		tree_model, iter_to_string(iter),
		parent_string, n);
	  g_free(parent_string);
	} else {
	  ENTER("model %p, iter %s, parent (null), n %d",
		tree_model, iter_to_string(iter), n);
	}
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

	if (parent == NULL) {
		if (priv->toplevel != NULL) {
			if (n > 0) {
				iter->stamp = 0;
				LEAVE("failed (1)");
				return FALSE;
			} else {
				iter->user_data = priv->toplevel;
				iter->user_data2 = NULL;
				iter->user_data3 = GINT_TO_POINTER (0);
				iter->stamp = model->stamp;
				LEAVE("iter (1) %s", iter_to_string(iter));
				return TRUE;
			}
		}

		account = xaccGroupGetAccount (priv->root, n);

		if (account == NULL) {
			iter->stamp = 0;
			LEAVE("failed (2)");			
			return FALSE;
		}

		iter->user_data = account;
		iter->user_data2 = priv->root;
		iter->user_data3 = GINT_TO_POINTER (n);
		iter->stamp = model->stamp;
		LEAVE("iter (2) %s", iter_to_string(iter));
		return TRUE;
	}

	g_return_val_if_fail (parent->user_data != NULL, FALSE);
	g_return_val_if_fail (parent->stamp == model->stamp, FALSE);

	if (priv->toplevel == parent->user_data) {
		group = priv->root;
	} else {
		group = xaccAccountGetChildren ((Account *) parent->user_data);
	}

	if (group == NULL || xaccGroupGetNumAccounts (group) <= n) {
		iter->stamp = 0;
		LEAVE("failed (3)");
		return FALSE;
	}

	account = xaccGroupGetAccount (group, n);
	
	if (account == NULL) {
		iter->stamp = 0;
		LEAVE("failed (4)");
		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (n);
	iter->stamp = model->stamp;
	LEAVE("iter (3) %s", iter_to_string(iter));
	return TRUE;
}

static gboolean
gnc_tree_model_account_iter_parent (GtkTreeModel *tree_model,
				    GtkTreeIter *iter,
    				    GtkTreeIter *child)
{
	GncTreeModelAccount *model;
	GncTreeModelAccountPrivate *priv;
	Account *account;
	AccountGroup *group;
	gint i;

	if (child) {
	  gchar *child_string;

	  child_string = strdup(iter_to_string(child));
	  ENTER("model %p, iter %s, child %s",
		tree_model, iter_to_string(iter),
		child_string);
	  g_free(child_string);
	} else {
	  ENTER("model %p, iter %s, child (null)",
		tree_model, iter_to_string(iter));
	}
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

	g_return_val_if_fail (child != NULL, FALSE);
	g_return_val_if_fail (child->user_data != NULL, FALSE);
	g_return_val_if_fail (child->stamp == model->stamp, FALSE);

	account = (Account *) child->user_data;

	if (account == priv->toplevel) {
		iter->stamp = 0;
		LEAVE("failed (1)");
		return FALSE;
	}

	account = xaccAccountGetParentAccount (account);
	group = xaccAccountGetParent (account);

	if (account == NULL || group == NULL) {
		if (priv->toplevel != NULL) {
			iter->user_data = priv->toplevel;
			iter->user_data2 = NULL;
			iter->user_data3 = GINT_TO_POINTER (0);
			iter->stamp = model->stamp;
			LEAVE("iter (1) %s", iter_to_string(iter));
			return TRUE;
		} else {
			iter->stamp = 0;
			LEAVE("failed (2)");
			return FALSE;
		}
	}

	for (i = 0; i < xaccGroupGetNumAccounts (group); i++) {
		if (xaccGroupGetAccount (group, i) == account) {
			iter->user_data = account;
			iter->user_data2 = group;
			iter->user_data3 = GINT_TO_POINTER (i);
			iter->stamp = model->stamp;
			LEAVE("iter (2) %s", iter_to_string(iter));
			return TRUE;	
		}
	}

	if (priv->toplevel != NULL) {
		iter->user_data = priv->toplevel;
		iter->user_data2 = NULL;
		iter->user_data3 = GINT_TO_POINTER (0);
		iter->stamp = model->stamp;
		LEAVE("iter (3) %s", iter_to_string(iter));
		return TRUE;
	}
	iter->stamp = 0;
	LEAVE("failed (3)");
	return FALSE;
}


/************************************************************/
/*             Account Tree View Root Functions             */
/************************************************************/

static gpointer
account_row_inserted (Account *account,
		      gpointer data)
{
	GtkTreePath *path;
	GtkTreeIter iter;

	ENTER("account %p (%s), model %p",
	      account, xaccAccountGetName(account), data);
	if (!gnc_tree_model_account_get_iter_from_account
            (GNC_TREE_MODEL_ACCOUNT (data), account, &iter))
	  return NULL;

	path = gtk_tree_model_get_path (GTK_TREE_MODEL (data), &iter);

	gtk_tree_model_row_inserted (GTK_TREE_MODEL (data), path, &iter);

	gtk_tree_path_free (path);

	LEAVE(" ");
	return NULL;
}

/*
 * Gets the top node of the model.  This node is for a pseudo-account
 * that lives above the main level accounts in the engine.
 */
Account *
gnc_tree_model_account_get_toplevel (GncTreeModelAccount *model)
{
	GncTreeModelAccountPrivate *priv;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);

	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	return priv->toplevel;
}

/*
 * Add a new top node to the model.  This node is for a pseudo-account
 * that lives above the main level accounts in the engine.
 */
static void
gnc_tree_model_account_set_toplevel (GncTreeModelAccount *model,
                                     Account *toplevel)
{
	GncTreeModelAccountPrivate *priv;
	GtkTreePath *path;
	gint i;
	GtkTreeIter iter;

	ENTER("model %p, toplevel %p", model, toplevel);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));

	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	DEBUG("old toplevel %p", priv->toplevel);
	if (priv->toplevel != NULL) {
            /* CAS: this can't happen because we only set toplevel on
             * new tree models. */
		path = gtk_tree_path_new_first ();
		gtk_tree_model_row_deleted (GTK_TREE_MODEL (model), path);
		gtk_tree_path_free (path);
	} else {
            /* CAS: I think this is bogus for the same reason - we'll
             * have no rows, so why are we emitting a bunch of
             * "row_deleted" signals when no rows can exist? */
		path = gtk_tree_path_new_first ();
		for (i = 0; i < xaccGroupGetNumAccounts (priv->root); i++) {
			gtk_tree_model_row_deleted (GTK_TREE_MODEL (model), path);
		}
		gtk_tree_path_free (path);
	}

	DEBUG("set new toplevel %p", toplevel);
	priv->toplevel = toplevel;

	if (priv->toplevel != NULL) {
		path = gtk_tree_path_new_first ();
		if (gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, path))
		  gtk_tree_model_row_inserted (GTK_TREE_MODEL (model), path, &iter);
		gtk_tree_path_free (path);
	}

	if (priv->root != NULL) {
		xaccGroupForEachAccount (priv->root, account_row_inserted, model, TRUE);
	}
	LEAVE("new toplevel %p", priv->root);
}

/************************************************************/
/*            Account Tree View Filter Functions            */
/************************************************************/

/*
 * Convert a model/iter pair to a gnucash account.  This routine should
 * only be called from an account tree view filter function.
 */
Account *
gnc_tree_model_account_get_account (GncTreeModelAccount *model,
				    GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);

	return (Account *) iter->user_data;
}

/*
 * Convert a model/account pair into a gtk_tree_model_iter.  This
 * routine should only be called from the file
 * gnc-tree-view-account.c.
 */
gboolean
gnc_tree_model_account_get_iter_from_account (GncTreeModelAccount *model,
					      Account *account,
					      GtkTreeIter *iter)
{
	GncTreeModelAccountPrivate *priv;
	AccountGroup *group;
	gboolean found = FALSE;
	gint i;
	
	ENTER("model %p, account %p, iter %p", model, account, iter);
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), FALSE);
	g_return_val_if_fail ((account != NULL), FALSE);
	g_return_val_if_fail ((iter != NULL), FALSE);

	iter->user_data = account;
	iter->stamp = model->stamp;

	priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);
	if (account == priv->toplevel) {
		iter->user_data2 = NULL;
		iter->user_data3 = GINT_TO_POINTER (0);
		LEAVE("Matched top level");
		return TRUE;
	}

	if (priv->root != xaccAccountGetRoot (account)) {
		LEAVE("Root doesn't match");
		return FALSE;
	}

	group = xaccAccountGetParent (account);
	DEBUG("Looking through %d accounts at this level", 
              xaccGroupGetNumAccounts (group));
	for (i = 0; i < xaccGroupGetNumAccounts (group); i++) {
		if (xaccGroupGetAccount (group, i) == account) {
			found = TRUE;
			break;
		}
	}

	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (i);
	LEAVE("iter %s", iter_to_string(iter));
	return found;
}

/*
 * Convert a model/account pair into a gtk_tree_model_path.  This
 * routine should only be called from the file
 * gnc-tree-view-account.c.
 */
GtkTreePath *
gnc_tree_model_account_get_path_from_account (GncTreeModelAccount *model,
					      Account *account)
{
	GtkTreeIter tree_iter;
	GtkTreePath *tree_path;

	ENTER("model %p, account %p", model, account);
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
	g_return_val_if_fail (account != NULL, NULL);

	if (!gnc_tree_model_account_get_iter_from_account (model, account, 
                                                           &tree_iter)) {
	  LEAVE("no iter");
	  return NULL;
	}

	tree_path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), &tree_iter);
	if (tree_path) {
	  gchar *path_string = gtk_tree_path_to_string(tree_path);
	  LEAVE("path (2) %s", path_string);
	  g_free(path_string);
	} else {
	  LEAVE("no path");
	}
	return tree_path;
}

/************************************************************/
/*   Account Tree Model - Engine Event Handling Functions   */
/************************************************************/

static void
increment_stamp(GncTreeModelAccount *model)
{
    do model->stamp++; 
    while (!model->stamp);
}

static void
propagate_change(GtkTreeModel *model, GtkTreePath *path, gint toggle_if_num)
{
    GtkTreeIter iter;

    /* Immediate parent */
    if (gtk_tree_path_up(path) && 
        gtk_tree_model_get_iter(model, &iter, path)) {
        gtk_tree_model_row_changed(model, path, &iter);
        if (gtk_tree_model_iter_n_children(model, &iter) == toggle_if_num)
            gtk_tree_model_row_has_child_toggled(model, path, &iter);
    }

    /* All other ancestors */
    while (gtk_tree_path_up(path) && gtk_tree_path_get_depth(path) > 0 &&
           gtk_tree_model_get_iter(model, &iter, path)) {
        gtk_tree_model_row_changed(model, path, &iter);
    }
}

/** This function is the handler for all event messages from the
 *  engine.  Its purpose is to update the account tree model any time
 *  an account is added to the engine or deleted from the engine.
 *  This change to the model is then propagated to any/all overlying
 *  filters and views.  This function listens to the ADD, REMOVE, and
 *  DESTROY events.
 *
 *  @internal
 *
 *  @warning There is a "Catch 22" situation here.
 *  gtk_tree_model_row_deleted() can't be called until after the item
 *  has been deleted from the real model (which is the engine's
 *  account tree for us), but once the account has been deleted from
 *  the engine we have no way to determine the path to pass to
 *  row_deleted().  This is a PITA, but the only other choice is to
 *  have this model mirror the engine's accounts instead of
 *  referencing them directly.
 *
 *  @param entity The guid of the affected item.
 *
 *  @param type The type of the affected item.  This function only
 *  cares about items of type "account".
 *
 *  @param event type The type of the event. This function only cares
 *  about items of type ADD, REMOVE, MODIFY, and DESTROY.
 *
 *  @param user_data A pointer to the account tree model.
 */
static void
gnc_tree_model_account_event_handler (QofEntity *entity,
				      QofEventId event_type,
				      GncTreeModelAccount *model,
				      GncEventData *ed)
{
  GncTreeModelAccountPrivate *priv;
  const gchar *parent_name;
  GtkTreePath *path = NULL;
  GtkTreeIter iter;
  Account *account, *parent;

  g_return_if_fail(model);	/* Required */
  if (!GNC_IS_ACCOUNT(entity))
    return;

  ENTER("entity %p of type %d, model %p, event_data %p",
	entity, event_type, model, ed);
  priv = GNC_TREE_MODEL_ACCOUNT_GET_PRIVATE(model);

  account = GNC_ACCOUNT(entity);
  if (xaccAccountGetBook(account) != priv->book) {
      LEAVE("not in this book");
      return;
  }
  if (xaccAccountGetRoot(account) != priv->root) {
      LEAVE("not in this model");
      return;
  }
  /* What to do, that to do. */
  switch (event_type) {
    case QOF_EVENT_ADD:
      /* Tell the filters/views where the new account was added. */
      DEBUG("add account %p (%s)", account, xaccAccountGetName(account));
      path = gnc_tree_model_account_get_path_from_account(model, account);
      if (!path) {
	DEBUG("can't generate path");
	break;
      }
      increment_stamp(model);
      if (!gnc_tree_model_account_get_iter(GTK_TREE_MODEL(model), &iter, path)) {
	DEBUG("can't generate iter");
	break;
      }
      gtk_tree_model_row_inserted (GTK_TREE_MODEL(model), path, &iter);
      propagate_change(GTK_TREE_MODEL(model), path, 1);
      break;

    case QOF_EVENT_REMOVE:
      if (!ed) /* Required for a remove. */
	break;
      parent = ed->node ? GNC_ACCOUNT(ed->node) : priv->toplevel;
      parent_name = ed->node ? xaccAccountGetName(parent) : "Root";
      DEBUG("remove child %d of account %p (%s)", ed->idx, parent, parent_name);
      path = gnc_tree_model_account_get_path_from_account(model, parent);
      if (!path) {
	DEBUG("can't generate path");
	break;
      }
      increment_stamp(model);
      gtk_tree_path_append_index (path, ed->idx);
      gtk_tree_model_row_deleted (GTK_TREE_MODEL(model), path);
      propagate_change(GTK_TREE_MODEL(model), path, 0);
      break;

    case QOF_EVENT_MODIFY:
      DEBUG("modify  account %p (%s)", account, xaccAccountGetName(account));
      path = gnc_tree_model_account_get_path_from_account(model, account);
      if (!path) {
	DEBUG("can't generate path");
	break;
      }
      if (!gnc_tree_model_account_get_iter(GTK_TREE_MODEL(model), &iter, path)) {
	DEBUG("can't generate iter");
	break;
      }
      gtk_tree_model_row_changed(GTK_TREE_MODEL(model), path, &iter);
      propagate_change(GTK_TREE_MODEL(model), path, -1);
      break;

    default:
      DEBUG("unknown event type");
      return;
  }

  if (path)
    gtk_tree_path_free(path);
  LEAVE(" ");
  return;
}
