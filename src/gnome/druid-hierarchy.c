/********************************************************************\
 * druid-hierarchy.c -- account hierarchy creation functionality    *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
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

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "Group.h"
#include "gnc-account-merge.h"
#include "dialog-new-user.h"
#include "dialog-utils.h"
#include "druid-hierarchy.h"
#include "druid-merge.h"
#include "druid-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-exp-parser.h"
#include "gnc-general-select.h"
#include "gnc-gconf-utils.h"
#include "gnc-hooks.h"
#include "gnc-component-manager.h"
#include "../gnome-utils/gnc-dir.h"
#include "gnc-gui-query.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui-util.h"
#include "io-example-account.h"
#include "top-level.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-account-tree.h"

#include "gnc-engine.h"
static QofLogModule log_module = GNC_MOD_IMPORT; 

#define GCONF_SECTION "dialogs/new_hierarchy"

typedef enum {
  COL_CHECKED,
  COL_TITLE,
  COL_SHORT_DESCRIPTION,
  COL_LONG_DESCRIPTION,
  COL_ACCOUNT,
  NUM_COLUMNS
} ColumnNames;


typedef struct {
  GtkWidget *dialog;

  GtkWidget *currency_selector;

  GtkTreeView *categories_tree;
  GtkTextView *category_description;
  GtkWidget *category_accounts_container;
  GtkTreeView *category_accounts_tree;
  gboolean category_set_changed;

  GncTreeViewAccount *final_account_tree;
  GtkWidget *final_account_tree_container;
  Account *selected_account;
  /** Map<Account*,gnc_numeric*> **/
  GHashTable *balance_hash;

  AccountGroup *our_final_group;
  QofBook *temporary;

  gboolean account_list_added;

  GncHierarchyDruidFinishedCallback when_completed;

} hierarchy_data;

void on_choose_account_categories_prepare (GnomeDruidPage  *gnomedruidpage,
					   gpointer         arg1,
					   hierarchy_data  *data);
void select_all_clicked (GtkButton       *button,
			 hierarchy_data  *data);
void clear_all_clicked (GtkButton       *button,
			hierarchy_data  *data);
void on_final_account_prepare (GnomeDruidPage  *gnomedruidpage,
			       gpointer         arg1,
			       hierarchy_data  *data);
gboolean on_final_account_next (GnomeDruidPage  *gnomedruidpage,
                                gpointer         arg1,
                                hierarchy_data  *data);

void on_cancel (GnomeDruid      *gnomedruid, hierarchy_data *data);
void on_finish (GnomeDruidPage  *gnomedruidpage, gpointer arg1, hierarchy_data *data);

// ------------------------------------------------------------

static void
delete_hierarchy_dialog (hierarchy_data *data)
{
  gtk_widget_destroy (data->dialog);
}

static void
destroy_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
  gnc_numeric *balance = value;
  g_free (balance);
}

static void
gnc_hierarchy_destroy_cb (GtkObject *obj,   hierarchy_data *data)
{
  GHashTable *hash;

  hash = data->balance_hash;
  if (hash)
  {
    g_hash_table_foreach (hash, destroy_hash_helper, NULL);
    g_hash_table_destroy (hash);
    data->balance_hash = NULL;
  }
}

static gnc_numeric
get_final_balance (GHashTable *hash, Account *account)
{
  gnc_numeric *balance;

  if (!hash || !account)
    return gnc_numeric_zero ();

  balance = g_hash_table_lookup(hash, account);
  if (balance)
    return *balance;
  return gnc_numeric_zero ();
}

static void
set_final_balance (GHashTable *hash, Account *account, gnc_numeric in_balance)
{
  gnc_numeric *balance;

  if (!hash || !account)
    return;

  balance = g_hash_table_lookup (hash, account);
  if (balance) {
    *balance = in_balance;
    return;
  }

  balance = g_new (gnc_numeric, 1);
  *balance = in_balance;
  g_hash_table_insert (hash, account, balance);
}

static gchar*
gnc_get_ea_locale_dir(const char *top_dir)
{
    static gchar *default_locale = "C";
    gchar *ret;
    gchar *locale;
    struct stat buf;
    int i;
    
#ifdef HAVE_LC_MESSAGES
    locale = g_strdup(setlocale(LC_MESSAGES, NULL));
#else
    /*
     * Mac OS X 10.1 and earlier, not only doesn't have LC_MESSAGES
     * setlocale can sometimes return NULL instead of "C"
     */
    locale = g_strdup(setlocale(LC_ALL, NULL) ? 
		      setlocale(LC_ALL, NULL) : "C");
#endif

    i = strlen(locale);
    ret = g_strdup_printf("%s/%s", top_dir, locale);

    while (stat(ret, &buf) != 0)
    { 
	i--;
	if (i<1) 
	{
	    g_free(ret);
	    ret = g_strdup_printf("%s/%s", top_dir, default_locale);
	    break;
	}
	locale[i] = '\0';
	g_free(ret);
	ret = g_strdup_printf("%s/%s", top_dir, locale);
    }
    
    g_free(locale);

    return ret;
}

/************************************************************
 *                  Choose Categories Page                  *
 ************************************************************/
static void
categories_selection_changed (GtkTreeModel *treemodel,
			      GtkTreePath *arg1,
			      GtkTreeIter *arg2,
			      hierarchy_data *data)
{
	data->category_set_changed = TRUE;
}


static void
add_one_category (GncExampleAccount *acc,
		  hierarchy_data *data)
{
	GtkTreeView *view;
	GtkListStore *store;
	GtkTreeIter iter;

	g_return_if_fail(acc != NULL);
	g_return_if_fail(data != NULL);

	view = data->categories_tree;
	store = GTK_LIST_STORE(gtk_tree_view_get_model(view));

	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter,
			   COL_CHECKED, acc->start_selected,
			   COL_TITLE, acc->title,
			   COL_SHORT_DESCRIPTION, acc->short_description,
			   COL_LONG_DESCRIPTION, acc->long_description,
			   COL_ACCOUNT, acc,
			   -1);

	if (acc->start_selected)
	  data->category_set_changed = TRUE;
}

static void
category_checkbox_toggled (GtkCellRendererToggle *toggle,
			   gchar                 *path,
			   GtkListStore          *store)
{
	GtkTreeIter iter;
	gboolean active;

	if (!gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(store),
						 &iter, path))
	  return;

	/* Get current state of the category */
	active = gtk_cell_renderer_toggle_get_active(toggle);
	gtk_list_store_set(store, &iter, COL_CHECKED, !active, -1);
}

static void
account_categories_tree_view_prepare (hierarchy_data  *data)
{
	GSList *list;
	gchar *locale_dir;
	GtkTreeView *tree_view;
	GtkListStore *model;
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	locale_dir = gnc_get_ea_locale_dir (GNC_ACCOUNTS_DIR);
 	list = gnc_load_example_account_list (data->temporary,
					      locale_dir);
	g_free (locale_dir);

	/* Prepare the account_categories GtkTreeView with a model and with some columns */
	tree_view = data->categories_tree;
	model = gtk_list_store_new(NUM_COLUMNS, G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_STRING,
				   G_TYPE_STRING, G_TYPE_POINTER);
	gtk_tree_view_set_model (tree_view, GTK_TREE_MODEL(model));
	g_object_unref (model);

	g_slist_foreach(list, (GFunc)add_one_category, data);

	g_signal_connect (G_OBJECT (model), "row_changed",
			  G_CALLBACK (categories_selection_changed),
			  data);

	renderer = gtk_cell_renderer_toggle_new ();
	g_object_set (G_OBJECT (renderer), "activatable", TRUE, NULL);
	column = gtk_tree_view_column_new_with_attributes (_("Selected"),
							   renderer,
							   "active", COL_CHECKED,
							   NULL);
	gtk_tree_view_append_column (tree_view, column);
	gtk_tree_view_column_set_sort_column_id (column, COL_CHECKED);
	g_signal_connect (G_OBJECT (renderer), "toggled",
			  G_CALLBACK (category_checkbox_toggled),
			  model);


	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes (_("Account Types"),
							   renderer,
							   "text", COL_TITLE,
							   NULL);
	gtk_tree_view_append_column (tree_view, column);
	gtk_tree_view_column_set_sort_column_id (column, COL_TITLE);

//	renderer = gtk_cell_renderer_text_new ();
//	column = gtk_tree_view_column_new_with_attributes (_("Description"),
//							   renderer,
//							   "text", COL_SHORT_DESCRIPTION,
//							   NULL);
//	gtk_tree_view_append_column (tree_view, column);
//	gtk_tree_view_column_set_sort_column_id (column, COL_SHORT_DESCRIPTION);

	gtk_tree_view_set_headers_clickable(tree_view, TRUE);
	gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(model),
					      COL_TITLE,
					      GTK_SORT_ASCENDING);
}

void
on_choose_account_categories_prepare (GnomeDruidPage  *gnomedruidpage,
				      gpointer         arg1,
				      hierarchy_data  *data)
{
  GtkTextBuffer* buffer;
  if (!data->account_list_added)
  {
    /* Build the categories tree if necessary */
    gnc_suspend_gui_refresh ();
    data->temporary = qof_book_new();
    account_categories_tree_view_prepare (data);
    gnc_resume_gui_refresh ();

    /* clear out the description/tree */
    if (data->category_accounts_tree)
      gtk_widget_destroy(GTK_WIDGET(data->category_accounts_tree));
    data->category_accounts_tree = NULL;
    buffer = gtk_text_view_get_buffer(data->category_description);
    gtk_text_buffer_set_text(buffer, "", -1);

    data->account_list_added = TRUE;
  }
}

static void
categories_tree_selection_changed (GtkTreeSelection *selection,
				   hierarchy_data *data)
{
	GtkTreeView *tree_view;
	GtkTreeModel *model;
	GtkTreeViewColumn *column;
	GtkTreeIter iter;
	GncExampleAccount *gea;
	GtkTextBuffer* buffer;

	/* Remove the old account tree */
	if (data->category_accounts_tree)
	  gtk_widget_destroy(GTK_WIDGET(data->category_accounts_tree));
	data->category_accounts_tree = NULL;
	buffer = gtk_text_view_get_buffer(data->category_description);
	gtk_text_buffer_set_text(buffer, "", -1);

	/* Add a new one if something selected */
	if (gtk_tree_selection_get_selected (selection, &model, &iter)) {
		gtk_tree_model_get (model, &iter, COL_ACCOUNT, &gea, -1);
		buffer = gtk_text_view_get_buffer(data->category_description);
		gtk_text_buffer_set_text(buffer, gea->long_description, -1);

		tree_view = gnc_tree_view_account_new_with_group (gea->group, FALSE);
		/* Override the normal fixed (user settable) sizing */
		column = gtk_tree_view_get_column(GTK_TREE_VIEW(tree_view), 0);
		gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);

		data->category_accounts_tree = tree_view;
		gtk_tree_view_expand_all (tree_view);
		gtk_container_add(GTK_CONTAINER(data->category_accounts_container), GTK_WIDGET(tree_view));
		gtk_widget_show(GTK_WIDGET(tree_view));
	}
}

static gboolean
select_helper (GtkListStore *store,
	       GtkTreePath  *path,
	       GtkTreeIter  *iter,
	       gpointer      data)
{
	GncExampleAccount *gea;

	g_return_val_if_fail(GTK_IS_LIST_STORE(store), FALSE);

	gtk_tree_model_get (GTK_TREE_MODEL(store), iter, COL_ACCOUNT, &gea, -1);
	if ((gea != NULL) && !gea->exclude_from_select_all) {
	  gtk_list_store_set(store, iter,
			     COL_CHECKED, GPOINTER_TO_INT(data),
			     -1);
	}

	return FALSE;  /* Run entire tree */
}

void
select_all_clicked (GtkButton       *button,
                    hierarchy_data  *data)
{
	gtk_tree_model_foreach (gtk_tree_view_get_model (data->categories_tree),
				(GtkTreeModelForeachFunc)select_helper,
				GINT_TO_POINTER(TRUE));
}

void
clear_all_clicked (GtkButton       *button,
		   hierarchy_data  *data)
{
	gtk_tree_model_foreach (gtk_tree_view_get_model (data->categories_tree),
				(GtkTreeModelForeachFunc)select_helper,
				GINT_TO_POINTER(FALSE));
}

/************************************************************
 *                  Opening Balances Page                   *
 ************************************************************/

static void
delete_our_final_group (hierarchy_data *data)
{
  if (data->our_final_group != NULL)
  {
    xaccAccountGroupBeginEdit (data->our_final_group);
    xaccAccountGroupDestroy (data->our_final_group);
    data->our_final_group = NULL;
  }
}

static Account*
clone_account (const Account* from, gnc_commodity *com)
{
  Account *ret;

  ret = xaccCloneAccountSimple (from, gnc_get_current_book ());

  xaccAccountSetCommodity (ret, com);

  return ret;
}

struct add_group_data_struct
{
  AccountGroup *to;
  Account *parent;
  gnc_commodity *com;
};

static gpointer
add_groups_for_each (Account *toadd, gpointer data)
{
  struct add_group_data_struct *dadata = data;
  Account *foundact;
    
  foundact = xaccGetAccountFromName (dadata->to, xaccAccountGetName(toadd));

  if (!foundact)
  {
    foundact = clone_account (toadd, dadata->com);

    if (dadata->to)
      xaccGroupInsertAccount (dadata->to, foundact);
    else if (dadata->parent)
      xaccAccountInsertSubAccount (dadata->parent, foundact);
    else
    {
      g_warning ("add_groups_for_each: no valid parent");
    }
  }

  {
    AccountGroup *addgrp = xaccAccountGetChildren (toadd);

    if (xaccGroupGetNumAccounts(addgrp) > 0)
    {
      struct add_group_data_struct downdata;

      downdata.to = xaccAccountGetChildren(foundact);
      downdata.parent = foundact;
      downdata.com = dadata->com;

      xaccGroupForEachAccount (addgrp, add_groups_for_each,
                               &downdata, FALSE);
    }
  }

  return NULL;
}

static void
add_groups_to_with_random_guids (AccountGroup *into, AccountGroup *from,
                                 gnc_commodity *com)
{
  struct add_group_data_struct data;
  data.to = into;
  data.parent = NULL;
  data.com = com;
    
  xaccGroupForEachAccount (from, add_groups_for_each, &data, FALSE);
}

static AccountGroup *
hierarchy_merge_groups (GSList *dalist, gnc_commodity *com)
{
  GSList *mark;
  AccountGroup *ret = xaccMallocAccountGroup (gnc_get_current_book ());

  for (mark = dalist; mark; mark = mark->next)
  {
    GncExampleAccount *xea = mark->data;

    add_groups_to_with_random_guids (ret, xea->group, com);
  }

  return ret;
}

static gboolean
accumulate_accounts (GtkListStore *store,
		     GtkTreePath *path,
		     GtkTreeIter *iter,
		     GSList **list)
{
	GncExampleAccount *gea;
	gboolean active;

	g_return_val_if_fail(GTK_IS_LIST_STORE(store), FALSE);
 
	gtk_tree_model_get (GTK_TREE_MODEL(store), iter,
			    COL_CHECKED, &active,
			    COL_ACCOUNT, &gea,
			    -1);
	if (active && gea)
	  *list = g_slist_prepend(*list, gea);

  	return FALSE;  /* Run entire list */
}


static GSList *
get_selected_account_list (GtkTreeView *tree_view)
{
	GSList *actlist = NULL;
	GtkTreeModel *model;

	model = gtk_tree_view_get_model (tree_view);
	gtk_tree_model_foreach (model,
				(GtkTreeModelForeachFunc)accumulate_accounts,
				&actlist);
	return actlist;
}

static void
balance_cell_data_func (GtkTreeViewColumn *tree_column,
	       		GtkCellRenderer *cell,
			GtkTreeModel *model,
			GtkTreeIter *iter,
			gpointer user_data)
{
	Account *account;
	gnc_numeric balance;
	const gchar *string;
	GNCPrintAmountInfo print_info;
	hierarchy_data *data = (hierarchy_data *)user_data;
	gboolean allow_value;

	g_return_if_fail (GTK_TREE_MODEL (model));
	account = gnc_tree_view_account_get_account_from_iter (model, iter);

	balance = get_final_balance (data->balance_hash, account);
	if (gnc_reverse_balance (account))
		balance = gnc_numeric_neg (balance);

	if (gnc_numeric_zero_p (balance)) {
		string = "";
	} else {
		print_info = gnc_account_print_info (account, FALSE);
		string = xaccPrintAmount (balance, print_info);
	}

 	if (xaccAccountGetType(account) == EQUITY) {
	  allow_value = FALSE;
	  string=_("zero");
	} else {
          GncAccountMergeDisposition disp;
          disp = determine_merge_disposition(gnc_book_get_group(gnc_get_current_book()), account);
          if (disp == GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW)
          {
                  allow_value = !xaccAccountGetPlaceholder(account);
          }
          else
          {
                  allow_value = FALSE;
                  string = _("existing account");
          }
	}
	g_object_set (G_OBJECT (cell),
		      "text", string,
		      "editable", allow_value,
		      "sensitive", allow_value,
		      NULL);
}

static void 
balance_cell_edited (GtkCellRendererText *cell,
		     gchar               *path,
		     gchar               *new_text,
		     gpointer             user_data)
{
	Account *account;
	char *error_loc;
	gnc_numeric amount;
	hierarchy_data *data = (hierarchy_data *)user_data;

	g_return_if_fail(data != NULL);

	account = gnc_tree_view_account_get_selected_account(data->final_account_tree);
	if (account == NULL) {
	  printf("Account is null\n");
	  return;
	}

	error_loc = NULL;
	if (!gnc_exp_parser_parse (new_text, &amount, &error_loc)) {
	  amount = gnc_numeric_zero();
	  g_object_set (G_OBJECT(cell), "text", "", NULL);
	}
	set_final_balance (data->balance_hash, account, amount);
	gnc_engine_gen_event ((QofEntity*)account, GNC_EVENT_MODIFY);
}

static void
use_existing_account_data_func(GtkTreeViewColumn *tree_column,
                               GtkCellRenderer *cell,
                               GtkTreeModel *tree_model,
                               GtkTreeIter *iter,
                               gpointer user_data)
{
  Account *new_acct;
  AccountGroup *real_root;
  GncAccountMergeDisposition disposition;
  char *to_user = "(error; unknown condition)";

  g_return_if_fail (GTK_TREE_MODEL (tree_model));
  new_acct = gnc_tree_view_account_get_account_from_iter(tree_model, iter);
  if (new_acct == NULL)
  {
    g_object_set (G_OBJECT(cell), "text", "(null account)", NULL);
    return;
  }

  real_root = gnc_book_get_group(gnc_get_current_book());
  disposition = determine_merge_disposition(real_root, new_acct);
  switch (disposition)
  {
  case GNC_ACCOUNT_MERGE_DISPOSITION_ERROR:
    to_user = "error: placeholders different";
    break;
  case GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING:
    to_user = "yes";
    break;
  case GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW:
    to_user = "no";
    break;
  }
  
  g_object_set(G_OBJECT(cell), "text", to_user, NULL);
}

void
on_final_account_prepare (GnomeDruidPage  *gnomedruidpage,
                          gpointer         arg1,
                          hierarchy_data  *data)
{
  GSList *actlist;
  GtkTreeView *tree_view;
  GtkTreeSelection *selection;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  gnc_commodity *com;

  /* Anything to do? */
  if (!data->category_set_changed)
    return;
  data->category_set_changed = FALSE;

  gnc_suspend_gui_refresh ();

  /* Delete any existing account tree */
  if (data->final_account_tree) {
    gtk_widget_destroy(GTK_WIDGET(data->final_account_tree));
    data->final_account_tree = NULL;
  }
  delete_our_final_group (data);


  /* Build a new account list */
  actlist = get_selected_account_list (data->categories_tree);
  com = gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT(data->currency_selector));
  data->our_final_group = hierarchy_merge_groups (actlist, com);


  /* Now build a new account tree */
  data->final_account_tree
    = GNC_TREE_VIEW_ACCOUNT(gnc_tree_view_account_new_with_group (data->our_final_group, FALSE));
  tree_view = GTK_TREE_VIEW(data->final_account_tree);
  gnc_tree_view_account_set_name_edited(data->final_account_tree,
                                        gnc_tree_view_account_name_edited_cb);
  gnc_tree_view_account_set_code_edited(data->final_account_tree,
                                        gnc_tree_view_account_code_edited_cb);
  gnc_tree_view_account_set_description_edited(data->final_account_tree,
                                               gnc_tree_view_account_description_edited_cb);
  gnc_tree_view_account_set_notes_edited(data->final_account_tree,
                                         gnc_tree_view_account_notes_edited_cb);

  gtk_tree_view_set_headers_visible (tree_view, TRUE);
  gnc_tree_view_configure_columns (GNC_TREE_VIEW(data->final_account_tree),
				   "type", "placeholder", NULL);
  gnc_tree_view_set_show_column_menu (GNC_TREE_VIEW(data->final_account_tree),
				      FALSE);

  selection = gtk_tree_view_get_selection (tree_view);
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer),
		"xalign", 1.0,
		(char *)NULL);
  g_signal_connect (G_OBJECT (renderer), "edited",
		    G_CALLBACK (balance_cell_edited),
		    data);
  column = gtk_tree_view_column_new_with_attributes (_("Opening Balance"),
						     renderer,
						     NULL);
  gtk_tree_view_column_set_cell_data_func (column, renderer, 
					   balance_cell_data_func,
					   (gpointer)data, NULL);
  gnc_tree_view_append_column (GNC_TREE_VIEW(tree_view), column);

  // only in the case where there *are* existing accounts...
  if (xaccGroupGetNumSubAccounts(gnc_book_get_group(gnc_get_current_book())) > 0)
  {
    GList *renderers;
    column = gnc_tree_view_add_text_column(GNC_TREE_VIEW(tree_view),
                                           _("Use Existing"),
                                           NULL,
                                           NULL,
                                           "yes",
                                           GNC_TREE_VIEW_COLUMN_DATA_NONE,
                                           GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                           NULL);
    renderers = gtk_tree_view_column_get_cell_renderers(column);
    g_object_set(G_OBJECT(renderer), "xalign", 1.0, (char*)NULL);
    gtk_tree_view_column_set_cell_data_func(column, GTK_CELL_RENDERER(renderers->data),
                                            use_existing_account_data_func, (gpointer)data, NULL);
    g_list_free(renderers);
  }

  gtk_container_add(GTK_CONTAINER(data->final_account_tree_container),
		    GTK_WIDGET(data->final_account_tree));

  /* Expand the entire tree */
  gtk_tree_view_expand_all (tree_view);
  gtk_widget_show(GTK_WIDGET(data->final_account_tree));
  gnc_resume_gui_refresh ();
}

static void _glist_free_helper(gpointer elt, gpointer user_data)
{
  g_free(elt);
}

gboolean
on_final_account_next (GnomeDruidPage  *gnomedruidpage,
                       gpointer         arg1,
                       hierarchy_data  *data)
{
  GList *errors;
  gboolean has_errors;

  errors  = account_merge_error_detection(gnc_book_get_group(gnc_get_current_book()),
                                          data->our_final_group);
  has_errors = (g_list_length(errors) > 0);
  if (has_errors)
  {
    gnc_info_dialog(data->dialog, 
        "%d accounts have placeholdders that conflict "
        "with existing accounts, please resolve.", g_list_length(errors));
  }

  {
    g_list_foreach(errors, _glist_free_helper, NULL);
    g_list_free(errors);
  }

  return has_errors;
}

void
on_cancel (GnomeDruid      *gnomedruid,
	   hierarchy_data  *data)
{
  gnc_suspend_gui_refresh ();
  delete_hierarchy_dialog (data);
  delete_our_final_group (data);
  gncp_new_user_finish ();
  g_free(data);
  gnc_resume_gui_refresh ();
}

static gpointer
starting_balance_helper (Account *account, hierarchy_data *data)
{
  gnc_numeric balance;

  balance = get_final_balance (data->balance_hash, account);
  if (!gnc_numeric_zero_p (balance))
    gnc_account_create_opening_balance (account, balance, time (NULL),
                                        gnc_get_current_book ());

  return NULL;
}

void
on_finish (GnomeDruidPage  *gnomedruidpage,
           gpointer         arg1,
           hierarchy_data  *data)
{
        GncHierarchyDruidFinishedCallback when_completed;
	ENTER (" ");

	if (data->our_final_group)
        {
	  xaccGroupForEachAccount (data->our_final_group,
                                   (AccountCallback)starting_balance_helper,
				   data, TRUE);
        }

        // delete before we suspend GUI events, and then muck with the model,
        // because the model doesn't seem to handle this correctly.
	delete_hierarchy_dialog (data);

	gnc_suspend_gui_refresh ();

        account_group_merge(gnc_get_current_group(), data->our_final_group);

        delete_our_final_group (data);
        qof_book_destroy(data->temporary);

        when_completed = data->when_completed;
	g_free(data);
	gnc_resume_gui_refresh ();
        if (when_completed)
        {
                (*when_completed)();
        }

	LEAVE (" ");
}

static GtkWidget *
gnc_create_hierarchy_druid (GncHierarchyDruidFinishedCallback when_completed)
{
	hierarchy_data *data;
	GtkWidget *dialog;
	GtkWidget *druid;
	GtkTreeView *tree_view;
	GtkWidget *box, *start_page;
	GladeXML *xml;
	GdkColor *color;
	
	data = g_new0 (hierarchy_data, 1);
	xml = gnc_glade_xml_new ("account.glade", "Hierarchy Druid");
	
	dialog = glade_xml_get_widget (xml, "Hierarchy Druid");
	data->dialog = dialog;
	
	druid = glade_xml_get_widget (xml, "hierarchy_druid");
	gnc_druid_set_colors (GNOME_DRUID (druid));
	
	start_page = glade_xml_get_widget (xml, "start_page");
	gtk_widget_show (start_page);
	gtk_widget_show (glade_xml_get_widget (xml, "newUserDruidFinishPage"));
	
	/* Currency Page */
	data->currency_selector = gnc_currency_edit_new();
	gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(data->currency_selector), gnc_default_currency());
	gtk_widget_show (data->currency_selector);
	box = glade_xml_get_widget (xml, "currency_chooser_vbox");
	gtk_box_pack_start(GTK_BOX(box), data->currency_selector, FALSE, FALSE, 0);

	/* Categories Page */
	tree_view = GTK_TREE_VIEW(glade_xml_get_widget (xml, "account_categories_tree_view"));
	g_signal_connect (G_OBJECT (gtk_tree_view_get_selection (tree_view)), "changed",
			  G_CALLBACK (categories_tree_selection_changed), data);
	gtk_tree_selection_set_mode (gtk_tree_view_get_selection (tree_view), GTK_SELECTION_SINGLE);
	data->categories_tree = tree_view;
	
	data->category_accounts_container = glade_xml_get_widget (xml, "accounts_in_category");
	data->category_description = GTK_TEXT_VIEW(glade_xml_get_widget (xml, "account_types_description"));
	color = &GNOME_DRUID_PAGE_EDGE(start_page)->textbox_color;
	gtk_widget_modify_base(GTK_WIDGET(data->category_description), GTK_STATE_INSENSITIVE, color);
	
	/* Final Accounts Page */
	data->final_account_tree_container = glade_xml_get_widget (xml, "final_account_tree_box");
	data->final_account_tree = NULL;
	
	data->balance_hash = g_hash_table_new(NULL, NULL);
	
	g_signal_connect (G_OBJECT(dialog), "destroy",
			  G_CALLBACK (gnc_hierarchy_destroy_cb), data);
	
	glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, data);

        data->when_completed = when_completed;
	
	return dialog;
}

GtkWidget*
gnc_ui_hierarchy_druid(void)
{
  return gnc_create_hierarchy_druid(NULL);
}

GtkWidget*
gnc_ui_hierarchy_druid_with_callback(GncHierarchyDruidFinishedCallback when_finished)
{
  return gnc_create_hierarchy_druid(when_finished);
}

static void
create_account_page(void)
{
  GncPluginPage *page;
  page = gnc_plugin_page_account_tree_new();
  gnc_main_window_open_page(NULL, page);
}

static void
gnc_ui_hierarchy_druid_hook (void)
{
  if (gnc_gconf_get_bool(GCONF_SECTION, "show_on_new_file", NULL)) {
    gnc_ui_hierarchy_druid_with_callback(create_account_page);
  }
}

void
gnc_ui_hierarchy_druid_initialize (void)
{
  gnc_hook_add_dangler(HOOK_NEW_BOOK,
		       (GFunc)gnc_ui_hierarchy_druid_hook, NULL);
}
