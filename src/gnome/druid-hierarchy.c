/********************************************************************\
 * druid-hierarchy.c -- account hierarchy creation functionality    *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
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
#include "dialog-new-user.h"
#include "dialog-utils.h"
#include "druid-hierarchy.h"
#include "druid-merge.h"
#include "druid-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-general-select.h"
#include "gnc-gconf-utils.h"
#include "gnc-hooks.h"
#include "gnc-component-manager.h"
#include "../gnome-utils/gnc-dir.h"
#include "gnc-gui-query.h"
#include "gnc-tree-model-example-account.h"
#include "gnc-tree-model-selection.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui-util.h"
#include "io-example-account.h"
#include "top-level.h"

#include "gnc-engine.h"
static QofLogModule log_module = GNC_MOD_IMPORT; 

#define GCONF_SECTION "dialogs/new_hierarchy"

static GtkWidget *hierarchy_window = NULL;
GtkWidget *qof_book_merge_window = NULL;
static AccountGroup *our_final_group = NULL;
QofBook *temporary;

typedef struct {
  GtkWidget *dialog;

  GtkWidget *currency_selector;

  GtkTreeView *categories_tree;
  GtkLabel *category_description;
  GtkBox *category_accounts_box;
  GtkTreeView *category_accounts_tree;
  gboolean category_set_changed;

  GncTreeViewAccount *final_account_tree;
  GtkWidget *final_account_tree_box;
  Account *selected_account;
  GNCAmountEdit *balance_edit;
  GHashTable *balance_hash;
} hierarchy_data;

static void on_balance_changed (GNCAmountEdit *gae, hierarchy_data *data);
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
void on_cancel (GnomeDruid      *gnomedruid, hierarchy_data *data);
void on_finish (GnomeDruidPage  *gnomedruidpage, gpointer arg1, hierarchy_data *data);



static void
delete_hierarchy_window (void)
{
  if (!hierarchy_window) return;

  gtk_widget_destroy (hierarchy_window);
  hierarchy_window = NULL;
}

static void
destroy_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
  char *fullname = key;
  gnc_numeric *balance = value;

  g_free (fullname);
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

static void
block_amount_changed (hierarchy_data *data)
{
  g_signal_handlers_block_by_func
    (G_OBJECT (data->balance_edit),
     G_CALLBACK (on_balance_changed), data);
}

static void
unblock_amount_changed (hierarchy_data *data)
{
  g_signal_handlers_unblock_by_func
    (G_OBJECT (data->balance_edit),
     G_CALLBACK (on_balance_changed), data);
}

static gnc_numeric
get_final_balance (GHashTable *hash, Account *account)
{
  gnc_numeric *balance;
  char *fullname;

  if (!hash || !account)
    return gnc_numeric_zero ();

  fullname = xaccAccountGetFullName (account, ':');
  balance = g_hash_table_lookup (hash, fullname);
  g_free (fullname);

  if (balance)
    return *balance;
  return gnc_numeric_zero ();
}

static void
set_final_balance (GHashTable *hash, Account *account, gnc_numeric in_balance)
{
  gnc_numeric *balance;
  char *fullname;

  if (!hash || !account)
    return;

  fullname = xaccAccountGetFullName (account, ':');

  balance = g_hash_table_lookup (hash, fullname);
  if (balance) {
    *balance = in_balance;
    g_free (fullname);
    return;
  }

  balance = g_new (gnc_numeric, 1);
  *balance = in_balance;
  g_hash_table_insert (hash, fullname, balance);
  /* fullname string now owned by the hash */
}

static void
update_account_balance (Account *account,
			hierarchy_data *data)
{
  gboolean result, placeholder;

  if (!account)
    return;

  block_amount_changed (data);
  result = gnc_amount_edit_evaluate (data->balance_edit);
  unblock_amount_changed (data);

  if (result)
  {
    gnc_numeric balance;
    GNCPrintAmountInfo print_info;
    const char *string;

    balance = gnc_amount_edit_get_amount (data->balance_edit);
    placeholder = xaccAccountGetPlaceholder (account);

    print_info = gnc_account_print_info (account, FALSE);
    string = xaccPrintAmount (balance, print_info);

    if (gnc_numeric_zero_p (balance) || placeholder)
      string = "";

    if (gnc_reverse_balance (account))
      balance = gnc_numeric_neg (balance);

    set_final_balance (data->balance_hash, account, balance);
    gnc_engine_gen_event ((QofEntity*)account,
			       GNC_EVENT_MODIFY);
  }
}

static void
on_balance_changed (GNCAmountEdit *gae, hierarchy_data *data)
{
	Account *account;

	g_return_if_fail(data != NULL);
	if (!GTK_WIDGET_SENSITIVE (GTK_WIDGET (gae)))
		return;

	account = gnc_tree_view_account_get_selected_account(data->final_account_tree);
	if (account == NULL)
	  return;
	update_account_balance (account, data);
}

static void
on_balance_focus_out (GNCAmountEdit *gae, GdkEventFocus *event, hierarchy_data *data)
{
  on_balance_changed (gae, data);
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


static gboolean
start_helper (GncTreeModelSelection *selection_model,
	      GtkTreePath           *selection_path,
	      GtkTreeIter           *selection_iter,
	      gpointer               data)
{
	GtkTreeModel      *model;
	GtkTreeIter  	   iter;
	GncExampleAccount *gea;

	g_return_val_if_fail(GNC_IS_TREE_MODEL_SELECTION(selection_model), FALSE);
	model = gnc_tree_model_selection_get_model (selection_model);
	gnc_tree_model_selection_convert_iter_to_child_iter (selection_model,
							     &iter, selection_iter);

	gea = gnc_tree_model_example_account_get_account (GNC_TREE_MODEL_EXAMPLE_ACCOUNT(model), &iter);
	if (gea != NULL) {
	  gnc_tree_model_selection_set_selected (selection_model,
						 selection_iter,
						 gea->start_selected);
	}

	return FALSE;  /* Run entire tree */
}

static void
account_categories_tree_view_prepare (hierarchy_data  *data)
{
	GSList *list;
	gchar *locale_dir;
	GtkTreeView *tree_view;
	GtkTreeModel *model, *selection_model;
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;

	locale_dir = gnc_get_ea_locale_dir (GNC_ACCOUNTS_DIR);
 	list = gnc_load_example_account_list (temporary,
					      locale_dir);
	g_free (locale_dir);

	/* Prepare the account_categories GtkTreeView with a model and with some columns */
	tree_view = data->categories_tree;
	model = gnc_tree_model_example_account_new (list);
	selection_model = gnc_tree_model_selection_new (model);
	gtk_tree_view_set_model (tree_view, selection_model);
	g_object_unref (model);
	g_object_unref (selection_model);

	g_signal_connect (G_OBJECT (selection_model), "row_changed",
			  G_CALLBACK (categories_selection_changed),
			  data);

	column = gnc_tree_model_selection_create_tree_view_column (GNC_TREE_MODEL_SELECTION (selection_model),
								   _("Selected"));
	gtk_tree_view_append_column (tree_view, column);

	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes (_("Account Types"),
							   renderer,
							   "text", GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_TITLE,
							   NULL);
	gtk_tree_view_append_column (tree_view, column);

	column = gtk_tree_view_column_new_with_attributes (_("Description"),
							   renderer,
							   "text", GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_SHORT_DESCRIPTION,
							   NULL);
	gtk_tree_view_append_column (tree_view, column);

	/* Now set the selected checkbox for each item */
	gtk_tree_model_foreach (GTK_TREE_MODEL (selection_model),
				(GtkTreeModelForeachFunc)start_helper,
				NULL);
}

void
on_choose_account_categories_prepare (GnomeDruidPage  *gnomedruidpage,
				      gpointer         arg1,
				      hierarchy_data  *data)
{
  gpointer added_ptr;

  added_ptr = g_object_get_data (G_OBJECT(hierarchy_window),
                                 "account_list_added");

  if (GPOINTER_TO_INT(added_ptr) == 0)
  {
    /* Build the categories tree if necessary */
    gnc_suspend_gui_refresh ();
    temporary = qof_book_new();
    account_categories_tree_view_prepare (data);
    gnc_resume_gui_refresh ();

    /* clear out the description/tree */
    if (data->category_accounts_tree)
      gtk_widget_destroy(GTK_WIDGET(data->category_accounts_tree));
    data->category_accounts_tree = NULL;
    gtk_label_set_text (data->category_description, "");

    g_object_set_data (G_OBJECT(hierarchy_window),
                       "account_list_added", GINT_TO_POINTER(1));
  }
}

static void
categories_tree_selection_changed (GtkTreeSelection *selection,
				      hierarchy_data *data)
{
	GtkTreeView *tree_view;
	GtkTreeModel *selection_model, *model;
	GtkTreeIter selection_iter, iter;
	GncExampleAccount *gea;

	/* Remove the old account tree */
	if (data->category_accounts_tree)
	  gtk_widget_destroy(GTK_WIDGET(data->category_accounts_tree));
	data->category_accounts_tree = NULL;
	gtk_label_set_text (data->category_description, "");

	/* Add a new one if something selected */
	if (gtk_tree_selection_get_selected (selection, &selection_model, &selection_iter)) {
		model = gnc_tree_model_selection_get_model (GNC_TREE_MODEL_SELECTION (selection_model));
		gnc_tree_model_selection_convert_iter_to_child_iter (GNC_TREE_MODEL_SELECTION (selection_model),
								     &iter, &selection_iter);
		gea = gnc_tree_model_example_account_get_account (GNC_TREE_MODEL_EXAMPLE_ACCOUNT (model),
								  &iter);
		gtk_label_set_text (data->category_description, gea->long_description);

		tree_view = gnc_tree_view_account_new_with_group (gea->group, FALSE);
		gnc_tree_view_configure_columns (GNC_TREE_VIEW(tree_view), NULL);

		data->category_accounts_tree = tree_view;
		gtk_tree_view_expand_all (tree_view);
		gtk_container_add(GTK_CONTAINER(data->category_accounts_box), GTK_WIDGET(tree_view));
		gtk_widget_show(GTK_WIDGET(tree_view));
	}
}

static gboolean
select_helper (GncTreeModelSelection *selection_model,
	       GtkTreePath 	     *selection_path,
	       GtkTreeIter 	     *selection_iter,
	       gpointer               data)
{
	GtkTreeModel 	  *model;
	GtkTreeIter  	   iter;
	GncExampleAccount *gea;

	g_return_val_if_fail(GNC_IS_TREE_MODEL_SELECTION(selection_model), FALSE);
	model = gnc_tree_model_selection_get_model (selection_model);
	gnc_tree_model_selection_convert_iter_to_child_iter (selection_model,
							     &iter, selection_iter);

	gea = gnc_tree_model_example_account_get_account (GNC_TREE_MODEL_EXAMPLE_ACCOUNT(model), &iter);
	if ((gea != NULL) && !gea->exclude_from_select_all) {
	  gnc_tree_model_selection_set_selected (selection_model,
						 selection_iter,
						 GPOINTER_TO_INT(data));
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
delete_our_final_group (void)
{
  if (our_final_group != NULL)
  {
    xaccAccountGroupBeginEdit (our_final_group);
    xaccAccountGroupDestroy (our_final_group);
    our_final_group = NULL;
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

static GSList *
get_selected_account_list (GtkTreeView *tree_view)
{
	GSList *actlist = NULL;
	GtkTreeModel *model, *selection_model;
	GtkTreeIter iter, selection_iter;
	GncExampleAccount *gea;

	selection_model = gtk_tree_view_get_model (tree_view);
	model = gnc_tree_model_selection_get_model (GNC_TREE_MODEL_SELECTION (selection_model));

	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (model), &iter)) {
		do {
			gnc_tree_model_selection_convert_child_iter_to_iter (GNC_TREE_MODEL_SELECTION (selection_model),
									     &selection_iter, &iter);

			if (gnc_tree_model_selection_is_selected (GNC_TREE_MODEL_SELECTION (selection_model),
								  &selection_iter)) {
				gea = gnc_tree_model_example_account_get_account (GNC_TREE_MODEL_EXAMPLE_ACCOUNT (model),
										  &iter);
				
				actlist = g_slist_append (actlist, gea);
			}
		} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (model), &iter));
	}

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

	g_object_set (G_OBJECT (cell),
		      "text", string,
		      (char *)NULL);
}

static void
on_final_account_tree_selection_changed (GtkTreeSelection *selection,
					 hierarchy_data *data)
{
	Account *account;
	GNCPrintAmountInfo print_info;
	gnc_numeric balance;
	GtkWidget *entry;

	/* Update the account we came from */
	if (data->selected_account) {
	  update_account_balance (data->selected_account, data);
	  data->selected_account = NULL;
	}

	/* Clean up the amount entry box */
	entry = gnc_amount_edit_gtk_entry (data->balance_edit);
	gtk_entry_set_text (GTK_ENTRY (entry), "");
	gtk_widget_set_sensitive (GTK_WIDGET (data->balance_edit), FALSE);

	/* What's the new selection? */
	if (!gtk_tree_selection_get_selected (selection, NULL, NULL))
		return;

	/* Ignore equity accounts */
	account = gnc_tree_view_account_get_selected_account(data->final_account_tree);
	if (xaccAccountGetType (account) == EQUITY)
		return;

	/* Set up the amount entry box */
	data->selected_account = account;
	gtk_widget_set_sensitive (GTK_WIDGET (data->balance_edit),
				  !xaccAccountGetPlaceholder (account));
	balance = get_final_balance (data->balance_hash, account);
	if (gnc_reverse_balance (account))
		balance = gnc_numeric_neg (balance);
	print_info = gnc_account_print_info (account, FALSE);
	gnc_amount_edit_set_print_info (data->balance_edit, print_info);
	gnc_amount_edit_set_fraction (data->balance_edit,
				      xaccAccountGetCommoditySCU (account));
	block_amount_changed (data);
	gnc_amount_edit_set_amount (data->balance_edit, balance);
	if (gnc_numeric_zero_p (balance))
		gtk_entry_set_text (GTK_ENTRY (entry), "");
	unblock_amount_changed (data);
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
  GtkWidget *entry;

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
  delete_our_final_group ();


  /* Build a new account list */
  actlist = get_selected_account_list (data->categories_tree);
  com = gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT(data->currency_selector));
  our_final_group = hierarchy_merge_groups (actlist, com);


  /* Now build a new account tree */
  data->final_account_tree = GNC_TREE_VIEW_ACCOUNT(gnc_tree_view_account_new_with_group (our_final_group, FALSE));
  tree_view = GTK_TREE_VIEW(data->final_account_tree);
  gtk_tree_view_set_headers_visible (tree_view, TRUE);

  gnc_tree_view_configure_columns (GNC_TREE_VIEW(data->final_account_tree),
				   "type", "placeholder", NULL);

  selection = gtk_tree_view_get_selection (tree_view);
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);
  g_signal_connect (G_OBJECT (selection), "changed",
		    G_CALLBACK (on_final_account_tree_selection_changed),
		    data);

  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer),
		"xalign", 1.0,
		(char *)NULL);
  column = gtk_tree_view_column_new_with_attributes (_("Opening Balance"),
						     renderer,
						     NULL);
  gtk_tree_view_column_set_cell_data_func (column, renderer, 
					   balance_cell_data_func,
					   (gpointer)data, NULL);
  gnc_tree_view_append_column (GNC_TREE_VIEW(tree_view), column);

  gtk_container_add(GTK_CONTAINER(data->final_account_tree_box),
		    GTK_WIDGET(data->final_account_tree));

  /* Expand the entire tree */
  gtk_tree_view_expand_all (tree_view);
  gtk_widget_show(GTK_WIDGET(data->final_account_tree));
  gnc_resume_gui_refresh ();



  /* Now set up the balance widget */
  entry = gnc_amount_edit_gtk_entry (data->balance_edit);

  block_amount_changed (data);
  gnc_amount_edit_set_amount (data->balance_edit, gnc_numeric_zero ());
  gtk_entry_set_text (GTK_ENTRY(entry), "");
  unblock_amount_changed (data);

  gtk_widget_set_sensitive (GTK_WIDGET (data->balance_edit), FALSE);
}


void
on_cancel (GnomeDruid      *gnomedruid,
	   hierarchy_data  *data)
{
  gnc_suspend_gui_refresh ();
  delete_our_final_group ();
  delete_hierarchy_window ();
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
	gnc_suspend_gui_refresh ();
	
	if (our_final_group)
	  xaccGroupForEachAccount (our_final_group, (AccountCallback)starting_balance_helper,
				   data, TRUE);
	ENTER (" ");
	qof_book_merge_window = g_object_get_data (G_OBJECT (hierarchy_window), "Merge Druid");
	if(qof_book_merge_window) {
		DEBUG ("qof_book_merge_window found");
		if (our_final_group) 
			xaccGroupConcatGroup (gnc_get_current_group (), our_final_group);
		gtk_widget_show(qof_book_merge_window);
		qof_book_destroy(temporary);
		delete_hierarchy_window ();
		gnc_resume_gui_refresh ();
		LEAVE (" ");
		return;
	}
	delete_hierarchy_window ();
	
	gncp_new_user_finish ();
	
	gnc_set_first_startup (FALSE);
	
	if (our_final_group)
	xaccGroupConcatGroup (gnc_get_current_group (), our_final_group);
	qof_book_destroy(temporary);
	
	g_free(data);
	gnc_resume_gui_refresh ();
	LEAVE (" ");
}

static GtkWidget *
gnc_create_hierarchy_druid (void)
{
	hierarchy_data *data;
	GtkWidget *balance_edit;
	GtkWidget *dialog;
	GtkWidget *druid;
	GtkTreeView *tree_view;
	GtkWidget *box;
	GladeXML *xml;
	
	data = g_new0 (hierarchy_data, 1);
	xml = gnc_glade_xml_new ("account.glade", "Hierarchy Druid");
	
	dialog = glade_xml_get_widget (xml, "Hierarchy Druid");
	data->dialog = dialog;
	
	druid = glade_xml_get_widget (xml, "hierarchy_druid");
	gnc_druid_set_colors (GNOME_DRUID (druid));
	
	gtk_widget_show (glade_xml_get_widget (xml, "start_page"));
	gtk_widget_show (glade_xml_get_widget (xml, "newUserDruidFinishPage"));
	
	/* Currency Page */
	data->currency_selector = gnc_currency_edit_new();
	gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(data->currency_selector), gnc_default_currency());
	gtk_widget_show (data->currency_selector);
	box = glade_xml_get_widget (xml, "currency_chooser_vbox");
	gtk_box_pack_start(GTK_BOX(box), data->currency_selector, FALSE, FALSE, 0);

	/* Categories Page */
	balance_edit = gnc_amount_edit_new ();
	data->balance_edit = GNC_AMOUNT_EDIT(balance_edit);
	gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (balance_edit), TRUE);
	gtk_widget_show (balance_edit);
	box = glade_xml_get_widget (xml, "start_balance_box");
	gtk_box_pack_start (GTK_BOX (box), balance_edit, TRUE, TRUE, 0);
	g_signal_connect (G_OBJECT (balance_edit), "amount_changed",
			  G_CALLBACK (on_balance_changed), data);
	g_signal_connect (G_OBJECT (balance_edit), "focus_out_event",
			  G_CALLBACK (on_balance_focus_out), data);
	
	/* Opening Balances Page */
	tree_view = GTK_TREE_VIEW(glade_xml_get_widget (xml, "account_categories_tree_view"));
	g_signal_connect (G_OBJECT (gtk_tree_view_get_selection (tree_view)), "changed",
			  G_CALLBACK (categories_tree_selection_changed), data);
	gtk_tree_selection_set_mode (gtk_tree_view_get_selection (tree_view), GTK_SELECTION_SINGLE);
	data->categories_tree = tree_view;
	
	data->category_accounts_box = GTK_BOX(glade_xml_get_widget (xml, "accounts_in_category"));
	data->category_description = GTK_LABEL(glade_xml_get_widget (xml, "account_types_description_entry"));
	
	data->final_account_tree_box = glade_xml_get_widget (xml, "final_account_tree_box");
	data->final_account_tree = NULL;
	
	data->balance_hash = g_hash_table_new (g_str_hash, g_str_equal);
	
	g_signal_connect (G_OBJECT(dialog), "destroy",
			  G_CALLBACK (gnc_hierarchy_destroy_cb), data);
	
	glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, data);
	
	return dialog;
}

GtkWidget*
gnc_ui_hierarchy_running (void)
{
	if (hierarchy_window) return hierarchy_window;
	return NULL;
}

void
gnc_ui_hierarchy_druid (void)
{
	if (hierarchy_window) return;

	hierarchy_window = gnc_create_hierarchy_druid ();

	return;
}

static void
gnc_ui_hierarchy_druid_hook (void)
{
  if (gnc_gconf_get_bool(GCONF_SECTION, "show_on_new_file", NULL)) {
    printf("start druid\n");
    gnc_ui_hierarchy_druid();
  }
}

void
gnc_ui_hierarchy_druid_initialize (void)
{
  gnc_hook_add_dangler(HOOK_NEW_BOOK,
		       (GFunc)gnc_ui_hierarchy_druid_hook, NULL);
}
