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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <libgnomeui/gnome-window-icon.h>

#include "Group.h"
#include "dialog-new-user.h"
#include "dialog-utils.h"
#include "druid-hierarchy.h"
#include "druid-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-general-select.h"
#include "gnc-component-manager.h"
#include "../gnome-utils/gnc-dir.h"
#include "gnc-gui-query.h"
#include "gnc-tree-model-account.h"
#include "gnc-tree-model-example-account.h"
#include "gnc-tree-model-selection.h"
#include "gnc-ui-util.h"
#include "global-options.h"
#include "io-example-account.h"
#include "top-level.h"

static GtkWidget *hierarchy_window = NULL;
static AccountGroup *our_final_group = NULL;


static void on_balance_changed (GNCAmountEdit *gae);


static GtkWidget*
hierarchy_get_widget (const char *name)
{
  if (!hierarchy_window) return NULL;

  return gnc_glade_lookup_widget (hierarchy_window, name);
}

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

static GNCAmountEdit *
get_balance_editor (void)
{
  if (!hierarchy_window) return NULL;

  return g_object_get_data (G_OBJECT (hierarchy_window), "balance_editor");
}

static GtkWidget *
get_currency_editor(void)
{
  GtkWidget *selector;
  GtkWidget *tmp_wid = g_object_get_data (G_OBJECT (hierarchy_window),
                                          "currency_editor");
  if (tmp_wid)
    return tmp_wid;

  selector = gnc_currency_edit_new();
  gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(selector), gnc_default_currency());
  gtk_widget_show (selector);
  g_object_set_data(G_OBJECT(hierarchy_window),
		    "currency_editor", selector);
  return selector;
}

static void
gnc_hierarchy_destroy_cb (GtkObject *obj, gpointer user_data)
{
  GHashTable *hash;

  hash = g_object_get_data (G_OBJECT (obj), "balance_hash");
  if (hash)
  {
    g_hash_table_foreach (hash, destroy_hash_helper, NULL);
    g_hash_table_destroy (hash);
    g_object_set_data (G_OBJECT (obj), "balance_hash", NULL);
  }
}

static void
block_amount_changed (void)
{
  GNCAmountEdit *balance_edit;

  balance_edit = get_balance_editor ();
  if (!balance_edit) return;

  g_signal_handlers_block_by_func
    (G_OBJECT (balance_edit),
     G_CALLBACK (on_balance_changed), NULL);
}

static void
unblock_amount_changed (void)
{
  GNCAmountEdit *balance_edit;

  balance_edit = get_balance_editor ();
  if (!balance_edit) return;

  g_signal_handlers_unblock_by_func
    (G_OBJECT (balance_edit),
     G_CALLBACK (on_balance_changed), NULL);
}

static gnc_numeric
get_final_balance (Account *account)
{
  GHashTable *hash;
  gnc_numeric *balance;
  char *fullname;

  if (!account || !hierarchy_window) return gnc_numeric_zero ();

  hash = g_object_get_data (G_OBJECT (hierarchy_window), "balance_hash");
  if (!hash) return gnc_numeric_zero ();

  fullname = xaccAccountGetFullName (account, ':');

  balance = g_hash_table_lookup (hash, fullname);

  g_free (fullname);

  if (balance)
    return *balance;

  return gnc_numeric_zero ();
}

static void
set_final_balance (Account *account, gnc_numeric in_balance)
{
  GHashTable *hash;
  gnc_numeric *balance;
  char *fullname;

  if (!account || !hierarchy_window) return;

  hash = g_object_get_data (G_OBJECT (hierarchy_window), "balance_hash");
  if (!hash) return;

  fullname = xaccAccountGetFullName (account, ':');

  balance = g_hash_table_lookup (hash, fullname);
  if (balance)
  {
    *balance = in_balance;
    g_free (fullname);
  }
  else
  {
    balance = g_new (gnc_numeric, 1);
    *balance = in_balance;

    g_hash_table_insert (hash, fullname, balance);
  }
}

static void
update_account_balance (GncTreeModelAccount *model, GtkTreeIter *iter)
{
  Account *account;
  GNCAmountEdit *balance_edit;
  gboolean result, placeholder;

  balance_edit = get_balance_editor ();

  account = gnc_tree_model_account_get_account (model, iter);
  if (!account)
    return;

  block_amount_changed ();
  result = gnc_amount_edit_evaluate (balance_edit);
  unblock_amount_changed ();

  if (result)
  {
    gnc_numeric balance;
    GNCPrintAmountInfo print_info;
    const char *string;
    GtkTreePath *path;

    balance = gnc_amount_edit_get_amount (balance_edit);
    placeholder = xaccAccountGetPlaceholder (account);

    print_info = gnc_account_print_info (account, FALSE);
    string = xaccPrintAmount (balance, print_info);

    if (gnc_numeric_zero_p (balance) || placeholder)
      string = "";

    path = gtk_tree_model_get_path (GTK_TREE_MODEL (model), iter);
    gtk_tree_model_row_changed (GTK_TREE_MODEL (model), path, iter);
    gtk_tree_path_free (path);

    if (gnc_reverse_balance (account))
      balance = gnc_numeric_neg (balance);

    set_final_balance (account, balance);
  }
}

static void
on_balance_changed (GNCAmountEdit *gae)
{
	GtkTreeView *tree_view;
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeIter iter;

	if (!GTK_WIDGET_SENSITIVE (GTK_WIDGET (gae)))
		return;

	tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("final_account_tree_view"));
	selection = gtk_tree_view_get_selection (tree_view);

	if (!gtk_tree_selection_get_selected (selection, &model, &iter))
		return;

	update_account_balance (GNC_TREE_MODEL_ACCOUNT (model), &iter);
}

static void
on_choose_currency_prepare (GnomeDruidPage  *gnomedruidpage,
                            gpointer         arg1,
                            gpointer         user_data)
{
  if(!GPOINTER_TO_INT (g_object_get_data
                       (G_OBJECT(hierarchy_window), "currency_added")))
  {
    g_object_set_data (G_OBJECT(hierarchy_window),
                       "currency_added", GINT_TO_POINTER (1));

    gtk_box_pack_start(GTK_BOX(gnc_glade_lookup_widget
                               (hierarchy_window, "currency_chooser_vbox")),
                       GTK_WIDGET(get_currency_editor()), FALSE, FALSE, 0);
  }
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

#if 1
/* GNOME 2 Port (TESTCODE) */
static GSList *
get_example_account_list (void)
{
	GSList *list = NULL;
	GncExampleAccount *gea;
	Account *account, *account2;

	gea = g_new0(GncExampleAccount, 1);

	gea->book = gnc_get_current_book ();
	gea->filename = "sample-file";
	gea->group = xaccMallocAccountGroup(gea->book);
	gea->title = "Sample Account";
	gea->short_description = "Sample Description";
	gea->long_description = "Sample Description. The long version";

	xaccAccountGroupBeginEdit (gea->group);

	account = xaccMallocAccount (gea->book);

	xaccAccountSetType (account, INCOME);
	xaccAccountSetName (account, "SampleAccount");
	xaccAccountSetCode (account, "SampleCode");
	xaccAccountSetDescription (account, "An Sample Account");
	xaccAccountSetNotes (account, "SampleNotes");

	xaccGroupInsertAccount (gea->group, account);

	account2 = xaccMallocAccount (gea->book);

	xaccAccountSetType (account2, EXPENSE);
	xaccAccountSetName (account2, "SampleAccount 3");
	xaccAccountSetCode (account2, "SampleCode");
	xaccAccountSetDescription (account2, "An Third Sample Account");
	xaccAccountSetNotes (account2, "SampleNotes");

	xaccAccountInsertSubAccount (account, account2);

	account2 = xaccMallocAccount (gea->book);

	xaccAccountSetType (account2, EXPENSE);
	xaccAccountSetName (account2, "SampleAccount 5");
	xaccAccountSetCode (account2, "SampleCode");
	xaccAccountSetDescription (account2, "A 5. Sample Account");
	xaccAccountSetNotes (account2, "SampleNotes");

	xaccAccountInsertSubAccount (account, account2);

	account = xaccMallocAccount (gea->book);

	xaccAccountSetType (account, EXPENSE);
	xaccAccountSetName (account, "SampleAccount 2");
	xaccAccountSetCode (account, "SampleCode");
	xaccAccountSetDescription (account, "An Second Sample Account");
	xaccAccountSetNotes (account, "SampleNotes");

	xaccGroupInsertAccount (gea->group, account);

	account2 = xaccMallocAccount (gea->book);

	xaccAccountSetType (account2, EXPENSE);
	xaccAccountSetName (account2, "SampleAccount 4");
	xaccAccountSetCode (account2, "SampleCode");
	xaccAccountSetDescription (account2, "A Fourth Sample Account");
	xaccAccountSetNotes (account2, "SampleNotes");

	xaccAccountInsertSubAccount (account, account2);

	account = xaccMallocAccount (gea->book);

	xaccAccountSetType (account, EXPENSE);
	xaccAccountSetName (account, "SampleAccount 6");
	xaccAccountSetCode (account, "SampleCode");
	xaccAccountSetDescription (account, "A 6. Sample Account");
	xaccAccountSetNotes (account, "SampleNotes");

	xaccGroupInsertAccount (gea->group, account);

	xaccGroupMarkSaved(gea->group);
	xaccAccountGroupCommitEdit(gea->group);

	list = g_slist_append (list, gea);

	return list;
}
#endif

static void
account_types_tree_view_prepare (void)
{
	GSList *list;
	gchar *locale_dir = gnc_get_ea_locale_dir (GNC_ACCOUNTS_DIR);
	GtkTreeModel *model, *selection_model;
	GtkTreeView *tree_view; 
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;
	GtkTreeIter iter, selection_iter;
	GncExampleAccount *gea;

        /* GNOME 2 port (There seem to be a xml bug, so create a TestAccount) */
	list = get_example_account_list ();

 	/*list = gnc_load_example_account_list (gnc_get_current_book (),
					      locale_dir);*/

	g_free (locale_dir);

	/* Prepare the account_types GtkTreeView with a model and with some columns */
	model = gnc_tree_model_example_account_new (list);
	selection_model = gnc_tree_model_selection_new (model);
	tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("account_types_tree_view"));
	gtk_tree_view_set_model (tree_view, selection_model);
	g_object_unref (G_OBJECT (model));
	g_object_unref (G_OBJECT (selection_model));

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


	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (model), &iter)) {
		do {
			gea = gnc_tree_model_example_account_get_account (GNC_TREE_MODEL_EXAMPLE_ACCOUNT (model),
									  &iter);
			if (gea != NULL) {
				gnc_tree_model_selection_convert_child_iter_to_iter (GNC_TREE_MODEL_SELECTION (selection_model),
										     &selection_iter, &iter);
				gnc_tree_model_selection_set_selected (GNC_TREE_MODEL_SELECTION (selection_model),
								       &selection_iter,
								       gea->start_selected);
			}
		} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (model), &iter));
	}
}

static void
on_choose_account_types_prepare (GnomeDruidPage  *gnomedruidpage,
                                 gpointer         arg1,
                                 gpointer         user_data)
{
  gpointer added_ptr;

  added_ptr = g_object_get_data (G_OBJECT(hierarchy_window),
                                 "account_list_added");

  if (GPOINTER_TO_INT(added_ptr) == 0)
  {
    gnc_suspend_gui_refresh ();

    account_types_tree_view_prepare ();


    gnc_resume_gui_refresh ();

    /* clear out the description/tree */
    {
      GtkLabel *datext = GTK_LABEL (hierarchy_get_widget
				    ("account_types_description_entry"));
      GtkTreeView *tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("account_type_tree"));
      GtkTreeModel *model = gnc_tree_model_account_new (NULL);
      GtkCellRenderer *renderer = gtk_cell_renderer_text_new ();
      GtkTreeViewColumn *column = gtk_tree_view_column_new_with_attributes ("Account",
									    renderer,
									    "text", GNC_TREE_MODEL_ACCOUNT_COL_NAME,
									    NULL);
      gtk_tree_view_append_column (tree_view, column);
      gtk_tree_view_set_expander_column (tree_view, column);
      gtk_tree_view_set_model (tree_view, model);
      g_object_unref (G_OBJECT (model));

      gtk_label_set_text (datext, "");
    }

    g_object_set_data (G_OBJECT(hierarchy_window),
                       "account_list_added", GINT_TO_POINTER(1));
  }
}

static void
account_types_selection_changed (GtkTreeSelection *selection,
				 gpointer data)
{
	GtkLabel *datext = GTK_LABEL (hierarchy_get_widget ("account_types_description_entry"));
	GtkTreeView *tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("account_type_tree"));
	GtkTreeModel *selection_model, *model;
	GtkTreeIter selection_iter, iter;
	GncExampleAccount *gea;

	if (gtk_tree_selection_get_selected (selection, &selection_model, &selection_iter)) {
		model = gnc_tree_model_selection_get_model (GNC_TREE_MODEL_SELECTION (selection_model));
		gnc_tree_model_selection_convert_iter_to_child_iter (GNC_TREE_MODEL_SELECTION (selection_model),
								     &iter, &selection_iter);
		gea = gnc_tree_model_example_account_get_account (GNC_TREE_MODEL_EXAMPLE_ACCOUNT (model),
								  &iter);
		gtk_label_set_text (datext, gea->long_description);
		gnc_tree_model_account_set_root (GNC_TREE_MODEL_ACCOUNT (gtk_tree_view_get_model (tree_view)),
						 gea->group);
		gtk_tree_view_expand_all (tree_view);
	} else {
		gtk_label_set_text (datext, "");
		gnc_tree_model_account_set_root (GNC_TREE_MODEL_ACCOUNT (gtk_tree_view_get_model (tree_view)),
						 NULL);
	}
}

static void
select_all_clicked (GtkButton       *button,
                    gpointer         user_data)
{
	GtkTreeView *account_types_tree_view;
	GtkTreeModel *model, *selection_model;
	GtkTreeIter iter, selection_iter;
	GncExampleAccount *gea;

	account_types_tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("account_types_tree_view"));
	selection_model = gtk_tree_view_get_model (account_types_tree_view);
	model = gnc_tree_model_selection_get_model (GNC_TREE_MODEL_SELECTION (selection_model));

	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (model), &iter)) {
		do {
			gea = gnc_tree_model_example_account_get_account (GNC_TREE_MODEL_EXAMPLE_ACCOUNT (model),
									  &iter);
			if (gea != NULL && gea->exclude_from_select_all == FALSE) {
				gnc_tree_model_selection_convert_child_iter_to_iter (GNC_TREE_MODEL_SELECTION (selection_model),
										     &selection_iter, &iter);
				gnc_tree_model_selection_set_selected (GNC_TREE_MODEL_SELECTION (selection_model),
								       &selection_iter,
								       TRUE);
			}
		} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (model), &iter));
	}
}

static void
clear_all_clicked (GtkButton       *button,
                   gpointer         user_data)
{
	GtkTreeView *account_types_tree_view;
	GtkTreeModel *model, *selection_model;
	GtkTreeIter iter, selection_iter;
	GncExampleAccount *gea;

	account_types_tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("account_types_tree_view"));
	selection_model = gtk_tree_view_get_model (account_types_tree_view);
	model = gnc_tree_model_selection_get_model (GNC_TREE_MODEL_SELECTION (selection_model));

	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (model), &iter)) {
		do {
			gea = gnc_tree_model_example_account_get_account (GNC_TREE_MODEL_EXAMPLE_ACCOUNT (model),
									  &iter);
			if (gea != NULL) {
				gnc_tree_model_selection_convert_child_iter_to_iter (GNC_TREE_MODEL_SELECTION (selection_model),
										     &selection_iter, &iter);
				gnc_tree_model_selection_set_selected (GNC_TREE_MODEL_SELECTION (selection_model),
								       &selection_iter,
								       FALSE);
			}
		} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (model), &iter));
	}
}

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
hierarchy_merge_groups (GSList *dalist)
{
  GSList *mark;
  gnc_commodity *com;
  AccountGroup *ret = xaccMallocAccountGroup (gnc_get_current_book ());

  com = gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT(get_currency_editor ()));

  for (mark = dalist; mark; mark = mark->next)
  {
    GncExampleAccount *xea = mark->data;

    add_groups_to_with_random_guids (ret, xea->group, com);
  }

  return ret;
}

static GSList *
get_selected_account_list (void)
{
	GSList *actlist = NULL;
	GtkTreeView *tree_view;
	GtkTreeModel *model, *selection_model;
	GtkTreeIter iter, selection_iter;
	GncExampleAccount *gea;

	tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("account_types_tree_view"));
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
			GtkTreeModel *tree_model,
			GtkTreeIter *iter,
			gpointer data)
{
	GncTreeModelAccount *model;
	Account *account;
	gnc_numeric balance;
	const gchar *string;
	GNCPrintAmountInfo print_info;

	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model));

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	account = gnc_tree_model_account_get_account (model, iter);

	balance = get_final_balance (account);
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
		      NULL);
}

static void
on_final_account_tree_placeholder_toggled (GtkCellRendererToggle *cell,
					   gchar *path_str,
					   gpointer data)
{
	GtkTreeView *tree_view;
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeIter  iter;
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);
	Account *account;
	gboolean selected;
	GNCAmountEdit *balance_edit;

	tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("final_account_tree_view"));
	selection = gtk_tree_view_get_selection (tree_view);
	model = gtk_tree_view_get_model (tree_view);

	gtk_tree_model_get_iter (model, &iter, path);
	gtk_tree_path_free (path);

	account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT (model), &iter);
	selected = !xaccAccountGetPlaceholder (account);
	xaccAccountSetPlaceholder (account, selected);

	if (gtk_tree_selection_iter_is_selected (selection, &iter)) {
		balance_edit = get_balance_editor ();
		if (balance_edit) {
			gtk_widget_set_sensitive(GTK_WIDGET(balance_edit), !selected);
		}
	}
}

static void
on_final_account_tree_selection_changed (GtkTreeSelection *selection,
					 gpointer data)
{
	GtkTreeModel *model;
	GtkTreeIter  iter;
	gboolean selected;
	Account *account;
	GNCAmountEdit *balance_edit;
	GNCPrintAmountInfo print_info;
	gnc_numeric balance;
	GtkWidget *entry;

	selected = gtk_tree_selection_get_selected (selection, &model, &iter);

	balance_edit = get_balance_editor ();

	entry = gnc_amount_edit_gtk_entry (balance_edit);
	gtk_entry_set_text (GTK_ENTRY (entry), "");

	gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);

	if (!selected)
		return;

	account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT (model), &iter);

	if (xaccAccountGetType (account) == EQUITY)
		return;

	gtk_widget_set_sensitive (GTK_WIDGET (balance_edit),
				  !xaccAccountGetPlaceholder (account));

	balance = get_final_balance (account);

	if (gnc_reverse_balance (account))
		balance = gnc_numeric_neg (balance);

	print_info = gnc_account_print_info (account, FALSE);
	gnc_amount_edit_set_print_info (balance_edit, print_info);
	gnc_amount_edit_set_fraction (balance_edit,
				      xaccAccountGetCommoditySCU (account));

	block_amount_changed ();

	gnc_amount_edit_set_amount (balance_edit, balance);
	
	if (gnc_numeric_zero_p (balance))
		gtk_entry_set_text (GTK_ENTRY (entry), "");

	unblock_amount_changed ();
}

static void
on_final_account_prepare (GnomeDruidPage  *gnomedruidpage,
                          gpointer         arg1,
                          gpointer         user_data)
{
  GSList *actlist;
  GtkTreeView *tree_view;
  GtkTreeModel *model;
  GtkTreeSelection *selection;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  tree_view = GTK_TREE_VIEW (hierarchy_get_widget ("final_account_tree_view"));

  if (g_object_get_data (G_OBJECT (gnomedruidpage), "initialized") == NULL) {
    model = gnc_tree_model_account_new (our_final_group);
    gtk_tree_view_set_model (tree_view, model);
    g_object_unref (G_OBJECT (model));
    selection = gtk_tree_view_get_selection (tree_view);
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
    g_signal_connect (G_OBJECT (selection), "changed",
		      G_CALLBACK (on_final_account_tree_selection_changed),
		      NULL);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Account Name"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_ACCOUNT_COL_NAME,
						       NULL);
    gtk_tree_view_append_column (tree_view, column);

    column = gtk_tree_view_column_new_with_attributes (_("Type"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_ACCOUNT_COL_TYPE,
						       NULL);
    gtk_tree_view_append_column (tree_view, column);

    renderer = gtk_cell_renderer_toggle_new ();
    g_signal_connect (G_OBJECT (renderer), "toggled",
		      G_CALLBACK (on_final_account_tree_placeholder_toggled),
		      NULL);
    column = gtk_tree_view_column_new_with_attributes (_("Placeholder"),
		    				       renderer,
						       "active", GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,
						       NULL);
    gtk_tree_view_append_column (tree_view, column);

    renderer = gtk_cell_renderer_text_new ();
    g_object_set (G_OBJECT (renderer),
		  "xalign", 1.0,
		  NULL);
    column = gtk_tree_view_column_new_with_attributes (_("Opening Balance"),
		    				       renderer,
						       NULL);

    gtk_tree_view_column_set_cell_data_func (column, renderer, 
					     balance_cell_data_func,
					     NULL, NULL);
    gtk_tree_view_append_column (tree_view, column);

    g_object_set_data (G_OBJECT (gnomedruidpage), "initialized", GINT_TO_POINTER(1));
  }

  model = gtk_tree_view_get_model (tree_view);

  gnc_suspend_gui_refresh ();
  gnc_tree_model_account_set_root (GNC_TREE_MODEL_ACCOUNT (model), NULL);
  delete_our_final_group ();
  actlist = get_selected_account_list ();
  our_final_group = hierarchy_merge_groups (actlist);
  gnc_tree_model_account_set_root (GNC_TREE_MODEL_ACCOUNT (model), our_final_group);
  gtk_tree_view_expand_all (tree_view);
  gnc_resume_gui_refresh ();
  
  {
    GNCAmountEdit *balance_edit;
    GtkWidget *entry;

    block_amount_changed ();

    balance_edit = get_balance_editor ();
    gnc_amount_edit_set_amount (balance_edit, gnc_numeric_zero ());

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");

    unblock_amount_changed ();

    gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);
  }
}


/*
static void
on_final_account_tree_select_row (GtkCTree        *ctree,
                                  GList           *node,
                                  gint             column,
                                  gpointer         user_data)
{
  Account *account;
  GtkToggleButton *placeholder_button;
  GNCAmountEdit *balance_edit;
  GNCPrintAmountInfo print_info;
  gnc_numeric balance;
  gboolean is_placeholder;

  balance_edit = get_balance_editor ();

  account = gtk_ctree_node_get_row_data (ctree, GTK_CTREE_NODE (node));
  if (!account || xaccAccountGetType (account) == EQUITY)
  {
    GtkWidget *entry;

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");

    gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);

    return;
  }

  is_placeholder = xaccAccountGetPlaceholder (account);
  placeholder_button = get_placeholder_checkbox ();
  gtk_toggle_button_set_active(placeholder_button, is_placeholder);

  gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), !is_placeholder);

  balance = get_final_balance (account);

  if (gnc_reverse_balance (account))
    balance = gnc_numeric_neg (balance);

  print_info = gnc_account_print_info (account, FALSE);
  gnc_amount_edit_set_print_info (balance_edit, print_info);
  gnc_amount_edit_set_fraction (balance_edit,
                                xaccAccountGetCommoditySCU (account));

  block_amount_changed ();

  gnc_amount_edit_set_amount (balance_edit, balance);
  if (gnc_numeric_zero_p (balance))
  {
    GtkWidget *entry;

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");
  }

  unblock_amount_changed ();
}

static void
on_final_account_tree_unselect_row (GtkCTree        *ctree,
                                    GList           *node,
                                    gint             column,
                                    gpointer         user_data)
{
  update_account_balance (ctree, GTK_CTREE_NODE (node));

  {
    GNCAmountEdit *balance_edit;
    GtkWidget *entry;

    balance_edit = get_balance_editor ();

    entry = gnc_amount_edit_gtk_entry (balance_edit);
    gtk_entry_set_text (GTK_ENTRY (entry), "");

    gtk_widget_set_sensitive (GTK_WIDGET (balance_edit), FALSE);
  }
}
*/

static gboolean
on_final_account_next (GnomeDruidPage  *gnomedruidpage,
                       gpointer         arg1,
                       gpointer         user_data)
{
  GNCAmountEdit *balance_edit;

  balance_edit = get_balance_editor ();

  if (!gnc_amount_edit_evaluate (balance_edit))
  {
    GtkWidget *top;
    const char *message = _("You must enter a valid balance.");

    top = gtk_widget_get_toplevel (GTK_WIDGET (gnomedruidpage));
    gnc_error_dialog(top, message);

    return TRUE;
  }

  return FALSE;
}

static void
cancel_everything_out(void)
{
  delete_our_final_group ();
  delete_hierarchy_window ();
  gncp_new_user_finish ();
}

static void
on_cancel (GnomeDruid      *gnomedruid,
                 gpointer         user_data)
{
  cancel_everything_out ();
}

static gpointer
starting_balance_helper (Account *account, gpointer data)
{
  gnc_numeric balance;

  balance = get_final_balance (account);
  if (!gnc_numeric_zero_p (balance))
    gnc_account_create_opening_balance (account, balance, time (NULL),
                                        gnc_get_current_book ());

  return NULL;
}

static void
on_finish (GnomeDruidPage  *gnomedruidpage,
           gpointer         arg1,
           gpointer         user_data)
{
  gnc_suspend_gui_refresh ();

  if (our_final_group)
    xaccGroupForEachAccount (our_final_group, starting_balance_helper,
                             NULL, TRUE);

  delete_hierarchy_window ();

  gncp_new_user_finish ();

  gnc_set_first_startup (FALSE);

  if (our_final_group)
    xaccGroupConcatGroup (gnc_get_current_group (), our_final_group);

  gnc_resume_gui_refresh ();
}

static GtkWidget *
gnc_create_hierarchy_druid (void)
{
  GtkWidget *balance_edit;
  GtkWidget *dialog;
  GtkWidget *druid;
  GtkWidget *tree_view;
  GtkWidget *box;
  GHashTable *hash;
  GladeXML *xml;

  xml = gnc_glade_xml_new ("account.glade", "Hierarchy Druid");

  glade_xml_signal_connect
    (xml, "on_choose_currency_prepare",
     G_CALLBACK (on_choose_currency_prepare));

  glade_xml_signal_connect
    (xml, "on_choose_account_types_prepare",
     G_CALLBACK (on_choose_account_types_prepare));

  glade_xml_signal_connect
    (xml, "on_final_account_prepare",
     G_CALLBACK (on_final_account_prepare));

  glade_xml_signal_connect
    (xml, "on_final_account_next",
     G_CALLBACK (on_final_account_next));

  glade_xml_signal_connect
    (xml, "select_all_clicked", G_CALLBACK (select_all_clicked));

  glade_xml_signal_connect
    (xml, "clear_all_clicked", G_CALLBACK (clear_all_clicked));

  glade_xml_signal_connect (xml, "on_finish", G_CALLBACK (on_finish));

  glade_xml_signal_connect (xml, "on_cancel", G_CALLBACK (on_cancel));

  dialog = glade_xml_get_widget (xml, "Hierarchy Druid");
  gnome_window_icon_set_from_default (GTK_WINDOW (dialog));

  druid = glade_xml_get_widget (xml, "hierarchy_druid");
  /* gnc_druid_set_colors (GNOME_DRUID (druid)); */

  gtk_widget_show (glade_xml_get_widget (xml, "start_page"));
  gtk_widget_show (glade_xml_get_widget (xml, "newUserDruidFinishPage"));

  balance_edit = gnc_amount_edit_new ();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (balance_edit), TRUE);
  gtk_widget_show (balance_edit);

  g_signal_connect (G_OBJECT (balance_edit), "amount_changed",
                    G_CALLBACK (on_balance_changed), NULL);

  tree_view = glade_xml_get_widget (xml, "account_types_tree_view");
  g_signal_connect (G_OBJECT (gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view))), "changed",
		    G_CALLBACK (account_types_selection_changed), NULL);
  gtk_tree_selection_set_mode (gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view)), GTK_SELECTION_SINGLE);

  box = glade_xml_get_widget (xml, "start_balance_box");
  gtk_box_pack_start (GTK_BOX (box), balance_edit, TRUE, TRUE, 0);

  g_object_set_data (G_OBJECT (dialog), "balance_editor", balance_edit);

  hash = g_hash_table_new (g_str_hash, g_str_equal);

  g_object_set_data (G_OBJECT (dialog), "balance_hash", hash);

  g_signal_connect (G_OBJECT(dialog), "destroy",
                    G_CALLBACK (gnc_hierarchy_destroy_cb), NULL);

  return dialog;
}

void
gnc_ui_hierarchy_druid (void)
{
  if (hierarchy_window) return;

  hierarchy_window = gnc_create_hierarchy_druid ();

  return;
}
