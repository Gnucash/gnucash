/********************************************************************\
 * dialog-tax-info.c -- tax information dialog                      *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libguile.h>
#include "guile-mappings.h"

#include "Account.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"
#include "gnc-gconf-utils.h"
#include "gnc-tree-view-account.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "qof.h"
#include "gnc-ui.h"


#define DIALOG_TAX_INFO_CM_CLASS "dialog-tax-info"
#define GCONF_SECTION "dialogs/tax_info"
#define PANED_POSITION "paned_position"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

static struct
{
  SCM payer_name_source;
  SCM form;
  SCM description;
  SCM help;

  SCM codes;
} getters;

typedef struct
{
  char *code;
  char *payer_name_source;
  char *form;
  char *description;
  char *help;
} TXFInfo;

typedef struct
{
  GtkWidget * dialog;

  GtkWidget * account_treeview;
  GtkWidget * select_button;

  GtkWidget * tax_related_button;
  GtkWidget * txf_category_view;
  GtkWidget * txf_help_text;
  GtkWidget * current_account_button;
  GtkWidget * parent_account_button;

  GList * income_txf_infos;
  GList * expense_txf_infos;

  gboolean income;
  gboolean changed;

  GNCAccountType account_type;
} TaxInfoDialog;


static gboolean getters_initialized = FALSE;


static void
initialize_getters (void)
{
  if (getters_initialized)
    return;

  getters.payer_name_source = scm_c_eval_string ("gnc:txf-get-payer-name-source");
  getters.form              = scm_c_eval_string ("gnc:txf-get-form");
  getters.description       = scm_c_eval_string ("gnc:txf-get-description");
  getters.help              = scm_c_eval_string ("gnc:txf-get-help");

  getters.codes             = scm_c_eval_string ("gnc:txf-get-codes");

  getters_initialized = TRUE;
}

static void
destroy_txf_info (gpointer data, gpointer user_data)
{
  TXFInfo *txf_info = data;

  g_free (txf_info->code);
  txf_info->code = NULL;

  g_free (txf_info->payer_name_source);
  txf_info->payer_name_source = NULL;

  g_free (txf_info->form);
  txf_info->form = NULL;

  g_free (txf_info->description);
  txf_info->description = NULL;

  g_free (txf_info->help);
  txf_info->help = NULL;

  g_free (txf_info);
}

static void
destroy_txf_infos (GList *infos)
{
  g_list_foreach (infos, destroy_txf_info, NULL);
  g_list_free (infos);
}

static void
gnc_tax_info_set_changed (TaxInfoDialog *ti_dialog, gboolean changed)
{
  ti_dialog->changed = changed;
}

static GList *
load_txf_info (gboolean income)
{
  GList *infos = NULL;
  SCM category;
  SCM codes;

  initialize_getters ();

  category = scm_c_eval_string (income ?
				"txf-income-categories" :
				"txf-expense-categories");
  if (category == SCM_UNDEFINED)
  {
    destroy_txf_infos (infos);
    return NULL;
  }

  codes = scm_call_1 (getters.codes, category);
  if (!SCM_LISTP (codes))
  {
    destroy_txf_infos (infos);
    return NULL;
  }

  while (!SCM_NULLP (codes))
  {
    TXFInfo *txf_info;
    SCM code_scm;
    const gchar *str;
    SCM scm;

    code_scm  = SCM_CAR (codes);
    codes     = SCM_CDR (codes);

    scm = scm_call_2 (getters.payer_name_source, category, code_scm);
    str = SCM_SYMBOL_CHARS (scm);
    if (safe_strcmp (str, "not-impl") == 0)
    {
      continue;
    }

    txf_info = g_new0 (TXFInfo, 1);

    if (safe_strcmp (str, "none") == 0)
      txf_info->payer_name_source = NULL;
    else
      txf_info->payer_name_source = g_strdup (str);

    str = SCM_SYMBOLP(code_scm) ? SCM_SYMBOL_CHARS(code_scm) : "";
    txf_info->code = g_strdup (str);

    scm = scm_call_2 (getters.form, category, code_scm);
    str = SCM_STRINGP(scm) ? SCM_STRING_CHARS(scm) : "";
    txf_info->form = g_strdup (str);

    scm = scm_call_2 (getters.description, category, code_scm);
    str = SCM_STRINGP(scm) ? SCM_STRING_CHARS(scm) : "";
    txf_info->description = g_strdup (str);

    scm = scm_call_2 (getters.help, category, code_scm);
    str = SCM_STRINGP(scm) ? SCM_STRING_CHARS(scm) : "";
    txf_info->help = g_strdup (str);

    infos = g_list_prepend (infos, txf_info);
  }

  return g_list_reverse (infos);
}

static GList *
tax_infos (TaxInfoDialog *ti_dialog)
{
  return
    ti_dialog->income ?
    ti_dialog->income_txf_infos : ti_dialog->expense_txf_infos;
}

static void
load_category_list (TaxInfoDialog *ti_dialog)
{
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeIter iter;
  GList *codes;

  view = GTK_TREE_VIEW(ti_dialog->txf_category_view);
  store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
  g_object_ref(store);
  gtk_tree_view_set_model(view, NULL);

  gtk_list_store_clear(store);

  codes = tax_infos (ti_dialog);
  for ( ; codes; codes = codes->next)
  {
    TXFInfo *txf_info = codes->data;

    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter,
		       0, txf_info->form,
		       1, txf_info->description,
		       -1);
  }

  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);
}

static void
clear_gui (TaxInfoDialog *ti_dialog)
{
  GtkTreeView *view;
  GtkTreeSelection *selection;

  gtk_toggle_button_set_active
    (GTK_TOGGLE_BUTTON (ti_dialog->tax_related_button), FALSE);

  view = GTK_TREE_VIEW(ti_dialog->txf_category_view);
  selection = gtk_tree_view_get_selection(view);
  gtk_tree_selection_unselect_all(selection);

  gtk_toggle_button_set_active
    (GTK_TOGGLE_BUTTON (ti_dialog->current_account_button), TRUE);
}

static gboolean
gnc_tax_info_dialog_account_filter_func (Account *account,
					 gpointer data)
{
  TaxInfoDialog *dialog = data;

  return xaccAccountGetType (account) == dialog->account_type;
}

static TXFInfo *
txf_infos_find_code (GList *infos, const char *code)
{
  for (; infos; infos = infos->next)
  {
    TXFInfo *info = infos->data;

    if (safe_strcmp (code, info->code) == 0)
      return info;
  }

  return NULL;
}

static void
account_to_gui (TaxInfoDialog *ti_dialog, Account *account)
{
  GtkTreeView *view;
  GtkTreeSelection *selection;
  GtkTreePath *path;
  gboolean tax_related;
  const char *str;
  TXFInfo *info;
  GList *infos;
  gint index;

  if (!account)
  {
    clear_gui (ti_dialog);
    return;
  }

  tax_related = xaccAccountGetTaxRelated (account);
  gtk_toggle_button_set_active
    (GTK_TOGGLE_BUTTON (ti_dialog->tax_related_button), tax_related);

  infos = tax_infos (ti_dialog);

  str = xaccAccountGetTaxUSCode (account);
  info = txf_infos_find_code (infos, str);
  if (info)
    index = g_list_index (infos, info);
  else
    index = 0;
  if (index < 0)
    index = 0;

  view = GTK_TREE_VIEW(ti_dialog->txf_category_view);
  selection = gtk_tree_view_get_selection(view);
  path =  gtk_tree_path_new_from_indices(index, -1);
  gtk_tree_selection_select_path(selection, path);
  gtk_tree_view_scroll_to_cell(view, path, NULL, FALSE, 0, 0);
  gtk_tree_path_free(path);

  str = xaccAccountGetTaxUSPayerNameSource (account);
  if (safe_strcmp (str, "parent") == 0)
    gtk_toggle_button_set_active
      (GTK_TOGGLE_BUTTON (ti_dialog->parent_account_button), TRUE);
  else
    gtk_toggle_button_set_active
      (GTK_TOGGLE_BUTTON (ti_dialog->current_account_button), TRUE);
}

static void
gui_to_accounts (TaxInfoDialog *ti_dialog)
{
  GtkTreeView *view;
  GtkTreeModel *model;
  GtkTreeSelection *selection;
  GtkTreePath *path;
  GtkTreeIter iter;
  gint *indices;
  gboolean tax_related;
  const char *code;
  const char *pns;
  GList *accounts;
  TXFInfo *info;
  GList *infos;
  GList *node;

  tax_related = gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON (ti_dialog->tax_related_button));

  infos = tax_infos (ti_dialog);

  view = GTK_TREE_VIEW(ti_dialog->txf_category_view);
  selection = gtk_tree_view_get_selection(view);
  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return;
  path = gtk_tree_model_get_path(model, &iter);
  indices = gtk_tree_path_get_indices(path);
  info = g_list_nth_data (infos, indices[0]);
  gtk_tree_path_free(path);
  g_return_if_fail (info != NULL);

  code = tax_related ? info->code : NULL;

  if (tax_related && info->payer_name_source)
  {
    gboolean current;

    current = gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON (ti_dialog->current_account_button));

    pns = current ? "current" : "parent";
  }
  else
    pns = NULL;

  accounts = gnc_tree_view_account_get_selected_accounts
    (GNC_TREE_VIEW_ACCOUNT(ti_dialog->account_treeview));

  for (node = accounts; node; node = node->next)
  {
    Account *account = node->data;

    xaccAccountBeginEdit (account);

    xaccAccountSetTaxRelated (account, tax_related);
    xaccAccountSetTaxUSCode (account, code);
    xaccAccountSetTaxUSPayerNameSource (account, pns);

    xaccAccountCommitEdit (account);
  }
}

static void
window_destroy_cb (GtkObject *object, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;

  gnc_unregister_gui_component_by_data (DIALOG_TAX_INFO_CM_CLASS, ti_dialog);

  destroy_txf_infos (ti_dialog->income_txf_infos);
  ti_dialog->income_txf_infos = NULL;

  destroy_txf_infos (ti_dialog->expense_txf_infos);
  ti_dialog->expense_txf_infos = NULL;

  g_free (ti_dialog);
}

static void
cursor_changed_cb (GtkWidget *widget, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;
  GncTreeViewAccount *account_tree;
  Account *account;
  gint num_children;

  account_tree = GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview);
  account = gnc_tree_view_account_get_cursor_account (account_tree);
  if (!account) {
    gtk_widget_set_sensitive(ti_dialog->select_button, FALSE);
    return;
  }

  num_children = gnc_tree_view_account_count_children(account_tree, account);
  gtk_widget_set_sensitive(ti_dialog->select_button, num_children > 0);
}

static void
select_subaccounts_clicked (GtkWidget *widget, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;
  GncTreeViewAccount *account_tree;
  Account *account;

  account_tree = GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview);
  account = gnc_tree_view_account_get_cursor_account (account_tree);
  if (!account)
    return;

  gnc_tree_view_account_select_subaccounts (account_tree, account);

  gtk_widget_grab_focus (ti_dialog->account_treeview);
}

static void
gnc_tax_info_dialog_response (GtkDialog *dialog, gint response, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;

  if (response == GTK_RESPONSE_OK && ti_dialog->changed)
      gui_to_accounts (ti_dialog);

  gnc_close_gui_component_by_data (DIALOG_TAX_INFO_CM_CLASS, ti_dialog);
}

static void
tax_info_show_income_accounts (TaxInfoDialog *ti_dialog, gboolean show_income)
{
  GncTreeViewAccount *tree;
  AccountViewInfo info;
  GNCAccountType type;
  GNCAccountType show_type;

  ti_dialog->income = show_income;

  tree = GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview);
  show_type = show_income ? ACCT_TYPE_INCOME : ACCT_TYPE_EXPENSE;

  gnc_tree_view_account_get_view_info (tree, &info);

  for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
    info.include_type[type] = (type == show_type);

  gnc_tree_view_account_set_view_info (tree, &info);

  load_category_list (ti_dialog);
  cursor_changed_cb(GTK_WIDGET(tree), ti_dialog);
}

static int
gnc_tax_info_update_accounts (TaxInfoDialog *ti_dialog)
{
  GncTreeViewAccount *tree;
  GtkTreeSelection* selection;
  GtkWidget *label;
  GtkWidget *vbox;
  int num_accounts;
  char *string;

  tree = GNC_TREE_VIEW_ACCOUNT(ti_dialog->account_treeview);
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
  num_accounts = gtk_tree_selection_count_selected_rows (selection);

  label = gnc_glade_lookup_widget (ti_dialog->dialog, "num_accounts_label");
  vbox = gnc_glade_lookup_widget (ti_dialog->dialog, "tax_info_vbox");

  string = g_strdup_printf ("%d", num_accounts);
  gtk_label_set_text (GTK_LABEL (label), string);
  g_free (string);

  gtk_widget_set_sensitive (vbox, num_accounts > 0);

  return num_accounts;
}

static void
gnc_tax_info_income_cb (GtkWidget *w, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;
  gboolean show_income;

  show_income = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w));

  tax_info_show_income_accounts (ti_dialog, show_income);

  ti_dialog->account_type = show_income ? ACCT_TYPE_INCOME : ACCT_TYPE_EXPENSE;
  gnc_tree_view_account_refilter (GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview));

  gnc_tax_info_update_accounts (ti_dialog);

  clear_gui (ti_dialog);
}

static void
gnc_tax_info_account_changed_cb (GtkTreeSelection *selection,
				 gpointer data)
{
  TaxInfoDialog *ti_dialog = data;
  GncTreeViewAccount *view;
  GList *accounts;
  int num_accounts;

  g_return_if_fail(GTK_IS_TREE_SELECTION(selection));

  num_accounts = gnc_tax_info_update_accounts (ti_dialog);
  switch (num_accounts) {
   case 0:
    clear_gui (ti_dialog);
    gnc_tax_info_set_changed (ti_dialog, FALSE);
    return;

   case 1:
    /* Get the account. This view is set for multiple selection, so we
       can only get a list of accounts. */
    view = GNC_TREE_VIEW_ACCOUNT(ti_dialog->account_treeview);
    accounts = gnc_tree_view_account_get_selected_accounts (view);
    account_to_gui (ti_dialog, accounts->data);
    g_list_free(accounts);

    gnc_tax_info_set_changed (ti_dialog, FALSE);
    break;

   default:
    gnc_tax_info_set_changed (ti_dialog, TRUE);
    return;
  }
}

static void
txf_code_select_row_cb (GtkTreeSelection *selection,
			gpointer user_data)
{
  TaxInfoDialog *ti_dialog = user_data;
  GtkTreeModel *model;
  GtkTreePath *path;
  GtkTreeIter iter;
  gint *indices;
  TXFInfo *txf_info;
  GtkAdjustment *adj;
  GtkWidget *scroll;
  GtkWidget *vbox;
  GtkTextBuffer *tb;
  const char *text;

  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return;
  path = gtk_tree_model_get_path(model, &iter);
  indices = gtk_tree_path_get_indices(path);
  txf_info = g_list_nth_data (tax_infos (ti_dialog), indices[0]);
  gtk_tree_path_free(path);

  tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(ti_dialog->txf_help_text));

  text = (txf_info && txf_info->help) ? txf_info->help : "";
  gtk_text_buffer_set_text (tb, text, -1);

  scroll = gnc_glade_lookup_widget (GTK_WIDGET (ti_dialog->dialog),
				    "help_scroll");

  adj = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (scroll));
  gtk_adjustment_set_value (adj, 0.0);

  vbox = gnc_glade_lookup_widget (GTK_WIDGET (ti_dialog->dialog),
                                   "payer_name_source_vbox");

  if (txf_info && txf_info->payer_name_source)
  {
    gboolean current;

    gtk_widget_set_sensitive (vbox, TRUE);

    current = (strcmp ("current", txf_info->payer_name_source) == 0);

    if (current)
      gtk_toggle_button_set_active
        (GTK_TOGGLE_BUTTON (ti_dialog->current_account_button), TRUE);
    else
      gtk_toggle_button_set_active
        (GTK_TOGGLE_BUTTON (ti_dialog->parent_account_button), TRUE);
  }
  else
  {
    gtk_widget_set_sensitive (vbox, FALSE);
    gtk_toggle_button_set_active
      (GTK_TOGGLE_BUTTON (ti_dialog->current_account_button), TRUE);
  }

  gnc_tax_info_set_changed (ti_dialog, TRUE);
}

static void
tax_related_toggled_cb (GtkToggleButton *togglebutton,
                        gpointer user_data)
{
  TaxInfoDialog *ti_dialog = user_data;
  GtkWidget *vbox;
  gboolean on;

  on = gtk_toggle_button_get_active (togglebutton);

  vbox = gnc_glade_lookup_widget (GTK_WIDGET (togglebutton),
                                   "txf_categories_vbox");
  gtk_widget_set_sensitive (vbox, on);

  gnc_tax_info_set_changed (ti_dialog, TRUE);
}

static void
current_account_toggled_cb (GtkToggleButton *togglebutton,
                            gpointer user_data)
{
  TaxInfoDialog *ti_dialog = user_data;

  gnc_tax_info_set_changed (ti_dialog, TRUE);
}

static void
gnc_tax_info_dialog_create (GtkWidget * parent, TaxInfoDialog *ti_dialog)
{
  GtkWidget *dialog;
  GtkObject *tido;
  GladeXML  *xml;
  GtkTreeView *tree_view;
  GtkTreeSelection *selection;
  GtkWidget *label;

  xml = gnc_glade_xml_new ("tax.glade", "Tax Information Dialog");

  dialog = glade_xml_get_widget (xml, "Tax Information Dialog");
  ti_dialog->dialog = dialog;
  tido = GTK_OBJECT (dialog);

  ti_dialog->account_type = ACCT_TYPE_EXPENSE;
  ti_dialog->income_txf_infos = load_txf_info (TRUE);
  ti_dialog->expense_txf_infos = load_txf_info (FALSE);

  g_signal_connect (G_OBJECT (dialog), "response",
                    G_CALLBACK (gnc_tax_info_dialog_response), ti_dialog);

  g_signal_connect (G_OBJECT (dialog), "destroy",
                    G_CALLBACK (window_destroy_cb), ti_dialog);

  /* parent */
  if (parent != NULL)
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

  /* default to ok */
  gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_OK);

  /* tax information */
  {
    GtkListStore *store;
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer; 
    GtkWidget *button;
    GtkWidget *text;

    button = glade_xml_get_widget (xml, "tax_related_button");
    ti_dialog->tax_related_button = button;

    g_signal_connect (G_OBJECT (button), "toggled",
                      G_CALLBACK  (tax_related_toggled_cb), ti_dialog);

    text = glade_xml_get_widget (xml, "txf_help_text");
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
    ti_dialog->txf_help_text = text;

    tree_view = GTK_TREE_VIEW(glade_xml_get_widget(xml, "txf_category_view"));
    store =  gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
    gtk_tree_view_set_model(tree_view, GTK_TREE_MODEL(store));
    g_object_unref(store);
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes
      (_("Form"), renderer, "text", 0, NULL);
    gtk_tree_view_append_column(tree_view, GTK_TREE_VIEW_COLUMN(column));
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes
      (_("Description"), renderer, "text", 1, NULL);
    gtk_tree_view_append_column(tree_view, GTK_TREE_VIEW_COLUMN(column));
    ti_dialog->txf_category_view = GTK_WIDGET(tree_view);

    selection = gtk_tree_view_get_selection(tree_view);
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK  (txf_code_select_row_cb), ti_dialog);

    label = glade_xml_get_widget(xml, "txf_category_label");
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), GTK_WIDGET(tree_view));

    button = glade_xml_get_widget (xml, "current_account_button");
    ti_dialog->current_account_button = button;

    button = glade_xml_get_widget (xml, "parent_account_button");
    ti_dialog->parent_account_button = button;

    g_signal_connect (G_OBJECT (button), "toggled",
                      G_CALLBACK  (current_account_toggled_cb),
                        ti_dialog);
  }

  /* account tree */
  {
    GtkWidget *income_radio, *expense_radio, *box;

    box = glade_xml_get_widget (xml, "account_scroll");
    tree_view = gnc_tree_view_account_new (FALSE);
    gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT(tree_view), 
				      gnc_tax_info_dialog_account_filter_func,
				      ti_dialog, NULL);
    ti_dialog->account_treeview = GTK_WIDGET(tree_view);

    selection = gtk_tree_view_get_selection (tree_view);
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_EXTENDED);
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_tax_info_account_changed_cb),
                      ti_dialog);

    gtk_widget_show (ti_dialog->account_treeview);
    gtk_container_add (GTK_CONTAINER (box), ti_dialog->account_treeview);

    label = glade_xml_get_widget(xml, "accounts_label");
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), GTK_WIDGET(tree_view));

    income_radio = glade_xml_get_widget (xml, "income_radio");
    expense_radio = glade_xml_get_widget (xml, "expense_radio");
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(expense_radio), TRUE);
    g_signal_connect (G_OBJECT (income_radio), "toggled",
                      G_CALLBACK  (gnc_tax_info_income_cb),
                        ti_dialog);
    //   gtk_button_clicked (GtkButton *button);
 }

  /* select subaccounts button */
  {
    GtkWidget *button;

    button = glade_xml_get_widget (xml, "select_subaccounts_button");
    ti_dialog->select_button = button;

    g_signal_connect (G_OBJECT (button), "clicked",
                      G_CALLBACK  (select_subaccounts_clicked),
		      ti_dialog);
    g_signal_connect (G_OBJECT (ti_dialog->account_treeview), "cursor_changed",
                      G_CALLBACK  (cursor_changed_cb),
		      ti_dialog);
  }

  tax_info_show_income_accounts (ti_dialog, FALSE);
  gnc_tax_info_update_accounts (ti_dialog);
  clear_gui (ti_dialog);
  gnc_tax_info_set_changed (ti_dialog, FALSE);

  gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(ti_dialog->dialog));

  if (gnc_gconf_get_bool(GCONF_GENERAL, KEY_SAVE_GEOMETRY, NULL)) {
    GtkWidget *paned = glade_xml_get_widget(xml, "paned");
    gint position = gnc_gconf_get_int(GCONF_SECTION, PANED_POSITION, NULL);
    gtk_paned_set_position(GTK_PANED(paned), position);
  }
}

static void
close_handler (gpointer user_data)
{
  TaxInfoDialog *ti_dialog = user_data;

  if (gnc_gconf_get_bool(GCONF_GENERAL, KEY_SAVE_GEOMETRY, NULL)) {
    GtkWidget *paned = gnc_glade_lookup_widget(ti_dialog->dialog, "paned");
    gnc_gconf_set_int(GCONF_SECTION, PANED_POSITION,
                      gtk_paned_get_position(GTK_PANED(paned)), NULL);
  }

  gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(ti_dialog->dialog));
  gtk_widget_destroy (ti_dialog->dialog);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  TaxInfoDialog *ti_dialog = user_data;

/*  gnc_account_tree_refresh (GNC_ACCOUNT_TREE (ti_dialog->account_tree));*/

  gnc_tax_info_update_accounts (ti_dialog);
}

/********************************************************************\
 * gnc_tax_info_dialog                                              *
 *   opens up a window to set account tax information               *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_tax_info_dialog (GtkWidget * parent)
{
  TaxInfoDialog *ti_dialog;
  gint component_id;

  ti_dialog = g_new0 (TaxInfoDialog, 1);

  gnc_tax_info_dialog_create (parent, ti_dialog);

  component_id = gnc_register_gui_component (DIALOG_TAX_INFO_CM_CLASS,
                                             refresh_handler, close_handler,
                                             ti_dialog);
  gnc_gui_component_set_session (component_id, gnc_get_current_session ());

  gnc_gui_component_watch_entity_type (component_id,
                                       GNC_ID_ACCOUNT,
                                       QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

  gtk_widget_grab_focus (ti_dialog->account_treeview);

  gtk_widget_show (ti_dialog->dialog);
}
