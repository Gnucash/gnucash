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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <guile/gh.h>

#include "account-tree.h"
#include "glade-gnc-dialogs.h"
#include "glade-support.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "messages.h"


#define DIALOG_TAX_INFO_CM_CLASS "dialog-tax-info"

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
  char *payer_name_source;
  char *form;
  char *description;
  char *help;
} TXFInfo;

typedef struct
{
  GtkWidget * dialog;

  GtkWidget * account_tree;

  GtkWidget * tax_related_button;
  GtkWidget * txf_category_clist;
  GtkWidget * txf_help_text;
  GtkWidget * current_account_button;

  GHashTable * income_txf_info;
  GHashTable * expense_txf_info;
} TaxInfoDialog;


static gboolean getters_initialized = FALSE;


static void
initialize_getters (void)
{
  if (getters_initialized)
    return;

  getters.payer_name_source = gh_eval_str ("gnc:txf-get-payer-name-source");
  getters.form              = gh_eval_str ("gnc:txf-get-form");
  getters.description       = gh_eval_str ("gnc:txf-get-description");
  getters.help              = gh_eval_str ("gnc:txf-get-help");

  getters.codes             = gh_eval_str ("gnc:txf-get-codes");

  getters_initialized = TRUE;
}

static void
destroy_txf_info_helper (gpointer key, gpointer value, gpointer data)
{
  char *code = key;
  TXFInfo *txf_info = value;

  g_free (key);

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
destroy_txf_info (GHashTable *info)
{
  g_hash_table_foreach (info, destroy_txf_info_helper, NULL);
  g_hash_table_destroy (info);
}

static GHashTable *
load_txf_info (gboolean income)
{
  GHashTable *info;
  SCM category;
  SCM codes;

  initialize_getters ();

  info = g_hash_table_new (g_str_hash, g_str_equal);

  category = gh_eval_str (income ?
                          "txf-income-categories" :
                          "txf-expense-categories");
  if (category == SCM_UNDEFINED)
  {
    destroy_txf_info (info);
    return NULL;
  }

  codes = gh_call1 (getters.codes, category);
  if (!gh_list_p (codes))
  {
    destroy_txf_info (info);
    return NULL;
  }

  while (!gh_null_p (codes))
  {
    TXFInfo *txf_info;
    SCM code_scm;
    char *code;
    char *str;
    SCM scm;

    code_scm  = gh_car (codes);
    codes     = gh_cdr (codes);

    if (!gh_symbol_p (code_scm))
      continue;

    str = gh_symbol2newstr (code_scm, NULL);
    if (!str)
      continue;

    code = g_strdup (str);
    free (str);

    txf_info = g_new0 (TXFInfo, 1);

    scm = gh_call2 (getters.payer_name_source, category, code_scm);
    str = gh_symbol2newstr (scm, NULL);
    txf_info->payer_name_source = g_strdup (str);
    free (str);

    scm = gh_call2 (getters.form, category, code_scm);
    str = gh_scm2newstr (scm, NULL);
    txf_info->form = g_strdup (str);
    free (str);

    scm = gh_call2 (getters.description, category, code_scm);
    str = gh_scm2newstr (scm, NULL);
    txf_info->description = g_strdup (str);
    free (str);

    scm = gh_call2 (getters.help, category, code_scm);
    str = gh_scm2newstr (scm, NULL);
    txf_info->help = g_strdup (str);
    free (str);

    g_hash_table_insert (info, code, txf_info);
  }

  return info;
}

static void
window_destroy_cb (GtkObject *object, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;

  gnc_unregister_gui_component_by_data (DIALOG_TAX_INFO_CM_CLASS, ti_dialog);

  destroy_txf_info (ti_dialog->income_txf_info);
  ti_dialog->income_txf_info = NULL;

  destroy_txf_info (ti_dialog->expense_txf_info);
  ti_dialog->expense_txf_info = NULL;

  g_free (ti_dialog);
}

static void
select_subaccounts_clicked (GtkWidget *widget, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;
  GNCAccountTree *tree;
  Account *account;

  tree = GNC_ACCOUNT_TREE (ti_dialog->account_tree);

  account = gnc_account_tree_get_focus_account (tree);
  if (!account)
    return;

  gnc_account_tree_select_subaccounts (tree, account, FALSE);

  gtk_widget_grab_focus (ti_dialog->account_tree);
}

static void
unselect_subaccounts_clicked (GtkWidget *widget, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;
  GNCAccountTree *tree;
  Account *account;

  tree = GNC_ACCOUNT_TREE (ti_dialog->account_tree);

  account = gnc_account_tree_get_focus_account (tree);
  if (!account)
    return;

  gnc_account_tree_unselect_subaccounts (tree, account, FALSE);

  gtk_widget_grab_focus (ti_dialog->account_tree);
}

static void
tax_info_ok_clicked (GtkWidget *widget, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;

  gnc_close_gui_component_by_data (DIALOG_TAX_INFO_CM_CLASS, ti_dialog);
}

static void
tax_info_apply_clicked (GtkWidget *widget, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;

  return;
}

static void
tax_info_cancel_clicked (GtkWidget *widget, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;

  gnc_close_gui_component_by_data (DIALOG_TAX_INFO_CM_CLASS, ti_dialog);
}

static void
tax_info_show_income_accounts (TaxInfoDialog *ti_dialog, gboolean show_income)
{
  GNCAccountTree *tree;
  AccountViewInfo info;
  GNCAccountType type;
  GNCAccountType show_type;

  tree = GNC_ACCOUNT_TREE (ti_dialog->account_tree);
  show_type = show_income ? INCOME : EXPENSE;

  gnc_account_tree_get_view_info (tree, &info);

  for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
    info.include_type[type] = (type == show_type);

  gnc_account_tree_set_view_info (tree, &info);
}

static void
gnc_tax_info_income_cb (GtkWidget *w, gpointer data)
{
  TaxInfoDialog *ti_dialog = data;
  gboolean show_income;

  show_income = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w));

  tax_info_show_income_accounts (ti_dialog, show_income);

  gnc_account_tree_refresh (GNC_ACCOUNT_TREE (ti_dialog->account_tree));
  gnc_account_tree_expand_all (GNC_ACCOUNT_TREE (ti_dialog->account_tree));
}

static void
gnc_tax_info_dialog_create (GtkWidget * parent, TaxInfoDialog *ti_dialog)
{
  GtkWidget *dialog;
  GtkObject *tido;

  dialog = create_Tax_Information_Dialog ();
  ti_dialog->dialog = dialog;
  tido = GTK_OBJECT (dialog);

  ti_dialog->income_txf_info = load_txf_info (TRUE);
  ti_dialog->expense_txf_info = load_txf_info (FALSE);

  gnome_dialog_button_connect (GNOME_DIALOG (dialog), 0,
                               GTK_SIGNAL_FUNC (tax_info_ok_clicked),
                               ti_dialog);

  gnome_dialog_button_connect (GNOME_DIALOG (dialog), 1,
                               GTK_SIGNAL_FUNC (tax_info_apply_clicked),
                               ti_dialog);

  gnome_dialog_button_connect (GNOME_DIALOG (dialog), 2,
                               GTK_SIGNAL_FUNC (tax_info_cancel_clicked),
                               ti_dialog);

  gtk_signal_connect (tido, "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), ti_dialog);

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  /* default to ok */
  gnome_dialog_set_default (GNOME_DIALOG(dialog), 0);

  /* account tree */
  {
    GtkWidget *income_radio;
    GNCAccountTree *tree;
    GtkWidget *scroll;

    ti_dialog->account_tree = gnc_account_tree_new ();
    tree = GNC_ACCOUNT_TREE (ti_dialog->account_tree);

    gtk_clist_column_titles_hide (GTK_CLIST (ti_dialog->account_tree));
    gtk_clist_set_selection_mode (GTK_CLIST (ti_dialog->account_tree),
                                  GTK_SELECTION_MULTIPLE);
    gnc_account_tree_hide_all_but_name (tree);

    tax_info_show_income_accounts (ti_dialog, FALSE);

    gnc_account_tree_refresh (tree);
    gnc_account_tree_expand_all (tree);

    gtk_widget_show (ti_dialog->account_tree);

    scroll = gtk_object_get_data (tido, "account_scroll");
    gtk_container_add (GTK_CONTAINER (scroll), ti_dialog->account_tree);

    income_radio = lookup_widget (dialog, "income_radio");
    gtk_signal_connect (GTK_OBJECT (income_radio), "toggled",
                        GTK_SIGNAL_FUNC (gnc_tax_info_income_cb),
                        ti_dialog);
  }

  /* account buttons */
  {
    GtkWidget *button;

    button = lookup_widget (dialog, "select_subaccounts_button");
    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (select_subaccounts_clicked),
                        ti_dialog);

    button = lookup_widget (dialog, "unselect_subaccounts_button");
    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (unselect_subaccounts_clicked),
                        ti_dialog);
  }
}

static void
close_handler (gpointer user_data)
{
  TaxInfoDialog *ti_dialog = user_data;

  gnome_dialog_close (GNOME_DIALOG (ti_dialog->dialog));
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

  ti_dialog = g_new0 (TaxInfoDialog, 1);

  gnc_tax_info_dialog_create (parent, ti_dialog);

  gnc_register_gui_component (DIALOG_TAX_INFO_CM_CLASS,
                              NULL, close_handler, ti_dialog);

  gtk_widget_grab_focus (ti_dialog->account_tree);

  gtk_widget_show (ti_dialog->dialog);
}
