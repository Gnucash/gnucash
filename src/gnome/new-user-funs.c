/********************************************************************\
 * new-user-funs.c -- new user functionality for GnuCash            *
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
#include <guile/gh.h>
#include <stdio.h>

#include "druid-qif-import.h"
#include "druid-utils.h"
#include "new-user-callbacks.h"
#include "new-user-interface.h"
#include "new-user-funs.h"
#include "glade-support.h"
#include "gnc-amount-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "window-help.h"

#include "Group.h"
#include "io-example-account.h"
#include "Account.h"


static GtkWidget *newUserDialog = NULL;
static GtkWidget *cancelDialog = NULL;

static Account*
clone_account(const Account* from, gnc_commodity *com)
{
    Account *ret;

    ret = xaccCloneAccountSimple(from);

    xaccAccountSetCurrency (ret, com);

    return ret;
}

GNCCommodityEdit *
gnc_get_new_user_commodity_editor(void)
{
    GtkWidget *tmp_wid = gtk_object_get_data(GTK_OBJECT(newUserDialog),
                                             "commod_editor");

    if(!tmp_wid)
    {
        GNCCommodityEdit *cur_editor = NULL;
        cur_editor = GNC_COMMODITY_EDIT(gnc_commodity_edit_new());
        gtk_widget_set_name (GTK_WIDGET(cur_editor),
                             "newAccountCurrencyChooser");
        gtk_widget_show(GTK_WIDGET(cur_editor));
        gnc_commodity_edit_set_commodity(cur_editor,
                                         gnc_locale_default_currency());
        gtk_object_set_data(GTK_OBJECT(newUserDialog), "commod_editor",
                            cur_editor);
        return cur_editor;
    }
    else
    {
        return GNC_COMMODITY_EDIT(tmp_wid);
    }

}

GtkWidget*
gnc_get_new_user_dialog(void)
{
    return newUserDialog;
}

GNCAmountEdit *
gnc_new_user_get_balance_editor(void)
{
  if (!newUserDialog) return NULL;

  return gtk_object_get_data (GTK_OBJECT (newUserDialog), "balance_editor");
}

gboolean
gnc_new_user_dialog_is_new_user(void)
{
    return (gboolean)gtk_object_get_data(GTK_OBJECT(newUserDialog),
                                         "is_new_user_dialog");
}

struct add_group_data_struct
{
    AccountGroup *to;
    gnc_commodity *com;
};

static gpointer
add_groups_for_each(Account *toadd, gpointer data)
{
    struct add_group_data_struct *dadata =
    (struct add_group_data_struct*)data;
    Account *foundact;
    
    foundact = xaccGetAccountFromName(dadata->to, xaccAccountGetName(toadd));

    if(!foundact)
    {
        foundact = clone_account(toadd, dadata->com);

        xaccGroupInsertAccount(dadata->to, foundact);
    }
    
    {
        AccountGroup *addgrp = xaccAccountGetChildren(toadd);

        if(xaccGroupGetNumAccounts(addgrp) > 0)
        {
            struct add_group_data_struct downdata;
            downdata.to = xaccAccountGetChildren(foundact);
            downdata.com = dadata->com;
            xaccGroupForEachAccount(addgrp, add_groups_for_each,
                                    &downdata, FALSE);
        }
    }
    return NULL;
}

static void
add_groups_to_with_random_guids(AccountGroup *into, AccountGroup *from,
                                gnc_commodity *com)
{
    struct add_group_data_struct data;
    data.to = into;
    data.com = com;
    
    xaccGroupForEachAccount(from, add_groups_for_each, &data, FALSE);
}


AccountGroup*
gnc_new_user_merge_groups(GSList *dalist)
{
    GSList *mark;
    gnc_commodity *com;
    AccountGroup *ret = xaccMallocAccountGroup();

    com = gnc_commodity_edit_get_commodity(
        gnc_get_new_user_commodity_editor());
    
    for(mark = dalist; mark; mark = mark->next)
    {
        add_groups_to_with_random_guids(
            ret, ((GncExampleAccount*)mark->data)->group, com);
    }

    return ret;
}

GtkWidget*
gnc_new_user_get_widget(const char *name)
{
    return lookup_widget(newUserDialog, name);
}

GtkCList*
gnc_new_user_get_clist(void)
{
    return GTK_CLIST(gnc_new_user_get_widget("newAccountTypesList"));
}

GtkCTree *
gnc_new_user_get_final_account_tree (void)
{
    return GTK_CTREE(gnc_new_user_get_widget("finalAccountCTree"));
}

void
gnc_new_user_set_balance (Account *account, gnc_numeric in_balance)
{
  GHashTable *hash;
  gnc_numeric *balance;
  char *fullname;

  if (!account || !newUserDialog) return;

  hash = gtk_object_get_data (GTK_OBJECT (newUserDialog), "balance_hash");
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

gnc_numeric
gnc_new_user_get_balance (Account *account)
{
  GHashTable *hash;
  gnc_numeric *balance;
  char *fullname;

  if (!account || !newUserDialog) return gnc_numeric_zero ();

  hash = gtk_object_get_data (GTK_OBJECT (newUserDialog), "balance_hash");
  if (!hash) return gnc_numeric_zero ();

  fullname = xaccAccountGetFullName (account, ':');

  balance = g_hash_table_lookup (hash, fullname);

  g_free (fullname);

  if (balance)
    return *balance;

  return gnc_numeric_zero ();
}

void
gnc_new_user_block_amount_changed (void)
{
  GNCAmountEdit *balance_edit;

  balance_edit = gnc_new_user_get_balance_editor ();
  if (!balance_edit) return;

  gtk_signal_handler_block_by_func
    (GTK_OBJECT (balance_edit),
     GTK_SIGNAL_FUNC(on_finalAccountBalanceEdit_changed), NULL);
}

void
gnc_new_user_unblock_amount_changed (void)
{
  GNCAmountEdit *balance_edit;

  balance_edit = gnc_new_user_get_balance_editor ();
  if (!balance_edit) return;

  gtk_signal_handler_unblock_by_func
    (GTK_OBJECT (balance_edit),
     GTK_SIGNAL_FUNC(on_finalAccountBalanceEdit_changed), NULL);
}

/***********************************************************************/
static int
createit(GtkWidget*(*creator)(), GtkWidget** placetoput)
{
    if(*placetoput != NULL)
    {
        return 0;
    }
    *placetoput = creator();
    gtk_widget_show(*placetoput);
    return 1;
}

static int
deleteit(GtkWidget** togetridof)
{
    if(*togetridof == NULL)
    {
        return 0;
    }
    gtk_widget_hide(*togetridof);
    gtk_widget_destroy(GTK_WIDGET(*togetridof));
    *togetridof = NULL;
    return 1;
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
gnc_new_user_destroy_cb (GtkObject *obj, gpointer user_data)
{
  GHashTable *hash;

  hash = gtk_object_get_data (obj, "balance_hash");
  if (hash)
  {
    g_hash_table_foreach (hash, destroy_hash_helper, NULL);
    g_hash_table_destroy (hash);
    gtk_object_set_data (obj, "balance_hash", NULL);
  }
}

static GtkWidget *
gnc_create_newUserDialog (void)
{
  GtkWidget *balance_edit;
  GtkWidget *dialog;
  GtkWidget *druid;
  GtkWidget *clist;
  GtkWidget *box;
  GHashTable *hash;

  dialog = create_newUserDialog();

  druid = lookup_widget (dialog, "newUserDruid");
  gnc_druid_set_colors (GNOME_DRUID (druid));

  balance_edit = gnc_amount_edit_new ();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (balance_edit), TRUE);
  gtk_widget_show (balance_edit);

  gtk_signal_connect (GTK_OBJECT (balance_edit), "amount_changed",
                      GTK_SIGNAL_FUNC(on_finalAccountBalanceEdit_changed),
                      NULL);

  clist = lookup_widget (dialog, "newAccountTypesList");
  gtk_clist_column_titles_passive (GTK_CLIST (clist));

  box = lookup_widget (dialog, "startBalanceBox");
  gtk_box_pack_start (GTK_BOX (box), balance_edit, TRUE, TRUE, 0);

  gtk_object_set_data (GTK_OBJECT(dialog), "balance_editor", balance_edit);

  hash = g_hash_table_new (g_str_hash, g_str_equal);

  gtk_object_set_data (GTK_OBJECT(dialog), "balance_hash", hash);

  gtk_signal_connect (GTK_OBJECT(dialog), "destroy",
                      GTK_SIGNAL_FUNC(gnc_new_user_destroy_cb), NULL);

  return dialog;
}

int
gnc_ui_show_new_user_window(gboolean new_user_dialog)
{
    int ret = createit(gnc_create_newUserDialog, &newUserDialog);
    if(ret)
    {
        gtk_object_set_data(GTK_OBJECT(newUserDialog),
                            "is_new_user_dialog", (gpointer)new_user_dialog);
    }
    return ret;
}

int
gnc_ui_delete_new_user_window(void)
{
    return deleteit(&newUserDialog);
}

int
gnc_ui_show_nu_cancel_dialog(void)
{
    return createit(create_addAccountCancelDialog, &cancelDialog);
}

int
gnc_ui_delete_nu_cancel_dialog(void)
{
    return deleteit(&cancelDialog);
}

void
gnc_ui_show_new_user_choice_window(void)
{
  GtkWidget *dialog;
  GtkWidget *new_accounts_button;
  GtkWidget *import_qif_button;
  GtkWidget *tutorial_button;
  gint result;

  dialog = create_newUserChoiceWindow ();

  gnome_dialog_close_hides (GNOME_DIALOG (dialog), TRUE);

  new_accounts_button = lookup_widget (dialog, "new_accounts_button");
  import_qif_button = lookup_widget (dialog, "import_qif_button");
  tutorial_button = lookup_widget (dialog, "tutorial_button");

  result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
  if (result != 0)
  {
    gnc_ui_show_nu_cancel_dialog();
    gtk_widget_destroy (dialog);
    gncp_new_user_finish ();
    return;
  }

  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (new_accounts_button)))
  {
    gnc_ui_show_new_user_window (1);
    gtk_widget_destroy (dialog);
    return;
  }

  if (gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON (import_qif_button)))
  {
    gnc_ui_qif_import_druid_make ();
  }
  else if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (tutorial_button)))
  {
    helpWindow (NULL, NULL, HH_QUICKSTART);
  }

  gncp_new_user_finish ();
  gtk_widget_destroy (dialog);
}

void
gncp_new_user_finish (void)
{
  gh_eval_str("(gnc:default-ui-start)");
  gh_eval_str("(gnc:show-main-window)");
  gh_eval_str("(gnc:hook-run-danglers gnc:*book-opened-hook* #f)");
}
