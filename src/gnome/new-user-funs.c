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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gnome.h>
#include <stdio.h>

#include "new-user-callbacks.h"
#include "new-user-interface.h"
#include "new-user-funs.h"
#include "glade-support.h"
#include "gnc-currency-edit.h"
#include "gnc-ui-util.h"

#include "Group.h"
#include "io-example-account.h"
#include "Account.h"

#include <guile/gh.h>

static GtkWidget *newUserDialog = NULL;
static GtkWidget *cancelDialog = NULL;
static GtkWidget *accountList = NULL;

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
    static GNCCommodityEdit *cur_editor = NULL;
    if(!cur_editor)
    {
        cur_editor = GNC_COMMODITY_EDIT(gnc_commodity_edit_new());
        gtk_widget_set_name (GTK_WIDGET(cur_editor),
                             "newAccountCurrencyChooser");
        gtk_widget_show(GTK_WIDGET(cur_editor));
        gnc_commodity_edit_set_commodity(cur_editor,
                                         gnc_locale_default_currency());
    }
    return cur_editor;
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

int
gnc_ui_show_new_user_window(void)
{
    return createit(create_newUserDialog, &newUserDialog);
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

int
gnc_ui_show_nu_account_list(void)
{
    return createit(create_newAccountList, &accountList);
}

int
gnc_ui_delete_nu_account_list(void)
{
    return deleteit(&accountList);
}
