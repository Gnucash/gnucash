/********************************************************************\
 * new-user-callbacks.c - new user functionality for GnuCash        *
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

#include <glib.h>

#include <gnome.h>

#include "new-user-callbacks.h"
#include "new-user-interface.h"
#include "gnc-commodity-edit.h"
#include "glade-support.h"
#include "new-user-funs.h"
#include "gnc-ui-util.h"

#include <guile/gh.h>

static int commodEditAdded = 0;

static void
set_first_startup(int first_startup)
{
    gchar *todo;

    todo = g_strdup_printf("((gnc:option-setter "
                           " (gnc:lookup-global-option \"__new_user\" "
                           "                           \"first_startup\"))"
                           " %d)", first_startup);
    gh_eval_str(todo);
    g_free(todo);
}


gboolean
on_newUserStartPage_next               (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{

    return FALSE;
}


gboolean
on_chooseAccountTypesPage_next         (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    return FALSE;
}



void
on_newUserDruidFinishPage_finish       (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    gnc_ui_delete_new_user_window();
    gnc_ui_delete_nu_account_list();
    
    gh_eval_str("(gnc:default-ui-start)");

    /* now we need to load all the accounts into the program */

    gh_eval_str("(gnc:show-main-window)");

    set_first_startup(0);
}


void
on_accountChooseDruidPage_cancel       (GnomeDruid      *gnomedruid,
                                        gpointer         user_data)
{
    gnc_ui_show_nu_cancel_dialog();
}


void
on_newAccountCancelDialog_OKButton_clicked
                                        (GtkButton       *button,
                                        gpointer         user_data)
{
    gboolean keepshowing = TRUE;

    keepshowing = gtk_toggle_button_get_active(
        GTK_TOGGLE_BUTTON(lookup_widget(
                              GTK_WIDGET(button),
                              "newAccountCancelDialog_RunAgainToggle")));

    set_first_startup(keepshowing);
    
    gnc_ui_delete_new_user_window();
    gnc_ui_delete_nu_cancel_dialog();
    gnc_ui_delete_nu_account_list();
    
    gh_eval_str("(gnc:default-ui-start)");
    gh_eval_str("(gnc:show-main-window)");
}


void
on_newAccountCurrencyChoosePage_prepare (GnomeDruidPage  *gnomedruidpage,
                                         gpointer         arg1,
                                         gpointer         user_data)
{
    /* need to load currency info here.  In fact drop a
       gnc-commodity-edit widget here */
    GtkWidget *commodityWid;
    GtkWidget *vbox;

    if(!commodEditAdded)
    {
        commodEditAdded = 1;
        commodityWid = gnc_commodity_edit_new();
        gtk_widget_show(commodityWid);
        gnc_commodity_edit_set_commodity(GNC_COMMODITY_EDIT(commodityWid),
                                         gnc_locale_default_currency());
        
        vbox = lookup_widget(GTK_WIDGET(gnomedruidpage),
                             "newAccountCurrencyChooser_vbox");
        
        gtk_box_pack_start(GTK_BOX(vbox), commodityWid, FALSE, FALSE, 0);
    }
    
}


void
on_chooseAccountTypesPage_prepare      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    /* Need to load the account type lists here */
}


void
on_newUserDruidFinishPage_prepare      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    gnc_ui_show_nu_account_list();

    /* need to fill up the account list info here */
}


void
on_newAccountTypesList_select_row      (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
    /* need to fill in useful data on a select row */
}


void
on_newAccountTree_select_row           (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
    /* need to put info in the box and account name here */
}

