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

#include <guile/gh.h>

static GtkWidget *newUserDialog = NULL;
static GtkWidget *cancelDialog = NULL;
static GtkWidget *accountList = NULL;

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
