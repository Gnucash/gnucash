#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gnome.h>
#include <stdio.h>

#include "new-user-callbacks.h"
#include "new-user-interface.h"

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
