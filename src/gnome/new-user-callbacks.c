#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <glib.h>

#include <gnome.h>

#include "new-user-callbacks.h"
#include "new-user-interface.h"
#include "glade-support-gnc-dialogs.h"
#include "new-user-funs.h"

#include <guile/gh.h>

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

    /* Need to load the account lists here */
    
    return FALSE;
}


gboolean
on_chooseAccountTypesPage_next         (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data)
{
    gnc_ui_show_nu_account_list();

    /* need to fill up the account list info here */
    
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
on_newAccountRunAgain_toggled          (GtkToggleButton *togglebutton,
                                        gpointer         user_data)
{
    /* I'm not sure this is needed.  FIXME: remove */
}


void
on_newAccountCancelDialog_OKButton_clicked
                                        (GtkButton       *button,
                                        gpointer         user_data)
{
    gboolean keepshowing = TRUE;

    /* keepshowing = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_BOX(GTK_DIALOG(GTK_WIDGET(button)->parent)->vbox)->children[1])); */

    set_first_startup(keepshowing);
    
    gnc_ui_delete_new_user_window();
    gnc_ui_delete_nu_cancel_dialog();
    gnc_ui_delete_nu_account_list();
    
    gh_eval_str("(gnc:default-ui-start)");
    gh_eval_str("(gnc:show-main-window)");
}

