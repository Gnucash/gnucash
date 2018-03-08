/********************************************************************\
 * import-account-matcher.c - flexible account picker/matcher       *
 *                                                                  *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
 * Copyright (C) 2012 Robert Fewell                                 *
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
/** @addtogroup Import_Export
    @{ */
/**@internal
	@file import-account-matcher.c
 * \brief A very generic and flexible account matcher/picker
 \author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "import-account-matcher.h"
#include "import-utilities.h"
#include "dialog-account.h"
#include "dialog-utils.h"

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"

static QofLogModule log_module = GNC_MOD_IMPORT;

#define STATE_SECTION "dialogs/import/generic_matcher/account_matcher"

/*-******************************************************************\
 * Functions needed by gnc_import_select_account
 *
\********************************************************************/
/** Constructor for AccountPickerDialog.
 * @return Pointer to a new AccountPickerDialog
 */
static AccountPickerDialog* gnc_import_new_account_picker(void)
{
    AccountPickerDialog* picker = g_new(AccountPickerDialog, 1);
    picker->dialog = NULL;
    picker->assistant = NULL;
    picker->account_tree = NULL;
    picker->account_tree_sw = NULL;
    picker->auto_create = TRUE;
    picker->account_human_description = NULL;
    picker->account_online_id_value = NULL;
    picker->account_online_id_label = NULL;
    picker->new_account_default_commodity = NULL;
    picker->new_account_default_type = 0;
    picker->default_account = NULL;
    picker->retAccount = NULL;
    return picker;
}


/**************************************************
 * test_acct_online_id_match
 *
 * test for match of account online_ids.
 **************************************************/
static gpointer test_acct_online_id_match(Account *acct, gpointer param_online_id)
{
    const gchar * current_online_id = gnc_import_get_acc_online_id(acct);
    if ( (current_online_id != NULL
            && param_online_id != NULL )
            && strcmp( current_online_id, param_online_id ) == 0 )
    {
        return (gpointer *) acct;
    }
    else
    {
        return NULL;
    }
}


/***********************************************************
 * build_acct_tree
 *
 * build the account tree with the custome column, online_id
 ************************************************************/
static void
build_acct_tree(AccountPickerDialog *picker)
{
    GtkTreeView *account_tree;
    GtkTreeViewColumn *col;

    /* Build a new account tree */
    DEBUG("Begin");
    account_tree = gnc_tree_view_account_new(FALSE);
    picker->account_tree = GNC_TREE_VIEW_ACCOUNT(account_tree);
    gtk_tree_view_set_headers_visible (account_tree, TRUE);
    col = gnc_tree_view_find_column_by_name(GNC_TREE_VIEW(account_tree), "type");
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    /* Add our custom column. */
    col = gnc_tree_view_account_add_property_column (picker->account_tree,
            _("Account ID"), "online-id");
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    // the color background data function is part of the add_property_column
    // function which will color the background based on preference setting

    gtk_container_add(GTK_CONTAINER(picker->account_tree_sw),
                      GTK_WIDGET(picker->account_tree));

    /* Configure the columns */
    gnc_tree_view_configure_columns (GNC_TREE_VIEW(picker->account_tree));
    g_object_set(account_tree,
                 "state-section", STATE_SECTION,
                 "show-column-menu", TRUE,
                 (gchar*) NULL);
}


/*******************************************************
 * gnc_import_add_account
 *
 * Callback for when user clicks to create a new account
 *******************************************************/
static void
gnc_import_add_account(GtkWidget *button, AccountPickerDialog *picker)
{
    Account *selected_account, *new_account;
    GList * valid_types = NULL;
    GtkWindow *parent = NULL;

    if (picker->dialog != NULL)
        parent = GTK_WINDOW (picker->dialog);
    else
        parent = GTK_WINDOW (picker->assistant);

    /*DEBUG("Begin");  */
    if (picker->new_account_default_type != ACCT_TYPE_NONE)
    {
        /*Yes, this is weird, but we really DO want to pass the value instead of the pointer...*/
        valid_types = g_list_prepend(valid_types, GINT_TO_POINTER(picker->new_account_default_type));
    }
    selected_account = gnc_tree_view_account_get_selected_account(picker->account_tree);
    new_account = gnc_ui_new_accounts_from_name_with_defaults (parent,
                                          picker->account_human_description,
                                          valid_types,
                                          picker->new_account_default_commodity,
                                          selected_account);
    g_list_free(valid_types);
    gnc_tree_view_account_set_selected_account(picker->account_tree, new_account);
}


/*******************************************************
 * account_tree_row_activated_cb
 *
 * Callback for when user double clicks on an account
 *******************************************************/
static void
account_tree_row_activated_cb(GtkTreeView *view, GtkTreePath *path,
                              GtkTreeViewColumn *column,
                              AccountPickerDialog *picker)
{
    const gchar *retval_name = NULL;
    Account *old_id_acc;

    /* See if we have a dialog, if not we are an assistant */
    if (picker->dialog == NULL)
    {
        GtkAssistant *assistant = GTK_ASSISTANT(picker->assistant);
        gint num = gtk_assistant_get_current_page (assistant);
        GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

        picker->retAccount = gnc_tree_view_account_get_selected_account(picker->account_tree);
        if (picker->retAccount)
            retval_name = xaccAccountGetName(picker->retAccount);
        if (!retval_name)
            retval_name = "(null)";
        DEBUG("Selected account %p, %s", picker->retAccount, retval_name);

        /* See if the selected account is a placeholder. */
        if (picker->retAccount && xaccAccountGetPlaceholder (picker->retAccount))
        {
            gnc_error_dialog (GTK_WINDOW (picker->dialog),
                              _("The account %s is a placeholder account and does not allow "
                                "transactions. Please choose a different account."),
                              retval_name);
        }
        else if ( picker->account_online_id_value != NULL)
        {
            /* find the old account for this on line id value and reset it */
            old_id_acc =
                gnc_account_foreach_descendant_until(gnc_get_current_root_account (),
                        test_acct_online_id_match,
                        /* This argument will only be used as a "const char*" */
                        (void*)picker->account_online_id_value);

            if (old_id_acc != NULL)
                gnc_import_set_acc_online_id(old_id_acc, "");

            gnc_import_set_acc_online_id(picker->retAccount, picker->account_online_id_value);
            gtk_assistant_set_page_complete (assistant, page, TRUE);
        }
        else
            gtk_assistant_set_page_complete (assistant, page, TRUE);
    }
    else
    {
        gtk_dialog_response(GTK_DIALOG(picker->dialog), GTK_RESPONSE_OK);
    }
}


/*******************************************************
 * gnc_import_select_account
 *
 * Main call for use with a dialog
 *******************************************************/
Account * gnc_import_select_account(GtkWidget *parent,
                                    const gchar * account_online_id_value,
                                    gboolean auto_create,
                                    const gchar * account_human_description,
                                    const gnc_commodity * new_account_default_commodity,
                                    GNCAccountType new_account_default_type,
                                    Account * default_selection,
                                    gboolean * ok_pressed)
{
#define ACCOUNT_DESCRIPTION_MAX_SIZE 255
    AccountPickerDialog * picker;
    gint response;
    Account * retval = NULL;
    const gchar *retval_name = NULL;
    GtkBuilder *builder;
    GtkWidget * online_id_label, *box, *pbox;
    gchar account_description_text[ACCOUNT_DESCRIPTION_MAX_SIZE] = "";
    gboolean ok_pressed_retval = FALSE;

    ENTER("Default commodity received: %s", gnc_commodity_get_fullname( new_account_default_commodity));
    DEBUG("Default account type received: %s", xaccAccountGetTypeStr( new_account_default_type));
    picker = g_new0(AccountPickerDialog, 1);

    picker->account_online_id_value = account_online_id_value;
    picker->account_human_description =  account_human_description;
    picker->new_account_default_commodity = new_account_default_commodity;
    picker->new_account_default_type = new_account_default_type;

    /*DEBUG("Looking for account with online_id: \"%s\"", account_online_id_value);*/
    if (account_online_id_value != NULL)
    {
        retval =
            gnc_account_foreach_descendant_until(gnc_get_current_root_account (),
                    test_acct_online_id_match,
                    /* This argument will only be used as a "const char*" */
                    (void*)account_online_id_value);

        /* BEGIN: try again without extra space at the end */
        /*
         * libofx, used for file import, generates online_id as
         * ACCTID + space + ACCTKEY which differs from the online_id
         * generated by aqbanking for online ofx transfer as ACCTID.
         *
         * If a gnucash account has been associated with an online_id
         * via aqbanking data, it is not possible to construct an OFX
         * file for gnucash import that matches the same online_id
         * because even with no ACCTKEY in the file, there will be a
         * trailing space.
         *
         * This is a hack to overcome that problem.
         */
        if ((retval == NULL) && g_str_has_suffix(account_online_id_value, " "))
        {
            gchar *trimmed = g_strndup(account_online_id_value, strlen(account_online_id_value) - 1);
            if (trimmed)
            {
                retval = gnc_account_foreach_descendant_until(
                             gnc_get_current_root_account (),
                             test_acct_online_id_match,
                             (void *)trimmed);
            }
            g_free(trimmed);
        }
        /* END: try again without extra space at the end */
    }
    if (retval == NULL && auto_create != 0)
    {
        /* load the interface */
        builder = gtk_builder_new();
        gnc_builder_add_from_file (builder, "dialog-import.glade", "account_new_icon");
        gnc_builder_add_from_file (builder, "dialog-import.glade", "account_picker_dialog");
        gnc_builder_add_from_file (builder, "dialog-import.glade", "account_picker_content");
        /* connect the signals in the interface */
        if (builder == NULL)
        {
            PERR("Error opening the glade builder interface");
        }
        picker->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "account_picker_dialog"));
        if (parent)
            gtk_window_set_transient_for (GTK_WINDOW (picker->dialog),
                                          GTK_WINDOW (parent));

        /* Pack the content into the dialog vbox */
        pbox = GTK_WIDGET(gtk_builder_get_object (builder, "account_picker_vbox"));
        box = GTK_WIDGET(gtk_builder_get_object (builder, "account_picker_content"));
        gtk_box_pack_start( GTK_BOX(pbox), box, TRUE, TRUE, 0);

        picker->account_tree_sw = GTK_WIDGET(gtk_builder_get_object (builder, "account_tree_sw"));
        online_id_label = GTK_WIDGET(gtk_builder_get_object (builder, "online_id_label"));

        //printf("gnc_import_select_account(): Fin get widget\n");

        if (account_human_description != NULL)
        {
            strncat(account_description_text, account_human_description,
                    ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
            strncat(account_description_text, "\n",
                    ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
        }
        if (account_online_id_value != NULL)
        {
            strncat(account_description_text, _("(Full account ID: "),
                    ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
            strncat(account_description_text, account_online_id_value,
                    ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
            strncat(account_description_text, ")",
                    ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
        }
        gtk_label_set_text((GtkLabel*)online_id_label, account_description_text);
        build_acct_tree(picker);
        gnc_tree_view_account_set_selected_account(picker->account_tree, default_selection);

        gtk_window_set_modal(GTK_WINDOW(picker->dialog), TRUE);
        g_signal_connect(picker->account_tree, "row-activated",
                         G_CALLBACK(account_tree_row_activated_cb), picker);
        do
        {
            response = gtk_dialog_run(GTK_DIALOG(picker->dialog));
            switch (response)
            {
            case GNC_RESPONSE_NEW:
                gnc_import_add_account(NULL, picker);
                response = GTK_RESPONSE_OK;
                /* no break */

            case GTK_RESPONSE_OK:
                retval = gnc_tree_view_account_get_selected_account(picker->account_tree);
                if (retval == NULL)
                {
                    response = GNC_RESPONSE_NEW;
                    break;
                }
                if (retval)
                    retval_name = xaccAccountGetName(retval);
                if (!retval_name)
                    retval_name = "(null)";
                DEBUG("Selected account %p, %s", retval, retval_name);

                /* See if the selected account is a placeholder. */
                if (retval && xaccAccountGetPlaceholder (retval))
                {
                    gnc_error_dialog
                    (GTK_WINDOW (picker->dialog),
                     _("The account %s is a placeholder account and does not allow "
                       "transactions. Please choose a different account."),
                     retval_name);
                    response = GNC_RESPONSE_NEW;
                    break;
                }

                if ( account_online_id_value != NULL)
                {
                    gnc_import_set_acc_online_id(retval, account_online_id_value);
                }
                ok_pressed_retval = TRUE;
                break;

            default:
                ok_pressed_retval = FALSE;
                break;
            }
        }
        while (response == GNC_RESPONSE_NEW);

        g_object_unref(G_OBJECT(builder));
        gtk_widget_destroy(picker->dialog);
    }
    else
    {
        retval_name = retval ? xaccAccountGetName(retval) : NULL;
        ok_pressed_retval = TRUE; /* There was no dialog involved, so the computer "pressed" ok */
    }
    /*FIXME: DEBUG("WRITEME: gnc_import_select_account() Here we should check if account type is compatible, currency matches, etc.\n"); */
    g_free(picker);
    /*DEBUG("Return value: %p%s%s%s",retval,", account name:",xaccAccountGetName(retval),"\n");*/
    if (ok_pressed != NULL)
    {
        *ok_pressed = ok_pressed_retval;
    }
    LEAVE("Selected account %p, %s", retval, retval_name ? retval_name : "(null)");
    return retval;
}


/**********************************************************************
 * These are the routines for use with an Assistant page
 **********************************************************************/

/*******************************************************
 * gnc_import_account_assist_setup
 *
 * Main call for page setup in an assistant
 *******************************************************/
AccountPickerDialog* gnc_import_account_assist_setup(GtkWidget *parent)
{
    AccountPickerDialog * picker;
    GtkBuilder *builder;
    GtkWidget  *box, *h_box;

    /* Init the account picker structure */
    picker = gnc_import_new_account_picker();

    /* load the interface */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "account_picker_content");
    /* connect the signals in the interface */
    if (builder == NULL)
    {
        PERR("Error opening the glade builder interface");
    }

    picker->assistant = gtk_widget_get_parent(parent);
    /* Pack content into Assistant page widget */
    box = GTK_WIDGET(gtk_builder_get_object (builder, "account_picker_content"));
    gtk_box_pack_start( GTK_BOX(parent), box, TRUE, TRUE, 6);

    picker->account_tree_sw = GTK_WIDGET(gtk_builder_get_object (builder, "account_tree_sw"));
    picker->account_online_id_label = GTK_WIDGET(gtk_builder_get_object (builder, "online_id_label"));

    /* Add the New Account Button */
    picker->new_button = gtk_button_new_with_mnemonic (_("_New Account"));

    h_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (h_box), TRUE);

    gtk_box_pack_start(GTK_BOX(h_box), picker->new_button, FALSE, FALSE, 0);
    gtk_box_pack_start( GTK_BOX(box), h_box, FALSE, FALSE, 6);
    gtk_widget_show (picker->new_button);
    g_signal_connect(picker->new_button, "clicked",
                     G_CALLBACK(gnc_import_add_account), picker);

    build_acct_tree(picker);

    g_signal_connect(picker->account_tree, "row-activated",
                     G_CALLBACK(account_tree_row_activated_cb), picker);

    g_object_unref(G_OBJECT(builder));
    return picker;
}


/*******************************************************
 * gnc_import_account_assist_disable
 *
 * disables account picker input.
 *******************************************************/
void
gnc_import_account_assist_disable (AccountPickerDialog *picker, gboolean disable)
{
    gtk_widget_set_sensitive (picker->account_tree_sw, !disable);
    gtk_widget_set_sensitive (picker->new_button, !disable);
}


/*******************************************************
 * gnc_import_account_assist_update
 *
 * updates the page and returns account found.
 *******************************************************/
Account * gnc_import_account_assist_update (AccountPickerDialog *picker)
{
#define ACCOUNT_DESCRIPTION_MAX_SIZE 255

    const gchar *retval_name = NULL;
    gchar account_description_text[ACCOUNT_DESCRIPTION_MAX_SIZE] = "";

    ENTER("Default commodity received: %s", gnc_commodity_get_fullname( picker->new_account_default_commodity));
    DEBUG("Default account type received: %s", xaccAccountGetTypeStr( picker->new_account_default_type));

    /*DEBUG("Looking for account with online_id: %s", picker->account_online_id_value);*/
    if (picker->account_online_id_value != NULL)
    {
        picker->retAccount =
            gnc_account_foreach_descendant_until(gnc_get_current_root_account (),
                    test_acct_online_id_match,
                    /* This argument will only be used as a "const char*" */
                    (void*)picker->account_online_id_value);
    }

    if (picker->account_human_description != NULL)
    {
        strncat(account_description_text, picker->account_human_description,
                ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
        strncat(account_description_text, "\n",
                ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
    }
    if (picker->account_online_id_value != NULL)
    {
        strncat(account_description_text, _("(Full account ID: "),
                ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
        strncat(account_description_text, picker->account_online_id_value,
                ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
        strncat(account_description_text, ")",
                ACCOUNT_DESCRIPTION_MAX_SIZE - strlen(account_description_text));
    }
    gtk_label_set_text(GTK_LABEL( picker->account_online_id_label), account_description_text);

    if (picker->default_account == NULL)
        gnc_tree_view_account_set_selected_account(picker->account_tree, picker->retAccount);
    else
        gnc_tree_view_account_set_selected_account(picker->account_tree, picker->default_account);

    /*FIXME: DEBUG("WRITEME: Here we should check if an account type is compatible, currency matches, etc.\n"); */

    /*DEBUG("Return value: %p%s%s%s",picker->retAccount,", account name:",xaccAccountGetName(picker->retAccount),"\n");*/
    retval_name = picker->retAccount ? xaccAccountGetName(picker->retAccount) : NULL;
    LEAVE("Selected account %p, %s", picker->retAccount, retval_name ? retval_name : "(null)");
    return picker->retAccount;
}

/**@}*/
