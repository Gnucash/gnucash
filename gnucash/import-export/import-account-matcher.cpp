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
#include "gnc-prefs.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"

static QofLogModule log_module = GNC_MOD_IMPORT;

#define STATE_SECTION "dialogs/import/generic_matcher/account_matcher"

#define GNC_PREFS_GROUP "dialogs.import.generic.account-picker"

typedef struct
{
    Account* partial_match;
    int count;
    const char* online_id;
} AccountOnlineMatch;

/*-******************************************************************\
 * Functions needed by gnc_import_select_account
 *
\********************************************************************/

/**************************************************
 * test_acct_online_id_match
 *
 * test for match of account online_ids.
 **************************************************/
static gpointer test_acct_online_id_match(Account *acct, gpointer data)
{
    AccountOnlineMatch *match = (AccountOnlineMatch*)data;
    char *acct_online_id = gnc_import_get_acc_online_id(acct);
    int acct_len, match_len;

    if (acct_online_id == NULL || match->online_id == NULL)
    {
        if (acct_online_id)
            g_free (acct_online_id);
        return NULL;
    }

    acct_len = strlen(acct_online_id);
    match_len = strlen(match->online_id);

    if (acct_online_id[acct_len - 1] == ' ')
        --acct_len;
    if (match->online_id[match_len - 1] == ' ')
        --match_len;

    if (strncmp (acct_online_id, match->online_id, acct_len) == 0)
    {
        if (strncmp(acct_online_id, match->online_id, match_len) == 0)
        {
            g_free (acct_online_id);
            return (gpointer *) acct;
        }
        if (match->partial_match == NULL)
        {
            match->partial_match = acct;
            ++match->count;
        }
        else
        {
            char *partial_online_id =
                gnc_import_get_acc_online_id(match->partial_match);
            int partial_len = strlen(partial_online_id);
            if (partial_online_id[partial_len - 1] == ' ')
                --partial_len;
            /* Both partial_online_id and acct_online_id are substrings of
             * match->online_id, but whichever is longer is the better match.
             * Reset match->count to 1 just in case there was ambiguity on the
             * shorter partial match.
             */
            if (partial_len < acct_len)
            {
                match->partial_match = acct;
                match->count = 1;
            }
            /* If they're the same size then there are two accounts with the
             * same online id and we don't know which one to select. Increment
             * match->count to dissuade gnc_import_find_account from using
             * match->online_id and log an error.
             */
            else if (partial_len == acct_len)
            {
                gchar *name1, *name2;
                ++match->count;
                name1 = gnc_account_get_full_name (match->partial_match);
                name2 = gnc_account_get_full_name (acct);
                PERR("Accounts %s and %s have the same online-id %s",
                     name1, name2, partial_online_id);
                g_free (name1);
                g_free (name2);
            }
            g_free (partial_online_id);
        }
    }

    g_free (acct_online_id);
    return NULL;
}


/***********************************************************
 * build_acct_tree
 *
 * build the account tree with the custom column, online_id
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


/***********************************************************
 * show_warning
 *
 * show the warning and disable OK button
 ************************************************************/
static void
show_warning (AccountPickerDialog *picker, gchar *text)
{
    gtk_label_set_text (GTK_LABEL(picker->warning), text);
    gnc_label_set_alignment (picker->warning, 0.0, 0.5);
    gtk_widget_show_all (GTK_WIDGET(picker->whbox));
    g_free (text);

    gtk_widget_set_sensitive (picker->ok_button, FALSE); // disable OK button
}


/***********************************************************
 * show_placeholder_warning
 *
 * show the warning when account is a place holder
 ************************************************************/
static void
show_placeholder_warning (AccountPickerDialog *picker, const gchar *name)
{
    gchar *text = g_strdup_printf (_("The account '%s' is a placeholder account and does not allow "
                                     "transactions. Please choose a different account."), name);

    show_warning (picker, text);
}


/*******************************************************
 * account_tree_row_changed_cb
 *
 * Callback for when user selects a different row
 *******************************************************/
static void
account_tree_row_changed_cb (GtkTreeSelection *selection,
                             AccountPickerDialog *picker)
{
    Account *sel_account = gnc_tree_view_account_get_selected_account (picker->account_tree);

    /* Reset buttons and warnings */
    gtk_widget_hide (GTK_WIDGET(picker->whbox));
    gtk_widget_set_sensitive (picker->ok_button, (sel_account != NULL));

    /* See if the selected account is a placeholder. */
    if (sel_account && xaccAccountGetPlaceholder (sel_account))
    {
        const gchar *retval_name = xaccAccountGetName (sel_account);
        show_placeholder_warning (picker, retval_name);
    }
}

static gboolean
account_tree_key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    // Expand the tree when the user starts typing, this will allow sub-accounts to be found.
    if (event->length == 0)
        return FALSE;
    
    switch (event->keyval)
    {
        case GDK_KEY_plus:
        case GDK_KEY_minus:
        case GDK_KEY_asterisk:
        case GDK_KEY_slash:
        case GDK_KEY_KP_Add:
        case GDK_KEY_KP_Subtract:
        case GDK_KEY_KP_Multiply:
        case GDK_KEY_KP_Divide:
        case GDK_KEY_Up:
        case GDK_KEY_KP_Up:
        case GDK_KEY_Down:
        case GDK_KEY_KP_Down:
        case GDK_KEY_Home:
        case GDK_KEY_KP_Home:
        case GDK_KEY_End:
        case GDK_KEY_KP_End:
        case GDK_KEY_Page_Up:
        case GDK_KEY_KP_Page_Up:
        case GDK_KEY_Page_Down:
        case GDK_KEY_KP_Page_Down:
        case GDK_KEY_Right:
        case GDK_KEY_Left:
        case GDK_KEY_KP_Right:
        case GDK_KEY_KP_Left:
        case GDK_KEY_space:
        case GDK_KEY_KP_Space:
        case GDK_KEY_backslash:
        case GDK_KEY_Return:
        case GDK_KEY_ISO_Enter:
        case GDK_KEY_KP_Enter:
            return FALSE;
            break;
        default:
            gtk_tree_view_expand_all (GTK_TREE_VIEW(user_data));
            return FALSE;
    }
    return FALSE;
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
    gtk_dialog_response(GTK_DIALOG(picker->dialog), GTK_RESPONSE_OK);
}


/*******************************************************
 * gnc_import_select_account
 *
 * Main call for use with a dialog
 *******************************************************/
Account * gnc_import_select_account(GtkWidget *parent,
                                    const gchar * account_online_id_value,
                                    gboolean prompt_on_no_match,
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
    GtkTreeSelection *selection;
    GtkWidget * online_id_label;
    gchar account_description_text[ACCOUNT_DESCRIPTION_MAX_SIZE + 1] = "";
    gboolean ok_pressed_retval = FALSE;

    ENTER("Default commodity received: %s", gnc_commodity_get_fullname( new_account_default_commodity));
    DEBUG("Default account type received: %s", xaccAccountGetTypeStr( new_account_default_type));
    picker = g_new0(AccountPickerDialog, 1);

    picker->account_human_description =  account_human_description;
    picker->new_account_default_commodity = new_account_default_commodity;
    picker->new_account_default_type = new_account_default_type;

    /*DEBUG("Looking for account with online_id: \"%s\"", account_online_id_value);*/
    if (account_online_id_value)
    {
        AccountOnlineMatch match = {NULL, 0, account_online_id_value};
        retval = static_cast<Account*>(gnc_account_foreach_descendant_until (gnc_get_current_root_account (),
                                                                             test_acct_online_id_match,
                                                                             (void*)&match));
        if (!retval && match.count == 1 &&
            new_account_default_type == ACCT_TYPE_NONE)
            retval = match.partial_match;
    }
    if (!retval && prompt_on_no_match)
    {
        /* load the interface */
        builder = gtk_builder_new();
        gnc_builder_add_from_file (builder, "dialog-import.glade", "account_new_icon");
        gnc_builder_add_from_file (builder, "dialog-import.glade", "account_picker_dialog");
        /* connect the signals in the interface */
        if (builder == NULL)
        {
            PERR("Error opening the glade builder interface");
        }
        picker->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "account_picker_dialog"));
        picker->whbox = GTK_WIDGET(gtk_builder_get_object (builder, "warning_hbox"));
        picker->warning = GTK_WIDGET(gtk_builder_get_object (builder, "warning_label"));
        picker->ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "okbutton"));

        // Set the name for this dialog so it can be easily manipulated with css
        gtk_widget_set_name (GTK_WIDGET(picker->dialog), "gnc-id-import-account-picker");
        gnc_widget_style_context_add_class (GTK_WIDGET(picker->dialog), "gnc-class-imports");

        if (parent)
            gtk_window_set_transient_for (GTK_WINDOW (picker->dialog),
                                          GTK_WINDOW (parent));

        gnc_restore_window_size (GNC_PREFS_GROUP,
                                 GTK_WINDOW(picker->dialog), GTK_WINDOW (parent));

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
        gtk_window_set_modal(GTK_WINDOW(picker->dialog), TRUE);
        g_signal_connect(picker->account_tree, "row-activated",
                         G_CALLBACK(account_tree_row_activated_cb), picker);
        
        /* Connect key press event so we can expand the tree when the user starts typing, allowing
        * any subaccount to match */
        g_signal_connect (picker->account_tree, "key-press-event", G_CALLBACK (account_tree_key_press_cb), picker->account_tree);

        selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(picker->account_tree));
        g_signal_connect(selection, "changed",
                         G_CALLBACK(account_tree_row_changed_cb), picker);

        gnc_tree_view_account_set_selected_account(picker->account_tree, default_selection);

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
                if (!retval)
                {
                    response = GNC_RESPONSE_NEW;
                    break;
                }
                retval_name = xaccAccountGetName(retval);
                DEBUG("Selected account %p, %s", retval, retval_name ? retval_name : "(null)");

                /* See if the selected account is a placeholder. */
                if (retval && xaccAccountGetPlaceholder (retval))
                {
                    show_placeholder_warning (picker, retval_name);
                    response = GNC_RESPONSE_NEW;
                    break;
                }

                if (account_online_id_value)
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
        gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(picker->dialog));
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

/**@}*/
