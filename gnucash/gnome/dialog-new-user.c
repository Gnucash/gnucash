/********************************************************************\
 * dialog-new-user.c -- new user dialog for GnuCash                 *
 * Copyright (C) 2001 Dave Peticolas <dave@krondo.com>              *
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-new-user.h"
#include "dialog-utils.h"
#include "dialog-file-access.h"
#include "assistant-hierarchy.h"
#include "gnc-engine.h"
#include "gnc-hooks.h"
#include "gnc-ui.h"
#include "gnc-file.h"
#include "gnc-prefs.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-account-tree.h"
#include "gnc-session.h"
#include "gnc-ui-util.h" // for gnc_get_current_book

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/* function to open a qif import assistant */
static void (*qifImportAssistantFcn)(void) = NULL;

struct _GNCNewUserDialog
{
    GtkWidget *window;

    GtkWidget *new_accounts_button;
    GtkWidget *import_qif_button;
    GtkWidget *tutorial_button;
    gboolean   ok_pressed;
};

static void
gnc_ui_new_user_cancel_dialog (GtkWindow *parent);

void
gnc_new_user_dialog_register_qif_assistant (void (*cb_fcn)(void))
{
    g_return_if_fail (qifImportAssistantFcn == NULL);
    qifImportAssistantFcn = cb_fcn;
}

void
gnc_set_first_startup (gboolean first_startup)
{
    gnc_prefs_set_bool (GNC_PREFS_GROUP_NEW_USER, GNC_PREF_FIRST_STARTUP, first_startup);
}

static void
after_hierarchy_assistant(void)
{
    gncp_new_user_finish ();
    gnc_set_first_startup (FALSE);

    qof_book_mark_session_dirty(gnc_get_current_book());
    gnc_ui_file_access_for_save_as (gnc_ui_get_main_window (NULL));
}

static void
gnc_ui_new_user_cancel_cb (GtkWidget * widget, gpointer data)
{
    GNCNewUserDialog *new_user = data;

    g_return_if_fail(new_user);
    gtk_widget_destroy (new_user->window);
}

static void
gnc_ui_new_user_destroy_cb (GtkWidget * widget, gpointer data)
{
    GNCNewUserDialog *new_user = data;

    g_return_if_fail(new_user);
    if (new_user->ok_pressed == FALSE)
        gnc_ui_new_user_cancel_dialog (GTK_WINDOW(new_user->window));

    g_free (new_user);
}

static void
gnc_ui_new_user_ok_cb (GtkWidget * widget, gpointer data)
{
    GNCNewUserDialog *new_user = data;

    g_return_if_fail(new_user);
    new_user->ok_pressed = TRUE;

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (new_user->new_accounts_button)))
    {
        gnc_ui_hierarchy_assistant_with_callback(TRUE, after_hierarchy_assistant);
    }
    else if ((qifImportAssistantFcn != NULL)
             && gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (new_user->import_qif_button)))
    {
        qifImportAssistantFcn();
        gncp_new_user_finish ();
    }
    else if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (new_user->tutorial_button)))
    {
        gnc_gnome_help (HF_GUIDE, NULL);
        gncp_new_user_finish ();
    }
    gtk_widget_destroy (new_user->window);
}

static gboolean
gnc_ui_new_user_window_present (GtkWindow *window)
{
    gtk_window_present (GTK_WINDOW(window));
    return FALSE;
}

static void
gnc_ui_new_user_dialog_create (GNCNewUserDialog *new_user)
{
    GtkWidget  *window;
    GtkBuilder *builder;
    GtkWidget  *button;
    gint result;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-new-user.glade", "new_user_window");
    new_user->window = GTK_WIDGET(gtk_builder_get_object (builder, "new_user_window"));

    gtk_window_set_keep_above (GTK_WINDOW(new_user->window), TRUE);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(new_user->window), "GncNewUserDialog");

    new_user->new_accounts_button = GTK_WIDGET(gtk_builder_get_object (builder, "new_accounts_button"));
    new_user->import_qif_button = GTK_WIDGET(gtk_builder_get_object (builder, "import_qif_button"));
    new_user->tutorial_button = GTK_WIDGET(gtk_builder_get_object (builder, "tutorial_button"));

    /* Set the sensitivity of the qif-import button based on the availability
     * of the qif-import assistant.
     */
    gtk_widget_set_sensitive (new_user->import_qif_button, (qifImportAssistantFcn != NULL));

    g_signal_connect(G_OBJECT(new_user->window), "destroy",
            G_CALLBACK(gnc_ui_new_user_destroy_cb), new_user);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "ok_but"));
    g_signal_connect(button, "clicked", G_CALLBACK(gnc_ui_new_user_ok_cb), new_user);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "cancel_but"));
    g_signal_connect(button, "clicked", G_CALLBACK(gnc_ui_new_user_cancel_cb), new_user);

    new_user->ok_pressed = FALSE;

    g_idle_add ((GSourceFunc)gnc_ui_new_user_window_present, GTK_WINDOW(new_user->window));

    g_object_unref(G_OBJECT(builder));
    LEAVE(" ");
}

static void
gnc_ui_new_user_cancel_dialog (GtkWindow *parent)
{
    GtkWidget *dialog;
    GtkBuilder  *builder;
    gint result;
    gboolean keepshowing;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-new-user.glade", "new_user_cancel_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "new_user_cancel_dialog"));

    gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);

    result = gtk_dialog_run (GTK_DIALOG (dialog));
    keepshowing = (result == GTK_RESPONSE_YES);

    gnc_set_first_startup (keepshowing);
    gncp_new_user_finish ();

    g_object_unref(G_OBJECT(builder));
    gtk_widget_destroy(dialog);
}

void
gncp_new_user_finish (void)
{
    gnc_hook_run(HOOK_BOOK_OPENED, gnc_get_current_session());
}

void
gnc_ui_new_user_dialog (void)
{
    GNCNewUserDialog *new_user;

    new_user = g_new0(GNCNewUserDialog, 1);
    gnc_ui_new_user_dialog_create (new_user);
    gtk_widget_show (new_user->window);
}

