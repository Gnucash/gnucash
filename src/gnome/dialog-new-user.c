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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libguile.h>

#include "dialog-new-user.h"
#include "dialog-utils.h"
#include "druid-hierarchy.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-hooks.h"
#include "gnc-ui.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-account-tree.h"
#include "gnc-session.h"

#define GCONF_SECTION "dialogs/new_user"
#define FIRST_STARTUP "first_startup"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/* function to open a qif import druid */
static void (*qifImportDruidFcn)(void) = NULL;

static void gnc_ui_new_user_cancel_dialog (void);

void
gnc_new_user_dialog_register_qif_druid (void (*cb_fcn)(void))
{
    g_return_if_fail (qifImportDruidFcn == NULL);
    qifImportDruidFcn = cb_fcn;
}

void
gnc_set_first_startup (gboolean first_startup)
{
    gnc_gconf_set_bool(GCONF_SECTION, FIRST_STARTUP, first_startup, NULL);
}

static void
after_hierarchy_druid(void)
{
    GncPluginPage *page;

    gncp_new_user_finish ();
    gnc_set_first_startup (FALSE);

    page = gnc_plugin_page_account_tree_new();
    gnc_main_window_open_page(NULL, page);
}

void
gnc_ui_new_user_dialog (void)
{
    GtkWidget *dialog;
    GtkWidget *new_accounts_button;
    GtkWidget *import_qif_button;
    GtkWidget *tutorial_button;
    GladeXML  *xml;
    gint result;

    ENTER(" ");
    xml = gnc_glade_xml_new ("newuser.glade", "New User Dialog");

    dialog = glade_xml_get_widget (xml, "New User Dialog");

    new_accounts_button = glade_xml_get_widget (xml, "new_accounts_button");
    import_qif_button = glade_xml_get_widget (xml, "import_qif_button");
    tutorial_button = glade_xml_get_widget (xml, "tutorial_button");

    /* Set the sensitivity of the qif-import button based on the availability
     * of the qif-import druid.
     */
    gtk_widget_set_sensitive (import_qif_button, (qifImportDruidFcn != NULL));

    result = gtk_dialog_run (GTK_DIALOG (dialog));
    switch (result)
    {
    case GTK_RESPONSE_CANCEL:
    case GTK_RESPONSE_DELETE_EVENT:
        gnc_ui_new_user_cancel_dialog ();
        break;
    case GTK_RESPONSE_OK:
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (new_accounts_button)))
        {
            gnc_ui_hierarchy_druid_with_callback(TRUE, after_hierarchy_druid);
            break;
        }
        else if ((qifImportDruidFcn != NULL)
                 && gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (import_qif_button)))
        {
            qifImportDruidFcn();
            gncp_new_user_finish ();
            break;
        }
        else if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (tutorial_button)))
        {
            gnc_gnome_help (HF_GUIDE, NULL);
            gncp_new_user_finish ();
            break;
        }
    default:
        g_print ("DEBUG: Response: %d", result);
        g_assert_not_reached ();
    }

    gtk_widget_destroy (dialog);
    LEAVE(" ");
}

static void
gnc_ui_new_user_cancel_dialog (void)
{
    GtkWidget *dialog;
    GladeXML  *xml;
    gint result;
    gboolean keepshowing;

    xml = gnc_glade_xml_new ("newuser.glade", "New User Cancel Dialog");

    dialog = glade_xml_get_widget (xml, "New User Cancel Dialog");

    result = gtk_dialog_run (GTK_DIALOG (dialog));
    keepshowing = (result == GTK_RESPONSE_YES);

    gnc_set_first_startup (keepshowing);
    gncp_new_user_finish ();

    gtk_widget_destroy(dialog);
}

void
gncp_new_user_finish (void)
{
    gnc_hook_run(HOOK_BOOK_OPENED, gnc_get_current_session());
}
