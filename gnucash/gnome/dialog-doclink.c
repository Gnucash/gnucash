/********************************************************************\
 * dialog-doclink.c -- Document link dialog                         *
 * Copyright (C) 2020 Robert Fewell                                 *
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

#include "dialog-doclink.h"
#include "dialog-doclink-utils.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "Transaction.h"

#include "gnc-plugin-page-invoice.h"
#include "gnc-plugin-page-register.h"
#include "gnc-main-window.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gnome-utils.h"
#include "gnc-uri-utils.h"
#include "gnc-filepath-utils.h"
#include "Account.h"
#include "dialog-invoice.h"

#define DIALOG_DOCLINK_CM_CLASS    "dialog-doclink"
#define GNC_PREFS_GROUP_BUS      "dialogs.business-doclink"
#define GNC_PREFS_GROUP_TRANS    "dialogs.trans-doclink"

/** Enumeration for the tree-store */
enum GncDoclinkColumn
{
    DATE_ITEM,
    DATE_INT64, // used just for sorting date_trans
    DESC_ID,
    DESC_ITEM,
    DISPLAY_URI,
    AVAILABLE,
    ITEM_POINTER,
    URI,
    URI_RELATIVE, // used just for sorting relative_pix
    URI_RELATIVE_PIX
};

typedef struct
{
    GtkWidget    *window;
    GtkWidget    *view;
    GtkWidget    *path_head_label;
    GtkWidget    *total_entries_label;
    gchar        *path_head;
    gboolean      is_list_trans;
    gboolean      book_ro;
    GtkTreeModel *model;
    gint          component_id;
    QofSession   *session;
}DoclinkDialog;

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

/* =================================================================== */

void
gnc_doclink_open_uri (GtkWindow *parent, const gchar *uri)
{
    if (uri && *uri)
    {
        gchar     *scheme = gnc_uri_get_scheme (uri);
        gchar  *path_head = gnc_doclink_get_path_head ();
        gchar    *run_uri = gnc_doclink_get_use_uri (path_head, uri, scheme);
        gchar *run_scheme = gnc_uri_get_scheme (run_uri);

        PINFO("Open uri scheme is '%s', uri is '%s'", run_scheme, run_uri);

        if (run_scheme) // make sure we have a scheme entry
        {
            gnc_launch_doclink (GTK_WINDOW (parent), run_uri);
            g_free (run_scheme);
        }
        g_free (run_uri);
        g_free (path_head);
        g_free (scheme);
    }
}

/* =================================================================== */

static void
location_ok_cb (GtkEntry *entry, gpointer user_data)
{
    GtkWidget *ok_button = user_data;
    gboolean have_scheme = FALSE;
    const gchar *text = gtk_entry_get_text (entry);
    GtkWidget *warning_hbox = g_object_get_data (G_OBJECT(entry), "whbox");

    if (text && *text)
    {
        gchar *scheme = gnc_uri_get_scheme (text);

        if (scheme)
            have_scheme = TRUE;
        g_free (scheme);
    }
    gtk_widget_set_visible (warning_hbox, !have_scheme);
    gtk_widget_set_sensitive (ok_button, have_scheme);
}

static void
file_ok_cb (GtkButton *button, GtkWidget *ok_button)
{
    const gchar *uri = g_object_get_data (G_OBJECT(button), "uri");
    gboolean file_true = FALSE;

    if (uri)
    {
        gchar *full_filename = gnc_uri_get_path (uri);

        /* Test for a valid filename and not a directory */
        if (full_filename && !g_file_test (full_filename, G_FILE_TEST_IS_DIR))
            file_true = TRUE;

        g_free (full_filename);
    }
    gtk_widget_set_sensitive (ok_button, file_true);
}

static void
fcb_clicked_cb (GtkButton *button, GtkWidget *ok_button)
{
    GtkWidget *dialog = gtk_widget_get_toplevel (GTK_WIDGET(button));
    GtkWidget *label = g_object_get_data (G_OBJECT(button), "fcb_label");
    const gchar *path_head = g_object_get_data (G_OBJECT(button), "path_head");
    const gchar *uri = g_object_get_data (G_OBJECT(button), "uri");
    GtkFileChooserNative *native;
    gint res;

    native = gtk_file_chooser_native_new (_("Select document"),
                                          GTK_WINDOW(dialog),
                                          GTK_FILE_CHOOSER_ACTION_OPEN,
                                          _("_OK"),
                                          _("_Cancel"));

    if (uri && *uri)
    {
        gchar *scheme = gnc_uri_get_scheme (uri);
        gchar *full_filename = gnc_doclink_get_unescape_uri (path_head, uri, scheme);
        gchar *path = g_path_get_dirname (full_filename);
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(native), path);
        g_free (full_filename);
        g_free (scheme);
        g_free (path);
    }
    else if (path_head)
        gtk_file_chooser_set_current_folder_uri (GTK_FILE_CHOOSER(native), path_head);

    res = gtk_native_dialog_run (GTK_NATIVE_DIALOG(native));
    if (res == GTK_RESPONSE_ACCEPT)
    {
        gchar *uri = gtk_file_chooser_get_uri (GTK_FILE_CHOOSER(native));

        if (uri && *uri)
        {
            gchar *filename = g_path_get_basename (uri);
            gchar *unescape_filename = g_uri_unescape_string (filename, NULL);
            gtk_label_set_text (GTK_LABEL(label), unescape_filename);

            DEBUG("Native file uri is '%s'", uri);

            g_object_set_data_full (G_OBJECT(button), "uri", g_strdup (uri), g_free);
            g_free (filename);
            g_free (unescape_filename);
        }
        g_free (uri);
        file_ok_cb (button, ok_button);
    }
    g_object_unref (native);
}

static void
uri_type_selected_cb (GtkToggleButton *button, GtkWidget *widget)
{
    GtkWidget *top = gtk_widget_get_toplevel (widget);
    GtkWidget *parent_hbox = gtk_widget_get_parent (widget);
    GtkWidget *ok_button = g_object_get_data (G_OBJECT(widget), "okbut");
    gboolean active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button));

    // set the visibility of the parent hbox for widget
    gtk_widget_set_visible (parent_hbox, active);

    // make the window resize after hiding widgets
    if (active)
    {
        if (g_strcmp0 (gtk_buildable_get_name (
                             GTK_BUILDABLE(parent_hbox)), "location_hbox") == 0)
            location_ok_cb (GTK_ENTRY (widget), ok_button);
        else
            file_ok_cb (GTK_BUTTON(widget), ok_button);

        gtk_window_resize (GTK_WINDOW(top), 600, 10); // width, height
    }
}

static void
setup_location_dialog (GtkBuilder *builder, GtkWidget *button_loc, const gchar *uri)
{
    GtkLabel *location_label = GTK_LABEL(gtk_builder_get_object (builder, "location_label"));
    GtkEntry *entry = GTK_ENTRY(gtk_builder_get_object (builder, "location_entry"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button_loc), TRUE);

    // set entry settings
    gtk_entry_set_width_chars (entry, 80);
    gtk_entry_set_activates_default (entry, TRUE);
    gtk_widget_grab_focus (GTK_WIDGET(entry));

    // update label and set entry text if required
    if (uri)
    {
        gtk_label_set_text (location_label, _("Amend the URL"));
        gtk_entry_set_text (entry, uri);
    }
    else
        {
            gchar *enter_uri = g_strdup_printf (_("Enter an URL like \"%s\""),
                                                PACKAGE_URL);
            gtk_label_set_text (location_label, enter_uri);
            g_free (enter_uri);
        }
}

static void
setup_file_dialog (GtkBuilder *builder, const gchar *path_head, const gchar *uri, gchar *scheme)
{
    GtkWidget *fcb = GTK_WIDGET(gtk_builder_get_object (builder, "file_chooser_button"));
    gchar *display_uri = gnc_doclink_get_unescape_uri (path_head, uri, scheme);

    if (display_uri)
    {
        GtkWidget *existing_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "existing_hbox"));
        GtkWidget *image = gtk_image_new_from_icon_name ("dialog-warning", GTK_ICON_SIZE_SMALL_TOOLBAR);
        gchar     *use_uri = gnc_doclink_get_use_uri (path_head, uri, scheme);
        gchar     *uri_label = g_strdup_printf ("%s \"%s\"", _("Existing Document Link is"), display_uri);
        GtkWidget *label = gtk_label_new (uri_label);

        if (g_file_test (display_uri, G_FILE_TEST_EXISTS))
            gtk_box_pack_start (GTK_BOX(existing_hbox), label, FALSE, TRUE, 0);
        else
        {
            gtk_box_pack_start (GTK_BOX(existing_hbox), image, FALSE, FALSE, 0);
            gtk_box_pack_start (GTK_BOX(existing_hbox), label, FALSE, TRUE, 0);
        }

        PINFO("Path head: '%s', URI: '%s', Filename: '%s'", path_head, uri, display_uri);

        gtk_label_set_ellipsize (GTK_LABEL(label), PANGO_ELLIPSIZE_START);

        // Set the style context for this label so it can be easily manipulated with css
        gnc_widget_style_context_add_class (GTK_WIDGET(label), "gnc-class-highlight");
        gtk_widget_show_all (existing_hbox);

        g_free (uri_label);
        g_free (use_uri);
    }
    g_object_set_data_full (G_OBJECT(fcb), "path_head", g_strdup (path_head), g_free);
    gtk_widget_grab_focus (GTK_WIDGET(fcb));
    g_free (display_uri);
}

static gboolean
gnc_doclink_get_uri_event_cb (GtkWidget *widget, GdkEventKey *event,
                              gpointer user_data)
{
    if (event->keyval == GDK_KEY_Escape)
    {
        gtk_dialog_response (GTK_DIALOG(widget),
                             GTK_RESPONSE_CANCEL);
        return TRUE;
     }
     return FALSE;
}

gchar *
gnc_doclink_get_uri_dialog (GtkWindow *parent, const gchar *title,
                            const gchar *uri)
{
    GtkWidget *dialog, *button_loc, *button_file, *ok_button, *warning_hbox;
    GtkBuilder *builder;
    gboolean uri_is_file, have_uri = FALSE;
    GtkEntry *entry;
    GtkWidget *fcb;
    GtkWidget *fcb_label;
    GtkWidget *head_label;
    int result;
    gchar *ret_uri = NULL;
    gchar *path_head = gnc_doclink_get_path_head ();
    gchar *scheme = NULL;

    /* Create the dialog box */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-doclink.glade",
                               "linked_doc_dialog");
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "linked_doc_dialog"));
    gtk_window_set_title (GTK_WINDOW(dialog), title);

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    // Set the name and style context for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-doclink");
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), "gnc-class-doclink");

    // Use this event to capture the escape key being pressed
    g_signal_connect (dialog, "key_press_event",
                      G_CALLBACK(gnc_doclink_get_uri_event_cb), dialog);

    head_label = GTK_WIDGET(gtk_builder_get_object (builder, "path_head_label"));
    ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ok_button"));

    fcb = GTK_WIDGET(gtk_builder_get_object (builder, "file_chooser_button"));
    fcb_label = GTK_WIDGET(gtk_builder_get_object (builder, "file_chooser_button_label"));
    g_object_set_data (G_OBJECT(fcb), "fcb_label", fcb_label);
    g_object_set_data (G_OBJECT(fcb), "okbut", ok_button);
    g_signal_connect (fcb, "clicked", G_CALLBACK(fcb_clicked_cb), ok_button);

    button_file = GTK_WIDGET(gtk_builder_get_object (builder, "linked_file"));
    g_signal_connect (button_file, "toggled", G_CALLBACK(uri_type_selected_cb), fcb);

    gtk_widget_show_all (GTK_WIDGET(gtk_builder_get_object (builder, "file_hbox")));

    warning_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "warning_hbox"));
    entry = GTK_ENTRY(gtk_builder_get_object (builder, "location_entry"));
    g_object_set_data (G_OBJECT(entry), "whbox", warning_hbox);
    g_object_set_data (G_OBJECT(entry), "okbut", ok_button);

    g_signal_connect (entry, "changed", G_CALLBACK(location_ok_cb), ok_button);

    button_loc = GTK_WIDGET(gtk_builder_get_object (builder, "linked_loc"));
    g_signal_connect (button_loc, "toggled", G_CALLBACK(uri_type_selected_cb), entry);

    // display path head text and test if present
    gnc_doclink_set_path_head_label (head_label, NULL, NULL);

    // Check for uri is empty or NULL
    if (uri && *uri)
    {
        scheme = gnc_uri_get_scheme (uri);
        have_uri = TRUE;

        if (!scheme || g_strcmp0 (scheme, "file") == 0) // use the correct dialog
            uri_is_file = TRUE;
        else
            uri_is_file = FALSE;
    }
    else
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button_loc), TRUE);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button_file), TRUE);
    }

    // make sure we start with the right dialog
    if (have_uri)
    {
        if (uri_is_file) // file
        {
            gchar *filename = g_path_get_basename (uri);

            g_object_set_data_full (G_OBJECT(fcb), "uri", g_strdup (uri), g_free);

            if (filename)
            {
                gchar *unescape_filename = g_uri_unescape_string (filename, NULL);
                gtk_label_set_text (GTK_LABEL(fcb_label), unescape_filename);
                g_free (unescape_filename);
                g_free (filename);
            }
            setup_file_dialog (builder, path_head, uri, scheme);
        }
        else // location
            setup_location_dialog (builder, button_loc, uri);
    }
    else
        g_object_set_data_full (G_OBJECT(fcb), "path_head", g_strdup (path_head), g_free);

    g_free (scheme);
    g_object_unref (G_OBJECT(builder));

    // run the dialog
    result = gtk_dialog_run (GTK_DIALOG(dialog));
    if (result == GTK_RESPONSE_OK) //ok button
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button_loc))) // location
        {
            const gchar *dialog_uri = gtk_entry_get_text (GTK_ENTRY(entry));

            ret_uri = g_strdup (dialog_uri);

            DEBUG("Dialog Location URI: '%s'", dialog_uri);
        }
        else // file
        {
            const gchar *dialog_uri = g_object_get_data (G_OBJECT(fcb), "uri");

            PINFO("Dialog File URI: '%s', Path head: '%s'", dialog_uri, path_head);

            // relative paths do not start with a '/'
            if (g_str_has_prefix (dialog_uri, path_head))
            {
                const gchar *part = dialog_uri + strlen (path_head);
                ret_uri = g_strdup (part);
            }
            else
                ret_uri = g_strdup (dialog_uri);

            DEBUG("Dialog File URI: '%s'", ret_uri);
        }
    }
    else if (result == GTK_RESPONSE_REJECT) // remove button
        ret_uri = g_strdup ("");
    else
        ret_uri = g_strdup (uri); // any other button

    g_free (path_head);
    gtk_widget_destroy (dialog);
    return ret_uri;
}


/* =================================================================== */


static void close_handler (gpointer user_data);

static gboolean
gnc_doclink_dialog_window_delete_event_cb (GtkWidget *widget,
                                           GdkEvent  *event,
                                           gpointer   user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    // this cb allows the window size to be saved on closing with the X
    if (doclink_dialog->is_list_trans)
        gnc_save_window_size (GNC_PREFS_GROUP_TRANS,
                              GTK_WINDOW(doclink_dialog->window));
    else
        gnc_save_window_size (GNC_PREFS_GROUP_BUS,
                              GTK_WINDOW(doclink_dialog->window));
    return FALSE;
}

static void
gnc_doclink_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component (doclink_dialog->component_id);

    if (doclink_dialog->window)
    {
        g_free (doclink_dialog->path_head);
        gtk_widget_destroy (doclink_dialog->window);
        doclink_dialog->window = NULL;
    }
    g_free (doclink_dialog);
    LEAVE(" ");
}

static gboolean
gnc_doclink_dialog_window_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                                        gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;

    if (event->keyval == GDK_KEY_Escape)
    {
        close_handler (doclink_dialog);
        return TRUE;
    }
    else
        return FALSE;
}

static void
doclink_dialog_update (DoclinkDialog *doclink_dialog)
{
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    gboolean          valid;

    /* disconnect the model from the treeview */
    model = gtk_tree_view_get_model (GTK_TREE_VIEW (doclink_dialog->view));
    g_object_ref (G_OBJECT(model));
    gtk_tree_view_set_model (GTK_TREE_VIEW (doclink_dialog->view), NULL);

    /* Get first row in list store */
    valid = gtk_tree_model_get_iter_first (model, &iter);

    while (valid)
    {
        gchar *uri;
        gchar *scheme;

        gtk_tree_model_get (model, &iter, URI, &uri, -1);

        scheme = gnc_uri_get_scheme (uri);

        if (!scheme || gnc_uri_is_file_scheme (scheme))
        {
            gchar *filename =
                gnc_doclink_get_unescape_uri (doclink_dialog->path_head,
                                               uri, scheme);

            if (g_file_test (filename, G_FILE_TEST_EXISTS))
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("File Found"), -1);
            else
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("File Not Found"), -1);

            g_free (filename);
        }
        else
        {
            gchar           *escaped = g_uri_escape_string (uri, ":/.", TRUE);
            GNetworkMonitor      *nm = g_network_monitor_get_default ();
            GSocketConnectable *conn = g_network_address_parse_uri (escaped, 80, NULL);

            if (conn)
            {
                if (g_network_monitor_can_reach (nm, conn, NULL, NULL))
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("Address Found"), -1);
                else
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("Address Not Found"), -1);
            }
            g_free (escaped);
        }
        g_free (uri);
        g_free (scheme);

        valid = gtk_tree_model_iter_next (model, &iter);
    }
    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW (doclink_dialog->view), model);
    g_object_unref (G_OBJECT(model));
}

static void
update_model_with_changes (DoclinkDialog *doclink_dialog, GtkTreeIter *iter,
                           const gchar *uri)
{
    gchar *display_uri;
    gboolean rel = FALSE;
    gchar *scheme = gnc_uri_get_scheme (uri);

    if (!scheme) // path is relative
        rel = TRUE;

    display_uri = gnc_doclink_get_unescape_uri (doclink_dialog->path_head,
                                                 uri, scheme);
    gtk_list_store_set (GTK_LIST_STORE (doclink_dialog->model), iter,
                        DISPLAY_URI, display_uri, AVAILABLE, _("File Found"),
                        URI, uri,
                        URI_RELATIVE, rel, // used just for sorting relative column
                        URI_RELATIVE_PIX, (rel == TRUE ? "emblem-default" : NULL), -1);

    if (!rel && !gnc_uri_is_file_scheme (scheme))
        gtk_list_store_set (GTK_LIST_STORE (doclink_dialog->model), iter,
                            AVAILABLE, _("Unknown"), -1);

    g_free (display_uri);
    g_free (scheme);
}

static void
update_total_entries (DoclinkDialog *doclink_dialog)
{
    gint entries =
        gtk_tree_model_iter_n_children (GTK_TREE_MODEL (doclink_dialog->model),
                                        NULL);

    if (entries > 0)
    {
        gchar *total = g_strdup_printf ("%s %d", _("Total Entries"), entries);
        gtk_label_set_text (GTK_LABEL (doclink_dialog->total_entries_label),
                            total);
        gtk_widget_show (doclink_dialog->total_entries_label);
        g_free (total);
    }
    else
        gtk_widget_hide (doclink_dialog->total_entries_label);
}

static void
row_selected_bus_cb (GtkTreeView *view, GtkTreePath *path,
                     GtkTreeViewColumn  *col, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    GtkTreeIter   iter;
    GncInvoice    *invoice;
    gchar         *uri = NULL;

    // path describes a non-existing row - should not happen
    g_return_if_fail (gtk_tree_model_get_iter (doclink_dialog->model,
                                               &iter, path));

    gtk_tree_model_get (doclink_dialog->model, &iter, URI,
                        &uri, ITEM_POINTER, &invoice, -1);

    // Open linked document, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW (doclink_dialog->view),
                                  DISPLAY_URI - 1) == col)
        gnc_doclink_open_uri (GTK_WINDOW (doclink_dialog->window), uri);

    if (!invoice)
    {
        g_free (uri);
        return;
    }

    // Open Invoice, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW (doclink_dialog->view),
                                  DESC_ID - 1) == col)
    {
        InvoiceWindow *iw;

        iw =  gnc_ui_invoice_edit (GTK_WINDOW (doclink_dialog->window),
                                   invoice);
        gnc_plugin_page_invoice_new (iw);
    }

    // Open Invoice document link dialog, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW (doclink_dialog->view),
                                  AVAILABLE - 1) == col)
    {
        gchar *ret_uri = NULL;

        if (doclink_dialog->book_ro)
        {
            gnc_warning_dialog (GTK_WINDOW (doclink_dialog->window), "%s",
                                _("Business item can not be modified."));
            g_free (uri);
            return;
        }

/* Translators: This is the title of a dialog box for linking an external
   file or URI with the current bill, invoice, transaction, or voucher. */
        ret_uri =
            gnc_doclink_get_uri_dialog (GTK_WINDOW (doclink_dialog->window),
                                        _("Manage Document Link"), uri);

        if (ret_uri && g_strcmp0 (uri, ret_uri) != 0)
        {
            gncInvoiceSetDocLink (invoice, ret_uri);

            if (g_strcmp0 (ret_uri, "") == 0) // delete uri
            {
                // update the asooc parts for invoice window if present
                gnc_invoice_update_doclink_for_window (invoice, ret_uri);
                gtk_list_store_remove (GTK_LIST_STORE (doclink_dialog->model),
                                       &iter);
                update_total_entries (doclink_dialog);
            }
            else // update uri
            {
                gchar *display_uri;
                gchar *scheme = gnc_uri_get_scheme (ret_uri);

                display_uri = gnc_doclink_get_unescape_uri (doclink_dialog->path_head, ret_uri, scheme);

                update_model_with_changes (doclink_dialog, &iter, ret_uri);

                // update the asooc parts for invoice window if present
                gnc_invoice_update_doclink_for_window (invoice, display_uri);

                g_free (scheme);
                g_free (display_uri);
            }
        }
        g_free (ret_uri);
    }
    g_free (uri);
}

static void
row_selected_trans_cb (GtkTreeView *view, GtkTreePath *path,
                       GtkTreeViewColumn  *col, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    GtkTreeIter   iter;
    Split         *split;
    gchar         *uri = NULL;

    // path describes a non-existing row - should not happen
    g_return_if_fail (gtk_tree_model_get_iter (doclink_dialog->model,
                                               &iter, path));

    gtk_tree_model_get (doclink_dialog->model, &iter, URI,
                        &uri, ITEM_POINTER, &split, -1);

    // Open linked document, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW (doclink_dialog->view),
                                  DISPLAY_URI - 1) == col)
        gnc_doclink_open_uri (GTK_WINDOW (doclink_dialog->window), uri);

    if (!split)
    {
        g_free (uri);
        return;
    }

    // Open transaction, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(doclink_dialog->view),
                                  DESC_ITEM - 1) == col)
    {
        GncPluginPage *page;
        GNCSplitReg   *gsr;
        Account       *account = xaccSplitGetAccount (split);

        page = gnc_plugin_page_register_new (account, FALSE);
        gnc_main_window_open_page (NULL, page);
        gsr = gnc_plugin_page_register_get_gsr (page);
        gnc_split_reg_raise (gsr);

        // Test for visibility of split
        if (gnc_split_reg_clear_filter_for_split (gsr, split))
            gnc_plugin_page_register_clear_current_filter (GNC_PLUGIN_PAGE(page));

        gnc_split_reg_jump_to_split (gsr, split);
    }

    // Open transaction document link dialog, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(doclink_dialog->view), AVAILABLE - 1) == col)
    {
        Transaction *trans;
        gchar       *ret_uri = NULL;

        trans = xaccSplitGetParent (split);

        if (xaccTransIsReadonlyByPostedDate (trans) ||
            xaccTransGetReadOnly (trans) ||
            doclink_dialog->book_ro)
        {
            gnc_warning_dialog (GTK_WINDOW (doclink_dialog->window), "%s",
                                _("Transaction can not be modified."));
            g_free (uri);
            return;
        }
        ret_uri =
            gnc_doclink_get_uri_dialog (GTK_WINDOW (doclink_dialog->window),
                                        _("Manage Document Link"), uri);

        if (ret_uri && g_strcmp0 (uri, ret_uri) != 0)
        {
            xaccTransSetDocLink (trans, ret_uri);
            if (g_strcmp0 (ret_uri, "") == 0) // deleted uri
            {
                gtk_list_store_remove (GTK_LIST_STORE (doclink_dialog->model),
                                       &iter);
                update_total_entries (doclink_dialog);
            }
            else // updated uri
                update_model_with_changes (doclink_dialog, &iter, ret_uri);
        }
        g_free (ret_uri);
    }
    g_free (uri);
}

static void
add_bus_info_to_model (QofInstance* data, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    GncInvoice    *invoice = GNC_INVOICE(data);
    const gchar   *uri = gncInvoiceGetDocLink (invoice);
    GtkTreeIter   iter;

    if (uri && *uri)
    {
        gchar *display_uri;
        gboolean rel = FALSE;
        gchar *scheme = gnc_uri_get_scheme (uri);
        time64 t = gncInvoiceGetDateOpened (invoice);
        gchar *inv_type;
        char datebuff[MAX_DATE_LENGTH + 1];
        memset (datebuff, 0, sizeof(datebuff));
        if (t == 0)
            t = gnc_time (NULL);
        qof_print_date_buff (datebuff, sizeof(datebuff), t);

        switch (gncInvoiceGetType (invoice))
        {
            case GNC_INVOICE_VEND_INVOICE:
            case GNC_INVOICE_VEND_CREDIT_NOTE:
                inv_type = _("Bill");
                break;
            case GNC_INVOICE_EMPL_INVOICE:
            case GNC_INVOICE_EMPL_CREDIT_NOTE:
                inv_type = _("Voucher");
                break;
            case GNC_INVOICE_CUST_INVOICE:
            case GNC_INVOICE_CUST_CREDIT_NOTE:
                inv_type = _("Invoice");
                break;
            default:
                inv_type = _("Undefined");
        }

        if (!scheme) // path is relative
            rel = TRUE;

        display_uri = gnc_doclink_get_unescape_uri (doclink_dialog->path_head,
                                                    uri, scheme);

        gtk_list_store_append (GTK_LIST_STORE (doclink_dialog->model), &iter);

        gtk_list_store_set (GTK_LIST_STORE (doclink_dialog->model), &iter,
                            DATE_ITEM, datebuff,
                            DATE_INT64, t, // used just for sorting date column
                            DESC_ID, gncInvoiceGetID (invoice),
                            DESC_ITEM, inv_type,
                            DISPLAY_URI, display_uri, AVAILABLE, _("Unknown"),
                            ITEM_POINTER, invoice, URI, uri,
                            URI_RELATIVE, rel, // used just for sorting relative column
                            URI_RELATIVE_PIX, (rel == TRUE ? "emblem-default" : NULL), -1);
        g_free (display_uri);
        g_free (scheme);
    }
}

static void
add_trans_info_to_model (QofInstance* data, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    Transaction   *trans = GNC_TRANSACTION(data);
    gchar         *uri;
    GtkTreeIter   iter;

    // fix an earlier error when storing relative paths before version 3.5
    uri = gnc_doclink_convert_trans_link_uri (trans, doclink_dialog->book_ro);

    if (uri && *uri)
    {
        Split *split = xaccTransGetSplit (trans, 0);
        gchar *scheme = gnc_uri_get_scheme (uri);
        gchar *display_uri;
        gboolean rel = FALSE;
        time64 t = xaccTransRetDatePosted (trans);
        char datebuff[MAX_DATE_LENGTH + 1];
        memset (datebuff, 0, sizeof(datebuff));
        if (t == 0)
            t = gnc_time (NULL);
        qof_print_date_buff (datebuff, MAX_DATE_LENGTH, t);
        gtk_list_store_append (GTK_LIST_STORE (doclink_dialog->model), &iter);

        if (!scheme) // path is relative
            rel = TRUE;

        display_uri = gnc_doclink_get_unescape_uri (doclink_dialog->path_head,
                                                    uri, scheme);

        gtk_list_store_set (GTK_LIST_STORE (doclink_dialog->model), &iter,
                            DATE_ITEM, datebuff,
                            DATE_INT64, t, // used just for sorting date column
                            DESC_ITEM, xaccTransGetDescription (trans),
                            DISPLAY_URI, display_uri, AVAILABLE, _("Unknown"),
                            ITEM_POINTER, split, URI, uri,
                            URI_RELATIVE, rel, // used just for sorting relative column
                            URI_RELATIVE_PIX, (rel == TRUE ? "emblem-default" : NULL), -1);
        g_free (display_uri);
        g_free (scheme);
        g_free (uri);
    }
}

static void
get_bus_info (DoclinkDialog *doclink_dialog)
{
    QofBook *book = gnc_get_current_book();

    /* disconnect the model from the treeview */
    doclink_dialog->model =
        gtk_tree_view_get_model (GTK_TREE_VIEW (doclink_dialog->view));
    g_object_ref (G_OBJECT (doclink_dialog->model));
    gtk_tree_view_set_model (GTK_TREE_VIEW (doclink_dialog->view), NULL);

    /* Clear the list store */
    gtk_list_store_clear (GTK_LIST_STORE (doclink_dialog->model));

    /* Loop through the invoices */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_INVOICE),
                            add_bus_info_to_model, doclink_dialog);

    update_total_entries (doclink_dialog);

    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW (doclink_dialog->view),
                             doclink_dialog->model);
    g_object_unref (G_OBJECT (doclink_dialog->model));
}

static void
get_trans_info (DoclinkDialog *doclink_dialog)
{
    QofBook *book = gnc_get_current_book();

    doclink_dialog->book_ro = qof_book_is_readonly (book);

    /* disconnect the model from the treeview */
    doclink_dialog->model =
        gtk_tree_view_get_model (GTK_TREE_VIEW (doclink_dialog->view));
    g_object_ref (G_OBJECT (doclink_dialog->model));
    gtk_tree_view_set_model (GTK_TREE_VIEW (doclink_dialog->view), NULL);

    /* Clear the list store */
    gtk_list_store_clear (GTK_LIST_STORE (doclink_dialog->model));

    /* Loop through the transactions */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_TRANS),
                            add_trans_info_to_model, doclink_dialog);

    update_total_entries (doclink_dialog);

    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW (doclink_dialog->view),
                             doclink_dialog->model);
    g_object_unref (G_OBJECT (doclink_dialog->model));
}

static void
gnc_doclink_dialog_reload_button_cb (GtkWidget *widget, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    gchar         *path_head = gnc_doclink_get_path_head ();

    if (g_strcmp0 (path_head, doclink_dialog->path_head) != 0)
    {
        g_free (doclink_dialog->path_head);
        doclink_dialog->path_head = g_strdup (path_head);

        // display path head text and test if present
        gnc_doclink_set_path_head_label (doclink_dialog->path_head_label,
                                          NULL, NULL);
    }
    g_free (path_head);

    if (doclink_dialog->is_list_trans)
        get_trans_info (doclink_dialog);
    else
        get_bus_info (doclink_dialog);
}

static void
gnc_doclink_dialog_reload_check_button_cb (GtkWidget *widget,
                                           gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;

    gnc_doclink_dialog_reload_button_cb (widget, user_data);
    doclink_dialog_update (doclink_dialog);
}

static void
gnc_doclink_dialog_check_button_cb (GtkWidget *widget, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    doclink_dialog_update (doclink_dialog);
}

static void
gnc_doclink_dialog_close_button_cb (GtkWidget *widget, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    gnc_close_gui_component (doclink_dialog->component_id);
}

static void
gnc_doclink_dialog_create (GtkWindow *parent, DoclinkDialog *doclink_dialog)
{
    GtkWidget         *window;
    GtkBuilder        *builder;
    GtkTreeSelection  *selection;
    GtkTreeViewColumn *expanding_column;
    GtkWidget         *button;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-doclink.glade", "list-store");
    gnc_builder_add_from_file (builder, "dialog-doclink.glade",
                               "linked_doc_window");

    window = GTK_WIDGET(gtk_builder_get_object (builder, "linked_doc_window"));
    doclink_dialog->window = window;
    doclink_dialog->session = gnc_get_current_session();

    button = GTK_WIDGET(gtk_builder_get_object (builder, "reload_button"));
        g_signal_connect (button, "clicked",
                          G_CALLBACK (gnc_doclink_dialog_reload_button_cb),
                          doclink_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "reload_and_check_button"));
        g_signal_connect (button, "clicked",
                          G_CALLBACK (gnc_doclink_dialog_reload_check_button_cb),
                          doclink_dialog);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "check_button"));
        g_signal_connect (button, "clicked",
                          G_CALLBACK (gnc_doclink_dialog_check_button_cb),
                          doclink_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
        g_signal_connect (button, "clicked",
                          G_CALLBACK (gnc_doclink_dialog_close_button_cb),
                          doclink_dialog);

    // Set the widget name and style context for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET (window), "gnc-id-transaction-doclinks");
    gnc_widget_style_context_add_class (GTK_WIDGET (window),
                                        "gnc-class-doclink");

    doclink_dialog->view =
        GTK_WIDGET (gtk_builder_get_object (builder, "treeview"));
    doclink_dialog->path_head_label =
        GTK_WIDGET (gtk_builder_get_object (builder, "path-head"));
    doclink_dialog->total_entries_label =
        GTK_WIDGET (gtk_builder_get_object (builder, "total_entries_label"));
    doclink_dialog->path_head = gnc_doclink_get_path_head ();

    // display path head text and test if present
    gnc_doclink_set_path_head_label (doclink_dialog->path_head_label, NULL, NULL);

    // Get the column we want to be the expanding column.
    expanding_column =
        GTK_TREE_VIEW_COLUMN (gtk_builder_get_object (builder, "doclink"));

    /* default sort order */
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(gtk_tree_view_get_model(
                                          GTK_TREE_VIEW (doclink_dialog->view))),
                                          DATE_INT64, GTK_SORT_ASCENDING);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (doclink_dialog->view),
                                  gnc_tree_view_get_grid_lines_pref ());

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (doclink_dialog->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

    g_signal_connect (doclink_dialog->window, "destroy",
                      G_CALLBACK (gnc_doclink_dialog_window_destroy_cb),
                      doclink_dialog);

    g_signal_connect (doclink_dialog->window, "delete-event",
                      G_CALLBACK(gnc_doclink_dialog_window_delete_event_cb), doclink_dialog);

    g_signal_connect (doclink_dialog->window, "key_press_event",
                      G_CALLBACK (gnc_doclink_dialog_window_key_press_cb),
                      doclink_dialog);

    // Setup the correct parts for each dialog
    if (doclink_dialog->is_list_trans)
    {
        GObject *desc_item_tree_column = G_OBJECT(gtk_builder_get_object (builder, "desc_item"));
        GObject *desc_id_tree_column = G_OBJECT(gtk_builder_get_object (builder, "desc_id"));

        /* Translators: This is the label of a dialog box that lists all of the
           transaction that have files or URIs linked with them. */
        gtk_window_set_title (GTK_WINDOW (window), _("Transaction Document Links"));

        gtk_tree_view_column_set_visible (GTK_TREE_VIEW_COLUMN(desc_id_tree_column), FALSE);
        gtk_tree_view_column_set_title (GTK_TREE_VIEW_COLUMN(desc_item_tree_column), _("Description"));

        g_signal_connect (doclink_dialog->view, "row-activated",
                          G_CALLBACK (row_selected_trans_cb),
                          (gpointer)doclink_dialog);
        get_trans_info (doclink_dialog);
    }
    else
    {
        GtkWidget *help_label = GTK_WIDGET(gtk_builder_get_object (builder, "help_label"));
        const gchar *item_string = N_(
            "Double click on the entry in the Id column to jump to the "
            "Business Item.\nDouble click on the entry in the Link column "
            "to open the Linked Document.\nDouble click on the entry in "
            "the Available column to modify the document link.");

        /* Translators: This is the label of a dialog box that lists all of the
           invoices, bills, and vouchers that have files or URIs linked with
           them. */
        gtk_window_set_title (GTK_WINDOW (doclink_dialog->window),
                              _("Business Document Links"));
        gtk_label_set_text (GTK_LABEL(help_label), gettext (item_string));

        g_signal_connect (doclink_dialog->view, "row-activated",
                          G_CALLBACK (row_selected_bus_cb),
                          (gpointer)doclink_dialog);
        get_bus_info (doclink_dialog);
    }

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      doclink_dialog);

    g_object_unref (G_OBJECT(builder));

    gtk_tree_view_column_set_expand (expanding_column, TRUE);
    gtk_tree_view_columns_autosize (GTK_TREE_VIEW(doclink_dialog->view));
    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;

    ENTER(" ");
    if (doclink_dialog->is_list_trans)
        gnc_save_window_size (GNC_PREFS_GROUP_TRANS,
                              GTK_WINDOW (doclink_dialog->window));
    else
        gnc_save_window_size (GNC_PREFS_GROUP_BUS,
                              GTK_WINDOW (doclink_dialog->window));
    gtk_widget_destroy (GTK_WIDGET (doclink_dialog->window));
    LEAVE(" ");
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    ENTER(" ");
    LEAVE(" ");
}

static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    gboolean is_bus = GPOINTER_TO_INT(iter_data);

    ENTER(" ");
    if (!doclink_dialog)
    {
        LEAVE("No data structure");
        return (FALSE);
    }

    // test if the dialog is the right one
    if (is_bus == doclink_dialog->is_list_trans)
        return (FALSE);

    gtk_window_present (GTK_WINDOW(doclink_dialog->window));
    LEAVE(" ");
    return (TRUE);
}

void
gnc_doclink_business_dialog (GtkWindow *parent)
{
    DoclinkDialog *doclink_dialog;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_DOCLINK_CM_CLASS,
                                   show_handler, GINT_TO_POINTER(1)))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    doclink_dialog = g_new0 (DoclinkDialog, 1);

    doclink_dialog->is_list_trans = FALSE;

    gnc_doclink_dialog_create (parent, doclink_dialog);

    doclink_dialog->component_id =
        gnc_register_gui_component (DIALOG_DOCLINK_CM_CLASS,
                                    refresh_handler, close_handler,
                                    doclink_dialog);

    gnc_gui_component_set_session (doclink_dialog->component_id,
                                   doclink_dialog->session);

    gnc_restore_window_size (GNC_PREFS_GROUP_BUS,
                             GTK_WINDOW(doclink_dialog->window), parent);
    gtk_widget_show_all (GTK_WIDGET(doclink_dialog->window));
    LEAVE(" ");
}

void
gnc_doclink_trans_dialog (GtkWindow *parent)
{
    DoclinkDialog *doclink_dialog;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_DOCLINK_CM_CLASS,
                                   show_handler, GINT_TO_POINTER(0)))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    doclink_dialog = g_new0 (DoclinkDialog, 1);
    doclink_dialog->is_list_trans = TRUE;

    gnc_doclink_dialog_create (parent, doclink_dialog);

    doclink_dialog->component_id =
        gnc_register_gui_component (DIALOG_DOCLINK_CM_CLASS,
                                    refresh_handler, close_handler,
                                    doclink_dialog);

    gnc_gui_component_set_session (doclink_dialog->component_id,
                                   doclink_dialog->session);

    gnc_restore_window_size (GNC_PREFS_GROUP_TRANS,
                             GTK_WINDOW(doclink_dialog->window), parent);
    gtk_widget_show_all (GTK_WIDGET(doclink_dialog->window));
    LEAVE(" ");
}
