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

    GtkTreeIter   iter;

    DoclinkReturn *ret_dlr;
}DoclinkDialog;


typedef struct
{
    GtkWidget    *ok_button;
    GtkWidget    *cancel_button;
    GtkWidget    *remove_button;

    GtkWidget    *entry;
    GtkWidget    *fcb;

    GtkWidget    *button_loc;
    GtkWidget    *button_file;

    GtkWidget    *file_hbox;
    GtkWidget    *location_hbox;

    gchar        *uri;

    DoclinkReturn *ret_dlr;
}DoclinkUpdate;


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
            gnc_launch_doclink (GTK_WINDOW(parent), run_uri);
            g_free (run_scheme);
        }
        g_free (run_uri);
        g_free (path_head);
        g_free (scheme);
    }
}

/* =================================================================== */

static void
get_uri_dialog_location_ok_cb (GtkEntry *entry, GtkWidget *ok_button)
{
    const gchar *text = gnc_entry_get_text (entry);
    GtkWidget *warning_hbox = g_object_get_data (G_OBJECT(entry), "whbox");
    gboolean have_scheme = FALSE;

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
get_uri_dialog_file_ok_cb (GtkButton *fcb, GtkWidget *ok_button)
{
    const gchar *uri = g_object_get_data (G_OBJECT(fcb), "uri");
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
get_uri_dialog_fcb_response_cb (GtkWidget *widget, int response, DoclinkUpdate *dlu)
{
    if (response == GTK_RESPONSE_ACCEPT)
    {
        GFile *file = gtk_file_chooser_get_file (GTK_FILE_CHOOSER(widget));
        gchar *uri = g_file_get_uri (file);

        if (uri && *uri)
        {
            GtkWidget *label = g_object_get_data (G_OBJECT(dlu->fcb), "fcb_label");
            gchar *filename = g_path_get_basename (uri);
            gchar *unescape_filename = g_uri_unescape_string (filename, NULL);
            gtk_label_set_text (GTK_LABEL(label), unescape_filename);

            DEBUG("Native file uri is '%s'", uri);

            g_object_set_data_full (G_OBJECT(dlu->fcb), "uri", g_strdup (uri), g_free);
            g_free (filename);
            g_free (unescape_filename);
        }
        g_free (uri);
        g_object_unref (file);
        get_uri_dialog_file_ok_cb (GTK_BUTTON(dlu->fcb), dlu->ok_button);
    }
    g_object_unref (widget);
}

static void
get_uri_dialog_fcb_clicked_cb (GtkButton *fcb, DoclinkUpdate *dlu)
{
    GtkRoot *window = gtk_widget_get_root (GTK_WIDGET(fcb));
    GtkWidget *label = g_object_get_data (G_OBJECT(fcb), "fcb_label");
    const gchar *path_head = g_object_get_data (G_OBJECT(fcb), "path_head");
    const gchar *uri = g_object_get_data (G_OBJECT(fcb), "uri");
    GtkFileChooserNative *native = gtk_file_chooser_native_new (
                                          _("Select document"),
                                          GTK_WINDOW(window),
                                          GTK_FILE_CHOOSER_ACTION_OPEN,
                                          _("_OK"),
                                          _("_Cancel"));

    if (uri && *uri)
    {
        gchar *scheme = gnc_uri_get_scheme (uri);
        gchar *full_filename = gnc_doclink_get_unescape_uri (path_head, uri, scheme);
        gchar *path = g_path_get_dirname (full_filename);
        GFile *file = g_file_new_for_path (path);
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(native), file, NULL);
        g_object_unref (file);
        g_free (full_filename);
        g_free (scheme);
        g_free (path);
    }
    else if (path_head)
    {
        GFile *file = g_file_new_for_uri (path_head);
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(native), file, NULL);
        g_object_unref (file);
    }
    g_signal_connect (G_OBJECT(native), "response",
                      G_CALLBACK(get_uri_dialog_fcb_response_cb), dlu);

    gtk_native_dialog_show (GTK_NATIVE_DIALOG(native));
}

static void
get_uri_dialog_uri_type_selected_cb (GtkToggleButton *button, gpointer user_data)
{
    DoclinkUpdate *dlu = user_data;
    GtkRoot *top = gtk_widget_get_root (GTK_WIDGET(button)); //button_file
    gboolean active = gtk_check_button_get_active (GTK_CHECK_BUTTON(button));

    // make the window resize after hiding widgets
    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(button)))
    {
        GtkWidget *warning_hbox = g_object_get_data (G_OBJECT(dlu->entry), "whbox");
        gtk_widget_set_visible (GTK_WIDGET(warning_hbox), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(dlu->location_hbox), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(dlu->file_hbox), TRUE);

        get_uri_dialog_file_ok_cb (GTK_BUTTON(dlu->fcb), dlu->ok_button);
    }
    else
    {
        gtk_widget_set_visible (GTK_WIDGET(dlu->location_hbox), TRUE);
        gtk_widget_set_visible (GTK_WIDGET(dlu->file_hbox), FALSE);

        get_uri_dialog_location_ok_cb (GTK_ENTRY(dlu->entry), dlu->ok_button);
    }
    gtk_window_set_default_size (GTK_WINDOW(top), 600, 10); // width, height
}

static void
get_uri_dialog_setup_location_dialog (GtkBuilder *builder, GtkWidget *button_loc, const gchar *uri)
{
    GtkLabel *location_label = GTK_LABEL(gtk_builder_get_object (builder, "location_label"));
    GtkEntry *entry = GTK_ENTRY(gtk_builder_get_object (builder, "location_entry"));
    gtk_check_button_set_active (GTK_CHECK_BUTTON(button_loc), TRUE);

    // set entry settings
    gtk_editable_set_max_width_chars (GTK_EDITABLE(entry), 80);
    gtk_entry_set_activates_default (entry, TRUE);
    gtk_widget_grab_focus (GTK_WIDGET(entry));

    // update label and set entry text if required
    if (uri)
    {
        gtk_label_set_text (location_label, _("Amend the URL"));
        gnc_entry_set_text (entry, uri);
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
get_uri_dialog_setup_file_dialog (GtkBuilder *builder, const gchar *path_head,
                                  const gchar *uri, gchar *scheme)
{
    GtkWidget *fcb = GTK_WIDGET(gtk_builder_get_object (builder, "file_chooser_button"));
    gchar *display_uri = gnc_doclink_get_unescape_uri (path_head, uri, scheme);

    if (display_uri)
    {
        GtkWidget *existing_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "existing_hbox"));
        GtkWidget *image = gtk_image_new_from_icon_name ("dialog-warning");
        gchar     *use_uri = gnc_doclink_get_use_uri (path_head, uri, scheme);
        gchar     *uri_label = g_strdup_printf ("%s \"%s\"", _("Existing Document Link is"), display_uri);
        GtkWidget *label = gtk_label_new (uri_label);

        gtk_image_set_icon_size (GTK_IMAGE(image), GTK_ICON_SIZE_NORMAL);

        if (g_file_test (display_uri, G_FILE_TEST_EXISTS))
            gtk_box_append (GTK_BOX(existing_hbox), GTK_WIDGET(label));
        else
        {
            gtk_box_append (GTK_BOX(existing_hbox), GTK_WIDGET(image));
            gtk_box_append (GTK_BOX(existing_hbox), GTK_WIDGET(label));
        }

        PINFO("Path head: '%s', URI: '%s', Filename: '%s'", path_head, uri, display_uri);

        gtk_label_set_ellipsize (GTK_LABEL(label), PANGO_ELLIPSIZE_START);

        // Set the style context for this label so it can be easily manipulated with css
        gnc_widget_style_context_add_class (GTK_WIDGET(label), "gnc-class-highlight");

        g_free (uri_label);
        g_free (use_uri);
    }
    g_object_set_data_full (G_OBJECT(fcb), "path_head", g_strdup (path_head), g_free);
    gtk_widget_grab_focus (GTK_WIDGET(fcb));
    g_free (display_uri);
}

static void
get_uri_dialog_update_reponse_cb (GtkWidget *widget, gpointer user_data)
{
    DoclinkUpdate *dlu = user_data;
    DoclinkReturn *dlr = dlu->ret_dlr;

    if (widget == dlu->ok_button)
    {
        g_free (dlr->updated_uri);
        dlr->response = GTK_RESPONSE_OK;

        if (gtk_check_button_get_active (GTK_CHECK_BUTTON(dlu->button_loc))) // location
        {
            const gchar *dialog_uri = gnc_entry_get_text (GTK_ENTRY(dlu->entry));

            dlr->updated_uri = g_strdup (dialog_uri);
            dlr->response = GTK_RESPONSE_OK;

            DEBUG("Dialog Location URI: '%s'", dlr->updated_uri);
        }
        else // file
        {
            const gchar *window_uri = g_object_get_data (G_OBJECT(dlu->fcb), "uri");
            gchar *path_head = gnc_doclink_get_path_head ();

            PINFO("Window File URI: '%s', Path head: '%s'", window_uri, path_head);

            // relative paths do not start with a '/'
            if (g_str_has_prefix (window_uri, path_head))
            {
                const gchar *part = window_uri + strlen (path_head);
                dlr->updated_uri = g_strdup (part);
            }
            else
                dlr->updated_uri = g_strdup (window_uri);

            g_free (path_head);

            DEBUG("Dialog File URI: '%s'", dlr->updated_uri);
        }
    }
    if (widget == dlu->remove_button)
    {
        g_free (dlr->updated_uri);
        dlr->updated_uri = g_strdup ("");
        dlr->response = GTK_RESPONSE_REJECT;
    }
    g_free (dlu->uri);
    g_free (dlu);

    gtk_window_destroy (GTK_WINDOW(gtk_widget_get_root (GTK_WIDGET(widget))));
}

static gboolean
get_uri_dialog_uri_event_cb (GtkEventControllerKey *key, guint keyval,
                             guint keycode, GdkModifierType state,
                             gpointer user_data)
{
    DoclinkUpdate *dlu = user_data;

    if (keyval == GDK_KEY_Escape)
    {
        GtkWidget *widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER(key));

        if (gnc_ok_to_close_window (GTK_WIDGET(widget)))
            get_uri_dialog_update_reponse_cb (dlu->cancel_button, dlu);

        return TRUE;
    }
    else
        return FALSE;
}

static gboolean
get_uri_dialog_close_request_cb (GtkWindow* window, gpointer user_data)
{
    DoclinkUpdate *dlu = user_data;

    g_free (dlu->uri);
    g_free (dlu);

    return FALSE;
}

GtkWidget *
gnc_doclink_get_uri_dialog (GtkWindow *parent, const gchar *title,
                            DoclinkReturn *dlr)
{
    GtkWidget *window, *warning_hbox;
    GtkBuilder *builder;
    gboolean uri_is_file, have_uri = FALSE;
    GtkWidget *fcb_label;
    GtkWidget *head_label;
    gchar *path_head = gnc_doclink_get_path_head ();
    gchar *scheme = NULL;

    DoclinkUpdate *dlu = g_new0 (DoclinkUpdate, 1);
    dlu->ret_dlr = dlr;

    /* Create the dialog box */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-doclink.ui", "linked_doc_uri_window");
    window = GTK_WIDGET(gtk_builder_get_object (builder, "linked_doc_uri_window"));
    gtk_window_set_title (GTK_WINDOW(window), title);

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(window), GTK_WINDOW(parent));

    gtk_window_set_modal (GTK_WINDOW(window), TRUE);

    // Set the name and style context for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(window), "gnc-id-doclink");
    gnc_widget_style_context_add_class (GTK_WIDGET(window), "gnc-class-doclink");

    // Use this event to capture the escape key being pressed
    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(window), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window), "key-pressed",
                      G_CALLBACK(get_uri_dialog_uri_event_cb), dlu);

    head_label = GTK_WIDGET(gtk_builder_get_object (builder, "path_head_label"));
    dlu->ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ok_button"));
    dlu->cancel_button = GTK_WIDGET(gtk_builder_get_object (builder, "cancel_button"));
    dlu->remove_button = GTK_WIDGET(gtk_builder_get_object (builder, "remove_button"));

    /* default to 'cancel' button */
    gtk_window_set_default_widget (GTK_WINDOW(window),
                                   GTK_WIDGET(dlu->cancel_button)); //FIXME gtk4, may not work

    dlu->fcb = GTK_WIDGET(gtk_builder_get_object (builder, "file_chooser_button"));
    fcb_label = GTK_WIDGET(gtk_builder_get_object (builder, "file_chooser_button_label"));
    g_object_set_data (G_OBJECT(dlu->fcb), "fcb_label", fcb_label);
    g_object_set_data (G_OBJECT(dlu->fcb), "okbut", dlu->ok_button);
    g_signal_connect (G_OBJECT(dlu->fcb), "clicked",
                      G_CALLBACK(get_uri_dialog_fcb_clicked_cb), dlu);

    dlu->location_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "location_hbox"));
    dlu->button_loc = GTK_WIDGET(gtk_builder_get_object (builder, "linked_loc"));

    dlu->file_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "file_hbox"));
    dlu->button_file = GTK_WIDGET(gtk_builder_get_object (builder, "linked_file"));
    g_signal_connect (G_OBJECT(dlu->button_file), "toggled",
                      G_CALLBACK(get_uri_dialog_uri_type_selected_cb), dlu);

    warning_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "warning_hbox"));
    dlu->entry = GTK_WIDGET(gtk_builder_get_object (builder, "location_entry"));
    g_object_set_data (G_OBJECT(dlu->entry), "whbox", warning_hbox);
    g_object_set_data (G_OBJECT(dlu->entry), "okbut", dlu->ok_button);

    g_signal_connect (G_OBJECT(dlu->entry), "changed",
                      G_CALLBACK(get_uri_dialog_location_ok_cb), dlu->ok_button);

    g_signal_connect (G_OBJECT(window), "close-request",
                      G_CALLBACK(get_uri_dialog_close_request_cb), dlu);

    gtk_widget_set_visible (GTK_WIDGET(window), TRUE);
    gtk_widget_set_visible (GTK_WIDGET(gtk_builder_get_object (builder, "location_hbox")), FALSE);

    g_signal_connect (G_OBJECT(dlu->ok_button), "clicked",
                      G_CALLBACK(get_uri_dialog_update_reponse_cb), dlu);
    g_signal_connect (G_OBJECT(dlu->cancel_button), "clicked",
                      G_CALLBACK(get_uri_dialog_update_reponse_cb), dlu);
    g_signal_connect (G_OBJECT(dlu->remove_button), "clicked",
                      G_CALLBACK(get_uri_dialog_update_reponse_cb), dlu);

    // display path head text and test if present
    gnc_doclink_set_path_head_label (head_label, NULL, NULL);

    // Check for uri is empty or NULL
    if (dlr->existing_uri && *dlr->existing_uri)
    {
        scheme = gnc_uri_get_scheme (dlr->existing_uri);
        have_uri = TRUE;

        if (!scheme || g_strcmp0 (scheme, "file") == 0) // use the correct dialog
            uri_is_file = TRUE;
        else
            uri_is_file = FALSE;
    }

    // make sure we start with the right dialog
    if (have_uri)
    {
        if (uri_is_file) // file
        {
            gchar *filename = g_path_get_basename (dlr->existing_uri);

            g_object_set_data_full (G_OBJECT(dlu->fcb), "uri", g_strdup (dlr->existing_uri), g_free);

            if (filename)
            {
                gchar *unescape_filename = g_uri_unescape_string (filename, NULL);
                gtk_label_set_text (GTK_LABEL(fcb_label), unescape_filename);
                g_free (unescape_filename);
                g_free (filename);
            }
            gtk_check_button_set_active (GTK_CHECK_BUTTON(dlu->button_file), TRUE);
            get_uri_dialog_setup_file_dialog (builder, path_head, dlr->existing_uri, scheme);
        }
        else // location
        {
            gtk_check_button_set_active (GTK_CHECK_BUTTON(dlu->button_loc), TRUE);
            get_uri_dialog_setup_location_dialog (builder, dlu->button_loc, dlr->existing_uri);
        }
    }
    g_object_set_data_full (G_OBJECT(dlu->fcb), "path_head", g_strdup (path_head), g_free);

    dlu->uri = g_strdup (dlr->existing_uri);
    dlr->response = GTK_RESPONSE_CANCEL; //default
    dlr->updated_uri = g_strdup (dlr->existing_uri); //default

    g_free (path_head);
    g_free (scheme);
    g_object_unref (G_OBJECT(builder));

    return window;
}


/* =================================================================== */


static void close_handler (gpointer user_data);

static void
gnc_doclink_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component (doclink_dialog->component_id);

    if (doclink_dialog->window)
    {
        g_free (doclink_dialog->path_head);
        gtk_window_destroy (GTK_WINDOW(doclink_dialog->window));
        doclink_dialog->window = NULL;
    }
    g_free (doclink_dialog);
    LEAVE(" ");
}

static gboolean
gnc_doclink_dialog_close_trans_event_cb (GtkWidget       *widget,
                                         const GdkEvent  *event,
                                         gpointer         user_data)
{
    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP_TRANS, GTK_WINDOW(widget));
    return false;
}

static gboolean
gnc_doclink_dialog_close_bus_event_cb (GtkWidget       *widget,
                                       const GdkEvent  *event,
                                       gpointer         user_data)
{
    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP_BUS, GTK_WINDOW(widget));
    return false;
}

static gboolean
gnc_doclink_dialog_window_key_press_cb (GtkEventControllerKey *key, guint keyval,
                                        guint keycode, GdkModifierType state,
                                        gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;

    if (keyval == GDK_KEY_Escape)
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
    model = gtk_tree_view_get_model (GTK_TREE_VIEW(doclink_dialog->view));
    g_object_ref (G_OBJECT(model));
    gtk_tree_view_set_model (GTK_TREE_VIEW(doclink_dialog->view), NULL);

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
    gtk_tree_view_set_model (GTK_TREE_VIEW(doclink_dialog->view), model);
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
    gtk_list_store_set (GTK_LIST_STORE(doclink_dialog->model), iter,
                        DISPLAY_URI, display_uri, AVAILABLE, _("File Found"),
                        URI, uri,
                        URI_RELATIVE, rel, // used just for sorting relative column
                        URI_RELATIVE_PIX, (rel == TRUE ? "emblem-default" : NULL), -1);

    if (!rel && !gnc_uri_is_file_scheme (scheme))
        gtk_list_store_set (GTK_LIST_STORE(doclink_dialog->model), iter,
                            AVAILABLE, _("Unknown"), -1);

    g_free (display_uri);
    g_free (scheme);
}

static void
update_total_entries (DoclinkDialog *doclink_dialog)
{
    gint entries =
        gtk_tree_model_iter_n_children (GTK_TREE_MODEL(doclink_dialog->model),
                                        NULL);

    if (entries > 0)
    {
        gchar *total = g_strdup_printf ("%s %d", _("Total Entries"), entries);
        gtk_label_set_text (GTK_LABEL(doclink_dialog->total_entries_label), total);
        gtk_widget_set_visible (GTK_WIDGET(doclink_dialog->total_entries_label), TRUE);
        g_free (total);
    }
    else
        gtk_widget_set_visible (GTK_WIDGET(doclink_dialog->total_entries_label), FALSE);
}

static void
update_bus_gui_destroy_cb (GtkWidget *object, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    DoclinkReturn *dlr = doclink_dialog->ret_dlr;
    GncInvoice *invoice = dlr->user_data;

    if (dlr->response != GTK_RESPONSE_CANCEL)
    {
        if (dlr->updated_uri && g_strcmp0 (dlr->existing_uri, dlr->updated_uri) != 0)
        {
            if (GNC_IS_INVOICE(invoice))
            {
                gncInvoiceSetDocLink (invoice, dlr->updated_uri);

                if (g_strcmp0 (dlr->updated_uri, "") == 0) // delete uri
                {
                    // update the asooc parts for invoice window if present
                    gnc_invoice_update_doclink_for_window (invoice, dlr->updated_uri);
                    gtk_list_store_remove (GTK_LIST_STORE(doclink_dialog->model),
                                           &doclink_dialog->iter);
                    update_total_entries (doclink_dialog);
                }
                else // update uri
                {
                    gchar *display_uri;
                    gchar *scheme = gnc_uri_get_scheme (dlr->updated_uri);

                    display_uri = gnc_doclink_get_unescape_uri (doclink_dialog->path_head,
                                                                dlr->updated_uri, scheme);

                    update_model_with_changes (doclink_dialog, &doclink_dialog->iter,
                                               dlr->updated_uri);

                    // update the asooc parts for invoice window if present
                    gnc_invoice_update_doclink_for_window (invoice, display_uri);

                    g_free (scheme);
                    g_free (display_uri);
                }
            }
        }
    }
    g_free (dlr->existing_uri);
    g_free (dlr->updated_uri);
    g_free (dlr);
}

static void
row_selected_bus_cb (GtkTreeView *view, GtkTreePath *path,
                     GtkTreeViewColumn  *col, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    GncInvoice    *invoice;
    gchar         *uri;

    // path describes a non-existing row - should not happen
    g_return_if_fail (gtk_tree_model_get_iter (doclink_dialog->model,
                                               &doclink_dialog->iter, path));

    gtk_tree_model_get (doclink_dialog->model, &doclink_dialog->iter, URI,
                        &uri, ITEM_POINTER, &invoice, -1);

    // Open linked document, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(doclink_dialog->view),
                                  DISPLAY_URI - 1) == col)
        gnc_doclink_open_uri (GTK_WINDOW(doclink_dialog->window), uri);

    if (!invoice)
    {
        g_free (uri);
        return;
    }

    // Open Invoice, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(doclink_dialog->view),
                                  DESC_ID - 1) == col)
    {
        InvoiceWindow *iw;

        iw =  gnc_ui_invoice_edit (GTK_WINDOW(doclink_dialog->window),
                                   invoice);
        gnc_plugin_page_invoice_new (iw);
    }

    // Open Invoice document link dialog, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(doclink_dialog->view),
                                  AVAILABLE - 1) == col)
    {
        if (doclink_dialog->book_ro)
        {
            gnc_warning_dialog (GTK_WINDOW(doclink_dialog->window), "%s",
                                _("Business item can not be modified."));
            g_free (uri);
            return;
        }

        DoclinkReturn *dlr = g_new0 (DoclinkReturn, 1);
        dlr->existing_uri = g_strdup (uri);
        dlr->updated_uri = NULL;
        dlr->user_data = invoice;

        doclink_dialog->ret_dlr = dlr;

/* Translators: This is the title of a dialog box for linking an external
   file or URI with the current bill, invoice, transaction, or voucher. */
        GtkWidget *win =
            gnc_doclink_get_uri_dialog (GTK_WINDOW(doclink_dialog->window),
                                        _("Manage Document Link"), dlr);

        g_signal_connect (G_OBJECT(win), "destroy",
                          G_CALLBACK(update_bus_gui_destroy_cb), doclink_dialog);
    }
    g_free (uri);
}

static void
update_trans_gui_destroy_cb (GtkWidget *object, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    DoclinkReturn *dlr = doclink_dialog->ret_dlr;
    Transaction *trans = dlr->user_data;

    if (dlr->response != GTK_RESPONSE_CANCEL)
    {
        if (dlr->updated_uri && g_strcmp0 (dlr->existing_uri, dlr->updated_uri) != 0)
        {
            if (GNC_IS_TRANSACTION(trans))
            {
                xaccTransSetDocLink (trans, dlr->updated_uri);
                if (g_strcmp0 (dlr->updated_uri, "") == 0) // deleted uri
                {
                    gtk_list_store_remove (GTK_LIST_STORE(doclink_dialog->model),
                                           &doclink_dialog->iter);
                    update_total_entries (doclink_dialog);
                }
                else // updated uri
                    update_model_with_changes (doclink_dialog, &doclink_dialog->iter, dlr->updated_uri);
            }
        }
    }
    g_free (dlr->existing_uri);
    g_free (dlr->updated_uri);
    g_free (dlr);
}

static void
row_selected_trans_cb (GtkTreeView *view, GtkTreePath *path,
                       GtkTreeViewColumn  *col, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    Split         *split;
    gchar         *uri;

    // path describes a non-existing row - should not happen
    g_return_if_fail (gtk_tree_model_get_iter (doclink_dialog->model,
                                               &doclink_dialog->iter, path));

    gtk_tree_model_get (doclink_dialog->model, &doclink_dialog->iter, URI,
                        &uri, ITEM_POINTER, &split, -1);

    // Open linked document, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(doclink_dialog->view),
                                  DISPLAY_URI - 1) == col)
        gnc_doclink_open_uri (GTK_WINDOW(doclink_dialog->window), uri);

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
        Transaction *trans = xaccSplitGetParent (split);

        if (xaccTransIsReadonlyByPostedDate (trans) ||
            xaccTransGetReadOnly (trans) ||
            doclink_dialog->book_ro)
        {
            gnc_warning_dialog (GTK_WINDOW(doclink_dialog->window), "%s",
                                _("Transaction can not be modified."));
            g_free (uri);
            return;
        }

        DoclinkReturn *dlr = g_new0 (DoclinkReturn, 1);
        dlr->existing_uri = g_strdup (uri);
        dlr->updated_uri = NULL;
        dlr->user_data = trans;

        doclink_dialog->ret_dlr = dlr;

        GtkWidget *win =
            gnc_doclink_get_uri_dialog (GTK_WINDOW(doclink_dialog->window),
                                        _("Manage Document Link"), dlr);

        g_signal_connect (G_OBJECT(win), "destroy",
                          G_CALLBACK(update_trans_gui_destroy_cb), doclink_dialog);
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

        gtk_list_store_append (GTK_LIST_STORE(doclink_dialog->model), &iter);

        gtk_list_store_set (GTK_LIST_STORE(doclink_dialog->model), &iter,
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

        gtk_list_store_set (GTK_LIST_STORE(doclink_dialog->model), &iter,
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
        gtk_tree_view_get_model (GTK_TREE_VIEW(doclink_dialog->view));
    g_object_ref (G_OBJECT(doclink_dialog->model));
    gtk_tree_view_set_model (GTK_TREE_VIEW(doclink_dialog->view), NULL);

    /* Clear the list store */
    gtk_list_store_clear (GTK_LIST_STORE(doclink_dialog->model));

    /* Loop through the invoices */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_INVOICE),
                            add_bus_info_to_model, doclink_dialog);

    update_total_entries (doclink_dialog);

    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW(doclink_dialog->view),
                             doclink_dialog->model);
    g_object_unref (G_OBJECT(doclink_dialog->model));
}

static void
get_trans_info (DoclinkDialog *doclink_dialog)
{
    QofBook *book = gnc_get_current_book();

    doclink_dialog->book_ro = qof_book_is_readonly (book);

    /* disconnect the model from the treeview */
    doclink_dialog->model =
        gtk_tree_view_get_model (GTK_TREE_VIEW(doclink_dialog->view));
    g_object_ref (G_OBJECT(doclink_dialog->model));
    gtk_tree_view_set_model (GTK_TREE_VIEW(doclink_dialog->view), NULL);

    /* Clear the list store */
    gtk_list_store_clear (GTK_LIST_STORE(doclink_dialog->model));

    /* Loop through the transactions */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_TRANS),
                            add_trans_info_to_model, doclink_dialog);

    update_total_entries (doclink_dialog);

    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW(doclink_dialog->view),
                             doclink_dialog->model);
    g_object_unref (G_OBJECT(doclink_dialog->model));
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
    gnc_builder_add_from_file (builder, "dialog-doclink.ui", "list-store");
    gnc_builder_add_from_file (builder, "dialog-doclink.ui", "linked_doc_window");

    window = GTK_WIDGET(gtk_builder_get_object (builder, "linked_doc_window"));
    doclink_dialog->window = window;
    doclink_dialog->session = gnc_get_current_session();

    button = GTK_WIDGET(gtk_builder_get_object (builder, "reload_button"));
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(gnc_doclink_dialog_reload_button_cb), doclink_dialog);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "reload_and_check_button"));
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(gnc_doclink_dialog_reload_check_button_cb), doclink_dialog);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "check_button"));
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(gnc_doclink_dialog_check_button_cb), doclink_dialog);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(gnc_doclink_dialog_close_button_cb), doclink_dialog);

    /* default to 'close' button */
    gtk_window_set_default_widget (GTK_WINDOW(window),
                                   GTK_WIDGET(button)); //FIXME gtk4, may not work

    // Set the widget name and style context for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(window), "gnc-id-transaction-doclinks");
    gnc_widget_style_context_add_class (GTK_WIDGET(window), "gnc-class-doclink");

    doclink_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));
    doclink_dialog->path_head_label = GTK_WIDGET(gtk_builder_get_object (builder, "path-head"));
    doclink_dialog->total_entries_label = GTK_WIDGET(gtk_builder_get_object (builder, "total_entries_label"));
    doclink_dialog->path_head = gnc_doclink_get_path_head ();

    // display path head text and test if present
    gnc_doclink_set_path_head_label (doclink_dialog->path_head_label, NULL, NULL);

    // Get the column we want to be the expanding column.
    expanding_column = GTK_TREE_VIEW_COLUMN(gtk_builder_get_object (builder, "doclink"));

    /* default sort order */
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(gtk_tree_view_get_model(
                                          GTK_TREE_VIEW(doclink_dialog->view))),
                                          DATE_INT64, GTK_SORT_ASCENDING);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (doclink_dialog->view),
                                  gnc_tree_view_get_grid_lines_pref ());

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(doclink_dialog->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

    g_signal_connect (G_OBJECT(doclink_dialog->window), "destroy",
                      G_CALLBACK(gnc_doclink_dialog_window_destroy_cb), doclink_dialog);

    if (doclink_dialog->is_list_trans)
        g_signal_connect (G_OBJECT(doclink_dialog->window), "close-request",
                          G_CALLBACK(gnc_doclink_dialog_close_trans_event_cb), NULL);
    else
        g_signal_connect (G_OBJECT(doclink_dialog->window), "close-request",
                          G_CALLBACK(gnc_doclink_dialog_close_bus_event_cb), NULL);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(doclink_dialog->window), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window), "key-pressed",
                      G_CALLBACK(gnc_doclink_dialog_window_key_press_cb), doclink_dialog);

    // Setup the correct parts for each dialog
    if (doclink_dialog->is_list_trans)
    {
        GObject *desc_item_tree_column = G_OBJECT(gtk_builder_get_object (builder, "desc_item"));
        GObject *desc_id_tree_column = G_OBJECT(gtk_builder_get_object (builder, "desc_id"));

        /* Translators: This is the label of a dialog box that lists all of the
           transaction that have files or URIs linked with them. */
        gtk_window_set_title (GTK_WINDOW(window), _("Transaction Document Links"));

        gtk_tree_view_column_set_visible (GTK_TREE_VIEW_COLUMN(desc_id_tree_column), FALSE);
        gtk_tree_view_column_set_title (GTK_TREE_VIEW_COLUMN(desc_item_tree_column), _("Description"));

        g_signal_connect (G_OBJECT(doclink_dialog->view), "row-activated",
                          G_CALLBACK(row_selected_trans_cb), doclink_dialog);
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
        gtk_window_set_title (GTK_WINDOW(doclink_dialog->window),
                              _("Business Document Links"));
        gtk_label_set_text (GTK_LABEL(help_label), gettext (item_string));

        g_signal_connect (G_OBJECT(doclink_dialog->view), "row-activated",
                          G_CALLBACK(row_selected_bus_cb), doclink_dialog);
        get_bus_info (doclink_dialog);
    }

    gtk_widget_set_visible (GTK_WIDGET(doclink_dialog->window), TRUE);

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
                              GTK_WINDOW(doclink_dialog->window));
    else
        gnc_save_window_size (GNC_PREFS_GROUP_BUS,
                              GTK_WINDOW(doclink_dialog->window));
    gtk_window_destroy (GTK_WINDOW(doclink_dialog->window));
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

    LEAVE(" ");
}
