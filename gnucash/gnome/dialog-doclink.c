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
#include "dialog-doclink-view.h"

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
#include "gnc-file.h"
#include "Account.h"
#include "dialog-invoice.h"

#define DIALOG_DOCLINK_CM_CLASS  "dialog-doclink"
#define GNC_PREFS_GROUP_BUS      "dialogs.business-doclink"
#define GNC_PREFS_GROUP_TRANS    "dialogs.trans-doclink"
#define GNC_DOCLINK_UPDATE_DATA  "gnc-doclink-update"

typedef struct
{
    GtkWidget    *window;
    GtkWidget    *path_head_label;
    GtkWidget    *total_entries_label;
    gchar        *path_head;
    gboolean      is_list_trans;
    gboolean      book_ro;
    gint          component_id;
    QofSession   *session;

    GtkWidget    *column_view;
    GListModel   *column_model;
    guint         column_model_position;
    GWeakRef      update_window;

    DoclinkReturn *ret_dlr;
}DoclinkDialog;
typedef struct
{
    GWeakRef dialog_window;
    GWeakRef page;
    GWeakRef page_window;
    QofBook *book;
    GncGUID split_guid;
    GncGUID transaction_guid;
} DoclinkRegisterRevealRequest;

static void
doclink_register_reveal_request_free (gpointer user_data)
{
    DoclinkRegisterRevealRequest *request = user_data;

    g_weak_ref_clear (&request->page_window);
    g_weak_ref_clear (&request->page);
    g_weak_ref_clear (&request->dialog_window);
    g_free (request);
}

static void
doclink_register_reveal_finished (GNCSplitReg *gsr, Split *split,
                                  GncSplitRegRevealResult result,
                                  gpointer user_data)
{
    DoclinkRegisterRevealRequest *request = user_data;
    GObject *dialog_window = g_weak_ref_get (&request->dialog_window);
    GObject *page_object = g_weak_ref_get (&request->page);
    GObject *page_window = g_weak_ref_get (&request->page_window);
    GncPluginPage *page;
    Transaction *transaction;

    if (!dialog_window || !page_object || !page_window ||
        !GTK_IS_WINDOW (dialog_window) ||
        !GNC_IS_PLUGIN_PAGE_REGISTER (page_object) ||
        !GTK_IS_WINDOW (page_window) ||
        !gtk_widget_get_visible (GTK_WIDGET (dialog_window)) ||
        request->book != gnc_get_current_book () ||
        qof_book_shutting_down (request->book))
        goto out;

    page = GNC_PLUGIN_PAGE (page_object);
    transaction = xaccTransLookup (&request->transaction_guid, request->book);
    if (gnc_plugin_page_get_window (page) != GTK_WIDGET (page_window) ||
        gnc_plugin_page_register_get_gsr (page) != gsr ||
        xaccSplitLookup (&request->split_guid, request->book) != split ||
        !transaction || xaccSplitGetParent (split) != transaction)
        goto out;

    if (result == GNC_SPLIT_REG_REVEAL_FILTER_CLEARED)
        gnc_plugin_page_register_clear_current_filter (page);
    gnc_split_reg_jump_to_split (gsr, split);

out:
    g_clear_object (&page_window);
    g_clear_object (&page_object);
    g_clear_object (&dialog_window);
}

static void
doclink_register_reveal_split_async (DoclinkDialog *dialog,
                                     GncPluginPage *page, GNCSplitReg *gsr,
                                     Split *split)
{
    DoclinkRegisterRevealRequest *request;
    GtkWidget *page_window;
    Transaction *transaction;

    if (!dialog || !dialog->window || !page || !gsr || !split ||
        !(transaction = xaccSplitGetParent (split)) ||
        !(page_window = gnc_plugin_page_get_window (page)))
        return;

    request = g_new0 (DoclinkRegisterRevealRequest, 1);
    request->book = gnc_get_current_book ();
    request->split_guid = *xaccSplitGetGUID (split);
    request->transaction_guid = *xaccTransGetGUID (transaction);
    g_weak_ref_init (&request->dialog_window, G_OBJECT (dialog->window));
    g_weak_ref_init (&request->page, G_OBJECT (page));
    g_weak_ref_init (&request->page_window, G_OBJECT (page_window));
    gnc_split_reg_reveal_split_async
        (gsr, split, doclink_register_reveal_finished, request,
         doclink_register_reveal_request_free);
}

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

static void
doclink_update_free (DoclinkUpdate *dlu)
{
    if (!dlu)
        return;

    g_free (dlu->uri);
    g_free (dlu);
}

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

typedef struct
{
    GWeakRef file_button;
    GWeakRef ok_button;
} DoclinkFileDialogData;

static void
get_uri_dialog_file_dialog_data_free (DoclinkFileDialogData *data)
{
    g_weak_ref_clear (&data->file_button);
    g_weak_ref_clear (&data->ok_button);
    g_free (data);
}

static GtkWindow *
get_uri_dialog_file_dialog_parent (GtkWidget *widget)
{
    GtkRoot *root = gtk_widget_get_root (widget);

    return GTK_IS_WINDOW (root) ? GTK_WINDOW (root) : NULL;
}

static void
get_uri_dialog_file_dialog_finished (GObject *source, GAsyncResult *result,
                                     gpointer user_data)
{
    DoclinkFileDialogData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file;
    GtkWidget *file_button;
    GtkWidget *ok_button;

    file = gnc_file_dialog_request_finish (request, result, &error);
    file_button = g_weak_ref_get (&data->file_button);
    ok_button = g_weak_ref_get (&data->ok_button);
    if (file && file_button)
    {
        gchar *uri = g_file_get_uri (file);

        if (uri && *uri)
        {
            GtkWidget *label = g_object_get_data (G_OBJECT (file_button),
                                                  "fcb_label");
            gchar *filename = g_path_get_basename (uri);
            gchar *unescape_filename = g_uri_unescape_string (filename, NULL);

            if (label)
                gtk_label_set_text (GTK_LABEL (label), unescape_filename);
            DEBUG ("Native file uri is '%s'", uri);
            g_object_set_data_full (G_OBJECT (file_button), "uri",
                                    g_strdup (uri), g_free);
            g_free (filename);
            g_free (unescape_filename);
            if (ok_button)
                get_uri_dialog_file_ok_cb (GTK_BUTTON (file_button), ok_button);
        }
        g_free (uri);
    }
    else if (file_button && error &&
             !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        gnc_error_dialog (get_uri_dialog_file_dialog_parent (file_button), "%s",
                          error->message);

    g_clear_object (&file);
    g_clear_error (&error);
    g_clear_object (&file_button);
    g_clear_object (&ok_button);
    get_uri_dialog_file_dialog_data_free (data);
}

static void
get_uri_dialog_fcb_clicked_cb (GtkButton *fcb, DoclinkUpdate *dlu)
{
    const gchar *path_head = g_object_get_data (G_OBJECT (fcb), "path_head");
    const gchar *uri = g_object_get_data (G_OBJECT (fcb), "uri");
    GtkWindow *parent = get_uri_dialog_file_dialog_parent (GTK_WIDGET (fcb));
    GFile *initial_folder = NULL;
    GncFileDialogRequest *request;
    DoclinkFileDialogData *data;

    if (uri && *uri)
    {
        gchar *scheme = gnc_uri_get_scheme (uri);
        gchar *full_filename = gnc_doclink_get_unescape_uri (path_head, uri,
                                                              scheme);
        gchar *path = full_filename ? g_path_get_dirname (full_filename) : NULL;

        if (path)
            initial_folder = g_file_new_for_path (path);
        g_free (full_filename);
        g_free (scheme);
        g_free (path);
    }
    else if (path_head)
        initial_folder = g_file_new_for_uri (path_head);

    data = g_new0 (DoclinkFileDialogData, 1);
    g_weak_ref_init (&data->file_button, fcb);
    g_weak_ref_init (&data->ok_button, dlu->ok_button);
    request = gnc_file_dialog_request_new_for_folder (
        parent, _("Select document"), NULL, initial_folder,
        GNC_FILE_DIALOG_OPEN);
    g_clear_object (&initial_folder);
    if (!request)
    {
        get_uri_dialog_file_dialog_data_free (data);
        return;
    }

    gnc_file_dialog_request_open_async (request, NULL,
                                        get_uri_dialog_file_dialog_finished,
                                        data);
    g_object_unref (request);
}
static void
get_uri_dialog_uri_type_selected_cb (GtkCheckButton *button, gpointer user_data)
{
    DoclinkUpdate *dlu = user_data;
    GtkRoot *top = gtk_widget_get_root (GTK_WIDGET(button)); //button_file

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
    GtkRoot *root = gtk_widget_get_root (GTK_WIDGET (widget));
    GtkWindow *window = GTK_IS_WINDOW (root) ? GTK_WINDOW (root) : NULL;
    DoclinkReturn *dlr;

    if (!window || g_object_steal_data (G_OBJECT (window),
                                        GNC_DOCLINK_UPDATE_DATA) != dlu)
        return;

    dlr = dlu->ret_dlr;
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

    doclink_update_free (dlu);
    gtk_window_destroy (window);
}

static void
get_uri_dialog_close_confirmed (GtkWindow *window, gboolean close_allowed,
                                gpointer user_data)
{
    DoclinkUpdate *dlu;

    (void)user_data;
    if (!close_allowed || !window)
        return;

    dlu = g_object_get_data (G_OBJECT (window), GNC_DOCLINK_UPDATE_DATA);
    if (dlu)
        get_uri_dialog_update_reponse_cb (dlu->cancel_button, dlu);
}

static gboolean
get_uri_dialog_uri_event_cb (GtkEventControllerKey *key, guint keyval,
                             guint keycode, GdkModifierType state,
                             gpointer user_data)
{
    GtkWidget *widget;

    (void)keycode;
    (void)state;
    (void)user_data;
    if (keyval != GDK_KEY_Escape)
        return FALSE;

    widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (key));
    if (GTK_IS_WINDOW (widget))
        gnc_ok_to_close_window_async (GTK_WINDOW (widget),
                                      get_uri_dialog_close_confirmed, NULL);
    return TRUE;
}

static gboolean
get_uri_dialog_close_request_cb (GtkWindow *window, gpointer user_data)
{
    (void)user_data;
    gnc_ok_to_close_window_async (window, get_uri_dialog_close_confirmed, NULL);
    return TRUE;
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
    g_object_set_data_full (G_OBJECT (window), GNC_DOCLINK_UPDATE_DATA, dlu,
                            (GDestroyNotify)doclink_update_free);
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
                      G_CALLBACK(get_uri_dialog_uri_event_cb), NULL);

    head_label = GTK_WIDGET(gtk_builder_get_object (builder, "path_head_label"));
    dlu->ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ok_button"));
    dlu->cancel_button = GTK_WIDGET(gtk_builder_get_object (builder, "cancel_button"));
    dlu->remove_button = GTK_WIDGET(gtk_builder_get_object (builder, "remove_button"));

    /* default to 'cancel' button */
    gtk_window_set_default_widget (GTK_WINDOW(window),
                                   GTK_WIDGET(dlu->cancel_button));

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
                      G_CALLBACK(get_uri_dialog_close_request_cb), NULL);

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
gnc_doclink_dialog_window_destroy_cb (G_GNUC_UNUSED GtkWidget *object,
                                      gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    GtkWidget *update_window;

    ENTER(" ");
    update_window = g_weak_ref_get (&doclink_dialog->update_window);
    if (update_window)
    {
        gtk_window_destroy (GTK_WINDOW (update_window));
        g_object_unref (update_window);
    }
    g_weak_ref_clear (&doclink_dialog->update_window);

    if (doclink_dialog->component_id != NO_COMPONENT)
        gnc_unregister_gui_component (doclink_dialog->component_id);

    g_clear_pointer (&doclink_dialog->path_head, g_free);
    g_clear_object (&doclink_dialog->column_model);
    doclink_dialog->window = NULL;
    g_free (doclink_dialog);
    LEAVE(" ");
}

static gboolean
gnc_doclink_dialog_close_request_cb (GtkWindow *window, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    const gchar *prefs_group;

    g_return_val_if_fail (doclink_dialog, FALSE);
    g_return_val_if_fail (doclink_dialog->window == GTK_WIDGET (window), FALSE);

    prefs_group = doclink_dialog->is_list_trans ? GNC_PREFS_GROUP_TRANS
                                                 : GNC_PREFS_GROUP_BUS;
    gnc_save_window_size (prefs_group, window);
    return FALSE;
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
    /* disconnect the model from the column view */
    GtkSelectionModel *selection_model = gtk_column_view_get_model (GTK_COLUMN_VIEW(
                                            doclink_dialog->column_view));
    g_object_ref (selection_model);

    gtk_column_view_set_model (GTK_COLUMN_VIEW(doclink_dialog->column_view), NULL);

    guint n_items = g_list_model_get_n_items (doclink_dialog->column_model);

    for (gint i = 0; i < n_items; i++)
    {
        DoclinkViewItem *item = g_list_model_get_item (G_LIST_MODEL(
                                                       doclink_dialog->column_model), i);

        if (item)
        {
            gchar *scheme = gnc_uri_get_scheme (item->uri);

            if (!scheme || gnc_uri_is_file_scheme (scheme))
            {
                gchar *filename = gnc_doclink_get_unescape_uri (
                                      doclink_dialog->path_head,
                                      item->uri, scheme);

                g_free (item->available);
                if (g_file_test (filename, G_FILE_TEST_EXISTS))
                    item->available = g_strdup (_("File Found"));
                else
                    item->available = g_strdup (_("File Not Found"));

                g_free (filename);
            }
            else
            {
                gchar           *escaped = g_uri_escape_string (item->uri, ":/.", TRUE);
                GNetworkMonitor      *nm = g_network_monitor_get_default ();
                GSocketConnectable *conn = g_network_address_parse_uri (escaped, 80, NULL);

                if (conn)
                {
                    g_free (item->available);
                    if (g_network_monitor_can_reach (nm, conn, NULL, NULL))
                        item->available = g_strdup (_("Address Found"));
                    else
                        item->available = g_strdup (_("Address Not Found"));
                }
                g_free (escaped);
            }
            g_free (scheme);
        }
        g_clear_object (&item);
    }
    /* reconnect the model to the column view */
    gtk_column_view_set_model (GTK_COLUMN_VIEW(doclink_dialog->column_view),
                               GTK_SELECTION_MODEL(selection_model));
    g_object_unref (selection_model);
}

static void
update_model_with_changes (DoclinkDialog *doclink_dialog, const gchar *uri)
{
    gchar *display_uri;
    gboolean rel = FALSE;
    gchar *scheme = gnc_uri_get_scheme (uri);

    if (!scheme) // path is relative
        rel = TRUE;

    display_uri = gnc_doclink_get_unescape_uri (doclink_dialog->path_head,
                                                 uri, scheme);

    DoclinkViewItem *item = g_list_model_get_item (G_LIST_MODEL(doclink_dialog->column_model),
                                                   doclink_dialog->column_model_position);
    if (item)
    {
        g_free (item->display_uri);
        item->display_uri = g_strdup (display_uri);

        g_free (item->available);
        item->available = g_strdup (_("File Found"));

        g_free (item->uri);
        item->uri = g_strdup (uri);

        item->uri_relative = FALSE;

        g_free (item->uri_relative_pix);
        item->uri_relative_pix = (rel == TRUE ? g_strdup ("emblem-default") : NULL);

        if (!rel && !gnc_uri_is_file_scheme (scheme))
        {
            g_free (item->available);
            item->available = g_strdup (_("Unknown"));
        }
        g_list_model_items_changed (G_LIST_MODEL(doclink_dialog->column_model),
                                    doclink_dialog->column_model_position, 1, 1);
    }
    g_clear_object (&item);
    g_free (display_uri);
    g_free (scheme);
}

static void
update_total_entries (DoclinkDialog *doclink_dialog)
{
    gint entries = g_list_model_get_n_items (doclink_dialog->column_model);

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

                    g_list_store_remove (G_LIST_STORE(doclink_dialog->column_model),
                                                      doclink_dialog->column_model_position);

                    update_total_entries (doclink_dialog);
                }
                else // update uri
                {
                    gchar *display_uri;
                    gchar *scheme = gnc_uri_get_scheme (dlr->updated_uri);

                    display_uri = gnc_doclink_get_unescape_uri (doclink_dialog->path_head,
                                                                dlr->updated_uri, scheme);

                    update_model_with_changes (doclink_dialog, dlr->updated_uri);

                    // update the asooc parts for invoice window if present
                    gnc_invoice_update_doclink_for_window (invoice, display_uri);

                    g_free (scheme);
                    g_free (display_uri);
                }
            }
        }
    }
    if (doclink_dialog->ret_dlr == dlr)
        doclink_dialog->ret_dlr = NULL;
    g_weak_ref_set (&doclink_dialog->update_window, NULL);
    g_free (dlr->existing_uri);
    g_free (dlr->updated_uri);
    g_free (dlr);
}

static gboolean
row_selected_bus_cb (GtkGestureClick *gesture, int n_press,
                     double x, double y, gpointer user_data)
{
    DoclinkDialog   *doclink_dialog = user_data;
    DoclinkViewItem *item;
    gint             prop_position = 0;

    if (n_press == 2)
    {
        GtkWidget *widget = gtk_widget_pick (doclink_dialog->column_view, x, y, GTK_PICK_DEFAULT);

        if (widget)
            prop_position = GPOINTER_TO_INT(g_object_get_data (G_OBJECT(widget), "prop_position"));
    }
    else
        return FALSE;

    if (prop_position == 0)
        return FALSE;

    item = gnc_doclink_get_selected_item (doclink_dialog->column_view);
    if (!item)
        return FALSE;

    // Open linked document
    if (prop_position == PROP_DOCLINK_DISPLAY_URI)
        gnc_doclink_open_uri (GTK_WINDOW(doclink_dialog->window), item->uri);

    if (!item->item_pointer) // invoice
    {
        g_clear_object (&item);
        return FALSE;
    }

    // Open Invoice
    if (prop_position == PROP_DOCLINK_DESCRIPTION)
    {
        GncInvoice    *invoice = GNC_INVOICE(item->item_pointer);
        InvoiceWindow *iw;

        iw =  gnc_ui_invoice_edit (GTK_WINDOW(doclink_dialog->window),
                                   invoice);
        gnc_plugin_page_invoice_new (iw);
    }

    // Open Invoice document link dialog
    if (prop_position == PROP_DOCLINK_AVAILABLE)
    {
        GncInvoice *invoice = GNC_INVOICE(item->item_pointer);

        if (doclink_dialog->book_ro)
        {
            gnc_warning_dialog (GTK_WINDOW(doclink_dialog->window), "%s",
                                _("Business item can not be modified."));
            g_clear_object (&item);
            return FALSE;
        }
        // not sure this is right, translate the select/sort position to real position
        guint n_items = g_list_model_get_n_items (G_LIST_MODEL(doclink_dialog->column_model));
        for (gint i = 0; i < n_items; i++)
        {
            DoclinkViewItem *test_item = g_list_model_get_item (G_LIST_MODEL(
                                                                doclink_dialog->column_model), i);
            if (test_item == item)
            {
                doclink_dialog->column_model_position = i;
                break;
            }
            g_clear_object (&test_item);
        }

        DoclinkReturn *dlr = g_new0 (DoclinkReturn, 1);
        dlr->existing_uri = g_strdup (item->uri);
        dlr->updated_uri = NULL;
        dlr->user_data = invoice;

        doclink_dialog->ret_dlr = dlr;

        /* Translators: This is the title of a dialog box for linking an external
           file or URI with the current bill, invoice, transaction, or voucher. */
        GtkWidget *win =
            gnc_doclink_get_uri_dialog (GTK_WINDOW(doclink_dialog->window),
                                        _("Manage Document Link"), dlr);

        g_weak_ref_set (&doclink_dialog->update_window, win);
        g_signal_connect (G_OBJECT(win), "destroy",
                          G_CALLBACK(update_bus_gui_destroy_cb), doclink_dialog);
    }
    g_clear_object (&item);
    return TRUE;
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
                    g_list_store_remove (G_LIST_STORE(doclink_dialog->column_model),
                                                      doclink_dialog->column_model_position);

                    update_total_entries (doclink_dialog);
                }
                else // updated uri
                    update_model_with_changes (doclink_dialog, dlr->updated_uri);
            }
        }
    }
    if (doclink_dialog->ret_dlr == dlr)
        doclink_dialog->ret_dlr = NULL;
    g_weak_ref_set (&doclink_dialog->update_window, NULL);
    g_free (dlr->existing_uri);
    g_free (dlr->updated_uri);
    g_free (dlr);
}

static gboolean
row_selected_trans_cb (GtkGestureClick *gesture, int n_press,
                       double x, double y, gpointer user_data)
{
    DoclinkDialog   *doclink_dialog = user_data;
    DoclinkViewItem *item;
    gint             prop_position = 0;

    if (n_press == 2)
    {
        GtkWidget *widget = gtk_widget_pick (doclink_dialog->column_view, x, y, GTK_PICK_DEFAULT);

        if (widget)
            prop_position = GPOINTER_TO_INT(g_object_get_data (G_OBJECT(widget), "prop-position"));
    }
    else
        return FALSE;

    if (prop_position == 0)
        return FALSE;

    item = gnc_doclink_get_selected_item (doclink_dialog->column_view);
    if (!item)
        return FALSE;

    // Open linked document
    if (prop_position == PROP_DOCLINK_DISPLAY_URI)
        gnc_doclink_open_uri (GTK_WINDOW(doclink_dialog->window), item->uri);

    if (!item->item_pointer) // split
    {
        g_clear_object (&item);
        return FALSE;
    }

    // Open transaction, subtract 1 to allow for date_int64
    if (prop_position == PROP_DOCLINK_DESCRIPTION)
    {
        GncPluginPage *page;
        GNCSplitReg   *gsr;
        Split         *split = GNC_SPLIT(item->item_pointer);
        Account       *account = xaccSplitGetAccount (split);

        page = gnc_plugin_page_register_new (account, FALSE);
        gnc_main_window_open_page (NULL, page);
        gsr = gnc_plugin_page_register_get_gsr (page);
        gnc_split_reg_raise (gsr);

        doclink_register_reveal_split_async (doclink_dialog, page, gsr, split);
    }

    // Open transaction document link dialog
    if (prop_position == PROP_DOCLINK_AVAILABLE)
    {
        Split       *split = GNC_SPLIT(item->item_pointer);
        Transaction *trans = xaccSplitGetParent (split);

        if (xaccTransIsReadonlyByPostedDate (trans) ||
            xaccTransGetReadOnly (trans) ||
            doclink_dialog->book_ro)
        {
            gnc_warning_dialog (GTK_WINDOW(doclink_dialog->window), "%s",
                                _("Transaction can not be modified."));
            g_clear_object (&item);
            return FALSE;
        }
        // not sure this is right, translate the select/sort position to real position
        guint n_items = g_list_model_get_n_items (G_LIST_MODEL(doclink_dialog->column_model));
        for (gint i = 0; i < n_items; i++)
        {
            DoclinkViewItem *test_item = g_list_model_get_item (G_LIST_MODEL(
                                                                doclink_dialog->column_model), i);
            if (test_item == item)
            {
                doclink_dialog->column_model_position = i;
                break;
            }
            g_clear_object (&test_item);
        }

        DoclinkReturn *dlr = g_new0 (DoclinkReturn, 1);
        dlr->existing_uri = g_strdup (item->uri);
        dlr->updated_uri = NULL;
        dlr->user_data = trans;

        doclink_dialog->ret_dlr = dlr;

        GtkWidget *win =
            gnc_doclink_get_uri_dialog (GTK_WINDOW(doclink_dialog->window),
                                        _("Manage Document Link"), dlr);

        g_weak_ref_set (&doclink_dialog->update_window, win);
        g_signal_connect (G_OBJECT(win), "destroy",
                          G_CALLBACK(update_trans_gui_destroy_cb), doclink_dialog);
    }
    g_clear_object (&item);
    return TRUE;
}

static void
add_bus_info_to_model (QofInstance* data, gpointer user_data)
{
    DoclinkDialog *doclink_dialog = user_data;
    GncInvoice    *invoice = GNC_INVOICE(data);
    const gchar   *uri = gncInvoiceGetDocLink (invoice);

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

        DoclinkViewItem *item = g_object_new (DOCLINKVIEW_TYPE_ITEM, NULL);

        item->item_date = g_strdup (datebuff);
        item->item_time64 = t;
        item->invoice_id = g_strdup (gncInvoiceGetID (invoice)),
        item->description = g_strdup (inv_type);
        item->display_uri = g_strdup (display_uri);
        item->available = g_strdup (_("Unknown"));
        item->item_pointer = invoice;
        item->uri = g_strdup (uri);
        item->uri_relative = rel;
        item->uri_relative_pix = (rel == TRUE ? g_strdup ("emblem-default") : NULL);

        g_list_store_append (G_LIST_STORE(doclink_dialog->column_model), item);
        g_object_unref (item);

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

        if (!scheme) // path is relative
            rel = TRUE;

        display_uri = gnc_doclink_get_unescape_uri (doclink_dialog->path_head,
                                                    uri, scheme);

        DoclinkViewItem *item = g_object_new (DOCLINKVIEW_TYPE_ITEM, NULL);

        item->item_date = g_strdup (datebuff);
        item->item_time64 = t;
        item->invoice_id = NULL;
        item->description = g_strdup (xaccTransGetDescription (trans));
        item->display_uri = g_strdup (display_uri);
        item->available = g_strdup (_("Unknown"));
        item->item_pointer = split;
        item->uri = g_strdup (uri);
        item->uri_relative = rel;
        item->uri_relative_pix = (rel == TRUE ? g_strdup ("emblem-default") : NULL);

        g_list_store_append (G_LIST_STORE(doclink_dialog->column_model), item);
        g_object_unref (item);

        g_free (display_uri);
        g_free (scheme);
        g_free (uri);
    }
}

static void
get_bus_info (DoclinkDialog *doclink_dialog)
{
    QofBook *book = gnc_get_current_book();

    doclink_dialog->book_ro = qof_book_is_readonly (book);

    /* disconnect the model from the column view */
    GtkSelectionModel *selection_model = gtk_column_view_get_model (GTK_COLUMN_VIEW(
                                            doclink_dialog->column_view));
    g_object_ref (selection_model);

    gtk_column_view_set_model (GTK_COLUMN_VIEW(doclink_dialog->column_view), NULL);

    g_list_store_remove_all (G_LIST_STORE(doclink_dialog->column_model));

    /* Loop through the transactions */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_INVOICE),
                            add_bus_info_to_model, doclink_dialog);

    /* reconnect the model to the column view */
    gtk_column_view_set_model (GTK_COLUMN_VIEW(doclink_dialog->column_view),
                               GTK_SELECTION_MODEL(selection_model));
    g_object_unref (selection_model);

    gtk_single_selection_set_selected (GTK_SINGLE_SELECTION(selection_model), 0);

    update_total_entries (doclink_dialog);
}

static void
get_trans_info (DoclinkDialog *doclink_dialog)
{
    QofBook *book = gnc_get_current_book();

    doclink_dialog->book_ro = qof_book_is_readonly (book);

    /* disconnect the model from the column view */
    GtkSelectionModel *selection_model = gtk_column_view_get_model (GTK_COLUMN_VIEW(
                                            doclink_dialog->column_view));
    g_object_ref (selection_model);

    gtk_column_view_set_model (GTK_COLUMN_VIEW(doclink_dialog->column_view), NULL);

    g_list_store_remove_all (G_LIST_STORE(doclink_dialog->column_model));

    /* Loop through the transactions */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_TRANS),
                            add_trans_info_to_model, doclink_dialog);

    /* reconnect the model to the column view */
    gtk_column_view_set_model (GTK_COLUMN_VIEW(doclink_dialog->column_view),
                               GTK_SELECTION_MODEL(selection_model));
    g_object_unref (selection_model);

    gtk_single_selection_set_selected (GTK_SINGLE_SELECTION(selection_model), 0);

    update_total_entries (doclink_dialog);
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
    GtkBuilder *builder;
    GtkWidget  *button;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-doclink.ui", "linked_doc_window");

    doclink_dialog->window = GTK_WIDGET(gtk_builder_get_object (builder, "linked_doc_window"));
    doclink_dialog->session = gnc_get_current_session();

    GtkWidget *sw = GTK_WIDGET(gtk_builder_get_object (builder, "scrolled_window"));
    doclink_dialog->column_model = G_LIST_MODEL(g_list_store_new (DOCLINKVIEW_TYPE_ITEM));
    doclink_dialog->column_view = gnc_doclink_create_column_view (sw, doclink_dialog->column_model);

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
    gtk_window_set_default_widget (GTK_WINDOW(doclink_dialog->window),
                                   GTK_WIDGET(button));

    // Set the widget name and style context for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(doclink_dialog->window), "gnc-id-transaction-doclinks");
    gnc_widget_style_context_add_class (GTK_WIDGET(doclink_dialog->window), "gnc-class-doclink");

    doclink_dialog->path_head_label = GTK_WIDGET(gtk_builder_get_object (builder, "path-head"));
    doclink_dialog->total_entries_label = GTK_WIDGET(gtk_builder_get_object (builder, "total_entries_label"));
    doclink_dialog->path_head = gnc_doclink_get_path_head ();

    // display path head text and test if present
    gnc_doclink_set_path_head_label (doclink_dialog->path_head_label, NULL, NULL);

    gtk_column_view_set_show_row_separators (
        GTK_COLUMN_VIEW (doclink_dialog->column_view),
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                            GNC_PREF_GRID_LINES_HORIZONTAL));
    gtk_column_view_set_show_column_separators (
        GTK_COLUMN_VIEW (doclink_dialog->column_view),
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                            GNC_PREF_GRID_LINES_VERTICAL));

    g_signal_connect (G_OBJECT(doclink_dialog->window), "destroy",
                      G_CALLBACK(gnc_doclink_dialog_window_destroy_cb), doclink_dialog);

    g_signal_connect (doclink_dialog->window, "close-request",
                      G_CALLBACK (gnc_doclink_dialog_close_request_cb),
                      doclink_dialog);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(doclink_dialog->window), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window), "key-pressed",
                      G_CALLBACK(gnc_doclink_dialog_window_key_press_cb), doclink_dialog);

    GtkGesture *event_gesture = gtk_gesture_click_new ();
    gtk_widget_add_controller (GTK_WIDGET(doclink_dialog->column_view),
                               GTK_EVENT_CONTROLLER(event_gesture));

    gtk_event_controller_set_name (GTK_EVENT_CONTROLLER(event_gesture), "mytest");
    gtk_event_controller_set_propagation_phase (GTK_EVENT_CONTROLLER(event_gesture), GTK_PHASE_CAPTURE);
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE(event_gesture), 1);

    // Setup the correct parts for each dialog
    if (doclink_dialog->is_list_trans)
    {
        GtkColumnViewColumn *id_column = g_object_get_data (G_OBJECT(doclink_dialog->column_view), "id-column");
        GtkColumnViewColumn *type_column = g_object_get_data (G_OBJECT(doclink_dialog->column_view), "type-column");

        /* Translators: This is the label of a dialog box that lists all of the
           transaction that have files or URIs linked with them. */
        gtk_window_set_title (GTK_WINDOW(doclink_dialog->window), _("Transaction Document Links"));

        gtk_column_view_column_set_visible (id_column, FALSE);
        gtk_column_view_column_set_title (type_column, _("Description"));

        g_signal_connect (G_OBJECT(event_gesture), "released",
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

        g_signal_connect (G_OBJECT(event_gesture), "released",
                          G_CALLBACK(row_selected_bus_cb), doclink_dialog);

        get_bus_info (doclink_dialog);
    }
    gtk_widget_set_visible (GTK_WIDGET(doclink_dialog->window), TRUE);

    g_object_unref (G_OBJECT(builder));

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
    doclink_dialog->component_id = NO_COMPONENT;
    g_weak_ref_init (&doclink_dialog->update_window, NULL);
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
    doclink_dialog->component_id = NO_COMPONENT;
    g_weak_ref_init (&doclink_dialog->update_window, NULL);
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
