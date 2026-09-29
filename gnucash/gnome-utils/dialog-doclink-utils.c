/********************************************************************\
 * dialog-doclink-utils.c -- Document link dialog Utils             *
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

#include "dialog-doclink-utils.h"

#include "dialog-utils.h"
#include "Transaction.h"
#include "gncInvoice.h"

#include "gnc-prefs.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gnome-utils.h"
#include "gnc-uri-utils.h"
#include "gnc-filepath-utils.h"
#include "Account.h"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

/* =================================================================== */

static gchar *
convert_uri_to_abs_path (const gchar *path_head, const gchar *uri, 
                         gchar *uri_scheme, gboolean return_uri)
{
    gchar *ret_value = NULL;

    if (!uri_scheme) // relative path
    {
        gchar *path = gnc_uri_get_path (path_head);
        gchar *file_path = gnc_file_path_absolute (path, uri);

        if (return_uri)
            ret_value = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, file_path);
        else
            ret_value = g_strdup (file_path);

        g_free (path);
        g_free (file_path);
    }

    if (g_strcmp0 (uri_scheme, "file") == 0) // absolute path
    {
        if (return_uri)
            ret_value = g_strdup (uri);
        else
            ret_value = gnc_uri_get_path (uri);
    }
    return ret_value;
}

gchar *
gnc_doclink_get_unescape_uri (const gchar *path_head, const gchar *uri, gchar *uri_scheme)
{
    gchar *display_str = NULL;

    if (uri && *uri)
    {
        // if scheme is null or 'file' we should get a file path
        gchar *file_path = convert_uri_to_abs_path (path_head, uri, uri_scheme, FALSE);

        if (file_path)
            display_str = g_uri_unescape_string (file_path, NULL);
        else
            display_str = g_uri_unescape_string (uri, NULL);

        g_free (file_path);

#ifdef G_OS_WIN32 // make path look like a traditional windows path
        g_strdelimit (display_str, "/", '\\');
#endif
    }
    DEBUG("Return display string is '%s'", display_str);
    return display_str;
}

gchar *
gnc_doclink_get_use_uri (const gchar *path_head, const gchar *uri, gchar *uri_scheme)
{
    gchar *use_str = NULL;

    if (uri && *uri)
    {
        // if scheme is null or 'file' we should get a file path
        gchar *file_path = convert_uri_to_abs_path (path_head, uri, uri_scheme, TRUE);

        if (file_path)
            use_str = g_strdup (file_path);
        else
            use_str = g_strdup (uri);

        g_free (file_path);
    }
    DEBUG("Return use string is '%s'", use_str);
    return use_str;
}

gchar *
gnc_doclink_get_unescaped_just_uri (const gchar *uri)
{
    gchar *path_head = gnc_doclink_get_path_head ();
    gchar *uri_scheme = gnc_uri_get_scheme (uri);
    gchar *ret_uri = gnc_doclink_get_unescape_uri (path_head, uri, uri_scheme);

    g_free (path_head);
    g_free (uri_scheme);
    return ret_uri;
}

gchar *
gnc_doclink_convert_trans_link_uri (gpointer trans, gboolean book_ro)
{
    gchar *uri = g_strdup (xaccTransGetDocLink (trans));
    const gchar *part = NULL;

    if (!uri)
        return NULL;

    if (g_str_has_prefix (uri, "file:") && !g_str_has_prefix (uri,"file://"))
    {
        /* fix an earlier error when storing relative paths before version 3.5
         * they were stored starting as 'file:' or 'file:/' depending on OS
         * relative paths are stored without a leading "/" and in native form
         */
        if (g_str_has_prefix (uri,"file:/"))
            part = uri + strlen ("file:/");
        else if (g_str_has_prefix (uri,"file:"))
            part = uri + strlen ("file:");

        gchar *converted = g_strdup (part);
        if (!xaccTransGetReadOnly (trans) && !book_ro)
            xaccTransSetDocLink (trans, converted);
        g_free (uri);
        return converted;
    }
    return uri;
}

/* =================================================================== */

static gchar *
doclink_get_path_head_and_set (gboolean *path_head_set)
{
    gchar *ret_path = NULL;
    gchar *path_head = gnc_prefs_get_string (GNC_PREFS_GROUP_GENERAL, GNC_DOC_LINK_PATH_HEAD);
    *path_head_set = FALSE;

    if (path_head && *path_head) // not default entry
    {
        *path_head_set = TRUE;
        ret_path = g_strdup (path_head);
    }
    else
    {
        const gchar *doc = g_get_user_special_dir (G_USER_DIRECTORY_DOCUMENTS);

        if (doc)
            ret_path = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, doc);
        else
            ret_path = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, gnc_userdata_dir ());
    }
    // make sure there is a trailing '/'
    if (!g_str_has_suffix (ret_path, "/"))
    {
        gchar *folder_with_slash = g_strconcat (ret_path, "/", NULL);
        g_free (ret_path);
        ret_path = g_strdup (folder_with_slash);
        g_free (folder_with_slash);

        if (*path_head_set) // prior to 3.5, assoc-head could be with or without a trailing '/'
        {
            if (!gnc_prefs_set_string (GNC_PREFS_GROUP_GENERAL, GNC_DOC_LINK_PATH_HEAD, ret_path))
                PINFO ("Failed to save preference at %s, %s with %s",
                       GNC_PREFS_GROUP_GENERAL, GNC_DOC_LINK_PATH_HEAD, ret_path);
        }
    }
    g_free (path_head);
    return ret_path;
}

gchar *
gnc_doclink_get_path_head (void)
{
    gboolean path_head_set = FALSE;

    return doclink_get_path_head_and_set (&path_head_set);
}

void
gnc_doclink_set_path_head_label (GtkWidget *path_head_label, const gchar *incoming_path_head, const gchar *prefix)
{
    gboolean path_head_set = FALSE;
    gchar *path_head = NULL;
    gchar *scheme;
    gchar *path_head_str;
    gchar *path_head_text;

    if (incoming_path_head)
    {
         path_head = g_strdup (incoming_path_head);
         path_head_set = TRUE;
    }
    else
        path_head = doclink_get_path_head_and_set (&path_head_set);

    scheme = gnc_uri_get_scheme (path_head);
    path_head_str = gnc_doclink_get_unescape_uri (NULL, path_head, scheme);

    if (path_head_set)
    {
        // test for current folder being present
        if (g_file_test (path_head_str, G_FILE_TEST_IS_DIR))
            path_head_text = g_strdup_printf ("%s '%s'", _("Path head for files is,"), path_head_str);
        else
            path_head_text = g_strdup_printf ("%s '%s'", _("Path head does not exist,"), path_head_str);
    }
    else
        path_head_text = g_strdup_printf (_("Path head not set, using '%s' for relative paths"), path_head_str);

    if (prefix)
    {
        gchar *tmp = g_strdup (path_head_text);
        g_free (path_head_text);

        path_head_text = g_strdup_printf ("%s %s", prefix, tmp);

        g_free (tmp);
    }

    gtk_label_set_text (GTK_LABEL(path_head_label), path_head_text);

    // Set the style context for this label so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(path_head_label), "gnc-class-highlight");

    g_free (scheme);
    g_free (path_head_str);
    g_free (path_head_text);
    g_free (path_head);
}

/* =================================================================== */

typedef struct
{
    const gchar *old_path_head_uri;
    gboolean     change_old;
    const gchar *new_path_head_uri;
    gboolean     change_new;
    QofBook    **book;
    const gboolean *parent_destroyed;
}DoclinkUpdate;

static QofBook *
doclink_update_book (DoclinkUpdate *update)
{
    gchar *path_head = gnc_doclink_get_path_head ();
    gboolean same_path = g_strcmp0 (path_head, update->new_path_head_uri) == 0;
    QofBook *book;
    g_free (path_head);
    if (!same_path)
        return NULL;
    book = *update->book;
    if (!book || *update->parent_destroyed || !qof_book_is_open (book) ||
        qof_book_shutting_down (book) || qof_book_is_readonly (book) ||
        !gnc_current_session_exist () ||
        qof_session_get_book (gnc_get_current_session ()) != book)
        return NULL;
    return book;
}

static gchar *
doclink_updated_uri (const gchar *uri, const DoclinkUpdate *update)
{
    gchar *scheme, *result = NULL;
    if (!uri || !*uri)
        return NULL;
    scheme = gnc_uri_get_scheme (uri);
    if (!scheme && update->change_old)
        result = gnc_doclink_get_use_uri (update->old_path_head_uri, uri, scheme);
    else if (scheme && update->change_new &&
             g_str_has_prefix (uri, update->new_path_head_uri))
        result = g_strdup (uri + strlen (update->new_path_head_uri));
    g_free (scheme);
    return result;
}

static void
doclink_collect_guid (QofInstance *instance, gpointer data)
{
    GncGUID guid = *qof_instance_get_guid (instance);
    g_array_append_val ((GArray *)data, guid);
}

static void
change_relative_and_absolute_uri_paths (DoclinkUpdate *update)
{
    QofBook *book = doclink_update_book (update);
    GArray *transactions, *invoices;
    if (!book)
        return;
    transactions = g_array_new (FALSE, FALSE, sizeof (GncGUID));
    invoices = g_array_new (FALSE, FALSE, sizeof (GncGUID));
    /* Snapshot identities before setters can emit events or remove objects. */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_TRANS),
                            doclink_collect_guid, transactions);
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_INVOICE),
                            doclink_collect_guid, invoices);
    for (guint i = 0; i < transactions->len && (book = doclink_update_book (update)); ++i)
    {
        GncGUID *guid = &g_array_index (transactions, GncGUID, i);
        Transaction *trans = xaccTransLookup (guid, book);
        gchar *uri, *new_uri;
        if (!trans || xaccTransGetReadOnly (trans))
            continue;
        uri = gnc_doclink_convert_trans_link_uri (trans, FALSE);
        book = doclink_update_book (update);
        trans = book ? xaccTransLookup (guid, book) : NULL;
        new_uri = doclink_updated_uri (uri, update);
        if (trans && !xaccTransGetReadOnly (trans) && new_uri &&
            g_strcmp0 (xaccTransGetDocLink (trans), uri) == 0)
            xaccTransSetDocLink (trans, new_uri);
        g_free (new_uri);
        g_free (uri);
    }
    for (guint i = 0; i < invoices->len && (book = doclink_update_book (update)); ++i)
    {
        GncInvoice *invoice = gncInvoiceLookup (book, &g_array_index (invoices, GncGUID, i));
        gchar *uri, *new_uri;
        if (!invoice)
            continue;
        uri = g_strdup (gncInvoiceGetDocLink (invoice));
        new_uri = doclink_updated_uri (uri, update);
        if (new_uri)
            gncInvoiceSetDocLink (invoice, new_uri);
        g_free (new_uri);
        g_free (uri);
    }
    g_array_free (transactions, TRUE);
    g_array_free (invoices, TRUE);
}

typedef struct
{
    GtkWidget       *dialog;
    GtkToggleButton *use_old_path_head;
    GtkToggleButton *use_new_path_head;
    gchar           *old_path_head_uri;
    gchar           *new_path_head_uri;
    gboolean         completed;
    QofBook          *book;
    GtkWindow        *parent;
    gboolean         parent_destroyed;
} DoclinkPathHeadRequest;

static void
doclink_path_head_parent_destroyed ([[maybe_unused]] GtkWidget *parent,
                                    DoclinkPathHeadRequest *request)
{
    request->parent_destroyed = TRUE;
}

static void
doclink_path_head_request_complete (DoclinkPathHeadRequest *request,
                                    gboolean accepted,
                                    gboolean dialog_destroying)
{
    gboolean use_old = FALSE;
    gboolean use_new = FALSE;

    if (request->completed)
        return;

    request->completed = TRUE;
    if (accepted)
    {
        use_old = gtk_toggle_button_get_active (request->use_old_path_head);
        use_new = gtk_toggle_button_get_active (request->use_new_path_head);
    }

    g_signal_handlers_disconnect_by_data (request->dialog, request);
    if (!dialog_destroying)
        gtk_widget_destroy (request->dialog);

    if (use_old || use_new)
    {
        DoclinkUpdate update = {request->old_path_head_uri, use_old,
                                request->new_path_head_uri, use_new,
                                &request->book, &request->parent_destroyed};
        change_relative_and_absolute_uri_paths (&update);
    }

    if (request->book)
        g_object_remove_weak_pointer (G_OBJECT (request->book),
                                      (gpointer *)&request->book);
    if (request->parent)
        g_signal_handlers_disconnect_by_data (request->parent, request);
    g_clear_object (&request->parent);

    g_clear_object (&request->use_old_path_head);
    g_clear_object (&request->use_new_path_head);
    g_clear_object (&request->dialog);
    g_free (request->old_path_head_uri);
    g_free (request->new_path_head_uri);
    g_free (request);
}

static void
doclink_path_head_response_cb ([[maybe_unused]] GtkDialog *dialog, gint response,
                               DoclinkPathHeadRequest *request)
{
    doclink_path_head_request_complete (request, response == GTK_RESPONSE_OK,
                                       FALSE);
}

static void
doclink_path_head_destroy_cb ([[maybe_unused]] GtkWidget *dialog,
                              DoclinkPathHeadRequest *request)
{
    doclink_path_head_request_complete (request, FALSE, TRUE);
}

void
gnc_doclink_pref_path_head_changed (GtkWindow *parent, const gchar *old_path_head_uri)
{
    GtkWidget  *dialog;
    GtkBuilder *builder;
    GtkWidget  *ok_button;
    GtkWidget  *use_old_path_head, *use_new_path_head;
    GtkWidget  *old_head_label, *new_head_label;
    DoclinkPathHeadRequest *request;
    gchar      *new_path_head_uri = gnc_doclink_get_path_head ();

    if (g_strcmp0 (old_path_head_uri, new_path_head_uri) == 0)
    {
        g_free (new_path_head_uri);
        return;
    }

    /* Create the dialog box */
    builder = gtk_builder_new();
    if (!gnc_builder_add_from_file (builder, "dialog-doclink.glade",
                                   "link_path_head_changed_dialog"))
    {
        g_object_unref (builder);
        g_free (new_path_head_uri);
        return;
    }
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "link_path_head_changed_dialog"));
    ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "button4"));

    old_head_label = GTK_WIDGET(gtk_builder_get_object (builder, "existing_path_head"));
    new_head_label = GTK_WIDGET(gtk_builder_get_object (builder, "new_path_head"));

    use_old_path_head = GTK_WIDGET(gtk_builder_get_object (builder, "use_old_path_head"));
    use_new_path_head = GTK_WIDGET(gtk_builder_get_object (builder, "use_new_path_head"));

    if (!dialog || !ok_button || !old_head_label || !new_head_label ||
        !use_old_path_head || !use_new_path_head)
    {
        g_object_unref (builder);
        g_free (new_path_head_uri);
        return;
    }

    request = g_new0 (DoclinkPathHeadRequest, 1);
    request->dialog = g_object_ref (dialog);
    request->use_old_path_head = g_object_ref (GTK_TOGGLE_BUTTON (use_old_path_head));
    request->use_new_path_head = g_object_ref (GTK_TOGGLE_BUTTON (use_new_path_head));
    request->old_path_head_uri = g_strdup (old_path_head_uri);
    request->new_path_head_uri = new_path_head_uri;
    if (gnc_current_session_exist ())
    {
        request->book = qof_session_get_book (gnc_get_current_session ());
        if (request->book)
            g_object_add_weak_pointer (G_OBJECT (request->book),
                                       (gpointer *)&request->book);
    }
    if (parent)
    {
        request->parent = g_object_ref (parent);
        g_signal_connect (parent, "destroy",
                          G_CALLBACK (doclink_path_head_parent_destroyed), request);
    }

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));
    gtk_window_set_modal (GTK_WINDOW(dialog), TRUE);
    gtk_window_set_destroy_with_parent (GTK_WINDOW(dialog), TRUE);

    // Set the name and style context for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-doclink-change");
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), "gnc-class-doclink");

    // display path head text and test if present
    gnc_doclink_set_path_head_label (old_head_label, old_path_head_uri, _("Existing"));
    gnc_doclink_set_path_head_label (new_head_label, request->new_path_head_uri, _("New"));

    g_signal_connect (dialog, "response",
                      G_CALLBACK (doclink_path_head_response_cb), request);
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (doclink_path_head_destroy_cb), request);
    g_object_unref (builder);
    gtk_widget_show (dialog);
}
