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
    const gchar *uri = xaccTransGetDocLink (trans); // get the existing uri
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

        if (!xaccTransGetReadOnly (trans) && !book_ro)
            xaccTransSetDocLink (trans, part);

        return g_strdup (part);
    }
    return g_strdup (uri);
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
    gboolean     book_ro;
}DoclinkUpdate;

static void
update_invoice_uri (QofInstance* data, gpointer user_data)
{
    DoclinkUpdate *doclink_update = user_data;
    GncInvoice *invoice = GNC_INVOICE(data);
    const gchar* uri = gncInvoiceGetDocLink (invoice);

    if (uri && *uri)
    {
        gboolean rel = FALSE;
        gchar *scheme = gnc_uri_get_scheme (uri);

        if (!scheme) // path is relative
            rel = TRUE;

        // check for relative and we want to change them
        if (rel && doclink_update->change_old)
        {
            gchar *new_uri = gnc_doclink_get_use_uri (doclink_update->old_path_head_uri, uri, scheme);
            gncInvoiceSetDocLink (invoice, new_uri);
            g_free (new_uri);
        }
        g_free (scheme);

        // check for not relative and we want to change them
        if (!rel && doclink_update->change_new && g_str_has_prefix (uri, doclink_update->new_path_head_uri))
        {
            // relative paths do not start with a '/'
            const gchar *part = uri + strlen (doclink_update->new_path_head_uri);
            gchar *new_uri = g_strdup (part);

            gncInvoiceSetDocLink (invoice, new_uri);
            g_free (new_uri);
        }
    }
}

static void
update_trans_uri (QofInstance* data, gpointer user_data)
{
    DoclinkUpdate *doclink_update = user_data;
    Transaction *trans = GNC_TRANSACTION(data);
    gchar *uri;

    // fix an earlier error when storing relative paths before version 3.5
    uri = gnc_doclink_convert_trans_link_uri (trans, doclink_update->book_ro);

    if (uri && *uri)
    {
        gboolean rel = FALSE;
        gchar *scheme = gnc_uri_get_scheme (uri);

        if (!scheme) // path is relative
            rel = TRUE;

        // check for relative and we want to change them
        if (rel && doclink_update->change_old)
        {
            gchar *new_uri = gnc_doclink_get_use_uri (doclink_update->old_path_head_uri, uri, scheme);

            if (!xaccTransGetReadOnly (trans))
                xaccTransSetDocLink (trans, new_uri);

            g_free (new_uri);
        }
        g_free (scheme);

        // check for not relative and we want to change them
        if (!rel && doclink_update->change_new && g_str_has_prefix (uri, doclink_update->new_path_head_uri))
        {
            // relative paths do not start with a '/'
            const gchar *part = uri + strlen (doclink_update->new_path_head_uri);
            gchar *new_uri = g_strdup (part);

            if (!xaccTransGetReadOnly (trans))
                xaccTransSetDocLink (trans, new_uri);

            g_free (new_uri);
        }
    }
    g_free (uri);
}

static void
change_relative_and_absolute_uri_paths (const gchar *old_path_head_uri, gboolean change_old,
                                        const gchar *new_path_head_uri, gboolean change_new)
{
    QofBook      *book = gnc_get_current_book();
    gboolean      book_ro = qof_book_is_readonly (book);
    DoclinkUpdate  *doclink_update;

    /* if book is read only, nothing to do */
    if (book_ro)
        return;

    doclink_update = g_new0 (DoclinkUpdate, 1);

    doclink_update->old_path_head_uri = old_path_head_uri;
    doclink_update->new_path_head_uri = new_path_head_uri;
    doclink_update->change_old = change_old;
    doclink_update->change_new = change_new;
    doclink_update->book_ro = book_ro;

    /* Loop through the transactions */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_TRANS),
                            update_trans_uri, doclink_update);

    /* Loop through the invoices */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_INVOICE),
                            update_invoice_uri, doclink_update);

    g_free (doclink_update);
}

void
gnc_doclink_pref_path_head_changed (GtkWindow *parent, const gchar *old_path_head_uri)
{
    GtkWidget  *dialog;
    GtkBuilder *builder;
    GtkWidget  *use_old_path_head, *use_new_path_head;
    GtkWidget  *old_head_label, *new_head_label;
    gint        result;
    gchar      *new_path_head_uri = gnc_doclink_get_path_head ();

    if (g_strcmp0 (old_path_head_uri, new_path_head_uri) == 0)
    {
        g_free (new_path_head_uri);
        return;
    }

    /* Create the dialog box */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-doclink.glade", "link_path_head_changed_dialog");
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "link_path_head_changed_dialog"));

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    // Set the name and style context for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-doclink-change");
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), "gnc-class-doclink");

    old_head_label = GTK_WIDGET(gtk_builder_get_object (builder, "existing_path_head"));
    new_head_label = GTK_WIDGET(gtk_builder_get_object (builder, "new_path_head"));

    use_old_path_head = GTK_WIDGET(gtk_builder_get_object (builder, "use_old_path_head"));
    use_new_path_head = GTK_WIDGET(gtk_builder_get_object (builder, "use_new_path_head"));

    // display path head text and test if present
    gnc_doclink_set_path_head_label (old_head_label, old_path_head_uri, _("Existing"));
    gnc_doclink_set_path_head_label (new_head_label, new_path_head_uri, _("New"));

    gtk_widget_show (dialog);
    g_object_unref (G_OBJECT(builder));

    // run the dialog
    result = gtk_dialog_run (GTK_DIALOG(dialog));
    if (result == GTK_RESPONSE_OK)
    {
        gboolean use_old = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(use_old_path_head));
        gboolean use_new = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(use_new_path_head));

        if (use_old || use_new)
            change_relative_and_absolute_uri_paths (old_path_head_uri, use_old,
                                                    new_path_head_uri, use_new);
    }
    g_free (new_path_head_uri);
    gtk_widget_destroy (dialog);
}
