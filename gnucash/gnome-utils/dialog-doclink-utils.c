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
#include "gnc-ui-util.h"
#include "gnc-uri-utils.h"

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
        path_head = gnc_doclink_get_path_head_and_set (&path_head_set);

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
