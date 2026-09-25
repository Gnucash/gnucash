/********************************************************************\
 * print-session.c -- simple printing manager for gnucash           *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#include "print-session.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.printing"

static GtkPrintSettings *print_settings = NULL;
static GtkPageSetup *page_setup = NULL;
G_LOCK_DEFINE_STATIC(print_settings);
G_LOCK_DEFINE_STATIC(page_setup);

typedef struct
{
    GtkPrintDialog *dialog;
    GCancellable *cancellable;
    GWeakRef parent;
} PageSetupRequest;

static void
page_setup_request_parent_destroyed (gpointer user_data,
                                     G_GNUC_UNUSED GObject *where_parent_was)
{
    PageSetupRequest *request = user_data;

    g_cancellable_cancel (request->cancellable);
}

static void
page_setup_request_free (PageSetupRequest *request)
{
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    if (parent)
    {
        g_object_weak_unref (G_OBJECT (parent),
                             page_setup_request_parent_destroyed, request);
        g_object_unref (parent);
    }
    g_weak_ref_clear (&request->parent);
    g_clear_object (&request->cancellable);
    g_clear_object (&request->dialog);
    g_free (request);
}

static void
save_print_state (GtkPrintSettings *settings, GtkPageSetup *new_page_setup)
{
    G_LOCK (print_settings);
    g_clear_object (&print_settings);
    if (settings)
        print_settings = g_object_ref (settings);
    G_UNLOCK (print_settings);

    G_LOCK (page_setup);
    g_clear_object (&page_setup);
    if (new_page_setup)
        page_setup = g_object_ref (new_page_setup);
    G_UNLOCK (page_setup);
}

static void
page_setup_request_finished (GObject *source, GAsyncResult *result,
                             gpointer user_data)
{
    PageSetupRequest *request = user_data;
    GError *error = NULL;
    GtkPrintSetup *setup =
        gtk_print_dialog_setup_finish (GTK_PRINT_DIALOG (source), result,
                                       &error);

    if (setup)
    {
        GtkPrintSettings *settings =
            gtk_print_setup_get_print_settings (setup);
        GtkPageSetup *new_page_setup =
            gtk_print_setup_get_page_setup (setup);

        save_print_state (settings, new_page_setup);

        gtk_print_setup_unref (setup);
    }
    else if (!g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
    {
        g_warning ("Could not update print settings: %s",
                   error ? error->message : "unknown error");
    }
    g_clear_error (&error);
    page_setup_request_free (request);
}


void
gnc_print_operation_save_print_settings(GtkPrintOperation *op)
{
    g_return_if_fail(op);

    G_LOCK(print_settings);
    if (print_settings)
        g_object_unref(print_settings);
    print_settings = g_object_ref(gtk_print_operation_get_print_settings(op));
    G_UNLOCK(print_settings);
}

void
gnc_print_setup_save (GtkPrintSetup *setup)
{
    g_return_if_fail (setup);

    save_print_state (gtk_print_setup_get_print_settings (setup),
                      gtk_print_setup_get_page_setup (setup));
}

void
gnc_print_operation_init(GtkPrintOperation *op, const gchar* jobname)
{
    g_return_if_fail(op);

    /* Restore print settings */
    G_LOCK(print_settings);
    if (print_settings)
        gtk_print_operation_set_print_settings(op, print_settings);
    G_UNLOCK(print_settings);

    /* Restore page setup */
    G_LOCK(page_setup);
    if (page_setup)
        gtk_print_operation_set_default_page_setup(op, page_setup);
    G_UNLOCK(page_setup);

    gtk_print_operation_set_job_name ( op, jobname);
}

void
gnc_ui_page_setup(GtkWindow *parent)
{
    PageSetupRequest *request = g_new0 (PageSetupRequest, 1);
    GtkPrintSettings *settings = NULL;
    GtkPageSetup *current_page_setup = NULL;

    G_LOCK(print_settings);
    if (print_settings)
        settings = g_object_ref (print_settings);
    G_UNLOCK(print_settings);

    G_LOCK(page_setup);
    if (page_setup)
        current_page_setup = g_object_ref (page_setup);
    G_UNLOCK(page_setup);

    request->dialog = gtk_print_dialog_new ();
    request->cancellable = g_cancellable_new ();
    g_weak_ref_init (&request->parent, parent);
    if (parent)
        g_object_weak_ref (G_OBJECT (parent),
                           page_setup_request_parent_destroyed, request);

    gtk_print_dialog_set_title (request->dialog, _("Page Setup"));
    gtk_print_dialog_set_modal (request->dialog, TRUE);
    if (settings)
        gtk_print_dialog_set_print_settings (request->dialog, settings);
    if (current_page_setup)
        gtk_print_dialog_set_page_setup (request->dialog, current_page_setup);

    gtk_print_dialog_setup (request->dialog, parent, request->cancellable,
                            page_setup_request_finished, request);

    g_clear_object (&settings);
    g_clear_object (&current_page_setup);
}

GtkPrintSettings *gnc_print_get_settings()
{
    return print_settings;
}
