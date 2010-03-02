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

#include "config.h"

#include <gtk/gtkprintoperation.h>

#include "print-session.h"
#include "gnc-gconf-utils.h" /* for gnc_gconf_set_string() */

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.printing"

/* Do not treat -Wstrict-aliasing warnings as errors because of problems of the
 * G_LOCK* macros as declared by glib.  See
 * http://bugzilla.gnome.org/show_bug.cgi?id=316221 for additional information.
 */
#if (__GNUC__ >= 4 && __GNUC_MINOR__ >= 2)
#    pragma GCC diagnostic warning "-Wstrict-aliasing"
#endif

static GtkPrintSettings *print_settings = NULL;
static GtkPageSetup *page_setup = NULL;
G_LOCK_DEFINE_STATIC(print_settings);
G_LOCK_DEFINE_STATIC(page_setup);


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
gnc_print_operation_init(GtkPrintOperation *op)
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
}

void
gnc_ui_page_setup(GtkWindow *parent)
{
    GtkPrintSettings *settings = NULL;
    GtkPageSetup *old_page_setup, *new_page_setup;

    /* Get a reference to the current print settings */
    G_LOCK(print_settings);
    settings = print_settings;
    if (settings)
        g_object_ref(settings);
    G_UNLOCK(print_settings);

    /* Get a reference to the current page setup */
    G_LOCK(page_setup);
    old_page_setup = page_setup;
    if (old_page_setup)
        g_object_ref(old_page_setup);
    G_UNLOCK(page_setup);

    /* Run dialog */
    new_page_setup = gtk_print_run_page_setup_dialog(parent, old_page_setup,
                     settings);

    /* Save new page setup */
    G_LOCK(page_setup);
    if (page_setup)
        g_object_unref(page_setup);
    page_setup = new_page_setup;
    G_UNLOCK(page_setup);

    /* Release references */
    if (settings)
        g_object_unref(settings);
    if (old_page_setup)
        g_object_unref(old_page_setup);
}
