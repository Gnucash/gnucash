/********************************************************************
 * gnc-report.c -- C functions for reports.                         *
 *                                                                  *
 * Copyright (C) 2001 Linux Developers Group                        *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>         *
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
 ********************************************************************/

#include "config.h"

#include <glib.h>
#include <glib/gstdio.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <stdio.h>
#include <string.h>
#include "gfec.h"
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#include "gnc-filepath-utils.h"
#include "gnc-guile-utils.h"
#include "gnc-report.h"
#include "gnc-engine.h"

static QofLogModule log_module = GNC_MOD_GUI;

/* Fow now, this is global, like it was in guile.  It _should_ be per-book. */
static GHashTable *reports = NULL;
static gint report_next_serial_id = 0;

static void
gnc_report_init_table(void)
{
    if (!reports)
    {
        reports = g_hash_table_new_full(
                      g_int_hash, g_int_equal,
                      g_free, (GDestroyNotify) scm_gc_unprotect_object);
    }
}

void
gnc_report_remove_by_id(gint id)
{
    if (reports)
        g_hash_table_remove(reports, &id);
}

SCM gnc_report_find(gint id)
{
    gpointer report = NULL;

    if (reports)
    {
        report = g_hash_table_lookup(reports, &id);
    }

    if (!report)
        return SCM_BOOL_F;

    return report;
}

gint gnc_report_add(SCM report)
{
    SCM get_id = scm_c_eval_string("gnc:report-id");
    SCM value;
    gint id, *key;

    gnc_report_init_table();

    value = scm_call_1(get_id, report);
    if (scm_is_number(value))
    {
        id = scm_to_int(value);
        if (!g_hash_table_lookup(reports, &id))
        {
            key = g_new(gint, 1);
            *key = id;
            g_hash_table_insert(reports, key, (gpointer)report);
            scm_gc_protect_object(report);
            return id;
        }
        g_warning("Report specified id of %d is already is use. "
                  "Using generated id.", id);
    }

    id = report_next_serial_id++;
    while (id < G_MAXINT)
    {
        if (!g_hash_table_lookup(reports, &id))
        {
            key = g_new(gint, 1);
            *key = id;
            g_hash_table_insert(reports, key, (gpointer)report);
            scm_gc_protect_object(report);
            return id;
        }
        id = report_next_serial_id++;
    }

    g_warning("Unable to add report to table. %d reports in use.", G_MAXINT);
    report_next_serial_id = G_MAXINT;
    return G_MAXINT;
}

static gboolean
yes_remove(gpointer key, gpointer val, gpointer data)
{
    return TRUE;
}

void
gnc_reports_flush_global(void)
{
    if (reports)
        g_hash_table_foreach_remove(reports, yes_remove, NULL);
}

GHashTable *
gnc_reports_get_global(void)
{
    gnc_report_init_table();
    return reports;
}

static void
error_handler(const char *str)
{
    g_warning("Failure running report: %s", str);
}

gboolean
gnc_run_report (gint report_id, char ** data)
{
    gchar *free_data;
    SCM scm_text;
    gchar *str;

    g_return_val_if_fail (data != NULL, FALSE);
    *data = NULL;

    str = g_strdup_printf("(gnc:report-run %d)", report_id);
    scm_text = gfec_eval_string(str, error_handler);
    g_free(str);

    if (scm_text == SCM_UNDEFINED || !scm_is_string (scm_text))
        return FALSE;

    *data = gnc_scm_to_utf8_string (scm_text);

    return TRUE;
}

gboolean
gnc_run_report_id_string (const char * id_string, char **data)
{
    gint report_id;

    g_return_val_if_fail (id_string != NULL, FALSE);
    g_return_val_if_fail (data != NULL, FALSE);
    *data = NULL;

    if (strncmp ("id=", id_string, 3) != 0)
        return FALSE;

    if (sscanf (id_string + 3, "%d", &report_id) != 1)
        return FALSE;

    return gnc_run_report (report_id, data);
}

gchar*
gnc_report_name( SCM report )
{
    SCM    get_name = scm_c_eval_string("gnc:report-name");

    if (report == SCM_BOOL_F)
        return NULL;

    return gnc_scm_call_1_to_string(get_name, report);
}

gchar*
gnc_get_default_report_font_family(void)
{
    GList*          top_list;
    GtkWidget*      top_widget;
    GtkStyle*       top_widget_style;
    const gchar*    default_font_family;

    top_list = gtk_window_list_toplevels();
    g_return_val_if_fail (top_list != NULL, NULL);
    top_widget = GTK_WIDGET(top_list->data);
    g_list_free(top_list);
    top_widget_style = gtk_rc_get_style(top_widget);
    default_font_family =
        pango_font_description_get_family(top_widget_style->font_desc);

    if (default_font_family == NULL)
        return g_strdup("Arial");
    else
        return g_strdup(default_font_family);
}

static gboolean
gnc_saved_reports_write_internal (const gchar *file, const gchar *contents, gboolean overwrite)
{
    gboolean success = TRUE;
    gint fd;
    extern int errno;
    ssize_t written;
    gint length;
    gint flags = O_WRONLY | O_CREAT | (overwrite ? O_TRUNC : O_APPEND);

    fd = g_open (file, flags, 0666);
    if (fd == -1)
    {
        PWARN("Cannot open file %s: %s\n", file, strerror(errno));
        return FALSE;
    }

    length = strlen (contents);
    written = write(fd, contents, length);
    if (written == -1 )
    {
        success = FALSE;
        PWARN("Cannot write to file %s: %s\n", file, strerror(errno));
        close(fd);
    }
    else if (written != length)
    {
        success = FALSE;
        PWARN("File %s truncated (provided %d, written %d)",
                file, length, (int)written);
        /* Ignore errors on close */
        close(fd);
    }
    else if (close(fd) == -1)
        PWARN("Close failed for file %s: %s", file, strerror(errno));

    return success;
}


gboolean gnc_saved_reports_backup (void)
{
    gboolean success = FALSE;
    gchar *saved_rpts_path     = gnc_build_dotgnucash_path (SAVED_REPORTS_FILE);
    gchar *saved_rpts_bkp_path = g_strconcat (saved_rpts_path, "-backup", NULL);
    gchar *contents = NULL;
    GError *save_error = NULL;

    if (g_file_test (saved_rpts_path, G_FILE_TEST_EXISTS))
    {
        if (!g_file_get_contents (saved_rpts_path, &contents, NULL, &save_error))
        {
            PWARN ("Couldn't read contents of %s.\nReason: %s", saved_rpts_path, save_error->message);
            g_error_free (save_error);
        }
    }

    if (contents)
    {
        DEBUG ("creating backup of file %s", saved_rpts_bkp_path);
        success = gnc_saved_reports_write_internal (saved_rpts_bkp_path, contents, TRUE);
    }

    g_free (saved_rpts_path);
    g_free (saved_rpts_bkp_path);
    g_free (contents);

    return success;
}

gboolean
gnc_saved_reports_write_to_file (const gchar* report_def, gboolean overwrite)
{
    gboolean success = FALSE;
    gchar *saved_rpts_path     = gnc_build_dotgnucash_path (SAVED_REPORTS_FILE);

    if (report_def)
    {
        DEBUG ("writing to %s", saved_rpts_path);
        success = gnc_saved_reports_write_internal (saved_rpts_path, report_def, overwrite);
    }

    g_free (saved_rpts_path);

    return success;
}
