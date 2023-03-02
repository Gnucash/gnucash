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

#include <config.h>
#ifdef __MINGW32__
#define _GL_UNISTD_H //Deflect poisonous define in Guile's GnuLib
#endif
#include <gnc-optiondb.hpp>
#include <glib.h>
#include <glib/gstdio.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <stdio.h>
#include <string.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#include <gfec.h>
#include <gnc-filepath-utils.h>
#include <gnc-guile-utils.h>
#include <gnc-engine.h>
#include "gnc-report.h"

extern "C" SCM scm_init_sw_report_module(void);

static QofLogModule log_module = GNC_MOD_GUI;

/* Fow now, this is global, like it was in guile.  It _should_ be per-book. */
static GHashTable *reports = NULL;
static gint report_next_serial_id = 0;

static gboolean
try_load_config_array(const gchar *fns[])
{
    gchar *filename;
    int i;

    for (i = 0; fns[i]; i++)
    {
        filename = gnc_build_userdata_path(fns[i]);
        if (gfec_try_load(filename))
        {
            g_free(filename);
            return TRUE;
        }
        g_free(filename);
    }
    return FALSE;
}

static void
update_message(const gchar *msg)
{
    //gnc_update_splash_screen(msg, GNC_SPLASH_PERCENTAGE_UNKNOWN);
    PINFO("%s", msg);
}

static void
load_custom_reports_stylesheets(void)
{
    /* Don't continue adding to this list. When 3.0 rolls around bump
     *      the 2.4 files off the list. */
    static const gchar *saved_report_files[] =
    {
        SAVED_REPORTS_FILE, SAVED_REPORTS_FILE_OLD_REV, NULL
    };
    static const gchar *stylesheet_files[] = { "stylesheets-2.0", NULL};
    static int is_user_config_loaded = FALSE;

    if (is_user_config_loaded)
        return;
    else is_user_config_loaded = TRUE;

    update_message("loading saved reports");
    try_load_config_array(saved_report_files);
    update_message("loading stylesheets");
    try_load_config_array(stylesheet_files);
}

void
gnc_report_init (void)
{
    scm_init_sw_report_module();
    scm_c_use_module ("gnucash report");
    scm_c_use_module ("gnucash reports");
    scm_c_eval_string("(report-module-loader (list '(gnucash report stylesheets)))");

    load_custom_reports_stylesheets();
}


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
    SCM report = nullptr;

    if (reports)
    {
        report = static_cast<SCM>(g_hash_table_lookup(reports, &id));
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

gboolean
gnc_run_report_with_error_handling (gint report_id, gchar ** data, gchar **errmsg)
{
    SCM report, res, html, captured_error;

    report = gnc_report_find (report_id);
    g_return_val_if_fail (data, FALSE);
    g_return_val_if_fail (errmsg, FALSE);
    g_return_val_if_fail (!scm_is_false (report), FALSE);

    res = scm_call_1 (scm_c_eval_string ("gnc:render-report"), report);
    html = scm_car (res);
    captured_error = scm_cadr (res);

    if (!scm_is_false (html))
    {
        *data = gnc_scm_to_utf8_string (html);
        *errmsg = NULL;
        return TRUE;
    }
    else
    {
        *errmsg = gnc_scm_to_utf8_string (captured_error);
        *data = NULL;
        PWARN ("Error in report: %s", *errmsg);
        return FALSE;
    }
}

gchar*
gnc_report_name( SCM report )
{
    SCM    get_name = scm_c_eval_string("gnc:report-name");

    if (report == SCM_BOOL_F)
        return NULL;

    return gnc_scm_call_1_to_string(get_name, report);
}

gboolean
gnc_run_report_id_string_with_error_handling (const char * id_string, char **data,
                                              gchar **errmsg)
{
    gint report_id;

    g_return_val_if_fail (id_string, FALSE);
    g_return_val_if_fail (data, FALSE);
    *data = NULL;

    if (strncmp ("id=", id_string, 3) != 0)
        return FALSE;

    if (sscanf (id_string + 3, "%d", &report_id) != 1)
        return FALSE;

    return gnc_run_report_with_error_handling (report_id, data, errmsg);
}

gchar*
gnc_get_default_report_font_family(void)
{
    GList                *top_list;
    GtkWidget            *top_widget;
    PangoFontDescription *font_desc;
    GtkStyleContext      *top_widget_style_c;
    gchar                *default_font_family;

    top_list = gtk_window_list_toplevels();
    if (top_list == NULL)
        return g_strdup ("Arial");
    top_widget = GTK_WIDGET(top_list->data);
    g_list_free(top_list);
    top_widget_style_c = gtk_widget_get_style_context (top_widget);
    gtk_style_context_get (top_widget_style_c, gtk_widget_get_state_flags (GTK_WIDGET(top_widget)),
                           "font", &font_desc, NULL);

    default_font_family = g_strdup(pango_font_description_get_family (font_desc));

    pango_font_description_free (font_desc);

    if (!default_font_family)
        return g_strdup ("Arial");
    else if (g_str_has_prefix (default_font_family, ".AppleSystemUIFont"))
    {
        g_free (default_font_family);
        return g_strdup ("Arial");
    }
    else
        return default_font_family;
}

static gboolean
gnc_saved_reports_write_internal (const gchar *file, const gchar *contents, gboolean overwrite)
{
    gboolean success = TRUE;
    gint fd;
    ssize_t written;
    gint length;
    gint flags = O_WRONLY | O_CREAT | (overwrite ? O_TRUNC : O_APPEND);
    /* Bug 764248: Keep write from adding \r to the line endings.  On
     * windows the file already has them set to \r\n and if we output
     * in text mode we get \r\r\n.
     */
#ifdef G_OS_WIN32
    if (strstr(file, "backup"))
	flags |= O_BINARY;
#endif
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
    gchar *saved_rpts_path     = gnc_build_userdata_path (SAVED_REPORTS_FILE);
    gchar *saved_rpts_bkp_path = gnc_build_userdata_path (SAVED_REPORTS_FILE "-backup");
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
    gchar *saved_rpts_path = gnc_build_userdata_path (SAVED_REPORTS_FILE);

    if (report_def)
    {
        DEBUG ("writing to %s", saved_rpts_path);
        success = gnc_saved_reports_write_internal (saved_rpts_path, report_def, overwrite);
    }

    g_free (saved_rpts_path);

    return success;
}

GncOptionDB*
gnc_get_optiondb_from_dispatcher(SCM dispatcher)
{
    SCM  get_options = scm_c_eval_string("gnc:optiondb");
    if (dispatcher == SCM_BOOL_F)
        return nullptr;
    auto scm_ptr{scm_call_1(get_options, dispatcher)};
    auto smob{!scm_is_null(scm_ptr) && SCM_INSTANCEP(scm_ptr) &&
              scm_is_true(scm_slot_exists_p(scm_ptr, SCM_EOL)) ?
              scm_slot_ref(scm_ptr, SCM_EOL) : (scm_ptr)};

    void *c_ptr{nullptr};
    if (!SCM_NULLP(smob))
    {
        if (SCM_POINTER_P(smob))
            c_ptr = SCM_POINTER_VALUE(smob);
        else
            c_ptr = reinterpret_cast<void*>(SCM_CELL_WORD_1(smob));
    }
    else
        return nullptr;

    auto u_ptr{static_cast<std::unique_ptr<GncOptionDB>*>(c_ptr)};
    return u_ptr->get();
}

