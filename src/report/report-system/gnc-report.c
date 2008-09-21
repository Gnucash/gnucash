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
#include <libguile.h>
#include <stdio.h>
#include <string.h>
#include "gfec.h"

#include "gnc-report.h"

/* Fow now, this is global, like it was in guile.  It _should_ be per-book. */
static GHashTable *reports = NULL;
static gint report_next_serial_id = 0;

static void
gnc_report_init_table(void)
{
    if (!reports) {
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

    if (reports) {
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
    if (SCM_NUMBERP(value)) {
      id = scm_num2int(value, SCM_ARG1, G_STRFUNC);
      if (!g_hash_table_lookup(reports, &id)) {
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
    while (id < G_MAXINT) {
      if (!g_hash_table_lookup(reports, &id)) {
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
  const gchar *free_data;
  SCM scm_text;
  gchar *str;

  g_return_val_if_fail (data != NULL, FALSE);
  *data = NULL;

  str = g_strdup_printf("(gnc:report-run %d)", report_id);
  scm_text = gfec_eval_string(str, error_handler);

  if (scm_text == SCM_UNDEFINED || !SCM_STRINGP (scm_text))
    return FALSE;

  free_data = SCM_STRING_CHARS (scm_text);
  *data = g_strdup (free_data);

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
  SCM    value;
  
  if (report == SCM_BOOL_F)
    return NULL;

  value = scm_call_1(get_name, report);
  if (!SCM_STRINGP(value))
    return NULL;

  return g_strdup(SCM_STRING_CHARS(value));
}

