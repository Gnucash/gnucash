/********************************************************************
 * gnc-report.c -- C functions for reports.                         *
 *                                                                  *
 * Copyright (C) 2001 Linux Developers Group                        *
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

#include "gnc-report.h"

gboolean
gnc_run_report (int report_id, char ** data)
{
  const gchar *free_data;
  SCM run_report;
  SCM scm_text;

  g_return_val_if_fail (data != NULL, FALSE);
  *data = NULL;

  run_report = scm_c_eval_string ("gnc:report-run");

  scm_text = scm_call_1 (run_report, scm_int2num (report_id));
  if (!SCM_STRINGP (scm_text))
    return FALSE;

  free_data = SCM_STRING_CHARS (scm_text);
  *data = g_strdup (free_data);

  return TRUE;
}

gboolean
gnc_run_report_id_string (const char * id_string, char **data)
{
  int report_id;

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

