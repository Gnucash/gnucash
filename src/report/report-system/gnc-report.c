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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include "config.h"

#include <glib.h>
#include <guile/gh.h>
#include <string.h>

#include "gnc-report.h"

gboolean
gnc_run_report (int report_id, char ** data)
{
  char *free_data;
  SCM run_report;
  SCM scm_text;

  g_return_val_if_fail (data != NULL, FALSE);
  *data = NULL;

  run_report = gh_eval_str ("gnc:report-run");

  scm_text = gh_call1 (run_report, gh_int2scm (report_id));
  if (!gh_string_p (scm_text))
    return FALSE;

  free_data = gh_scm2newstr (scm_text, NULL);
  *data = g_strdup (free_data);
  if (free_data) free (free_data);

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
