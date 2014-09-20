/********************************************************************
 * gnc-report.h -- C functions for reports.                         *
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

#ifndef GNC_REPORT_H
#define GNC_REPORT_H

#include <glib.h>
#include <libguile.h>

#define SAVED_REPORTS_FILE "saved-reports-2.4"
#define SAVED_REPORTS_FILE_OLD_REV "saved-reports-2.0"

gboolean gnc_run_report (gint report_id, char ** data);
gboolean gnc_run_report_id_string (const char * id_string, char **data);

/**
 * @param report The SCM version of the report.
 * @return a caller-owned copy of the name of the report, or NULL if report
 * is invalid.
 **/
gchar* gnc_report_name( SCM report );

/* returns #f if the report id cannot be found */
SCM gnc_report_find(gint id);
void gnc_report_remove_by_id(gint id);
gint gnc_report_add(SCM report);

void gnc_reports_flush_global(void);
GHashTable *gnc_reports_get_global(void);

gchar* gnc_get_default_report_font_family(void);

gboolean gnc_saved_reports_backup (void);
gboolean gnc_saved_reports_write_to_file (const gchar* report_def, gboolean overwrite);

#endif
