/********************************************************************\
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
 *                                                                  *
\********************************************************************/

%module sw_report_system
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gnc-report.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_report_system_module (void);
%}
#endif

%import "base-typemaps.i"

SCM gnc_report_find(gint id);
gint gnc_report_add(SCM report);

%newobject gnc_get_default_report_font_family;
gchar* gnc_get_default_report_font_family();

void gnc_saved_reports_backup (void);
gboolean gnc_saved_reports_write_to_file (const gchar* report_def, gboolean overwrite);