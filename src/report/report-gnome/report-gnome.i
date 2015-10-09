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

%module sw_report_gnome
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gtk/gtk.h>
#include <gnc-plugin-page-report.h>
#include <window-report.h>
#include <dialog-custom-report.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_report_gnome_module (void);
%}
#endif

%import "base-typemaps.i"

void gnc_main_window_open_report(int report_id, GncMainWindow *window);
void gnc_ui_custom_report(GncMainWindow * window);
