/********************************************************************\
 * window-report.h -- a report window for hypertext report.         *
 * Copyright (C) 1997 Robin D. Clark                                *
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
\********************************************************************/

#ifndef __GNC_REPORT_WINDOW_H__
#define __GNC_REPORT_WINDOW_H__

#include <gnome.h>
#include <guile/gh.h>

#include "gnc-html.h"
#include "dialog-options.h"
#include "window-main.h"
  
typedef struct _gnc_report_window gnc_report_window;

/** PROTOTYPES ******************************************************/

gnc_report_window * gnc_report_window_new(GNCMainChildInfo * mc);
void       gnc_report_window_destroy(gnc_report_window * rep);
void       gnc_report_window_show_report(gnc_report_window * rw, int id);
void       gnc_report_window_reload(gnc_report_window * rw);
gnc_html   * gnc_report_window_get_html(gnc_report_window * rw);
GtkWidget  * gnc_report_window_get_container(gnc_report_window * rw);
SCM        gnc_report_window_get_report(gnc_report_window * rw);

void       gnc_report_window_create_menu(gnc_report_window * report, 
                                         GNCMainChildInfo * child);
void       gnc_report_window_create_toolbar(gnc_report_window * report, 
                                            GNCMainChildInfo * child);

void       gnc_report_window_default_params_editor(SCM options, SCM report);

void       gnc_main_window_open_report (int report_id, gint toplevel);
void       gnc_main_window_open_report_url (const char * url, gint toplevel);

GnomeMDIChild * gnc_report_window_create_child(const gchar * url);
void       reportWindow(int id);
void       gnc_print_report (int report_id);

#endif
