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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_REPORT_WINDOW_H
#define GNC_REPORT_WINDOW_H

#include <libguile.h>

//#include "gnc-html.h"
#include "qof.h"

typedef struct gnc_report_window_s gnc_report_window;

/** PROTOTYPES ******************************************************/

// scm-exposed
GtkWidget * gnc_report_window_default_params_editor(SCM options, SCM report);

// called from multiple places
// [business-gnome/dialog-invoice.c;gnome/window-register.c]; and
// scm-exposed; 3-liner which calls gnc_main_window_open_report after handling busy-cursor.
void       reportWindow(int id);
gboolean   gnc_report_edit_options(SCM report);
// module[/plugin]-init
void       gnc_report_init (void);

#endif
