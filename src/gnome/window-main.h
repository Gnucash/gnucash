/********************************************************************
 * window-main.h -- public GNOME main window functions              *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
 * Copyright (C) 2001 Bill Gribble <grib@gnumatic.com>              *
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

#ifndef WINDOW_MAIN_H
#define WINDOW_MAIN_H

#include <gnome.h>
#include <guile/gh.h>

#include "gnc-mdi-utils.h"

GNCMDIInfo    * gnc_main_window_new(void);
void            gnc_main_window_destroy(GNCMDIInfo * wind); 
gboolean        gnc_main_window_can_save(GNCMDIInfo * wind);
gboolean        gnc_main_window_can_cancel_save (GNCMDIInfo *wind);
void            gnc_main_window_save(GNCMDIInfo * wind, char * session);
void            gnc_main_window_restore(GNCMDIInfo * wind,
                                        const char * session);
void            gnc_main_window_create_child_toolbar(GNCMDIInfo * mi, 
                                                     GNCMDIChildInfo * child);
GnomeMDIChild * gnc_main_window_create_child(const gchar * configstring);
void            gnc_main_window_close_children(GNCMDIInfo * main);

#endif
