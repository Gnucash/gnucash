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

typedef struct  {
  GnomeMDI * mdi;
  int      component_id;
  SCM      toolbar_change_callback_id;
  SCM      mdi_change_callback_id;
  GList    * children;
} GNCMainInfo;

typedef struct {
  GnomeMDIChild   * child;
  GtkWidget       * contents;
  GnomeApp        * app;

  GtkWidget       * toolbar;  
  GnomeUIInfo     * toolbar_info;
  int             toolbar_size;
  GnomeUIInfo     * menu_info;

  int             component_id;
  void            * user_data;
  char            * title;
} GNCMainChildInfo;


GNCMainInfo   * gnc_main_window_new(void);
void            gnc_main_window_destroy(GNCMainInfo * wind); 
gboolean        gnc_main_window_can_save(GNCMainInfo * wind);
gboolean        gnc_main_window_can_cancel_save (GNCMainInfo *wind);
void            gnc_main_window_save(GNCMainInfo * wind, char * session);
void            gnc_main_window_restore(GNCMainInfo * wind,
                                        const char * session);
void            gnc_main_window_create_child_toolbar(GNCMainInfo * mi, 
                                                     GNCMainChildInfo * child);
void            gnc_main_window_add_child(GNCMainInfo * main,
                                          GNCMainChildInfo * child);
void            gnc_main_window_remove_child(GNCMainInfo * main,
                                             GNCMainChildInfo * child);
void            gnc_main_window_child_refresh(gpointer data);
GnomeMDIChild * gnc_main_window_create_child(const gchar * configstring);
void            gnc_main_window_close_children(GNCMainInfo * main);

#endif
