/********************************************************************\
 * gnc-mdi-utils.h -- utility functions for gnome/mdi               *
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
 *                                                                  *
\********************************************************************/

#ifndef GNC_MDI_UTILS_H
#define GNC_MDI_UTILS_H

#include <gnome.h>

typedef struct
{
  GnomeMDI * mdi;
  int      component_id;
  SCM      toolbar_change_callback_id;
  SCM      mdi_change_callback_id;
  GList    * children;
} GNCMDIInfo;

typedef struct
{
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
} GNCMDIChildInfo;

#endif
