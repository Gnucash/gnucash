/********************************************************************\
 * gnc-menu-extensions.h -- functions to build dynamic menus        *
 * Copyright (C) 1999 Rob Browning         	                    *
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

#ifndef GNC_MENU_EXTENSIONS_H
#define GNC_MENU_EXTENSIONS_H

#include <guile/gh.h>
#include <gnome.h>

void gnc_add_c_extension(GnomeUIInfo *info, gchar *path);
void gnc_add_scm_extension(SCM extension);
void gnc_extensions_menu_setup(GnomeApp * app);
void gnc_extensions_shutdown(void);

#endif
