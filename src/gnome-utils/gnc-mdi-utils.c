/********************************************************************\
 * gnc-mdi-util.c -- utility functions for gnome/mdi                *
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

#include "config.h"

#include <gnome.h>

#include "gnc-ui.h"


gncUIWidget
gnc_ui_get_toplevel (void)
{
  GList *containers = gtk_container_get_toplevels ();
  GnomeApp *app = NULL;
  
  for (; containers; containers = containers->next)
  {
    GtkWidget *w = containers->data;
    GnomeMDI *mdi;

    if (!GNOME_IS_APP (w))
      continue;

    app = GNOME_APP (w);

    mdi = gtk_object_get_data (GTK_OBJECT (w), "gnc_mdi");
    if (!mdi)
      continue;

    app = gnome_mdi_get_active_window (mdi);

    break;
  }

  if (app)
    return GTK_WIDGET (app);

  return NULL;
}
