/* 
 * gnc-window.c -- structure which represents a GnuCash window.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include "gnc-window.h"


GType
gnc_window_get_type (void)
{
  static GType gnc_window_type = 0;

  if (gnc_window_type == 0) {
    static const GTypeInfo our_info = {
      sizeof (GncWindowIface),
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      0,
      0,
      NULL
    };

    gnc_window_type = g_type_register_static (G_TYPE_INTERFACE,
					      "GncWindow",
					      &our_info, 0);
    g_type_interface_add_prerequisite (gnc_window_type, G_TYPE_OBJECT);
  }

  return gnc_window_type;
}

GtkWidget *
gnc_window_get_statusbar (GncWindow *window)
{
  g_return_val_if_fail(GNC_WINDOW (window), NULL);

  /* mandatory */
  g_return_val_if_fail(GNC_WINDOW_GET_IFACE (window)->get_statusbar, NULL);

  return GNC_WINDOW_GET_IFACE (window)->get_statusbar (window);
}

GtkWidget *
gnc_window_get_progressbar (GncWindow *window)
{
  g_return_val_if_fail(GNC_WINDOW (window), NULL);

  /* optional */
  if (GNC_WINDOW_GET_IFACE (window)->get_progressbar == NULL)
    return NULL;

  return GNC_WINDOW_GET_IFACE (window)->get_progressbar (window);
}
