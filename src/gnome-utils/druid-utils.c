/********************************************************************\
 * druid-utils.c -- utility functions for creating druids           *
 * Copyright (C) 2001 Jeremy Collins                                *
 * Copyright (C) 2001 Dave Peticolas <dave@krondo.com>              *
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

#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-engine-util.h"


void
gnc_druid_set_colors (GnomeDruid *druid)
{
  GList *pages = gtk_container_get_children (GTK_CONTAINER (druid));
  GdkColor color;
  GdkColormap *cm;

  if (!druid) return;
  if (!GNOME_IS_DRUID (druid)) return;

  color.red =   (gushort) (.60 * 65535);
  color.green = (gushort) (.75 * 65535);
  color.blue =  (gushort) (.60 * 65535);

  cm = gtk_widget_get_colormap (GTK_WIDGET (druid));

  gdk_colormap_alloc_color(cm, &color, FALSE, TRUE);

  while (pages != NULL)
  {
    GnomeDruidPage *page = GNOME_DRUID_PAGE (pages->data);

    if (GNOME_IS_DRUID_PAGE_EDGE (page))
    {
      GnomeDruidPageEdge *page_edge;

      page_edge = GNOME_DRUID_PAGE_EDGE (page);
      gnome_druid_page_edge_set_bg_color (page_edge, &color);
      gnome_druid_page_edge_set_logo_bg_color (page_edge, &color);
    }
    else 
    {
      GnomeDruidPageStandard *page_standard;

      page_standard = GNOME_DRUID_PAGE_STANDARD (page);
      gnome_druid_page_standard_set_background (page_standard, &color);
      gnome_druid_page_standard_set_logo_background (page_standard, &color);
    }
    
    pages = g_list_next (pages);
  }
}
