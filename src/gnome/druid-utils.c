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


/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_GUI;

void
gnc_druid_set_title_image (GnomeDruid *druid, char *image_path)
{
  GtkWidget       *canvas;
  GnomeCanvasItem *item;
  GnomeCanvasItem *title_item;
  GdkImlibImage   *image;
  GList           *pages = GNOME_DRUID(druid)->children;
	
  while(pages != NULL) {

    image = gnc_get_gdk_imlib_image(image_path); 

    if (g_list_previous(pages) == NULL) {
      canvas     = GNOME_DRUID_PAGE_START(pages->data)->canvas;
      title_item = GNOME_DRUID_PAGE_START(pages->data)->title_item;
    } else if (g_list_next(pages) == NULL) {
      canvas     = GNOME_DRUID_PAGE_FINISH(pages->data)->canvas;
      title_item = GNOME_DRUID_PAGE_FINISH(pages->data)->title_item;
    } else {
      canvas     = GNOME_DRUID_PAGE_STANDARD(pages->data)->canvas;
      title_item = GNOME_DRUID_PAGE_STANDARD(pages->data)->title_item;
    }

    item = gnome_canvas_item_new (gnome_canvas_root (GNOME_CANVAS (canvas)),
                                  gnome_canvas_image_get_type (),
                                  "image", image,
                                  "x", 0.0,
                                  "y", 0.0,
                                  "anchor", GTK_ANCHOR_NORTH_WEST,
                                  "width", (gfloat) 462,
                                  "height", (gfloat) 67,
                                  NULL);

    gnome_canvas_item_raise_to_top (title_item);

    pages = g_list_next(pages);
  }
}

void
gnc_druid_set_watermark_image (GnomeDruid *druid, char *image_path)
{
  GdkImlibImage *image;
  GList         *pages = GNOME_DRUID(druid)->children;

  while(pages != NULL) {

    image = gnc_get_gdk_imlib_image(image_path); 

    if (g_list_previous(pages) == NULL) {
      gnome_druid_page_start_set_watermark
        (GNOME_DRUID_PAGE_START(pages->data), image);
    } else if (g_list_next(pages) == NULL) {
      gnome_druid_page_finish_set_watermark
        (GNOME_DRUID_PAGE_FINISH(pages->data), image);
    } 

    pages = g_list_next(pages);
  }
}

void
gnc_druid_set_logo_image (GnomeDruid *druid, char *image_path)
{
  GdkImlibImage   *image;
  GList           *pages = GNOME_DRUID(druid)->children;
	
  while(pages != NULL) {

    image = gnc_get_gdk_imlib_image(image_path); 

    if (g_list_previous(pages) == NULL) {
      gnome_druid_page_start_set_logo
        (GNOME_DRUID_PAGE_START(pages->data), image);
    } else if (g_list_next(pages) == NULL) {
      gnome_druid_page_finish_set_logo
        (GNOME_DRUID_PAGE_FINISH(pages->data), image);
    } else {
      gnome_druid_page_standard_set_logo
        (GNOME_DRUID_PAGE_STANDARD(pages->data), image);
    }

    pages = g_list_next(pages);
  }
}

void
gnc_druid_set_colors (GnomeDruid *druid)
{
  GList *pages;
  GdkColor color;
  GdkColormap *cm;

  if (!druid) return;
  if (!GNOME_IS_DRUID (druid)) return;

  color.red =   (gushort) (.60 * 65535);
  color.green = (gushort) (.75 * 65535);
  color.blue =  (gushort) (.60 * 65535);

  cm = gtk_widget_get_colormap (GTK_WIDGET (druid));

  gdk_colormap_alloc_color(cm, &color, FALSE, TRUE);

  pages = GNOME_DRUID(druid)->children;

  while (pages != NULL)
  {
    GnomeDruidPage *page = GNOME_DRUID_PAGE (pages->data);

    if (GNOME_IS_DRUID_PAGE_START (page))
    {
      GnomeDruidPageStart *page_start;

      page_start = GNOME_DRUID_PAGE_START (page);

      gnome_druid_page_start_set_bg_color (page_start, &color);
      gnome_druid_page_start_set_logo_bg_color (page_start, &color);
    }
    else if (GNOME_IS_DRUID_PAGE_STANDARD (page))
    {
      GnomeDruidPageStandard *page_standard;

      page_standard = GNOME_DRUID_PAGE_STANDARD (page);

      gnome_druid_page_standard_set_bg_color (page_standard, &color);
      gnome_druid_page_standard_set_logo_bg_color (page_standard, &color);
    }
    else if (GNOME_IS_DRUID_PAGE_FINISH (page))
    {
      GnomeDruidPageFinish *page_finish;

      page_finish = GNOME_DRUID_PAGE_FINISH (page);

      gnome_druid_page_finish_set_bg_color (page_finish, &color);
      gnome_druid_page_finish_set_logo_bg_color (page_finish, &color);
    }

    pages = g_list_next (pages);
  }
}
