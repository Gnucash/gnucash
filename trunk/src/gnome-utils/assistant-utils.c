/********************************************************************\
 * assistant-utils.c -- utility functions for creating assistants   *
 * Copyright (C) 2001 Jeremy Collins                                *
 * Copyright (C) 2001 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2010 Geert Janssens                                *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>

#include "dialog-utils.h"
#include "assistant-utils.h"
#include "qof.h"
#include "gnc-gnome-utils.h"

void
gnc_assistant_set_colors (GtkAssistant *assistant)
{
    GdkColor bluish;
    GdkColor white;
    GdkColormap *cm;

    if (!assistant) return;

    bluish.red =   (gushort) (.40 * 65535);
    bluish.green = (gushort) (.40 * 65535);
    bluish.blue =  (gushort) (.60 * 65535);

    white.red =   65535;
    white.green = 65535;
    white.blue =  65535;

    cm = gtk_widget_get_colormap (GTK_WIDGET (assistant));

    gdk_colormap_alloc_color(cm, &bluish, FALSE, TRUE);
    gdk_colormap_alloc_color(cm, &white, FALSE, TRUE);

}
