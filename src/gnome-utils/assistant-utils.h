/********************************************************************\
 * assistant-utils.h -- utility functions for creating druids       *
 * Copyright (C) 2001 Jeremy Collins                                *
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

#ifndef ASSISTANT_UTILS_H
#define ASSISTANT_UTILS_H

#include <gtk/gtk.h>

void gnc_assistant_set_watermark_images (GtkAssistant *assistant,
                                     const char *top_path,
                                     const char *side_path);
void gnc_assistant_set_colors (GtkAssistant *assistant);

#endif
