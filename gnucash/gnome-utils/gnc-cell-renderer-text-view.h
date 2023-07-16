/*************************************************************************
 * The following code implements a text view in a custom GtkCellRenderer.
 *
 * Copyright (C) 2020 Robert Fewell
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *************************************************************************/

#ifndef __GNC_CELL_RENDERER_TEXT_VIEW_H__
#define __GNC_CELL_RENDERER_TEXT_VIEW_H__

#include <pango/pango.h>
#include <gtk/gtk.h>

#define GNC_TYPE_CELL_RENDERER_TEXT_VIEW            (gnc_cell_renderer_text_view_get_type ())
G_DECLARE_FINAL_TYPE (GncCellRendererTextView, gnc_cell_renderer_text_view, GNC, CELL_RENDERER_TEXT_VIEW, GtkCellRendererText)

GtkCellRenderer *gnc_cell_renderer_text_view_new (void);


#endif /* __GNC_CELL_RENDERER_TEXT_VIEW_H__ */
