/********************************************************************
 * gnc-cell-renderer-label.h -- A GtkCellRendererText subclass that
 * shows a selectable (but not editable) GtkLabel when activated,
 * allowing the user to select and copy cell text via Ctrl+C.
 *
 * Copyright (C) 2026 GnuCash contributors
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *******************************************************************/

#ifndef __GNC_CELL_RENDERER_LABEL_H__
#define __GNC_CELL_RENDERER_LABEL_H__

#include <gtk/gtk.h>

#define GNC_TYPE_CELL_RENDERER_LABEL (gnc_cell_renderer_label_get_type ())
G_DECLARE_FINAL_TYPE (GncCellRendererLabel, gnc_cell_renderer_label,
                      GNC, CELL_RENDERER_LABEL, GtkCellRendererText)

GtkCellRenderer *gnc_cell_renderer_label_new (void);

#endif /* __GNC_CELL_RENDERER_LABEL_H__ */
