/*
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

#ifndef __GNC_CELL_VIEW_H__
#define __GNC_CELL_VIEW_H__
#include <gtk/gtk.h>
G_BEGIN_DECLS
#define GNC_TYPE_CELL_VIEW (gnc_cell_view_get_type ())
G_DECLARE_FINAL_TYPE (GncCellView, gnc_cell_view, GNC, CELL_VIEW, GtkBox)
GtkWidget *gnc_cell_view_new (void);
void gnc_cell_view_set_text (GncCellView *view, const gchar *text);
gchar *gnc_cell_view_get_text (GncCellView *view);
G_END_DECLS
#endif
