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
#define GNC_CELL_RENDERER_TEXT_VIEW(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_CELL_RENDERER_TEXT_VIEW, GncCellRendererTextView))
#define GNC_CELL_RENDERER_TEXT_VIEW_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_CELL_RENDERER_TEXT_VIEW, GncCellRendererTextViewClass))
#define GNC_IS_CELL_RENDERER_TEXT_VIEW(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_CELL_RENDERER_TEXT_VIEW))
#define GNC_IS_CELL_RENDERER_TEXT_VIEW_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), GNC_TYPE_CELL_RENDERER_TEXT_VIEW))
#define GNC_CELL_RENDERER_TEXT_VIEW_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_CELL_RENDERER_TEXT_VIEW, GncCellRendererTextViewClass))


typedef struct _GncCellRendererTextView      GncCellRendererTextView;
typedef struct _GncCellRendererTextViewClass GncCellRendererTextViewClass;

struct _GncCellRendererTextView
{
    GtkCellRendererText  parent;

    /* The editable entry. */
    GtkWidget *editable;
};

struct _GncCellRendererTextViewClass
{
    GtkCellRendererTextClass parent_class;
};

GType            gnc_cell_renderer_text_view_get_type (void) G_GNUC_CONST;

GtkCellRenderer *gnc_cell_renderer_text_view_new (void);


#endif /* __GNC_CELL_RENDERER_TEXT_VIEW_H__ */
