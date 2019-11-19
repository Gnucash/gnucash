/*************************************************************************
 * The following code has come from Planner. This code implements a
 * GtkCalendar in a custom GtkCellEditable popup from GtkCellRenderer.
 *
 * These files have been renamed and changed to remove code not required
 * and to remove a dependency on libplanner.
 *
 * Copyright (C) 2012 Robert Fewell
 *
 * Copyright (C) 2001-2002 CodeFactory AB
 * Copyright (C) 2001-2002 Richard Hult <richard@imendio.com>
 * Copyright (C) 2001-2002 Mikael Hallendal <micke@imendio.com>
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

#ifndef __GNC_CELL_RENDERER_POPUP_H__
#define __GNC_CELL_RENDERER_POPUP_H__

#include <pango/pango.h>
#include <gtk/gtkcellrenderertext.h>

#define GNC_TYPE_CELL_RENDERER_POPUP            (gnc_cell_renderer_popup_get_type ())
#define GNC_CELL_RENDERER_POPUP(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_CELL_RENDERER_POPUP, GncCellRendererPopup))
#define GNC_CELL_RENDERER_POPUP_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_CELL_RENDERER_POPUP, GncCellRendererPopupClass))
#define GNC_IS_CELL_RENDERER_POPUP(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_CELL_RENDERER_POPUP))
#define GNC_IS_CELL_RENDERER_POPUP_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), GNC_TYPE_CELL_RENDERER_POPUP))
#define GNC_CELL_RENDERER_POPUP_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_CELL_RENDERER_POPUP, GncCellRendererPopupClass))

typedef struct _GncCellRendererPopup      GncCellRendererPopup;
typedef struct _GncCellRendererPopupClass GncCellRendererPopupClass;

struct _GncCellRendererPopup
{
    GtkCellRendererText  parent;

    /* Cached width of the popup button. */
    gint                 button_width;

    /* The popup window. */
    GtkWidget           *popup_window;

    /* The widget that should grab focus on popup. */
    GtkWidget           *focus_window;

    /* The editable entry. */
    GtkWidget           *editable;

    gboolean             shown;
    gboolean             editing_canceled;
    gchar               *cell_text;
};

struct _GncCellRendererPopupClass
{
    GtkCellRendererTextClass parent_class;

    void   (* show_popup) (GncCellRendererPopup *cell,
                           const gchar          *path,
                           gint                  x1,
                           gint                  y1,
                           gint                  x2,
                           gint                  y2);

    void   (* hide_popup) (GncCellRendererPopup *cell);
};

GType gnc_cell_renderer_popup_get_type (void) G_GNUC_CONST;

GtkCellRenderer *gnc_cell_renderer_popup_new (void);

void gnc_cell_renderer_popup_show (GncCellRendererPopup *cell,
                                   const gchar          *path,
                                   gint                  x1,
                                   gint                  y1,
                                   gint                  x2,
                                   gint                  y2);

void gnc_cell_renderer_popup_hide (GncCellRendererPopup *cell);

#endif /* __GNC_CELL_RENDERER_POPUP_H__ */
