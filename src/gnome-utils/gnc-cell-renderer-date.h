/*************************************************************************
 * The following code has come from Planner. This code implements a
 * GtkCalendar in a custom GtkCellEditable popup from GtkCellRenderer.
 *
 * These files have been renamed and changed to remove code not required
 * and to remove a dependency on libplanner.
 *
 * Copyright (C) 2012 Robert Fewell
 *
 * Copyright (C) 2005 Imendio AB
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
 **************************************************************************/

#ifndef __GNC_CELL_RENDERER_DATE_H__
#define __GNC_CELL_RENDERER_DATE_H__

#include <glib-object.h>
#include <gtk/gtkwidget.h>
#include <gnc-date.h>
#include "gnc-cell-renderer-popup.h"
#include "gnc-cell-renderer-popup-entry.h"

#define GNC_TYPE_CELL_RENDERER_DATE	       (gnc_cell_renderer_date_get_type ())
#define GNC_CELL_RENDERER_DATE(obj)	       (GTK_CHECK_CAST ((obj), GNC_TYPE_CELL_RENDERER_DATE, GncCellRendererDate))
#define GNC_CELL_RENDERER_DATE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GNC_TYPE_CELL_RENDERER_DATE, GncCellRendererDateClass))
#define GNC_IS_CELL_RENDERER_DATE(obj)	       (GTK_CHECK_TYPE ((obj), GNC_TYPE_CELL_RENDERER_DATE))
#define GNC_IS_CELL_RENDERER_DATE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((obj), GNC_TYPE_CELL_RENDERER_DATE))
#define GNC_CELL_RENDERER_DATE_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), GNC_TYPE_CELL_RENDERER_DATE, GncCellRendererDateClass))

typedef struct _GncCellRendererDate      GncCellRendererDate;
typedef struct _GncCellRendererDateClass GncCellRendererDateClass;

struct _GncCellRendererDate
{
    GncCellRendererPopup      parent;
    GtkWidget                *calendar;
    GtkWidget                *today_button;

    time64                    time;
    gboolean                  use_buttons;
    GtkWidget                *button_box;

};

struct _GncCellRendererDateClass
{
    GncCellRendererPopupClass parent_class;
};

GType            gnc_cell_renderer_date_get_type (void) G_GNUC_CONST;
GtkCellRenderer *gnc_cell_renderer_date_new      (gboolean use_buttons);


#endif /* __GNC_CELL_RENDERER_DATE_H__ */
