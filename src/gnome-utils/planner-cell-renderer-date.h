/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
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
 */

#ifndef __PLANNER_CELL_RENDERER_DATE_H__
#define __PLANNER_CELL_RENDERER_DATE_H__

#include <glib-object.h>
#include <gtk/gtkwidget.h>
#include <libplanner/mrp-time.h>
#include <libplanner/mrp-types.h>
#include "planner-cell-renderer-popup.h"

#define PLANNER_TYPE_CELL_RENDERER_DATE	      (planner_cell_renderer_date_get_type ())
#define PLANNER_CELL_RENDERER_DATE(obj)	      (GTK_CHECK_CAST ((obj), PLANNER_TYPE_CELL_RENDERER_DATE, PlannerCellRendererDate))
#define PLANNER_CELL_RENDERER_DATE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), PLANNER_TYPE_CELL_RENDERER_DATE, PlannerCellRendererDateClass))
#define PLANNER_IS_CELL_RENDERER_DATE(obj)	      (GTK_CHECK_TYPE ((obj), PLANNER_TYPE_CELL_RENDERER_DATE))
#define PLANNER_IS_CELL_RENDERER_DATE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((obj), PLANNER_TYPE_CELL_RENDERER_DATE))
#define PLANNER_CELL_RENDERER_DATE_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), PLANNER_TYPE_CELL_RENDERER_DATE, PlannerCellRendererDateClass))

typedef struct _PlannerCellRendererDate      PlannerCellRendererDate;
typedef struct _PlannerCellRendererDateClass PlannerCellRendererDateClass;

struct _PlannerCellRendererDate
{
	PlannerCellRendererPopup  parent;
	GtkWidget                *calendar;
	GtkWidget                *option_menu;
	GtkWidget                *today_button;

	gboolean                  use_constraint;
	GtkWidget                *constraint_vbox;
	mrptime                   time;
	MrpConstraintType         type;
};

struct _PlannerCellRendererDateClass
{
	PlannerCellRendererPopupClass parent_class;
};

GType            planner_cell_renderer_date_get_type (void) G_GNUC_CONST;
GtkCellRenderer *planner_cell_renderer_date_new      (gboolean use_constraint);


#endif /* __PLANNER_CELL_RENDERER_DATE_H__ */
