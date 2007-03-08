/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
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

#ifndef __PLANNER_POPUP_ENTRY_H__
#define __PLANNER_POPUP_ENTRY_H__

#include <pango/pango.h>
#include <gtk/gtkeventbox.h>

#define PLANNER_TYPE_POPUP_ENTRY		(planner_popup_entry_get_type ())
#define PLANNER_POPUP_ENTRY(obj)		(GTK_CHECK_CAST ((obj), PLANNER_TYPE_POPUP_ENTRY, PlannerPopupEntry))
#define PLANNER_POPUP_ENTRY_CLASS(klass)	(GTK_CHECK_CLASS_CAST ((klass), PLANNER_TYPE_POPUP_ENTRY, PlannerPopupEntryClass))
#define PLANNER_IS_POPUP_ENTRY(obj)		(GTK_CHECK_TYPE ((obj), PLANNER_TYPE_POPUP_ENTRY))
#define PLANNER_IS_POPUP_ENTRY_CLASS(klass)	(GTK_CHECK_CLASS_TYPE ((obj), PLANNER_TYPE_POPUP_ENTRY))
#define PLANNER_POPUP_ENTRY_GET_CLASS(obj)	(GTK_CHECK_GET_CLASS ((obj), PLANNER_TYPE_POPUP_ENTRY, PlannerPopupEntryClass))

typedef struct _PlannerPopupEntry      PlannerPopupEntry;
typedef struct _PlannerPopupEntryClass PlannerPopupEntryClass;

struct _PlannerPopupEntry
{
	GtkEventBox  parent;

	GtkWidget   *hbox;
	GtkWidget   *button;
	GtkWidget   *entry;

	gboolean     editing_canceled;
};

struct _PlannerPopupEntryClass
{
	GtkEventBoxClass parent_class;
};

GtkType      planner_popup_entry_get_type   (void) G_GNUC_CONST;

GtkWidget   *planner_popup_entry_new        (void);

void         planner_popup_entry_set_text   (PlannerPopupEntry *popup,
					const gchar  *text);

const gchar *planner_popup_entry_get_text   (PlannerPopupEntry *popup);

gint         planner_popup_get_button_width (void);


#endif /* __PLANNER_POPUP_ENTRY_H__ */
