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

#ifndef __GNC_POPUP_ENTRY_H__
#define __GNC_POPUP_ENTRY_H__

#include <pango/pango.h>
#include <gtk/gtkeventbox.h>

#define GNC_TYPE_POPUP_ENTRY            (gnc_popup_entry_get_type ())
#define GNC_POPUP_ENTRY(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_POPUP_ENTRY, GncPopupEntry))
#define GNC_POPUP_ENTRY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_POPUP_ENTRY, GncPopupEntryClass))
#define GNC_IS_POPUP_ENTRY(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_POPUP_ENTRY))
#define GNC_IS_POPUP_ENTRY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), GNC_TYPE_POPUP_ENTRY))
#define GNC_POPUP_ENTRY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_POPUP_ENTRY, GncPopupEntryClass))

typedef struct _GncPopupEntry      GncPopupEntry;
typedef struct _GncPopupEntryClass GncPopupEntryClass;

struct _GncPopupEntry
{
    GtkEventBox  parent;

    GtkWidget   *hbox;
    GtkWidget   *button;
    GtkWidget   *entry;

    gboolean     editing_canceled;
};

struct _GncPopupEntryClass
{
    GtkEventBoxClass parent_class;
};

GType        gnc_popup_entry_get_type   (void) G_GNUC_CONST;

GtkWidget   *gnc_popup_entry_new        (void);

void         gnc_popup_entry_set_text   (GncPopupEntry *popup,
                                         const gchar  *text);

const gchar *gnc_popup_entry_get_text   (GncPopupEntry *popup);

gint         gnc_popup_get_button_width (void);


#endif /* __GNC_POPUP_ENTRY_H__ */
