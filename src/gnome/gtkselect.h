/* gtkselect.h - select widget for gtk+
 * Copyright 1997 Paolo Molaro (from gtkcombo, where this was copied from)
 * Copyright 2000 Gordon Oliver
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#ifndef __GTK_SMART_SELECT_H__
#define __GTK_SMART_SELECT_H__

#include <gtk/gtkhbox.h>
#include <gtk/gtkitem.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define GTK_SELECT(obj)		GTK_CHECK_CAST (obj, gtk_select_get_type (), GtkSelect)
#define GTK_SELECT_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gtk_select_get_type (), GtkSelectClass)
#define GTK_IS_SELECT(obj)       GTK_CHECK_TYPE (obj, gtk_select_get_type ())

typedef struct _GtkSelect		GtkSelect;
typedef struct _GtkSelectClass	GtkSelectClass;

/* you should access only the selected field directly */
struct _GtkSelect {
	GtkHBox hbox;
	GtkWidget *selected;

	GtkWidget *entry;
	GtkWidget *button;
	GtkWidget *popup;
	GtkWidget *popwin;
	GtkWidget *list;
	GtkWidget *empty;

	GList *entries;

	guint entry_change_id;
	guint list_change_id;

	guint use_arrows:1;

        guint16 current_button;
	guint activate_id;
};

struct _GtkSelectClass {
	GtkHBoxClass parent_class;
};

guint      gtk_select_get_type              (void);

GtkWidget *gtk_select_new                   (void);

/* set/unset arrows working for changing the value (can be annoying */
void       gtk_select_set_use_arrows        (GtkSelect*    select, 
                                            gint         val);

void       gtk_select_disable_activate      (GtkSelect*    select);

void	   gtk_select_insert_items	  (GtkSelect	    *select,
					   GList	    *items,
					   gint		     position);

void	   gtk_select_append_items	  (GtkSelect	    *select,
					   GList	    *items);

void	   gtk_select_prepend_items	  (GtkSelect	    *select,
					   GList	    *items);

void	   gtk_select_remove_items	  (GtkSelect	    *list,
					   GList	    *items);

void	   gtk_select_remove_items_no_unref (GtkSelect	    *list,
					   GList	    *items);

void	   gtk_select_clear_items	  (GtkSelect	    *list,
					   gint		     start,
					   gint		     end);

void	   gtk_select_select_item	  (GtkSelect	    *list,
					   gint		     item);

void	   gtk_select_select_child	  (GtkSelect	    *list,
					   GtkWidget	    *child);

gint	   gtk_select_child_position	  (GtkSelect	    *list,
					   GtkWidget	    *child);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_SMART_SELECT_H__ */
