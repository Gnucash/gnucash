/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 * GtkToolbar copyright (C) Federico Mena
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#ifndef __EGG_TOOLBAR_H__
#define __EGG_TOOLBAR_H__

#include <gdk/gdk.h>
#include <gtk/gtkcontainer.h>
#include <gtk/gtkenums.h>
#include <gtk/gtktooltips.h>

#include "eggtoolitem.h"

#ifndef GTK_DISABLE_DEPRECATED

/* Not needed, retained for compatibility -Yosh */
#include <gtk/gtkpixmap.h>
#include <gtk/gtksignal.h>

#endif

G_BEGIN_DECLS

#define EGG_TYPE_TOOLBAR                  (egg_toolbar_get_type ())
#define EGG_TOOLBAR(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_TOOLBAR, EggToolbar))
#define EGG_TOOLBAR_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_TOOLBAR, EggToolbarClass))
#define EGG_IS_TOOLBAR(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_TOOLBAR))
#define EGG_IS_TOOLBAR_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), EGG_TYPE_TOOLBAR))
#define EGG_TOOLBAR_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), EGG_TYPE_TOOLBAR, EggToolbarClass))

#ifndef EGG_DISABLE_DEPRECATED
typedef enum
{
  EGG_TOOLBAR_CHILD_SPACE,
  EGG_TOOLBAR_CHILD_BUTTON,
  EGG_TOOLBAR_CHILD_TOGGLEBUTTON,
  EGG_TOOLBAR_CHILD_RADIOBUTTON,
  EGG_TOOLBAR_CHILD_WIDGET
} EggToolbarChildType;

typedef struct _EggToolbarChild	     EggToolbarChild;

struct _EggToolbarChild
{
  EggToolbarChildType type;
  GtkWidget *widget;
  GtkWidget *icon;
  GtkWidget *label;
};

typedef enum
{
  EGG_TOOLBAR_SPACE_EMPTY,
  EGG_TOOLBAR_SPACE_LINE
} EggToolbarSpaceStyle;

#endif /* EGG_DISABLE_DEPRECATED */

typedef struct _EggToolbar           EggToolbar;
typedef struct _EggToolbarClass      EggToolbarClass;

struct _EggToolbar
{
  GtkContainer container;

  gint             num_children;
  GList           *children;
  GtkOrientation   orientation;
  GtkToolbarStyle  style;
  GtkIconSize      icon_size;

  GtkTooltips     *tooltips;
  
  gint             button_maxw;		/* maximum width of homogeneous children */
  gint             button_maxh;		/* maximum height of homogeneous children */

  guint            style_set_connection;
  guint            icon_size_connection;

  guint            style_set : 1;
  guint            icon_size_set : 1;
};

struct _EggToolbarClass
{
  GtkContainerClass parent_class;

  /* signals */
  void (* orientation_changed) (EggToolbar       *toolbar,
				GtkOrientation    orientation);
  void (* style_changed)       (EggToolbar       *toolbar,
				GtkToolbarStyle   style);
  void (* popup_context_menu)  (EggToolbar       *toolbar);

  /* keybinding signals -- these should go away/become padding when we become part of gtk+ */
  gboolean (* move_focus)          (EggToolbar       *toolbar,
				    GtkDirectionType  dir);
  gboolean (* focus_ends)          (EggToolbar       *toolbar,
				    gboolean          home);

  /* Padding for future expansion */
  void (*_gtk_reserved1) (void);
};

GType           egg_toolbar_get_type        (void) G_GNUC_CONST;
GtkWidget*      egg_toolbar_new             (void);

void            egg_toolbar_insert           (EggToolbar      *toolbar,
					      EggToolItem     *item,
					      gint             pos);
gint            egg_toolbar_get_item_index   (EggToolbar      *toolbar,
					      EggToolItem     *item);
gint		egg_toolbar_get_n_items      (EggToolbar      *toolbar);
EggToolItem *   egg_toolbar_get_nth_item     (EggToolbar      *toolbar,
					      gint             n);
gint            egg_toolbar_get_drop_index   (EggToolbar      *toolbar,
					      gint             x,
					      gint             y);
void            egg_toolbar_set_show_arrow   (EggToolbar      *toolbar,
					      gboolean         show_arrow);
void            egg_toolbar_set_orientation  (EggToolbar      *toolbar,
					      GtkOrientation   orientation);
void            egg_toolbar_set_tooltips     (EggToolbar      *toolbar,
					      gboolean         enable);
void            egg_toolbar_unset_icon_size  (EggToolbar      *toolbar);
gboolean        egg_toolbar_get_show_arrow   (EggToolbar      *toolbar);
GtkOrientation  egg_toolbar_get_orientation  (EggToolbar      *toolbar);
GtkToolbarStyle egg_toolbar_get_style        (EggToolbar      *toolbar);
GtkIconSize     egg_toolbar_get_icon_size    (EggToolbar      *toolbar);
gboolean        egg_toolbar_get_tooltips     (EggToolbar      *toolbar);
GtkReliefStyle  egg_toolbar_get_relief_style (EggToolbar      *toolbar);

#ifndef EGG_DISABLE_DEPRECATED
/* Simple button items */
void       egg_toolbar_set_style     (EggToolbar      *toolbar,
				      GtkToolbarStyle  style);
void       egg_toolbar_set_icon_size (EggToolbar      *toolbar,
				      GtkIconSize      icon_size);
void       egg_toolbar_unset_style   (EggToolbar      *toolbar);
GtkWidget* egg_toolbar_append_item   (EggToolbar      *toolbar,
				      const char      *text,
				      const char      *tooltip_text,
				      const char      *tooltip_private_text,
				      GtkWidget       *icon,
				      GtkSignalFunc    callback,
				      gpointer         user_data);
GtkWidget* egg_toolbar_prepend_item  (EggToolbar      *toolbar,
				      const char      *text,
				      const char      *tooltip_text,
				      const char      *tooltip_private_text,
				      GtkWidget       *icon,
				      GtkSignalFunc    callback,
				      gpointer         user_data);
GtkWidget* egg_toolbar_insert_item   (EggToolbar      *toolbar,
				      const char      *text,
				      const char      *tooltip_text,
				      const char      *tooltip_private_text,
				      GtkWidget       *icon,
				      GtkSignalFunc    callback,
				      gpointer         user_data,
				      gint             position);

/* Stock Items */
GtkWidget* egg_toolbar_insert_stock    (EggToolbar      *toolbar,
					const gchar     *stock_id,
					const char      *tooltip_text,
					const char      *tooltip_private_text,
					GtkSignalFunc    callback,
					gpointer         user_data,
					gint             position);

/* Space Items */
void       egg_toolbar_append_space    (EggToolbar      *toolbar);
void       egg_toolbar_prepend_space   (EggToolbar      *toolbar);
void       egg_toolbar_insert_space    (EggToolbar      *toolbar,
					gint             position);
void       egg_toolbar_remove_space    (EggToolbar      *toolbar,
                                        gint             position);
/* Any element type */
GtkWidget* egg_toolbar_append_element  (EggToolbar      *toolbar,
					EggToolbarChildType type,
					GtkWidget       *widget,
					const char      *text,
					const char      *tooltip_text,
					const char      *tooltip_private_text,
					GtkWidget       *icon,
					GtkSignalFunc    callback,
					gpointer         user_data);

GtkWidget* egg_toolbar_prepend_element (EggToolbar      *toolbar,
					EggToolbarChildType type,
					GtkWidget       *widget,
					const char      *text,
					const char      *tooltip_text,
					const char      *tooltip_private_text,
					GtkWidget       *icon,
					GtkSignalFunc    callback,
					gpointer         user_data);

GtkWidget* egg_toolbar_insert_element  (EggToolbar      *toolbar,
					EggToolbarChildType type,
					GtkWidget       *widget,
					const char      *text,
					const char      *tooltip_text,
					const char      *tooltip_private_text,
					GtkWidget       *icon,
					GtkSignalFunc    callback,
					gpointer         user_data,
					gint             position);

/* Generic Widgets */
void       egg_toolbar_append_widget   (EggToolbar      *toolbar,
					GtkWidget       *widget,
					const char      *tooltip_text,
					const char      *tooltip_private_text);
void       egg_toolbar_prepend_widget  (EggToolbar      *toolbar,
					GtkWidget       *widget,
					const char      *tooltip_text,
					const char	*tooltip_private_text);
void       egg_toolbar_insert_widget   (EggToolbar      *toolbar,
					GtkWidget       *widget,
					const char      *tooltip_text,
					const char      *tooltip_private_text,
					gint             position);

#endif /* EGG_DISABLE_DEPRECATED */


G_END_DECLS

#endif /* __EGG_TOOLBAR_H__ */
