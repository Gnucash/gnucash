/*
 *  Copyright (C) 2003 Marco Pesenti Gritti
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef EGG_EDITABLE_TOOLBAR_H
#define EGG_EDITABLE_TOOLBAR_H

#include "egg-toolbars-model.h"
#include "egg-menu-merge.h"

#include <gtk/gtkvbox.h>

G_BEGIN_DECLS

typedef struct EggEditableToolbarClass EggEditableToolbarClass;

#define EGG_EDITABLE_TOOLBAR_TYPE             (egg_editable_toolbar_get_type ())
#define EGG_EDITABLE_TOOLBAR(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_EDITABLE_TOOLBAR_TYPE, EggEditableToolbar))
#define EGG_EDITABLE_TOOLBAR_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_EDITABLE_TOOLBAR_TYPE, EggEditableToolbarClass))
#define IS_EGG_EDITABLE_TOOLBAR(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_EDITABLE_TOOLBAR_TYPE))
#define IS_EGG_EDITABLE_TOOLBAR_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), EGG_EDITABLE_TOOLBAR_TYPE))
#define EGG_EDITABLE_TOOLBAR_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), EGG_EDITABLE_TOOLBAR_TYPE, EggEditableToolbarClass))


typedef struct EggEditableToolbar EggEditableToolbar;
typedef struct EggEditableToolbarPrivate EggEditableToolbarPrivate;

#define EGG_TOOLBAR_ITEM_TYPE "application/x-toolbar-item"

struct EggEditableToolbar
{
  GtkVBox parent_object;
  EggEditableToolbarPrivate *priv;
};

struct EggEditableToolbarClass
{
  GtkVBoxClass parent_class;

  void (* action_request) (EggEditableToolbar *etoolbar,
			   char *action_name);
};

GType               egg_editable_toolbar_get_type        (void);
GtkWidget	   *egg_editable_toolbar_new		 (EggMenuMerge         *merge,
							  EggToolbarsModel     *model);
void		    egg_editable_toolbar_set_edit_mode	 (EggEditableToolbar   *etoolbar,
							  gboolean              mode);
void		    egg_editable_toolbar_show		 (EggEditableToolbar   *etoolbar,
							  const char           *name);
void		    egg_editable_toolbar_hide		 (EggEditableToolbar   *etoolbar,
							  const char           *name);
void		    egg_editable_toolbar_set_drag_dest   (EggEditableToolbar   *etoolbar,
							  const GtkTargetEntry *targets,
							  gint                  n_targets,
							  const char           *toolbar_name);

/* Private Functions */

GtkWidget 	   *_egg_editable_toolbar_new_separator_image (void);

G_END_DECLS

#endif
