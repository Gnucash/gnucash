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

#ifndef EGG_TOOLBAR_EDITOR_H
#define EGG_TOOLBAR_EDITOR_H

#include <gtk/gtkvbox.h>

#include "egg-menu-merge.h"
#include "egg-toolbars-model.h"

G_BEGIN_DECLS

typedef struct EggToolbarEditorClass EggToolbarEditorClass;

#define EGG_TOOLBAR_EDITOR_TYPE             (egg_toolbar_editor_get_type ())
#define EGG_TOOLBAR_EDITOR(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TOOLBAR_EDITOR_TYPE, EggToolbarEditor))
#define EGG_TOOLBAR_EDITOR_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TOOLBAR_EDITOR_TYPE, EggToolbarEditorClass))
#define IS_EGG_TOOLBAR_EDITOR(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TOOLBAR_EDITOR_TYPE))
#define IS_EGG_TOOLBAR_EDITOR_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), EGG_TOOLBAR_EDITOR_TYPE))
#define EGG_TOOLBAR_EDITOR_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), EGG_TOOLBAR_EDITOR_TYPE, EggToolbarEditorClass))


typedef struct EggToolbarEditor EggToolbarEditor;
typedef struct EggToolbarEditorPrivate EggToolbarEditorPrivate;

struct EggToolbarEditor
{
  GtkVBox parent_object;
  EggToolbarEditorPrivate *priv;
};

struct EggToolbarEditorClass
{
  GtkVBoxClass parent_class;
};


GType             egg_toolbar_editor_get_type     (void);
GtkWidget        *egg_toolbar_editor_new          (EggMenuMerge *merge,
						   EggToolbarsModel *model);
void              egg_toolbar_editor_load_actions (EggToolbarEditor *editor,
						   const char       *xml_file);

G_END_DECLS

#endif
