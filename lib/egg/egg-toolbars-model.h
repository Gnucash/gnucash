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

#ifndef EGG_TOOLBARS_MODEL_H
#define EGG_TOOLBARS_MODEL_H

#include <glib.h>
#include <glib-object.h>
#include <gdk/gdktypes.h>

G_BEGIN_DECLS

typedef struct EggToolbarsModelClass EggToolbarsModelClass;

#define EGG_TOOLBARS_MODEL_TYPE             (egg_toolbars_model_get_type ())
#define EGG_TOOLBARS_MODEL(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TOOLBARS_MODEL_TYPE, EggToolbarsModel))
#define EGG_TOOLBARS_MODEL_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TOOLBARS_MODEL_TYPE, EggToolbarsModelClass))
#define IS_EGG_TOOLBARS_MODEL(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TOOLBARS_MODEL_TYPE))
#define IS_EGG_TOOLBARS_MODEL_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), EGG_TOOLBARS_MODEL_TYPE))
#define EGG_TOOLBARS_MODEL_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), EGG_TOOLBARS_MODEL_TYPE, EggToolbarsModelClass))

typedef struct EggToolbarsModel EggToolbarsModel;
typedef struct EggToolbarsModelPrivate EggToolbarsModelPrivate;

#define EGG_TOOLBAR_ITEM_TYPE "application/x-toolbar-item"

typedef enum
{
  EGG_TB_MODEL_NOT_REMOVABLE = 1,
  EGG_TB_MODEL_ICONS_ONLY = 2
} EggTbModelFlags;

struct EggToolbarsModel
{
  GObject parent_object;
  EggToolbarsModelPrivate *priv;
};

struct EggToolbarsModelClass
{
  GObjectClass parent_class;

  /* Signals */
  void (* item_added)      (EggToolbarsModel *group,
			    int toolbar_position,
			    int position);
  void (* item_removed)    (EggToolbarsModel *group,
			    int toolbar_position,
			    int position);
  void (* toolbar_added)   (EggToolbarsModel *group,
			    int position);
  void (* toolbar_changed) (EggToolbarsModel *group,
			    int position);
  void (* toolbar_removed) (EggToolbarsModel *group,
			    int position);

  /* Virtual Table */
  gboolean (* add_item)    (EggToolbarsModel *t,
			    int	              toolbar_position,
			    int               position,
			    const char       *id,
			    const char       *type);
  char * (* get_item_type) (EggToolbarsModel *t,
			    GdkAtom           dnd_type);
  char * (* get_item_id)   (EggToolbarsModel *t,
			    const char       *type,
			    const char       *name);
  char * (* get_item_name) (EggToolbarsModel *t,
			    const char       *type,
			    const char       *id);
};

GType		  egg_toolbars_model_get_type       (void);
EggToolbarsModel *egg_toolbars_model_new	    (void);
void              egg_toolbars_model_load           (EggToolbarsModel *t,
						     const char *xml_file);
void              egg_toolbars_model_save           (EggToolbarsModel *t,
						     const char *xml_file,
						     const char *version);
int               egg_toolbars_model_add_toolbar    (EggToolbarsModel *t,
						     int               position,
						     const char       *name);
EggTbModelFlags   egg_toolbars_model_get_flags      (EggToolbarsModel *t,
						     int               toolbar_position);
void              egg_toolbars_model_set_flags      (EggToolbarsModel *t,
						     EggTbModelFlags   flags,
						     int               toolbar_position);
void              egg_toolbars_model_add_separator  (EggToolbarsModel *t,
						     int               toolbar_position,
						     int               position);
char             *egg_toolbars_model_get_item_type  (EggToolbarsModel *t,
				                     GdkAtom           dnd_type);
char             *egg_toolbars_model_get_item_id    (EggToolbarsModel *t,
						     const char       *type,
			                             const char       *name);
char             *egg_toolbars_model_get_item_name  (EggToolbarsModel *t,
						     const char       *type,
			                             const char       *id);
gboolean	  egg_toolbars_model_add_item       (EggToolbarsModel *t,
						     int	       toolbar_position,
				                     int               position,
						     const char       *id,
						     const char       *type);
void		  egg_toolbars_model_remove_toolbar (EggToolbarsModel *t,
						     int               position);
void		  egg_toolbars_model_remove_item    (EggToolbarsModel *t,
						     int               toolbar_position,
						     int               position);
int		  egg_toolbars_model_n_items	    (EggToolbarsModel *t,
						     int               toolbar_position);
const char	 *egg_toolbars_model_item_nth	    (EggToolbarsModel *t,
						     int	       toolbar_position,
						     int               position,
						     gboolean         *is_separator);
int		  egg_toolbars_model_n_toolbars	    (EggToolbarsModel *t);
const char	 *egg_toolbars_model_toolbar_nth    (EggToolbarsModel *t,
						     int               position);

G_END_DECLS

#endif
