/*
 *  Copyright (C) 2002 Marco Pesenti Gritti
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

#include "egg-toolbars-model.h"
#include "eggmarshalers.h"

#include <string.h>
#include <libxml/tree.h>
#include <gdk/gdkproperty.h>

static void egg_toolbars_model_class_init (EggToolbarsModelClass *klass);
static void egg_toolbars_model_init       (EggToolbarsModel      *t);
static void egg_toolbars_model_finalize   (GObject               *object);

enum
{
  ITEM_ADDED,
  ITEM_REMOVED,
  TOOLBAR_ADDED,
  TOOLBAR_CHANGED,
  TOOLBAR_REMOVED,
  LAST_SIGNAL
};

typedef struct
{
  char *name;
  EggTbModelFlags flags;
} EggToolbarsToolbar;

typedef struct
{
  char *id;
  char *type;
  gboolean separator;
} EggToolbarsItem;

static guint egg_toolbars_model_signals[LAST_SIGNAL] = { 0 };

static GObjectClass *parent_class = NULL;

struct EggToolbarsModelPrivate
{
  GNode *toolbars;
};

GType
egg_toolbars_model_get_type (void)
{
  static GType egg_toolbars_model_type = 0;

  if (egg_toolbars_model_type == 0)
    {
      static const GTypeInfo our_info = {
	sizeof (EggToolbarsModelClass),
	NULL,			/* base_init */
	NULL,			/* base_finalize */
	(GClassInitFunc) egg_toolbars_model_class_init,
	NULL,
	NULL,			/* class_data */
	sizeof (EggToolbarsModel),
	0,			/* n_preallocs */
	(GInstanceInitFunc) egg_toolbars_model_init
      };

      egg_toolbars_model_type = g_type_register_static (G_TYPE_OBJECT,
							"EggToolbarsModel",
							&our_info, 0);
    }

  return egg_toolbars_model_type;

}

static xmlDocPtr
egg_toolbars_model_to_xml (EggToolbarsModel *t)
{
  GNode *l1, *l2, *tl;
  xmlDocPtr doc;

  g_return_val_if_fail (IS_EGG_TOOLBARS_MODEL (t), NULL);

  tl = t->priv->toolbars;

  xmlIndentTreeOutput = TRUE;
  doc = xmlNewDoc ("1.0");
  doc->children = xmlNewDocNode (doc, NULL, "toolbars", NULL);

  for (l1 = tl->children; l1 != NULL; l1 = l1->next)
    {
      xmlNodePtr tnode;
      EggToolbarsToolbar *toolbar = l1->data;

      tnode = xmlNewChild (doc->children, NULL, "toolbar", NULL);
      xmlSetProp (tnode, "name", toolbar->name);

      for (l2 = l1->children; l2 != NULL; l2 = l2->next)
	{
	  xmlNodePtr node;
	  EggToolbarsItem *item = l2->data;

	  if (item->separator)
	    {
	      node = xmlNewChild (tnode, NULL, "separator", NULL);
	    }
	  else
	    {
	      char *name;

	      node = xmlNewChild (tnode, NULL, "toolitem", NULL);
	      name = egg_toolbars_model_get_item_name (t, item->type, item->id);
	      xmlSetProp (node, "type", item->type);
	      xmlSetProp (node, "name", name);
	      g_free (name);
	    }
	}
    }

  return doc;
}

gboolean
safe_save_xml (const char *xml_file, xmlDocPtr doc)
{
	char *tmp_file;
	char *old_file;
	gboolean old_exist;
	gboolean retval = TRUE;

	tmp_file = g_strconcat (xml_file, ".tmp", NULL);
	old_file = g_strconcat (xml_file, ".old", NULL);

	if (!xmlSaveFormatFile (tmp_file, doc, 1))
	{
		g_warning ("Failed to write XML data to %s", tmp_file);
		goto failed;
	}

	old_exist = g_file_test (xml_file, G_FILE_TEST_EXISTS);

	if (old_exist)
	{
		if (rename (xml_file, old_file) < 0)
		{
			g_warning ("Failed to rename %s to %s", xml_file, old_file);
			retval = FALSE;
			goto failed;
		}
	}

	if (rename (tmp_file, xml_file) < 0)
	{
		g_warning ("Failed to rename %s to %s", tmp_file, xml_file);

		if (rename (old_file, xml_file) < 0)
		{
			g_warning ("Failed to restore %s from %s", xml_file, tmp_file);
		}
		retval = FALSE;
		goto failed;
	}

	if (old_exist)
	{
		if (unlink (old_file) < 0)
		{
			g_warning ("Failed to delete old file %s", old_file);
		}
	}

	failed:
	g_free (old_file);
	g_free (tmp_file);

	return retval;
}

void
egg_toolbars_model_save (EggToolbarsModel *t,
			 const char *xml_file,
			 const char *version)
{
  xmlDocPtr doc;
  xmlNodePtr root;

  g_return_if_fail (IS_EGG_TOOLBARS_MODEL (t));

  doc = egg_toolbars_model_to_xml (t);
  root = xmlDocGetRootElement (doc);
  xmlSetProp (root, "version", version);
  safe_save_xml (xml_file, doc);
  xmlFreeDoc (doc);
}

static EggToolbarsToolbar *
toolbars_toolbar_new (const char *name)
{
  EggToolbarsToolbar *toolbar;

  toolbar = g_new0 (EggToolbarsToolbar, 1);
  toolbar->name = g_strdup (name);
  toolbar->flags = 0;

  return toolbar;
}

static EggToolbarsItem *
toolbars_item_new (const char *id,
		   const char *type,
		   gboolean    separator)
{
  EggToolbarsItem *item;

  g_return_val_if_fail (id != NULL, NULL);
  g_return_val_if_fail (type != NULL, NULL);

  item = g_new0 (EggToolbarsItem, 1);
  item->id = g_strdup (id);
  item->type = g_strdup (type);
  item->separator = separator;

  return item;
}

static void
free_toolbar_node (EggToolbarsToolbar *toolbar)
{
  g_return_if_fail (toolbar != NULL);

  g_free (toolbar->name);
  g_free (toolbar);
}

static void
free_item_node (EggToolbarsItem *item)
{
  g_return_if_fail (item != NULL);

  g_free (item->id);
  g_free (item->type);
  g_free (item);
}

EggTbModelFlags
egg_toolbars_model_get_flags (EggToolbarsModel *t,
			      int               toolbar_position)
{
  GNode *toolbar_node;
  EggToolbarsToolbar *toolbar;

  toolbar_node = g_node_nth_child (t->priv->toolbars, toolbar_position);
  g_return_val_if_fail (toolbar_node != NULL, -1);

  toolbar = toolbar_node->data;

  return toolbar->flags;
}

void
egg_toolbars_model_set_flags (EggToolbarsModel *t,
			      EggTbModelFlags   flags,
			      int               toolbar_position)
{
  GNode *toolbar_node;
  EggToolbarsToolbar *toolbar;

  toolbar_node = g_node_nth_child (t->priv->toolbars, toolbar_position);
  g_return_if_fail (toolbar_node != NULL);

  toolbar = toolbar_node->data;

  toolbar->flags = flags;

  g_signal_emit (G_OBJECT (t), egg_toolbars_model_signals[TOOLBAR_CHANGED],
		 0, toolbar_position);
}

void
egg_toolbars_model_add_separator (EggToolbarsModel *t,
			          int		    toolbar_position,
			          int		    position)
{
  GNode *parent_node;
  GNode *node;
  EggToolbarsItem *item;
  int real_position;

  g_return_if_fail (IS_EGG_TOOLBARS_MODEL (t));

  parent_node = g_node_nth_child (t->priv->toolbars, toolbar_position);
  item = toolbars_item_new ("separator", "separator", TRUE);
  node = g_node_new (item);
  g_node_insert (parent_node, position, node);

  real_position = g_node_child_position (parent_node, node);

  g_signal_emit (G_OBJECT (t), egg_toolbars_model_signals[ITEM_ADDED], 0,
		 toolbar_position, real_position);
}

gboolean
impl_add_item (EggToolbarsModel    *t,
	       int		    toolbar_position,
	       int		    position,
	       const char          *id,
	       const char          *type)
{
  GNode *parent_node;
  GNode *node;
  EggToolbarsItem *item;
  int real_position;

  g_return_if_fail (IS_EGG_TOOLBARS_MODEL (t));
  g_return_if_fail (id != NULL);
  g_return_if_fail (type != NULL);

  parent_node = g_node_nth_child (t->priv->toolbars, toolbar_position);
  item = toolbars_item_new (id, type, FALSE);
  node = g_node_new (item);
  g_node_insert (parent_node, position, node);

  real_position = g_node_child_position (parent_node, node);

  g_signal_emit (G_OBJECT (t), egg_toolbars_model_signals[ITEM_ADDED], 0,
		 toolbar_position, real_position);

  return TRUE;
}

static void
parse_item_list (EggToolbarsModel *t,
		 xmlNodePtr        child,
		 int               position)
{
  while (child)
    {
      if (xmlStrEqual (child->name, "toolitem"))
	{
	  xmlChar *name, *type;
	  char *id;

	  name = xmlGetProp (child, "name");
	  type = xmlGetProp (child, "type");
          if (type == NULL)
            {
              type = g_strdup (EGG_TOOLBAR_ITEM_TYPE);
            }

          id = egg_toolbars_model_get_item_id (t, type, name);
	  if (id != NULL)
	    {
	      egg_toolbars_model_add_item (t, position, -1, id, type);
            }
	  xmlFree (name);
          g_free (type);
          g_free (id);
	}
      else if (xmlStrEqual (child->name, "separator"))
	{
	  egg_toolbars_model_add_separator (t, position, -1);
	}

      child = child->next;
    }
}

int
egg_toolbars_model_add_toolbar (EggToolbarsModel *t,
				int               position,
				const char       *name)
{
  GNode *node;
  int real_position;

  g_return_val_if_fail (IS_EGG_TOOLBARS_MODEL (t), -1);

  node = g_node_new (toolbars_toolbar_new (name));
  g_node_insert (t->priv->toolbars, position, node);

  real_position = g_node_child_position (t->priv->toolbars, node);

  g_signal_emit (G_OBJECT (t), egg_toolbars_model_signals[TOOLBAR_ADDED],
		 0, real_position);

  return g_node_child_position (t->priv->toolbars, node);
}

static void
parse_toolbars (EggToolbarsModel *t,
		xmlNodePtr        child)
{
  while (child)
    {
      if (xmlStrEqual (child->name, "toolbar"))
	{
	  xmlChar *name;
	  int position;

	  name = xmlGetProp (child, "name");
	  position = egg_toolbars_model_add_toolbar (t, -1, name);
	  xmlFree (name);

	  parse_item_list (t, child->children, position);
	}

      child = child->next;
    }
}

void
egg_toolbars_model_load (EggToolbarsModel *t,
			 const char *xml_file)
{
  xmlDocPtr doc;
  xmlNodePtr root;

  g_return_if_fail (IS_EGG_TOOLBARS_MODEL (t));

  doc = xmlParseFile (xml_file);
  root = xmlDocGetRootElement (doc);

  t->priv->toolbars = g_node_new (NULL);
  parse_toolbars (t, root->children);

  xmlFreeDoc (doc);
}

char *
impl_get_item_id (EggToolbarsModel *t,
		  const char       *type,
		  const char       *name)
{
  if (strcmp (type, EGG_TOOLBAR_ITEM_TYPE) == 0)
    {
      return g_strdup (name);
    }

  return NULL;
}

char *
impl_get_item_name (EggToolbarsModel *t,
		    const char       *type,
		    const char       *id)
{
  if (strcmp (type, EGG_TOOLBAR_ITEM_TYPE) == 0)
    {
      return g_strdup (id);
    }

  return NULL;
}

char *
impl_get_item_type (EggToolbarsModel *t,
		    GdkAtom type)
{
  if (gdk_atom_intern (EGG_TOOLBAR_ITEM_TYPE, FALSE) == type)
    {
      return g_strdup (EGG_TOOLBAR_ITEM_TYPE);
    }

  return NULL;
}

static void
egg_toolbars_model_class_init (EggToolbarsModelClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = egg_toolbars_model_finalize;

  klass->add_item = impl_add_item;
  klass->get_item_id = impl_get_item_id;
  klass->get_item_name = impl_get_item_name;
  klass->get_item_type = impl_get_item_type;

  egg_toolbars_model_signals[ITEM_ADDED] =
    g_signal_new ("item_added",
		  G_OBJECT_CLASS_TYPE (object_class),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggToolbarsModelClass, item_added),
		  NULL, NULL, _egg_marshal_VOID__INT_INT,
		  G_TYPE_NONE, 2, G_TYPE_INT, G_TYPE_INT);
  egg_toolbars_model_signals[TOOLBAR_ADDED] =
    g_signal_new ("toolbar_added",
		  G_OBJECT_CLASS_TYPE (object_class),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggToolbarsModelClass, toolbar_added),
		  NULL, NULL, g_cclosure_marshal_VOID__INT,
		  G_TYPE_NONE, 1, G_TYPE_INT);
  egg_toolbars_model_signals[ITEM_REMOVED] =
    g_signal_new ("item_removed",
		  G_OBJECT_CLASS_TYPE (object_class),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggToolbarsModelClass, item_removed),
		  NULL, NULL, _egg_marshal_VOID__INT_INT,
		  G_TYPE_NONE, 2, G_TYPE_INT, G_TYPE_INT);
  egg_toolbars_model_signals[TOOLBAR_REMOVED] =
    g_signal_new ("toolbar_removed",
		  G_OBJECT_CLASS_TYPE (object_class),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggToolbarsModelClass, toolbar_removed),
		  NULL, NULL, g_cclosure_marshal_VOID__INT,
		  G_TYPE_NONE, 1, G_TYPE_INT);
  egg_toolbars_model_signals[TOOLBAR_CHANGED] =
    g_signal_new ("toolbar_changed",
		  G_OBJECT_CLASS_TYPE (object_class),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggToolbarsModelClass, toolbar_changed),
		  NULL, NULL, g_cclosure_marshal_VOID__INT,
		  G_TYPE_NONE, 1, G_TYPE_INT);
}

static void
egg_toolbars_model_init (EggToolbarsModel *t)
{
  t->priv = g_new0 (EggToolbarsModelPrivate, 1);

  t->priv->toolbars = NULL;
}

static void
egg_toolbars_model_finalize (GObject *object)
{
  EggToolbarsModel *t = EGG_TOOLBARS_MODEL (object);

  g_return_if_fail (object != NULL);
  g_return_if_fail (IS_EGG_TOOLBARS_MODEL (object));

  /* FIXME free nodes */
  g_node_destroy (t->priv->toolbars);

  g_free (t->priv);

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

EggToolbarsModel *
egg_toolbars_model_new (void)
{
  EggToolbarsModel *t;

  t = EGG_TOOLBARS_MODEL (g_object_new (EGG_TOOLBARS_MODEL_TYPE, NULL));

  g_return_val_if_fail (t->priv != NULL, NULL);

  return t;
}

void
egg_toolbars_model_remove_toolbar (EggToolbarsModel   *t,
				   int                 position)
{
  GNode *node;
  EggTbModelFlags flags;

  g_return_if_fail (IS_EGG_TOOLBARS_MODEL (t));

  flags = egg_toolbars_model_get_flags (t, position);

  if (!(flags && EGG_TB_MODEL_NOT_REMOVABLE))
    {
      node = g_node_nth_child (t->priv->toolbars, position);
      g_return_if_fail (node != NULL);

      free_toolbar_node (node->data);
      g_node_destroy (node);

      g_signal_emit (G_OBJECT (t), egg_toolbars_model_signals[TOOLBAR_REMOVED],
		     0, position);
    }
}

void
egg_toolbars_model_remove_item (EggToolbarsModel *t,
				int               toolbar_position,
				int               position)
{
  GNode *node, *toolbar;

  g_return_if_fail (IS_EGG_TOOLBARS_MODEL (t));

  toolbar = g_node_nth_child (t->priv->toolbars, toolbar_position);
  g_return_if_fail (toolbar != NULL);

  node = g_node_nth_child (toolbar, position);
  g_return_if_fail (node != NULL);

  free_item_node (node->data);
  g_node_destroy (node);

  g_signal_emit (G_OBJECT (t), egg_toolbars_model_signals[ITEM_REMOVED], 0,
		 toolbar_position, position);
}

int
egg_toolbars_model_n_items (EggToolbarsModel *t,
			    int               toolbar_position)
{
  GNode *toolbar;

  toolbar = g_node_nth_child (t->priv->toolbars, toolbar_position);
  g_return_val_if_fail (toolbar != NULL, -1);

  return g_node_n_children (toolbar);
}

const char *
egg_toolbars_model_item_nth (EggToolbarsModel *t,
			     int	       toolbar_position,
			     int               position,
			     gboolean         *is_separator)
{
  GNode *toolbar;
  GNode *item;
  EggToolbarsItem *idata;

  toolbar = g_node_nth_child (t->priv->toolbars, toolbar_position);
  g_return_val_if_fail (toolbar != NULL, NULL);

  item = g_node_nth_child (toolbar, position);
  g_return_val_if_fail (item != NULL, NULL);

  idata = item->data;

  *is_separator = idata->separator;

  return idata->id;
}

int
egg_toolbars_model_n_toolbars (EggToolbarsModel *t)
{
  return g_node_n_children (t->priv->toolbars);
}

const char *
egg_toolbars_model_toolbar_nth (EggToolbarsModel *t,
				int               position)
{
  GNode *toolbar;
  EggToolbarsToolbar *tdata;

  toolbar = g_node_nth_child (t->priv->toolbars, position);
  g_return_val_if_fail (toolbar != NULL, NULL);

  tdata = toolbar->data;

  return tdata->name;
}

gboolean
egg_toolbars_model_add_item (EggToolbarsModel *t,
			     int	       toolbar_position,
			     int               position,
			     const char       *id,
			     const char       *type)
{
  EggToolbarsModelClass *klass = EGG_TOOLBARS_MODEL_GET_CLASS (t);
  return klass->add_item (t, toolbar_position, position, id, type);
}

char *
egg_toolbars_model_get_item_id (EggToolbarsModel *t,
			        const char       *type,
			        const char       *name)
{
  EggToolbarsModelClass *klass = EGG_TOOLBARS_MODEL_GET_CLASS (t);
  return klass->get_item_id (t, type, name);
}

char *
egg_toolbars_model_get_item_name (EggToolbarsModel *t,
				  const char       *type,
			          const char       *id)
{
  EggToolbarsModelClass *klass = EGG_TOOLBARS_MODEL_GET_CLASS (t);
  return klass->get_item_name (t, type, id);
}

char *
egg_toolbars_model_get_item_type (EggToolbarsModel *t,
				  GdkAtom type)
{
  EggToolbarsModelClass *klass = EGG_TOOLBARS_MODEL_GET_CLASS (t);
  return klass->get_item_type (t, type);
}
