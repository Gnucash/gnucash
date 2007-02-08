/********************************************************************\
 * qofid.c -- QOF entity identifier implementation                  *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <string.h>
#include <glib.h>

#include "qof.h"
#include "qofid-p.h"

static QofLogModule log_module = QOF_MOD_ENGINE;
static gboolean qof_alt_dirty_mode = FALSE;

/* GObject declarations */

static void qof_collection_class_init(QofCollectionClass *klass);
static void qof_collection_init(QofCollection *sp);
static void qof_collection_finalize(GObject *object);

struct _QofCollectionPrivate
{
  GType		 type;
  gboolean     is_dirty;
  
  GHashTable * hash_of_entities;
};

typedef struct _QofCollectionSignal QofCollectionSignal;
typedef enum _QofCollectionSignalType QofCollectionSignalType;

enum _QofCollectionSignalType {
	/* Signals */
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0,
        PROP_TYPE
};

struct _QofCollectionSignal {
	QofCollection *object;
};

static guint qof_collection_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
qof_collection_get_type()
{
	static GType type = 0;

	if(type == 0) {
		static const GTypeInfo our_info = {
			sizeof (QofCollectionClass),
			NULL,
			NULL,
			(GClassInitFunc)qof_collection_class_init,
			NULL,
			NULL,
			sizeof (QofCollection),
			0,
			(GInstanceInitFunc)qof_collection_init,
		};

		type = g_type_register_static(QOF_TYPE_INSTANCE, 
			"QofCollection", &our_info, 0);
	}

	return type;
}

static void
qof_collection_class_init(QofCollectionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent(klass);
	object_class->finalize = qof_collection_finalize;
	object_class->set_property = qof_collection_set_property;
    object_class->get_property = qof_collection_get_property;

	/* Install properties */
	
	g_object_class_install_property (object_class, PROP_TYPE,
					 g_param_spec_object ("type", _("Object's GType the Collection holds"), NULL,
                                                              	G_TYPE_GTYPE,
							       								(G_PARAM_READABLE | G_PARAM_WRITABLE |
																			G_PARAM_CONSTRUCT_ONLY)));
	/* Create signals here:*/
 	
}

static void
qof_collection_init(QofCollection *obj)
{
	/* Initialize private members, etc. */
  col->priv = g_new0 (QofCollectionPrivate, 1);
  
  col->priv->type = G_TYPE_INVALID;
  col->priv->hash_of_entities = g_hash_table_new (id_hash, id_compare);
}

static void
qof_collection_finalize(GObject *object)
{
	
	/* Free private members, etc. */

  g_hash_table_destroy(col->hash_of_entities);
  col->type = G_TYPE_INVALID;
  col->hash_of_entities = NULL;
  
	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
qof_collection_set_property (GObject *object,
				  guint param_id,
				  const GValue *value,
				  GParamSpec *pspec)
{
	QofCollection *obj;
	
	obj = QOF_COLLECTION (object);
	switch (param_id) {	
		case PROP_TYPE:
			if (obj->priv->hash_of_entities == NULL)
			obj->priv->type =  g_value_get_gtype (value));
			break;
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    	break;
	}
}

static void
qof_collection_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  QofCollection *obj;
  
  obj = QOF_COLLECTION(object);

  switch (property_id) {
  case PROP_TYPE:
  		g_value_set_int (value, obj->priv->type);
  		break;
  default:
    /* We don't have any other property... */
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    break;
  }
}

static guint id_hash (gconstpointer key);
static gboolean id_compare(gconstpointer key_1, gconstpointer key_2);

QofCollection *
qof_collection_new (GType type)
{
  QofCollection *col;
  
  g_return_val_if_fail (G_TYPE_IS_OBJECT (type));
  
  col = QOF_COLLECTION (g_object_new (QOF_TYPE_COLLECTION, "type", type));
  
  return col;
}

void
qof_collection_destroy (QofCollection *col)
{
  g_return_if_fail (QOF_IS_COLLECTION (col));
  
  g_object_unref (col);
}

const GType
qof_collection_get_g_type (const QofInstance *entity)
{
	return collection->priv->type;
}


void
qof_collection_remove_element (QofCollection *coll, QofInstance *inst)
{
  QofInstance *obj;
  
  g_return_if_fail (QOF_IS_COLLECTION (coll) && QOF_IS_INSTANCE (inst));
  
  obj = qof_collection_get_element (coll, qof_collection_get_guid (inst));
  
  g_return_if_fail (QOF_IS_INSTANCE (col)); 
  
  g_hash_table_remove (col->priv->hash_of_entities, qof_instance_get_guid (inst));
  
  if (!qof_alt_dirty_mode)
    qof_collection_mark_dirty (col);
}


gboolean
qof_collection_add_element (QofCollection *coll, QofInstance *inst)
{
	
	g_return_val_if_fail ( coll != NULL && 
									!guid_equal(qof_instance_get_guid (inst), guid_null()) &&
									qof_collection_lookup_element (coll, qof_instance_get_guid (inst)) == NULL &&
									QOF_IS_INSTANCE (inst), 
									FALSE);
	
	g_hash_table_insert (coll->hash_of_entities, qof_instance_get_guid (inst), inst);
	
	if (!qof_alt_dirty_mode)
	  qof_collection_mark_dirty(coll);
	  
	return TRUE;
}

static void
collection_merge_cb (QofInstance *ent, gpointer data)
{
	QofCollection *target;

	target = (QofCollection*)data;
	
	qof_collection_add_element (target, ent);	
}

gboolean
qof_collection_merge (QofCollection *target, QofCollection *merge)
{
	g_return_val_if_fail ( QOF_IS_COLLECTION (target) && QOF_IS_COLLECTION (merge), FALSE);
	
	g_return_val_if_fail (target->priv->type == merge->priv->type, FALSE);
	
	qof_collection_foreach(merge, collection_merge_cb, target);
	
	return TRUE;
}

static void
collection_compare_cb (QofInstance *inst, gpointer user_data)
{
	QofCollection *target;
	QofInstance *e;
	gint value;
	
	g_return_if_fail (QOF_IS_INSTANCE (inst) && QOF_IS_COLLECTION (QOF_COLLECTION (user_data)));
	
	e = NULL;
	
	target = QOF_COLLECTION (user_data);
	
	value = *(gint*) g_object_get_data (G_OBJECT (target), "value");
	
	if (value != 0) { return; }
	if (guid_equal(qof_instance_get_guid (inst), guid_null())) 
	{
		value = -1;
		g_object_set_data (G_OBJECT (target), "value", &value);
		return; 
	}
	
	g_return_if_fail (target->type == G_OBJECT_TYPE (inst));
	
	e = qof_collection_lookup_element (target, qof_instance_get_guid (inst));
	
	if ( e == NULL )
	{
		value = 1;
		g_object_set_data (G_OBJECT (target), "value", &value);
		return;
	}
	else
	{
		value = 0;
		g_object_set_data (G_OBJECT (target), "value", &value);
		return;
	}
}

gint
qof_collection_compare (QofCollection *target, QofCollection *merge)
{
	gint value;

	value = 0;
	if (!target && !merge) { return 0; }
	if (target == merge) { return 0; }
	if (!target && merge) { return -1; }
	if (target && !merge) { return 1; }
	if(target->priv->type != merge->priv->type) { return -1; }
	
	g_object_set_data (G_OBJECT (target), "value", &value);

	qof_collection_foreach(merge, collection_compare_cb, target);
	
	value = *(gint*) g_object_get_data(G_OBJECT (target), "value");
	
	if(value == 0) {
		g_object_set_data (G_OBJECT (merge), "value", &value);
		qof_collection_foreach(target, collection_compare_cb, merge);
		value = *(gint*) g_object_get_data(G_OBJECT (merge), "value");
	}
	return value;
}

QofInstance *
qof_collection_lookup_element (const QofCollection *col, const GUID * guid)
{
  QofInstance *inst;
  
  g_return_val_if_fail (QOF_IS_COLLECTION(col) && !guid_equal (guid, guid_null()), NULL);
  
  if (guid == NULL) return NULL;
  
  ent = g_hash_table_lookup (col->priv->hash_of_entities, guid);
  
  return ent;
}

QofCollection *
qof_collection_from_glist (QofBook *book, GType type, GList *glist)
{
	QofCollection *coll;
	QofInstance *ent;
	GList *list;

	coll = qof_collection_new (book, type);
	
	for(list = glist; list != NULL; list = list->next)
	{
		ent = (QofInstance*)list->data;
		if(FALSE == qof_collection_add_element (coll, ent))
		{
			return NULL;
		}
	}
	return coll;
}

guint
qof_collection_count (const QofCollection *col)
{
	guint c;

	c = g_hash_table_size (col->priv->hash_of_entities);
	return c;
}

/* =============================================================== */

static guint
id_hash (gconstpointer key)
{
  const GUID *guid = key;

  if (key == NULL)
    return 0;

  /* Compiler should optimize this all away! */
  if (sizeof(guint) <= 16)
    return *((guint *) guid->data);
  else
  {
    guint hash = 0;
    unsigned int i, j;

    for (i = 0, j = 0; i < sizeof(guint); i++, j++)
    {
      if (j == 16)
        j = 0;

      hash <<= 4;
      hash |= guid->data[j];
    }

    return hash;
  }
}

static gboolean
id_compare(gconstpointer key_1, gconstpointer key_2)
{
  return guid_equal (key_1, key_2);
}


/* =============================================================== */

gboolean 
qof_collection_is_dirty (const QofCollection *col)
{
   return col ? col->priv->is_dirty : FALSE;
}

void 
qof_collection_mark_clean (QofCollection *col)
{
   if (col) { col->priv->is_dirty = FALSE; }
}

void 
qof_collection_mark_dirty (QofCollection *col)
{
   if (col) { 
   		col->priv->is_dirty = TRUE;
   		qof_collection_mark_dirty (col->priv->book);
   	}
}

void
qof_collection_print_dirty (const QofCollection *col, gpointer dummy)
{
  if (col->priv->is_dirty)
    printf("%s collection is dirty.\n", col->priv->e_type);
  qof_collection_foreach(col, (QofInstanceForeachCB)qof_instance_print_dirty, NULL);
}

/* =============================================================== */

struct _iterate {
  QofInstanceForeachCB      fcn;
  gpointer                data;
};

static void foreach_cb (gpointer key, gpointer item, gpointer arg)
{
  struct _iterate *iter = arg;
  QofInstance *ent = item;

  iter->fcn (ent, iter->data);
}

void
qof_collection_foreach (const QofCollection *col, QofInstanceForeachCB cb_func, 
                        gpointer user_data)
{
  struct _iterate iter;

  g_return_if_fail (QOF_IS_COLLECTION (col));
  g_return_if_fail (cb_func);

  iter.fcn = cb_func;
  iter.data = user_data;

  g_hash_table_foreach (col->priv->hash_of_entities, foreach_cb, &iter);
}

/* =============================================================== */

gboolean
qof_get_alt_dirty_mode (void)
{
  return qof_alt_dirty_mode;
}

void
qof_set_alt_dirty_mode (gboolean enabled)
{
  qof_alt_dirty_mode = enabled;
}

/* =============================================================== */
