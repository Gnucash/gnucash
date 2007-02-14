/********************************************************************\
 * qofinstance.c -- handler for fields common to all objects        *
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

/*
 * Object instance holds many common fields that most
 * gnucash objects use.
 *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 */

#include "config.h"
#include <glib.h>
#include "qof.h"
#include "kvp-util-p.h"
#include "qofbook-p.h"
#include "qofid-p.h"
#include "qofinstance-p.h"

static QofLogModule log_module = QOF_MOD_ENGINE;

/* GObject declarations */

static void qof_instance_class_init(QofInstanceClass *klass);
static void qof_instance_init(QofInstance *sp);
static void qof_instance_finalize(GObject *object);

struct _QofInstancePrivate
{

   /* The entity_table in which this instance is stored */
   QofBook * book;

  /* kvp_data is a key-value pair database for storing arbirtary
   * information associated with this instance.  
   * See src/engine/kvp_doc.txt for a list and description of the 
   * important keys. */
   KvpFrame *kvp_data;

   /*  Timestamp used to track the last modification to this 
    *  instance.  Typically used to compare two versions of the
    *  same object, to see which is newer.  When used with the 
    *  SQL backend, this field is reserved for SQL use, to compare
    *  the version in local memory to the remote, server version.
    */
   Timespec last_update;

   /*  Keep track of nesting level of begin/end edit calls */
   gint    editlevel;

   /*  In process of being destroyed */
   gboolean  do_free;

   /*  dirty/clean flag. If dirty, then this instance has been modified,
    *  but has not yet been written out to storage (file/database)
    */
   gboolean  dirty;

   /* True iff this instance has never been committed. */
   gboolean infant;
   
   /* Unique identifier for this object */
   GUID *guid;
};

typedef struct _QofInstanceSignal QofInstanceSignal;
typedef enum _QofInstanceSignalType QofInstanceSignalType;

enum _QofInstanceSignalType {
	/* Signals */
	COMMITED,
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0,
        PROP_BOOK
};

struct _QofInstanceSignal {
	QofInstance *object;
};

static guint qof_instance_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
qof_instance_get_type()
{
	static GType type = 0;

	if(type == 0) {
		static const GTypeInfo our_info = {
			sizeof (QofInstanceClass),
			NULL,
			NULL,
			(GClassInitFunc)qof_instance_class_init,
			NULL,
			NULL,
			sizeof (QofInstance),
			0,
			(GInstanceInitFunc)qof_instance_init,
		};

		type = g_type_register_static(QOF_TYPE_ENTITY, 
			"QofInstance", &our_info, 0);
	}

	return type;
}

static void
qof_instance_class_init(QofInstanceClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent(klass);
	object_class->finalize = qof_instance_finalize;
	object_class->set_property = qof_instance_set_property;
    object_class->get_property = qof_instance_get_property;

	/* Install properties */
	g_object_class_install_property (object_class, PROP_BOOK,
					 g_param_spec_object ("book", _("Book"), NULL,
                                                              	QOF_TYPE_BOOK,
							       								(G_PARAM_READABLE | G_PARAM_WRITABLE |
																			G_PARAM_CONSTRUCT_ONLY)));

	/* Create signals here:*/
	
	qof_instance_signals[COMMITED] =
			g_signal_new ("commited",
				      GNC_TYPE_INSTANCE,
				      G_SIGNAL_RUN_LAST,
				      NULL, NULL, NULL,
				      g_cclosure_marshal_VOID__VOID,
				      G_TYPE_NONE, 0, NULL);
 	
}

static void
qof_instance_init(QofInstance *obj)
{
	/* Initialize private members, etc. */
	inst->priv = g_new0 (QofInstancePrivate, 1);
	
	inst->priv->book = NULL;
	inst->priv->kvp_data = kvp_frame_new();
	inst->priv->last_update.tv_sec = 0;
	inst->priv->last_update.tv_nsec = -1;
	inst->priv->editlevel = 0;
	inst->priv->do_free = FALSE;
	inst->priv->dirty = FALSE;
	inst->priv->infant = TRUE;
	inst->priv->guid = NULL;
}

static void
qof_instance_finalize(GObject *object)
{
	
	/* Free private members, etc. */
	kvp_frame_delete (inst->priv->kvp_data);
	inst->priv->kvp_data = NULL;
	inst->priv->editlevel = 0;
	inst->priv->do_free = FALSE;
	inst->priv->dirty = FALSE;
	qof_book_remove_element (QOF_INSTANCE (object));
	
	
	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
qof_instance_set_property (GObject *object,
				  guint param_id,
				  const GValue *value,
				  GParamSpec *pspec)
{
	QofInstance *obj;
	
	obj = QOF_INSTANCE (object);
	
	switch (param_id) {		
		case PROP_BOOK:
			obj->priv->book = qof_instance_set_book (obj, QOF_BOOK (g_value_get_object (value)));
			break;
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    	break;
	}
}

static void
qof_instance_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  QofInstance *obj;
  
  obj = QOF_INSTANCE(object);

  switch (property_id) {
  case PROP_BOOK:
  	g_value_set_object (value, obj->priv->book);
  	break;
  default:
    /* We don't have any other property... */
    G_OBJECT_WARNinst->priv = g_new0 (QofInstancePrivate, 1);
    break;
  }
}




/* ========================================================== */

void
qof_instance_set_book (QofInstance *instance, QofBook *book)
{
	QofCollection *coll;
		
	g_return_if_fail (QOF_IS_BOOK (book));
	
	
	if ( QOF_IS_BOOK (inst->book) && instance->priv->guid != NULL) 
	{		
		if (QOF_IS_INSTANCE (qof_book_get_element (inst->book, G_OBJECT_TYPE (instance), ent->priv->guid)))
		{
			qof_collection_remove_element (qof_book_get_collection (ent->priv->book, 
																										G_OBJECT_TYPE (instance)), 
																										instance);
		}
		inst->priv->book = book;
	}
	else
	{
		inst->priv->book = book;
  	}
  	
  	if (inst->priv->guid == NULL )
  	{
  		GUID *guid;
  		
  		guid = NULL;
  		
  		do
  		{
   			 guid_new (guid);

   			 if (NULL == qof_book_get_element (instance->priv->book, G_OBJECT_TYPE (instance), guid)) break;

    		PWARN("duplicate id created, trying again");
  		} while(1);
  		
  		inst->priv->guid = guid;
  	}
  	
  	qof_book_insert_element (instance->priv->book, instance);
}

QofCollection* 
qof_instance_get_collection (QofInstance *instance)
{
	return qof_book_get_collection (instance->priv->book, G_OBJECT_TYPE (instance));
}

/* This is a restricted function, should be used only during 
 * read from file */
void
qof_instance_set_guid (QofInstance *inst, const GUID *guid)
{
  if (guid_equal (guid, inst->priv->guid)) return;

  qof_book_remove_element (inst->priv->book, inst);
  inst->priv->guid = guid;
  qof_book_insert_element (inst->priv->book, inst);
}


QofInstance*
qof_instance_create (GType type, QofBook *book)
{
	QofInstance *inst;

	inst = QOF_INSTANCE (g_object_new (type, "book", book, NULL));
	
	return inst;
}


void
qof_instance_destroy (QofInstance *inst)
{
	qof_instance_begin_edit (inst);
	qof_instance_commit_edit (inst);
	g_object_unref (G_OBJECT (inst));
}

const GUID *
qof_instance_get_guid (const QofInstance *inst)
{
	g_return_value_if_fail (QOF_IS_INSTANCE(inst), NULL);

	return inst->priv->guid;
}

QofBook *
qof_instance_get_book (const QofInstance *inst)
{
	if (!inst) return NULL;
	return inst->priv->book;
}

KvpFrame*
qof_instance_get_slots (const QofInstance *inst)
{
  if (!inst) return NULL;
  return inst->priv->kvp_data;
}

Timespec
qof_instance_get_last_update (const QofInstance *inst)
{
	if (!inst)
	{
		Timespec ts = {0,-1};
		return ts;
	}
	return inst->priv->last_update;
}

int
qof_instance_version_cmp (const QofInstance *left, const QofInstance *right)
{
	if (!left && !right) return 0;
	if (!left) return -1;
	if (!right) return +1;
	if (left->last_update.tv_sec < right->last_update.tv_sec) return -1;
	if (left->last_update.tv_sec > right->last_update.tv_sec) return +1;
	if (left->last_update.tv_nsec < right->last_update.tv_nsec) return -1;
	if (left->last_update.tv_nsec > right->last_update.tv_nsec) return +1;
	return 0;
}

void
qof_instance_print_dirty (const QofEntity *entity, gpointer dummy)
{
  QofInstance *inst = QOF_INSTANCE(entity);

  if (inst->priv->dirty)
    printf("%s instance %s is dirty.\n", qof_entity_get_qof_type (QOF_ENTITY (inst),
	   guid_to_string(qof_entity_get_guid (QOF_ENTITY (inst)))));
}

gboolean
qof_instance_is_dirty (QofInstance *inst)
{
	QofCollection *coll;

	if (!inst) { return FALSE; }
	if (qof_get_alt_dirty_mode())
	  return inst->priv->dirty;
	coll = qof_entity_get_collection (QOF_ENTITY (inst));
	if(qof_collection_is_dirty(coll)) { return inst->priv->dirty; }
	inst->priv->dirty = FALSE;
	return FALSE;
}

void
qof_instance_set_dirty(QofInstance* inst, gboolean value)
{
	QofCollection *coll;

	if (value) {
		inst->priv->dirty = TRUE;
		if (!qof_get_alt_dirty_mode()) {
		  coll = qof_entity_get_collection (QOF_ENTITY (inst));
		  qof_collection_mark_dirty(coll);
		}
	}
	inst->priv->dirty = FALSE;
}

gboolean
qof_instance_check_edit(const  QofInstance *inst)
{
	if(inst->priv->editlevel > 0) { return TRUE; }
	return FALSE;
}

gboolean
qof_instance_do_free(const QofInstance *inst)
{
	return inst->priv->do_free;
}

void
qof_instance_mark_free(QofInstance *inst)
{
	inst->priv->do_free = TRUE;
}

/* ========================================================== */
/* setters */

void
qof_instance_mark_clean (QofInstance *inst)
{
  if(!inst) return;
  inst->priv->dirty = FALSE;
}

void
qof_instance_set_slots (QofInstance *inst, KvpFrame *frm)
{
  if (!inst) return;
  if (inst->priv->kvp_data && (inst->priv->kvp_data != frm))
  {
    kvp_frame_delete(inst->priv->kvp_data);
  }

  inst->priv->dirty = TRUE;
  inst->priv->kvp_data = frm;
}

void
qof_instance_set_last_update (QofInstance *inst, Timespec ts)
{
  if (!inst) return;
  inst->priv->last_update = ts;
}

/* ========================================================== */

void
qof_instance_gemini (QofInstance *to, const QofInstance *from)
{
  time_t now;

  /* Books must differ for a gemini to be meaningful */
  if (!from || !to || (from->book == to->book)) return;

  now = time(0);

  /* Make a note of where the copy came from */
  gnc_kvp_bag_add (to->kvp_data, "gemini", now,
                                  "inst_guid", &from->entity.guid,
                                  "book_guid", &from->book->inst.entity.guid,
                                  NULL);
  gnc_kvp_bag_add (from->kvp_data, "gemini", now,
                                  "inst_guid", &to->entity.guid,
                                  "book_guid", &to->book->inst.entity.guid,
                                  NULL);

  to->dirty = TRUE;
}

QofInstance *
qof_instance_lookup_twin (const QofInstance *src, QofBook *target_book)
{
	QofCollection *col;
	KvpFrame *fr;
	GUID * twin_guid;
	QofInstance * twin;

	if (!src || !target_book) return NULL;
	ENTER (" ");

	fr = gnc_kvp_bag_find_by_guid (src->kvp_data, "gemini",
	                             "book_guid", &target_book->inst.entity.guid);

	twin_guid = kvp_frame_get_guid (fr, "inst_guid");

	col = qof_book_get_collection (target_book, src->entity.e_type);
	twin = (QofInstance *) qof_collection_lookup_entity (col, twin_guid);

	LEAVE (" found twin=%p", twin);
	return twin;
}

/* Gets the KvpFrame kvp data value stored in the instance */

KvpFrame* 
qof_instance_get_kvp_data (const QofInstance *instance)
{
	return instance->kvp_data;
}

void			
qof_instance_set_kvp_data (QofInstance *instance, KvpFrame *data)
{
	kvp_frame_delete (instance->kvp_data);
	instance->kvp_data = kvp_frame_copy (data);
}

/* Gets the Instance's Edit Level*/
gint qof_instance_get_edit_level (const QofInstance *instance)
{
	return instance->editlevel;
}

void 		qof_instance_set_edit_level (QofInstance *instance, gint editlevel)
{
	instance->editlevel = editlevel;
}

const char* qof_instance_printable (QofInstance *inst)
{
	g_return_if_fail (QOF_IS_INSTANCE (inst));
	
	if (QOF_INSTANCE_GET_CLASS (inst)->printable)
		(QOF_INSTANCE_GET_CLASS (inst)->printable) (inst);
	else
		g_warning ("%s() method not supported\n", __FUNCTION__);
}

gboolean
qof_instance_begin_edit (QofInstance *inst, GError **error)
{
  QofBackend * be;

  g_return_val_if_fail (QOF_IS_INSTANCE (inst),FALSE);
  
  inst->priv->editlevel++;
  
  g_return_val_if_fail (1 < inst->priv->editlevel), FALSE);
  
  if (0 >= inst->editlevel) 
      inst->editlevel = 1;

  be = qof_book_get_backend (inst->priv->book);
  
  if (QOF_IS_BACKEND (be) && qof_backend_begin_exists(be))
      qof_backend_run_begin (be, inst, error);
  else
      inst->priv->dirty = TRUE; 
  
  return TRUE;
}

gboolean 
qof_instance_commit_edit (QofInstance *inst, GError **error)
{
  QofBackend * be;
  gboolean dirty;

  g_return_val_if_fail(QOF_IS_INSTANCE(inst), FALSE);
  
  inst->priv->editlevel--;
  
  dirty = inst->dirty;
  
  if (0 < inst->priv->editlevel) return FALSE;

  if ((0 == inst->priv->editlevel) && inst->priv->dirty)
  {
    be = qof_book_get_backend (inst->priv->book);
    
    if (be && qof_backend_commit_exists(be)) {

        qof_backend_run_commit(be, inst, &error);
        
        if (error) {
            /* XXX Should perform a rollback here */
            inst->do_free = FALSE;

            return FALSE;
        }   
        /* XXX the backend commit code should clear dirty!! */
        inst->dirty = FALSE;
    
    }
  }
  
    if (dirty && qof_get_alt_dirty_mode() && 
        !(inst->priv->infant && inst->priv->do_free)) {
        /* TODO: mark the book dirty using the QofBook API instead the Collection API
        	may be qof_book_mark_dirty (QofBook, GType) the you will mark the book and the
        	collection with that GType
        */
      qof_collection_mark_dirty(qof_book_get_collection(inst->priv->book, 	G_TYPE_OBJECT(inst)));
      qof_book_mark_dirty(inst->priv->book);
    }
    
    inst->infant = FALSE;
    
    if (0 > inst->priv->editlevel) { 
      
      PERR ("unbalanced call - resetting (was %d)", inst->priv->editlevel);
      
      inst->priv->editlevel = 0;
      if (error)
      {
      		g_set_error (error, QOF_BACKEND_ERROR, QOF_BACKEND_UNBALANCED_CALL_ERROR, 
				   				 "unbalanced call - EditLevel set to 0 (last value %d)", inst->priv->editlevel);
	  }
	  
	  return FALSE;
  	}  
    
    return TRUE;

}


/* ========================== END OF FILE ======================= */
