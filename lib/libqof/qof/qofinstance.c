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
#include "qofbook.h"
#include "qofbook-p.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofinstance-p.h"
#include <glib/gi18n-lib.h>
#include "qofbackend.h"

static QofLogModule log_module = QOF_MOD_ENGINE;

/* GObject declarations */

static void qof_instance_class_init(QofInstanceClass *klass);
static void qof_instance_init(QofInstance *sp);
static void qof_instance_finalize(GObject *object);
static void qof_instance_set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);
static void qof_instance_get_property (GObject *object, guint property_id, GValue *value, GParamSpec *pspec);


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
	BEGIN_EDIT,
	BEGIN_COMMIT,
	COMMITED,
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0,
        PROP_BOOK,
        PROP_GUID
};

struct _QofInstanceSignal {
	QofInstance *object;
};

static guint qof_instance_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
qof_instance_get_type(void)
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

		type = g_type_register_static(G_TYPE_OBJECT, 
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
					 g_param_spec_object ("book", NULL, _("Book"), QOF_TYPE_BOOK,
							       								(G_PARAM_READABLE | G_PARAM_WRITABLE |
																			G_PARAM_CONSTRUCT_ONLY)));
  
  g_object_class_install_property (object_class, PROP_GUID,
					 g_param_spec_object ("guid", NULL, _("GUID"), QOF_TYPE_GUID,
							       								(G_PARAM_READABLE | G_PARAM_WRITABLE)));
	/* Create signals here:*/
	
	qof_instance_signals[COMMITED] =
			g_signal_new ("commit:finished",
				      QOF_TYPE_INSTANCE,
				      G_SIGNAL_RUN_FIRST,
				      0, NULL, NULL,
				      g_cclosure_marshal_VOID__VOID,
				      G_TYPE_NONE, 0, NULL);
 	
 	qof_instance_signals[BEGIN_COMMIT] =
			g_signal_new ("commit:beginning",
				      QOF_TYPE_INSTANCE,
				      G_SIGNAL_RUN_FIRST,
				      0, NULL, NULL,
				      g_cclosure_marshal_VOID__VOID,
				      G_TYPE_NONE, 0, NULL);
 	
 	qof_instance_signals[BEGIN_EDIT] =
			g_signal_new ("begin_edit",
				      QOF_TYPE_INSTANCE,
				      G_SIGNAL_RUN_FIRST,
				      0, NULL, NULL,
				      g_cclosure_marshal_VOID__VOID,
				      G_TYPE_NONE, 0, NULL);
	
				      
}

static void
qof_instance_init(QofInstance *obj)
{
	
	/* Initialize private members, etc. */
	obj->priv = g_new0 (QofInstancePrivate, 1);
	
	obj->priv->book = NULL;
	obj->priv->kvp_data = kvp_frame_new();
	obj->priv->last_update.tv_sec = 0;
	obj->priv->last_update.tv_nsec = -1;
	obj->priv->editlevel = 0;
	obj->priv->do_free = FALSE;
	obj->priv->dirty = FALSE;
	obj->priv->infant = TRUE;
	obj->priv->guid = NULL;
}

static void
qof_instance_finalize(GObject *object)
{
	QofInstance *inst = QOF_INSTANCE (object);
	/* Free private members, etc. */
	kvp_frame_delete (inst->priv->kvp_data);
	inst->priv->kvp_data = NULL;
	inst->priv->editlevel = 0;
	inst->priv->do_free = FALSE;
	inst->priv->dirty = FALSE;
	qof_book_remove_element (inst->priv->book, inst);
	guid_free (inst->priv->guid);
	
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
			qof_instance_set_book (obj, QOF_BOOK (g_value_get_object (value)));
			obj->priv->book = QOF_BOOK (g_value_get_object (value));
			break;
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,param_id,pspec);
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
  case PROP_GUID:
    g_value_set_boxed (value, obj->priv->guid);
    break;
  default:
    /* We don't have any other property... */
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    break;
  }
}




/* ========================================================== */

void
qof_instance_set_book (QofInstance *inst, QofBook *book)
{
	QofCollection *coll;
		
	g_return_if_fail (QOF_IS_BOOK (book));
	
	
	if ( QOF_IS_BOOK (inst->priv->book) && inst->priv->guid != NULL) 
	{		
		if (QOF_IS_INSTANCE (qof_book_get_element (inst->priv->book, G_OBJECT_TYPE (inst), inst->priv->guid)))
		{
			qof_collection_remove_element (qof_book_get_collection (inst->priv->book, 
																										G_OBJECT_TYPE (inst)), 
																										inst);
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

   			 if (NULL == qof_book_get_element (inst->priv->book, G_OBJECT_TYPE (inst), guid)) break;

    		PWARN("duplicate id created, trying again");
  		} while(1);
  		
  		inst->priv->guid = guid;
  	}
  	
  	qof_book_insert_element (inst->priv->book, inst);
}


/* This is a restricted function, should be used only during 
 * read from file */
void
qof_instance_set_guid (QofInstance *inst, GUID *guid)
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
qof_instance_release (QofInstance *inst)
{
	g_object_unref (G_OBJECT (inst));
}


gboolean
qof_instance_destroy (QofInstance *inst, GError **error)
{
	qof_instance_begin_edit (inst, error);
	if(error) {
	  return FALSE;
	}
	qof_instance_commit_edit (inst, error);
	if(error) {
	/*FIXME: Add actions to cancel the commit if exist an error*/
	  return FALSE;
	}
	
	qof_event_gen (inst, QOF_EVENT_DESTROY, NULL);
	
	g_object_unref (G_OBJECT (inst));
	return TRUE;
}

GUID *
qof_instance_get_guid (QofInstance *inst)
{
	g_return_val_if_fail (QOF_IS_INSTANCE(inst), NULL);

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
	if (left->priv->last_update.tv_sec < right->priv->last_update.tv_sec) return -1;
	if (left->priv->last_update.tv_sec > right->priv->last_update.tv_sec) return +1;
	if (left->priv->last_update.tv_nsec < right->priv->last_update.tv_nsec) return -1;
	if (left->priv->last_update.tv_nsec > right->priv->last_update.tv_nsec) return +1;
	return 0;
}

void
qof_instance_print_dirty (QofInstance *inst, gpointer dummy)
{

  if (inst->priv->dirty)
    printf("%s instance %s is dirty.\n", g_type_name (G_OBJECT_TYPE (inst)),
	   guid_to_string(qof_instance_get_guid (inst)));
}

gboolean
qof_instance_is_dirty (QofInstance *inst)
{
	QofCollection *coll;
  g_return_val_if_fail (QOF_IS_INSTANCE (inst), FALSE);

	if (qof_get_alt_dirty_mode())
	{
	  return inst->priv->dirty;
	}
	 
	coll = qof_instance_get_collection (inst);
	
	if(qof_collection_is_dirty(coll)) 
	{
	  return inst->priv->dirty; 
	}
	
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
		  coll = qof_book_get_collection (inst->priv->book, G_OBJECT_TYPE (inst));
		  qof_collection_mark_dirty(coll);
		  qof_book_mark_dirty (inst->priv->book); // This actualy a macro becouse the QofBook is a QofInstane: TODO: Check if QofBook could be a GObject 
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

gboolean 
qof_instance_get_do_free (QofInstance *inst)
{
  return inst->priv->do_free;
}

void
qof_instance_set_do_free (QofInstance *inst, gboolean val)
{
  inst->priv->do_free = val;
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
  if (!from || !to || (from->priv->book == to->priv->book)) return;

  now = time(0);

  /* Make a note of where the copy came from */
  gnc_kvp_bag_add (to->priv->kvp_data, "gemini", now,
                                  "inst_guid", &from->priv->guid,
                                  "book_guid", qof_instance_get_guid (QOF_INSTANCE (from->priv->book)),
                                  NULL);
  gnc_kvp_bag_add (from->priv->kvp_data, "gemini", now,
                                  "inst_guid", &to->priv->guid,
                                  "book_guid", qof_instance_get_guid (QOF_INSTANCE (to->priv->book)),
                                  NULL);

  to->priv->dirty = TRUE;
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

	fr = gnc_kvp_bag_find_by_guid (src->priv->kvp_data, "gemini",
	                             "book_guid", qof_instance_get_guid (QOF_INSTANCE (target_book)));

	twin_guid = kvp_frame_get_guid (fr, "inst_guid");

	col = qof_book_get_collection (target_book, G_OBJECT_TYPE (src));
	twin = (QofInstance *) qof_collection_lookup_element (col, twin_guid);

	LEAVE (" found twin=%p", twin);
	return twin;
}

/* Gets the KvpFrame kvp data value stored in the instance */

KvpFrame* 
qof_instance_get_kvp_data (const QofInstance *instance)
{
	return instance->priv->kvp_data;
}

void			
qof_instance_set_kvp_data (QofInstance *instance, KvpFrame *data)
{
	kvp_frame_delete (instance->priv->kvp_data);
	instance->priv->kvp_data = kvp_frame_copy (data);
}

void
qof_instance_delete_kvp_data (QofInstance *inst)
{
  kvp_frame_delete (inst->priv->kvp_data);
}

/* Gets the Instance's Edit Level*/
gint qof_instance_get_edit_level (const QofInstance *instance)
{
	return instance->priv->editlevel;
}

void 		
qof_instance_set_edit_level (QofInstance *instance, gint editlevel)
{
	instance->priv->editlevel = editlevel;
}


void
qof_instance_foreach (QofInstance *inst, 
                      QofInstanceForeachCB cb_func, 
                      gpointer user_data)
{
  QofCollection *col;
  
  g_return_if_fail (QOF_IS_INSTANCE (inst));
	
	if (QOF_INSTANCE_GET_CLASS (inst)->foreach)
		(QOF_INSTANCE_GET_CLASS (inst)->foreach) (inst, cb_func, user_data);
	else
	{
		g_warning ("%s() calling default handler\n", __FUNCTION__);
		
		col = qof_book_get_collection (inst->priv->book, G_OBJECT_TYPE (inst));
		qof_collection_foreach (col, cb_func, user_data);
	}
}

gchar*
qof_instance_to_string (QofInstance *inst)
{
	gchar* ret = NULL;
	
	g_return_val_if_fail (QOF_IS_INSTANCE (inst), NULL);
	
	if (QOF_INSTANCE_GET_CLASS (inst)->to_string)
	{
	  ret = ((QOF_INSTANCE_GET_CLASS (inst)->to_string) (inst));
		return ret;
	}
	else
	{
		g_warning ("%s() method not supported\n", __FUNCTION__);
		return NULL;
	}
}

gboolean
qof_instance_begin_edit (QofInstance *inst, GError **error)
{
  QofBackend * be;
  
  g_return_val_if_fail (QOF_IS_INSTANCE (inst), FALSE);
  
  g_signal_emit_by_name (inst, "begin-edit");
  
  inst->priv->editlevel++;
  
  g_return_val_if_fail ((1 < inst->priv->editlevel), FALSE);
  
  if (0 >= inst->priv->editlevel) 
      inst->priv->editlevel = 1;

  be = qof_book_get_backend (inst->priv->book);
  
  if (be && qof_backend_begin_exists(be))
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

  g_return_val_if_fail (QOF_IS_INSTANCE(inst), FALSE);
  
  /* 	IMPROVEMENT: call a callback with a return type to see if I can actualy commit or not
  *	for example, in GncTransaction you can check if it is readonly, if you can't commit this function
  *	must return and set the error variable with the error (even better the **error pointer could be
  *	a parameter to the callback and set by it).
  */  
  g_signal_emit_by_name (inst, "commit:beginning");
  
  inst->priv->editlevel--;
  
  dirty = inst->priv->dirty;
  
  if (0 < inst->priv->editlevel) return FALSE;

  if ((0 == inst->priv->editlevel) && inst->priv->dirty)
  {
    be = qof_book_get_backend (inst->priv->book);
    
    if (be && qof_backend_commit_exists(be)) {

        qof_backend_run_commit(be, inst, error);
        
        if (error) {
            /* XXX Should perform a rollback here */
            inst->priv->do_free = FALSE;

            return FALSE;
        }   
        /* XXX the backend commit code should clear dirty!! */
        inst->priv->dirty = FALSE;
    
    }
  }
  
    if (dirty && qof_get_alt_dirty_mode() && 
        !(inst->priv->infant && inst->priv->do_free)) {
        /* TODO: mark the book dirty using the QofBook API instead the Collection API
        	may be qof_book_mark_dirty (QofBook, GType) the you will mark the book and the
        	collection with that GType
        */
      qof_collection_mark_dirty(qof_book_get_collection(inst->priv->book, 	G_OBJECT_TYPE(inst)));
      qof_book_mark_dirty(inst->priv->book);
    }
    
    inst->priv->infant = FALSE;
    
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
  	
  	g_signal_emit_by_name (inst, "commit:finished"); 
    
    return TRUE;

}


/* ========================== END OF FILE ======================= */
