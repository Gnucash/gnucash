/********************************************************************\
 * qofsesssion.c -- session access (connection to backend)          *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

/*
 * FILE:
 * qofsession.c
 *
 * FUNCTION:
 * Encapsulate a connection to a storage backend.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2004 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
 */

  /* TODO: XXX we should probably move this resolve function to the
   * file backend.  I think the idea would be to open the backend
   * and then ask it if it can contact it's storage media (disk,
   * network, server, etc.) and abort if it can't.  Mal-formed
   * file URL's would be handled the same way!
   */

#include "config.h"

#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>

#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "gnc-trace.h"
#include "qofbackend-p.h"
#include "qofbook.h"
#include "qofbook-p.h"
#include "qofobject.h"
#include "qofsession.h"
#include "qofsession-p.h"

/* Some gnucash-specific code */
#ifdef GNUCASH_MAJOR_VERSION
#include "gnc-module.h"
#endif /* GNUCASH */

static QofSession * current_session = NULL;
static GHookList * session_closed_hooks = NULL;
static short module = MOD_BACKEND;
static GSList *provider_list = NULL;

/* ====================================================================== */

void
qof_backend_register_provider (QofBackendProvider *prov)
{
	provider_list = g_slist_prepend (provider_list, prov);
}

/* ====================================================================== */

/* hook routines */

void
qof_session_add_close_hook (GFunc fn, gpointer data)
{
  GHook *hook;

  if (session_closed_hooks == NULL) {
    session_closed_hooks = malloc(sizeof(GHookList));
    g_hook_list_init (session_closed_hooks, sizeof(GHook));
  }

  hook = g_hook_alloc(session_closed_hooks);
  if (!hook)
    return;

  hook->func = (GHookFunc)fn;
  hook->data = data;
  g_hook_append(session_closed_hooks, hook);
}

void
qof_session_call_close_hooks (QofSession *session)
{
  GHook *hook;
  GFunc fn;

  if (session_closed_hooks == NULL)
    return;

  hook = g_hook_first_valid (session_closed_hooks, FALSE);
  while (hook) {
    fn = (GFunc)hook->func;
    fn(session, hook->data);
    hook = g_hook_next_valid (session_closed_hooks, hook, FALSE);
  }
}

/* ====================================================================== */
/* error handling routines */

static void
qof_session_clear_error (QofSession *session)
{
  QofBackendError err;

  session->last_err = ERR_BACKEND_NO_ERR;
  g_free(session->error_message);
  session->error_message = NULL;

  /* pop the stack on the backend as well. */
  if (session->backend)
  {
    do
    {
       err = qof_backend_get_error (session->backend);
    } while (ERR_BACKEND_NO_ERR != err);
  }
}

void
qof_session_push_error (QofSession *session, QofBackendError err,
                        const char *message)
{
  if (!session) return;

  g_free (session->error_message);

  session->last_err = err;
  session->error_message = g_strdup (message);
}

QofBackendError
qof_session_get_error (QofSession * session)
{
  QofBackendError err;

  if (!session) return ERR_BACKEND_NO_BACKEND;

  /* if we have a local error, return that. */
  if (ERR_BACKEND_NO_ERR != session->last_err)
  {
    return session->last_err;
  }

  /* maybe we should return a no-backend error ??? */
  if (! session->backend) return ERR_BACKEND_NO_ERR;

  err = qof_backend_get_error (session->backend);
  session->last_err = err;
  return err;
}

static const char *
get_default_error_message(QofBackendError err)
{
    return "";
}

const char *
qof_session_get_error_message(QofSession *session)
{
    if(!session) return "";
    if(!session->error_message)
      return get_default_error_message(session->last_err);
    return session->error_message;
}

QofBackendError
qof_session_pop_error (QofSession * session)
{
  QofBackendError err;

  if (!session) return ERR_BACKEND_NO_BACKEND;

  err = qof_session_get_error(session);
  qof_session_clear_error(session);

  return err;
}

/* ====================================================================== */

static void
qof_session_init (QofSession *session)
{
  if (!session) return;

  session->books = g_list_append (NULL, qof_book_new ());
  session->book_id = NULL;
  session->backend = NULL;

  qof_session_clear_error (session);
}

QofSession *
qof_session_new (void)
{
  QofSession *session = g_new0(QofSession, 1);
  qof_session_init(session);
  return session;
}

QofSession *
qof_session_get_current_session (void)
{
  if (!current_session)
  {
    gnc_engine_suspend_events ();
    current_session = qof_session_new ();
    gnc_engine_resume_events ();
  }

  return current_session;
}

void
qof_session_set_current_session (QofSession *session)
{
  current_session = session;
}

QofBook *
qof_session_get_book (QofSession *session)
{
   GList *node;
   if (!session) return NULL;

   for (node=session->books; node; node=node->next)
   {
      QofBook *book = node->data;
      if ('y' == book->book_open) return book;
   }
   return NULL;
}

void
qof_session_add_book (QofSession *session, QofBook *addbook)
{
  GList *node;
  if (!session) return;

  ENTER (" sess=%p book=%p", session, addbook);

  /* See if this book is already there ... */
  for (node=session->books; node; node=node->next)
  {
     QofBook *book = node->data;
     if (addbook == book) return;
  }

  if ('y' == addbook->book_open)
  {
    /* hack alert -- someone should free all the books in the list,
     * but it should probably not be us ... since the books backends
     * should be shutdown first, etc */
/* XXX this should probably be an error XXX */
    g_list_free (session->books);
    session->books = g_list_append (NULL, addbook);
  }
  else 
  {
/* XXX Need to tell the backend to add a book as well */
    session->books = g_list_append (session->books, addbook);
  }

  qof_book_set_backend (addbook, session->backend);
  LEAVE (" ");
}

QofBackend * 
qof_session_get_backend (QofSession *session)
{
   if (!session) return NULL;
   return session->backend;
}

const char *
qof_session_get_file_path (QofSession *session)
{
   if (!session) return NULL;
   if (!session->backend) return NULL;
   return session->backend->fullpath;
}

const char *
qof_session_get_url (QofSession *session)
{
   if (!session) return NULL;
   return session->book_id;
}

/* =============================================================== */

typedef struct qof_entity_copy_data {
	QofEntity *from;
	QofEntity *to;
	GHashTable *referenceTable;	
	GSList *param_list;
	QofSession *new_session;
	gboolean error;
}QofEntityCopyData;

static void
qof_entity_param_cb(QofParam *param, gpointer data)
{
	QofEntityCopyData *qecd;

	g_return_if_fail(data != NULL);
	qecd = (QofEntityCopyData*)data;
	g_return_if_fail(qecd != NULL);
	if((param->param_getfcn != NULL)&&(param->param_setfcn != NULL)) {
			qecd->param_list = g_slist_append(qecd->param_list, param);
	}
	if(g_slist_length(qecd->param_list) == 0) { qecd->error = TRUE; }
}

static void
qof_entity_foreach_copy(gpointer data, gpointer user_data)
{
	QofEntity 		*importEnt, *targetEnt, *referenceEnt;
	QofEntityCopyData 	*context;
	QofEntityReference  *reference;
	gboolean		registered_type;
	/* cm_ prefix used for variables that hold the data to commit */
	QofParam 		*cm_param;
	gchar 			*cm_string, *cm_char;
	const GUID 		*cm_guid;
	GUID            *cm_src_guid;
	KvpFrame 		*cm_kvp;
	char 		    cm_sa[GUID_ENCODING_LENGTH + 1];
	/* function pointers and variables for parameter getters that don't use pointers normally */
	gnc_numeric 	cm_numeric, (*numeric_getter)	(QofEntity*, QofParam*);
	double 			cm_double, 	(*double_getter)	(QofEntity*, QofParam*);
	gboolean 		cm_boolean, (*boolean_getter)	(QofEntity*, QofParam*);
	gint32 			cm_i32, 	(*int32_getter)		(QofEntity*, QofParam*);
	gint64 			cm_i64, 	(*int64_getter)		(QofEntity*, QofParam*);
	Timespec 		cm_date, 	(*date_getter)		(QofEntity*, QofParam*);
	/* function pointers to the parameter setters */
	void	(*string_setter)	(QofEntity*, const char*);
	void	(*date_setter)		(QofEntity*, Timespec);
	void	(*numeric_setter)	(QofEntity*, gnc_numeric);
	void	(*guid_setter)		(QofEntity*, const GUID*);
	void	(*double_setter)	(QofEntity*, double);
	void	(*boolean_setter)	(QofEntity*, gboolean);
	void	(*i32_setter)		(QofEntity*, gint32);
	void	(*i64_setter)		(QofEntity*, gint64);
	void	(*char_setter)		(QofEntity*, char*);
	void	(*kvp_frame_setter)	(QofEntity*, KvpFrame*);
	
	g_return_if_fail(data != NULL);
	g_return_if_fail(user_data != NULL);
	context = (QofEntityCopyData*) user_data;
	importEnt = context->from;
	targetEnt = context->to;
	registered_type = FALSE;
	cm_param = (QofParam*) data;
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_STRING) == 0)  { 
		cm_string = g_strdup(cm_param->param_getfcn(importEnt, cm_param));
		string_setter = (void(*)(QofEntity*, const char*))cm_param->param_setfcn;
		if(string_setter != NULL) {	string_setter(targetEnt, cm_string); }
		registered_type = TRUE;
	}
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_DATE) == 0) { 
		date_getter = (Timespec (*)(QofEntity*, QofParam*))cm_param->param_getfcn;
		cm_date = date_getter(importEnt, cm_param);
		date_setter = (void(*)(QofEntity*, Timespec))cm_param->param_setfcn;
		if(date_setter != NULL) { date_setter(targetEnt, cm_date); }
		registered_type = TRUE;
	}
	if((safe_strcmp(cm_param->param_type, QOF_TYPE_NUMERIC) == 0)  ||
	(safe_strcmp(cm_param->param_type, QOF_TYPE_DEBCRED) == 0)) { 
		numeric_getter = (gnc_numeric (*)(QofEntity*, QofParam*))cm_param->param_getfcn;
		cm_numeric = numeric_getter(importEnt, cm_param);
		numeric_setter = (void(*)(QofEntity*, gnc_numeric))cm_param->param_setfcn;
		if(numeric_setter != NULL) { numeric_setter(targetEnt, cm_numeric); }
		registered_type = TRUE;
	}
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_GUID) == 0) { 
		cm_guid = cm_param->param_getfcn(importEnt, cm_param);
		guid_setter = (void(*)(QofEntity*, const GUID*))cm_param->param_setfcn;
		if(guid_setter != NULL) { guid_setter(targetEnt, cm_guid); }
		registered_type = TRUE;
	}
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_INT32) == 0) { 
		int32_getter = (gint32 (*)(QofEntity*, QofParam*)) cm_param->param_getfcn;
		cm_i32 = int32_getter(importEnt, cm_param);
		i32_setter = (void(*)(QofEntity*, gint32))cm_param->param_setfcn;
		if(i32_setter != NULL) { i32_setter(targetEnt, cm_i32); }
		registered_type = TRUE;
	}
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_INT64) == 0) { 
		int64_getter = (gint64 (*)(QofEntity*, QofParam*)) cm_param->param_getfcn;
		cm_i64 = int64_getter(importEnt, cm_param);
		i64_setter = (void(*)(QofEntity*, gint64))cm_param->param_setfcn;
		if(i64_setter != NULL) { i64_setter(targetEnt, cm_i64); }
		registered_type = TRUE;
	}
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_DOUBLE) == 0) { 
		double_getter = (double (*)(QofEntity*, QofParam*)) cm_param->param_getfcn;
		cm_double = double_getter(importEnt, cm_param);
		double_setter = (void(*)(QofEntity*, double))cm_param->param_setfcn;
		if(double_setter != NULL) { double_setter(targetEnt, cm_double); }
		registered_type = TRUE;
	}
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_BOOLEAN) == 0){ 
		boolean_getter = (gboolean (*)(QofEntity*, QofParam*)) cm_param->param_getfcn;
		cm_boolean = boolean_getter(importEnt, cm_param);
		boolean_setter = (void(*)(QofEntity*, gboolean))cm_param->param_setfcn;
		if(boolean_setter != NULL) { boolean_setter(targetEnt, cm_boolean); }
		registered_type = TRUE;
	}
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_KVP) == 0) { 
		cm_kvp = kvp_frame_copy(cm_param->param_getfcn(importEnt,cm_param));
		kvp_frame_setter = (void(*)(QofEntity*, KvpFrame*))cm_param->param_setfcn;
		if(kvp_frame_setter != NULL) { kvp_frame_setter(targetEnt, cm_kvp); }
		registered_type = TRUE;
	}
	if(safe_strcmp(cm_param->param_type, QOF_TYPE_CHAR) == 0) { 
		cm_char = cm_param->param_getfcn(importEnt,cm_param);
		char_setter = (void(*)(QofEntity*, char*))cm_param->param_setfcn;
		if(char_setter != NULL) { char_setter(targetEnt, cm_char); }
		registered_type = TRUE;
	}
	if(registered_type == FALSE) {
		referenceEnt = cm_param->param_getfcn(importEnt, cm_param);
		reference = g_new(QofEntityReference, 1);
		reference->type = g_strdup(referenceEnt->e_type);
		reference->guid = g_new(GUID, 1);
		cm_guid = qof_entity_get_guid(referenceEnt);
		guid_to_string_buff(cm_guid, cm_sa);
		cm_string = g_strdup(cm_sa);
		if(TRUE == string_to_guid(cm_string, reference->guid)) {
			cm_src_guid = &importEnt->guid;
			g_hash_table_insert(context->referenceTable, cm_src_guid, reference);
		}
	}
}

static gboolean
qof_entity_guid_match(QofSession *new_session, QofEntity *original)
{
	QofEntity *copy;
	const GUID *g;
	QofIdTypeConst type;
	QofBook *targetBook;
	QofCollection *coll;
	
	copy = NULL;
	g_return_val_if_fail(original != NULL, FALSE);
	targetBook = qof_session_get_book(new_session);
	g_return_val_if_fail(targetBook != NULL, FALSE);
	g = qof_entity_get_guid(original);
	type = g_strdup(original->e_type);
	coll = qof_book_get_collection(targetBook, type);
	copy = qof_collection_lookup_entity(coll, g);
	if(copy) { return TRUE; }
	return FALSE;	
}

static void
qof_entity_list_foreach(gpointer data, gpointer user_data)
{
	QofEntityCopyData *qecd;
	QofEntity *original;
	QofInstance *inst;
	QofBook *book;
	const GUID *g;
	
	g_return_if_fail(data != NULL);
	original = (QofEntity*)data;
	g_return_if_fail(user_data != NULL);
	qecd = (QofEntityCopyData*)user_data;
	qecd->from = original;
	book = qof_session_get_book(qecd->new_session);
	inst = (QofInstance*)qof_object_new_instance(original->e_type, book);
	qecd->to = &inst->entity;
	g = qof_entity_get_guid(original);
	qof_entity_set_guid(qecd->to, g);
	qof_class_param_foreach(original->e_type, qof_entity_param_cb, qecd);
	g_slist_foreach(qecd->param_list, qof_entity_foreach_copy, qecd);
	qof_book_set_data(book, ENTITYREFERENCE, qecd->referenceTable);
	qof_book_set_data(book, PARTIAL_QOFBOOK, (gboolean*)TRUE);
}

static void
qof_entity_coll_foreach(QofEntity *original, gpointer user_data)
{
	QofEntityCopyData *qecd;
	const GUID *g;
	QofBook *targetBook;
	QofCollection *coll;
	QofEntity *copy;
	
	g_return_if_fail(user_data != NULL);
	qecd = (QofEntityCopyData*)user_data;
	targetBook = qof_session_get_book(qecd->new_session);
	g = qof_entity_get_guid(original);
	coll = qof_book_get_collection(targetBook, original->e_type);
	copy = qof_collection_lookup_entity(coll, g);
	if(copy) { qecd->error = TRUE; }
}

static void
qof_entity_coll_copy(QofEntity *original, gpointer user_data)
{
	QofEntityCopyData *qecd;
	QofBook *book;
	QofInstance *inst;
	const GUID *g;
	
	g_return_if_fail(user_data != NULL);
	qecd = (QofEntityCopyData*)user_data;
	book = qof_session_get_book(qecd->new_session);
	inst = (QofInstance*)qof_object_new_instance(original->e_type, book);
	qecd->to = &inst->entity;
	qecd->from = original;
	g = qof_entity_get_guid(original);
	qof_entity_set_guid(qecd->to, g);
	qof_class_param_foreach(original->e_type, qof_entity_param_cb, qecd);
	g_slist_foreach(qecd->param_list, qof_entity_foreach_copy, qecd);
	qof_book_set_data(book, ENTITYREFERENCE, qecd->referenceTable);
	qof_book_set_data(book, PARTIAL_QOFBOOK, (gboolean*)TRUE);
}

gboolean qof_entity_copy_to_session(QofSession* new_session, QofEntity* original)
{
	QofEntityCopyData qecd;
	QofInstance *inst;
	QofBook *book;
	const GUID *g;

	if(qof_entity_guid_match(new_session, original)) return FALSE;
	qecd.param_list = NULL;
	qecd.referenceTable = g_hash_table_new(NULL, NULL);
	qecd.new_session = new_session;
	qecd.error = FALSE;
	book = qof_session_get_book(new_session);
	inst = (QofInstance*)qof_object_new_instance(original->e_type, book);
	qecd.to = &inst->entity;
	qecd.from = original;
	g = qof_entity_get_guid(original);
	qof_entity_set_guid(qecd.to, g);
	qof_class_param_foreach(original->e_type, qof_entity_param_cb, &qecd);
	g_slist_foreach(qecd.param_list, qof_entity_foreach_copy, &qecd);
	qof_book_set_data(book, ENTITYREFERENCE, qecd.referenceTable);
	qof_book_set_data(book, PARTIAL_QOFBOOK, (gboolean*)TRUE);
	return TRUE;
}

gboolean qof_entity_copy_list(QofSession *new_session, GList *entity_list)
{
	GList *e;
	QofEntity *original;
	QofEntityCopyData qecd;

	qecd.param_list = NULL;
	qecd.new_session = new_session;
	qecd.referenceTable = g_hash_table_new(NULL, NULL);
	qecd.error = FALSE;
	for(e=entity_list; e; e=e->next)
	{
		original = (QofEntity*) e->data;
		/* The GList can contain mixed entity types, it may 
		appear slow, but we do need to check the type every time
		because we can't re-use the QofCollection or QofIdType.	*/
		if(qof_entity_guid_match(new_session, original)) return FALSE;
	}
	g_list_foreach(entity_list, qof_entity_list_foreach, &qecd);
	return TRUE;
}

gboolean qof_entity_copy_coll(QofSession *new_session, QofCollection *entity_coll)
{
	QofEntityCopyData qecd;

	qecd.param_list = NULL;
	qecd.new_session = new_session;
	qecd.referenceTable = g_hash_table_new(NULL, NULL);
	qecd.error = FALSE;
	qof_collection_foreach(entity_coll, qof_entity_coll_foreach, &qecd);
	if(qecd.error == TRUE) return FALSE;
	qof_collection_foreach(entity_coll, qof_entity_coll_copy, &qecd);
	return TRUE;
}

/* ====================================================================== */

/* Specify a library, and a function name. Load the library, 
 * call the function name in the library.  */
static void
load_backend_library (const char * libso, const char * loadfn)
{
	void (*initfn) (void);
	void *dl_hand = dlopen (libso, RTLD_LAZY);
	if (NULL == dl_hand)
	{
		const char * err_str = dlerror();
		PERR("Can't load %s backend, %s\n", libso, err_str);
		return;
	}
	initfn = dlsym (dl_hand, loadfn);
	if (initfn)
	{
		 (*initfn)();
	}
	else
	{
		const char * err_str = dlerror();
		PERR("Can't find %s:%s, %s\n", libso, loadfn, err_str);
	}
}

#ifdef GNUCASH_MAJOR_VERSION 

static void
qof_session_int_backend_load_error(QofSession *session,
                                   char *message, char *dll_err)
{
    PWARN ("%s %s", message, dll_err ? dll_err : "");

    g_free(session->book_id);
    session->book_id = NULL;

    qof_session_push_error (session, ERR_BACKEND_NO_BACKEND, NULL);
}

/* Gnucash uses its module system to load a backend; other users
 * use traditional dlopen calls.
 */
static void
qof_session_load_backend(QofSession * session, char * backend_name)
{
  GList *node;
  QofBook *book;
	GNCModule  mod;
	GSList    *p;
	char       *mod_name, *access_method, *msg;
  QofBackend    *(* be_new_func)(void);
	QofBackendProvider *prov;

	mod	= 0;
	mod_name = g_strdup_printf("gnucash/backend/%s", backend_name);
	msg = g_strdup_printf(" ");
  /* FIXME : reinstate better error messages with gnc_module errors */
  ENTER (" ");
  /* FIXME: this needs to be smarter with version numbers. */
  /* FIXME: this should use dlopen(), instead of guile/scheme, 
   *    to load the modules.  Right now, this requires the engine to
   *    link to scheme, which is an obvious architecture flaw. 
	*    XXX this is fixed below, in the non-gnucash version. Cut
   *    over at some point.
   */
  mod = gnc_module_load(mod_name, 0);
  if (mod) 
  {
    be_new_func = gnc_module_lookup(mod, "gnc_backend_new");
    if(be_new_func) 
    {
      session->backend = be_new_func();
      for (node=session->books; node; node=node->next)
      {
         book = node->data;
         qof_book_set_backend (book, session->backend);
      }
    }
		else { qof_session_int_backend_load_error(session, " can't find backend_new ",""); }
  }
  else
  {
	/* QSF is built for the QOF version, use that if no module is found.
	  This allows the GnuCash version to be called with an access method,
	  as it would be in QOF, instead of a resolved module name.
	*/
	access_method = g_strdup(backend_name);
	for (p = provider_list; p; p=p->next)
	{
		prov = p->data;
		/* Does this provider handle the desired access method? */
		if (0 == strcasecmp (access_method, prov->access_method))
		{
			if (NULL == prov->backend_new) continue;
			/* Use the providers creation callback */
        	session->backend = (*(prov->backend_new))();
			session->backend->provider = prov;
			/* Tell the books about the backend that they'll be using. */
			for (node=session->books; node; node=node->next)
	{
				book = node->data;
				qof_book_set_backend (book, session->backend);
			}
		return;
	}
	}
	msg = g_strdup_printf("failed to load '%s' backend", backend_name);
	qof_session_push_error (session, ERR_BACKEND_NO_HANDLER, msg);
	}
  g_free(mod_name);
  LEAVE (" ");
}

#else /* GNUCASH */

static void
qof_session_load_backend(QofSession * session, char * access_method)
{
	GSList *p;
	GList *node;
	QofBackendProvider *prov;
	QofBook *book;

	ENTER (" ");
	/* If the provider list is null, try to register the 'well-known'
	 *  backends. Right now, there's only two. */
	if (NULL == provider_list)
	{
		/* hack alert: If you change this, change qof_session_save as well. */
#ifdef BUILD_DWI
		load_backend_library ("libqof_backend_dwi.so", "dwiend_provider_init");
#endif
		load_backend_library ("libqof-backend-qsf.so", "qsf_provider_init" );
	}
	p = g_slist_copy(provider_list);
	while(p != NULL)
	{
		prov = p->data;
		/* Does this provider handle the desired access method? */
		if (0 == strcasecmp (access_method, prov->access_method))
		{
			if (NULL == prov->backend_new) continue;
			/* Use the providers creation callback */
      	session->backend = (*(prov->backend_new))();
			session->backend->provider = prov;
			/* Tell the books about the backend that they'll be using. */
			for (node=session->books; node; node=node->next)
			{
				book = node->data;
				qof_book_set_backend (book, session->backend);
			}
			return;
		}
		p = p->next;
	}
	msg = g_strdup_printf("failed to load '%s' backend", backend_name);
	qof_session_push_error (session, ERR_BACKEND_NO_HANDLER, msg);
	LEAVE (" ");
}
#endif /* GNUCASH */

/* ====================================================================== */

static void
qof_session_destroy_backend (QofSession *session)
{
  g_return_if_fail (session);

  if (session->backend)
  {
    /* clear any error message */
    char * msg = qof_backend_get_message (session->backend);
    g_free (msg);

    /* Then destroy the backend */
    if (session->backend->destroy_backend)
    {
      session->backend->destroy_backend(session->backend);
    }
    else
    {
      g_free(session->backend);
    }
  }

  session->backend = NULL;
}

void
qof_session_begin (QofSession *session, const char * book_id, 
                   gboolean ignore_lock, gboolean create_if_nonexistent)
{
  char *p, *access_method, *msg;
  if (!session) return;
  int err;

  ENTER (" sess=%p ignore_lock=%d, book-id=%s", 
         session, ignore_lock,
         book_id ? book_id : "(null)");

  /* Clear the error condition of previous errors */
  qof_session_clear_error (session);

  /* Check to see if this session is already open */
  if (session->book_id)
  {
    qof_session_push_error (session, ERR_BACKEND_LOCKED, NULL);
    LEAVE("push error book is already open ");
    return;
  }

  /* seriously invalid */
  if (!book_id)
  {
    qof_session_push_error (session, ERR_BACKEND_BAD_URL, NULL);
    LEAVE("push error missing book_id");
    return;
  }

  /* Store the session URL  */
  session->book_id = g_strdup (book_id);

  /* destroy the old backend */
  qof_session_destroy_backend(session);

  /* Look for something of the form of "file:/", "http://" or 
   * "postgres://". Everything before the colon is the access 
   * method.  Load the first backend found for that access method.
   */
  p = strchr (book_id, ':');
  if (p)
  {
    access_method = g_strdup (book_id);
    p = strchr (access_method, ':');
    *p = 0;
    qof_session_load_backend(session, access_method);
    g_free (access_method);
  }
  else
  {
     /* If no colon found, assume it must be a file-path */
     qof_session_load_backend(session, "file"); 
  }

  /* No backend was found. That's bad. */
  if (NULL == session->backend)
  {
    qof_session_push_error (session, ERR_BACKEND_BAD_URL, NULL);
    LEAVE (" BAD: no backend: sess=%p book-id=%s", 
         session,  book_id ? book_id : "(null)");
    return;
  }

  /* If there's a begin method, call that. */
  if (session->backend->session_begin)
  {
      
      (session->backend->session_begin)(session->backend, session,
                                  session->book_id, ignore_lock,
                                  create_if_nonexistent);
      PINFO("Done running session_begin on backend");
      err = qof_backend_get_error(session->backend);
      msg = qof_backend_get_message(session->backend);
      if (err != ERR_BACKEND_NO_ERR)
      {
          g_free(session->book_id);
          session->book_id = NULL;
          qof_session_push_error (session, err, msg);
          LEAVE("backend error %d", err);
          return;
      }
      if (msg != NULL) 
      {
          PWARN("%s", msg);
          g_free(msg);
      }
  }

  LEAVE (" sess=%p book-id=%s", 
         session,  book_id ? book_id : "(null)");
}

/* ====================================================================== */

void
qof_session_load (QofSession *session,
                  QofPercentageFunc percentage_func)
{
	QofBook *newbook, *ob;
  QofBookList *oldbooks, *node;
  QofBackend *be;
  QofBackendError err;

  if (!session) return;
  if (!session->book_id) return;

  ENTER ("sess=%p book_id=%s", session, session->book_id
         ? session->book_id : "(null)");

  /* At this point, we should are supposed to have a valid book 
   * id and a lock on the file. */

  oldbooks = session->books;

  /* XXX why are we creating a book here? I think the books
   * need to be handled by the backend ... especially since 
   * the backend may need to load multiple books ... XXX. FIXME.
   */
  newbook = qof_book_new();
  session->books = g_list_append (NULL, newbook);
  PINFO ("new book=%p", newbook);

  qof_session_clear_error (session);

  /* This code should be sufficient to initialize *any* backend,
   * whether http, postgres, or anything else that might come along.
   * Basically, the idea is that by now, a backend has already been
   * created & set up.  At this point, we only need to get the
   * top-level account group out of the backend, and that is a
   * generic, backend-independent operation.
   */
  be = session->backend;
  qof_book_set_backend(newbook, be);

  /* Starting the session should result in a bunch of accounts
   * and currencies being downloaded, but probably no transactions;
   * The GUI will need to do a query for that.
   */
  if (be)
  {
      be->percentage = percentage_func;

      if (be->load) 
      {
          be->load (be, newbook);
          qof_session_push_error (session, qof_backend_get_error(be), NULL);
      }
  }

  /* XXX if the load fails, then we try to restore the old set of books;
   * however, we don't undo the session id (the URL).  Thus if the 
   * user attempts to save after a failed load, they weill be trying to 
   * save to some bogus URL.   This is wrong. XXX  FIXME.
   */
  err = qof_session_get_error(session);
  if ((err != ERR_BACKEND_NO_ERR) &&
      (err != ERR_FILEIO_FILE_TOO_OLD) &&
      (err != ERR_SQL_DB_TOO_OLD))
  {
      /* Something broke, put back the old stuff */
      qof_book_set_backend (newbook, NULL);
      qof_book_destroy (newbook);
      g_list_free (session->books);
      session->books = oldbooks;
      LEAVE("error from backend %d", qof_session_get_error(session));
      return;
  }

  for (node=oldbooks; node; node=node->next)
  {
		ob = node->data;
     qof_book_set_backend (ob, NULL);
     qof_book_destroy (ob);
  }

  LEAVE ("sess = %p, book_id=%s", session, session->book_id
         ? session->book_id : "(null)");
}

/* ====================================================================== */

gboolean
qof_session_save_may_clobber_data (QofSession *session)
{
  if (!session) return FALSE;
  if (!session->backend) return FALSE;
  if (!session->backend->save_may_clobber_data) return FALSE;

  return (*(session->backend->save_may_clobber_data)) (session->backend);
}

static gboolean
save_error_handler(QofBackend *be, QofSession *session)
{
    int err;
    err = qof_backend_get_error(be);
    
    if (ERR_BACKEND_NO_ERR != err)
    {
        qof_session_push_error (session, err, NULL);
        return TRUE;
    }
    return FALSE;
}

void
qof_session_save (QofSession *session,
                  QofPercentageFunc percentage_func)
{
  GList *node;
  QofBackend *be;
	gboolean partial, change_backend;
	QofBackendProvider *prov;
	GSList *p;
	QofBook *book, *abook;
	int err;
	char *msg, *book_id;

  if (!session) return;
  ENTER ("sess=%p book_id=%s", 
         session, session->book_id ? session->book_id : "(null)");
	/* Partial book handling. */
	book = qof_session_get_book(session);
	partial = (gboolean)qof_book_get_data(book, PARTIAL_QOFBOOK);
	change_backend = FALSE;
	msg = g_strdup_printf(" ");
	book_id = g_strdup(session->book_id);
	if(partial == TRUE)
	{
		if(session->backend->provider) {
			prov = session->backend->provider;
			if(TRUE == prov->partial_book_supported)
			{
				/* if current backend supports partial, leave alone. */
				change_backend = FALSE;
			}
			else { change_backend = TRUE; }
		}
		/* If provider is undefined, assume partial not supported. */
		else { change_backend = TRUE; }
	}
	if(change_backend == TRUE)
	{
		qof_session_destroy_backend(session);
		if (NULL == provider_list)
		{
			load_backend_library ("libqsf-backend-file.so", "qsf_provider_init" );
		}
		p = g_slist_copy(provider_list);
		while(p != NULL)
		{
			prov = p->data;
			if(TRUE == prov->partial_book_supported)
			{
			/** \todo check the access_method too, not in scope here, yet. */
			/*	if((TRUE == prov->partial_book_supported) && 
			(0 == strcasecmp (access_method, prov->access_method)))
			{*/
				if (NULL == prov->backend_new) continue;
				/* Use the providers creation callback */
				session->backend = (*(prov->backend_new))();
				session->backend->provider = prov;
				if (session->backend->session_begin)
				{
					/* Call begin - what values to use for booleans? */
					g_free(session->book_id);
					session->book_id = NULL;
					(session->backend->session_begin)(session->backend, session,
						book_id, TRUE, FALSE);
					PINFO("Done running session_begin on changed backend");
					err = qof_backend_get_error(session->backend);
					msg = qof_backend_get_message(session->backend);
					if (err != ERR_BACKEND_NO_ERR)
					{
						g_free(session->book_id);
						session->book_id = NULL;
						qof_session_push_error (session, err, msg);
						LEAVE("changed backend error %d", err);
						return;
					}
					if (msg != NULL) 
					{
						PWARN("%s", msg);
						g_free(msg);
					}
				}
				/* Tell the books about the backend that they'll be using. */
				for (node=session->books; node; node=node->next)
				{
					book = node->data;
					qof_book_set_backend (book, session->backend);
				}
				p = NULL;
			}
			if(p) {
				p = p->next;
			}
		}
		if(!session->backend) 
		{
			msg = g_strdup_printf("failed to load backend");
			qof_session_push_error(session, ERR_BACKEND_NO_HANDLER, msg);
			return;
		}
	}
	g_free(book_id);
  /* If there is a backend, and the backend is reachable
   * (i.e. we can communicate with it), then synchronize with 
   * the backend.  If we cannot contact the backend (e.g.
   * because we've gone offline, the network has crashed, etc.)
   * then give the user the option to save to the local disk. 
   *
   * hack alert -- FIXME -- XXX the code below no longer
   * does what the words above say.  This needs fixing.
   */
  be = session->backend;
  if (be)
  {
    for (node = session->books; node; node=node->next)
    {
			abook = node->data;
      /* if invoked as SaveAs(), then backend not yet set */
      qof_book_set_backend (abook, be);
      be->percentage = percentage_func;
      if (be->sync)
      {
        (be->sync)(be, abook);
        if (save_error_handler(be, session)) return;
      }
    }
    /* If we got to here, then the backend saved everything 
     * just fine, and we are done. So return. */
    qof_session_clear_error (session);
    LEAVE("Success");
    return;
  } 
	else
	{
		msg = g_strdup_printf("failed to load backend");
		qof_session_push_error(session, ERR_BACKEND_NO_HANDLER, msg);
	}
  LEAVE("error -- No backend!");
}

/* ====================================================================== */
/* XXX This exports the list of accounts to a file.  It does not export
 * any transactions.  Its a place-holder until full book-closing is implemented.
 */

#ifdef GNUCASH_MAJOR_VERSION

gboolean
qof_session_export (QofSession *tmp_session,
                    QofSession *real_session,
                    QofPercentageFunc percentage_func)
{
  QofBook *book;
  QofBackend *be;

  if ((!tmp_session) || (!real_session)) return FALSE;

  book = qof_session_get_book (real_session);
  ENTER ("tmp_session=%p real_session=%p book=%p book_id=%s", 
         tmp_session, real_session, book,
         tmp_session -> book_id
         ? tmp_session->book_id : "(null)");

  /* There must be a backend or else.  (It should always be the file
   * backend too.)
   */
  be = tmp_session->backend;
  if (!be)
    return FALSE;

  be->percentage = percentage_func;
  if (be->export)
    {

      (be->export)(be, book);
      if (save_error_handler(be, tmp_session)) return FALSE;
    }

  return TRUE;
}
#endif /* GNUCASH_MAJOR_VERSION */

/* ====================================================================== */

void
qof_session_end (QofSession *session)
{
  if (!session) return;

  ENTER ("sess=%p book_id=%s", session, session->book_id
         ? session->book_id : "(null)");

  /* close down the backend first */
  if (session->backend && session->backend->session_end)
  {
    (session->backend->session_end)(session->backend);
  }

  qof_session_clear_error (session);

  g_free (session->book_id);
  session->book_id = NULL;

  LEAVE ("sess=%p book_id=%s", session, session->book_id
         ? session->book_id : "(null)");
}

void 
qof_session_destroy (QofSession *session) 
{
  GList *node;
  if (!session) return;

  ENTER ("sess=%p book_id=%s", session, session->book_id
         ? session->book_id : "(null)");

  qof_session_end (session);

  /* destroy the backend */
  qof_session_destroy_backend(session);

  for (node=session->books; node; node=node->next)
  {
    QofBook *book = node->data;
    qof_book_set_backend (book, NULL);
    qof_book_destroy (book);
  }

  session->books  = NULL;
  if (session == current_session)
    current_session = NULL;

  g_free (session);

  LEAVE ("sess=%p", session);
}

/* ====================================================================== */
/* this call is weird. */

void
qof_session_swap_data (QofSession *session_1, QofSession *session_2)
{
  GList *books_1, *books_2, *node;

  if (session_1 == session_2) return;
  if (!session_1 || !session_2) return;

  ENTER ("sess1=%p sess2=%p", session_1, session_2);

  books_1 = session_1->books;
  books_2 = session_2->books;

  session_1->books = books_2;
  session_2->books = books_1;

  for (node=books_1; node; node=node->next)
  {
    QofBook *book_1 = node->data;
    qof_book_set_backend (book_1, session_2->backend);
  }
  for (node=books_2; node; node=node->next)
  {
    QofBook *book_2 = node->data;
    qof_book_set_backend (book_2, session_1->backend);
  }

  LEAVE (" ");
}

/* ====================================================================== */

gboolean
qof_session_events_pending (QofSession *session)
{
  if (!session) return FALSE;
  if (!session->backend) return FALSE;
  if (!session->backend->events_pending) return FALSE;

  return session->backend->events_pending (session->backend);
}

gboolean
qof_session_process_events (QofSession *session)
{
  if (!session) return FALSE;
  if (!session->backend) return FALSE;
  if (!session->backend->process_events) return FALSE;

  return session->backend->process_events (session->backend);
}

/* ====================================================================== */

#ifdef GNUCASH_MAJOR_VERSION

/* this should go in a separate binary to create a rpc server */

void
gnc_run_rpc_server (void)
{
  const char * dll_err;
  void * dll_handle;
  int (*rpc_run)(short);
  int ret;
 
  /* open and resolve all symbols now (we don't want mystery 
   * failure later) */
#ifndef RTLD_NOW
# ifdef RTLD_LAZY
#  define RTLD_NOW RTLD_LAZY
# endif
#endif
  dll_handle = dlopen ("libgnc_rpc.so", RTLD_NOW);
  if (! dll_handle) 
  {
    dll_err = dlerror();
    PWARN (" can't load library: %s\n", dll_err ? dll_err : "");
    return;
  }
  
  rpc_run = dlsym (dll_handle, "rpc_server_run");
  dll_err = dlerror();
  if (dll_err) 
  {
    dll_err = dlerror();
    PWARN (" can't find symbol: %s\n", dll_err ? dll_err : "");
    return;
  }
  
  ret = (*rpc_run)(0);

  /* XXX How do we force an exit? */
}
#endif /* GNUCASH_MAJOR_VERSION */

/* =================== END OF FILE ====================================== */
