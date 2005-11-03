/***************************************************************************
 *            qofundo.c
 *
 *  Thu Aug 25 09:19:17 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
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
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <libintl.h>
#include <locale.h>
#include <errno.h>
#include "qofundo-p.h"
#include "qofundo.h"
/* for gettext support - change this later. */
#include "qof-main.h"

/* the undo list itself 
 definition will later be placed within the QofBook opaque struct
 GList of qof_undo_event*
 */
static GList *book_undo;
/* list of undo_entity, pending commit */
static GList *undo_cache;
static gchar* undo_label;
/* the current position within the undo list within this book */
static gint index_position; 
static gboolean undo_operation_open = FALSE;

typedef enum
{
	UNDO_NOOP = 0,
	UNDO_CREATE,
	UNDO_DELETE,
	UNDO_MODIFY
}undo_action;

struct qof_undo_entity_t
{
	QofParam *param;  /* static anyway so only store a pointer */
	const GUID *guid; /* enable re-creation of this entity */
	QofIdType type;   /* ditto param, static. */
	char *value;      /* cached string? */
	char *path;       /* for KVP */
	QofIdType choice; /* For QOF_TYPE_CHOICE */
	undo_action how;  /* how to act on the undo */
};

struct qof_undo_operation_t
{
	const char* label;
	Timespec ts;
	GList *entity_list; /* GList of qof_undo_entity* */
};

void
qof_entity_set_param(QofEntity *ent, QofParam *param, char *value)
{
	gchar *tail;
	gnc_numeric cli_numeric;
	gboolean    cli_bool;
	gint32      cli_i32;
	gint64      cli_i64;
	Timespec    cli_date;
	GUID        *cm_guid;
	struct tm   cli_time;
	time_t      cli_time_t;
	const char  *fmt;
	void (*string_setter)   (QofEntity*, char*);
	void (*date_setter)     (QofEntity*, Timespec);
	void (*i32_setter)      (QofEntity*, gint32);
	void (*i64_setter)      (QofEntity*, gint64);
	void (*numeric_setter)  (QofEntity*, gnc_numeric);
	void (*boolean_setter)  (QofEntity*, gboolean);
	void (*guid_setter)     (QofEntity*, const GUID*);

	if(0 == safe_strcmp(param->param_type, QOF_TYPE_STRING)) {
		string_setter = (void(*)(QofEntity*, char*))param->param_setfcn;
		if(string_setter) { param->param_setfcn(ent, value); }
	}
	if(0 == safe_strcmp(param->param_type, QOF_TYPE_GUID)) {
		cm_guid = g_new(GUID, 1);
		if(TRUE == string_to_guid(value, cm_guid))
		{
			guid_setter = (void(*)(QofEntity*, const GUID*))param->param_setfcn;
			if(guid_setter != NULL) { guid_setter(ent, cm_guid); }
		}
	}
	if((0 == safe_strcmp(param->param_type, QOF_TYPE_NUMERIC)) || 
			(safe_strcmp(param->param_type, QOF_TYPE_DEBCRED) == 0)) {
		numeric_setter = (void(*)(QofEntity*, gnc_numeric))param->param_setfcn;
		string_to_gnc_numeric(value, &cli_numeric);
		if(numeric_setter != NULL) { numeric_setter(ent, cli_numeric); }
	}
	if(0 == safe_strcmp(param->param_type, QOF_TYPE_BOOLEAN)) {
		cli_bool = FALSE;
		if(qof_util_bool_to_int(value) == 1) { cli_bool = TRUE; }
		boolean_setter = (void(*)(QofEntity*, gboolean))param->param_setfcn;
		if(boolean_setter != NULL) { boolean_setter(ent, cli_bool); }
	}
	if(0 == safe_strcmp(param->param_type, QOF_TYPE_INT32)) {
		errno = 0;
		cli_i32 = (gint32)strtol (value, &tail, 0);
		if(errno == 0) {
			i32_setter = (void(*)(QofEntity*, gint32))param->param_setfcn;
			if(i32_setter != NULL) { i32_setter(ent, cli_i32); }
		}
		 else { 
		 	fmt = _("%s: Cannot convert %s into a number: an overflow has been detected.");
			fprintf (stderr, fmt, PACKAGE, value);
		}
	}
	if(0 == safe_strcmp(param->param_type, QOF_TYPE_INT64)) {
		errno = 0;
		cli_i64 = (gint64)strtol (value, &tail, 0);
		if(errno == 0) {
			i64_setter = (void(*)(QofEntity*, gint64))param->param_setfcn;
			if(i64_setter != NULL) { i64_setter(ent, cli_i64); }
		}
		 else { 
		 	fmt = _("%s: Cannot convert %s into a number: an overflow has been detected.");
			fprintf (stderr, fmt, PACKAGE, value);
		}
	}
	if(0 == safe_strcmp(param->param_type, QOF_TYPE_DATE)) {
		date_setter = (void(*)(QofEntity*, Timespec))param->param_setfcn;
		strptime(value, QOF_UTC_DATE_FORMAT, &cli_time);
		cli_time_t = mktime(&cli_time);
		timespecFromTime_t(&cli_date, cli_time_t);
		if(date_setter != NULL) { date_setter(ent, cli_date); }
	}
	if(0 == safe_strcmp(param->param_type, QOF_TYPE_CHAR)) {
		param->param_setfcn(ent, value);
	}
}

static void
undo_from_kvp_helper(const char *path, KvpValue *content, gpointer data)
{
	qof_undo_entity *undo_entity;

	undo_entity = (qof_undo_entity*)data;
	undo_entity->path = g_strdup(path);
	undo_entity->value = kvp_value_to_bare_string(content);
}

qof_undo_entity* 
qof_prepare_undo (QofEntity *ent, QofParam *param)
{
	qof_undo_entity *undo_entity;
	KvpFrame *undo_frame;

	undo_frame = NULL;
	undo_entity = g_new0(qof_undo_entity, 1);
	undo_entity->guid = qof_entity_get_guid(ent);
	undo_entity->param = param;
	undo_entity->how = UNDO_MODIFY;
	undo_entity->type = ent->e_type;
	undo_entity->value = qof_book_merge_param_as_string(param, ent);
	if(0 == (safe_strcmp(param->param_type, QOF_TYPE_KVP)))
	{
		undo_frame = kvp_frame_copy(param->param_getfcn(ent,param));
		kvp_frame_for_each_slot(undo_frame, undo_from_kvp_helper, undo_entity);
	}
	/* need to do COLLECT and CHOICE */
	return undo_entity;
}

static void
qof_reinstate_entity (qof_undo_entity *undo_entity, QofBook *book)
{
	QofParam *undo_param;
	QofCollection *coll;
	QofEntity *ent;

	undo_param = undo_entity->param;
	if(!undo_param) { return; }
	g_message("reinstate:%s", undo_entity->type);
	coll = qof_book_get_collection(book, undo_entity->type);
	if(!coll) { return; }
	ent = qof_collection_lookup_entity(coll, undo_entity->guid);
	if(!ent) { return; }
	g_message("undoing %s %s", undo_param->param_name, undo_entity->value);
	qof_entity_set_param(ent, undo_param, undo_entity->value);
}

static void
qof_recreate_entity (qof_undo_entity *undo_entity, QofBook *book)
{
	QofEntity *ent;
	const GUID *guid;
	QofIdType type;
	QofInstance *inst;

	guid = undo_entity->guid;
	type = undo_entity->type;
	g_return_if_fail(guid || type);
	inst = (QofInstance*)qof_object_new_instance(type, book);
	ent = (QofEntity*)inst;
	qof_entity_set_guid(ent, guid);
}

static void
qof_dump_entity (qof_undo_entity *undo_entity, QofBook *book)
{
	QofCollection *coll;
	QofEntity *ent;
	const GUID *guid;
	QofIdType type;

	type = undo_entity->type;
	guid = undo_entity->guid;
	g_return_if_fail(type || book);
	coll = qof_book_get_collection(book, type);
	ent = qof_collection_lookup_entity(coll, guid);
	qof_entity_release(ent);
}

void 
qof_book_undo(QofBook *book)
{
	qof_undo_operation *undo_operation;
	qof_undo_entity *undo_entity;
	GList *ent_list;
	gint length;

	length = g_list_length(book_undo);
	if (index_position >1 ) { index_position--; }
	else { index_position = 0; }
	undo_operation = (qof_undo_operation*)(g_list_nth(book_undo, index_position))->data;
	g_return_if_fail(undo_operation);
	ent_list = undo_operation->entity_list;
	while (ent_list != NULL)
	{
		undo_entity = (qof_undo_entity*)ent_list->data;
		if(!undo_entity) { break; }
		switch(undo_entity->how) {
			case UNDO_MODIFY : { qof_reinstate_entity(undo_entity, book); break; }
			case UNDO_CREATE : { qof_recreate_entity(undo_entity, book);  break; }
			case UNDO_DELETE : { qof_dump_entity (undo_entity, book); break; }
			case UNDO_NOOP : { break; }
		}
		ent_list = g_list_next(ent_list);
	}
}

void 
qof_book_redo(QofBook *book)
{
	qof_undo_operation *undo_operation;
	qof_undo_entity *undo_entity;
	GList *ent_list;
	gint length;

	undo_operation = (qof_undo_operation*)(g_list_nth(book_undo, index_position))->data;
	if(!undo_operation) { return; }
	ent_list = undo_operation->entity_list;
	while (ent_list != NULL)
	{
		undo_entity = (qof_undo_entity*)ent_list->data;
		if(!undo_entity) { break; }
		switch(undo_entity->how) {
			case UNDO_MODIFY : { qof_reinstate_entity(undo_entity, book); break; }
			case UNDO_CREATE : { qof_dump_entity (undo_entity, book); break; }
			case UNDO_DELETE : { qof_recreate_entity(undo_entity, book); break; }
			case UNDO_NOOP : { break; }
		}
		ent_list = g_list_next(ent_list);
	}
	length = g_list_length(book_undo);
	if (index_position < length ) { index_position++; }
	else { index_position = length; }
}

void 
qof_book_clear_undo(QofBook *book)
{
	qof_undo_operation *operation;

	if(!book || !book_undo) { return; }
	while (book_undo != NULL)
	{
		operation = (qof_undo_operation*)book_undo->data;
		g_list_free(operation->entity_list);
		book_undo = g_list_next(book_undo);
	}
	index_position = 0;
	g_free(undo_label);
	book_undo = NULL;
	undo_cache = NULL;
}

gboolean 
qof_book_can_undo(QofBook *book)
{
	gint length;
	
	length = g_list_length(book_undo);
	if ((index_position == 0) || (length == 0)) { return FALSE; }
	return TRUE;
}

gboolean 
qof_book_can_redo(QofBook *book)
{
	gint length;

	length = g_list_length(book_undo);
	if ((index_position == length) || (length == 0)) { return FALSE; }
	return TRUE;
}

qof_undo_operation* 
qof_undo_new_operation(char* label)
{
	qof_undo_operation *undo_operation;
	time_t t;
	Timespec ts;

	undo_operation = NULL;
	t = time (NULL);
	timespecFromTime_t(&ts, t);
	undo_operation = g_new0(qof_undo_operation, 1);
	undo_operation->label = label;
	undo_operation->ts =  ts;
	undo_operation->entity_list = NULL;
	g_list_foreach(undo_cache, qof_undo_new_entry, undo_operation);
	undo_cache = NULL;
	return undo_operation;
}

void
qof_undo_new_entry(gpointer cache, gpointer operation)
{
	qof_undo_operation *undo_operation;
 	qof_undo_entity *undo_entity;

	g_return_if_fail(operation || cache);
	undo_operation = (qof_undo_operation*)operation;
	undo_entity = (qof_undo_entity*)cache;
	g_return_if_fail(undo_operation || undo_entity);
	undo_operation->entity_list = g_list_prepend(undo_operation->entity_list, undo_entity);
}

void
undo_create_record (QofInstance *instance)
{
	qof_undo_entity *undo_entity;

	if(!instance) { return; }
	undo_entity = g_new0(qof_undo_entity, 1);
	// to undo a create, use a delete.
	undo_entity->how = UNDO_DELETE;
	undo_entity->guid = qof_instance_get_guid(instance);
	undo_entity->type = instance->entity.e_type;
	undo_cache = g_list_prepend(undo_cache, undo_entity);
}

static void
undo_get_entity (QofParam *param, gpointer data)
{
	QofInstance *instance;
	qof_undo_entity *undo_entity;

	instance = (QofInstance*)data;
	g_return_if_fail(instance || param);
	undo_entity = qof_prepare_undo(&instance->entity, param);
	undo_cache = g_list_prepend(undo_cache, undo_entity);
}

void
undo_delete_record (QofInstance *instance)
{
	qof_undo_entity *undo_entity;
	QofIdType type;

	if(!instance) { return; }
	// now need to store each parameter in a second entity, MODIFY.
	type = instance->entity.e_type;
	qof_class_param_foreach(type, undo_get_entity, instance);
	undo_entity = g_new0(qof_undo_entity, 1);
	// to undo a delete, use a create.
	undo_entity->how = UNDO_CREATE;
	undo_entity->guid = qof_instance_get_guid(instance);
	undo_entity->type = type;
	undo_cache = g_list_prepend(undo_cache, undo_entity);
}

void 
undo_edit_record (QofInstance *instance, QofParam *param)
{
	qof_undo_entity *undo_entity;

	if(!instance || !param) { return; }
	// handle if record is called without a commit.
	undo_entity = qof_prepare_undo(&instance->entity, param);
	// get book from the instance.
	undo_cache = g_list_prepend(undo_cache, undo_entity);
	// set the initial state that undo will reinstate.
	if(index_position == 0)
	{
		book_undo = g_list_prepend(book_undo, qof_undo_new_operation("initial"));
		index_position++;
	}
}

void 
undo_edit_commit (QofInstance *instance, QofParam *param)
{
	qof_undo_entity *undo_entity;

	if(!instance || !param) { return; }
	undo_entity = qof_prepare_undo(&instance->entity, param);
	undo_cache = g_list_prepend(undo_cache, undo_entity);
	// get book from the instance.
}

void 
qof_book_start_operation(QofBook *book, char *label)
{
	if(undo_operation_open && undo_cache) { 
		g_list_free(undo_cache);
		undo_operation_open = FALSE;
		if(undo_label) { g_free(undo_label); }
	}
	/** \todo handle the book parameter. */
	undo_label = g_strdup(label);
	undo_cache = NULL;
	undo_operation_open = TRUE;
}

void
qof_book_end_operation(QofBook *book)
{
	book_undo = g_list_prepend(book_undo, qof_undo_new_operation(undo_label));
	index_position++;
//	g_list_free(undo_cache);
	undo_operation_open = FALSE;
}

Timespec 
qof_book_undo_first_modified(QofBook *book)
{
	qof_undo_operation *undo_operation;

	undo_operation = (qof_undo_operation*)g_list_last(book_undo);
	return undo_operation->ts;
}

gint
qof_book_undo_count(QofBook *book)
{
	return g_list_length(book_undo);
}

/* ====================== END OF FILE ======================== */
