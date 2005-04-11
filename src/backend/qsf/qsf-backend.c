/***************************************************************************
 *            qsf-backend.c
 *
 *  Sat Jan  1 15:07:14 2005
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
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#define _GNU_SOURCE

#include "qsf-xml.h"
#include "qsf-dir.h"
#include <errno.h>

#define QSF_TYPE_BINARY "binary"
#define QSF_TYPE_GLIST "glist"
#define QSF_TYPE_FRAME "frame"

static short int module = MOD_BACKEND;

struct QSFBackend_s 
{
	QofBackend be;
	qsf_param *params;
	char *fullpath;
};

typedef struct QSFBackend_s QSFBackend;

void
qsf_param_init(qsf_param *params)
{
	Timespec *qsf_ts;
	gchar qsf_time_string[QSF_DATE_LENGTH];
	gchar qsf_enquiry_date[QSF_DATE_LENGTH];
	gchar qsf_time_match[QSF_DATE_LENGTH];
	gchar qsf_time_now[QSF_DATE_LENGTH];
	time_t qsf_time_now_t;
	gchar *qsf_time_precision;

	g_return_if_fail(params != NULL);
	params->count = 0;
	params->supported_types = NULL;
	params->file_type = QSF_UNDEF;
	params->qsf_ns = NULL;
	params->output_doc = NULL;
	params->output_node = NULL;
	params->lister = NULL;
	params->map_ns = NULL;
	params->qsf_object_list = NULL;
	params->qsf_parameter_hash = g_hash_table_new(g_str_hash, g_str_equal);
	params->qsf_default_hash = g_hash_table_new(g_str_hash, g_str_equal);
	params->qsf_define_hash = g_hash_table_new(g_str_hash, g_str_equal);
	params->qsf_calculate_hash = g_hash_table_new(g_str_hash, g_str_equal);
	params->referenceList = NULL;
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_STRING);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_GUID);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_BOOLEAN);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_NUMERIC);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_DATE);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_INT32);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_INT64);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_DOUBLE);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_CHAR);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_KVP);
	qsf_time_precision = "%j";
	qsf_time_now_t = time(NULL);
	qsf_ts = g_new(Timespec, 1);
	timespecFromTime_t(qsf_ts, qsf_time_now_t);
	strftime(qsf_enquiry_date, QSF_DATE_LENGTH, QSF_XSD_TIME, gmtime(&qsf_time_now_t));
	strftime(qsf_time_match, QSF_DATE_LENGTH, qsf_time_precision, gmtime(&qsf_time_now_t));
	strftime(qsf_time_string, QSF_DATE_LENGTH, "%F", gmtime(&qsf_time_now_t));
	strftime(qsf_time_now, QSF_DATE_LENGTH, QSF_XSD_TIME, gmtime(&qsf_time_now_t));
	g_hash_table_insert(params->qsf_default_hash, "qsf_enquiry_date", qsf_enquiry_date);
	g_hash_table_insert(params->qsf_default_hash, "qsf_time_now", &qsf_time_now_t);
	g_hash_table_insert(params->qsf_default_hash, "qsf_time_string", qsf_time_string);
}

/* GnuCash does LOTS of filesystem work, QSF is going to leave most of it to libxml2. :-)
Just strip the file: from the start of the book_path URL. Locks and file
creation are not implemented.
*/
static void
qsf_session_begin(QofBackend *be, QofSession *session, const char *book_path,
                   gboolean ignore_lock, gboolean create_if_nonexistent)
{
	QSFBackend *qsf_be;
	char *p, *path;
	
	g_return_if_fail(be != NULL);
	qsf_be = (QSFBackend*)be;
	g_return_if_fail(qsf_be->params != NULL);
	qsf_be->fullpath = NULL;
	if(book_path == NULL)
	{
		qof_backend_set_error(be, ERR_BACKEND_NO_ERR);
		return;
	}
	p = strchr (book_path, ':');
	if (p) {
		path = g_strdup (book_path);
		if (!g_strncasecmp(path, "file:", 5)) {
			p = g_new(char, strlen(path) - 5 + 1);
			strcpy(p, path + 5);
		}
		qsf_be->fullpath = g_strdup(p);
		g_free (path);
	}
	else {
		qsf_be->fullpath = g_strdup(book_path);
	}
	qof_backend_set_error(be, ERR_BACKEND_NO_ERR);
}

static void
qsf_session_end( QofBackend *be)
{
	QSFBackend *qsf_be;
	
	qsf_be = (QSFBackend*)be;
	g_return_if_fail(qsf_be != NULL);

	qsf_free_params(qsf_be->params);
	g_free(qsf_be->fullpath);
	qsf_be->fullpath = NULL;
	xmlCleanupParser();
}

void qsf_destroy_backend (QofBackend *be)
{
	g_free(be);
}

QofBackendError 
qof_session_load_our_qsf_object(QofSession *first_session, const char *path)
{
	QofSession *qsf_session;
	
	qsf_session = qof_session_new();
	qof_session_begin(qsf_session, path, FALSE, FALSE);
	qof_session_load(qsf_session, NULL);
	PINFO (" path=%s", path);
	/* FIXME: This needs to return success and set the open not merge error in file_open */
	return ERR_QSF_OPEN_NOT_MERGE;
}

QofBackendError 
qof_session_load_qsf_object(QofSession *first_session, const char *path)
{
	DEBUG (" ERR_QSF_NO_MAP");
	return ERR_QSF_NO_MAP;
}

void
qsf_file_type(QofBackend *be, QofBook *book)
{
	QSFBackend *qsf_be;
	qsf_param *params;
	char *path;
	gboolean result;

	g_return_if_fail(be != NULL);
	g_return_if_fail(book != NULL);
	qsf_be = (QSFBackend*) be;
	g_return_if_fail(qsf_be != NULL);
	g_return_if_fail(qsf_be->fullpath != NULL);
	g_return_if_fail(qsf_be->params != NULL);
	params = qsf_be->params;
	params->book = book;
	path = g_strdup(qsf_be->fullpath);
	params->filepath = g_strdup(path);
	qof_backend_get_error(be);
	result = is_our_qsf_object_be(params);
	if(result == TRUE) {
		params->file_type = OUR_QSF_OBJ;
		result = load_our_qsf_object(book, path, params);
		if(FALSE == result) {
			qof_backend_set_error(be, ERR_FILEIO_PARSE_ERROR);
		}
		return;
	} 
	else if(is_qsf_object_be(params)) {
		qsf_be->params->file_type = IS_QSF_OBJ;
		if(FALSE == load_qsf_object(book, qsf_be->params->filepath, qsf_be->params)) {
			qof_backend_set_error(be, ERR_FILEIO_PARSE_ERROR);
		}
	}
	else if(is_qsf_map_be(params)) {
		qsf_be->params->file_type = IS_QSF_MAP;
		qof_backend_set_error(be, ERR_QSF_MAP_NOT_OBJ);
	}
}

/*================================================
	Load QofEntity into QofBook from XML in memory
==================================================*/

static gboolean 
qsfdoc_to_qofbook(xmlDocPtr doc, qsf_param *params)
{
	QofInstance *inst;
	struct qsf_node_iterate iter;
	QofBook *book;
	GList *object_list;
	xmlNodePtr qsf_root;
	xmlNsPtr qsf_ns;

	g_return_val_if_fail(params != NULL, FALSE);
	g_return_val_if_fail(doc != NULL, FALSE);
	g_return_val_if_fail(params->book != NULL, FALSE);
	g_return_val_if_fail(params->file_type == OUR_QSF_OBJ, FALSE);

	qsf_root = xmlDocGetRootElement(doc);
	qsf_ns = qsf_root->ns;
	iter.ns = qsf_ns;
	book = params->book;
	params->referenceList = (GList*)qof_book_get_data(book, ENTITYREFERENCE);
	qsf_node_foreach(qsf_root, qsf_book_node_handler, &iter, params);
	object_list = g_list_copy(params->qsf_object_list);
	while(object_list != NULL)
	{
		params->object_set = object_list->data;
		params->qsf_parameter_hash = params->object_set->parameters;
		inst = (QofInstance*)qof_object_new_instance(params->object_set->object_type, book);
		g_return_val_if_fail(inst != NULL, FALSE);
		params->qsf_ent = &inst->entity;
		g_hash_table_foreach(params->qsf_parameter_hash, qsf_object_commitCB, params);
		object_list = g_list_next(object_list);
	}
	qof_book_set_data(book, ENTITYREFERENCE, params->referenceList);
	return TRUE;
}

static void
qsf_object_sequence(QofParam *qof_param, gpointer data)
{
	qsf_param *params;
	GSList *checklist, *result;

	g_return_if_fail(data != NULL);
	params = (qsf_param*) data;
	result = NULL;
	checklist = NULL;
	params->knowntype = FALSE;
	checklist = g_slist_copy(params->supported_types);
	for(result = checklist; result != NULL; result = result->next)
	{
		if(0 == safe_strcmp((QofIdType)result->data, qof_param->param_type))
		{
			params->knowntype = TRUE;
		}
	}
	g_slist_free(checklist);
	if(0 == safe_strcmp(qof_param->param_type, params->qof_type))
	{
		params->qsf_sequence = g_slist_append(params->qsf_sequence, qof_param);
		params->knowntype = TRUE;
	}
	/* handle params->qof_type = QOF_TYPE_GUID and qof_param->param_type != known type */
	if(0 == safe_strcmp(params->qof_type, QOF_TYPE_GUID)
		&& (params->knowntype == FALSE))
	{
		params->qsf_sequence = g_slist_append(params->qsf_sequence, qof_param);
		params->knowntype = TRUE;
	}
}	

/* receives each entry from supported_types in sequence
	type = qof data type from supported list
	user_data = params. Holds object type
*/
static void
qsf_supported_parameters(gpointer type, gpointer user_data)
{
	qsf_param *params;

	g_return_if_fail(user_data != NULL);
	params = (qsf_param*) user_data;
	params->qof_type = (QofIdType)type;
	params->knowntype = FALSE;
	qof_class_param_foreach(params->qof_obj_type, qsf_object_sequence, params);
}

static KvpValueType
qsf_to_kvp_helper(const char *type_string)
{
	if(0 == safe_strcmp(QOF_TYPE_STRING, type_string)) { return KVP_TYPE_STRING; }
	if(0 == safe_strcmp(QOF_TYPE_GUID, type_string)) { return KVP_TYPE_GUID; }
	if(0 == safe_strcmp(QOF_TYPE_INT64, type_string)) { return KVP_TYPE_GINT64; }
	if(0 == safe_strcmp(QOF_TYPE_DOUBLE, type_string)) { return KVP_TYPE_DOUBLE; }
	if(0 == safe_strcmp(QOF_TYPE_NUMERIC, type_string)) { return KVP_TYPE_NUMERIC; }
	if(0 == safe_strcmp(QSF_TYPE_BINARY, type_string))  { return KVP_TYPE_BINARY; }
	if(0 == safe_strcmp(QSF_TYPE_GLIST, type_string))   { return KVP_TYPE_GLIST; }
	if(0 == safe_strcmp(QSF_TYPE_FRAME, type_string))   { return KVP_TYPE_FRAME; }
	return 0;
}

static void
qsf_from_kvp_helper(const char *path, KvpValue *content, gpointer data)
{
	qsf_param *params;
	QofParam *qof_param;
	xmlNodePtr node;

	params = (qsf_param*)data;
	qof_param = params->qof_param;
	g_return_if_fail(params != NULL);
	switch(kvp_value_get_type(content))
	{
		case KVP_TYPE_STRING:
			node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns, qof_param->param_type));
			xmlNodeAddContent(node, kvp_value_to_bare_string(content));
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
			xmlNewProp(node, QSF_OBJECT_KVP, path);
			xmlNewProp(node, QSF_OBJECT_VALUE, QOF_TYPE_STRING);
			break;
		case KVP_TYPE_GUID:
			node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns, qof_param->param_type));
			xmlNodeAddContent(node, kvp_value_to_bare_string(content));
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
			xmlNewProp(node, QSF_OBJECT_KVP, path);
			xmlNewProp(node, QSF_OBJECT_VALUE, QOF_TYPE_GUID);
			break;
		case KVP_TYPE_BINARY:
			node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns, qof_param->param_type));
			xmlNodeAddContent(node, kvp_value_to_bare_string(content));
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
			xmlNewProp(node, QSF_OBJECT_KVP, path);
			xmlNewProp(node, QSF_OBJECT_VALUE, QSF_TYPE_BINARY);
			break;
		case KVP_TYPE_GLIST:
			node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns, qof_param->param_type));
			xmlNodeAddContent(node, kvp_value_to_bare_string(content));
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
			xmlNewProp(node, QSF_OBJECT_KVP, path);
			xmlNewProp(node, QSF_OBJECT_VALUE, QSF_TYPE_GLIST);
			break;
		case KVP_TYPE_FRAME:
			node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns, qof_param->param_type));
			xmlNodeAddContent(node, kvp_value_to_bare_string(content));
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
			xmlNewProp(node, QSF_OBJECT_KVP, path);
			xmlNewProp(node, QSF_OBJECT_VALUE, QSF_TYPE_FRAME);
			break;
		case KVP_TYPE_GINT64:
			node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns, qof_param->param_type));
			xmlNodeAddContent(node, kvp_value_to_bare_string(content));
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
			xmlNewProp(node, QSF_OBJECT_KVP, path);
			xmlNewProp(node, QSF_OBJECT_VALUE, QOF_TYPE_INT64);
			break;
		case KVP_TYPE_DOUBLE:
			node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns, qof_param->param_type));
			xmlNodeAddContent(node, kvp_value_to_bare_string(content));
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
			xmlNewProp(node, QSF_OBJECT_KVP, path);
			xmlNewProp(node, QSF_OBJECT_VALUE, QOF_TYPE_DOUBLE);
			break;
		case KVP_TYPE_NUMERIC:
			node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns, qof_param->param_type));
			xmlNodeAddContent(node, kvp_value_to_bare_string(content));
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
			xmlNewProp(node, QSF_OBJECT_KVP, path);
			xmlNewProp(node, QSF_OBJECT_VALUE, QOF_TYPE_NUMERIC);
			break;
		default:
		break;
	}
}

/******* reference handling ***********/

static gint
qof_reference_list_cb(gconstpointer a, gconstpointer b)
{
	const QofEntityReference *aa;
	const QofEntityReference *bb;

	aa = (QofEntityReference*) a;
	bb = (QofEntityReference*) b;
	if(aa == NULL) { return 1; }
	g_return_val_if_fail((bb != NULL), 1);
	g_return_val_if_fail((aa->type != NULL), 1);
	if((0 == guid_compare(bb->ent_guid, aa->ent_guid))
		&&(0 == safe_strcmp(bb->type, aa->type))
		&&(0 == safe_strcmp(bb->param->param_name, aa->param->param_name)))
	{
		return 0;
	}
	return 1;
}

static QofEntityReference*
qof_reference_lookup(GList *referenceList, QofEntityReference *find)
{
	GList *single_ref;
	QofEntityReference *ent_ref;

	if(referenceList == NULL) { return NULL; }
	g_return_val_if_fail(find != NULL, NULL);
	single_ref = NULL;
	ent_ref = NULL;
	single_ref = g_list_find_custom(referenceList, find, qof_reference_list_cb);
	if(single_ref == NULL) { return ent_ref; }
	ent_ref = (QofEntityReference*)single_ref->data;
	g_list_free(single_ref);
	return ent_ref;
}

static void
reference_list_lookup(gpointer data, gpointer user_data)
{
	QofEntity *ent;
	QofParam *ref_param;
	QofEntityReference *reference, *starter;
	qsf_param  *params;
	xmlNodePtr node, object_node;
	xmlNsPtr ns;
	GList *copy_list;
	gchar qsf_guid[GUID_ENCODING_LENGTH + 1], *ref_name;

	params = (qsf_param*)user_data;
	ref_param = (QofParam*)data;
	object_node = params->output_node;
	ent = params->qsf_ent;
	ns = params->qsf_ns;
	starter = g_new(QofEntityReference, 1);
	starter->ent_guid = qof_entity_get_guid(ent);
	starter->type = g_strdup(ent->e_type);
	starter->param = ref_param;
	starter->ref_guid = NULL;
	copy_list = g_list_copy(params->referenceList);
	reference = qof_reference_lookup(copy_list, starter);
	g_free(starter);
	if(reference != NULL) {
		if((ref_param->param_getfcn == NULL)||(ref_param->param_setfcn == NULL))
		{
			return;
		}
		ref_name = g_strdup(reference->param->param_name);
		node = xmlAddChild(object_node, xmlNewNode(ns, QOF_TYPE_GUID));
		guid_to_string_buff(reference->ref_guid, qsf_guid);
		xmlNodeAddContent(node, qsf_guid);
		xmlNewProp(node, QSF_OBJECT_TYPE ,ref_name);
		g_free(ref_name);
	}
}


/*=====================================
	Convert QofEntity to QSF XML node
qof_param holds the parameter sequence.
=======================================*/
static void
qsf_entity_foreach(QofEntity *ent, gpointer data)
{
	qsf_param  *params;
	GSList     *param_list, *supported;
	GList      *ref;
	xmlNodePtr node, object_node;
	xmlNsPtr   ns;
	gchar      *string_buffer;
	GString    *buffer;
	QofParam   *qof_param;
	KvpFrame   *qsf_kvp;
	int        param_count;
	gboolean   own_guid;
	const GUID *cm_guid;
	char       cm_sa[GUID_ENCODING_LENGTH + 1];

	g_return_if_fail(data != NULL);
	params = (qsf_param*)data;
	param_count = ++params->count;
	ns = params->qsf_ns;
	own_guid = FALSE;
	object_node = xmlNewChild(params->book_node, params->qsf_ns, QSF_OBJECT_TAG, NULL);
	xmlNewProp(object_node, QSF_OBJECT_TYPE, ent->e_type); 
	buffer = g_string_new(" ");
	g_string_printf(buffer, "%i", param_count);
	xmlNewProp(object_node, QSF_OBJECT_COUNT, buffer->str);
	param_list = g_slist_copy(params->qsf_sequence);
	while(param_list != NULL) {
		qof_param = param_list->data;
		g_return_if_fail(qof_param != NULL);
		if(0 == safe_strcmp(qof_param->param_type, QOF_TYPE_GUID))
		{
			if(!own_guid)
			{
				cm_guid = qof_entity_get_guid(ent);
				node = xmlAddChild(object_node, xmlNewNode(ns, QOF_TYPE_GUID));
				guid_to_string_buff(cm_guid, cm_sa);
				string_buffer = g_strdup(cm_sa);
				xmlNodeAddContent(node, string_buffer);
				xmlNewProp(node, QSF_OBJECT_TYPE , QOF_PARAM_GUID);
				own_guid = TRUE;
			}
			params->qsf_ent = ent;
			params->output_node = object_node;
			ref = qof_class_get_referenceList(ent->e_type);
			if(ref != NULL) {
				g_list_foreach(ref, reference_list_lookup, params);
			}
		}
		if(0 == safe_strcmp(qof_param->param_type, QOF_TYPE_KVP))
		{
			qsf_kvp = kvp_frame_copy(qof_param->param_getfcn(ent,qof_param));
			params->qof_param = qof_param;
			params->output_node = object_node;
			kvp_frame_for_each_slot(qsf_kvp, qsf_from_kvp_helper, params);
		}
		if((qof_param->param_setfcn != NULL) && (qof_param->param_getfcn != NULL))
		{
			supported = g_slist_copy(params->supported_types);
			for( supported = g_slist_copy(params->supported_types); 
				supported != NULL; supported = g_slist_next(supported))
			{
				if(0 == safe_strcmp((const char*)supported->data, (const char*)qof_param->param_type))
				{
			node = xmlAddChild(object_node, xmlNewNode(ns, qof_param->param_type));
			string_buffer = g_strdup(qof_book_merge_param_as_string(qof_param, ent));
			xmlNodeAddContent(node, string_buffer);
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
		}
			}
		}
		param_list = g_slist_next(param_list);
	}
}

static void
qsf_foreach_obj_type(QofObject *qsf_obj, gpointer data)
{
	qsf_param *params;
	QofBook *book;
	GSList *support;
	
	g_return_if_fail(data != NULL);
	params = (qsf_param*) data;
	/* Skip unsupported objects */
	if((qsf_obj->create == NULL)||(qsf_obj->foreach == NULL)){
		DEBUG (" qsf_obj QOF support failed %s", qsf_obj->e_type);
		return;
	}
	params->qof_obj_type = qsf_obj->e_type;
	params->qsf_sequence = NULL;
	book = params->book;
	support = g_slist_copy(params->supported_types);
	g_slist_foreach(support,qsf_supported_parameters, params);
	qof_object_foreach(qsf_obj->e_type, book, qsf_entity_foreach, params);
}

/*=====================================================
	Take a QofBook and prepare a QSF XML doc in memory
=======================================================*/
/*	QSF only uses one QofBook per file - count may be removed later. */
static xmlDocPtr
qofbook_to_qsf(QofBook *book)
{
	xmlNodePtr top_node, node;
	xmlDocPtr doc;
	gchar buffer[GUID_ENCODING_LENGTH + 1];
	qsf_param *params;
	const GUID *book_guid;
	
	g_return_val_if_fail(book != NULL, NULL);
	params = g_new(qsf_param, 1);
	qsf_param_init(params);
	params->book = book;
	params->referenceList = g_list_copy((GList*)qof_book_get_data(book, ENTITYREFERENCE));
	doc = xmlNewDoc(QSF_XML_VERSION);
	top_node = xmlNewNode(NULL, QSF_ROOT_TAG);
	xmlDocSetRootElement(doc, top_node);
	xmlSetNs(top_node, xmlNewNs(top_node, QSF_DEFAULT_NS, NULL));
	params->qsf_ns = top_node->ns;
	node = xmlNewChild(top_node, params->qsf_ns, QSF_BOOK_TAG, NULL);
	params->book_node = node;
	xmlNewProp(node, QSF_BOOK_COUNT, "1");
	book_guid = qof_book_get_guid(book);
	guid_to_string_buff(book_guid, buffer);
	xmlNewChild(params->book_node, params->qsf_ns, QSF_BOOK_GUID, buffer);
	params->output_doc = doc;
	params->book_node = node;
	qof_object_foreach_type(qsf_foreach_obj_type, params);
	return params->output_doc;
}

static gboolean
write_qsf_from_book(FILE *out, QofBook *book)
{
	xmlDocPtr qsf_doc;
	gint write_result;
	QofBackend *be;
	
	qsf_doc = qofbook_to_qsf(book);
	write_result = 0;
	g_return_val_if_fail(qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, qsf_doc) == TRUE, FALSE);
	write_result = xmlDocFormatDump(out, qsf_doc, 1);
	if(write_result < 0) {
		be = qof_book_get_backend(book);
		qof_backend_set_error(be, ERR_FILEIO_WRITE_ERROR);
		return FALSE;
	}
	fprintf(out, "\n");
	xmlFreeDoc(qsf_doc);
	return TRUE;
}

static void
write_qsf_to_stdout(QofBook *book)
{
	xmlDocPtr qsf_doc;
	
	qsf_doc = qofbook_to_qsf(book);
	g_return_if_fail(qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, qsf_doc) == TRUE);
	xmlDocFormatDump(stdout, qsf_doc, 1);
	fprintf(stdout, "\n");
	xmlFreeDoc(qsf_doc);
}

void
qsf_write_file(QofBackend *be, QofBook *book)
{
	QSFBackend *qsf_be;
	FILE *out;
	char *path;
	gboolean result;
	
	qsf_be = (QSFBackend*)be;
	/* if fullpath is blank, book_id was set to QOF_STDOUT */
	if(0 == safe_strcmp(qsf_be->fullpath, "")) {
		write_qsf_to_stdout(book);
		return;
	}
	path = strdup(qsf_be->fullpath);
	out = fopen(path, "w");
	result = write_qsf_from_book(out, book);
	if(result == FALSE) { 
		g_free(path);
		return; 
	}
	g_free(path);
	fclose(out);
}

/* QofBackend routine to load from file - needs a map.
*/
gboolean
load_qsf_object(QofBook *book, const char *fullpath, qsf_param *params)
{
	xmlNodePtr qsf_root;

	params->input_doc = xmlParseFile(fullpath);
	if (params->input_doc == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
		return FALSE;
	}
	qsf_root = NULL;
	qsf_root = xmlDocGetRootElement(params->input_doc);
	params->qsf_ns = qsf_root->ns;
	params->book = book;
	/* Create a QofBook from the QSF document <b>using a QSF map</b>.
	
	May seem strange, but I think we can do this by using the map handlers to
	create the output_doc in memory as OUR_QSF_OBJ, then pass to the same routine!
	*/

	return FALSE;
}

gboolean
load_our_qsf_object(QofBook *book, const char *fullpath, qsf_param *params)
{
	xmlNodePtr qsf_root;
	
	params->input_doc = xmlParseFile(fullpath);
	if (params->input_doc == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
		return FALSE;
	}
	qsf_root = NULL;
	qsf_root = xmlDocGetRootElement(params->input_doc);
	params->qsf_ns = qsf_root->ns;
	return qsfdoc_to_qofbook(params->input_doc, params);
}

KvpValue*
string_to_kvp_value(const char *content, KvpValueType type)
{
	char        *tail;
	gint64 	    cm_i64;
	double      cm_double;
	gnc_numeric cm_numeric;
	GUID        *cm_guid;
	struct tm   kvp_time;
	time_t	    kvp_time_t;
	Timespec    cm_date;
	
	switch(type) {
	  case KVP_TYPE_GINT64:
		errno = 0;
		cm_i64 = strtoll(content, &tail, 0);
		if(errno == 0) {
			return kvp_value_new_gint64(cm_i64);
		}
		break;
	  case KVP_TYPE_DOUBLE:
  		errno = 0;
		cm_double = strtod(content, &tail);
		if(errno == 0) {
			return kvp_value_new_double(cm_double);
		}
		break;
	  case KVP_TYPE_NUMERIC:
		string_to_gnc_numeric(content, &cm_numeric);
		return kvp_value_new_gnc_numeric(cm_numeric);
		break;
	  case KVP_TYPE_STRING:
		return kvp_value_new_string(content);
		break;
	  case KVP_TYPE_GUID:
		cm_guid = g_new(GUID, 1);
		if(TRUE == string_to_guid(content, cm_guid))
		{
			return kvp_value_new_guid(cm_guid);
		}
		break;
	  case KVP_TYPE_TIMESPEC:
		strptime(content, QSF_XSD_TIME, &kvp_time);
		kvp_time_t = mktime(&kvp_time);
		timespecFromTime_t(&cm_date, kvp_time_t);
		return kvp_value_new_timespec(cm_date);
		break;
	  case KVP_TYPE_BINARY:
//		return kvp_value_new_binary(value->value.binary.data,
//									value->value.binary.datasize);
		break;
	  case KVP_TYPE_GLIST:
//		return kvp_value_new_glist(value->value.list);
		break;
	  case KVP_TYPE_FRAME:
//		return kvp_value_new_frame(value->value.frame);
		break;
	}
	return NULL;
}

/*======================================================
	Commit XML data from file to QofEntity in a QofBook
========================================================*/
void
qsf_object_commitCB(gpointer key, gpointer value, gpointer data)
{
	qsf_param          *params;
	qsf_objects        *object_set;
	xmlNodePtr         node;
	QofEntityReference *reference;
	QofEntity          *qsf_ent;
	QofBook            *targetBook;
	const char         *qof_type, *parameter_name;
	QofIdType          obj_type, reference_type;
	struct tm          qsf_time;
	time_t             qsf_time_t;
	char               *tail;
	/* cm_ prefix used for variables that hold the data to commit */
	gnc_numeric    cm_numeric;
	double         cm_double;
	gboolean       cm_boolean;
	gint32         cm_i32;
	gint64         cm_i64;
	Timespec       cm_date;
	char           cm_char,    (*char_getter)  (xmlNodePtr);
	GUID           *cm_guid;
	KvpFrame       *cm_kvp;
	KvpValue       *cm_value;
	KvpValueType   cm_type;
	QofSetterFunc  cm_setter;
	const QofParam *cm_param;
	void (*string_setter)    (QofEntity*, const char*);
	void (*date_setter)      (QofEntity*, Timespec);
	void (*numeric_setter)   (QofEntity*, gnc_numeric);
	void (*double_setter)    (QofEntity*, double);
	void (*boolean_setter)   (QofEntity*, gboolean);
	void (*i32_setter)       (QofEntity*, gint32);
	void (*i64_setter)       (QofEntity*, gint64);
	void (*char_setter)      (QofEntity*, char);
	void (*kvp_frame_setter) (QofEntity*, KvpFrame*);

	g_return_if_fail(data != NULL);
	g_return_if_fail(value != NULL);
	params = (qsf_param*)data;
	node = (xmlNodePtr)value;
	parameter_name = (const char*)key;
	qof_type = node->name;
	qsf_ent = params->qsf_ent;
	targetBook = params->book;
	obj_type = xmlGetProp(node->parent, QSF_OBJECT_TYPE);
	if(0 == safe_strcasecmp(obj_type, parameter_name)) { return; }
	cm_setter = qof_class_get_parameter_setter(obj_type, parameter_name);
	cm_param = qof_class_get_parameter(obj_type, parameter_name);
	object_set = params->object_set;
	if(safe_strcmp(qof_type, QOF_TYPE_STRING) == 0)  { 
		string_setter = (void(*)(QofEntity*, const char*))cm_setter;
		if(string_setter != NULL) { string_setter(qsf_ent, xmlNodeGetContent(node)); }
	}
	if(safe_strcmp(qof_type, QOF_TYPE_DATE) == 0) { 
		date_setter = (void(*)(QofEntity*, Timespec))cm_setter;
		strptime(xmlNodeGetContent(node), QSF_XSD_TIME, &qsf_time);
		qsf_time_t = mktime(&qsf_time);
		timespecFromTime_t(&cm_date, qsf_time_t);
		if(date_setter != NULL) { date_setter(qsf_ent, cm_date); }
	}
	if((safe_strcmp(qof_type, QOF_TYPE_NUMERIC) == 0)  ||
	(safe_strcmp(qof_type, QOF_TYPE_DEBCRED) == 0)) { 
		numeric_setter = (void(*)(QofEntity*, gnc_numeric))cm_setter;
		string_to_gnc_numeric(xmlNodeGetContent(node), &cm_numeric);
		if(numeric_setter != NULL) { numeric_setter(qsf_ent, cm_numeric); }
	}
	if(safe_strcmp(qof_type, QOF_TYPE_GUID) == 0) { 
		cm_guid = g_new(GUID, 1);
		if(TRUE != string_to_guid(xmlNodeGetContent(node), cm_guid))
		{
			qof_backend_set_error(params->be, ERR_QSF_BAD_OBJ_GUID);
			LEAVE (" string to guid failed for %s", xmlNodeGetContent(node));
			return;
		}
		reference_type = xmlGetProp(node, QSF_OBJECT_TYPE);
		if(0 == safe_strcmp(QOF_PARAM_GUID, reference_type)) 
		{
			qof_entity_set_guid(qsf_ent, cm_guid);
		}
		else {
			reference = qof_entity_get_reference_from(qsf_ent, cm_param);
			if(reference) {
				params->referenceList = g_list_append(params->referenceList, reference);
			}
		}
	}
	if(safe_strcmp(qof_type, QOF_TYPE_INT32) == 0) { 
		errno = 0;
		cm_i32 = (gint32)strtol (xmlNodeGetContent(node), &tail, 0);
		if(errno == 0) {
			i32_setter = (void(*)(QofEntity*, gint32))cm_setter;
			if(i32_setter != NULL) { i32_setter(qsf_ent, cm_i32); }
		}
		else { qof_backend_set_error(params->be, ERR_QSF_OVERFLOW); }
	}
	if(safe_strcmp(qof_type, QOF_TYPE_INT64) == 0) { 
		errno = 0;
		cm_i64 = strtoll(xmlNodeGetContent(node), &tail, 0);
		if(errno == 0) {
			i64_setter = (void(*)(QofEntity*, gint64))cm_setter;
			if(i64_setter != NULL) { i64_setter(qsf_ent, cm_i64); }
		}
		else { qof_backend_set_error(params->be, ERR_QSF_OVERFLOW); }
	}
	if(safe_strcmp(qof_type, QOF_TYPE_DOUBLE) == 0) { 
		errno = 0;
		cm_double = strtod(xmlNodeGetContent(node), &tail);
		if(errno == 0) {
			double_setter = (void(*)(QofEntity*, double))cm_setter;
			if(double_setter != NULL) { double_setter(qsf_ent, cm_double); }
		}
	}
	if(safe_strcmp(qof_type, QOF_TYPE_BOOLEAN) == 0){ 
		if(0 == safe_strcasecmp(xmlNodeGetContent(node), QSF_XML_BOOLEAN_TEST)) {
			cm_boolean = TRUE;
		}
		else { cm_boolean = FALSE; }
		boolean_setter = (void(*)(QofEntity*, gboolean))cm_setter;
		if(boolean_setter != NULL) { boolean_setter(qsf_ent, cm_boolean); }
	}
		if(safe_strcmp(qof_type, QOF_TYPE_KVP) == 0) { 
			cm_type = qsf_to_kvp_helper(xmlGetProp(node, QSF_OBJECT_VALUE));
			if(!cm_type) { return; }
			cm_value = string_to_kvp_value(xmlNodeGetContent(node), cm_type);
			cm_kvp = kvp_frame_copy(cm_param->param_getfcn(qsf_ent, cm_param));
			cm_kvp = kvp_frame_set_value(cm_kvp, xmlGetProp(node, QSF_OBJECT_KVP), cm_value);
			kvp_frame_setter = (void(*)(QofEntity*, KvpFrame*))cm_setter;
			if(kvp_frame_setter != NULL) { kvp_frame_setter(qsf_ent, cm_kvp); }
		}
	if(safe_strcmp(qof_type, QOF_TYPE_CHAR) == 0) { 
		char_getter = (char (*)(xmlNodePtr))xmlNodeGetContent;
		cm_char = char_getter(node);
		char_setter = (void(*)(QofEntity*, char))cm_setter;
		if(char_setter != NULL) { char_setter(qsf_ent, cm_char); }
	}
}

QofBackend*
qsf_backend_new(void)
{
	QSFBackend *qsf_be;
	QofBackend *be;
	
	qsf_be = g_new0(QSFBackend, 1);
	be = (QofBackend*) qsf_be;
	qof_backend_init(be);
	qsf_be->params = g_new(qsf_param, 1);
	qsf_be->params->be = be;
	qsf_param_init(qsf_be->params);
	qsf_be->be.session_begin = qsf_session_begin;

	be->session_end = qsf_session_end;
	be->destroy_backend = qsf_destroy_backend;
	be->load = qsf_file_type;
	be->save_may_clobber_data = NULL;
	/* The QSF backend will always load and save the entire QSF XML file. */
	be->begin = NULL;
	be->commit = NULL;
	be->rollback = NULL;
	/* QSF uses the built-in SQL, not a dedicated SQL server. */
	be->compile_query = NULL;
	be->free_query = NULL;
	be->run_query = NULL;
	be->counter = NULL;
	/* The QSF backend is not multi-user. */
	be->events_pending = NULL;
	be->process_events = NULL;
	
	be->sync = qsf_write_file;
	qsf_be->fullpath = NULL;
	return be;
}

/** \brief The QOF method of loading each backend.

QSF does not use a GnuCash module, it is loaded using the QOF
method - QofBackendProvider.
*/
static void 
qsf_provider_free (QofBackendProvider *prov)
{
	prov->provider_name = NULL;
	prov->access_method = NULL;
	g_free (prov);
}

void
qsf_provider_init(void)
{
	QofBackendProvider *prov;
	prov = g_new0 (QofBackendProvider, 1);
	prov->provider_name = "QSF Backend Version 0.1";
	prov->access_method = "file";
	prov->partial_book_supported = TRUE;
	prov->backend_new = qsf_backend_new;
	prov->provider_free = qsf_provider_free;
	qof_backend_register_provider (prov);
}
