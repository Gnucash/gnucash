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
 /** @addtogroup Backend
    @{ */
/** @addtogroup QSF QOF Serialisation Format

	The qsf-backend.c source file is included in this documentation during development
	to help explain certain methods. The doxygen tags can be
	removed / set as ordinary comments once each method is finalised.

    @{ */
/** @file qsf-backend.c
    @brief  QSF Backend definition, creation and handling.
    @author Copyright (C) 2004-2005 Neil Williams <linux@codehelp.co.uk>
*/

#define _GNU_SOURCE

#include "qsf-xml.h"
#include "qsf-dir.h"

//static short module = MOD_BACKEND;

/** \brief QSF wrapper for QofBackend

This makes a qsf_param struct available to the backend.
*/
struct QSFBackend_s 
{
	QofBackend be;
	qsf_param *params;
	char *fullpath;
};

typedef struct QSFBackend_s QSFBackend;

/** \brief Backend init routine.

*/
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
	params->count = 1;
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
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_STRING);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_GUID);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_BOOLEAN);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_NUMERIC);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_DATE);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_INT32);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_INT64);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_DOUBLE);
	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_CHAR);
/*	params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_KVP);*/
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

/** \brief GnuCash does LOTS of filesystem work, QSF is going to leave most of it to libxml2. :-)

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
	g_return_if_fail(book_path != NULL);
	
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

/** \brief Clean up the backend and the libxml2 parser. */
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

void
qsf_write_file(QofBackend *be, QofBook *book)
{
	QSFBackend *qsf_be;
	FILE *out;
	char *path;
	
	qsf_be = (QSFBackend*)be;
	path = strdup(qsf_be->fullpath);
	out = fopen(path, "w");
	write_qsf_from_book(out, book);
	fclose(out);
}

QofBackendError qof_session_load_our_qsf_object(QofSession *first_session, const char *path)
{
	QofSession *qsf_session;
	
	first_session = qof_session_get_current_session();
	qsf_session = qof_session_new();
	qof_session_begin(qsf_session, path, FALSE, FALSE);
	qof_session_load(qsf_session, NULL);
	return qof_session_get_error(qsf_session);
}

QofBackendError qof_session_load_qsf_object(QofSession *first_session, const char *path)
{
	return ERR_QSF_NO_MAP;
}

/** \brief Types of QSF file:

- is_our_qsf_object, OUR_QSF_OBJ, QSF object file using only QOF objects known to the calling process.
	No map is required.
- is_qsf_object, IS_QSF_OBJ, QSF object file that may or may not have a QSF map
	to convert external objects. This temporary type will be set to HAVE_QSF_MAP if a suitable
	map exists, or an error value returned: ERR_QSF_NO_MAP, ERR_QSF_BAD_MAP or ERR_QSF_WRONG_MAP
	This allows the calling process to inform the user that the QSF itself is valid but a
	suitable map cannot be found.
- is_qsf_map, IS_QSF_MAP, QSF map file. In the backend, this generates ERR_QSF_MAP_NOT_OBJ but
	it can be used internally when processing maps to match a QSF object.
*/
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
	/* remove any previous error from the backend stack.*/
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

/** \todo Create a QofBook from \b our QSF document.

Method:

input_doc must be of file_type OUR_QSF_OBJ.

Iterate over the existing params->qsf_parameter_hash hashtable
qsf_node_foreach(qsf_root, qsf_book_node_handler, &iter, params);

1. object handler -> create new QofEntity 
2. parameter handler -> param_setfcn	
*/
static gboolean qsfdoc_to_qofbook(xmlDocPtr doc, qsf_param *params)
{
	QofInstance *inst;
	struct qsf_node_iterate iter;
	GList *object_list;
	xmlNodePtr qsf_root;
	xmlNsPtr qsf_ns;

	g_return_val_if_fail(params != NULL, FALSE);
	g_return_val_if_fail(doc != NULL, FALSE);
	/* qsf_param_init must have been run and the doc correctly validated. */
	g_return_val_if_fail(params->book != NULL, FALSE);
	g_return_val_if_fail(params->file_type == OUR_QSF_OBJ, FALSE);

	qsf_root = xmlDocGetRootElement(doc);
	qsf_ns = qsf_root->ns;
	iter.ns = qsf_ns;
	/* qsf_book_node_handler calls the object and parameter handlers recursively */
	qsf_node_foreach(qsf_root, qsf_book_node_handler, &iter, params);
	/* All the QSF data (one object at a time) is in qsf_parameter_hash */
	object_list = g_list_copy(params->qsf_object_list);
	while(object_list != NULL)
	{
		params->object_set = object_list->data;
		params->qsf_parameter_hash = params->object_set->parameters;
		inst = (QofInstance*)qof_object_new_instance(params->object_set->object_type, params->book);
		g_return_val_if_fail(inst != NULL, FALSE);
		params->qsf_ent = &inst->entity;
		g_hash_table_foreach(params->qsf_parameter_hash, qsf_object_commitCB, params);
		object_list = g_list_next(object_list);
	}
	return TRUE;
}

/** \brief Writes a QofBook to an open filehandle.

@param	out Pointer to a FILE filehandle.
@param	book Pointer to a QofBook to write as QSF XML.

Any QofBook can be written to QSF XML, it does not need an
AccountGroup or any specific QOF object. Any process that can
create a new QofSession and populate the QofBook with QOF objects
can write the data as QSF XML.
*/
void
write_qsf_from_book(FILE *out, QofBook *book)
{
	xmlDocPtr qsf_doc;
	
	qsf_doc = qofbook_to_qsf(book);
	g_return_if_fail(qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, qsf_doc) == TRUE);
	xmlDocFormatDump(out, qsf_doc, 1);
	fprintf(out, "\n");
	xmlFreeDoc(qsf_doc);
}

/** \brief QofBackend routine to load from file - needs a map.
*/
gboolean
load_qsf_object(QofBook *book, const char *fullpath, qsf_param *params)
{
	xmlNodePtr qsf_root;

	/* is_qsf_object has already been run, doc should not fail to parse. */
	params->input_doc = xmlParseFile(fullpath);
	if (params->input_doc == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
		return FALSE;
	}
	qsf_root = NULL;
	qsf_root = xmlDocGetRootElement(params->input_doc);
	params->qsf_ns = qsf_root->ns;
	params->book = book;
	/** \todo Create a QofBook from the QSF document <b>using a QSF map</b>.
	
	May seem strange, but I think we can do this by using the map handlers to
	create the output_doc in memory as OUR_QSF_OBJ, then pass to the same routine!
	*/

	return FALSE;
}

/** \brief Load a QSF XML file without a map.

The QSF XML file must ONLY contain QOF objects already known to the host
application. i.e. which return TRUE in qof_class_is_registered();
*/
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

	/** \todo Add correctly typed functions for each remaining component 
	of the QofBackend for QSF */
	be->session_end = qsf_session_end;
	be->destroy_backend = qsf_destroy_backend;
	be->load = qsf_file_type;
	be->save_may_clobber_data = NULL; /* gboolean qof_session_save_may_clobber_data (QofSession * session)*/
	be->begin = NULL; /*   void (*begin) (QofBackend *, QofInstance *); */
	be->commit = NULL; /*   void (*commit) (QofBackend *, QofInstance *); */
	be->rollback = NULL; /* void (*rollback) (QofBackend *, QofInstance *); */
	/* The QSF backend doesn't always load all data ... change these! */
	be->compile_query = NULL;
	be->free_query = NULL;
	be->run_query = NULL;
	be->counter = NULL;
	
	/* Is the QSF backend to be multi-user?? */
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

/** \brief Describe this backend to the application. */
void
qsf_provider_init(void)
{
	QofBackendProvider *prov;
	prov = g_new0 (QofBackendProvider, 1);
	prov->provider_name = "QSF Backend Version 0.1";
	prov->access_method = "file";
	prov->backend_new = qsf_backend_new;
	prov->provider_free = qsf_provider_free;
	qof_backend_register_provider (prov);
}


/** @} */
/** @} */
