/***************************************************************************
 *            qsf-xml.c
 *
 *  Fri Nov 26 19:29:47 2004
 *  Copyright  2004,2005,2006  Neil Williams  <linux@codehelp.co.uk>
 *
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "config.h"
#include <glib.h>
#include <libxml/xmlversion.h>
#include <libxml/xmlmemory.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlschemas.h>
#include "qof.h"
#include "qof-backend-qsf.h"
#include "qsf-dir.h"
#include "qsf-xml.h"

static QofLogModule log_module = QOF_MOD_QSF;

gint
qsf_compare_tag_strings(const xmlChar *node_name, gchar *tag_name)
{
	return xmlStrcmp(node_name, (const xmlChar *)tag_name);
}

gint
qsf_strings_equal(const xmlChar *node_name, gchar *tag_name)
{
	if(0 == qsf_compare_tag_strings(node_name, tag_name)) { return 1; }
	return 0;
}

gint
qsf_is_element(xmlNodePtr a, xmlNsPtr ns, gchar *c)
{
	g_return_val_if_fail(a != NULL, 0);
	g_return_val_if_fail(ns != NULL, 0);
	g_return_val_if_fail(c != NULL, 0);
	if ((a->ns == ns) && (a->type == XML_ELEMENT_NODE) &&
		qsf_strings_equal(a->name, c)) { return 1; }
	return 0;
}

gint
qsf_check_tag(qsf_param *params, gchar *qof_type)
{
	return qsf_is_element(params->child_node, params->qsf_ns, qof_type);
}

gboolean
qsf_is_valid(const gchar *schema_dir, const gchar* schema_filename, xmlDocPtr doc)
{
	xmlSchemaParserCtxtPtr qsf_schema_file;
	xmlSchemaPtr qsf_schema;
	xmlSchemaValidCtxtPtr qsf_context;
	gchar *schema_path;
	gint result;

	g_return_val_if_fail(doc || schema_filename, FALSE);
	schema_path = g_strdup_printf("%s/%s", schema_dir, schema_filename);
	qsf_schema_file = xmlSchemaNewParserCtxt(schema_path);
	qsf_schema = xmlSchemaParse(qsf_schema_file);
	qsf_context = xmlSchemaNewValidCtxt(qsf_schema);
	result = xmlSchemaValidateDoc(qsf_context, doc);
	xmlSchemaFreeParserCtxt(qsf_schema_file);
	xmlSchemaFreeValidCtxt(qsf_context);
	xmlSchemaFree(qsf_schema);
	g_free(schema_path);
	if(result == 0) { return TRUE; }
	return FALSE;
}

void
qsf_valid_foreach(xmlNodePtr parent, qsf_validCB cb,
	struct qsf_node_iterate *iter, qsf_validator *valid)
{
	xmlNodePtr cur_node;

	iter->v_fcn = &cb;
	for(cur_node = parent->children; cur_node != NULL; cur_node = cur_node->next)
	{
		cb(cur_node, iter->ns, valid);
	}
}

void
qsf_node_foreach(xmlNodePtr parent, qsf_nodeCB cb,
	struct qsf_node_iterate *iter, qsf_param *params)
{
	xmlNodePtr cur_node;

	g_return_if_fail(iter->ns);
	iter->fcn = &cb;
	for(cur_node = parent->children; cur_node != NULL; cur_node = cur_node->next)
	{
		cb(cur_node, iter->ns, params);
	}
}

void
qsf_object_validation_handler(xmlNodePtr child, xmlNsPtr ns, qsf_validator *valid)
{
	xmlNodePtr cur_node;
	xmlChar *object_declaration;
	guint count;

	count = 0;
	for(cur_node = child->children; cur_node != NULL;
		cur_node = cur_node->next)
	{
		if(qsf_is_element(cur_node, ns, QSF_OBJECT_TAG)) {
			object_declaration = xmlGetProp(cur_node, BAD_CAST QSF_OBJECT_TYPE);
			count = g_hash_table_size(valid->validation_table);
			g_hash_table_insert(valid->validation_table, object_declaration, xmlNodeGetContent(cur_node));
			if(g_hash_table_size(valid->validation_table) > count)
			{
				valid->valid_object_count++;
				if(TRUE == qof_class_is_registered((QofIdTypeConst) object_declaration))
				{
					valid->qof_registered_count++;
				}
			}
		}
	}
}

gboolean is_our_qsf_object(const gchar *path)
{
	xmlDocPtr doc;
	struct qsf_node_iterate iter;
	xmlNodePtr object_root;
	qsf_validator valid;
	gint table_count;

	g_return_val_if_fail((path != NULL),FALSE);
	doc = xmlParseFile(path);
	if(doc == NULL)  { return FALSE; }
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, doc)) { 
		PINFO (" validation failed %s %s %s", QSF_SCHEMA_DIR, 
			QSF_OBJECT_SCHEMA, path);
		xmlFreeDoc(doc);
		return FALSE; 
	}
	object_root = xmlDocGetRootElement(doc);
	/* check that all objects in the file are already registered in QOF */
	valid.validation_table = g_hash_table_new(g_str_hash, g_str_equal);
	valid.qof_registered_count = 0;
	valid.valid_object_count = 0;
	iter.ns = object_root->ns;
	qsf_valid_foreach(object_root, qsf_object_validation_handler, &iter, &valid);
	table_count = g_hash_table_size(valid.validation_table);
	g_hash_table_destroy(valid.validation_table);
	if(table_count == valid.qof_registered_count) { return TRUE; }
	return FALSE;
}

gboolean is_qsf_object(const gchar *path)
{
	xmlDocPtr doc;

	g_return_val_if_fail((path != NULL),FALSE);
	if(path == NULL) { return FALSE; }
	doc = xmlParseFile(path);
	if(doc == NULL) { return FALSE; }
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, doc)) {
		xmlFreeDoc(doc);
	    return FALSE;
	}
	xmlFreeDoc(doc);
	/* Note cannot test against a map here, so if the file is valid QSF,
	accept it and work out the details later. */
	return TRUE;
}

gboolean is_our_qsf_object_be(qsf_param *params)
{
	xmlDocPtr doc;
	struct qsf_node_iterate iter;
	xmlNodePtr object_root;
	qsf_validator valid;
	gint table_count;
	gchar *path;

	g_return_val_if_fail((params != NULL),FALSE);
	path = g_strdup(params->filepath);
	if(path == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_FILE_NOT_FOUND);
		return FALSE;
	}
	if(params->file_type != QSF_UNDEF) { return FALSE; }
	doc = xmlParseFile(path);
	if(doc == NULL)  {
		qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
		return FALSE;
	}
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, doc))
	{
		qof_backend_set_error(params->be, ERR_QSF_INVALID_OBJ);
		return FALSE;
	}
	params->file_type = IS_QSF_OBJ;
	object_root = xmlDocGetRootElement(doc);
	valid.validation_table = g_hash_table_new(g_str_hash, g_str_equal);
	valid.qof_registered_count = 0;
	iter.ns = object_root->ns;
	qsf_valid_foreach(object_root, qsf_object_validation_handler, &iter, &valid);
	table_count = g_hash_table_size(valid.validation_table);
	if(table_count == valid.qof_registered_count)
	{
		g_hash_table_destroy(valid.validation_table);
		qof_backend_set_error(params->be, ERR_BACKEND_NO_ERR);
		return TRUE;
	}
	g_hash_table_destroy(valid.validation_table);
	qof_backend_set_error(params->be, ERR_QSF_NO_MAP);
	return FALSE;
}

gboolean is_qsf_object_be(qsf_param *params)
{
	gboolean result;
	xmlDocPtr doc;
	GList *maps;
	gchar *path;

	g_return_val_if_fail((params != NULL),FALSE);
	path = g_strdup(params->filepath);
	if(path == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_FILE_NOT_FOUND);
		return FALSE;
	}
	/* skip validation if is_our_qsf_object has already been called. */
	if(ERR_QSF_INVALID_OBJ == qof_backend_get_error(params->be)) { return FALSE; }
	if(params->file_type == QSF_UNDEF)
	{
		doc = xmlParseFile(path);
		if(doc == NULL) {
			qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
			return FALSE;
		}
		if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, doc))
		{
			qof_backend_set_error(params->be, ERR_QSF_INVALID_OBJ);
			return FALSE;
		}
	}
	result = FALSE;
	/* retrieve list of maps from config frame. */
	for(maps = params->map_files; maps; maps=maps->next)
	{
        QofBackendError err;
		result = is_qsf_object_with_map_be(maps->data, params);
        err = qof_backend_get_error(params->be);
        if((err == ERR_BACKEND_NO_ERR) && result) 
        {
            params->map_path = maps->data;
            PINFO ("map chosen = %s", params->map_path);
            break;
        }
        /* pop the error back on the stack. */
        else { qof_backend_set_error(params->be, err); }
	}
	return result;
}

static void
qsf_supported_data_types(gpointer type, gpointer user_data)
{
	qsf_param *params;

	g_return_if_fail(user_data != NULL);
	g_return_if_fail(type != NULL);
	params = (qsf_param*) user_data;
	if(qsf_is_element(params->param_node, params->qsf_ns, (gchar*)type))
	{
		g_hash_table_insert(params->qsf_parameter_hash,
			xmlGetProp(params->param_node, BAD_CAST QSF_OBJECT_TYPE), params->param_node);
	}
}

static void
qsf_parameter_handler(xmlNodePtr child, xmlNsPtr qsf_ns, qsf_param *params)
{
	params->param_node = child;
	g_slist_foreach(params->supported_types, qsf_supported_data_types, params);
}

void
qsf_object_node_handler(xmlNodePtr child, xmlNsPtr qsf_ns, qsf_param *params)
{
	struct qsf_node_iterate iter;
	qsf_objects *object_set;
	gchar *tail, *object_count_s;
	gint64 c;

	g_return_if_fail(child != NULL);
	g_return_if_fail(qsf_ns != NULL);
	params->qsf_ns = qsf_ns;
	if(qsf_is_element(child, qsf_ns, QSF_OBJECT_TAG)) {
		params->qsf_parameter_hash = NULL;
		c = 0;
		object_set = g_new(qsf_objects, 1);
		params->object_set = object_set;
		object_set->object_count = 0;
		object_set->parameters = g_hash_table_new(g_str_hash, g_str_equal);
		object_set->object_type = g_strdup((gchar*)xmlGetProp(child, 
			BAD_CAST QSF_OBJECT_TYPE));
		object_count_s = g_strdup((gchar*)xmlGetProp(child, 
			BAD_CAST QSF_OBJECT_COUNT));
		c = (gint64)strtol(object_count_s, &tail, 0);
		object_set->object_count = (gint)c;
		g_free(object_count_s);
		params->qsf_object_list = g_list_prepend(params->qsf_object_list, object_set);
		iter.ns = qsf_ns;
		params->qsf_parameter_hash = object_set->parameters;
		qsf_node_foreach(child, qsf_parameter_handler, &iter, params);
	}
}

void
qsf_book_node_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params)
{
	gchar *book_count_s, *tail;
	gint book_count;
	xmlNodePtr child_node;
	struct qsf_node_iterate iter;
	gchar *buffer;
	GUID book_guid;

	g_return_if_fail(child);
	g_return_if_fail(params);
	ENTER (" child=%s", child->name);
	if(qsf_is_element(child, ns, QSF_BOOK_TAG)) {
		book_count_s = (gchar*)xmlGetProp(child, BAD_CAST QSF_BOOK_COUNT);
		if(book_count_s) {
			book_count = (gint)strtol(book_count_s, &tail, 0);
			/* More than one book not currently supported. */
			g_return_if_fail(book_count == 1);
		}
		iter.ns = ns;
		child_node = child->children->next;
		if(qsf_is_element(child_node, ns, QSF_BOOK_GUID)) {
			DEBUG (" trying to set book GUID");
			buffer = g_strdup((gchar*)xmlNodeGetContent(child_node));
			g_return_if_fail(TRUE == string_to_guid(buffer, &book_guid));
			qof_instance_set_guid(QOF_INSTANCE(params->book), &book_guid);
			xmlNewChild(params->output_node, params->qsf_ns, 
			BAD_CAST QSF_BOOK_GUID, BAD_CAST buffer);
			g_free(buffer);
		}
		qsf_node_foreach(child, qsf_object_node_handler, &iter, params);
	}
    LEAVE (" ");
}
