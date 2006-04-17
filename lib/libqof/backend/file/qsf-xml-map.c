/***************************************************************************
 *            qsf-xml-map.c
 *
 *  Sat Jan  1 07:31:55 2005
 *  Copyright  2005-2006  Neil Williams
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#define _GNU_SOURCE

#include "config.h"
#include <glib.h>
#include <libxml/xmlversion.h>
#include <libxml/xmlmemory.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlschemas.h>
#include "qof.h"
#include "qof-backend-qsf.h"
#include "qsf-xml.h"
#include "qsf-dir.h"

static QofLogModule log_module = QOF_MOD_QSF;

static void
qsf_date_default_handler(const gchar *default_name, GHashTable *qsf_default_hash,
	xmlNodePtr parent_tag, xmlNodePtr import_node, xmlNsPtr ns)
{
	xmlNodePtr output_parent;
	time_t *qsf_time;
	gchar date_as_string[QSF_DATE_LENGTH];

	output_parent = xmlAddChild(parent_tag, xmlNewNode(ns,
		xmlGetProp(import_node, BAD_CAST QSF_OBJECT_TYPE)));
	xmlNewProp(output_parent, BAD_CAST QSF_OBJECT_TYPE,
		xmlGetProp(import_node, BAD_CAST MAP_VALUE_ATTR));
	qsf_time = (time_t*)g_hash_table_lookup(qsf_default_hash, default_name);
	strftime(date_as_string, QSF_DATE_LENGTH, QSF_XSD_TIME, gmtime(qsf_time));
	xmlNodeAddContent(output_parent, BAD_CAST date_as_string);
}

static void
qsf_string_default_handler(const gchar *default_name, GHashTable *qsf_default_hash,
	xmlNodePtr parent_tag, xmlNodePtr import_node, xmlNsPtr ns)
{
	xmlNodePtr node;
	xmlChar *output;

	node = xmlAddChild(parent_tag,
		xmlNewNode(ns, xmlGetProp(import_node, BAD_CAST QSF_OBJECT_TYPE)));
	xmlNewProp(node, BAD_CAST QSF_OBJECT_TYPE,
		xmlGetProp(import_node, BAD_CAST MAP_VALUE_ATTR));
	output = (xmlChar *)g_hash_table_lookup(qsf_default_hash, default_name);
	xmlNodeAddContent(node, output);
}

static void
qsf_map_validation_handler(xmlNodePtr child, xmlNsPtr ns, qsf_validator *valid)
{
	xmlChar *qof_version, *obj_type;
	gboolean match, is_registered;
	gchar *buff;
	xmlNodePtr child_node;
	QsfStatus type, incoming_type;

	match = FALSE;
	buff = NULL;
	is_registered = FALSE;
	type = QSF_NO_OBJECT;
	if (qsf_is_element(child, ns, MAP_DEFINITION_TAG)) {
		qof_version = xmlGetProp(child, BAD_CAST MAP_QOF_VERSION);
		buff = g_strdup_printf("%i", QSF_QOF_VERSION);
		if(xmlStrcmp(qof_version, BAD_CAST buff) != 0)
		{
			PERR (" Wrong QOF_VERSION in map '%s', should be %s",
				qof_version, buff);
			valid->error_state = ERR_QSF_BAD_QOF_VERSION;
			g_free(buff);
			return;
		}
		g_free(buff);
		for(child_node = child->children; child_node != NULL;
			child_node = child_node->next)
		{
			if (qsf_is_element(child_node, ns, MAP_DEFINE_TAG)) {
				obj_type = xmlGetProp(child_node, MAP_E_TYPE);
				type = QSF_DEFINED_OBJECT;
				is_registered = qof_class_is_registered(obj_type);
				if(is_registered) { type = QSF_REGISTERED_OBJECT; }
				g_hash_table_insert(valid->map_table, obj_type,
					GINT_TO_POINTER(type));
			}
		}
	}
	if(qsf_is_element(child, ns, MAP_OBJECT_TAG)) {
		obj_type = xmlGetProp(child, BAD_CAST MAP_TYPE_ATTR);
		/* check each listed object is either registered or calculated. */
		type = GPOINTER_TO_INT(g_hash_table_lookup(valid->map_table, obj_type));
		switch(type)
		{
			case QSF_DEFINED_OBJECT :
			/* we have a calculation for an unregistered object. */
			/* Ignore the calculation that exists to support bidirectional maps. */
			/* Check that the incoming QSF contains data for this object */
			{
				/* lookup the same object in QSF object_table */
				incoming_type = GPOINTER_TO_INT(g_hash_table_lookup(valid->object_table,
					obj_type));
				switch (incoming_type)
				{
					case QSF_DEFINED_OBJECT : 
					{
						valid->incoming_count++;
						g_hash_table_insert(valid->map_table, obj_type, 
							GINT_TO_POINTER(type));
						break; /* good, proceed. */
					}
					default :
					{
						PERR (" Missing data: %s", obj_type);
						type = QSF_INVALID_OBJECT; break;
					}
				}
				break;
			}
			case QSF_REGISTERED_OBJECT : /* use this calculation. */
			{
				type = QSF_CALCULATED_OBJECT;
				valid->map_calculated_count++;
				valid->qof_registered_count++;
				/* store the result */
				g_hash_table_insert(valid->map_table, obj_type, GINT_TO_POINTER(type));
				break;
			}
			default :
			{
				type = QSF_INVALID_OBJECT;
				break;
			}
		}
		PINFO (" final type=%s result=%d", obj_type, type);
		if(type == QSF_INVALID_OBJECT) { valid->error_state = ERR_QSF_WRONG_MAP; }
	}
}

static QofBackendError
check_qsf_object_with_map_internal(xmlDocPtr map_doc, xmlDocPtr doc)
{
	xmlNodePtr map_root, object_root;
	struct qsf_node_iterate iter;
	qsf_validator valid;
	xmlNsPtr map_ns;

	valid.map_table = g_hash_table_new(g_str_hash, g_str_equal);
	valid.object_table = g_hash_table_new(g_str_hash, g_str_equal);
	map_root = xmlDocGetRootElement(map_doc);
	object_root = xmlDocGetRootElement(doc);
	valid.map_calculated_count = 0;
	valid.valid_object_count = 0;
	valid.qof_registered_count = 0;
	valid.incoming_count = 0;
	valid.error_state = ERR_BACKEND_NO_ERR;
	map_ns = map_root->ns;
	iter.ns = object_root->ns;
	qsf_valid_foreach(object_root, qsf_object_validation_handler, &iter, &valid);
	iter.ns = map_ns;
	qsf_valid_foreach(map_root, qsf_map_validation_handler, &iter, &valid);
	if (valid.error_state != ERR_BACKEND_NO_ERR) {
		PINFO (" Map is wrong. Trying the next map.");
		g_hash_table_destroy(valid.object_table);
		g_hash_table_destroy(valid.map_table);
		return valid.error_state;
	}
	/* check all counted objects are valid:
	Objects to be calculated must also be registered
	so that new objects can be created and populated
	from the incoming data: qof_registered_count > 0
	The incoming data must contain valid objects -
	not an empty QofBook: valid_object_count > 0
	The map must contain at least some calculations:
	map_calculated_count > 0
	*/
	if((valid.qof_registered_count < 1) 
		|| (valid.map_calculated_count < 1)
		|| (valid.valid_object_count < 1)
		|| (valid.incoming_count < g_hash_table_size(valid.object_table)))
	{
		PINFO (" Map is wrong. map:%d object:%d reg:%d incoming:%d size:%d",
			valid.map_calculated_count, valid.valid_object_count, 
			valid.qof_registered_count, valid.incoming_count,
			g_hash_table_size(valid.object_table));
		if(valid.error_state != ERR_BACKEND_NO_ERR)
		{
			valid.error_state = ERR_QSF_WRONG_MAP;
		}
		g_hash_table_destroy(valid.object_table);
		g_hash_table_destroy(valid.map_table);
		return valid.error_state;
	}
	g_hash_table_destroy(valid.object_table);
	g_hash_table_destroy(valid.map_table);
	return ERR_BACKEND_NO_ERR;
}

gboolean is_qsf_object_with_map_be(gchar *map_file, qsf_param *params)
{
	xmlDocPtr doc, map_doc;
	QofBackendError result;
	gchar *path, *map_path;

	g_return_val_if_fail((params != NULL),FALSE);
	path = g_strdup(params->filepath);
	map_path = g_strdup_printf("%s/%s", QSF_SCHEMA_DIR, map_file);
	PINFO (" checking map file '%s'", map_path);
	if(path == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_FILE_NOT_FOUND);
		return FALSE;
	}
	doc = xmlParseFile(path);
	if(doc == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
		return FALSE;
	}
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, doc)) {
		qof_backend_set_error(params->be, ERR_QSF_INVALID_OBJ);
		return FALSE;
	}
	if(map_path == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_FILE_NOT_FOUND);
		return FALSE;
	}
	map_doc = xmlParseFile(map_path);
	if(map_doc == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
		return FALSE;
	}
	result = check_qsf_object_with_map_internal(map_doc, doc);
	qof_backend_set_error(params->be, result);
	return (result == ERR_BACKEND_NO_ERR) ? TRUE : FALSE;
}

gboolean is_qsf_object_with_map(const gchar *path, gchar *map_file)
{
	xmlDocPtr doc, map_doc;
	QofBackendError result;
	gchar *map_path;

	map_path = g_strdup_printf("%s/%s", QSF_SCHEMA_DIR, map_file);
	if(path == NULL) {
		return FALSE;
	}
	doc = xmlParseFile(path);
	if(doc == NULL) {
		return FALSE;
	}
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, doc)) {
		return FALSE;
	}
	if(map_path == NULL) {
		return FALSE;
	}
	map_doc = xmlParseFile(map_path);
	result = check_qsf_object_with_map_internal(map_doc, doc);
	return (result == ERR_BACKEND_NO_ERR) ? TRUE : FALSE;
}

gboolean is_qsf_map_be(qsf_param *params)
{
	xmlDocPtr doc;
	struct qsf_node_iterate iter;
	qsf_validator valid;
	xmlNodePtr map_root;
	xmlNsPtr map_ns;
	gchar *path;

	g_return_val_if_fail((params != NULL),FALSE);
	qof_backend_get_error(params->be);
	path = g_strdup(params->filepath);
	if(path == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_FILE_NOT_FOUND);
		return FALSE;
	}
	doc = xmlParseFile(path);
	if(doc == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
		return FALSE;
	}
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_MAP_SCHEMA, doc)) {
		qof_backend_set_error(params->be, ERR_QSF_INVALID_MAP);
		return FALSE;
	}
	map_root = xmlDocGetRootElement(doc);
	map_ns = map_root->ns;
	iter.ns = map_ns;
	valid.object_table = g_hash_table_new(g_str_hash, g_str_equal);
	valid.map_table = g_hash_table_new(g_str_hash, g_str_equal);
	valid.error_state = ERR_BACKEND_NO_ERR;
	qsf_valid_foreach(map_root, qsf_map_validation_handler, &iter, &valid);
	if (valid.error_state != ERR_BACKEND_NO_ERR) {
		qof_backend_set_error(params->be, valid.error_state);
		g_hash_table_destroy(valid.object_table);
		return FALSE;
	}
	qof_backend_get_error(params->be);
	g_hash_table_destroy(valid.object_table);
	return TRUE;
}

gboolean is_qsf_map(const gchar *path)
{
	xmlDocPtr doc;
	struct qsf_node_iterate iter;
	qsf_validator valid;
	xmlNodePtr map_root;
	xmlNsPtr map_ns;

	g_return_val_if_fail((path != NULL),FALSE);
	if(path == NULL) { return FALSE; }
	doc = xmlParseFile(path);
	if(doc == NULL) { return FALSE; }
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_MAP_SCHEMA, doc)) {
		return FALSE;
	}
	map_root = xmlDocGetRootElement(doc);
	map_ns = map_root->ns;
	iter.ns = map_ns;
	valid.error_state = ERR_BACKEND_NO_ERR;
	valid.map_table = g_hash_table_new(g_str_hash, g_str_equal);
	qsf_valid_foreach(map_root, qsf_map_validation_handler, &iter, &valid);
	if (valid.error_state != ERR_BACKEND_NO_ERR) {
		g_hash_table_destroy(valid.map_table);
		return FALSE;
	}
	g_hash_table_destroy(valid.map_table);
	return TRUE;
}

static void
qsf_map_default_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params )
{
	xmlChar *qsf_enum;
	gchar* iterate;

	g_return_if_fail(params->qsf_define_hash != NULL);
	iterate = NULL;
	if (qsf_is_element(child, ns, MAP_DEFINE_TAG)) {
		iterate = xmlGetProp(child, MAP_ITERATE_ATTR);
		if((qof_util_bool_to_int(iterate) == 1) &&
			(qof_class_is_registered(xmlGetProp(child, BAD_CAST MAP_E_TYPE))))
		{
			params->qof_foreach = xmlGetProp(child, BAD_CAST MAP_E_TYPE);
			PINFO (" iterating over '%s' objects", params->qof_foreach);
		}
		if(NULL == g_hash_table_lookup(params->qsf_define_hash,
			xmlGetProp(child, BAD_CAST MAP_E_TYPE)))
		{
			g_hash_table_insert(params->qsf_define_hash,
				xmlGetProp(child, BAD_CAST MAP_E_TYPE), params->child_node);
		}
		else {
			qof_backend_set_error(params->be, ERR_QSF_BAD_MAP);
			PERR (" ERR_QSF_BAD_MAP set");
			return;
		}
	}
	if(qsf_is_element(child, ns, MAP_DEFAULT_TAG)) {
		if(qsf_strings_equal(xmlGetProp(child, BAD_CAST MAP_TYPE_ATTR), MAP_ENUM_TYPE))
		{
			qsf_enum = xmlNodeGetContent(child);
			/** Use content to discriminate enums in QOF */
			PERR (" enum todo incomplete");
			/** \todo FIXME: the default enum value is not used
			implemented properly or fully handled.
			*/
			if(NULL == g_hash_table_lookup(params->qsf_default_hash,
				xmlNodeGetContent(child)))
			{
				g_hash_table_insert(params->qsf_default_hash,
					xmlNodeGetContent(child), child);
			}
			else
			{
				qof_backend_set_error(params->be, ERR_QSF_BAD_MAP);
				PERR (" ERR_QSF_BAD_MAP set");
				return;
			}
		}
		/** Non-enum defaults */
		else {
			if(NULL == g_hash_table_lookup(params->qsf_default_hash,
					xmlGetProp(child, BAD_CAST MAP_NAME_ATTR)))
			{
				g_hash_table_insert(params->qsf_default_hash,
					xmlGetProp(child, BAD_CAST MAP_NAME_ATTR), child);
			}
			else
/*					if(0 != xmlHashAddEntry(params->default_map,
				xmlGetProp(child_node, MAP_NAME_ATTR), child_node))*/
			{
				qof_backend_set_error(params->be, ERR_QSF_BAD_MAP);
				PERR (" ERR_QSF_BAD_MAP set");
				return;
			}
		}
	}
}

static void
qsf_map_top_node_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params)
{
	xmlChar	*qof_version;
	gchar *buff;
	struct qsf_node_iterate iter;

	if(!params->qsf_define_hash) return;
	if(!params->qsf_default_hash) return;
	ENTER (" map top node child=%s", child->name);
	buff = NULL;
	if(qsf_is_element(child, ns, MAP_DEFINITION_TAG)) {
		qof_version = xmlGetProp(child, BAD_CAST MAP_QOF_VERSION);
		buff = g_strdup_printf("%i", QSF_QOF_VERSION);
		if(xmlStrcmp(qof_version, BAD_CAST buff) != 0) {
			qof_backend_set_error(params->be, ERR_QSF_BAD_QOF_VERSION);
			LEAVE (" ERR_QSF_BAD_QOF_VERSION set");
			return;
		}
		iter.ns = ns;
		qsf_node_foreach(child, qsf_map_default_handler, &iter, params);
	}
	LEAVE (" ");
}

static char*
qsf_else_set_value(xmlNodePtr parent, GHashTable *default_hash,
		gchar *content, xmlNsPtr map_ns)
{
	xmlNodePtr cur_node;

	content = NULL;
	for(cur_node = parent->children; cur_node != NULL; cur_node = cur_node->next)
	{
		if(qsf_is_element(cur_node, map_ns, QSF_CONDITIONAL_SET)) {
			content = (gchar*)xmlNodeGetContent(cur_node);
			return content;
		}
	}
	return NULL;
}

/* Handles the set tag in the map.
This function will be overhauled once inside QOF
QOF hook required for "Lookup in the receiving application"
*/
static gchar*
qsf_set_handler(xmlNodePtr parent, GHashTable *default_hash,
	gchar *content, qsf_param *params)
{
	xmlNodePtr cur_node, lookup_node;

	ENTER (" lookup problem");
	content = NULL;
	for(cur_node = parent->children; cur_node != NULL; cur_node = cur_node->next)
	{
		if(qsf_is_element(cur_node, params->map_ns, QSF_CONDITIONAL_SET))
		{
			content = (gchar*)xmlGetProp(cur_node, BAD_CAST QSF_OPTION);
			if(qsf_strings_equal(xmlGetProp(cur_node, 
					BAD_CAST QSF_OPTION), "qsf_lookup_string"))
			{
				lookup_node = (xmlNodePtr) g_hash_table_lookup(default_hash,
					xmlNodeGetContent(cur_node));
				content = (gchar*)xmlGetProp(lookup_node, BAD_CAST MAP_VALUE_ATTR);
				/** \todo FIXME: do the lookup. type is defined by output object. */
				/* Find by name, get GUID, return GUID as string. */
				g_message("Lookup %s in the receiving application\n", content );
				LEAVE (" todo");
				return content;
			}
			if(content)
			{
				lookup_node = (xmlNodePtr) g_hash_table_lookup(default_hash,
					xmlNodeGetContent(cur_node));
				content = (gchar*)xmlGetProp(lookup_node, BAD_CAST "value");
				return content;
			}
			content = (gchar*)xmlGetProp(parent, BAD_CAST "boolean");
			if(!content) {
				/** \todo Check qsf_parameter_hash arguments */
				lookup_node = (xmlNodePtr) g_hash_table_lookup(params->qsf_parameter_hash,
					xmlGetProp(parent->parent, BAD_CAST MAP_TYPE_ATTR));
				if(lookup_node) { return (gchar*)xmlNodeGetContent(lookup_node); }
				LEAVE (" check arguments");
				return (gchar*)xmlNodeGetContent(cur_node);
			}
		}
	}
	LEAVE (" null");
	return NULL;
}

static void
qsf_calculate_else(xmlNodePtr param_node, xmlNodePtr child, qsf_param *params)
{
	xmlNodePtr export_node;
	xmlChar *output_content, *object_data;

	if(qsf_is_element(param_node, params->map_ns, QSF_CONDITIONAL_ELSE)) {
		if(params->boolean_calculation_done == 0) {
			output_content = object_data = NULL;
			output_content = BAD_CAST qsf_set_handler(param_node,
				params->qsf_default_hash, (gchar*)output_content, params);
			if(output_content == NULL) {
				output_content = xmlGetProp(param_node, BAD_CAST MAP_TYPE_ATTR);
				object_data = BAD_CAST qsf_else_set_value(param_node, params->qsf_default_hash,
					(gchar*)output_content, params->map_ns);
				output_content = BAD_CAST xmlGetProp( (xmlNodePtr) g_hash_table_lookup(
					params->qsf_default_hash, object_data), BAD_CAST MAP_VALUE_ATTR);
			}
			if(object_data != NULL) {
				export_node =(xmlNodePtr) g_hash_table_lookup(
					params->qsf_parameter_hash,
					xmlGetProp(params->child_node, BAD_CAST QSF_OBJECT_TYPE));
				object_data = xmlNodeGetContent(export_node);
			}
			if(output_content != NULL) { object_data = output_content; }
			export_node = xmlAddChild(params->lister, xmlNewNode(params->qsf_ns,
				xmlGetProp(child, BAD_CAST QSF_OBJECT_TYPE)));
			xmlNewProp(export_node, BAD_CAST QSF_OBJECT_TYPE,
				xmlGetProp(child, BAD_CAST MAP_VALUE_ATTR));
			xmlNodeAddContent(export_node, object_data);
			params->boolean_calculation_done = 1;
		}
	}
}

static void
qsf_set_format_value(xmlChar *format, gchar *qsf_time_now_as_string,
	xmlNodePtr cur_node, qsf_param *params)
{
	gint result;
	xmlChar *content;
	time_t *output;
	struct tm *tmp;
	time_t tester;
	xmlNodePtr kl;
	regex_t reg;

	/** Comments retained - this behaves a little strangely */

	result = 0;
	if(format == NULL) { return; }
	ENTER (" ");
	content = xmlNodeGetContent(cur_node);
	output = (time_t*) g_hash_table_lookup(params->qsf_default_hash, content);
	if(!output) {
		/** No default time set, use the object time */
		/** fill the time structs with temp data */
		tester = time(NULL);
		tmp = gmtime(&tester);
		/** Lookup the object value to read */
		/** \todo qsf_parameter_hash check correct arguments */
		kl = (xmlNodePtr) g_hash_table_lookup(params->qsf_parameter_hash, content);
		if(!kl) {
			LEAVE (" no suitable date set.");
			return;
		}
		/** Read the object value as a dateTime  */
		strptime((char*)xmlNodeGetContent(kl), QSF_XSD_TIME, tmp);
		if(!tmp) {
			LEAVE (" empty date field in QSF object.\n");
			return;
		}
		tester = mktime(tmp);
		output = &tester;
	}
	result = regcomp(&reg, "%[a-zA-Z]", REG_EXTENDED|REG_NOSUB);
	result = regexec(&reg, (gchar*)format,(size_t)0,NULL,0);
	if(result == REG_NOMATCH) { format = BAD_CAST "%F"; }
	regfree(&reg);
	/* QSF_DATE_LENGTH preset for all internal and QSF_XSD_TIME string formats. */
	strftime(qsf_time_now_as_string, QSF_DATE_LENGTH, (char*)format, gmtime(output));
	LEAVE (" ok");
}

static void
qsf_boolean_set_value(xmlNodePtr parent, qsf_param *params,
		gchar *content, xmlNsPtr map_ns)
{
	xmlNodePtr cur_node;
	xmlChar *boolean_name;

	boolean_name = NULL;
	for(cur_node = parent->children; cur_node != NULL; cur_node = cur_node->next) {
		if(qsf_is_element(cur_node, map_ns, QSF_CONDITIONAL_SET)) {
			boolean_name = xmlGetProp(cur_node, BAD_CAST QSF_FORMATTING_OPTION);
			qsf_set_format_value(boolean_name, content, cur_node, params);
		}
	}
}

static void
qsf_calculate_conditional(xmlNodePtr param_node, xmlNodePtr child, qsf_param *params)
{
	xmlNodePtr export_node;
	xmlChar *output_content;

	output_content = NULL;
	if(qsf_is_element(param_node, params->map_ns, QSF_CONDITIONAL)) {
		if(params->boolean_calculation_done == 0) {
		/* set handler */
		output_content = BAD_CAST qsf_set_handler(param_node, params->qsf_default_hash,
				(gchar*)output_content, params);
		/* If the 'if' contains a boolean that has a default value */
		if(output_content == NULL) {
			if(NULL != xmlGetProp(param_node, BAD_CAST QSF_BOOLEAN_DEFAULT)) {
			output_content = xmlGetProp( (xmlNodePtr) g_hash_table_lookup(
				params->qsf_default_hash, xmlGetProp(param_node,
				BAD_CAST QSF_BOOLEAN_DEFAULT) ), BAD_CAST MAP_VALUE_ATTR);
			}
			/* Is the default set to true? */
			if( 0 == qsf_compare_tag_strings(output_content, QSF_XML_BOOLEAN_TEST))
			{
				qsf_boolean_set_value(param_node, params, (gchar*)output_content, 
					params->map_ns);
				export_node = xmlAddChild(params->lister, xmlNewNode(params->qsf_ns,
					xmlGetProp(child, BAD_CAST QSF_OBJECT_TYPE)));
				xmlNewProp(export_node, BAD_CAST QSF_OBJECT_TYPE,
					xmlGetProp(child, BAD_CAST MAP_VALUE_ATTR));
				xmlNodeAddContent(export_node, output_content);
				params->boolean_calculation_done = 1;
				}
			}
		}
	}
}

static void
qsf_add_object_tag(qsf_param *params, gint count)
{
	xmlNodePtr extra_node;
	GString *str;
	xmlChar *property;

	str = g_string_new (" ");
	g_string_printf(str, "%i", count);
	extra_node = NULL;
	extra_node = xmlAddChild(params->output_node,
		xmlNewNode(params->qsf_ns, BAD_CAST QSF_OBJECT_TAG));
	xmlNewProp(extra_node, BAD_CAST QSF_OBJECT_TYPE,
		xmlGetProp(params->convert_node, BAD_CAST QSF_OBJECT_TYPE));
	property = xmlCharStrdup(str->str);
	xmlNewProp(extra_node, BAD_CAST QSF_OBJECT_COUNT, property);
	params->lister = extra_node;
}

static gint
identify_source_func(gconstpointer qsf_object, gconstpointer map)
{
	PINFO (" qsf_object=%s, map=%s", 
		((qsf_objects*)qsf_object)->object_type, (QofIdType)map);
	return safe_strcmp(((qsf_objects*)qsf_object)->object_type, (QofIdType)map);
}

static void
qsf_map_calculate_output(xmlNodePtr param_node, xmlNodePtr child, qsf_param *params)
{
	xmlNodePtr export_node;
	xmlChar *output_content;
	xmlNodePtr input_node;
	GList *source;

	output_content = xmlNodeGetContent(param_node);
	DEBUG (" %s", output_content);
	/* source refers to the source object that provides the data */
	source = g_list_find_custom(params->qsf_object_list, 
		BAD_CAST xmlGetProp(param_node, MAP_OBJECT_ATTR), identify_source_func);
	PINFO (" checking %s", BAD_CAST xmlGetProp(param_node, MAP_OBJECT_ATTR));
	if(!source) { DEBUG (" no source found in list."); return; }
	params->object_set = source->data;
	input_node = g_hash_table_lookup(params->object_set->parameters, 
		output_content);
	DEBUG (" node_value=%s, content=%s", 
		xmlGetProp(child, BAD_CAST MAP_VALUE_ATTR), 
		xmlNodeGetContent(input_node));
	export_node = xmlAddChild(params->lister, xmlNewNode(params->qsf_ns,
		xmlGetProp(child, BAD_CAST QSF_OBJECT_TYPE)));
	xmlNewProp(export_node, BAD_CAST QSF_OBJECT_TYPE,
		xmlGetProp(child, BAD_CAST MAP_VALUE_ATTR));
	xmlNodeAddContent(export_node, xmlNodeGetContent(input_node));
}

static void
qsf_map_object_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params)
{
	xmlNodePtr param_node;
	xmlNsPtr map_ns, qsf_ns;
	gint result;

	map_ns = ns;
	qsf_ns = params->qsf_ns;
	param_node = NULL;
	result = 0;
	if(child == NULL) { return; }
	if(ns == NULL) { return; }
	params->boolean_calculation_done = 0;

	if(qsf_is_element(child, map_ns, MAP_CALCULATE_TAG)) {
		params->boolean_calculation_done = 0;
        /* read the child nodes to prepare the calculation. */
		for(param_node = child->children; param_node != NULL;
			param_node = param_node->next)
		{
			if(qsf_is_element(param_node, map_ns, QSF_CONDITIONAL_SET))
			{
				/* Map the pre-defined defaults */
				if(0 == qsf_compare_tag_strings(
					xmlNodeGetContent(param_node), "qsf_enquiry_date"))
				{
					qsf_string_default_handler("qsf_enquiry_date",
						params->qsf_default_hash, params->lister, child, qsf_ns);
				}
				if(0 == qsf_compare_tag_strings(
					xmlNodeGetContent(param_node), "qsf_time_now"))
				{
					qsf_date_default_handler("qsf_time_now",
						params->qsf_default_hash, params->lister, child, qsf_ns);
				}
				if(0 == qsf_compare_tag_strings(
					xmlNodeGetContent(param_node), "qsf_time_string"))
				{
					qsf_string_default_handler("qsf_time_string",
						params->qsf_default_hash, params->lister, child, qsf_ns);
				}
				qsf_map_calculate_output(param_node, child, params);
			}
			qsf_calculate_conditional( param_node, child, params);
			qsf_calculate_else(param_node, child, params);
		}
		/* calculate_map currently not in use */
		/* ensure uniqueness of the key before re-instating */
/*		result = xmlHashAddEntry2(calculate_map,
			xmlGetProp(child_node, MAP_TYPE_ATTR),
			xmlGetProp(child_node, MAP_VALUE_ATTR), child_node);
		if(result != 0) {
			printf("add entry to calculate hash failed. %s\t%s\t%s.\n",
				xmlGetProp(child_node, MAP_TYPE_ATTR),
			xmlGetProp(child_node, MAP_VALUE_ATTR), child_node->name);
			return;
		}

		is_qsf_object_with_map(path, map_path);
*/
	}
}

static void
iterator_cb(xmlNodePtr child, xmlNsPtr ns, qsf_param *params)
{
	gchar *object_name;

	/* count the number of iterators in the QSF file */
	if(qsf_is_element(child, ns, QSF_OBJECT_TAG))
	{
		object_name = xmlGetProp(child, QSF_OBJECT_TYPE);
		if(0 == safe_strcmp(object_name, params->qof_foreach))
		{
			params->foreach_limit++;
		}
	}
}

xmlDocPtr
qsf_object_convert(xmlDocPtr mapDoc, xmlNodePtr qsf_root, qsf_param *params)
{
	/* mapDoc : map document. qsf_root: incoming QSF root node. */
	struct qsf_node_iterate iter;
	xmlDocPtr output_doc;
	xmlNode *cur_node;
	xmlNode *map_root, *output_root;

	g_return_val_if_fail((mapDoc && qsf_root && params), NULL);
	ENTER (" root=%s", qsf_root->name);
	/* prepare the intermediary document */
	iter.ns = params->qsf_ns;
	output_doc = xmlNewDoc(BAD_CAST QSF_XML_VERSION);
	output_root = xmlNewNode(NULL, BAD_CAST QSF_ROOT_TAG);
	xmlDocSetRootElement(output_doc, output_root);
	xmlSetNs(output_root, params->qsf_ns);
	params->output_node = xmlNewChild(output_root, params->qsf_ns, 
        BAD_CAST QSF_BOOK_TAG, NULL);
	xmlNewProp(params->output_node, BAD_CAST QSF_BOOK_COUNT, BAD_CAST "1");
	/* parse the incoming QSF */
	qsf_book_node_handler(qsf_root->children->next, params->qsf_ns, params);
	/* parse the map and calculate the values */
	map_root = xmlDocGetRootElement(mapDoc);
	params->foreach_limit = 0;
	iter.ns = params->map_ns;
	/* sets qof_foreach iterator, defines and defaults. */
	qsf_node_foreach(map_root, qsf_map_top_node_handler, &iter, params);
	/* identify the entities of iterator type. */
	iter.ns = params->qsf_ns;
	qsf_node_foreach(qsf_root->children->next, iterator_cb, &iter, params);
	PINFO (" counted %d records", params->foreach_limit);
	params->count = 0;
	for(cur_node = map_root->children; cur_node != NULL; cur_node = cur_node->next)
	{
		params->convert_node = cur_node;
		if(qsf_is_element(cur_node, params->map_ns, MAP_OBJECT_TAG))
		{
			gint i;

			params->lister = NULL;
			PINFO (" found an object tag. starting calculation");
			/* cur_node describes the target object */
			if(!qof_class_is_registered(BAD_CAST
				xmlGetProp(cur_node, MAP_TYPE_ATTR))) { continue; }
			qsf_add_object_tag(params, params->count);
			params->count++;
			iter.ns = params->map_ns;
			PINFO (" params->foreach_limit=%d", params->foreach_limit);
			for(i = -1; i < params->foreach_limit; i++)
			{
				qsf_node_foreach(cur_node, qsf_map_object_handler, &iter, params);
				params->qsf_object_list = g_list_next(params->qsf_object_list);
				params->count++;
			}
		}
	}
	params->file_type = OUR_QSF_OBJ;
	/* use for debugging */
	xmlSaveFormatFileEnc("-", output_doc, "UTF-8", 1);
	LEAVE (" ");
	return output_doc;
}
