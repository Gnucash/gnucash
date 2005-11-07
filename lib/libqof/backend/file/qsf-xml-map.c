/***************************************************************************
 *            qsf-xml-map.c
 *
 *  Sat Jan  1 07:31:55 2005
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

#include <libxml/xmlversion.h>
#include <libxml/xmlmemory.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlschemas.h>
#include "qof-backend-qsf.h"
#include "qsf-xml.h"
#include "qsf-dir.h"

static void
qsf_date_default_handler(const char *default_name, GHashTable *qsf_default_hash,
	xmlNodePtr parent_tag, xmlNodePtr import_node, xmlNsPtr ns)
{
	xmlNodePtr output_parent;
	time_t *qsf_time;
	char date_as_string[QSF_DATE_LENGTH];

	output_parent = xmlAddChild(parent_tag, xmlNewNode(ns,
		xmlGetProp(import_node, BAD_CAST QSF_OBJECT_TYPE)));
	xmlNewProp(output_parent, BAD_CAST QSF_OBJECT_TYPE,
		xmlGetProp(import_node, BAD_CAST MAP_VALUE_ATTR));
	qsf_time = (time_t*)g_hash_table_lookup(qsf_default_hash, default_name);
	strftime(date_as_string, QSF_DATE_LENGTH, QSF_XSD_TIME, gmtime(qsf_time));
	xmlNodeAddContent(output_parent, BAD_CAST date_as_string);
}

static void
qsf_string_default_handler(const char *default_name, GHashTable *qsf_default_hash,
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

void
qsf_map_validation_handler(xmlNodePtr child, xmlNsPtr ns, qsf_validator *valid)
{
	xmlChar *qof_version, *match;
	GString *buff;
	xmlNodePtr child_node;

	if (qsf_is_element(child, ns, MAP_DEFINITION_TAG)) {
		qof_version = xmlGetProp(child, BAD_CAST MAP_QOF_VERSION);
		buff = g_string_new(" ");
		g_string_printf(buff, "%i", QSF_QOF_VERSION);
		if(xmlStrcmp(qof_version, BAD_CAST buff->str) != 0)
		{
			valid->error_state = ERR_QSF_BAD_QOF_VERSION;
			return;
		}
		for(child_node = child->children; child_node != NULL;
			child_node = child_node->next)
		{
			if (qsf_is_element(child_node, ns, MAP_DEFINE_TAG)) {
				g_hash_table_insert(valid->validation_table,
					xmlGetProp(child_node, BAD_CAST MAP_E_TYPE),
					xmlNodeGetContent(child_node));
			}
		}
	}
	if(qsf_is_element(child, ns, MAP_OBJECT_TAG)) {
		match = NULL;
		match = BAD_CAST  g_hash_table_lookup( valid->validation_table,
			xmlGetProp(child, BAD_CAST MAP_TYPE_ATTR));
		if(match) {
			valid->map_calculated_count++;
		}
	}
}

gboolean is_qsf_object_with_map_be(char *map_file, qsf_param *params)
{
	xmlDocPtr doc, map_doc;
	int valid_count;
	struct qsf_node_iterate iter;
	xmlNodePtr map_root, object_root;
	xmlNsPtr map_ns;
	qsf_validator valid;
	char *path;
	gchar *map_path;

	g_return_val_if_fail((params != NULL),FALSE);
	path = g_strdup(params->filepath);
	map_path = g_strdup_printf("%s/%s", QSF_SCHEMA_DIR, map_file);
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
	object_root = xmlDocGetRootElement(doc);
	if(map_path == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_FILE_NOT_FOUND);
		return FALSE;
	}
	valid.validation_table = g_hash_table_new(g_str_hash, g_str_equal);
	map_doc = xmlParseFile(map_path);
	if(map_doc == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
		return FALSE;
	}
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_MAP_SCHEMA, map_doc)) {
		qof_backend_set_error(params->be, ERR_QSF_INVALID_MAP);
		return FALSE;
	}
	map_root = xmlDocGetRootElement(map_doc);
	valid.map_calculated_count = 0;
	valid.valid_object_count = 0;
	valid.qof_registered_count = 0;
	valid.error_state = ERR_BACKEND_NO_ERR;
	map_ns = map_root->ns;
	iter.ns = object_root->ns;
	qsf_valid_foreach(object_root, qsf_object_validation_handler, &iter, &valid);
	iter.ns = map_ns;
	qsf_valid_foreach(map_root, qsf_map_validation_handler, &iter, &valid);
	if (valid.error_state != ERR_BACKEND_NO_ERR) {
		qof_backend_set_error(params->be, valid.error_state);
		g_hash_table_destroy(valid.validation_table);
		return FALSE;
	}
	valid_count = 0 - g_hash_table_size(valid.validation_table);
	valid_count += valid.map_calculated_count;
	valid_count += valid.valid_object_count;
	g_hash_table_destroy(valid.validation_table);
	if(valid_count == 0) {
		qof_backend_get_error(params->be);
		return TRUE;
	}
	qof_backend_set_error(params->be, ERR_QSF_WRONG_MAP);
	/* the object is OK, only the map is wrong. */
	return TRUE;
}

gboolean is_qsf_object_with_map(const char *path, char *map_file)
{
	xmlDocPtr doc, map_doc;
	int valid_count;
	struct qsf_node_iterate iter;
	xmlNodePtr map_root, object_root;
	xmlNsPtr map_ns;
	qsf_validator valid;
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
	object_root = xmlDocGetRootElement(doc);
	if(map_path == NULL) {
		return FALSE;
	}
	valid.validation_table = g_hash_table_new(g_str_hash, g_str_equal);
	map_doc = xmlParseFile(map_path);
	if(map_doc == NULL) {
		return FALSE;
	}
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_MAP_SCHEMA, map_doc)) {
		return FALSE;
	}
	map_root = xmlDocGetRootElement(map_doc);
	valid.map_calculated_count = 0;
	valid.valid_object_count = 0;
	valid.error_state = ERR_BACKEND_NO_ERR;
	map_ns = map_root->ns;
	iter.ns = map_ns;
	qsf_valid_foreach(map_root, qsf_map_validation_handler, &iter, &valid);
	iter.ns = object_root->ns;
	qsf_valid_foreach(object_root, qsf_object_validation_handler, &iter, &valid);
	if (valid.error_state != ERR_BACKEND_NO_ERR) {
		g_hash_table_destroy(valid.validation_table);
		return FALSE;
	}
	valid_count = 0 - g_hash_table_size(valid.validation_table);
	valid_count += valid.map_calculated_count;
	valid_count += valid.valid_object_count;
	g_hash_table_destroy(valid.validation_table);
	if(valid_count == 0) {
		return TRUE;
	}
	return FALSE;
}

gboolean is_qsf_map_be(qsf_param *params)
{
	xmlDocPtr doc;
	struct qsf_node_iterate iter;
	qsf_validator valid;
	xmlNodePtr map_root;
	xmlNsPtr map_ns;
	char *path;

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
	valid.error_state = ERR_BACKEND_NO_ERR;
	qsf_valid_foreach(map_root, qsf_map_validation_handler, &iter, &valid);
	if (valid.error_state != ERR_BACKEND_NO_ERR) {
		qof_backend_set_error(params->be, valid.error_state);
		g_hash_table_destroy(valid.validation_table);
		return FALSE;
	}
	qof_backend_get_error(params->be);
	g_hash_table_destroy(valid.validation_table);
	return TRUE;
}

gboolean is_qsf_map(const char *path)
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
	qsf_valid_foreach(map_root, qsf_map_validation_handler, &iter, &valid);
	if (valid.error_state != ERR_BACKEND_NO_ERR) {
		g_hash_table_destroy(valid.validation_table);
		return FALSE;
	}
	g_hash_table_destroy(valid.validation_table);
	return TRUE;
}


static void
qsf_map_default_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params )
{
	xmlChar *qsf_enum;

	g_return_if_fail(params->qsf_define_hash != NULL);
	if (qsf_is_element(child, ns, MAP_DEFINE_TAG)) {
		if(NULL == g_hash_table_lookup(params->qsf_define_hash,
			xmlGetProp(child, BAD_CAST MAP_E_TYPE)))
		{
			g_hash_table_insert(params->qsf_define_hash,
				xmlGetProp(child, BAD_CAST MAP_E_TYPE), params->child_node);
		}
		else {
			qof_backend_set_error(params->be, ERR_QSF_BAD_MAP);
			return;
		}
	}
	if(qsf_is_element(child, ns, MAP_DEFAULT_TAG)) {
		if(qsf_strings_equal(xmlGetProp(child, BAD_CAST MAP_TYPE_ATTR), MAP_ENUM_TYPE))
		{
			qsf_enum = xmlNodeGetContent(child);
			/** Use content to discriminate enums in QOF */
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
				return;
			}
		}
	}
}

void
qsf_map_top_node_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params)
{
	xmlChar	*qof_version;
	GString *buff;
	struct qsf_node_iterate iter;

	if(!params->qsf_define_hash) return;
	if(!params->qsf_default_hash) return;
	if(qsf_is_element(child, ns, MAP_DEFINITION_TAG)) {
		qof_version = xmlGetProp(child, BAD_CAST MAP_QOF_VERSION);
		buff = g_string_new(" ");
		g_string_printf(buff, "%i", QSF_QOF_VERSION);
		if(xmlStrcmp(qof_version, BAD_CAST buff->str) != 0) {
			qof_backend_set_error(params->be, ERR_QSF_BAD_QOF_VERSION);
			return;
		}
		iter.ns = ns;
		qsf_node_foreach(child, qsf_map_default_handler, &iter, params);
	}
}

static char*
qsf_else_set_value(xmlNodePtr parent, GHashTable *default_hash,
		char *content, xmlNsPtr map_ns)
{
	xmlNodePtr cur_node;

	content = NULL;
	for(cur_node = parent->children; cur_node != NULL; cur_node = cur_node->next)
	{
		if(qsf_is_element(cur_node, map_ns, QSF_CONDITIONAL_SET)) {
			content = (char*)xmlNodeGetContent(cur_node);
			return content;
		}
	}
	return NULL;
}

/* Handles the set tag in the map.
This function will be overhauled once inside QOF
QOF hook required for "Lookup in the receiving application"
*/
static char*
qsf_set_handler(xmlNodePtr parent, GHashTable *default_hash,
	char *content, qsf_param *params)
{
	xmlNodePtr cur_node, lookup_node;

	content = NULL;
	for(cur_node = parent->children; cur_node != NULL; cur_node = cur_node->next)
	{
		if(qsf_is_element(cur_node, params->map_ns, QSF_CONDITIONAL_SET))
		{
			content = (char*)xmlGetProp(cur_node, BAD_CAST QSF_OPTION);
			if(qsf_strings_equal(xmlGetProp(cur_node, BAD_CAST QSF_OPTION), "qsf_lookup_string"))
			{
				lookup_node = (xmlNodePtr) g_hash_table_lookup(default_hash,
					xmlNodeGetContent(cur_node));
				content = (char*)xmlGetProp(lookup_node, BAD_CAST MAP_VALUE_ATTR);
				/** \todo FIXME: do the lookup. type is defined by output object. */
				/* Find by name, get GUID, return GUID as string. */
				g_message("Lookup %s in the receiving application\n", content );
				return content;
			}
			if(content)
			{
				lookup_node = (xmlNodePtr) g_hash_table_lookup(default_hash,
					xmlNodeGetContent(cur_node));
				content = (char*)xmlGetProp(lookup_node, BAD_CAST "value");
				return content;
			}
			content = (char*)xmlGetProp(parent, BAD_CAST "boolean");
			if(!content) {
				/** \todo Check qsf_parameter_hash arguments */
				lookup_node = (xmlNodePtr) g_hash_table_lookup(params->qsf_parameter_hash,
					xmlGetProp(parent->parent, BAD_CAST MAP_TYPE_ATTR));
				if(lookup_node) { return (char*)xmlNodeGetContent(lookup_node); }
				return (char*)xmlNodeGetContent(cur_node);
			}
		}
	}
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
				params->qsf_default_hash, (char*)output_content, params);
			if(output_content == NULL) {
				output_content = xmlGetProp(param_node, BAD_CAST MAP_TYPE_ATTR);
				object_data = BAD_CAST qsf_else_set_value(param_node, params->qsf_default_hash,
					(char*)output_content, params->map_ns);
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
qsf_set_format_value(xmlChar *format, char *qsf_time_now_as_string,
	xmlNodePtr cur_node, qsf_param *params)
{
	int result;
	xmlChar *content;
	time_t *output;
	struct tm *tmp;
	time_t tester;
	xmlNodePtr kl;
	regex_t reg;

	/** Comments retained - this behaves a little strangely */

	result = 0;
	if(format == NULL) { return; }
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
			printf("no suitable date set.\n");
			return;
		}
		/** Read the object value as a dateTime  */
		strptime((char*)xmlNodeGetContent(kl), QSF_XSD_TIME, tmp);
		if(!tmp) {
			printf("empty date field in QSF object.\n");
			return;
		}
		tester = mktime(tmp);
		output = &tester;
	}
	result = regcomp(&reg, "%[a-zA-Z]", REG_EXTENDED|REG_NOSUB);
	result = regexec(&reg, (char*)format,(size_t)0,NULL,0);
	if(result == REG_NOMATCH) { format = BAD_CAST "%F"; }
	regfree(&reg);
	/** QSF_DATE_LENGTH preset for all internal and QSF_XSD_TIME string formats.
	 */
	strftime(qsf_time_now_as_string, QSF_DATE_LENGTH, (char*)format, gmtime(output));
}

static void
qsf_boolean_set_value(xmlNodePtr parent, qsf_param *params,
		char *content, xmlNsPtr map_ns)
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
		printf("param_node=%s\n", param_node->name);
		if(params->boolean_calculation_done == 0) {
		/* set handler */
		output_content = BAD_CAST qsf_set_handler(param_node, params->qsf_default_hash,
				(char*)output_content, params);
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
				qsf_boolean_set_value(param_node, params, (char*)output_content, params->map_ns);
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

static xmlNodePtr
qsf_add_object_tag(qsf_param *params, int count)
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
		xmlGetProp(params->cur_node, BAD_CAST QSF_OBJECT_TYPE));
	property = xmlCharStrdup(str->str);
	xmlNewProp(extra_node, BAD_CAST QSF_OBJECT_COUNT, property);
	return extra_node;
}

void
qsf_map_object_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params)
{
	xmlNodePtr param_node, export_node;
	xmlNsPtr map_ns, qsf_ns;
	xmlChar *output_content;
	int result, count;

	map_ns = ns;
	qsf_ns = params->qsf_ns;
	param_node = NULL;
	result = 0;
	count = params->count;
	if(child == NULL) { return; }
	if(ns == NULL) { return; }
	params->boolean_calculation_done = 0;

	if(qsf_is_element(child, map_ns, MAP_CALCULATE_TAG)) {
		params->boolean_calculation_done = 0;
		for(param_node = child->children; param_node != NULL;
			param_node = param_node->next)
		{
			output_content = NULL;
			export_node = NULL;
			if(!params->lister) { params->lister = qsf_add_object_tag(params, count); }
			if(qsf_is_element(param_node, map_ns, "set"))
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
				output_content = xmlNodeGetContent(param_node);
				if(output_content != NULL) {
					output_content = xmlNodeGetContent((xmlNodePtr) g_hash_table_lookup(
						params->qsf_parameter_hash, xmlGetProp(child, BAD_CAST MAP_TYPE_ATTR)));
					if(output_content != NULL) {
						export_node = xmlAddChild(params->lister, xmlNewNode(qsf_ns,
							xmlGetProp(child, BAD_CAST QSF_OBJECT_TYPE)));
						xmlNewProp(export_node, BAD_CAST QSF_OBJECT_TYPE,
							xmlGetProp(child, BAD_CAST MAP_VALUE_ATTR));
						xmlNodeAddContent(export_node, output_content);
						xmlFree(output_content);
					}
				}
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
xmlDocPtr
qsf_object_convert(xmlDocPtr mapDoc, xmlNodePtr qsf_root, qsf_param *params)
{
	struct qsf_node_iterate iter;
	xmlDocPtr output_doc;
	xmlNode *cur_node;
	xmlNode *map_root, *output_root, *output_node;

	output_doc = xmlNewDoc(BAD_CAST QSF_XML_VERSION);
	output_root = xmlDocCopyNode(qsf_root,output_doc,2);
	xmlSetNs(output_root, params->qsf_ns);
	output_node = NULL;

	qsf_node_foreach(qsf_root, qsf_object_node_handler, &iter, params);
	map_root = xmlDocGetRootElement(mapDoc);

	iter.ns = params->map_ns;
	qsf_node_foreach(map_root, qsf_map_top_node_handler, &iter, params);

//	iter.ns = qsf_ns;
//	qsf_node_foreach(qsf_root, qsf_map_object_handler, &iter, params);
	for(cur_node = map_root->children; cur_node != NULL; cur_node = cur_node->next)
	{
		params->cur_node = cur_node;
		params->count = 0;
		if(qsf_is_element(cur_node, params->map_ns, MAP_OBJECT_TAG))
		{
			params->lister = NULL;
			params->count++;
			iter.ns = params->map_ns;
			qsf_node_foreach(cur_node, qsf_map_object_handler, &iter, params);
		}
	}
	params->file_type = OUR_QSF_OBJ;
	return output_doc;
}
