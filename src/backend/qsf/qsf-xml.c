/***************************************************************************
 *            qsf-xml.c
 *
 *  Fri Nov 26 19:29:47 2004
 *  Copyright  2004-2005  Neil Williams  <linux@codehelp.co.uk>
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
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
/** @addtogroup Backend
    @{ */
/** @addtogroup QSF QOF Serialisation Format

	The qsf-xml.c source file is included in this documentation during development
	to help explain certain methods. The doxygen tags can be
	removed / set as ordinary comments once each method is finalised.

    @{ */
/** @file qsf-xml.c
    @brief  QSF Object validation, input and export.
    @author Copyright (C) 2004-2005 Neil Williams <linux@codehelp.co.uk>
*/

#define _GNU_SOURCE
#include <errno.h>

#include "libxml/xmlversion.h"
#include "qsf-dir.h"
/** The libxml pre-processor conditional has been removed
from the QOF and pilot-link versions of this file. Development
within GnuCash requires the conditional. 
uncomment this (and the conditionals that depend on it)
only for GnuCash.
To allow doxygen to process the C source
Change the doxygen config to include statics:
EXTRACT_STATIC         = YES
EXTRACT_LOCAL_CLASSES  = YES
and todo's:
GENERATE_TODOLIST      = YES
INLINE_SOURCES         = YES

if defined(LIBXML_VERSION) && LIBXML_VERSION >= 20000
*/
#include "qsf-xml.h"
#include "qofbook-p.h"

static short int module = MOD_BACKEND;

void qsf_free_params(qsf_param *params)
{
	while(params->qsf_object_list != NULL)
	{
		/** \todo Change to free the struct instead. */
/*		g_hash_table_destroy(params->qsf_parameter_hash);
		params->qsf_parameter_hash = params->qsf_object_list->data;
		params->qsf_object_list = g_list_next(params->qsf_object_list);
*/	}
	g_hash_table_destroy(params->qsf_calculate_hash);
	g_hash_table_destroy(params->qsf_default_hash);
	g_slist_free(params->supported_types);
	xmlFreeDoc(params->output_doc);
	xmlFreeNs(params->qsf_ns);
	xmlFreeNs(params->map_ns);
}

/** \brief Output

Adds the object tag and object type to the output node.

\todo Rationalise the node variable names in ::qsf_add_object_tag
*/
xmlNodePtr
qsf_add_object_tag(qsf_param *params, int count)
{
	xmlNodePtr lister;
	GString *str;
	xmlChar *property;

	str = g_string_new (" ");
	/** \todo QOF contains numerous g_string_sprintf and g_string_sprintfa calls.
	These are deprecated and should be renamed to g_string_printf and g_string_append_printf
	respectively.*/
	g_string_printf(str, "%i", count);
	lister = NULL;
	lister = xmlAddChild(params->output_node,
		xmlNewNode(params->qsf_ns, QSF_OBJECT_TAG));
	xmlNewProp(lister, QSF_OBJECT_TYPE,
		xmlGetProp(params->cur_node, QSF_OBJECT_TYPE));
	property = xmlCharStrdup(str->str);
	xmlNewProp(lister, QSF_OBJECT_COUNT, property);
	return lister;
}

int
qsf_compare_tag_strings(const xmlChar *node_name, char *tag_name)
{
	return xmlStrcmp(node_name, (const xmlChar *)tag_name);
}

int
qsf_strings_equal(const xmlChar *node_name, char *tag_name)
{
	if(0 == qsf_compare_tag_strings(node_name, tag_name)) { return 1; }
	return 0;
}

int
qsf_is_element(xmlNodePtr a, xmlNsPtr ns, char *c)
{
	g_return_val_if_fail(a != NULL, 0);
	g_return_val_if_fail(ns != NULL, 0);
	g_return_val_if_fail(c != NULL, 0);
	if ((a->ns == ns) && (a->type == XML_ELEMENT_NODE) &&
		qsf_strings_equal(a->name, c)) { return 1; }
	return 0;
}

int
qsf_check_tag(qsf_param *params, char *qof_type)
{
	return qsf_is_element(params->child_node, params->qsf_ns, qof_type);
}

gboolean
qsf_is_valid(const char *schema_dir, const char* schema_filename, xmlDocPtr doc)
{
	xmlSchemaParserCtxtPtr qsf_schema_file;
	xmlSchemaPtr qsf_schema;
	xmlSchemaValidCtxtPtr qsf_context;
	gchar *schema_path;
	gint result;

	if(doc == NULL) return FALSE;
	schema_path = g_strdup_printf("%s/%s", schema_dir, schema_filename);
	qsf_schema_file = xmlSchemaNewParserCtxt(schema_path);
	qsf_schema = xmlSchemaParse(qsf_schema_file);
	qsf_context = xmlSchemaNewValidCtxt(qsf_schema);
	result = xmlSchemaValidateDoc(qsf_context, doc);
	xmlSchemaFreeParserCtxt(qsf_schema_file);
	xmlSchemaFreeValidCtxt(qsf_context);
	xmlSchemaFree(qsf_schema);
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

	for(cur_node = child->children; cur_node != NULL;
		cur_node = cur_node->next)
	{
		if(qsf_is_element(cur_node, ns, QSF_OBJECT_TAG)) {
			object_declaration = xmlGetProp(cur_node, QSF_OBJECT_TYPE);
			g_hash_table_insert(valid->validation_table, object_declaration, xmlNodeGetContent(cur_node));
			if(TRUE == qof_class_is_registered((QofIdTypeConst) object_declaration))
			{
				valid->qof_registered_count++;
			}
		}
	}
}

/** \brief Set the QSF-specific parameter order

::qof_class_param_foreach callback.

- All string parameters, any order
- All GUID parameters, including references to parent or child objects, in any order.
- all boolean parameters, any order
- all numeric parameters, any order
- all date parameters, any order
- all int32 parameters, any order
- all int64 parameters, any order

Receive one QOF_TYPE and iterate over the current object in params
	to build a GSList of parameters in the correct sequence
	for this object and put into another GHashTable.
	One hash table containing all defined objects.
	Each value is a GSList of the parameters in the QSF sequence.
	Exclude using the qof_book_merge criterion: QofAccessFunc && QofSetterFunc
	not NULL.

	GSList *supported_types;

*/
static void
qsf_object_sequence(QofParam *qof_param, gpointer data)
{
	qsf_param *params;

	g_return_if_fail(data != NULL);
	params = (qsf_param*) data;
	if(0 == safe_strcmp(qof_param->param_type, params->qof_type))
	{
		params->qsf_sequence = g_slist_append(params->qsf_sequence, qof_param);
	}
}	

/** \brief Dictates which QOF types are supported and in which \b order

Called once for each supported type, for each object.
Filters the parameter list to set each type in order:
- QOF_TYPE_STRING
- QOF_TYPE_GUID
- QOF_TYPE_BOOLEAN
- QOF_TYPE_NUMERIC
- QOF_TYPE_DATE
- QOF_TYPE_INT32
- QOF_TYPE_INT64

*/
static void
qsf_supported_parameters(gpointer type, gpointer user_data)
{
	qsf_param *params;

	g_return_if_fail(user_data != NULL);
	params = (qsf_param*) user_data;
	params->qof_type = (QofIdType)type;
	qof_class_param_foreach(params->qof_obj_type, qsf_object_sequence, params);
}

/** \brief Export routine.

For each entity, builds the output tags for the parameters, in QSF order.
*/
static void
qsf_entity_foreach(QofEntity *ent, gpointer data)
{
	qsf_param *params;
	GSList *param_list;
	xmlNodePtr node, object_node;
	xmlNsPtr ns;
	gchar *string_buffer;
	GString *buffer;
	QofParam *qof_param;
	int param_count;
	
	g_return_if_fail(data != NULL);
	params = (qsf_param*)data;
	param_count = params->count;
	ns = params->qsf_ns;
	object_node = xmlNewChild(params->book_node, params->qsf_ns, QSF_OBJECT_TAG, NULL);
	xmlNewProp(object_node, QSF_OBJECT_TYPE, ent->e_type); 
	buffer = g_string_new(" ");
	g_string_printf(buffer, "%i", param_count);
	xmlNewProp(object_node, QSF_OBJECT_COUNT, buffer->str);
	param_list = g_slist_copy(params->qsf_sequence);
	while(param_list != NULL) {
		qof_param = param_list->data;
		g_return_if_fail(qof_param != NULL);
		if(((qof_param->param_setfcn != NULL) && (qof_param->param_getfcn != NULL)) ||
			(0 == safe_strcmp(qof_param->param_type, QOF_TYPE_GUID)))
		{
			node = xmlAddChild(object_node, xmlNewNode(ns, qof_param->param_type));
			string_buffer = g_strdup(qof_book_merge_param_as_string(qof_param, ent));
			xmlNodeAddContent(node, string_buffer);
			xmlNewProp(node, QSF_OBJECT_TYPE ,qof_param->param_name);
		}
		param_list = g_slist_next(param_list);
	}
}

/** \brief Export routine. For each registered object type, get parameters. 

Runs for each object type.
Gets the parameter list for this object type,
sorts into the QSF order and
moves on to the
next object type.
*/
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

/** \brief Export routine

	QSF only uses one QofBook per file, so far.
*/
xmlDocPtr
qofbook_to_qsf(QofBook *book)
{
	xmlNodePtr top_node, node;//, book_node;
	xmlDocPtr doc;
//	xmlChar *output;
	gchar buffer[GUID_ENCODING_LENGTH + 1];
//	gint counter;
	qsf_param *params;
	const GUID *book_guid;
	
	g_return_val_if_fail(book != NULL, NULL);
	params = g_new(qsf_param, 1);
	qsf_param_init(params);
	params->book = book;
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
	params->output_node = node;
	qof_object_foreach_type(qsf_foreach_obj_type, params);
	return params->output_doc;
}

gboolean is_our_qsf_object(const char *path)
{
	xmlDocPtr doc;
	struct qsf_node_iterate iter;
	xmlNodePtr object_root;
	qsf_validator valid;
	gint table_count;

	g_return_val_if_fail((path != NULL),FALSE);
	if(path == NULL) { return FALSE; }
	doc = xmlParseFile(path);
	if(doc == NULL)  { return FALSE; }
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, doc)) 
	{
		return FALSE;
	}
	object_root = xmlDocGetRootElement(doc);
	valid.validation_table = g_hash_table_new(g_str_hash, g_str_equal);
	valid.qof_registered_count = 0;
	iter.ns = object_root->ns;
	qsf_valid_foreach(object_root, qsf_object_validation_handler, &iter, &valid);
	table_count = g_hash_table_size(valid.validation_table);
	if(table_count == valid.qof_registered_count)
	{
		g_hash_table_destroy(valid.validation_table);
		return TRUE;
	}
	g_hash_table_destroy(valid.validation_table);
	return FALSE;
}

gboolean is_qsf_object(const char *path)
{
	xmlDocPtr doc;

	g_return_val_if_fail((path != NULL),FALSE);
	if(path == NULL) { return FALSE; }
	doc = xmlParseFile(path);
	if(doc == NULL) { return FALSE; }
	if(TRUE != qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, doc)) { return FALSE; }
	/** \todo implement a way of finding more than one map */
	/** \todo set the map xmlDocPtr in params for later processing. */
	return TRUE;
}

gboolean is_our_qsf_object_be(qsf_param *params)
{
	xmlDocPtr doc;
	struct qsf_node_iterate iter;
	xmlNodePtr object_root;
	qsf_validator valid;
	gint table_count;
	char *path;

	g_return_val_if_fail((params != NULL),FALSE);
	path = g_strdup(params->filepath);
	if(path == NULL) {
		qof_backend_set_error(params->be, ERR_FILEIO_FILE_NOT_FOUND);
		return FALSE;
	}
	if(params->file_type != QSF_UNDEF) { 
		return FALSE; 
	}
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
	xmlDocPtr doc;
	char *path;

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
	/** \todo implement a way of finding more than one map */
	/** \todo set the map xmlDocPtr in params for later processing. */
	return is_qsf_object_with_map_be("pilot-qsf-GnuCashInvoice.xml", params);
}

static void
qsf_supported_data_types(gpointer type, gpointer user_data)
{
	qsf_param *params;

	g_return_if_fail(user_data != NULL);
	g_return_if_fail(type != NULL);
	params = (qsf_param*) user_data;
	if(qsf_is_element(params->param_node, params->qsf_ns, (char*)type))
	{
		g_hash_table_insert(params->qsf_parameter_hash,
			xmlGetProp(params->param_node, QSF_OBJECT_TYPE), params->param_node);
	}
}

static void
qsf_parameter_handler(xmlNodePtr child, xmlNsPtr qsf_ns, qsf_param *params)
{

	params->param_node = child;
	g_slist_foreach(params->supported_types, qsf_supported_data_types, params);
}


/** \brief Handles the QSF object file

Despite the name, this function handles the QSF object book tag
\b AND the object tags.

\todo FIXME: rationalise?

Stores details of the object data for map processing later.

Need to work on only one object at a time
	Each new object, create a new GHashTable in the GList.
	The GHashTable contains all the object data, 
	indexed by the type attribute.
*/
static void
qsf_object_node_handler(xmlNodePtr child, xmlNsPtr qsf_ns, qsf_param *params)
{
	struct qsf_node_iterate iter;
	qsf_objects *object_set;
	char *tail, *object_count_s;
	int c;

	g_return_if_fail(child != NULL);
	g_return_if_fail(qsf_ns != NULL);
	params->qsf_ns = qsf_ns;
	if(qsf_is_element(child, qsf_ns, QSF_OBJECT_TAG)) {
		params->qsf_parameter_hash = NULL;
		object_set = g_new(qsf_objects, 1);
		params->object_set = object_set;
		object_set->parameters = g_hash_table_new(g_str_hash, g_str_equal);
		object_set->object_type = g_strdup(xmlGetProp(child, QSF_OBJECT_TYPE));
		object_count_s = g_strdup(xmlGetProp(child, QSF_OBJECT_COUNT));
		c = (int)strtol(object_count_s, &tail, 0);
		g_free(object_count_s);
		params->qsf_object_list = g_list_prepend(params->qsf_object_list, object_set);
		iter.ns = qsf_ns;
		params->qsf_parameter_hash = object_set->parameters;
		qsf_node_foreach(child, qsf_parameter_handler, &iter, params);
	}
}

/** \brief Book and book-guid node handler.

Reads the book count="" attribute (currently only 1 QofBook is supported per QSF object file)
Sets the book-guid as the GUID of the current QofBackend QofBook in qsf_param.
Calls the next handler, qsf_object_node_handler, with the child of the book tag.
*/
void
qsf_book_node_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params)
{
	char *book_count_s, *tail;
	int book_count;
	xmlNodePtr child_node;
	struct qsf_node_iterate iter;
	gchar *buffer;
	GUID book_guid;

	if(qsf_is_element(child, ns, QSF_BOOK_TAG)) {
		book_count_s = xmlGetProp(child,QSF_BOOK_COUNT);
		if(book_count_s) {
			book_count = (int)strtol(book_count_s, &tail, 0);
			/* More than one book not currently supported. */
			g_return_if_fail(book_count == 1);
		}
		iter.ns = ns;
		qsf_node_foreach(child, qsf_object_node_handler, &iter, params);
	}
	for(child_node = child->children; child_node != NULL;
		child_node = child_node->next)
		{
		if(qsf_is_element(child_node, ns, QSF_BOOK_GUID)) {
			buffer = g_strdup(xmlNodeGetContent(child_node));
			g_return_if_fail(TRUE == string_to_guid(buffer, &book_guid));
			qof_entity_set_guid((QofEntity*)params->book, &book_guid);
			g_free(buffer);
		}
		if(qsf_is_element(child_node, ns, QSF_OBJECT_TAG)) {
			iter.ns = ns;
			qsf_node_foreach(child_node, qsf_object_node_handler, &iter, params);
		}
	}
}

/** \brief Commit the QSF object data to a new QofBook.

The parentage of qof_book_merge should be obvious in this function.

Large chunks were just lifted directly from the commit loop and adjusted
to obtain the data to commit from the xmlNodePtr instead of qof_book_mergeRule. If
anything, it's easier here because all entities are new, there are no targets.

Unlike qof_book_merge, this routine runs once per parameter within a loop
that iterates over objects - it does not have a loop of it's own.

All entities are new.

Using the parent of the current node to 
retrieve the type parameter of the parent provides the type parameter of
the object tag - the e_type of the current QofObject which allows 
qof_class_get_parameter_setter(obj_type, key);

@param	key		name of the parameter: QofIdType
@param	value	xmlNodePtr value->name == QOF_TYPE, content(value) = data to commit.
@param	data	qsf_param* - inevitably.

*/
void
qsf_object_commitCB(gpointer key, gpointer value, gpointer data)
{
	qsf_param 		*params;
	qsf_objects		*object_set;
	xmlNodePtr		node;
//	QofEntity 		*referenceEnt;
//	GSList 			*linkage;
	const char		*qof_type, *parameter_name;
	QofIdType		obj_type;
	QofEntity		*qsf_ent;
	struct tm		qsf_time;
	time_t			qsf_time_t;
	char			*tail;
	/* function pointers and variables for parameter getters that don't use pointers normally */
	gnc_numeric 	cm_numeric;
	double 			cm_double;
	gboolean 		cm_boolean;
	gint32 			cm_i32;
	gint64 			cm_i64;
	Timespec 		cm_date;
	/* cm_ prefix used for variables that hold the data to commit */
	gchar 			/**cm_string,*/ *cm_char;
	GUID 			*cm_guid;
//	KvpFrame 		*cm_kvp;
	QofSetterFunc 	cm_setter;
	void	(*string_setter)	(QofEntity*, const char*);
	void	(*date_setter)		(QofEntity*, Timespec);
	void	(*numeric_setter)	(QofEntity*, gnc_numeric);
//	void	(*guid_setter)		(QofEntity*, const GUID*);
	void	(*double_setter)	(QofEntity*, double);
	void	(*boolean_setter)	(QofEntity*, gboolean);
	void	(*i32_setter)		(QofEntity*, gint32);
	void	(*i64_setter)		(QofEntity*, gint64);
	void	(*char_setter)		(QofEntity*, char*);
//	void	(*kvp_frame_setter)	(QofEntity*, KvpFrame*);
//	void	(*reference_setter)	(QofEntity*, QofEntity*);
	
	g_return_if_fail(data != NULL);
	g_return_if_fail(value != NULL);
	params = (qsf_param*)data;
	node = (xmlNodePtr)value;
	parameter_name = (const char*)key;
	
	qof_type = node->name;
	qsf_ent = params->qsf_ent;
	obj_type = xmlGetProp(node->parent, QSF_OBJECT_TYPE);
	if(0 == safe_strcasecmp(obj_type, parameter_name)) { return; }
	cm_setter = qof_class_get_parameter_setter(obj_type, parameter_name);
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
		g_return_if_fail(TRUE == string_to_guid(xmlNodeGetContent(node), cm_guid));
		qof_entity_set_guid(qsf_ent, cm_guid);
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
/*		if(safe_strcmp(qof_type, QOF_TYPE_KVP) == 0) { 
			cm_kvp = kvp_frame_copy(cm_param->param_getfcn(rule->importEnt,cm_param));
			kvp_frame_setter = (void(*)(QofEntity*, KvpFrame*))cm_param->param_setfcn;
			if(kvp_frame_setter != NULL) { kvp_frame_setter(rule->targetEnt, cm_kvp); }
			registered_type = TRUE;
		}
*/
	if(safe_strcmp(qof_type, QOF_TYPE_CHAR) == 0) { 
		cm_char = xmlNodeGetContent(node);
		char_setter = (void(*)(QofEntity*, char*))cm_setter;
		if(char_setter != NULL) { char_setter(qsf_ent, cm_char); }
	}
/*		if(registered_type == FALSE) {
			linkage = g_slist_copy(rule->linkedEntList);
			referenceEnt = NULL;
			reference_setter = (void(*)(QofEntity*, QofEntity*))cm_param->param_setfcn;
			if((linkage == NULL)&&(rule->mergeResult == MERGE_NEW)) {
				referenceEnt = cm_param->param_getfcn(rule->importEnt, cm_param);
				reference_setter(rule->targetEnt, qof_book_mergeLocateReference(referenceEnt, mergeData));
			}
			while(linkage != NULL) {
				referenceEnt = linkage->data;
				if((referenceEnt)
					&&(referenceEnt->e_type)
					&&(safe_strcmp(referenceEnt->e_type, qof_type) == 0)) {
					// The function behind reference_setter must create objects for any non-QOF references
					reference_setter(rule->targetEnt, qof_book_mergeLocateReference(referenceEnt, mergeData));
				}
				linkage = g_slist_next(linkage);
			}
		}
*/
}
