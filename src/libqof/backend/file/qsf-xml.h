/***************************************************************************
 *            qsf-xml.h
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

 #ifndef QSF_XML_H
 #define QSF_XML_H

/** @file qsf-xml.h
    @brief  Private QSF header - not for use by applications
    @author Copyright (C) 2004-2005 Neil Williams <linux@codehelp.co.uk>
*/

#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <time.h>
#include "qof.h"

#include <libintl.h>
#define _(String) dgettext (GETTEXT_PACKAGE, String)

typedef enum  {
	QSF_UNDEF = 0, /**< Initial undefined value. */
	IS_QSF_MAP,   /**< A QSF map */
	IS_QSF_OBJ,   /**< A QSF object without a map - it may or may not need one. */
	HAVE_QSF_MAP, /**< A QSF object with the map it needs. */
	OUR_QSF_OBJ,  /**< A QSF object that can be loaded without a map. */
}qsf_type;

/** \brief Holds a description of the QofObject.

Used when converting QOF objects from another application. The incoming,
\b unknown, objects need to be stored prior to conversion. This allows 
many-to-many conversions where an invoice can receive data from an incoming
expense AND datebook and use data from an incoming contacts object to lookup
the customer for the invoice.
*/
typedef struct qsf_object_set
{
	GHashTable *parameters;
	QofIdType object_type;
	gint object_count;
}qsf_objects;

#define QSF_QOF_VERSION QOF_OBJECT_VERSION /**< QOF Version check.

Make sure the same version of QOF is in use in both applications.
*/
/** @name QSF Object XML

@{ */
#define QSF_ROOT_TAG	"qof-qsf" /**< The top level root tag */
#define QSF_DEFAULT_NS	"http://qof.sourceforge.net/" /**< Default namespace for QSF root tag

The map namespace is not included as maps are not currently written out by QOF.
*/
#define QSF_DATE_LENGTH MAX_DATE_LENGTH /**< Max length of QSF_XSD_TIME.

MAX_DATE_LENGTH itself is defined in gnc-date.h */
#define QSF_BOOK_TAG	"book"      /**< First level child: book tag - the ::QofBook. */
#define QSF_BOOK_GUID	"book-guid" /**< QOF GUID tag for the QofBook
					described by this QSF object file */
#define QSF_BOOK_COUNT	"count"     /**< Sequential counter of each book in this file */
#define QSF_OBJECT_TAG	"object"    /**< Second level child: object tag */
#define QSF_OBJECT_TYPE	"type"      /**< QSF parameter name for object type specifiers */
#define QSF_OBJECT_COUNT "count"    /**< Sequential counter for each QSF object
					in this file */
#define QSF_XML_VERSION  "1.0"      /**< The current XML version. */

/** @} */
/** @name Representing KVP as XML

&lt;kvp type="kvp" path="/from-sched-xaction" value="guid"&gt;c858b9a3235723b55bc1179f0e8c1322&lt;/kvp&gt;
A kvp type KVP parameter located at $path containing a GUID $value.

The relevance of type="kvp" won't be evident in GnuCash, they all use "kvp".

A non-GnuCash example helps:
&lt;kvp type="pilot_addr_kvp" path="/user/name" value="guid"&gt;c858b9a3235723b55bc1179f0e8c1322&lt;/kvp&gt;
A pilot_addr_kvp type KVP parameter located at /user/name containing a guid value.
@{ */

#define QSF_OBJECT_KVP   "path" /**< The path to this KVP value in the entity frame. */
#define QSF_OBJECT_VALUE "value" /**< The KVP Value. */
/** @} */
/** @name QSF Map XML

@{ */
#define MAP_ROOT_TAG     "qsf-map" /**< Top level root tag for QSF Maps */
#define MAP_DEFINITION_TAG "definition" /**< Second level container for defined objects 

Attributes: qof_version - Taken from the QOF_OBJECT_VERSION macro in QOF,
At the time of QSF development, QOF_OBJECT_VERSION is defined as 3. All
QSF maps and QSF objects must use the same qof_version which in turn must
match the QOF_OBJECT_VERSION for the QOF library in use by the calling process.

No text content allowed.
*/
#define MAP_DEFINE_TAG	"define" /**< defines each object supported by this QSF map 

Attributes: e_type Copied directly from the QofObject definition.
Content: The full QofObject description for the defined QOF object.
*/
#define MAP_ITERATE_ATTR "foreach" /**< Dictate which object type is the basis
for iteration in a hierarchical object set. */
#define MAP_DEFAULT_TAG	"default"  /**< User editable defaults for data not
available within the available QSF objects.

Some defaults will relate to how to format descriptive dates, whether discount
should be considered, which account to use for certain QSF data from applications
that do not use accounts.

Some defaults are pre-defined and cannot be over-written:
- qsf_time_now
- qsf_time_string

Attributes (All are mandatory): 

\a name The text name for this default. Certain pre-defined defaults exist but
user- or map-defined defaults can have any unique text name. Spaces are \b NOT allowed, 
use undersccores instead. The value of name must not duplicate any existing default,
define, object or parameter unless the special type, enum, is used.

\a type QOF_TYPE - must be one of the recognised QOF data types for the
qof_version in use or the special type, enum.

\a value Text representation of the required value. For numeric, use the format
[0-9]?/[0-9]?

\attention Using boolean defaults 

A boolean default is not output in the QSF directly, instead the value is
used in the calculations to modify certain values. If the boolean default
is set to true, the if statement containing the boolean name will be evaluated.
If the boolean default is set to false, the corresponding else will be evaluted.
Make sure your calculations contain an appropriate else statement so that the
boolean value can be adjusted without invalidating the map!

QSF deals with partial QofBooks - each object is fully described but the
book does not have to contain any specific object types or have any
particular structure. To merge partial books into usual QofBook data
sources, the map must deal with entities that need to be referenced in
the target QofBook but which simply do not exist in the QofBook used to generate
the QSF. e.g. pilot-link knows nothing of Accounts yet when QSF creates
a gncInvoice from qof-datebook, gncInvoice needs to know the GUID of 
certain accounts in the target QofBook. This is handled in the map 
by specifying the name of the account as a default for that map. When imported,
the QSF QofBackend looks up the object required using the name of
the parameter to obtain the parameter type. This is the only situation
where QSF converts between QOF data types. A string description of the
required object is converted to the GUID for that specific entity. The
map cannot contain the GUID as it is generic and used by multiple users.

\attention Using enumerators
- enum types are the only defaults that are allowed to use the same name value
more than once. 
- enum types are used to increase the readability of a QSF map.
- The enum name acts to group the enum values together - in a similar fashion to
radio buttons in HTML forms. 
- enum types are used only where the QOF object itself uses an enum type. 

e.g. the tax_included enum type allows maps to use the full name of the enum
value GNC_TAXINCLUDED_YES, instead of the cryptic digit value, 1.

*/
#define MAP_OBJECT_TAG	"object" /**< Contains all the calculations to make one
object from others.

Note that creating an object for the import application can involve using data
from more than one QSF object, as well as defaults and lookups in the import
application itself. Conditionals, simple arithmetic and date/time formatting 
options are also available.
*/
#define MAP_CALCULATE_TAG	"calculate" /**< One calculation for every parameter
that needs to be set.

QSF follows the same rule as qof_book_merge. Only if a getter and a setter
function are defined for a parameter is it available to QSF. If a ::QofAccessFunc 
and ::QofSetterFunc are both defined for any QofObject parameter, that parameter 
\b MUST be calculated in any map that defines that object.
*/
#define MAP_QOF_VERSION	"qof_version" /**< This is the QOF_OBJECT_VERSION from QOF.

QSF maps may need to be updated if QOF itself is upgraded. This setting is coded
into QOF and maps for one version cannot necessarily be used by other versions.
At the first release of QSF, QOF_OBJECT_VERSION = 3.
*/
#define MAP_NAME_ATTR	"name" /**< The name of the default setting.

Use this name to refer to the value of this default in the map calculations.

Make sure that the type of this default matches the type of the parameter being
set by the parent calculation!
*/
#define MAP_TYPE_ATTR	"type" /**< QSF will NOT convert between QOF types.

QSF will allow a conditional to use a parameter of one type to determine the
value from a parameter of another type, but the final value assigned \b MUST be
of the same type as the parent calculation.
*/
#define MAP_VALUE_ATTR	"value" /**< The value of the tag, used in defaults and
calculations.

The value of a default is a string representation of the value to be inserted into
the calculation where the default is used.

The value of a calculation is the name of the parameter that will be set by that
calculation.
*/
#define MAP_OBJECT_ATTR "object" /**< The object to use to provide the data
being set using the map.*/
#define MAP_E_TYPE	"e_type" /**< Validates the objects defined in the map 

The e_type will be used to match incoming QSF objects with the relevant QSF map.
The value of the e_type must be the value of the e_type for that object in the
originating QOF application. The define tag must contain the value of the description
of the same object in the same originating QOF application.
*/
/** \todo enum is an attempt to make enumerator values descriptive in the maps
and QSF (possibly). Not working yet. */
#define MAP_ENUM_TYPE "enum"

/** \brief A specific boolean default for this map.
*/
#define QSF_BOOLEAN_DEFAULT "boolean"

#define QSF_CONDITIONAL "if"  /**< child of calculate.

Conditionals can reference objects as if within the original application. In operation,
the map is overlaid across both sets of defined objects, an import object in the source 
application and an output object for the destination object. The current import and output 
QSF objects are therefore always available to the map. 
Conditionals can reference parameter as well as object values.
*/
#define QSF_CONDITIONAL_SET "set" /**< Assignment statement

Map assignments can use the native values within the output object. The output object
must support setting the relevant parameter using the value exactly as given in the map 
because the relevant set() function will be called using this value. This may reduce the 
readability of the map but the relevant application could also be modified to support
a more readable set function.
*/
#define QSF_CONDITIONAL_ELSE "else" /**< Alternative

if(){} else{} is also supported. Nesting of conditionals causes problems for
validating the final map against any sensible XML Schema and a map that does not 
validate will be rejected. When editing conditionals in a QSF map, ALWAYS 
validate the map using xmllint. If necessary, define a variable at the foot of 
the definitions block, using a similar syntax to a default, then use that 
variable in another conditional

\a variable \a name="my_rate" \a type="numeric" \a value="0/1"

The syntax for xmllint is:

\a xmllint \a --schema \a &lt;schema file&gt; \a &lt;qsf-file&gt;

Use the qsf-object.xsd.xml schema for objects and qsf-map.xsd.xml for map files.

e.g. xmllint --schema qsf-object.xsd.xml --noout qof-qsf.xml

*/
#define QSF_OPTION "option" /**< enum operator

Not implemented yet - may need to change once work starts.
Theoretically, option will specify when an enumerator value is in use -
it is quite possible that it will be unnecessary.
*/

#define QSF_FORMATTING_OPTION "format" /**< How to format dates/times

When the QSF map uses a date/time value as a \b string, the formatting
can be adjusted to personal preference. \a format will only be evaluated
if the calculated parameter is a QOF_TYPE_STRING - any format attributes
on other data types will be ignored.
 */

/** @} */

#define QSF_XSD_TIME  QOF_UTC_DATE_FORMAT /**< xsd:dateTime format in coordinated
universal time, UTC.

You can reproduce the string from the GNU/Linux command line using the date utility: 

date -u +%Y-%m-%dT%H:%M:%SZ

2004-12-12T23:39:11Z

The datestring must be timezone independent and include all specified fields.

Remember to use gmtime() NOT localtime()!. From the command line, use the
-u switch with the date command: date -u

To generate a timestamp based on a real time, use the qsf_time_now and
qsf_time_string defaults.

qsf_time_now : Format: QOF_TYPE_DATE. The current time taken from the moment
the default is read into a QSF object at runtime.

qsf_time_string : Format: QOF_TYPE_STRING. The current timestamp taken from the 
moment the default is read into a QSF object at runtime. This form is used when 
the output parameter needs a formatted date string, not an actual date object. 
The format is determined by the optional format attribute of the set tag which 
takes the same operators as the GNU C Library for strftime() and output may therefore 
be dependent on the locale of the calling process - \b take \b care. Default value 
is %F, used when qsf_time_string is set without the format attribute.

Both defaults use UTC.

*/
#define QSF_XML_BOOLEAN_TEST "true" /**< needs to be lowercase for XML validation */

#define QSF_OBJECT_SCHEMA "qsf-object.xsd.xml" /**< Name of the QSF Object Schema. */
#define QSF_MAP_SCHEMA "qsf-map.xsd.xml" /**< Name of the QSF Map Schema. */
/** \brief QSF Parameters

This struct is a catch-all for all parameters required
for various stages of the process. There are lots of elements
here that will finally be removed.
*/
typedef struct qsf_metadata
{
	qsf_type file_type;          /**< what type of file is being handled */
	qsf_objects *object_set;     /**< current object set for qsf_object_list. */
	gint count;                  /**< sequential counter for each object in the book */
	GList *qsf_object_list;      /**< list of qsf_objects */
	GSList *qsf_sequence;        /**< Parameter list sorted into QSF order */
	GList *referenceList;        /**< Table of references, ::QofInstanceReference. */
	GHashTable *qsf_parameter_hash; /**< Hashtable of parameters for each object */
	GHashTable *qsf_calculate_hash, *qsf_default_hash, *qsf_define_hash;
	GSList *supported_types;     /**< The list of QOF types currently supported, in QSF order. */
	xmlDocPtr input_doc;         /**< Pointer to the input xml document(s). */
	xmlDocPtr output_doc;        /**< Pointer to the output xml document(s). */
	xmlNodePtr child_node;       /**< The current child_node. */
	xmlNodePtr convert_node;     /**< Node in the converted object */
	xmlNodePtr param_node;       /**< Node for parameter data. */
	xmlNodePtr output_node;      /**< Node in the output document. */
	xmlNodePtr output_root;      /**< Root node of the output document. */
	xmlNodePtr book_node;        /**< Node for the book. */
	xmlNodePtr lister;           /**< Comparison node for map defaults. */
	xmlNsPtr qsf_ns, map_ns;     /**< Separate namespaces for QSF objects and QSF maps. */
	const gchar *qof_type;       /**< Holds details of the QOF_TYPE */
	QofIdType qof_obj_type;	     /**< current QofObject type (e_type) for the parameters. */
	QofIdType qof_foreach;       /**< How to iterate over hierarchical entities. */
	gint foreach_limit;          /**< How many iterations are found in the QSF */
	QofInstance *qsf_ent;          /**< Current entity in the book. */
	QofBackend *be;              /**< the current QofBackend for this operation. */
	gboolean knowntype;          /**< detect references by comparing with known QOF types. */
	QofParam *qof_param;         /**< used by kvp to handle the frame hash table */
	QofBook *book;	             /**< the current QofBook.

		Theoretically, QSF can handle multiple QofBooks - currently limited to 1.
	*/
	gint boolean_calculation_done; /**< simple trip once this boolean is complete. */
	gchar *filepath;              /**< Path to the QSF file. */
	gchar *map_path;              /**< Path to best match map, if any. */
	gchar* full_kvp_path;         /**< Full path for each KvpValue written out. */
	gint64 use_gz_level;          /**< Default compression level. */
	GList *map_files;             /**< List of selected map files for this session.

	Defaults to the pre-installed QSF maps, currently: pilot-qsf-GnuCashInvoice.xml
	*/
	const gchar *encoding;        /**< Backend encoding option - defaults to UTF-8. */ 
}qsf_param;

/** \brief Validation metadata

The validation is a separate parse with separate data.
This is used to determine which backend should load the data.
*/
typedef struct qsf_validates
{
	QofBackendError error_state;
	const gchar *object_path;
	const gchar *map_path;
	GHashTable *validation_table;
	gint valid_object_count;
	gint map_calculated_count;
	gint qof_registered_count;
}qsf_validator;

/** \brief shorthand function

This may look repetitive but each one is used separately
as well as in a block.
*/
gint
qsf_compare_tag_strings(const xmlChar *node_name, gchar *tag_name);

/** \brief shorthand function

This may look repetitive but each one is used separately
as well as in a block.
*/
gint
qsf_strings_equal(const xmlChar *node_name, gchar *tag_name);

/** \brief shorthand function

This may look repetitive but each one is used separately
as well as in a block.
*/
gint
qsf_is_element(xmlNodePtr a, xmlNsPtr ns, gchar *c);

/** \brief shorthand function

This may look repetitive but each one is used separately
as well as in a block.
*/
gint
qsf_check_tag(qsf_param *params, gchar *qof_type);

/** \brief Checks all incoming objects for QOF registration.

Sums all existing objects in the QSF and counts the number of those
objects that are also registered with QOF in the host application.
*/
void
qsf_object_validation_handler(xmlNodePtr child, xmlNsPtr ns, qsf_validator *valid);

/** \brief Compares an xmlDoc in memory against the schema file.

@param	schema_dir  set at compile time to $prefix/share/qsf/
@param schema_filename Either the QSF Object Schema or the QSF Map Schema.
@param doc 	The xmlDoc read from the file using libxml2.

Ensure that you call the right schema_filename for the doc in question!

Incorrect validation will result in output to the terminal window.

@return TRUE if the doc validates against the assigned schema, otherwise FALSE.
*/
gboolean
qsf_is_valid(const gchar *schema_dir, const gchar* schema_filename, xmlDocPtr doc);

/** \brief Prepare the default list of maps.

Prepend the default maps to the supplied GList.

The GList remains the property of the caller.
*/
GList** qsf_map_prepare_list(GList **maps);

/** \name Why two sets of functions and typedefs?

These functions are in pairs, one to use in an existing session and one to use
when deciding which backend should be selected for a new session.
- When there is an existing QofSession, the qsf_param
context will be available, so set error codes in the backend.
Use the *_be functions.
- When just determining the type of file, qsf_param is
not necessary and no backend is available (it has not been selected yet).
Use the twin function. e.g. in ::qsf_file_type()

@{
*/
/** \brief map and qsf object callback

This callback cannot do both the map and the validation tasks
because validation sometimes needs to be done without qsf_params.

e.g. when selecting which backend should be used for a particular
data source where two or more backends share the same access_method.
*/
typedef void (* qsf_nodeCB)(xmlNodePtr, xmlNsPtr, qsf_param*);
/** \brief validator callback

\todo The need for separate metadata means a separate callback typedef
	is needed for the validator, but this should be fixed to only need one.
*/
typedef void (* qsf_validCB)(xmlNodePtr, xmlNsPtr, qsf_validator*);
/** \brief One iterator, two typedefs

\todo resolve the two callbacks in ::qsf_node_iterate into one.
*/
struct qsf_node_iterate {
	qsf_nodeCB *fcn;
	qsf_validCB *v_fcn;
	xmlNsPtr ns;
};

/** \brief Validate a QSF file and identify a suitable QSF map

@param	params	Pointer to qsf_param context

These functions are in pairs. When called from within a QofSession, the qsf_param
context will be available. When just determining the type of file, qsf_param is
not necessary. Use the *_be functions from within the QofBackend and the 
corresponding function in other code.

The file is validated against the QSF object schema, qsf-object.xsd.xml and
each object described in the file is checked to find out if a suitable QSF
map exists. Map files are accepted if all objects described in the QSF object
file are defined in the QSF map.

@return TRUE if the file validates and a QSF map can be found,
otherwise FALSE.
*/
gboolean is_qsf_object_be(qsf_param *params);
/** \brief Validate a QSF file and identify a suitable QSF map

@param	path	Absolute or relative path to the file to be validated.

These functions are in pairs. When called from within a QofSession, the qsf_param
context will be available. When just determining the type of file, qsf_param is
not necessary. Use the *_be functions from within the QofBackend and the 
corresponding function in other code.

The file is validated against the QSF object schema, qsf-object.xsd.xml and
each object described in the file is checked to find out if a suitable QSF
map exists. Map files are accepted if all objects described in the QSF object
file are defined in the QSF map.

@return TRUE if the file validates and a QSF map can be found,
otherwise FALSE.
*/
gboolean is_qsf_object(const gchar *path);
/** \brief Validate a QSF file and determine type.

@param	params	Pointer to qsf_param context

The file is validated against the QSF object schema, qsf-object.xsd.xml and
each object described in the file is checked to see if it is registered
with QOF within the QOF environment of the calling process.

Files that pass the test can be imported into the QOF appliction without the need
for a QSF map.

@return TRUE if the file validates and all objects pass,
otherwise FALSE.
*/
gboolean is_our_qsf_object_be(qsf_param *params);
/** \brief Validate a QSF file.

@param	path	Absolute or relative path to the file to be validated

The file is validated against the QSF object schema, qsf-object.xsd.xml and
each object described in the file is checked to see if it is registered
with QOF within the QOF environment of the calling process.

Files that pass the test can be imported into the QOF appliction without the need
for a QSF map.

@return TRUE if the file validates and all objects pass,
otherwise FALSE.
*/
gboolean is_our_qsf_object(const gchar *path);
/** \brief Validate a QSF map file.

@param	params	Pointer to qsf_param context

The file is validated aginst the QSF map schema, qsf-map.xsd.xsml. This
function is called by ::is_qsf_object. If called directly, the map file
is validated and closed with a QofBackend error. QSF maps do not contain
user data and are used to import QSF object files.

@return TRUE if the map validates, otherwise FALSE.
*/
gboolean is_qsf_map_be(qsf_param *params);
/** \brief Validate a QSF map file.

@param	path	Absolute or relative path to the file to be validated

These functions are in pairs. When called from within a QofSession, the qsf_param
context will be available. When just determining the type of file, qsf_param is
not necessary. Use the *_be functions from within the QofBackend and the 
corresponding function in other code.

The file is validated aginst the QSF map schema, qsf-map.xsd.xsml. This
function is called by ::is_qsf_object. If called directly, the map file
is validated and closed, no data is retrieved. QSF maps do not contain
user data but are used to import QSF object files from other applications.

@return TRUE if the map validates, otherwise FALSE.
*/
gboolean is_qsf_map(const gchar *path);
/** \brief Validate a QSF file and a selected QSF map

@param	map_path	Absolute or relative path to the selected QSF map file
@param	params	Pointer to qsf_param context

The file is validated against the QSF object schema, qsf-object.xsd.xml and
each object described in the file is checked to find out if the supplied QSF
map is suitable. Map files are accepted if all objects described in the QSF object
file are defined in the QSF map.

This backend twin also sets QofBackendError codes.

@return TRUE if the file validates and the supplied QSF map is usable,
otherwise FALSE.
*/
gboolean is_qsf_object_with_map_be(gchar *map_path, qsf_param *params);
/** \brief Validate a QSF file and a selected QSF map

@param	path	Absolute or relative path to the selected QSF map file
@param	map_file	Name of the map file

The file is validated against the QSF object schema, qsf-object.xsd.xml and
each object described in the file is checked to find out if the supplied QSF
map is suitable. Map files are accepted if all objects described in the QSF object
file are defined in the QSF map.

@return TRUE if the file validates and the supplied QSF map is usable,
otherwise FALSE.
*/
gboolean is_qsf_object_with_map(const gchar *path, gchar *map_file);

/** @} */

/** \brief Book and book-guid node handler.

Reads the book count="" attribute (currently only 1 QofBook is supported per QSF object file)
Sets the book-guid as the GUID of the current QofBackend QofBook in qsf_param.
Calls the next handler, qsf_object_node_handler, with the child of the book tag.
*/
void qsf_book_node_handler(xmlNodePtr child, xmlNsPtr qsf_ns, qsf_param *params);

/** \brief Convert a string value into KvpValue

Partner to ::kvp_value_to_string. Given the type of KvpValue
required, attempts to convert the string into that type of
value.

@param content A string representation of the value, ideally as
		output by kvp_value_to_string.
@param type KvpValueType of the intended KvpValue

@return KvpValue* or NULL on failure.
*/
KvpValue*
string_to_kvp_value(const gchar *content, KvpValueType type);

/** Validate the children of the parent node.

\note Slightly different to qsf_node_foreach because
the validation can be run without qsf_param being
initialized.
*/
void
qsf_valid_foreach(xmlNodePtr parent, qsf_validCB cb,
	struct qsf_node_iterate *iter, qsf_validator *valid);

/** Iterate over the children of the parent node.

Only iterates over the immediate children of the parent -
this function is \b not recursive.
*/
void
qsf_node_foreach(xmlNodePtr parent, qsf_nodeCB cb,
	struct qsf_node_iterate *iter, qsf_param *params);

/** \brief Convert between QSF objects

This is the main workhorse of the conversion between QSF objects using
maps.

@param mapDoc The map document, parsed by libxml2.
@param qsf_root The top node of the QSF object to be converted using the map.
@param params The QSF backend parameters.

Each calculation in the map is performed over the child nodes of the 
object tree. A new xmlDoc is created and this is made available to QOF to
be loaded into the book.

*/
xmlDocPtr
qsf_object_convert(xmlDocPtr mapDoc, xmlNodePtr qsf_root, qsf_param *params);

/** Despite the name, this function handles the QSF object book tag
AND the object tags.

Used to parse object and map files.
*/
void
qsf_object_node_handler(xmlNodePtr child, xmlNsPtr qsf_ns, qsf_param *params);

/** @} */
/** @} */

#endif /* QSF_XML_H */
