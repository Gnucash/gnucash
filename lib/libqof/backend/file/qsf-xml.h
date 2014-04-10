/***************************************************************************
 *            qsf-xml.h
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

 #ifndef QSF_XML_H
 #define QSF_XML_H

/** @file qsf-xml.h
    @brief  Private QSF header
    @author Copyright (C) 2004-2005 Neil Williams <linux@codehelp.co.uk>
*/

#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include <regex.h>
#include <time.h>
#include <libxml/xmlmemory.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlschemas.h>
#include "gnc-date.h"
#include "qof_book_merge.h"
#include "qofbook.h"
#include "qofclass.h"
#include "qofobject.h"
#include "kvp_frame.h"
#include "qofbackend-p.h"
#include "qofsession-p.h"
#include "qofbook-p.h"

#if defined(HAVE_GETTEXT)             /* HAVE_GETTEXT */

#include <libintl.h>
#include <locale.h>

#undef _
#undef Q_

#ifdef DISABLE_GETTEXT_UNDERSCORE
#define _(String) (String)
#define Q_(String) gnc_qualifier_prefix_noop(String)
#else                                 /* ENABLE_GETTEXT_UNDERSCORE */
#define _(String) gettext(String)
#define Q_(String) gnc_qualifier_prefix_gettext(String)
#endif                                /* End ENABLE_GETTEXT_UNDERSCORE */

#else                                 /* Not HAVE_GETTEXT */
#if !defined(__USE_GNU_GETTEXT)

#undef _
#undef Q_
#define _(String)       (String)
#define Q_(String) gnc_qualifier_prefix_noop(String)
#define gettext(String) (String)
#define ngettext(msgid, msgid_plural, n) (((n)==1) ? \
                                            (msgid) : (msgid_plural))

#endif                                /* End not__USE_GNU_GETTEXT */
#endif                                /* End Not HAVE_GETTEXT */

#undef  N_
#define N_(String) (String)


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
	int object_count;
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
#define QSF_DATE_LENGTH 31 /**< Max length of QSF_XSD_TIME */
#define QSF_BOOK_TAG	"book" /**< First level child: book tag - the ::QofBook. */
#define QSF_BOOK_GUID	"book-guid" /**< QOF GUID tag for the QofBook described by this QSF object file */
#define QSF_BOOK_COUNT	"count" /**< Sequential counter of each book in this file */
#define QSF_OBJECT_TAG	"object" /**< Second level child: object tag */
#define QSF_OBJECT_TYPE	"type" /**< QSF parameter name for object type specifiers */
#define QSF_OBJECT_COUNT "count" /**< Sequential counter for each QSF object in this file */
#define QSF_XML_VERSION  "1.0"  /**< The current XML version. */

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
#define MAP_DEFAULT_TAG	"default"  /**< User editable defaults for data not available within the
available QSF objects.

Some defaults will relate to how to format descriptive dates, whether discount should be considered,
which account to use for certain QSF data from applications that don't use accounts.

Some defaults are pre-defined and cannot be over-written:
- qsf_time_now
- qsf_time_string

Attributes (All are mandatory): 

\a name The text name for this default. Certain pre-defined defaults exist but user- or
map-defined defaults can have any unique text name. Spaces are \b NOT allowed, use undersccores
instead. The value of name must not duplicate any existing default, define, object or parameter
unless the special type, enum, is used.

\a type QOF_TYPE - must be one of the recognised QOF data types for the
qof_version in use or the special type, enum.

\a value Text representation of the required value. For numeric, use the format
[0-9]?/[0-9]?

\attention Using boolean defaults 

A boolean default is not output in the QSF directly, instead the value is
used in the calculations to modify certain values. If the boolean default
is set to true, the if statement containing the boolean name will be evaluated. If the boolean
default is set to false, the corresponding else will be evaluted. Make sure your
calculations contain an appropriate else statement so that the boolean value
can be adjusted without invalidating the map!

QSF deals with partial QofBooks - each object is fully described but the
book does not have to contain any specific object types or have any
particular structure. To merge partial books into usual QofBook data
sources, the map must deal with entities that need to be referenced in
the target QofBook but which simply don't exist in the QofBook used to generate
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
- enum types are the only defaults that are allowed to use the same name value more than once. 
- enum types are used to increase the readability of a QSF map.
- The enum name acts to group the enum values together - in a similar fashion to radio buttons in HTML forms. 
- enum types are used only where the QOF object itself uses an enum type. 

e.g. the tax_included enum type allows maps to use the full name of the enum value GNC_TAXINCLUDED_YES,
instead of the cryptic digit value, 1.

*/
#define MAP_OBJECT_TAG	"object" /**< Contains all the calculations to make one object from others.

Note that creating an object for the import application can involve using data from more than one
QSF object, as well as defaults and lookups in the import application itself. Conditionals, simple
arithmetic and date/time formatting options are also available.
*/
#define MAP_CALCULATE_TAG	"calculate" /**< One calculation for every parameter that needs to be set.

QSF follows the same rule as qof_book_merge. Only if a getter and a setter function are defined for
a parameter is it available to QSF. If a ::QofAccessFunc and ::QofSetterFunc are both defined
for any QofObject parameter, that parameter \b MUST be calculated in any map that defines that object.
*/
#define MAP_QOF_VERSION	"qof_version" /**< This is the QOF_OBJECT_VERSION from QOF.

QSF maps may need to be updated if QOF itself is upgraded. This setting is coded into QOF and 
maps for one version cannot necessarily be used by other versions. At the first release of QSF,
QOF_OBJECT_VERSION = 3.
*/
#define MAP_NAME_ATTR	"name" /**< The name of the default setting.

Use this name to refer to the value of this default in the map calculations.

Make sure that the type of this default matches the type of the parameter being set by the 
parent calculation!
*/
#define MAP_TYPE_ATTR	"type" /**< QSF will NOT convert between QOF types.

QSF will allow a conditional to use a parameter of one type to determine the value from a parameter of
another type, but the final value assigned \b MUST be of the same type as the parent calculation.
*/
#define MAP_VALUE_ATTR	"value" /**< The value of the tag, used in defaults and calculations.

The value of a default is a string representation of the value to be inserted into
the calculation where the default is used.

The value of a calculation is the name of the parameter that will be set by that calculation.
*/
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
readability of the map but the relevant application could also be modified to support a more 
readable set function.
*/
#define QSF_CONDITIONAL_ELSE "else" /**< Alternative

if(){} else{} is also supported. Nesting of conditionals causes problems for validating the
final map against any sensible XML Schema and a map that doesn't validate will be rejected. 
When editing conditionals in a QSF map, ALWAYS validate the map using xmllint. If necessary, 
define a variable at the foot of the definitions block, using a similar syntax to a default, 
then use that variable in another conditional

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

#define QSF_XSD_TIME  QOF_UTC_DATE_FORMAT /**< xsd:dateTime format in coordinated universal time, UTC.

You can reproduce the string from the GNU/Linux command line using the date utility: 

date -u +%Y-%m-%dT%H:%M:%SZ

2004-12-12T23:39:11Z

The datestring must be timezone independent and include all specified fields.

Remember to use gmtime() NOT localtime()!. From the command line, use the -u switch with the 
date command: date -u

To generate a timestamp based on a real time, use the qsf_time_now and qsf_time_string defaults.

qsf_time_now : Format: QOF_TYPE_DATE. The current time taken from the moment the default
is read into a QSF object at runtime.

qsf_time_string : Format: QOF_TYPE_STRING. The current timestamp taken from the moment the
default is read into a QSF object at runtime. This form is used when the output parameter 
needs a formatted date string, not an actual date object. The format is determined by the 
optional format attribute of the set tag which takes the same operators as the GNU C Library 
for strftime() and output may therefore be dependent on the locale of the calling process - 
\b take \b care. Default value is %F, used when qsf_time_string is set without the format
attribute.

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
	int count;                   /**< sequential counter for each object in the book */
	GList *qsf_object_list;      /**< list of qsf_objects */
	GSList *qsf_sequence;        /**< Parameter list sorted into QSF order */
	GList *referenceList;        /**< Table of references, ::QofEntityReference. */
	GHashTable *qsf_parameter_hash; /**< Hashtable of parameters for each object */
	GHashTable *qsf_calculate_hash, *qsf_default_hash, *qsf_define_hash;
	GSList *supported_types;     /**< The partial list of QOF types currently supported, in QSF order. */
	xmlDocPtr input_doc, output_doc; /**< Pointers to the input and output xml document(s). */
	/** \todo Review the list of xml nodes in qsf_param and rationalise. */
	xmlNodePtr child_node, cur_node, param_node, output_node, output_root, book_node, lister;
	xmlNsPtr qsf_ns, map_ns;     /**< Separate namespaces for QSF objects and QSF maps. */
	const char *qof_type;        /**< Holds details of the QOF_TYPE */
	QofIdType qof_obj_type;	     /**< current QofObject type (e_type) for the parameters. */
	QofEntity *qsf_ent;          /**< Current entity in the book. */
	QofBackend *be;              /**< the current QofBackend for this operation. */
	gboolean knowntype;          /**< detect references by comparing with known QOF types. */
	QofParam *qof_param;         /**< used by kvp to handle the frame hash table */
	QofBook *book;	             /**< the current QofBook.

		Theoretically, QSF can handle multiple QofBooks - currently limited to 1.
	*/
	int boolean_calculation_done; /**< simple trip once this boolean is complete. */
	char *filepath; /**< Path to the QSF file. */
}qsf_param;

/** \brief Free the QSF context.

Frees the two GHashTables, the GSList, the output xmlDoc
and the two xmlNs namespaces.
*/
void qsf_free_params(qsf_param *params);

/** \brief Validation metadata

The validation is a separate parse with separate data.
This may change but it currently saves workload.

\todo Examine ways of making the Validation metadata
into a sub-set of the main code, not an island on it's own.
*/
typedef struct qsf_validates
{
	QofBackendError error_state;
	const char *object_path;
	const char *map_path;
	GHashTable *validation_table;
	int valid_object_count;
	int map_calculated_count;
	int qof_registered_count;
}qsf_validator;


/** \brief shorthand function

This may look repetitive but each one is used separately
as well as in a block.
*/
int
qsf_compare_tag_strings(const xmlChar *node_name, char *tag_name);

/** \brief shorthand function

This may look repetitive but each one is used separately
as well as in a block.
*/
int
qsf_strings_equal(const xmlChar *node_name, char *tag_name);

/** \brief shorthand function

This may look repetitive but each one is used separately
as well as in a block.
*/
int
qsf_is_element(xmlNodePtr a, xmlNsPtr ns, char *c);

/** \brief shorthand function

This may look repetitive but each one is used separately
as well as in a block.
*/
int
qsf_check_tag(qsf_param *params, char *qof_type);

/** \brief Checks all incoming objects for QOF registration.

Sums all existing objects in the QSF and counts the number of those
objects that are also registered with QOF in the host application.
*/
void
qsf_object_validation_handler(xmlNodePtr child, xmlNsPtr ns, qsf_validator *valid);

/** @name Map Checks
@{
Check that the map is sufficient for this object. 

Map is usable if all input objects are defined in the object file.
Count define tags, subtract those calculated in the map (defined as objects)
Check each remaining object e_type and description against the objects
declared in the object file. Fail if some map objects remain undefined.

not finished - expect noticeable changes.

*/
void
qsf_map_validation_handler(xmlNodePtr child, xmlNsPtr ns, qsf_validator *valid);

void
qsf_map_top_node_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params);

void
qsf_map_object_handler(xmlNodePtr child, xmlNsPtr ns, qsf_param *params);
/** @} */

/** \brief Compares an xmlDoc in memory against the schema file.

@param	schema_dir  set at compile time to $prefix/share/qsf/
@param schema_filename Either the QSF Object Schema or the QSF Map Schema.
@param doc 	The xmlDoc read from the file using libxml2.

Ensure that you call the right schema_filename for the doc in question!

Incorrect validation will result in output to the terminal window.

@return TRUE if the doc validates against the assigned schema, otherwise FALSE.
*/
gboolean
qsf_is_valid(const char *schema_dir, const char* schema_filename, xmlDocPtr doc);

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

/** \brief Validate a QSF map file.

@param	params	Pointer to qsf_param context

The file is validated aginst the QSF map schema, qsf-map.xsd.xsml. This
function is called by ::is_qsf_object. If called directly, the map file
is validated and closed with a QofBackend error. QSF maps do not contain
user data and are used to import QSF object files.

@return TRUE if the map validates, otherwise FALSE.
*/
gboolean is_qsf_map_be(qsf_param *params);

/** \brief Validate a QSF file and a selected QSF map

@param	map_path	Absolute or relative path to the selected QSF map file
@param	params	Pointer to qsf_param context

The file is validated against the QSF object schema, qsf-object.xsd.xml and
each object described in the file is checked to find out if the supplied QSF
map is suitable. Map files are accepted if all objects described in the QSF object
file are defined in the QSF map.

\todo Need to code for how to find these files.


@return TRUE if the file validates and the supplied QSF map is usable,
otherwise FALSE.
*/
gboolean is_qsf_object_with_map_be(char *map_path, qsf_param *params);

gboolean is_qsf_object_with_map(const char *path, char *map_file);

/**	\brief QOF processing routine.

Called by ::qof_session_load if a map is required.
Accepts QSF_OBJECT.

Checks available QSF maps for match. Only succeeds if a suitable map exists.

*/
gboolean
load_qsf_object(QofBook *book, const char *fullpath, qsf_param *params);

/**	\brief QOF processing routine.

Called using ::qof_session_load when all objects defined in the
XML are registered in the current instance of QOF.
*/
gboolean
load_our_qsf_object(QofBook *book, const char *fullpath, qsf_param *params);

/** \brief Book and book-guid node handler.

Reads the book count="" attribute (currently only 1 QofBook is supported per QSF object file)
Sets the book-guid as the GUID of the current QofBackend QofBook in qsf_param.
Calls the next handler, qsf_object_node_handler, with the child of the book tag.
*/
void qsf_book_node_handler(xmlNodePtr child, xmlNsPtr qsf_ns, qsf_param *params);

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
void qsf_object_commitCB(gpointer key, gpointer value, gpointer data);

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
string_to_kvp_value(const char *content, KvpValueType type);

/** \brief Backend init routine.

Sets the sequence of parameters to match the schema and provide a reliable
parse. Sets the default strings for qsf_enquiry_date, qsf_time_now and
qsf_time_string.

Filters the parameter list to set each type in this order:
- QOF_TYPE_STRING
- QOF_TYPE_GUID
- QOF_TYPE_BOOLEAN
- QOF_TYPE_NUMERIC
- QOF_TYPE_DATE
- QOF_TYPE_INT32
- QOF_TYPE_INT64
- QOF_TYPE_DOUBLE
- QOF_TYPE_CHAR
- QOF_TYPE_KVP
- QOF_TYPE_COLLECT
- QOF_TYPE_CHOICE

*/
void qsf_param_init(qsf_param *params);


/** \brief map callback

Investigate ways to get the map callback to do both
the map and the validation tasks.
**/
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
gboolean is_qsf_object(const char *path);

/** \brief Validate a QSF file and determine type.

@param	path	Absolute or relative path to the file to be validated

These functions are in pairs. When called from within a QofSession, the qsf_param
context will be available. When just determining the type of file, qsf_param is
not necessary. Use the *_be functions from within the QofBackend and the 
corresponding function in other code.

The file is validated against the QSF object schema, qsf-object.xsd.xml and
each object described in the file is checked to see if it is registered
with QOF within the QOF environment of the calling process.

Files that pass the test can be imported into the QOF appliction without the need
for a QSF map.

@return TRUE if the file validates and all objects pass,
otherwise FALSE.
*/
gboolean is_our_qsf_object(const char *path);

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
gboolean is_qsf_map(const char *path);

/** \brief Determine the type of QSF and load it into the QofBook

- is_our_qsf_object, OUR_QSF_OBJ, QSF object file using only QOF objects known to the calling process.
	No map is required.
- is_qsf_object, IS_QSF_OBJ, QSF object file that may or may not have a QSF map
	to convert external objects. This temporary type will be set to HAVE_QSF_MAP if a suitable
	map exists, or an error value returned: ERR_QSF_NO_MAP, ERR_QSF_BAD_MAP or ERR_QSF_WRONG_MAP
	This allows the calling process to inform the user that the QSF itself is valid but a
	suitable map cannot be found.
- is_qsf_map, IS_QSF_MAP, QSF map file. In the backend, this generates ERR_QSF_MAP_NOT_OBJ but
	it can be used internally when processing maps to match a QSF object.

@return NULL on error, otherwise a pointer to the QofBook. Use
	the qof_book_merge API to merge the new data into the current
	QofBook. 
*/
void
qsf_file_type (QofBackend *be, QofBook *book);

void
qsf_valid_foreach(xmlNodePtr parent, qsf_validCB cb,
	struct qsf_node_iterate *iter, qsf_validator *valid);

void
qsf_node_foreach(xmlNodePtr parent, qsf_nodeCB cb,
	struct qsf_node_iterate *iter, qsf_param *params);

/** \brief Loads the QSF into a QofSession, ready to merge.
 
Loads a QSF object file containing only GnuCash objects
into a second QofSession.
 
@param first_session A QofSession pointer to the original session. This
will become the target of the subsequent qof_book_merge.

@param path	Absolute or relative path to the file to be loaded
 
@return ERR_BACKEND_NO_ERR == 0 on success, otherwise the QofBackendError
	set by the QSFBackend.
  			
\todo Build the qof_book_merge code onto this function if session loads
  properly.  	
*/
QofBackendError 
qof_session_load_our_qsf_object(QofSession *first_session, const char *path);

/** \brief Placeholder so far.

\todo Determine the map to use and convert the QOF objects
 
 Much of the map code is written but there is still work to do.
*/
QofBackendError 
qof_session_load_qsf_object(QofSession *first_session, const char *path);

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

void
qsf_object_node_handler(xmlNodePtr child, xmlNsPtr qsf_ns, qsf_param *params);

/** \brief Backend routine to write a file or stdout.

This function is used by ::qof_session_save to write any QofBook to QSF,
any process that can create a new QofSession and populate the QofBook 
with QOF objects can write the data as QSF XML - the book does not need
an AccountGroup. Remember that only fully \b QOF-compliant objects
are supported by QSF.

Your QOF objects must have:
	- a create: function in the QofObject definition
	- a foreach: function in the QofObject definition
	- QofParam params[] registered with QOF using
		qof_class_register and containing all necessary parameters
		to reconstruct this object without any further information.
	- Logical distinction between those parameters that should be
		set (have a QofAccessFunc and QofSetterFunc) and those that 
		should only be calculated (only a QofAccessFunc).

If you begin your QSF session with ::QOF_STDOUT as the book_id,
QSF will write to STDOUT - usually a terminal. This is used by QOF
applications to provide data streaming. If you don't want terminal
output, take care to check the path given to 
::qof_session_begin - don't try to change it later!

The XML is validated against the QSF object schema before being
written (to file or stdout).

Check the QofBackendError - don't assume the file is OK.

*/
void qsf_write_file(QofBackend *be, QofBook *book);

/** \brief Create a new QSF backend.
*/
QofBackend* qsf_backend_new(void);

/** @} */
/** @} */

#endif /* QSF_XML_H */
