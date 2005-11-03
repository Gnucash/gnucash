/***************************************************************************
 *            qof-main.h
 *
 *  Thu Jan 13 12:15:41 2005
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
/** @addtogroup QOFCLI Query Object Framework Command Line Interface and shell.

The CLI uses a top level shell and two sub shells. The top level shell provides
general interactivity with object types, entire books and SQL support.

The select sub-shell identifies a single instance of an object. e.g. the print 
command in the top shell uses the select subshell to locate the instance to be
printed and the delete command, the instance to be deleted. These commands then
return to the top shell.

The edit sub-shell allows data to be set in a selected instance. The edit command
uses a select sub-shell to identify the instance then changes to an edit sub-shell
to handle setting the individual parameters of that instance. Before returning to the
top shell, the edited data can be saved to the instance using 'commit' or the edit can
be aborted using 'quit'. 

The add command creates a new instance and passes that instance, already selected,
to the edit sub-shell for data entry.

\note CashUtil relies on installed versions of the QOF, libcashobjects and
libgnc-backend-file libraries. If you change any source files for these libraries,
ensure you run 'make install' rather than just 'make' or your changes will have no
effect. There is no support for loading local or 'test' versions of the libraries.
You can usually run cashutil against a freshly installed set of QOF libraries without
recompiling cashutil, depending on the level of changes in QOF.

@{
*/
/** @file qof-main.h
  @brief Common functions for the QOF external framework
  @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/

#define _GNU_SOURCE
#include "config.h"
#ifndef _QOF_MAIN_H
#define _QOF_MAIN_H
#include <popt.h>
#include "qof.h"
#include "qofundo.h"

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

/** gnc file backend library name */
#define GNC_LIB_NAME "libgnc-backend-file.la"
/** init_fcn for gnc file backend library. */
#define GNC_LIB_INIT "gnc_provider_init"

#define CU_MOD_ENGINE "cashutil-engine"

gpointer qof_backend_extension(const char* IDstring);

/** \name Control functions.
@{
*/
/** \brief List of all parameters for this object of one type.

Return a GSList of all parameters of this object that are a
particular QOF type, QOF_TYPE_STRING, QOF_TYPE_BOOLEAN etc.

The returned GSList should be freed by the caller.

\note The return list is a singly linked list - GSList -
\b not the doubly-linked list - GList - returned by
::qof_class_get_referenceList.

\param object_type  object->e_type for the relevant object.
\param param_type  The type of parameter to match, QOF_TYPE_STRING etc.

\return GSList of all matching parameters or NULL if none exist.
*/
GSList*
qof_get_param_list(QofIdTypeConst object_type, QofType param_type);

#define QOF_DATE_STRING_LENGTH  31 /**< Inherited from QSF */
#define QOF_UTC_DATE_FORMAT     "%Y-%m-%dT%H:%M:%SZ" /**< Inherited from QSF */

/**  \brief The SQL commands supported by QOF

A regular expression used to exclude unsupported commands
from SQL files. Anything that does \b not match the expression
will be silently ignored by cashutil. This allows genuine
SQL dump files to be parsed by cashutil without errors.

 A QOF object is similar to a definition of a SQL table.\n
 A QOF entity is similar to an instance of a SQL record.\n
 A QOF parameter is similar to data in a SQL field.

Certain SQL commands have no QOF equivalent and should
always be ignored silently:
 - ALTER (the object parameters cannot be changed at runtime)
 - CREATE (new tables - new objects - cannot be created at runtime)
 - DROP  (an object cannot be "de-registered" without re-compiling)
 - FLUSH (QOF has no permissions system)
 - GRANT
 - KILL
 - LOCK
 - OPTIMIZE
 - REVOKE
 - USE (QOF only has one database, itself.)
*/
#define QOF_SQL_SUPPORTED  "^SELECT|INSERT"

/** Indent and pad the shell output nicely.*/
#define QOF_SHELL_FORMAT "    %-30s%s"

/** \brief Common QOF CLI options

 * These are definitions for popt support in the CLI. Every program's
 * popt table should start with QOF_CLI_OPTIONS to insert
 * the standard options into it. Also enables autohelp.
 */
#define QOF_CLI_OPTIONS POPT_AUTOHELP \
	{"list", 'l', POPT_ARG_NONE, NULL, qof_op_list, \
	 _("List all databases supported by the current QOF framework and exit."), \
	 NULL}, \
	{"explain", 0, POPT_ARG_NONE, NULL, qof_op_explain, \
	 _("List the fields within the specified database and exit, requires -d."), \
	 NULL}, \
	{"date", 't', POPT_ARG_STRING, &date_time, qof_op_timespec, \
	 _("Shorthand to only query objects that contain the specified date."), \
	 "string"}, \
	{"database", 'd', POPT_ARG_STRING, &database, qof_op_database, \
	 _("Shorthand to only query objects within a specific supported database. "), \
	 "string"}, \
	{"exclude", 'e', POPT_ARG_STRING, &exclude, qof_op_exclude, \
	 _("Shorthand to exclude a supported database from the query."), \
	 "string"}, \
	{"sql", 's', POPT_ARG_STRING, &sql_query, qof_op_sql, \
	 _("Specify a SQL query on the command line."), "string"}, \
	{"sql-file", 'f', POPT_ARG_STRING, &sql_file, qof_op_sql_file, \
	 _("Specify one or more SQL queries contained in a file."), \
	 "filename"}, \
	{"write", 'w', POPT_ARG_STRING, &write_file, qof_op_write, \
	 _("Write the results of any query to the file"), "filename"}, \
	{"compress", 0, POPT_ARG_INT, &gz_level, qof_op_compress, \
	 _("Compress output files, 0 for none, 9 for maximum"), "integer"}, \
	{"debug", 0, POPT_ARG_NONE, NULL, qof_op_debug, \
	 _("Print debugging information to a temporary file."), NULL}, \
	{"shell", 0, POPT_ARG_NONE, NULL, qof_op_shell, \
	 _("Enter the QOF interactive shell"), NULL}, \
	{"version", 0, POPT_ARG_NONE, NULL, qof_op_vers, \
	 _("Display version information"), NULL},

/** \brief Output error messages from QOF

QOF will set errors in the QofSession. The
application determines how to output those
messages and for CLI programw, this will be to
stderr. Some of these error messages are not used in
all CLI programs.
*/
void qof_show_error(QofSession *session, const char *file);

/** \brief Handle the type of each subshell. */
typedef enum {
	NO_SHELL,
	TOP_SHELL,     /**< the first, top level shell. */
	EDIT_SHELL,    /**< Edit the selected instance */
}qof_subshell;

typedef enum {
	NO_OP,
	PRINT_MODE,
	DELETE_MODE,
	EDIT_MODE,
}qof_cli_mode;

/** \brief The QOF CLI context struct */
typedef struct qofdata_s {
	gchar *filename;            /**< Input filename containing QSF XML, if any.*/
	gchar *write_file;          /**< Export filename, if any.*/
	gchar *sql_file;            /**< SQL file, if any. */
	gchar *sql_str;             /**< The current SQL, overwritten each iteration if using a file.*/
	gchar *database;            /**< The database to include with -d. */
	gchar *exclude;             /**< The database to exclude with -e. */
	gchar *shortname;           /**< A shortname for this program if truncation to first 4 characters is not suitable. */
	Timespec min_ts;            /**< Matches objects above minimum time_t value. */
	Timespec max_ts;            /**< Matches objects below maximum time_t value. */
	QofSession *input_session;  /**< The input session. */
	QofSession *export_session; /**< The query results session, for STDOUT or -w. */
	gboolean error;             /**< general error, abort. */
	GList *all_objects;         /**< List of all supported databases. */
	GList *sql_list;            /**< List of sql commands from a file. */
	int argc;                   /**< Shell copy of argc */
	char **argv;                /**< Shell copy of commands */
	QofBook *book;              /**< the current book for the shell function. */
	qof_subshell shell_type;    /**< the type of subshell, top or edit */
	qof_cli_mode cli_mode;          /**< current operation mode. */
	QofIdTypeConst inst_type;   /**< The current registered QofObject type. */
	QofInstance *instance;      /**< The currently selected instance. */
	gint counter;
	GHashTable *select_table;
	gint gz_level;
}qof_data;

/** \brief Register all QOF objects.

If new objects are added, call the register func()
here.

qof_init must be called by any program wanting to
use the QOF framework with GnuCash objects.

\return A usable qof_data* context.
*/
void  qof_init (void);

/** \brief initialise the QOF CLI context. 

All QOF CLI programs must create a context.
*/
qof_data* qof_create(void);

/** \brief Shutdown the QOF framework
*/
void qof_close(void);

/* \brief Clear up the qof_data context */
void qof_data_free(qof_data *data);

/** \brief Check that the SQL command is supported.*/
gboolean qof_check_sql(const char *sql);

/** \enum qof_op_type

main operator enum
*/
/** \enum qof_op_type::qof_op_noop

undefined check value
*/
/**\enum qof_op_type::qof_op_input

execute input command
*/
/** \enum qof_op_type::qof_op_list

List supported databases command.
*/

/** \brief command line command options.*/
typedef enum {
	qof_op_noop = 0,
	qof_op_input,
	qof_op_list,
	qof_op_shell,
	qof_op_vers,
	qof_op_database,
	qof_op_timespec,
	qof_op_exclude,
	qof_op_sql,
	qof_op_sql_file,
	qof_op_write,
	qof_op_explain,
	qof_op_compress,
	qof_op_debug
}qof_op_type;

/** \brief Build a list of all available objects */
void qof_select_all(QofObject *obj, gpointer data);

/** @} */
/** @name Command handlers.
@{
*/

/** \brief load the QOF interactive shell

Where available, uses READLINE to store a history of
previous shell commands.
*/
void qof_cmd_shell(qof_data *context);

/** \brief List each parameter for the selected object. */
void qof_cmd_explain (gpointer user_data);

/** \brief List the supported databases.

Uses a callback to ::qof_class_is_registered.
*/
void qof_cmd_list (void);

/** \brief query a QSF XML file

Query the QSF XML in <filename>.
*/
void qof_cmd_offline (qof_data *context);

/** \brief Lists all databases supported by the current QOF framework.

Prints the name and description for each object type
registered with this instance of QOF. No options are used.
*/
void qof_cmd_list (void);
/** @} */

/** @name Command modulators.
@{
*/

/** \brief Shorthand to only query objects within one specific supported database.

Used to only query objects within the specified
database. In a hotsync, all other supported databases are skipped
and data is only read from the named database. Without a HotSync (using
offline storage), only objects of this type are queried.
*/
void qof_mod_database (const char *database, qof_data *data);

/** \brief Shorthand to only query objects that contain the specified date.

Used to modify the QOF query to only query objects that contain
at least one parameter containing a QOF_TYPE_DATE that
matches the range specified. Dates need to be specified as YY-MM-DD.

You can specify a UTC timestring, just as normally output by QSF,
but the time will not be matched when using the shorthand option,
only the year, month and day.

For more precise time matches or to set a defined period that doesn't follow
whole calendar months, (e.g. the UK financial year) use a SQL statement:

Partial matches are allowed, so YY-MM matches
any object where a date is within the specified month and year,
YY matches any object where a date is within the specified year.

The query range starts at midnight on the first day of the range
and ends at 1 second to midnight on the last day of the range.
*/
void qof_mod_timespec (const char *date_time, qof_data *data);

/** \brief Shorthand to exclude a supported database from the query.

Excludes the (single) specified database from the query.
During a hotsync, data in that database is not read from the Palm.
When working offline, the objects of that type are not queried.
*/
void qof_mod_exclude (const char *exclude, qof_data *data);

/** \brief Specify a SQL query on the command line.

For SELECT, the returned list is a list of all of the instances of 'SomeObj' that
match the query. The 'SORT' term is optional. The 'WHERE' term is optional; but
if you don't include 'WHERE', you will get a list of all of the object instances.
The Boolean operations 'AND' and 'OR' together with parenthesis can be used to construct
arbitrarily nested predicates.

For INSERT, the returned list is a list containing the newly created instance of 'SomeObj'.

Date queries handle full date and time strings, using the format exported by the QSF
backend. To query dates and times, convert user input into UTC time using the
QOF_UTC_DATE_FORMAT string. e.g. set the UTC date format and call qof_print_time_buff
with a time_t obtained via timespecToTime_t.

If the param is a KVP frame, then we use a special markup to indicate frame values.
The markup should look like /some/kvp/path:value. Thus, for example,\n
SELECT * FROM SomeObj WHERE (param_a < '/some/kvp:10.0')\n
will search for the object where param_a is a KVP frame, and this KVP frame contains
a path '/some/kvp' and the value stored at that path is floating-point and that float
value is less than 10.0.

@param sql_query Examples:

"select * from gncAddress"

*/
void qof_mod_sql (const char *sql_query, qof_data *data);

/** \brief Specify one or more SQL queries contained in a file.

The rules for single SQL commands also apply with regard to the lack of explicit
support for joins and the pending support for selecting only certain parameters
from a certain object.

See ::qof_mod_sql for information on the queries supported.

\note Where possible, this function uses the safer GNU extension: getline().
On Mac OSX and other platforms that do not provide getline, the call uses
the less reliable fgets(). If the input file contains a NULL, fgets will
get confused and the read may terminate early on such platforms.\n
http://www.gnu.org/software/libc/manual/html_node/Line-Input.html

*/
void qof_mod_sql_file (const char *sql_file, qof_data *data);

/** \brief Write the results of any query to the file

Sets the \a filename of the file to be written out using 
the QSF XML QofBackend.

*/
void qof_mod_write (const char *write_file, qof_data *data);

void extensions_init(void);

void qof_backend_extension_add(char *IDstring, gpointer data);

gpointer qof_backend_extension(const char* IDstring);


/** @} */
/** @} */

#endif				/* _QOF_MAIN_H */
