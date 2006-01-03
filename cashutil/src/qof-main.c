/***************************************************************************
 *            qof-main.c
 *
 *  This is an auto-generated file. Patches are available from
 *  http://qof-gen.sourceforge.net/
 *
 *  Thu Jan 13 10:55:44 2005
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#define _GNU_SOURCE
#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include "qof-main.h"
/** \todo temporary until undo goes into QofBook */
#include "qofundo-p.h"

static QofLogModule log_module = QOF_MAIN_CLI;

struct param_ref_list
{
	GSList *slist;
	QofType param_type;
	int i;
};

static void
find_param_cb(QofParam *param, gpointer user_data)
{
	struct param_ref_list *b;
	char *buf;

	b = (struct param_ref_list*)user_data;
	if((param->param_getfcn == NULL)||(param->param_setfcn == NULL)) { return; }
	if(0 == safe_strcmp(b->param_type, param->param_type))
	{
		b->i++;
		buf = g_strdup(param->param_name);
		if(buf != NULL) {
			b->slist = g_slist_append(b->slist, buf);
		}
		return;
	}
}

GSList*
qof_main_get_param_list(QofIdTypeConst object_type, QofType param_type)
{
	GSList *param_list;
	char *i;
	struct param_ref_list p;

	g_return_val_if_fail(object_type != NULL, NULL);
	param_list = NULL;
	p.slist = NULL;
	p.i = 0;
	p.param_type = g_strdup(param_type);
	qof_class_param_foreach(object_type, find_param_cb, &p);
	param_list = g_slist_copy(p.slist);
	i = g_strdup(object_type);
	return param_list;
}

void
qof_main_free (qof_main_context *context)
{
	g_free(context->filename);
	g_free(context->write_file);
	g_free(context->sql_file);
	g_free(context->database);
	g_free(context->category);
}

static void
qof_main_run_sql (qof_main_context *context)
{
	QofSqlQuery *q;
	gchar *sql;

	q = qof_sql_query_new();
	sql = g_strdup(context->sql_str);
	qof_sql_query_parse(q, sql);
	context->query = qof_sql_query_get_query(q);
}

static void
qof_main_run_query(qof_main_context *context)
{
	QofBook *book;
	GList *results;

	book = qof_session_get_book(context->input_session);
	qof_query_set_book(context->query, book);
	results = qof_query_run (context->query);
	if(results != NULL) {
		qof_entity_copy_list(context->export_session, results);
	}
}

/** takes one database name and runs -c and -t queries against it. */
static void 
build_database_list(QofIdTypeConst obj_type, qof_main_context *context)
{
	GSList *date_param_list, *category_param_list;
	QofQueryPredData *date_pred_data;
	QofQueryPredData *category_pred;
	QofIdTypeConst find;
	Timespec min_ts;
	Timespec max_ts;

    if(!obj_type || !context) { return; }
    context->query = qof_query_create_for(obj_type);
    find = qof_query_get_search_for(context->query);
	if(context->category != NULL) {
		category_param_list = qof_query_build_param_list(CATEGORY_NAME, NULL);
		category_pred = qof_query_string_predicate(QOF_COMPARE_EQUAL, 
			context->category, QOF_STRING_MATCH_CASEINSENSITIVE, FALSE);
        qof_query_add_term(context->query, category_param_list,
            category_pred, QOF_QUERY_AND);
	}
	if(context->min_ts.tv_sec > 0) {
		min_ts = context->min_ts;
		max_ts = context->max_ts;
		date_param_list = g_slist_copy(qof_main_get_param_list(find, QOF_TYPE_DATE));
		if(!date_param_list) 
		{
            if(context->query) { qof_query_clear(context->query); }
            PINFO (" no date_param_list");
			return;
	}
	date_pred_data = qof_query_date_predicate(QOF_COMPARE_GTE,
		   QOF_DATE_MATCH_NORMAL, min_ts);
	qof_query_add_term(context->query, date_param_list,
		   date_pred_data, QOF_QUERY_AND);
	date_param_list = qof_main_get_param_list(
		   qof_query_get_search_for(context->query), QOF_TYPE_DATE);
	date_pred_data = qof_query_date_predicate(QOF_COMPARE_LTE,
		   QOF_DATE_MATCH_NORMAL, max_ts);
	qof_query_add_term(context->query, date_param_list,
		   date_pred_data, QOF_QUERY_AND);
	}
	qof_main_run_query(context);
	if(context->query) { qof_query_clear(context->query); }
}

static void
select_cb(QofObject *obj, gpointer data)
{
	qof_main_context *context;
	
	context = (qof_main_context*)data;
	g_return_if_fail(context);
	if(0 != safe_strcmp(context->exclude, obj->e_type))
	{
		build_database_list(obj->e_type, context);
	}
}

void
qof_main_moderate_query(qof_main_context *context)
{
	GSList *date_param_list, *category_param_list;
	gboolean all;

	all = TRUE;
    context->query = qof_query_create();
	date_param_list = NULL;
	category_param_list = NULL;
	while (context->sql_list)
	{
		context->sql_str = g_strdup(context->sql_list->data);
		qof_main_run_sql(context);
		qof_main_run_query(context);
		if(context->query) { qof_query_clear(context->query); }
		g_free(context->sql_str);
		all = FALSE;
		context->sql_list = g_list_next(context->sql_list);
	}
	if(0 < g_list_length(context->sql_list)) { 
		context->sql_str = NULL;
		g_list_free(context->sql_list);
		all = FALSE;
	}
	if(context->sql_str != NULL) {
		qof_main_run_sql(context);
		qof_main_run_query( context);
		if(context->query) { qof_query_clear(context->query); }
		all = FALSE;
	}
	if((context->exclude != NULL)&&(qof_class_is_registered(context->exclude)))
	{
		qof_object_foreach_type(select_cb, context);
		all = FALSE;
	}
	if((context->database != NULL)&&(qof_class_is_registered(context->database)))
	{
		build_database_list(context->database, context);
		all = FALSE;
	}
	if(all == TRUE)
	{
		qof_object_foreach_type(select_cb, context);
	}
}

static void option_cb (QofBackendOption *option, gpointer data)
{
	gint gz_level;

	gz_level = GPOINTER_TO_INT(data);
	if(0 == safe_strcmp(QSF_COMPRESS, option->option_name)) {
		option->value = (gpointer)&gz_level;
	}
}

void
qof_mod_compression (gint gz_level, qof_main_context *context)
{
	KvpFrame *be_config;
	QofBook *book;
	QofBackend *be;

	if((gz_level > 0) && (gz_level <= 9))
	{
		ENTER (" ");
		book = qof_session_get_book(context->export_session);
		be = qof_book_get_backend(book);
		be_config = qof_backend_get_config(be);
		qof_backend_option_foreach(be_config, option_cb, &gz_level);
		qof_backend_load_config(be, be_config);
		LEAVE (" ");
	}
}

void
qof_cmd_xmlfile (qof_main_context *context)
{
	QofSession *input_session, *export_session;
	gchar current_work[PATH_MAX];
	gchar *temp;
	QofBackend *be;
	QofBook *book;
	KvpFrame *backend_config;

	backend_config = NULL;
	input_session = context->input_session;
	if(0 == safe_strcmp(context->exclude, context->database)
		&&(context->exclude != NULL))
	{
		fprintf(stderr, _("%s: Error: Cannot exclude database \"%s\" with option -e\n"
		"    because option -d is set to the include the same database: \"%s\"\n"
		"Use the \'-l\' command to see the full list of supported databases.\n"),
			PACKAGE, context->exclude, context->database);
		qof_session_end(input_session);
		return;
	}
	qof_session_begin(input_session, context->filename, FALSE, TRUE);
	qof_session_load(input_session, NULL);
	if(ERR_BACKEND_LOCKED == qof_session_get_error(input_session))
	{
		/** \todo ask the user if it is OK to ignore the lock. */
		qof_session_begin(input_session, context->filename, TRUE, FALSE);
		qof_session_load(input_session, NULL);
	}
	book = qof_session_get_book(input_session);
	be = qof_book_get_backend(book);
	backend_config = qof_backend_get_config(be);
	export_session = qof_session_new();
	context->export_session = export_session;
	if(context->write_file != NULL) {
		if(*context->write_file != '/')
		{
			getcwd(current_work, PATH_MAX);
			temp = g_strconcat(current_work, "/", context->write_file, NULL);
			context->write_file = temp;
		}
		qof_session_begin(export_session, context->write_file, FALSE, TRUE);
	}
	else { qof_session_begin(export_session, QOF_STDOUT, TRUE, FALSE); }
	qof_session_set_current_session(input_session);
	qof_main_moderate_query(context);
	qof_session_save(export_session, NULL);
	qof_main_show_error(export_session);
	qof_main_show_error(input_session);
	qof_session_end(input_session);
	qof_session_end(export_session);
}

static void
qof_list_cb(QofObject *obj, gpointer data)
{
	if(qof_class_is_registered(obj->e_type)) {
		fprintf(stdout, "%-20s\t%s\n", obj->e_type, obj->type_label);
	}
}

void
qof_main_select(qof_main_context *context)
{
	g_return_if_fail(context);
	qof_object_foreach_type(select_cb, context);
}

void
qof_cmd_list (void)
{
	fprintf(stdout, _("\n%s currently supports these database names:\n"
	"You can use the names with %s -d\n"
	"and in SQL queries (as the table name) with %s -s|f\n"
	"Descriptions are shown only for readability.\n\n"
	"Name                    Description\n\n"
	)
	, PACKAGE, PACKAGE, PACKAGE);
	qof_object_foreach_type(qof_list_cb, NULL);
	fprintf(stdout, _("\nUse '-d <database> --explain' to see the list of fields within\n"
	"any supported database.\n"));
	fprintf(stdout, _("\nThank you for using %s\n\n"), PACKAGE);
}

static void
explain_cb (QofParam* param, gpointer user_data)
{
	if(param->param_getfcn && param->param_setfcn)
	{
		fprintf(stdout, _("Type: %s\tName: %s\n"), 
			param->param_type, param->param_name);
	}
}

void
qof_cmd_explain (qof_main_context *context)
{

	if(context->error) { return; }
	fprintf(stdout, _("\nParameters of the %s database:\n\n"), context->database);
	qof_class_param_foreach(context->database, explain_cb, NULL);
	fprintf(stdout, _("\nThank you for using %s\n\n"), PACKAGE);
}

void
qof_mod_category (const char *category, qof_main_context *data)
{
	data->category = g_strdup(category);
}

void
qof_mod_database (const char *database, qof_main_context *data)
{
	if(qof_class_is_registered(database)) {
		data->database = g_strdup(database);
	}
}

void
qof_mod_timespec (const char *date_time, qof_main_context *data)
{
	gchar *temp;
	int year, month, day;
	gboolean takemonth, takeyear, scanned;
	char *first_field, *second_field, *third_field;
	static char *delims = ".,-+/\\() ";

	takemonth = takeyear = scanned = FALSE;
	day = month = year = 0;
	second_field = "";
	third_field = "";
	temp = g_strdup(date_time);
	qof_date_format_set(QOF_DATE_FORMAT_UTC);
	scanned = qof_scan_date(temp, &day, &month, &year);
	if(scanned == FALSE)
	{
		first_field = strtok (temp, delims);
		if (first_field)
		{
			second_field = strtok (NULL, delims);
			if (second_field)
			{
				third_field = strtok (NULL, delims);
			}
		}
		if (third_field && second_field)
		{
			year = atoi(first_field);
			month = atoi(second_field);
			day = atoi(third_field);
		} else if (second_field)
		{
			year = atoi(first_field);
			month = atoi(second_field);
			takemonth = TRUE;
		} else if (first_field)
		{
			year = atoi(first_field);
			takeyear = TRUE;
		}
	}
	if(takemonth) { day = 1; }
	if(takeyear)  { day = 1; month = 1; }
	data->min_ts = gnc_dmy2timespec(day, month, year);
	if(takemonth) { day = gnc_date_my_last_mday(month, year); }
	if(takeyear)  {
		month = 12;
		day = gnc_date_my_last_mday(month, year);
	}
	data->max_ts = gnc_dmy2timespec_end(day, month, year);
}

void
qof_mod_exclude (const char *exclude, qof_main_context *data)
{
	if(qof_class_is_registered(exclude)) {
		data->exclude = g_strdup(exclude);
	}
}

void
qof_mod_sql (const char *sql_query, qof_main_context *data)
{
	if(!qof_check_sql(sql_query)) { return; }
	data->sql_str = g_strdup(sql_query);
}

void
qof_mod_sql_file (const char *sql_file, qof_main_context *data)
{
	FILE *filehandle;
#ifndef HAVE_GETLINE
	char lineptr[1024];
#else
	char *lineptr;
#endif
	char *buf;
	size_t n;
	QofQuery *q;
	struct stat sbuf;

	data->sql_file = g_strdup(sql_file);
	n = 0;
	q = NULL;
	data->sql_list = NULL;
	if (stat(sql_file, &sbuf) <0) {
		fprintf(stderr,"%s: ERROR. Unable to open %s (%s)\n\n",
			PACKAGE, sql_file, strerror(errno));
		return;
		}
	filehandle = fopen(sql_file, "r");
#ifndef HAVE_GETLINE
	while (NULL != (fgets(lineptr, sizeof(lineptr), filehandle)))
#else
	lineptr = NULL;
	while (0 < getline(&lineptr, &n, filehandle))
#endif
	{
		if(!qof_check_sql(lineptr)) { continue; }
		if(0 == safe_strcmp(lineptr, "\n")) { continue; }
		buf = g_strdup(lineptr);
		data->sql_list = g_list_append(data->sql_list, buf);
	}

	fclose(filehandle);
}

void
qof_mod_write (const char *write_file, qof_main_context *data)
{
	FILE *f;

	data->write_file = g_strdup(write_file);
	f = fopen(data->write_file, "a+");
	if(f) {fclose(f); }
}

void
qof_main_show_error(QofSession *session)
{
	QofBackendError io_error;
	char *newfile;
	gboolean uh_oh;
	const char *fmt;

	uh_oh = TRUE;
	newfile = g_strdup(qof_session_get_file_path(session));
	io_error = qof_session_get_error(session);
	switch (io_error)
	{
	case ERR_BACKEND_NO_ERR : {
		uh_oh = FALSE;
		return;
	}
	case ERR_BACKEND_NO_HANDLER: {
		fmt = _("%s: No suitable backend was found for %s.\n");
		fprintf(stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_NO_BACKEND: {
		fmt = _("%s: The URL '%s' is not supported by this "
		"version of %s.\n");
		fprintf(stderr, fmt, PACKAGE, newfile, PACKAGE);
		break;
	}
	case ERR_BACKEND_BAD_URL: {
		fmt = _("%s: Cannot parse the URL '%s'\n");
		fprintf(stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_CANT_CONNECT: {
		fmt = _("%s: Cannot connect to '%s'. "
		"The host, username or password were incorrect.\n");
		fprintf(stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_CONN_LOST: {
		fmt = _("%s: Cannot connect to '%s'. "
		"Connection was lost, unable to send data.\n");
		fprintf(stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_TOO_NEW: {
		fmt = _("%s: This file/URL appears to be from a newer "
		"version of %s.\n");
		fprintf (stderr, fmt, PACKAGE, PACKAGE);
		break;
	}
	case ERR_BACKEND_NO_SUCH_DB: {
		fmt = _("%s: The database '%s' does not seem to exist.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_LOCKED: {
		fmt = _("%s: Could not obtain the lock for '%s'.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_READONLY: {
		fmt = _("%s could not write to '%s'. "
		"That database may be on a read-only file system, "
		"or you may not have write permission for the directory.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_DATA_CORRUPT: {
		fmt = _("%s: The file/URL '%s' does not contain %s "
		"data or the data is corrupt.\n");
		fprintf (stderr, fmt, PACKAGE, newfile, PACKAGE);
		break;
	}
	case ERR_BACKEND_SERVER_ERR: {
		fmt = _("%s: The server at URL '%s' "
		"experienced an error or encountered bad or corrupt data.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_PERM: {
		fmt = _("%s: You do not have permission to access '%s'.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_BACKEND_MISC: {
		fmt = _("%s: An error occurred while processing '%s'.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	/* QSF additions */
	case ERR_QSF_INVALID_OBJ: {
		fmt = _("%s: Invalid QSF Object file! The QSF object file '%s' "
		" failed to validate  against the QSF object schema. "
		"The XML structure of the file is either not well-formed "
		"or the file contains illegal data.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_QSF_INVALID_MAP: {
		fmt = _("%s: Invalid QSF Map file! The QSF map file "
		"failed to validate against the QSF map schema. "
		"The XML structure of the file is either not well-formed "
		"or the file contains illegal data.\n");
		fprintf (stderr, fmt, PACKAGE);
		break;
	}
	case ERR_QSF_BAD_QOF_VERSION: {
		fmt = _("%s: The QSF Map file '%s' was written for a different "
		"version of QOF. It may need to be modified to work with "
		"your current QOF installation.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_QSF_BAD_MAP: {
		fmt = _("%s: The selected QSF map '%s' contains unusable or missing data. "
		"This is usually because not all the required parameters for "
		"the defined objects have calculations described in the map.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_QSF_BAD_OBJ_GUID: {
		fmt = _("%s: The selected QSF object file '%s' contains one or "
		"more invalid GUIDs. The file cannot be processed - "
		"please check the source of the file and try again.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_QSF_NO_MAP: {
		fmt = _("%s: The selected QSF Object file '%s' requires a map"
		"but it was not provided.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_QSF_WRONG_MAP: {
		fmt = _("%s: Wrong QSF map selected. The selected map, validates "
		"but was written for different QOF objects. "
		"The list of objects defined in this map does not include "
		"all the objects described in the current QSF object file.\n");
		fprintf (stderr, fmt, PACKAGE);
		break;
	}
	case ERR_QSF_MAP_NOT_OBJ: {
		fmt = _("%s: The selected file '%s' is a QSF map and cannot"
		"be opened as a QSF object.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_QSF_OVERFLOW : {
		fmt = _("%s: When converting XML strings into numbers, an overflow "
		"has been detected. The QSF object file '%s' contains invalid "
		"data in a field that is meant to hold a number.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_QSF_OPEN_NOT_MERGE : {
		fmt = _("%s: The QSF object file '%s' should be merged, "
		"not opened directly.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_FILEIO_FILE_BAD_READ: {
		fmt = _("%s: There was an error reading the file '%s'.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_FILEIO_PARSE_ERROR: {
		fmt = _("%s: There was an error parsing the file '%s'.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_FILEIO_FILE_EMPTY: {
		fmt = _("%s: The file '%s' is empty.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_FILEIO_FILE_NOT_FOUND: {
		fmt = _("%s: The file '%s' could not be found.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_FILEIO_FILE_TOO_OLD: {
		fmt = _("%s: This file is from an older version.\n");
		fprintf (stderr, fmt, PACKAGE);
		break;
	}
	case ERR_FILEIO_UNKNOWN_FILE_TYPE: {
		fmt = _("%s: Unknown file type, '%s'.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_FILEIO_BACKUP_ERROR: {
		fmt = _("%s: Could not make a backup of '%s'.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
		break;
	}
	case ERR_FILEIO_WRITE_ERROR: {
		fmt = _("%s: Could not write to '%s'. Check that you have "
		"permission to write to this file and that there is sufficient "
		"space to create it.\n");
		fprintf (stderr, fmt, PACKAGE, newfile);
	  break;
	}
	case ERR_SQL_DB_TOO_OLD: {
		fmt = _("%s: This database is from an older version.\n");
		fprintf (stderr, fmt, PACKAGE);
		break;
	}
	case ERR_SQL_DB_BUSY: {
		fmt = _("%s: The SQL database is in use by other users.\n");
		fprintf (stderr, fmt, PACKAGE);
		break;
	}
	default:
		fmt = _("%s: An unknown I/O error occurred.\n");
		fprintf (stderr, fmt, PACKAGE);
	break;
	}
}

/*==================== END OF FILE ======================*/
