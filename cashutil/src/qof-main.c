/***************************************************************************
 *            qof-main.c
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
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
/** @addtogroup QOFCLI

Includes common functions for all QOF CLI programs and provides generic
functions to implement command line and interactive shell options.
@{
*/
/** @file qof-main.c
    @brief Common functions for the QOF external framework
    @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/
#define _GNU_SOURCE
#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include "qof-main.h"
/** \todo temporary until undo goes into QofBook */
#include "qofundo-p.h"

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

static GHashTable *backend_extensions;
static gchar* log_module = CU_MOD_ENGINE;

#define QSF_COMPRESS "compression_level"

/** standardise tab output to help output line up nicely. */
#define QOF_TAB "    "

/** the maximum number of entities to offer for a single selection.

\todo Improve the maximum to allow scrolling / next page.
*/
#define CLI_MAX_SELECT 20

/** Relate the command to the function. */
typedef int (*cmd_fcn) (qof_data *context);
/** \todo Reorganise the C into multiple files. */
static int qof_parse_command (const char *cmd, qof_data *context);

/** Provide a simple numerical index for selectable objects.

@param ent  each entity in turn of the selected type.
@param data The qof_data context for the CLI.

Ensure counter is reset to zero between runs.
*/
static void
cli_select_cb (QofEntity *ent, gpointer data)
{
	qof_data *context;
	gchar *temp;

	context = (qof_data*)data;
	g_return_if_fail(context);
	context->counter++;
	if(context->counter >= CLI_MAX_SELECT) { return; }
	fprintf (stdout, "%d    %s\n", context->counter, 
		qof_object_printable(context->inst_type, (QofInstance*)ent));
	temp = g_strdup_printf("%d", context->counter);
	g_hash_table_insert(context->select_table, temp, ent);
}

/** \brief Relate the commands to the functions within the shell. */
struct shell_cmd
{
	const char *name;     /**< Name of the command. */
	cmd_fcn func;         /**< Name of function to execute the command. */
};

/** command object param value */
struct search_helper
{
	QofInstance *inst;
	QofParam *param;  /**< the parameter in argv[2] */
	const char *term; /**< argv[3] */
	char *value;      /**< argv[4] */
};

/** check_for_single_instance */
struct count_helper
{
	gint count;
	QofInstance *first_inst;
};

/** pre-selects if only one entity of this type exists. */
static void
count_cb (QofEntity *ent, gpointer user_data)
{
	struct count_helper *ch;

	ch = (struct count_helper*)user_data;
	ch->count++;
	if(ch->count == 1)
	{
		ch->first_inst = (QofInstance*)ent;
	}
}

/** select_table helper

The select_table hashtable relies on a conversion from
the integer counter to a string. This frees the temporary
conversion string when the hashtable is no longer needed.
*/
static void
cli_free_select (gpointer key, gpointer value, gpointer data)
{
	g_free(key);	
}

static void option_cb (QofBackendOption *option, gpointer data)
{
	gint gz_level;

	gz_level = *(gint*)data;
	if(0 == safe_strcmp(QSF_COMPRESS, option->option_name)) {
		option->value = (gpointer)&gz_level;
	}
}

static void
qof_mod_compression (gint gz_level, qof_data *data)
{
	KvpFrame *be_config;
	QofBook *book;
	QofBackend *be;

	if((gz_level > 0) && (gz_level <= 9))
	{
		book = qof_session_get_book(data->export_session);
		be = qof_book_get_backend(book);
		be_config = qof_backend_get_config(be);
		qof_backend_option_foreach(be_config, option_cb, &gz_level);
		qof_backend_load_config(be, be_config);
	}
}

/** select an instance

Offers multiple entities for selection. Ensure that
check_for_single_instance has been run first.

\todo Offer multiple listings of CLI_MAX_SELECT each.
*/
static int select_fcn (qof_data *context)
{
	gint choice, max;
	gchar *temp;

	if(!context->inst_type) { return -1; }
	choice = 0;
	context->counter = 0;
	context->instance = NULL;
	context->select_table = g_hash_table_new(g_str_hash, g_str_equal);
	qof_object_foreach(context->inst_type, context->book, cli_select_cb, context);
	max = g_hash_table_size(context->select_table);
	if(max > CLI_MAX_SELECT) { max = CLI_MAX_SELECT; }
	/* Translators: %s is an object name, %d the number of objects in the book. */
	switch (context->cli_mode)
	{
		case EDIT_MODE : {
			fprintf (stdout, _("Choose the %s to edit: (1 - %d): "), 
				context->inst_type, max);
			break;
		}
		case DELETE_MODE : {
			fprintf (stdout, _("Choose the %s to DELETE: (1 - %d): "), 
				context->inst_type, max);
			break;
		}
		case PRINT_MODE : {
			fprintf (stdout, _("Choose the %s to print: (1 - %d): "), 
				context->inst_type, max);
			break;
		}
		case NO_OP : { break; }
	}
	scanf("%2i", &choice);
	fprintf (stdout, "\n");
	while(choice <= 0 || choice > max)
	{
		char c;
		c = getchar();
		while (c != '\n') { c = getchar(); }
		fprintf (stdout, _("Only %d objects are available of type '%s'.\n"), 
			max, context->inst_type);
		fprintf (stdout, _("Choose the %s to use: (1 - %d): "), context->inst_type, max);
		scanf("%2i", &choice);
		fprintf (stdout, "\n");
	}
	temp = g_strdup_printf("%d", choice);
	context->instance = (QofInstance*)g_hash_table_lookup(context->select_table, temp);
	g_free(temp);
	g_hash_table_foreach(context->select_table, cli_free_select, NULL);
	g_hash_table_destroy(context->select_table);
	return 0;
}

/** \brief Shorthand search routine.

Rather than using a QofQuery, this shorthand version uses a
string for all parameter types. It will support the existing
-t --date shorthand versions for compatibility with the non-interactive
command set by actually using an underlying QofQuery.

If this quick search fails, an interactive QofQuery could be used.
A new query would have been needed anyway, with options enabled to
isolate only one match. QofQuery relies on the term being the same
type as the param_type. SQL queries will also be supported via the
sql command in the shell. */

static void
cli_search_cb (QofEntity * ent, gpointer data)
{
	struct search_helper *sh;
	gchar *value;
	const QofParam *param;
	gboolean found;

	found = FALSE;
	sh = (struct search_helper*)data;
	param = sh->param;
/* if param_type == QOF_TYPE_DATE, process term as -t --date and skip to a QofQuery format. */
	if(0 == safe_strcmp(param->param_type, QOF_TYPE_DATE)) { return; }
	value = qof_book_merge_param_as_string((QofParam*)param, ent);
	if(0 == safe_strcmp(value, sh->term))
	{
		 /* if term matches more than once, fallback to SQL */
		if(found) { sh->inst = NULL; return; }
		found = TRUE;
		sh->inst = (QofInstance*)ent;
	}
}

/** Shortcut if only one entity of this type exists. */
static gboolean
check_for_single_instance (qof_data *context)
{
	QofCollection *coll;
	struct count_helper ch;
	struct search_helper sh;

	ch.count = 0;
	qof_object_foreach(context->inst_type, context->book, count_cb, &ch);
	if(!ch.count)
	{
		fprintf (stderr, _("%s: No objects of type '%s' found.\n"),
			PACKAGE, context->inst_type);
		context->inst_type = NULL;
		return FALSE;
	}
	if(ch.count == 1) 
	{ 
		context->instance = ch.first_inst; 
		if(!context->argv[2] || !context->argv[3]) { return TRUE; }
	}
	if(!context->argv[2] || !context->argv[3]) { return FALSE; }
	sh.inst = NULL;
	sh.param = NULL;
	sh.term = context->argv[3];
	sh.param = (QofParam*)qof_class_get_parameter(context->inst_type, context->argv[2]);
	if(sh.param == NULL) { 
		fprintf (stderr, _("No parameter named '%s' found for object '%s'\n"), 
			sh.term, sh.param->param_name);
		return FALSE;
	}
	coll = qof_book_get_collection(context->book, context->inst_type);
	if(!coll) { return FALSE; }
	qof_collection_foreach(coll, cli_search_cb, &sh);
	if(sh.inst)
	{
		context->instance = sh.inst;
		if(context->argv[4]) {
			g_message("argv[4]=%s param=%s", context->argv[4], sh.param->param_name);
			sh.value = g_strdup(context->argv[4]);
			qof_begin_edit(context->instance);
			undo_edit_record(context->instance, sh.param);
			qof_entity_set_param(&context->instance->entity, (QofParam*)sh.param, sh.value);
			qof_commit_edit(context->instance);
			undo_edit_commit(context->instance, sh.param);
		}
		return TRUE;
	}
	return FALSE;
}

/** @name Top level shell functions.
@{
*/

/** Run a sub-shell to either select or edit an instance. */
static int
qof_sub_shell (qof_data *context)
{
#ifdef HAVE_LIBREADLINE
	char *line;
	char *prompt;

	if(!context->inst_type) { return 0; }
	line = (char*)malloc(256 * sizeof(char));
	prompt = g_strdup_printf("%s/%s> ", context->shortname, context->inst_type);
#else
	char buf[256];

	if(!context->inst_type) { return 0; }
#endif
	for (;;) {
		fflush (stdout);
#ifdef HAVE_LIBREADLINE
		line = readline(prompt);
		if (line == NULL) { break; }
		if (*line) { add_history(line); }
		if (qof_parse_command(line, context) < 0) { break; }
		g_free (line);
#else
		fprintf (stdout, "%s/%s> ", context->shortname, context->inst_type);
		if (qof_parse_command(buf, context) < 0) { break; }
#endif
	}
	fprintf(stdout, "\n");
	return 0;
}

/** \brief add a new instance and edit the values

Expects the second argument (context->argv[1]) to be the name of a registed object.
*/
static int add_fcn(qof_data *context)
{
	QofInstance *inst;
	gint result;
	gchar *temp;

	result = 0;
	if(!qof_class_is_registered(context->argv[1]))
	{
		fprintf (stdout, _("%s: Cannot add '%s' - object name not found\n"),
			PACKAGE, context->argv[1]);
		return 0;
	}
	context->inst_type = g_strdup(context->argv[1]);
	temp = g_strdup_printf("add-%s", context->inst_type);
	qof_book_start_operation(context->book, temp);
	inst = (QofInstance*)qof_object_new_instance(context->inst_type, context->book);
	if(!inst) {
		fprintf (stdout, _("Failed to add an instance of %s\n"), context->inst_type);
		return 0;
	}
	fprintf (stdout, _("Added an instance of %s\n\n"), context->inst_type);
	fprintf (stdout, _("Now edit the parameter details of this instance.\n"));
	fprintf (stdout, _("Type 'help' for available commands or parameters.\n\n"));
	undo_create_record(inst);
	qof_book_end_operation(context->book);
	context->shell_type = EDIT_SHELL;
	context->instance = inst;
	result = qof_sub_shell(context);
	context->shell_type = TOP_SHELL;
	return result;
}

/** \brief edit [object]

\todo skip over user-friendly forms that may use
where or =.

*/
static int edit_fcn(qof_data *context)
{
	gint result;
	gchar *temp;

	if(!qof_class_is_registered(context->argv[1]))
	{
		fprintf (stdout, _("%s: Cannot edit '%s' - object name not found\n"),
			PACKAGE, context->argv[1]);
		return 0;
	}
	result = 0;
	context->inst_type = g_strdup(context->argv[1]);
	if(!check_for_single_instance(context)) {
		fprintf (stdout, _("Select an instance of %s to edit:\n"), context->inst_type);
		fprintf (stdout, _("Type 'help' for available commands or parameters.\n\n"));
		context->cli_mode = EDIT_MODE;
		result = select_fcn(context);
		context->cli_mode = NO_OP;
		if (result) { return result; }
	}
	context->shell_type = EDIT_SHELL;
	temp = g_strdup_printf("modify-%s", context->inst_type);
	qof_book_start_operation(context->book, temp);
	fprintf (stdout, _("Edit the parameter details of the selected instance.\n"));
	fprintf (stdout, _("Type 'help' for available commands or parameters.\n\n"));
	if(context->instance) { result = qof_sub_shell(context); }
	context->shell_type = TOP_SHELL;
	context->inst_type = NULL;
	qof_book_end_operation(context->book);
	return result;
}
/** \brief delete an instance of an object.

Selects one instance and frees the entity.
Warns the user AFTER deletion. 

\todo a bespoke undo command could be useful, rather
than quitting the program completely! :-)
*/
static int delete_fcn(qof_data *context)
{
	gint result;
	QofEntity *ent;
	gchar *temp;

	result = 0;
	if(!qof_class_is_registered(context->argv[1]))
	{
		fprintf (stdout, _("%s: Cannot delete '%s' - object name not found\n"),
		PACKAGE, context->argv[1]);
		return 0;
	}
	context->inst_type = g_strdup(context->argv[1]);
	if(!check_for_single_instance(context))
	{
		fprintf (stdout, _("Select the instance of %s to delete\n"), context->inst_type);
		fprintf (stdout, _("Type 'help' for available commands or parameters.\n\n"));
		context->cli_mode = DELETE_MODE;
		result = select_fcn(context);
		context->cli_mode = NO_OP;
		if ((result)||(!context->instance)) { return result; }
	}
	temp = g_strdup_printf("delete_%s", context->inst_type);
	qof_book_start_operation(context->book, temp);
	undo_delete_record(context->instance);
	ent = (QofEntity*)context->instance;
	qof_collection_mark_dirty(qof_book_get_collection(context->book, ent->e_type));
	qof_entity_release(ent);
	qof_book_end_operation(context->book);
	fprintf (stdout, _("The instance has been DELETED. Type 'quit' to exit and undo. "
	"The file will not be changed until you type 'write'.\n"));
	return 0;
}

static void
qof_list_cb(QofObject *obj, gpointer data)
{
	fprintf(stdout, "%-20s\t%s\n", obj->e_type, obj->type_label);
}

static int list_fcn(qof_data *context)
{
	fprintf (stdout, _("\nThe QOF shell supports these object names:\n"
	"You can use the names with 'edit', 'print' or 'delete'\n"
	"and in SQL queries (as the table name) with 'sql'\n"
	"Descriptions are shown only for readability.\n\n"));
	fprintf (stdout, "%-20s%s", _("Object Name"), _("Description\n"));
	qof_object_foreach_type(qof_list_cb, NULL);
	fprintf (stdout, _("\nUse 'explain <database>' to see the list of fields within\n"
	"any supported database.\n"));
	return 0;
}

static void
qof_print_cb (QofParam *param, gpointer data)
{
	gchar *str;
	qof_data *context;

	context = (qof_data*)data;
	g_return_if_fail(context);
	str = qof_book_merge_param_as_string(param, (QofEntity*)context->instance);
	fprintf (stdout, _("%-24s %-12s %s\n"), param->param_name, param->param_type, str);
}

/** \brief print an instance to the terminal. */
static int print_fcn(qof_data *context)
{
	gint result;

	result = 0;
	if(!qof_class_is_registered(context->argv[1]))
	{
		fprintf (stdout, _("%s: Cannot print '%s' - object name not found\n"),
			PACKAGE, context->argv[1]);
		return 0;
	}
	context->inst_type = g_strdup(context->argv[1]);
	if(!check_for_single_instance(context))
	{
		if(!context->inst_type) { return 0; }
		fprintf (stdout, _("Select the instance of '%s' to print.\n"), context->inst_type);
		fprintf (stdout, _(" Type 'help' for available commands or parameters.\n\n"));
		context->cli_mode = PRINT_MODE;
		result = select_fcn(context);
		context->cli_mode = NO_OP;
		if ((result)||(!context->instance)) { return result; }
	}
	fprintf (stdout, "%-24s %-12s %s\n\n", _("Name"), _("Type"), _("Value"));
	qof_class_param_foreach (context->inst_type, qof_print_cb, context);
	fprintf(stdout, "\n");
	return 0;
}

static void
qof_explain_cb (QofParam* param, gpointer user_data)
{
	if(param->param_getfcn && param->param_setfcn)
	{
		fprintf (stdout, _("Type: %-12s\tName: %-12s\n"), param->param_type, param->param_name);
	}
}

/** print a list of available parameters for this object type */
static int explain_fcn(qof_data *context)
{
	if(qof_class_is_registered(context->argv[1])) {
		fprintf (stdout, _("\nParameters of the %s database:\n\n"), context->argv[1]);
		qof_class_param_foreach(context->argv[1], qof_explain_cb, NULL);
		fprintf (stdout, "\n\n");
	}
	else {
		fprintf (stderr, _("\n%s: %s object not found.\n"), PACKAGE, context->argv[1]);
	}
	return 0;
}

/** \brief load [filename]

Expects a readable filename in context->argv[1]
*/
static int load_fcn(qof_data *context)
{
	char* answer;

	answer = g_strdup(" ");
	if(qof_book_not_saved(context->book))
	{
		fprintf (stderr, _("%s: The current book has not been saved.\n"
		"Do you still want to overwrite it? [y/n]\n"), PACKAGE);
		scanf("%1s", answer);
		if((0 != safe_strcmp(answer, "y")) && (0 != safe_strcmp(answer, "Y"))) {
			qof_session_save(context->input_session, NULL);
		}
	}
	qof_session_end(context->input_session);
	context->input_session = qof_session_new();
	if(context->argv[1]) {
		context->filename = g_strdup(context->argv[1]);
	}
	if(context->filename) {
		qof_session_begin(context->input_session, context->filename, FALSE, TRUE);
		qof_mod_compression(context->gz_level, context);
	}
	else {
		qof_session_begin(context->input_session, QOF_STDOUT, FALSE, FALSE);
	}
	qof_session_load(context->input_session, NULL);
	context->book = qof_session_get_book(context->input_session);
	qof_show_error(context->input_session, context->filename);
	/* implement in the backend eventually */
	qof_book_clear_undo(context->book);
	return 0;
}
/** \brief write [filename]

write the current book to file.
	If no filename is provided, save to the original file
	(to STDOUT if STDIN used). Analagous to Save / Save As...

\todo fix problems with writing to an alternative file.
*/
static int write_fcn(qof_data *context)
{
	QofSession *save_as;
	gchar current_work[PATH_MAX];
	gchar *temp;

	if(context->argv[1])
	{
		save_as = qof_session_new();
		if(*context->argv[1] != '/')
		{
			getcwd(current_work, PATH_MAX);
			temp = g_strconcat(current_work, "/", context->argv[1], NULL);
			context->argv[1] = temp;
		}
		qof_session_begin(save_as, context->argv[1], FALSE, TRUE);
		qof_session_swap_data(save_as, context->input_session);
		qof_session_save(save_as, NULL);
		qof_show_error(save_as, context->argv[1]);
		qof_session_end(save_as);
		return 0;
	}
	if(context->write_file)
	{
		qof_session_save(context->export_session, NULL);
		qof_show_error(context->export_session, context->write_file);
	}
	else {
		qof_session_save(context->input_session, NULL);
		qof_show_error(context->input_session, context->filename);
	}
	/* implement in the backend eventually */
	if(qof_book_can_undo(context->book)) { qof_book_clear_undo(context->book); }
	return 0;
}

/** \brief merge UI

\todo the entire merge routine needs testing.
*/
static void
qof_merge_loop (qof_book_mergeData *mergeData, qof_book_mergeRule *rule, guint remainder)
{
	GSList *user_reports;
	QofParam *one_param;
	gchar *importstring, *targetstring, *buffer;
	gint count, resolution;
	gboolean input_ok;
	gchar y;

	buffer = "";
	count = 0;
	input_ok = FALSE;
	user_reports = rule->mergeParam;
	while(user_reports != NULL) {
		one_param = user_reports->data;
		buffer = g_strconcat(buffer, g_strdup_printf(_("%i:Parameter name: %s "),
			count, one_param->param_name), NULL);
		importstring = qof_book_merge_param_as_string(one_param, rule->importEnt);
		buffer = g_strconcat(buffer, g_strdup_printf(_("Import data : %s "), importstring), NULL);
		targetstring = qof_book_merge_param_as_string(one_param, rule->targetEnt);
		buffer = g_strconcat(buffer, g_strdup_printf(_("Original data : %s\n"), targetstring), NULL);
		user_reports = g_slist_next(user_reports);
		count++;
	}
	while(!input_ok) {
		resolution = 0;
		fprintf (stdout, _("\nPlease resolve this conflict. Enter\n"
		"    1 to use the import data or \n"
		"    2 to keep the original data or"));
		if(rule->mergeAbsolute == FALSE) {
			fprintf (stdout, _("\n    3 to import the data as a NEW object or"));
		}
		fprintf (stdout, _("\n    9 to abort the entire merge operation.\n"));
		fprintf (stdout, "> (1, 2");
		if(rule->mergeAbsolute == FALSE) {
			fprintf (stdout, ", 3 ");
		}
		fprintf (stdout, _("or 9) : "));
		scanf("%1i", &resolution);
		switch(resolution) {
			case 1 : {
				mergeData = qof_book_mergeUpdateResult(mergeData, MERGE_UPDATE);
				input_ok = TRUE;
				break;
				}
			case 2 : {
				if(rule->mergeAbsolute == FALSE) {
					mergeData = qof_book_mergeUpdateResult(mergeData, MERGE_DUPLICATE);
				}
				if(rule->mergeAbsolute == TRUE) {
					mergeData = qof_book_mergeUpdateResult(mergeData, MERGE_ABSOLUTE);
				}
				input_ok = TRUE;
				break;
			}
			case 3 : {
				if(rule->mergeAbsolute == FALSE) {
					mergeData = qof_book_mergeUpdateResult(mergeData, MERGE_NEW);
					input_ok = TRUE;
				}
				break;
			}
			case 9 : {
				fprintf (stdout, _("Are you sure you want to abort the entire merge operation?\n"
				"The rest of the import data will not be processed.\n"
				"Your original data will not be modified. Abort? y/n : "));
				scanf("%1s", &y);
				if((safe_strcmp(_("y"),&y) == 0)||(safe_strcmp("",&y) == 0)) {
					fprintf (stdout, _("Aborting . . \n\n"));
					mergeData = qof_book_mergeUpdateResult(mergeData, MERGE_INVALID);
					input_ok = TRUE;
				}
				break;
			}
			default : break;
		}
	}
}

/** \brief merge [filename]

Accept a filename from the shell, check it is readable and
load it, then if the book is valid, merge it into the
current book.

If the current book is NULL, this is equivalent to 'load' but
a LOT more longwinded. (Not recommended.)
*/
static int merge_fcn(qof_data *context)
{
	qof_book_mergeData *mergeData;
	QofBook *importBook, *targetBook;
	gint result;

	/** \todo check the filename can be read. */
	if(!context->argv[1]) { return 0; }
	qof_session_begin(context->export_session, context->argv[1], FALSE, FALSE);
	qof_session_load(context->export_session, NULL);
	qof_show_error(context->export_session, context->write_file);
	importBook = qof_session_get_book(context->export_session);
	targetBook = context->book;
	mergeData = qof_book_mergeInit(importBook, targetBook);
	g_return_val_if_fail((mergeData != NULL), -1);
	qof_book_mergeRuleForeach(mergeData, qof_merge_loop, MERGE_REPORT);
	result = qof_book_mergeCommit(mergeData);
	return result;
}

/** check if book is dirty, show errors (if any) and exit */
static int exit_fcn(qof_data *context)
{
	char* answer;

	answer = g_strdup(" ");
	if(qof_book_not_saved(context->book))
	{
		fprintf (stdout, _("The current book has not been saved.\n"
		"Do you still want to quit without saving? %s"), "[y/n]");
		scanf("%1s", answer);
		fprintf (stdout, "\n");
		if(0 != safe_strcmp(answer, "y") && (0 != safe_strcmp(answer, "Y"))) {
			qof_session_save(context->input_session, NULL);
		}
	}
	if(qof_book_not_saved(qof_session_get_book(context->export_session)))
	{
		qof_show_error(context->export_session, context->write_file);
	}
	qof_show_error(context->input_session, context->filename);
	qof_session_end(context->input_session);
	if(context->export_session) { qof_session_end(context->export_session); }
	fprintf (stdout, _("\nThank you for using %s.\n"), PACKAGE);
	return -1;
}

static int help_fcn(qof_data *context)
{
	fprintf (stdout, _("Commands available in the shell are:\n\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "add [object]", 
		_("Add a new instance of the object.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "edit [object]",
		_("Select one instance and edit (set) the parameter values.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "delete [object]", 
		_("Select one instance for deletion.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "list", 
		_("Synonym for the --list command outside the shell.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "print [object]", 
		_("Select one instance and print the parameter values.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "explain [object]", 
		_("Synonym for the --explain command outside the shell.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "load [filename]",
		_("Replace the current book with data from the file.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, " ", 
		_("Prompts to save the current book, if any.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "write [filename]", 
		_("Write out the current book. If no filename is given,\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, " ", _("write to the original file.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, " ", _("(uses STDOUT if STDIN is used.)\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "merge [filename]", 
		_("Merge data from the file into the current book.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "sql [sql_query]", 
		_("Run a \"quoted\" SQL statement.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "help | ?", _("This screen.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "quit | q | exit", _("Quit the shell\n"));
	fprintf (stdout, "\n");
	return 0;
}

/** \brief sql - take a SQL statement.

\todo improve the sql_parser - it is very noisy and
yet not very helpful.
*/
static int sql_fcn(qof_data *context)
{
	QofSqlQuery *q;
	gchar *sql, *temp;
	gint result;
	QofQuery *qq;
	QofBook *book;
	GList *results;

	q = qof_sql_query_new();
	sql = g_strdup(context->sql_str);
	if(!sql) { sql = context->argv[1]; }
	qof_sql_query_parse(q, sql);
	qq = qof_sql_query_get_query(q);
	book = qof_session_get_book(context->input_session);
	qof_query_set_book(qq, book);
	qof_query_set_sort_order(qq, NULL, NULL, NULL);
	results = qof_query_run (qq);
	if(!results) { return 0; }
	if(g_list_length(results) == 1) { 
		context->instance = (QofInstance*)results->data;
		context->shell_type = EDIT_SHELL;
		temp = g_strdup_printf("modify-%s", context->inst_type);
		qof_book_start_operation(context->book, temp);
		fprintf (stdout, _("Edit the parameter details of the selected instance.\n"));
		fprintf (stdout, _("Type 'help' for available commands or parameters.\n\n"));
		if(context->instance) { result = qof_sub_shell(context); }
		context->shell_type = TOP_SHELL;
		context->inst_type = NULL;
		qof_book_end_operation(context->book);
		return 0;
	}
	fprintf (stdout, _("Query returned %d entities\n"), g_list_length(results));
	return 0;
}

/** @} */
/** \name Edit shell functions
@{
*/

/** \brief Set a value in a parameter

\todo improve error handling - currently it is almost silent.

Boolean values accept TRUE, 1, or translated values for Y, YES or TRUE
and are matched case insensitively, so y, yes and true (and their translations)
also match. If the match fails, false is set.

*/
static int set_edit_fcn (qof_data *context)
{
	QofParam *param;
	QofType type;
	gchar *value;

	if(!context->instance || !context->argv[1] || !context->argv[2]) { return 0; }
	value = g_strdup(context->argv[2]);
	param = (QofParam*)qof_class_get_parameter(context->inst_type, context->argv[1]);
	if(!param)
	{
		fprintf (stderr, _("%s: parameter name '%s' of object '%s' not recognised.\n"),
			PACKAGE, context->argv[1], context->inst_type);
		fprintf (stderr, _("Type 'explain' for more information.\n"));
		return 0;
	}
	type = qof_class_get_parameter_type(context->inst_type, context->argv[1]);
	qof_begin_edit(context->instance);
	/* undo_edit_record will be called by begin_edit eventually. */
	undo_edit_record(context->instance, param);
	qof_entity_set_param(&context->instance->entity, param, value);
	qof_commit_edit(context->instance);
	undo_edit_commit(context->instance, param);
	g_free(value);
	return 0;
}

/** List the available parameters for this edit */
static int explain_edit_fcn (qof_data *context)
{
	fprintf (stdout, _("\nEditable parameters of the %s object:\n\n"), context->inst_type);
	qof_class_param_foreach(context->inst_type, qof_explain_cb, NULL);
	fprintf (stdout, "\n\n");
	return 0;
}

/** \brief Commit data to instance and leave the sub-shell.

\todo Should this clear the undo??
*/
static int commit_edit_fcn (qof_data *context)
{
//	if(qof_book_can_undo(context->book)) { qof_book_clear_undo(context->book); }
	return -1;
}

/** print the instance being edited.*/
static int print_edit_fcn (qof_data *context)
{
	if(!context->inst_type) { return 0; }
	fprintf (stdout, "%-24s %-12s %s\n\n", _("Name"), _("Type"), _("Value"));
	qof_class_param_foreach (context->inst_type, qof_print_cb, context);
	fprintf (stdout, _("\nNot all parameters are editable, use 'explain' "
	"to see the list of editable parameters for the object '%s'.\n\n"), context->inst_type);
	return 0;
}

static int help_edit_fcn (qof_data *context)
{
	gchar *temp;

	fprintf (stdout, _("Commands available in this sub-shell are:\n\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "edit [Name] [Value]", 
	_("Edit the parameter 'Name' to have the 'Value' given.\n"));
	/* Translators: TRUE and numeral one are always acceptable for boolean values here. */
	fprintf (stdout, QOF_SHELL_FORMAT, " ", 
		_("Boolean values accept TRUE|true, 1, Y|y or yes|YES\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, " ", _("String values that include spaces should be quoted\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "set [Name] [Value]", _("Synonym for edit.\n"));
	temp = g_strdup_printf(_("Print the current values for this '%s' instance.\n"), context->inst_type);
	fprintf (stdout, QOF_SHELL_FORMAT, "print", temp);
	g_free(temp);
	temp = g_strdup_printf(_("Show the editable parameters for '%s'\n"), context->inst_type);
	fprintf (stdout, QOF_SHELL_FORMAT, "explain", temp);
	g_free(temp);
	fprintf (stdout, QOF_SHELL_FORMAT, "commit",  _("Set the edited parameter values.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "help | ?", _("This screen.\n"));
	fprintf (stdout, QOF_SHELL_FORMAT, "quit | q | exit", 
		_("Return to the top shell without setting changes.\n"));
	fprintf (stdout, "\n");
	return 0;
}

static int quit_edit_fcn (qof_data *context)
{
	if(qof_book_can_undo(context->book)) { qof_book_undo(context->book); }
	return -1;
}

/** @} */
/** \name Shell control handlers
@{
*/
struct shell_cmd shell_list[] = {
	{ "add",     add_fcn          },
	{ "edit",    edit_fcn         },
	{ "delete",  delete_fcn       },
	{ "list",    list_fcn         },
	{ "print",   print_fcn        },
	{ "explain", explain_fcn      },
	{ "load",    load_fcn         },
	{ "write",   write_fcn        },
	{ "merge",   merge_fcn        },
	{ "sql",     sql_fcn          },
	{ "help",    help_fcn         },
	{ "?",       help_fcn         },
	{ "q",       exit_fcn         },
	{ "quit",    exit_fcn         },
	{ "exit",    exit_fcn         },
	{ "bye",     exit_fcn         },
	{ NULL, NULL }
};

struct shell_cmd edit_list[] = {
	{ "edit",    set_edit_fcn     },
	{ "set",     set_edit_fcn     },
	{ "explain", explain_edit_fcn },
	{ "commit",  commit_edit_fcn  },
	{ "print",   print_edit_fcn   },
	{ "quit",    quit_edit_fcn    },
	{ "help",    help_edit_fcn    },
	{ "?",       help_edit_fcn    },
	{ "q",       quit_edit_fcn    },
	{ "exit",    quit_edit_fcn    },
	{ "bye",     quit_edit_fcn    },
	{ NULL, NULL }
};

static char *strtoke(char *str, const char *ws, const char *delim)
{
	int inc;
	static char *start, *s = NULL;

	if (str != NULL) { s = str; }
	inc = strspn(s, ws);
	s += inc;
	start = s;
	if (*s == '\0') { return NULL; }
	else if (strchr(delim, *s) != NULL) {
		start++;
		s = strchr(s + 1, *s);
		*s = '\0';
		s++;
	}
	else {
		inc = strcspn(s, ws);
		if (s[inc] == '\0') { s += inc; }
		else {
			s[inc] = '\0';
			s += inc + 1;
		}
	}
	return start;
}

gboolean
qof_check_sql(const char *sql)
{
	regex_t *r;
	int reg_exp_check;
	static char *pattern = QOF_SQL_SUPPORTED;
//	QofSqlQuery *q;
	gboolean result;

	result = FALSE;
	r = g_new(regex_t, 1);
	reg_exp_check = regcomp(r, pattern,
		REG_ICASE | REG_NOSUB | REG_EXTENDED);
	g_return_val_if_fail(reg_exp_check == 0, FALSE);
	if(0 == regexec(r, sql, 0, NULL, 0)) { result = TRUE; }
	regfree(r);
	g_free(r);
	return result;
}

/** \brief Relate the command to the function.

\todo Stop changing the input with strtoke ?
*/
static int
qof_parse_command (const char *cmd, qof_data *context)
{
	char *argv[32];
	int inc, argc;
	char *cmd_dup;
	gboolean good;

	argc = 0;
	good = FALSE;
	memset(argv, 0, sizeof(argv) / sizeof(char*));
	cmd_dup = strdup(cmd);
	argv[0] = strtoke(cmd_dup, " \t\n", "\"'");
	while (argv[argc] != NULL) {
		argc++;
		argv[argc] = strtoke(NULL, " \t\n", "\"'");
	}
	if (argc == 0) {
		free(cmd_dup);
		return 0;
	}
	context->argc = argc;
	context->argv = argv;
	switch(context->shell_type)
	{
		case TOP_SHELL :
		{
			for (inc = 0; shell_list[inc].name != NULL; inc++) {
				if (strcasecmp(argv[0], shell_list[inc].name) == 0) {
					good = TRUE;
					if((shell_list[inc].func(context)) < 0) { return -1; }
				}
			}
			if(!good) {
				fprintf(stderr, _("%s: bad option - %s, available commands are:\n"),
					PACKAGE, argv[0]);
				for (inc = 0; shell_list[inc].name != NULL; inc++) {
					fprintf (stderr, "%s ", shell_list[inc].name);
				}
				fprintf (stderr, _("\nUse help for more information.\n\n"));
			}
			break;
		}
		case EDIT_SHELL :
		{
			for (inc = 0; edit_list[inc].name != NULL; inc++) {
				if(strcasecmp(argv[0], edit_list[inc].name) == 0) {
					good = TRUE;
					if((edit_list[inc].func(context)) < 0) { return -1; }
				}
			}
			if(!good) {
				fprintf(stderr, _("%s: bad option - %s, available commands are:\n"),
					PACKAGE, argv[0]);
				for (inc = 0; edit_list[inc].name != NULL; inc++) {
					fprintf (stderr, "%s ", edit_list[inc].name);
				}
				fprintf (stderr, _("\nUse help for more information.\n\n"));
			}
			break;
		}
		default : { break; }
	}
	free(cmd_dup);
	return 0;
}

void
qof_cmd_shell(qof_data *context)
{
	QofSession *input_session;
#ifdef HAVE_LIBREADLINE
	char *line;
	char *prompt;

	line = (char *)malloc(256*sizeof(char));
	prompt = g_strdup_printf("%s> ", PACKAGE);
#else
	char buf[256];

#endif
	context->argc = 0;
	context->argv = NULL;
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
	if(context->filename) {
		qof_session_begin(input_session, context->filename, TRUE, TRUE);
		qof_session_load(input_session, NULL);
	}
	else { qof_session_begin(input_session, QOF_STDOUT, TRUE, FALSE); }
	qof_show_error(input_session, context->filename);
	context->book = qof_session_get_book(context->input_session);
	context->shell_type = TOP_SHELL;
	context->inst_type = NULL;
	context->instance = NULL;
	if(!context->shortname) { context->shortname = g_strndup(PACKAGE, 4); }
	context->argc = 0;
	fprintf (stdout, _("\nWelcome to the QOF interactive shell ...\n"));
	fprintf (stdout, _(" Type 'help' for additional information\n\n"));

	for (;;) {
		fflush(stdout);
#ifdef HAVE_LIBREADLINE
		line = readline(prompt);
		/* user pressed ^d or so */
		if (line == NULL) {
			fprintf(stdout, _("\n\nThank you for using %s.\n"), PACKAGE);
		       	break;
		}
		/* skip blanks */
		if (*line) { add_history(line); }
		if(qof_parse_command(line, context) != 0) { break; }
		free(line);
#else
		fprintf (stdout, "%s> ", PACKAGE);
		if(qof_parse_command(buf, context) != 0) { break; }
		if (fgets(buf, 256, stdin) == NULL) { break; }
#endif
	}
	fprintf(stdout, "\n");
}
/** @} */

qof_data*
qof_create(void)
{
	qof_data *context;

	context = g_new0(qof_data, 1);
	return context;
}

/** Prints helpful error messages

\todo make sure the file is always available, some
errors still print (null).
*/
void
qof_show_error(QofSession *session, const char *newfile)
{
	QofBackendError io_error;
	gboolean uh_oh;
	const char *fmt;

	uh_oh = TRUE;
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
	fprintf (stderr, "\n");
}

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
qof_get_param_list(QofIdTypeConst object_type, QofType param_type)
{
	GSList *param_list;
	char *i;
	struct param_ref_list p;

	param_list = NULL;
	p.slist = NULL;
	p.i = 0;
	g_return_val_if_fail(object_type != NULL, NULL);
	p.param_type = g_strdup(param_type);
	qof_class_param_foreach(object_type, find_param_cb, &p);
	param_list = g_slist_copy(p.slist);
	i = g_strdup(object_type);
	return param_list;
}

void
qof_data_free(qof_data *data)
{
	g_free(data->filename);
	g_free(data->write_file);
	g_free(data->sql_file);
	g_free(data->sql_str);
	g_free(data->database);
	g_free(data->shortname);
}

/** \name SQL handlers
@{
*/
static QofQuery*
qof_main_run_sql(qof_data *context)
{
	QofSqlQuery *q;
	gchar *sql;
	QofQuery *qq;

	q = qof_sql_query_new();
	sql = g_strdup(context->sql_str);
	qof_sql_query_parse(q, sql);
	qq = qof_sql_query_get_query(q);
	return qq;
}

static void
qof_main_run_query(QofQuery *qq, qof_data *context)
{
	QofBook *book;
	GList *results;

	g_return_if_fail(qq);
	results = NULL;
	book = qof_session_get_book(context->input_session);
	qof_query_set_book(qq, book);
	qof_query_set_sort_order(qq, NULL, NULL, NULL);
	results = qof_query_run (qq);
#ifdef PRINT_DEBUG
	qof_query_print(qq);
#endif
	if(results == NULL) { return; }
	if(results != NULL) {
		qof_entity_copy_list(context->export_session, results);
	}
}

/** \brief Assemble the components of the query.

If any SQL statements are found, run
separately from any -c, -d or -t options.

All queries are additive: Successive queries add
more entities to the result set but no entity is
set more than once.
*/
static void
qof_moderate_query(qof_data *context)
{
	Timespec min_ts;
	Timespec max_ts;
	QofQueryPredData *date_pred_data;
//	QofQueryPredData *category_pred;
	QofQuery *q;
	QofIdTypeConst find;
//	char *buf;
	GSList *date_param_list, *category_param_list;
	GList *f;
	gboolean all;

	all = TRUE;
	q = qof_query_create();
	date_param_list = NULL;
	category_param_list = NULL;
	for (f = context->sql_list; f ; f = context->sql_list->next)
	{
		context->sql_str = g_strdup(f->data);
		q = qof_main_run_sql(context);
		qof_main_run_query(q, context);
		if(q) { qof_query_clear(q); }
		g_free(context->sql_str);
		all = FALSE;
	}
	if(0 < g_list_length(context->sql_list)) {
		context->sql_str = NULL;
		g_list_free(context->sql_list);
		all = FALSE;
	}
	if(context->sql_str != NULL) {
		q = qof_main_run_sql(context);
		qof_main_run_query(q, context);
		if(q) { qof_query_clear(q); }
		all = FALSE;
	}
	if((context->database != NULL)&&(qof_class_is_registered(context->database)))
	{
		qof_query_search_for(q, context->database);
		find = qof_query_get_search_for(q);
		if(context->min_ts.tv_sec > 0) {
			min_ts = context->min_ts;
			max_ts = context->max_ts;
			date_param_list = g_slist_copy(qof_get_param_list(find, QOF_TYPE_DATE));
			if(!date_param_list) { qof_query_clear(q); return;}
			date_pred_data = qof_query_date_predicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, min_ts);
			qof_query_add_term(q, date_param_list, date_pred_data, QOF_QUERY_AND);
			date_param_list = qof_get_param_list(qof_query_get_search_for(q), QOF_TYPE_DATE);
			date_pred_data = qof_query_date_predicate(QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, max_ts);
			qof_query_add_term(q, date_param_list, date_pred_data, QOF_QUERY_AND);
		}
		qof_main_run_query(q, context);
		if(q) { qof_query_clear(q); }
		all = FALSE;
	}
	if(all == TRUE)
	{
		while(context->all_objects)
		{
			q = qof_query_create_for(context->all_objects->data);
		find = qof_query_get_search_for(q);
			if(context->min_ts.tv_sec > 0) {
				min_ts = context->min_ts;
				max_ts = context->max_ts;
				date_param_list = g_slist_copy(qof_get_param_list(find, QOF_TYPE_DATE));
				if(!date_param_list) {
					if(q) { qof_query_clear(q); }
					context->all_objects = context->all_objects->next;
					continue;
				}
				date_pred_data = qof_query_date_predicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, min_ts);
				qof_query_add_term(q, date_param_list, date_pred_data, QOF_QUERY_AND);
				date_param_list = qof_get_param_list(qof_query_get_search_for(q), QOF_TYPE_DATE);
				date_pred_data = qof_query_date_predicate(QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, max_ts);
				qof_query_add_term(q, date_param_list, date_pred_data, QOF_QUERY_AND);
			}
			qof_main_run_query(q, context);
			if(q) { qof_query_clear(q); }
			context->all_objects = context->all_objects->next;
		}
	}
}
/** @} */

static void
print_config_cb (QofBackendOption *option, gpointer data)
{
	fprintf (stdout, "option name=%s\n", option->option_name);
	fprintf (stdout, "option desc=%s\n", option->description);
	fprintf (stdout, "option tip =%s\n", option->tooltip);
	switch(option->type) {
		case KVP_TYPE_GINT64   : {
			fprintf (stdout, "option value=%" G_GINT64_FORMAT,
				*(gint64*)option->value);
			fprintf (stdout, "\noption type=%s\n", QOF_TYPE_INT64);
			break;
		}
		case KVP_TYPE_DOUBLE   : {
			break; 
		}
		case KVP_TYPE_NUMERIC  : {
			break; 
		}
		case KVP_TYPE_STRING   : {
			fprintf (stdout, "option value=%s\n", (char*)option->value);
			fprintf (stdout, "option type=%s\n", QOF_TYPE_STRING);
			break;
		}
		case KVP_TYPE_GUID     : { break; } /* unsupported */
		case KVP_TYPE_TIMESPEC : {
			break;
		}
		case KVP_TYPE_BINARY   : { break; } /* unsupported */
		case KVP_TYPE_GLIST    : { break; } /* unsupported */
		case KVP_TYPE_FRAME    : { break; } /* unsupported */
	}		
}

void
qof_cmd_offline (qof_data *context)
{
	QofSession *input_session, *export_session;
	gchar current_work[PATH_MAX];
	gchar *temp;
	QofBackend *be;
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
	context->book = qof_session_get_book(input_session);
	be = qof_book_get_backend(context->book);
	backend_config = qof_backend_get_config(be);
	PINFO (" trying to get backend config");
	if(backend_config) 
	{
		qof_backend_option_foreach(backend_config, 
			print_config_cb, context);
	}
	else { PINFO (" failed"); }
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
	qof_moderate_query(context);
	qof_session_save(export_session, NULL);
	qof_show_error(export_session, context->write_file);
	qof_show_error(input_session, context->filename);
	qof_session_end(input_session);
	qof_session_end(export_session);
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

void
qof_select_all(QofObject *obj, gpointer data)
{
	qof_data *context;
	char* type;

	context = (qof_data*)data;
	type = g_strdup(obj->e_type);
	if(!qof_class_is_registered(type)) { return; }
	context->all_objects = g_list_prepend(context->all_objects, type);
}

void
qof_cmd_explain (gpointer user_data)
{
	qof_data *context;

	context = (qof_data*)user_data;
	if(context->error) { return; }
	fprintf(stdout, _("\nParameters of the %s database:\n\n"), context->database);
	qof_class_param_foreach(context->database, qof_explain_cb, NULL);
	fprintf(stdout, _("\nThank you for using %s\n\n"), PACKAGE);
}

void
qof_mod_database (const char *database, qof_data *data)
{
	if(qof_class_is_registered(database)) {
		data->database = g_strdup(database);
	}
}

void
qof_mod_timespec (const char *date_time, qof_data *data)
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
qof_mod_exclude (const char *exclude, qof_data *data)
{
	if(qof_class_is_registered(exclude)) {
		data->exclude = g_strdup(exclude);
	}
}

void
qof_mod_sql (const char *sql_query, qof_data *data)
{
	if(!qof_check_sql(sql_query)) { return; }
	data->sql_str = g_strdup(sql_query);
}

void
qof_mod_sql_file (const char *sql_file, qof_data *data)
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
qof_mod_write (const char *write_file, qof_data *data)
{
	data->write_file = g_strdup(write_file);
}

void extensions_init(void)
{
	backend_extensions = g_hash_table_new(g_str_hash, g_str_equal);
}

void qof_backend_extension_add(char *IDstring, gpointer data)
{
	g_hash_table_insert(backend_extensions, IDstring, data);
}

gpointer qof_backend_extension(const char* IDstring)
{
	gpointer func;

	func = g_hash_table_lookup(backend_extensions, IDstring);
	if(func) { return func; }
	return NULL;
}

/** @} */
/** @} */
