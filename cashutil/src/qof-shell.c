/***************************************************************************
 *            qof-shell.c
 *
 *  Mon Nov 28 13:02:40 2005
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

#include <unistd.h>
#include <regex.h>
#include "config.h"
#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif
#include "qof-shell.h"
#include "qofundo-p.h"

/* unused
static QofLogModule log_module = QOF_SHELL_CLI;
*/

void
qof_shell_free (qof_shell_context *context)
{
	qof_main_free(&context->qof);
	context->shortname = NULL;
	if(context->select_table)
	{
		g_hash_table_destroy(context->select_table);
	}
}

/** standardise tab output to help output line up nicely. */
#define QOF_TAB "    "

/** the maximum number of entities to offer for a single selection.

\todo Improve the maximum to allow scrolling / next page.
*/
#define CLI_MAX_SELECT 20

/** Relate the command to the function. */
typedef int (*cmd_fcn) (qof_shell_context *context);
/** \todo Reorganise the C into multiple files. */
static int qof_parse_command (const char *cmd, qof_shell_context *context);

/** \brief write [filename]

write the current book to file.
	If no filename is provided, save to the original file
	(to STDOUT if STDIN used). Analagous to Save / Save As...

\todo fix problems with writing to an alternative file.
*/
static int write_fcn(qof_shell_context *context)
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
		qof_session_swap_data(save_as, context->qof.input_session);
		qof_session_save(save_as, NULL);
		qof_main_show_error(save_as);
		qof_session_end(save_as);
		return 0;
	}
	if(context->qof.write_file)
	{
		qof_session_save(context->qof.export_session, NULL);
		qof_main_show_error(context->qof.export_session);
	}
	else {
		qof_session_save(context->qof.input_session, NULL);
		qof_main_show_error(context->qof.input_session);
	}
	/* implement in the backend eventually */
	context->book = qof_session_get_book(context->qof.input_session);
	if(qof_book_can_undo(context->book)) 
	{ 
		qof_book_clear_undo(context->book); 
	}
	return 0;
}

/** Run a sub-shell to either select or edit an instance. */
static int
qof_sub_shell (qof_shell_context *context)
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

/** \brief sql - take a SQL statement.

\todo improve the sql_parser - it is very noisy and
yet not very helpful.
*/
static int sql_fcn(qof_shell_context *context)
{
	QofSqlQuery *q;
	gchar *sql, *temp;
	gint result;
	QofQuery *qq;
	QofBook *book;
	GList *results;

	q = qof_sql_query_new();
	sql = g_strdup(context->qof.sql_str);
	if(!sql) { sql = context->argv[1]; }
	qof_sql_query_parse(q, sql);
	qq = qof_sql_query_get_query(q);
	book = qof_session_get_book(context->qof.input_session);
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

gboolean
qof_check_sql(const char *sql)
{
	regex_t *r;
	int reg_exp_check;
	static char *pattern = QOF_SQL_SUPPORTED;
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

/** Provide a simple numerical index for selectable objects.

@param ent  each entity in turn of the selected type.
@param data The qof_data context for the CLI.

Ensure counter is reset to zero between runs.
*/
static void
cli_select_cb (QofEntity *ent, gpointer data)
{
	qof_shell_context *context;
	gchar *temp;

	context = (qof_shell_context*)data;
	g_return_if_fail(context);
	context->counter++;
	if(context->counter >= CLI_MAX_SELECT) { return; }
	fprintf (stdout, "%d    %s\n", context->counter, 
		qof_object_printable(context->inst_type, (QofInstance*)ent));
	temp = g_strdup_printf("%d", context->counter);
	g_hash_table_insert(context->select_table, temp, ent);
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

/** select an instance

Offers multiple entities for selection. Ensure that
check_for_single_instance has been run first.

\todo Offer multiple listings of CLI_MAX_SELECT each.
*/
static int select_fcn (qof_shell_context *context)
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

/** Shortcut if only one entity of this type exists. */
static gboolean
check_for_single_instance (qof_shell_context *context)
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

/** \brief add a new instance and edit the values

Expects the second argument (context->argv[1]) to be the name of a registed object.
*/
static int add_fcn(qof_shell_context *context)
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
static int edit_fcn(qof_shell_context *context)
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
static int delete_fcn(qof_shell_context *context)
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
	fprintf(stdout, "%-20s%s\n", obj->e_type, obj->type_label);
}

static int list_fcn(qof_shell_context *context)
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
	qof_shell_context *context;

	context = (qof_shell_context*)data;
	g_return_if_fail(context);
	str = qof_book_merge_param_as_string(param, (QofEntity*)context->instance);
	fprintf (stdout, _("%-24s %-12s %s\n"), param->param_name, param->param_type, str);
}

/** \brief print an instance to the terminal. */
static int print_fcn(qof_shell_context *context)
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
static int explain_fcn(qof_shell_context *context)
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
static int load_fcn(qof_shell_context *context)
{
	char* answer;

	answer = g_strdup(" ");
	if(qof_book_not_saved(context->book))
	{
		fprintf (stderr, _("%s: The current book has not been saved.\n"
		"Do you still want to overwrite it? [y/n]\n"), PACKAGE);
		scanf("%1s", answer);
		if((0 != safe_strcmp(answer, "y")) && (0 != safe_strcmp(answer, "Y"))) {
			qof_session_save(context->qof.input_session, NULL);
		}
	}
	qof_session_end(context->qof.input_session);
	context->qof.input_session = qof_session_new();
	if(context->argv[1]) {
		context->qof.filename = g_strdup(context->argv[1]);
	}
	if(context->qof.filename) {
		qof_session_begin(context->qof.input_session, context->qof.filename, FALSE, TRUE);
		qof_mod_compression(context->qof.gz_level, &context->qof);
	}
	else {
		qof_session_begin(context->qof.input_session, QOF_STDOUT, FALSE, FALSE);
	}
	qof_session_load(context->qof.input_session, NULL);
	context->book = qof_session_get_book(context->qof.input_session);
	qof_main_show_error(context->qof.input_session);
	/* implement in the backend eventually */
	qof_book_clear_undo(context->book);
	return 0;
}

/** check if book is dirty, show errors (if any) and exit */
static int exit_fcn(qof_shell_context *context)
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
			qof_session_save(context->qof.input_session, NULL);
		}
	}
	if(qof_book_not_saved(qof_session_get_book(context->qof.export_session)))
	{
		qof_main_show_error(context->qof.export_session);
	}
	qof_main_show_error(context->qof.input_session);
	qof_session_end(context->qof.input_session);
	if(context->qof.export_session) { qof_session_end(context->qof.export_session); }
	fprintf (stdout, _("\nThank you for using %s.\n"), PACKAGE);
	return -1;
}

static int help_fcn(qof_shell_context *context)
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

/** \name Edit shell functions
@{
*/

/** \brief Set a value in a parameter

\todo improve error handling - currently it is almost silent.

Boolean values accept TRUE, 1, or translated values for Y, YES or TRUE
and are matched case insensitively, so y, yes and true (and their translations)
also match. If the match fails, false is set.

*/
static int set_edit_fcn (qof_shell_context *context)
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
static int explain_edit_fcn (qof_shell_context *context)
{
	fprintf (stdout, _("\nEditable parameters of the %s object:\n\n"), context->inst_type);
	qof_class_param_foreach(context->inst_type, qof_explain_cb, NULL);
	fprintf (stdout, "\n\n");
	return 0;
}

/** \brief Commit data to instance and leave the sub-shell.

\todo Should this clear the undo??
*/
static int commit_edit_fcn (qof_shell_context *context)
{
	if(qof_book_can_undo(context->book)) { qof_book_clear_undo(context->book); }
	return -1;
}

/** print the instance being edited.*/
static int print_edit_fcn (qof_shell_context *context)
{
	if(!context->inst_type) { return 0; }
	fprintf (stdout, "%-24s %-12s %s\n\n", _("Name"), _("Type"), _("Value"));
	qof_class_param_foreach (context->inst_type, qof_print_cb, context);
	fprintf (stdout, _("\nNot all parameters are editable, use 'explain' "
	"to see the list of editable parameters for the object '%s'.\n\n"), context->inst_type);
	return 0;
}

static int help_edit_fcn (qof_shell_context *context)
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

static int quit_edit_fcn (qof_shell_context *context)
{
	if(qof_book_can_undo(context->book)) { qof_book_undo(context->book); }
	return -1;
}

/** @} */
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
static int merge_fcn(qof_shell_context *context)
{
	qof_book_mergeData *mergeData;
	QofBook *importBook, *targetBook;
	gint result;

	/** \todo check the filename can be read. */
	if(!context->argv[1]) { return 0; }
	qof_session_begin(context->qof.export_session, context->argv[1], FALSE, FALSE);
	qof_session_load(context->qof.export_session, NULL);
	qof_main_show_error(context->qof.export_session);
	importBook = qof_session_get_book(context->qof.export_session);
	targetBook = context->book;
	mergeData = qof_book_mergeInit(importBook, targetBook);
	g_return_val_if_fail((mergeData != NULL), -1);
	qof_book_mergeRuleForeach(mergeData, qof_merge_loop, MERGE_REPORT);
	result = qof_book_mergeCommit(mergeData);
	return result;
}

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

/** \brief Relate the command to the function.

\todo Stop changing the input with strtoke ?
*/
static int
qof_parse_command (const char *cmd, qof_shell_context *context)
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
qof_cmd_shell(qof_shell_context *context)
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
	input_session = context->qof.input_session;
	if(0 == safe_strcmp(context->qof.exclude, context->qof.database)
		&&(context->qof.exclude != NULL))
	{
		fprintf(stderr, _("%s: Error: Cannot exclude database \"%s\" with option -e\n"
		"    because option -d is set to the include the same database: \"%s\"\n"
		"Use the \'-l\' command to see the full list of supported databases.\n"),
			PACKAGE, context->qof.exclude, context->qof.database);
		qof_session_end(input_session);
		return;
	}
	if(context->qof.filename) {
		qof_session_begin(input_session, context->qof.filename, TRUE, TRUE);
		qof_session_load(input_session, NULL);
	}
	else { qof_session_begin(input_session, QOF_STDOUT, TRUE, FALSE); }
	qof_main_show_error(input_session);
	context->book = qof_session_get_book(context->qof.input_session);
	context->shell_type = TOP_SHELL;
	context->inst_type = NULL;
	context->instance = NULL;
	if(!context->shortname) { context->shortname = g_strndup(PACKAGE, 4); }
	context->argc = 0;
	fprintf (stdout, _("\nWelcome to the QOF interactive shell ...\n"));
	fprintf (stdout, " %s is free software; see the source for copying conditions.\n", 
		PACKAGE);
	fprintf (stdout, " There is NO warranty; not even MERCHANTABILITY or FITNESS\n");
	fprintf (stdout, " FOR A PARTICULAR PURPOSE.\n\n");
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
