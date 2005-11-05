/***************************************************************************
 *            cashutil.c
 *
 *   Sat Feb  5 10:40:03 GMT 2005
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
    @{ */

/** @addtogroup cashutil A command line interface for GnuCash data

cashutil provides an executable interface to GnuCash data
It supports writing the QSF XML files and SQL-type queries.

The types of SQL queries that are allowed at this point are a little limited.
In general, only the following types of queries are supported: \n
SELECT * FROM SomeObj WHERE (param_a < 10.0) AND (param_b = "asdf") SORT BY param_c DESC;\n
INSERT INTO SomeObj (param_a, param_b, param_c) VALUES ("value_a", true, "0/1");

Joins are not supported directly.\n
SELECT * FROM ObjA,ObjB WHERE (ObjA.param_id = ObjB.param_other_id);\n
The problem with the above is that the search requires a nested search loop, aka a 'join',
which is not currently supported in the underlying QofQuery code.

However, by repeating queries and adding the entities to a new session using
::qof_entity_copy_list, a series of queries can be added to a single book.
e.g. You can insert multiple entities and save out as a QSF XML file or use multiple
SELECT queries to build a precise list - this can be used to replicate most of the
functionality of a SQL join.

SELECT * from ObjA where param_id = value; SELECT * from ObjB where param_other_id = value;

Equivalent to:\n
SELECT * from ObjA,ObjB where param_id = param_other_id and param_id = value;

When combined with a foreach callback on the value of param_id for each entity in the
QofBook, you can produce the effect of a join from running the two SELECT queries for each
value of param_id held in 'value'.

See ::QofEntityForeachCB and ::qof_object_foreach.

SELECT a,b,c FROM ...

Used to convert QOF objects between applications by using the returned parameter values
to create a second object. One application using QOF could register objects from two
applications and convert data from one to the other by using\n
SELECT a,b,c FROM ObjA; SELECT d,f,k FROM ObjB; qof_object_new_instance();
ObjC_set_a(value_c); ObjC_set_b(value_k) etc.

What's needed is for the SELECT to return a complete object that only contains the
parameters selected.

Unsupported: UPDATE, DELETE.

It will not be possible to support CREATE, AMEND or DROP for understandable reasons.

@{
*/
/** @file cashutil.c
    @brief Command line interface.
    @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <libintl.h>
#include <locale.h>
#include <errno.h>
#include <gmodule.h>
#include <sys/stat.h>
#include "config.h"
#include "qof-main.h"
#include "cashutil.h"
#include "cashobjects.h"
#include "gncla-dir.h"
#include "qofundo-p.h"
#include "backend-bus.h"

static gchar* log_module = CU_MOD_CLI;
static gboolean debug_on = FALSE;

static gboolean
load_bus_backend (const char *directory)
{
	typedef void (* bus_backend_init) (void);
	GModule *bus_backend;
	gchar *fullpath;
	struct stat sbuf;
	bus_backend_init bus_init;
	gpointer g;

	g_return_val_if_fail(g_module_supported(), FALSE);
	fullpath = g_module_build_path(directory, "libgnc-backend-bus.la");
	g_return_val_if_fail((stat(fullpath, &sbuf) == 0), FALSE);
	bus_backend = g_module_open(fullpath, G_MODULE_BIND_LAZY);
	if(!bus_backend) { 
		PWARN ("%s: %s\n", PACKAGE, g_module_error ()); 
		return FALSE;
	}
	g = &bus_init;
	if (!g_module_symbol (bus_backend, "backend_business_add", g))
	{
		PWARN ("%s: %s\n", PACKAGE, g_module_error ());
		return FALSE;
	}
	g_module_make_resident(bus_backend);
	bus_init();
	return TRUE;
}

int
main (int argc, const char *argv[])
{
	const char *exclude, *date_time, *database;
	const char *sql_file, *write_file, *sql_query, *filename;
	const char *help_header_text;
	gboolean use_stdin;
	qof_data *context;
	int optc, gz_level;
	poptContext pc;
	qof_op_type command;
	QofSession *session;
	FILE *f;

	struct poptOption options[] = {
		QOF_CLI_OPTIONS
		{"input", 'i', POPT_ARG_STRING, &filename, qof_op_input,
		 _("Load a GnuCash or QSF book from <filename>"), "filename"},
		POPT_TABLEEND
	};
	#ifdef ENABLE_NLS
	setlocale (LC_ALL, "");
	bindtextdomain (GETTEXT_PACKAGE, LOCALE_DIR);
	bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
	textdomain (GETTEXT_PACKAGE);
	#endif
	use_stdin = TRUE;
	command = qof_op_noop;
	exclude = NULL;
	database = NULL;
	sql_file = NULL;
	write_file = NULL;
	sql_query = NULL;
	filename = NULL;
	f = NULL;
	gnc_set_log_level_global(1);

	help_header_text = _(
		"\n"
		"   Command line query interface to GnuCash using QOF -\n"
		"   the Query Object Framework.\n"
		"   Supports reading personal and business GnuCash data and running \n"
		"   SQL-type queries on the live data or XML file. \n"
		"   Data can be added, edited, removed, printed, merged and written \n"
		"   out in the QOF interactive shell.\n"
		"   QSF XML can be imported into other QOF applications.\n\n"
		"   Use exactly one of -i -l --explain --shell; \n"
		"   options are -tde, with either -s or -f, then -w.\n\n");

	pc = poptGetContext (PACKAGE, argc, argv, options, 0);

	poptSetOtherOptionHelp (pc, help_header_text);

	if (argc < 2)
	{
		poptPrintUsage (pc, stderr, 0);
		return 1;
	}
	qof_init();
	g_return_val_if_fail(cashobjects_register(), -1);
        g_return_val_if_fail(bus_cashobjects_register(), -1);
	context = qof_create();
	if(!context) {
		fprintf(stderr, _("Fatal error: Cannot initialise QOF.\n\n"));
		return -1; 
	}
	session = qof_session_new ();
	context->book = qof_session_get_book(session);
	/* Read user alias settings */
	poptReadDefaultConfig(pc, 0);
	/* could use poptAddAlias(poptContext pc, struct poptAlias alias, 0) to specify
	 * a default alias, perhaps to ease GnuCash or PilotQOF usage. */
	while ((optc = poptGetNextOpt (pc)) >= 0)
	{
		switch (optc)
		{
			/* commands - mutually exclusive */
			case qof_op_input:
			case qof_op_list:
			case qof_op_explain:
			case qof_op_shell:
			{
				if (qof_op_noop != command)
				{
					fprintf (stderr, 
						_("%s: ERROR. Specify only one command out of -i, -l."), PACKAGE);
					fprintf (stderr, _("--explain or --shell\n"));
					return 1;
				}
				command = optc;
				use_stdin = FALSE;
				break;
			}
			case qof_op_vers :
			{
				fprintf (stdout, " %s v%s\n", PACKAGE, VERSION);
				fprintf (stdout, _(" GnuCash Command Line Interface.\n"));
				fprintf (stdout, _(" Build date....: %s %s\n\n"), __DATE__, __TIME__);
				fprintf (stdout, " Copyright 2005 Neil Williams <linux@codehelp.co.uk>\n");
				fprintf (stdout, " %s is free software; see the source for copying conditions.\n", PACKAGE);
				fprintf (stdout, " There is NO warranty; not even MERCHANTABILITY or FITNESS\n");
				fprintf (stdout, " FOR A PARTICULAR PURPOSE.\n\n");
				fprintf (stdout, _(" For CashUtil support, join the QOF-devel mailing list at\n"));
				fprintf (stdout, " http://lists.sourceforge.net/mailman/listinfo/qof-devel\n");
				fprintf (stdout, _("   Please use --help for more detailed options.\n\n"));
				return 0;
			}
			/* optional modifiers - store to act on later. */
			case qof_op_database:
			{
				qof_mod_database (database, context);
				break;
			}
			case qof_op_timespec:
			{
				qof_mod_timespec (date_time, context);
				break;
			}
			case qof_op_exclude:
			{
				qof_mod_exclude (exclude, context);
				break;
			}
			case qof_op_sql:
			{
				qof_mod_sql (sql_query, context);
				break;
			}
			case qof_op_sql_file:
			{
				qof_mod_sql_file (sql_file, context);
				break;
			}
			case qof_op_write:
			{
				qof_mod_write (write_file, context);
				break;
			}
			case qof_op_compress:
			{
				context->gz_level = gz_level;
				break;
			}
			case qof_op_debug:
			{
				debug_on = TRUE;
				break;
			}
			default:
			{
				fprintf (stderr, _("%s: ERROR. Unknown option %d, argument: %s\n"),
					 PACKAGE, optc, poptGetOptArg (pc));
				return 1;
			}
		}
	}
	if (use_stdin && command == qof_op_noop) {
		fprintf (stderr, _("%s: Please specify only one command out of -i, -l."), PACKAGE);
		fprintf (stderr, _("--explain or --shell\n"));
		poptPrintUsage(pc, stderr, 0);
		return 1;

	}
	if (use_stdin) {
		fprintf (stderr, _("%s: Sorry, %s cannot yet read STDIN.\n"), PACKAGE, PACKAGE);
		poptPrintUsage(pc, stderr, 0);
		return 1;
	}
	if (optc < -1)
	{
		fprintf(stderr, "%s: %s %s\n\n", PACKAGE,
		poptBadOption(pc, POPT_BADOPTION_NOALIAS),
		poptStrerror(optc));
		poptPrintUsage(pc, stderr, 0);
		return 1;
	}
	/* If we get this far, we should have sensible options: start the work. */
	if(debug_on) {
		f = fopen("/tmp/cashutil.trace", "w");
		gnc_set_logfile(f);
		gnc_log_init();
		qof_log_set_default(GNC_LOG_DETAIL);
		gnc_set_log_level(CU_MOD_CLI, GNC_LOG_DETAIL);
		gnc_set_log_level(CU_MOD_ENGINE, GNC_LOG_DETAIL);
	}
	g_return_val_if_fail((qof_load_backend_library 
		(QOF_LIB_DIR, "libqof-backend-qsf.la", "qsf_provider_init")), -1);
	g_return_val_if_fail((qof_load_backend_library
		(GNC_LIBDIR, GNC_LIB_NAME, GNC_LIB_INIT)), -1);
	g_return_val_if_fail(load_bus_backend(GNC_LIBDIR), -1);
	context->input_session = session;
	qof_book_clear_undo(context->book);
	qof_object_foreach_type(qof_select_all, context);
	switch (command)
	{
		case qof_op_input:
		{
			context->filename = g_strdup(filename);
			qof_cmd_offline (context);
			break;
		}
		case qof_op_list:
		{
			qof_cmd_list ();
			break;
		}
		case qof_op_explain:
		{
			if(!context->database)
			{
				fprintf (stderr, 
					_("%s: Error. please specify the database to explain.\n\n"), 
					PACKAGE);
				break;
			}
			qof_cmd_explain(context);
			break;
		}
		case qof_op_shell:
		{
			qof_cmd_shell(context);
			break;
		}
		default:
		{
			/* should be impossible */
			break;
		}
	}
	poptFreeContext(pc);
	qof_data_free(context);
	qof_close();
	if(f) { fclose(f); }
	return 0;
}

/** @} */
/** @} */
