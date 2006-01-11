/***************************************************************************
 *            gnucash2.c
 *
 *  Mon Dec 12 11:18:51 2005
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

#define _GNU_SOURCE
#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <qof.h>
#include <gmodule.h>
#include <sys/stat.h>
#include <gtk/gtkmain.h>
#include <libgnome/libgnome.h>
#include <libgnomeui/libgnomeui.h>
#include "qof-main.h"
#include "cashobjects.h"
#include "gnc-module.h"
#include "gnc-splash.h"
#include "gncla-dir.h"
#include "backend-bus.h"
#include "gnc-file.h"
#include "gnc-plugin-file-history.h"
#ifndef HAVE_SETENV
#include "setenv.h"
#endif

#define GNC_MOD_CLI "gnucash2-cli"
#define GNC_QOF_LOG "/tmp/gnucash.trace"
#define GNC_LIB_NAME "libgnc-backend-file.la"
#define GNC_LIB_INIT "gnc_provider_init"
#define GNC_MODULE_LOG "gnucash-modules"

/* used to print debug logs. */
static QofLogModule log_module = GNC_MOD_CLI;
static gboolean skip_auto = FALSE;
#define ARGUMENT_BAD_OPTION	17227

#define GNC_MAIN_OP(_) \
 	_(qof_op_noop, = 0) \
	_(qof_op_list,)     \
	_(qof_op_offline,)  \
	_(qof_op_category,) \
	_(qof_op_database,) \
	_(qof_op_timespec,) \
	_(qof_op_exclude,)  \
	_(qof_op_sql,)      \
	_(qof_op_sql_file,) \
	_(qof_op_write, )   \
	_(qof_op_explain,)  \
	_(qof_op_vers,)     \
	_(qof_op_compress,) \
	_(qof_op_guile,)    \
	_(qof_op_nofile,)   \
	_(qof_op_debug,)

	DEFINE_ENUM(qof_op_type, GNC_MAIN_OP)

static gboolean
load_bus_backend (const char *directory)
{
	typedef void (* bus_backend_init) (void);
	GModule *bus_backend;
	gchar *fullpath;
	bus_backend_init bus_init;
	gpointer g;

	g_return_val_if_fail(g_module_supported(), FALSE);
	fullpath = g_module_build_path(directory, "libgnc-backend-bus");
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

static qof_main_context*
gnc_cli_init(void)
{
	qof_main_context *gnc_cli;

	qof_init();
	/* register objects here */
	g_return_val_if_fail(cashobjects_register(), NULL);
	/* makes the business objects mandatory - fix. How? */
	g_return_val_if_fail(bus_cashobjects_register(), NULL);
	/* load the backends */
	g_return_val_if_fail((qof_load_backend_library 
		(QOF_LIB_DIR, QSF_BACKEND_LIB, QSF_MODULE_INIT)), NULL);
	g_return_val_if_fail((qof_load_backend_library
		(GNC_LIBDIR, GNC_LIB_NAME, GNC_LIB_INIT)), NULL);
	g_return_val_if_fail(load_bus_backend(GNC_LIBDIR), NULL);
	gnc_cli = g_new0(qof_main_context, 1);
	return gnc_cli;
}

static void
build_environment(void)
{
	gchar *gmp, *llp, *glibp, *ltdlp;

	gmp = g_strconcat(LIBDIR, "/gnucash:", 
		DATADIR, "/guile-modules:",
		DATADIR, "/scm:", 
		getenv("GNC_MODULE_PATH"), 
		NULL);
	glibp = g_strconcat(DATADIR, "/guile-modules:",
		DATADIR, "/scm:",
		getenv("GUILE_LOAD_PATH"), 
		NULL);
	llp = g_strconcat(LIBDIR, "/gnucash:", 
		LIBDIR, ":",
		getenv("LD_LIBRARY_PATH"), NULL);
	ltdlp = g_strconcat(gmp, LIBDIR, ":", LIBDIR, "/gnucash:", 
		getenv("LTDL_LIBRARY_PATH"), NULL);
	setenv("GNC_MODULE_PATH", gmp, 1);
	setenv("LD_LIBRARY_PATH", llp, 1);
	setenv("GUILE_LOAD_PATH", glibp, 1);
	setenv("LTDL_LIBRARY_PATH", ltdlp, 1);
	PINFO ("set GNC_MODULE_PATH %s LD_LIBRARY_PATH %s"
		" GUILE_LOAD_PATH %s LTDL_LIBRARY_PATH %s", gmp, llp, glibp, ltdlp);
}

static void
guile_main(void *closure, int argc, char ** argv)
{
	GncMainWindow *gnc_win2;

	/** autoloading.
	Some may need to be loaded in sequence but each should
	handle this internally - by calling it's own dependencies.
	Each can only be loaded once anyway.
	
	\note the engine module has been removed and replaced with
	gnc_cli_init - each gnc-mod has been modified to avoid 
	calling the old engine.
	*/
	gnc_module_load_all(GNC_MOD_INTERFACE_VERSION);
	/* handle --no-file */
	if(!skip_auto)
	{
		gchar *last_file;

		last_file = gnc_history_get_last();
		gnc_update_splash_screen(_("Loading data... "));
		gnc_file_open_file(last_file);
	}
	/** \todo Replace this scheme:
	    (gnc:update-splash-screen (_ "Checking Finance::Quote..."))
		gnc:fq-check-sources perl quotes/finance-quote-check
		(gnc:update-splash-screen (_ "Loading configs..."))
		(define (gnc:load-account-file)
		(let ((file (gnc:account-file-to-load)))
		(if file
        (begin
          (gnc:update-splash-screen (_ "Loading data..."))
          (and (not (gnc:file-open-file file))
             (begin
                (gnc:destroy-splash-screen)
                (gnc:new-user-dialog))
	*/
	gnc_win2 = gnc_main_window_new();
	gnc_main_window_set_progressbar_window(gnc_win2);
	gnc_destroy_splash_screen();
}

static int
qof_cmd_gui(int argc, char ** argv)
{
	GnomeProgram *gnucash2;

	gtk_init (&argc, &argv);
	build_environment();
	gnucash2 = gnome_program_init(PACKAGE, VERSION,
		LIBGNOMEUI_MODULE, argc, argv, 
		GNOME_PARAM_APP_PREFIX, PREFIX,
		GNOME_PARAM_APP_SYSCONFDIR, SYSCONFDIR,
		GNOME_PARAM_APP_DATADIR, DATADIR,
		GNOME_PARAM_APP_LIBDIR, GNC_LIBDIR,
		GNOME_PARAM_NONE);
	gnome_program_postinit(gnucash2);
	gnc_module_system_init();
	gnc_show_splash_screen();
	gnc_update_splash_screen(_("Loading modules... "));
	/* I only do this here because a module may call scm */
	scm_boot_guile(argc, (char **)argv, guile_main, NULL);
	return 0;
}

int
main (int argc, const char *argv[])
{
	const char *help_header_text, *input_file;
	qof_main_context *gnc_cli;
	gboolean debug_on;
	poptContext pc;
	int optc, gz_level;
	qof_op_type cli_command;
	QOF_OP_VARS

	struct poptOption options[] = {
		{"gui", 'g', POPT_ARG_NONE, NULL, qof_op_guile,
		_("Load the Gtk/guile graphic interface."
			"Options are ignored."), NULL},
		{"no-file", 0, POPT_ARG_NONE, NULL, qof_op_nofile,
		_("Omit automatic load of the last file. "
			"Requires -g"), NULL},
		QOF_CLI_OPTIONS
		POPT_TABLEEND
	};
	cli_command = qof_op_noop;
	debug_on = FALSE;
	skip_auto = FALSE;
	QOF_OP_INIT;
	input_file = NULL;

	help_header_text = _(
		"\n"
		"   GnuCash: Open Source Accounting Software \n"
		"   See http://www.gnucash.org/\n"
		"   Use --help for more detailed help or\n"
		"   -g to load the gnucash main window.\n"
		"   Use exactly one of -x -l -g --explain;\n"
		"   options are -c -t -w, -d or -e, -s or -f.\n\n");

	#ifdef ENABLE_NLS
	setlocale (LC_ALL, "");
	bindtextdomain (GETTEXT_PACKAGE, LOCALE_DIR);
	bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
	textdomain (GETTEXT_PACKAGE);
	#endif
	pc = poptGetContext (PACKAGE, argc, argv, options, 0);

	poptSetOtherOptionHelp (pc, help_header_text);

	if (argc < 2)
	{
		poptPrintUsage (pc, stderr, 0);
		return EXIT_FAILURE;
	}
	gnc_cli = gnc_cli_init();
	if(!gnc_cli) 
	{ 
		PERR (" Failed to initialise!");
		return EXIT_FAILURE; 
	}
	while ((optc = poptGetNextOpt (pc)) >= 0)
	{
		switch (optc)
		{
			/* commands - mutually exclusive */
			case qof_op_offline:
			case qof_op_list:
			case qof_op_explain:
			case qof_op_guile:
			{
				if (qof_op_noop != cli_command)
				{
					fprintf (stderr, _("%s: ERROR: specify only one of"
						" -x -l -g or --explain.\n"), PACKAGE);
					poptPrintUsage(pc, stderr, 0);
					return EXIT_FAILURE;
				}
				cli_command = optc;
				break;
			}
			case qof_op_vers :
			{
				fprintf (stdout, _("\n This is %s v%s\n"), PACKAGE, VERSION);
				fprintf (stdout, _(" Open Source Accounting Software.\n"));
				fprintf (stdout, "\n Copyright (c) 2005 "
					"Neil Williams <linux@codehelp.co.uk>\n");
				/* Translators: Add or subtract dots to keep the translated
				lines aligned vertically */
//				fprintf (stdout, _(" Build target.........: %s\n"), HOST_OS);
				fprintf (stdout, _(" Build date...........: %s %s\n"), 
					__DATE__, __TIME__);
				fprintf (stdout, _(" --debug logs to......: %s\n\n"), GNC_QOF_LOG);
				fprintf (stdout, _(" Please use --help for more detailed options.\n\n"));
				return EXIT_SUCCESS;
			}
			/* optional modifiers - store to act on later. */
			case qof_op_category:
			{
				qof_mod_category (category, gnc_cli);
				break;
			}
			case qof_op_database:
			{
				qof_mod_database (database, gnc_cli);
				break;
			}
			case qof_op_timespec:
			{
				qof_mod_timespec (date_time, gnc_cli);
				break;
			}
			case qof_op_exclude:
			{
				qof_mod_exclude (exclude, gnc_cli);
				break;
			}
			case qof_op_sql:
			{
				qof_mod_sql (sql_query, gnc_cli);
				break;
			}
			case qof_op_sql_file:
			{
				qof_mod_sql_file (sql_file, gnc_cli);
				break;
			}
			case qof_op_write:
			{
				qof_mod_write (write_file, gnc_cli);
				break;
			}
			case qof_op_debug:
			{
				qof_log_init_filename(GNC_QOF_LOG);
				qof_log_set_default(QOF_LOG_DETAIL);
				qof_log_set_level(QOF_MAIN_CLI, QOF_LOG_DETAIL);
				qof_log_set_level(QOF_MOD_QSF, QOF_LOG_DETAIL);
				qof_log_set_level(GNC_MOD_CLI, QOF_LOG_DETAIL);
				qof_log_set_level(GNC_MODULE_LOG, QOF_LOG_DETAIL);
				debug_on = TRUE;
				break;
			}
			case qof_op_nofile :
			{
				skip_auto = TRUE;
				break;
			}
			case qof_op_compress:
			{
				gnc_cli->gz_level = gz_level;
				break;
			}
			default:
			{
				fprintf (stderr, _("%s: ERROR: got option %d, arg %s\n"), PACKAGE,
					 optc, poptGetOptArg (pc));
				return EXIT_FAILURE;
			}
		}
	}
	if (optc < -1)
	{
		fprintf(stderr, "%s: %s %s\n\n", PACKAGE,
		poptBadOption(pc, POPT_BADOPTION_NOALIAS),
		poptStrerror(optc));
		poptPrintUsage(pc, stderr, 0);
		return EXIT_FAILURE;
	}
	/* If we get this far, we should have sensible options: start the work. */
	gnc_cli->input_session = qof_session_new();
	switch (cli_command)
	{
		case qof_op_offline:
		{
			gnc_cli->filename = g_strdup(filename);
			qof_cmd_xmlfile (gnc_cli);
			break;
		}
		case qof_op_list:
		{
			qof_cmd_list ();
			break;
		}
		case qof_op_explain:
		{
			if(!gnc_cli->database) 
			{ 
				fprintf (stderr, _("%s: Error: please specify the database to explain.\n\n"), PACKAGE);
				break;
			}
			qof_cmd_explain(gnc_cli);
			break;
		}
		case qof_op_guile :
		{
			/* load guile here */
			qof_cmd_gui(argc, (char**)argv);
			break;
		}
		default:
		{
			/* should be impossible */
			break;
		}
	}
	poptFreeContext(pc);
	g_free(gnc_cli);
	if(debug_on) { qof_log_shutdown(); }
	qof_close();
	return EXIT_SUCCESS;
}
