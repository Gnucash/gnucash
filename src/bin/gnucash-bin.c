/*
 * gnucash-bin.c -- The program entry point for GnuCash
 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */
#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libguile.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libgnome/libgnome.h>
#include "glib.h"
#include "gnc-module.h"
#include "gnc-path.h"
#include "binreloc.h"
#include "gnc-version.h"
#include "gnc-engine.h"
#include "gnc-filepath-utils.h"
#include "gnc-file.h"
#include "gnc-hooks.h"
#include "top-level.h"
#include "gfec.h"
#include "gnc-commodity.h"
#include "gnc-main.h"
#include "gnc-main-window.h"
#include "gnc-splash.h"
#include "gnc-gnome-utils.h"
#include "gnc-plugin-file-history.h"
#include "gnc-gconf-utils.h"
#include "dialog-new-user.h"
#include "gnc-session.h"
#include "engine-helpers.h"
#include "swig-runtime.h"

#ifdef HAVE_GETTEXT
#  include <libintl.h>
#  include <locale.h>
#endif

#define APP_GNUCASH "/apps/gnucash"

/* GNUCASH_SVN is defined whenever we're building from an SVN tree */
#ifdef GNUCASH_SVN
static int is_development_version = TRUE;
#else
static int is_development_version = FALSE;
#endif

/* Command-line option variables */
static int gnucash_show_version = 0;
static const char *add_quotes_file = NULL;
static int nofile = 0;
static const char *file_to_load = NULL;
static gchar **log_flags = NULL;
static gchar *log_to_filename = NULL;

static void
gnc_print_unstable_message(void)
{
    if (!is_development_version) return;

    g_print("\n\n%s%s%s%s%s\n%s%s\n\n",
            _("This is a development version. It may or may not work.\n"),
            _("Report bugs and other problems to gnucash-devel@gnucash.org.\n"),
            _("You can also lookup and file bug reports at http://bugzilla.gnome.org\n"),
            _("The last stable version was "), "GnuCash 2.2.9",
            _("The next stable version will be "), "GnuCash 2.4");
}

/* Priority of paths: The default is set at build time.  It may be
   overridden by environment variables, which may, in turn, be
   overridden by command line options.  */
static char *config_path = PKGSYSCONFDIR;
static char *share_path = PKGDATADIR;
static char *help_path = GNC_HELPDIR;
static char *gconf_path = APP_GNUCASH;

static gchar  *environment_expand(gchar *param)
{
    gchar *search_start;
    gchar *opening_brace;
    gchar *closing_brace;
    gchar *result;
    gchar *tmp;
    gchar *expanded=NULL;

    if (!param)
        return NULL;

    /* Set an initial return value, so we can always use g_strconcat below) */
    result=g_strdup ("x");

    /* Look for matching pairs of { and }. Anything in between should be expanded */
    search_start = param;
    opening_brace = g_strstr_len (search_start, -1, "{");
    closing_brace = g_strstr_len (search_start, -1, "}");

    /* Note: the test on valid braces is fairly simple:
     *       * if no pair of opening/closing braces is found, no expansion occurs
     *       * braces can't be nested, this will give unexpected results
     *       * the string should contain no other braces than those used to mark
     *         expandable variables, or unexpected results will be returned.
     */
    while ( opening_brace && closing_brace && (closing_brace > opening_brace) )
    {
        /* Found a first matching pair */
        gchar *to_expand;
        const gchar *env_val;

        /* If the string had characters before the opening {, copy them first */
        if (opening_brace > search_start)
        {
            gchar *prefix = g_strndup (search_start, opening_brace - search_start);

            tmp = g_strconcat (result, prefix, NULL);
            g_free (result);
            result = tmp;
            g_free (prefix);
        }

        /* Expand the variable  we found and append it to the result */
        to_expand = g_strndup (opening_brace + 1, closing_brace - opening_brace -1);
        env_val = g_getenv (to_expand);
        tmp = g_strconcat (result, env_val, NULL);
        g_free (result);
        result = tmp;
        g_free (to_expand);

        /* Look for matching pairs of { and }. Anything in between should be expanded */
        search_start = closing_brace + 1;
        opening_brace = g_strstr_len (search_start, -1, "{");
        closing_brace = g_strstr_len (search_start, -1, "}");
    }

    /* No more braces found, append the remaining characters */
    tmp = g_strconcat (result, search_start, NULL);
    g_free (result);
    result = tmp;

    /* Remove the "x" from our result */
    if (g_strcmp0 (result, "x"))
        expanded = g_strdup (result + 1);
    g_free (result);

    return expanded;
}

static void
environment_override()
{
    const gchar *path;
    gchar *env_file;
    GKeyFile    *keyfile=g_key_file_new();
    GError      *error;
    gchar **env_vars;
    gsize param_count;
    gint i;
    gboolean got_keyfile;

    if ((path = g_getenv("GNC_CONFIG_PATH")))
        config_path = g_strdup(path);
    if ((path = g_getenv("GNC_SHARE_PATH")))
        share_path = g_strdup(path);
    if ((path = g_getenv("GNC_DOC_PATH")))
        help_path = g_strdup(path);
    if ((path = g_getenv("GNC_GCONF_PATH")))
        gconf_path = g_strdup(path);
#ifdef G_OS_WIN32
    {
        /* unhide files without extension */
        gchar *pathext = g_build_path(";", ".", g_getenv("PATHEXT"),
                                      (gchar*) NULL);
        g_setenv("PATHEXT", pathext, TRUE);
        g_free(pathext);
    }
#endif

    env_file = g_strjoin(G_DIR_SEPARATOR_S, config_path, "environment", NULL);
    got_keyfile = g_key_file_load_from_file (keyfile, env_file, G_KEY_FILE_NONE, &error);
    g_free (env_file);
    if ( !got_keyfile )
        return;

    /* Read the environment overrides and apply them */
    env_vars = g_key_file_get_keys(keyfile, "Variables", &param_count, &error);
    for ( i = 0; i < param_count; i++ )
    {
        gchar **val_list;
        gsize val_count;
        gint j;
        gchar *new_val, *tmp_val;

        /* For each variable, read its new value, optionally expand it and (un)set it */
        val_list = g_key_file_get_string_list (keyfile, "Variables",
                                               env_vars[i], &val_count,
                                               &error );
        if (!val_list)
            g_unsetenv (env_vars[i]);
        else
        {
            /* Set an initial return value, so we can always use g_build_path below) */
            tmp_val = g_strdup ("x");
            for ( j = 0; j < val_count; j++ )
            {
                gchar *expanded = environment_expand (val_list[j]);
                new_val = g_build_path (G_SEARCHPATH_SEPARATOR_S, tmp_val, expanded, NULL);
                g_free (tmp_val);
                tmp_val = new_val;
            }
            g_strfreev (val_list);

            /* Remove the "x" from our result */
            if (g_strcmp0 (tmp_val, "x"))
                new_val = g_strdup (tmp_val + sizeof (G_SEARCHPATH_SEPARATOR_S));
            g_free (tmp_val);

            if (!g_setenv (env_vars[i], new_val, TRUE))
                g_warning ("Couldn't properly override environment variable \"%s\". "
                           "This may lead to unexpected results", env_vars[i]);
        }
    }

    g_strfreev(env_vars);

}

static gboolean
try_load_config_array(const gchar *fns[])
{
    gchar *filename;
    int i;

    for (i = 0; fns[i]; i++)
    {
        filename = gnc_build_dotgnucash_path(fns[i]);
        if (gfec_try_load(filename))
        {
            g_free(filename);
            return TRUE;
        }
        g_free(filename);
    }
    return FALSE;
}

static void
update_message(const gchar *msg)
{
    gnc_update_splash_screen(msg, GNC_SPLASH_PERCENTAGE_UNKNOWN);
    g_message("%s", msg);
}

static void
load_system_config(void)
{
    static int is_system_config_loaded = FALSE;
    gchar *system_config;

    if (is_system_config_loaded) return;

    update_message("loading system configuration");
    /* FIXME: use runtime paths from gnc-path.c here */
    system_config = g_build_filename(config_path, "config", NULL);
    is_system_config_loaded = gfec_try_load(system_config);
    g_free(system_config);
}

static void
load_user_config(void)
{
    /* Don't continue adding to this list. When 2.0 rolls around bump
       the 1.4 (unnumbered) files off the list. */
    static const gchar *user_config_files[] =
    {
        "config-2.0.user", "config-1.8.user", "config-1.6.user",
        "config.user", NULL
    };
    static const gchar *auto_config_files[] =
    {
        "config-2.0.auto", "config-1.8.auto", "config-1.6.auto",
        "config.auto", NULL
    };
    static const gchar *saved_report_files[] =
    {
        "saved-reports-2.4", "saved-reports-2.0", NULL
    };
    static const gchar *stylesheet_files[] = { "stylesheets-2.0", NULL};
    static int is_user_config_loaded = FALSE;

    if (is_user_config_loaded)
        return;
    else is_user_config_loaded = TRUE;

    update_message("loading user configuration");
    try_load_config_array(user_config_files);
    update_message("loading auto configuration");
    try_load_config_array(auto_config_files);
    update_message("loading saved reports");
    try_load_config_array(saved_report_files);
    update_message("loading stylesheets");
    try_load_config_array(stylesheet_files);
}

/* Parse command line options, using GOption interface */

static void
gnucash_command_line(int *argc, char **argv)
{
    char *p;
    int debugging = 0, extra = 0;
    char *namespace_regexp = NULL;
    GError *error = NULL;
    GOptionContext *context;
    GOptionEntry options[] =
    {
        {
            "version", 'v', 0, G_OPTION_ARG_NONE, &gnucash_show_version,
            _("Show GnuCash version"), NULL
        },

        {
            "debug", '\0', 0, G_OPTION_ARG_NONE, &debugging,
            _("Enable debugging mode: increasing logging to provide deep detail."), NULL
        },

        {
            "extra", '\0', 0, G_OPTION_ARG_NONE, &extra,
            _("Enable extra/development/debugging features."), NULL
        },

        {
            "log", '\0', 0, G_OPTION_ARG_STRING_ARRAY, &log_flags,
            _("Log level overrides, of the form \"log.ger.path={debug,info,warn,crit,error}\""),
            NULL
        },

        {
            "logto", '\0', 0, G_OPTION_ARG_STRING, &log_to_filename,
            _("File to log into; defaults to \"/tmp/gnucash.trace\"; can be \"stderr\" or \"stdout\"."),
            NULL
        },

        {
            "nofile", '\0', 0, G_OPTION_ARG_NONE, &nofile,
            _("Do not load the last file opened"), NULL
        },

        {
            "config-path", '\0', 0, G_OPTION_ARG_STRING, &config_path,
            _("Set configuration path"),
            /* Translators: Argument description for autohelp; see
               http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            _("CONFIGPATH")
        },

        {
            "share-path", '\0', 0, G_OPTION_ARG_STRING, &share_path,
            _("Set shared data file search path"),
            /* Translators: Argument description for autohelp; see
               http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            _("SHAREPATH")
        },
        {
            "doc-path", '\0', 0, G_OPTION_ARG_STRING, &help_path,
            _("Set the search path for documentation files"),
            /* Translators: Argument description for autohelp; see
               http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            _("DOCPATH")
        },
        {
            "gconf-path", '\0', 0, G_OPTION_ARG_STRING, &gconf_path,
            _("Set the prefix path for gconf queries"),
            /* Translators: Argument description for autohelp; see
               http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            _("GCONFPATH")
        },
        {
            "add-price-quotes", '\0', 0, G_OPTION_ARG_STRING, &add_quotes_file,
            _("Add price quotes to given GnuCash datafile"),
            /* Translators: Argument description for autohelp; see
               http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            _("FILE")
        },
        {
            "namespace", '\0', 0, G_OPTION_ARG_STRING, &namespace_regexp,
            _("Regular expression determining which namespace commodities will be retrieved"),
            /* Translators: Argument description for autohelp; see
               http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            _("REGEXP")
        },
        { NULL }
    };

    /* Pretend that argv[0] is "gnucash" */
    if ((p = strstr(argv[0], "-bin"))) * p = '\0';

    context = g_option_context_new (" [datafile]");
    g_option_context_add_main_entries (context, options, GETTEXT_PACKAGE);
    g_option_context_add_group (context, gtk_get_option_group (FALSE));
    if (!g_option_context_parse (context, argc, &argv, &error))
    {
        g_warning("Error parsing command line arguments: [%s]; try `gnucash --help` for available options.", error->message);
        exit(1);
    }
    g_option_context_free (context);
    if (error)
        g_error_free(error);

    if (*argc > 0)
        file_to_load = argv[1];

    if (gnucash_show_version)
    {
        if (is_development_version)
        {
            /* Translators: %s is the version number */
            g_print(_("GnuCash %s development version"), VERSION);
        }
        else
        {
            /* Translators: %s is the version number */
            g_print(_("GnuCash %s"), VERSION);
        }
        g_print("\n");
        /* Translators: 1st %s is the build date; 2nd %s is the SVN
           revision number */
        g_print(_("Built %s from r%s"), GNUCASH_BUILD_DATE, GNUCASH_SVN_REV);
        g_print("\n");
        exit(0);
    }

    gnc_set_extra(extra);
    gnc_set_gconf_path(gconf_path);
    gnc_set_debugging(debugging);

    if (namespace_regexp)
        gnc_main_set_namespace_regexp(namespace_regexp);
}

static void
load_gnucash_modules()
{
    int i, len;
    struct
    {
        gchar * name;
        int version;
        gboolean optional;
    } modules[] =
    {
        { "gnucash/app-utils", 0, FALSE },
        { "gnucash/engine", 0, FALSE },
        { "gnucash/register/ledger-core", 0, FALSE },
        { "gnucash/register/register-core", 0, FALSE },
        { "gnucash/register/register-gnome", 0, FALSE },
        { "gnucash/import-export/qif-import", 0, FALSE },
        { "gnucash/import-export/ofx", 0, TRUE },
        { "gnucash/import-export/csv", 0, TRUE },
        { "gnucash/import-export/log-replay", 0, TRUE },
        { "gnucash/import-export/aqbanking", 0, TRUE },
        { "gnucash/import-export/hbci", 0, TRUE },
        { "gnucash/report/report-system", 0, FALSE },
        { "gnucash/report/stylesheets", 0, FALSE },
        { "gnucash/report/standard-reports", 0, FALSE },
        { "gnucash/report/utility-reports", 0, FALSE },
        { "gnucash/report/locale-specific/us", 0, FALSE },
        { "gnucash/report/report-gnome", 0, FALSE },
        { "gnucash/business-gnome", 0, TRUE }
    };

    /* module initializations go here */
    len = sizeof(modules) / sizeof(*modules);
    for (i = 0; i < len; i++)
    {
        gnc_update_splash_screen(modules[i].name, GNC_SPLASH_PERCENTAGE_UNKNOWN);
        if (modules[i].optional)
            gnc_module_load_optional(modules[i].name, modules[i].version);
        else
            gnc_module_load(modules[i].name, modules[i].version);
    }
    if (!gnc_engine_is_initialized())
    {
        /* On Windows this check used to fail anyway, see
         * https://lists.gnucash.org/pipermail/gnucash-devel/2006-September/018529.html
         * but more recently it seems to work as expected
         * again. 2006-12-20, cstim. */
        g_warning("GnuCash engine failed to initialize.  Exiting.\n");
        exit(1);
    }
}

static void
inner_main_add_price_quotes(void *closure, int argc, char **argv)
{
    SCM mod, add_quotes, scm_book, scm_result = SCM_BOOL_F;
    QofSession *session = NULL;

    scm_c_eval_string("(debug-set! stack 200000)");

    mod = scm_c_resolve_module("gnucash price-quotes");
    scm_set_current_module(mod);

    load_gnucash_modules();

    qof_event_suspend();
    scm_c_eval_string("(gnc:price-quotes-install-sources)");

    if (!gnc_quote_source_fq_installed())
    {
        g_print("%s", _("No quotes retrieved. Finance::Quote isn't "
                        "installed properly.\n"));
        goto fail;
    }

    add_quotes = scm_c_eval_string("gnc:book-add-quotes");
    session = gnc_get_current_session();
    if (!session) goto fail;

    qof_session_begin(session, add_quotes_file, FALSE, FALSE);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR) goto fail;

    qof_session_load(session, NULL);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR) goto fail;

    scm_book = gnc_book_to_scm(qof_session_get_book(session));
    scm_result = scm_call_2(add_quotes, SCM_BOOL_F, scm_book);

    qof_session_save(session, NULL);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR) goto fail;

    qof_session_destroy(session);
    if (!scm_is_true(scm_result))
    {
        g_warning("Failed to add quotes to %s.", add_quotes_file);
        goto fail;
    }

    qof_event_resume();
    gnc_shutdown(0);
    return;
fail:
    if (session && qof_session_get_error(session) != ERR_BACKEND_NO_ERR)
        g_warning("Session Error: %s", qof_session_get_error_message(session));
    qof_event_resume();
    gnc_shutdown(1);
}

static char *
get_file_to_load()
{
    if (file_to_load)
        return g_strdup(file_to_load);
    else
        return gnc_history_get_last();
}

static void
inner_main (void *closure, int argc, char **argv)
{
    SCM main_mod;
    char* fn;
    GError *error = NULL;

    scm_c_eval_string("(debug-set! stack 200000)");

    main_mod = scm_c_resolve_module("gnucash main");
    scm_set_current_module(main_mod);

    load_gnucash_modules();

    /* Load the config before starting up the gui. This insures that
     * custom reports have been read into memory before the Reports
     * menu is created. */
    load_system_config();
    load_user_config();

    /* Setting-up the report menu must come after the module
       loading but before the gui initialization. */
    scm_c_use_module("gnucash report report-gnome");
    scm_c_eval_string("(gnc:report-menu-setup)");

    /* TODO: After some more guile-extraction, this should happen even
       before booting guile.  */
    gnc_main_gui_init();

    gnc_hook_add_dangler(HOOK_UI_SHUTDOWN, (GFunc)gnc_file_quit, NULL);

    scm_c_eval_string("(gnc:main)");

    /* Install Price Quote Sources */
    gnc_update_splash_screen(_("Checking Finance::Quote..."), GNC_SPLASH_PERCENTAGE_UNKNOWN);
    scm_c_use_module("gnucash price-quotes");
    scm_c_eval_string("(gnc:price-quotes-install-sources)");

    gnc_hook_run(HOOK_STARTUP, NULL);

    if (!nofile && (fn = get_file_to_load()))
    {
        gnc_update_splash_screen(_("Loading data..."), GNC_SPLASH_PERCENTAGE_UNKNOWN);
        gnc_file_open_file(fn);
        g_free(fn);
    }
    else if (gnc_gconf_get_bool("dialogs/new_user", "first_startup", &error)
             && !error)
    {
        gnc_destroy_splash_screen();
        gnc_ui_new_user_dialog();
    }

    gnc_destroy_splash_screen();

    gnc_main_window_show_all_windows();

    gnc_hook_run(HOOK_UI_POST_STARTUP, NULL);
    gnc_ui_start_event_loop();
    gnc_hook_remove_dangler(HOOK_UI_SHUTDOWN, (GFunc)gnc_file_quit);

    gnc_shutdown(0);
    return;
}

static void
gnc_log_init()
{
    if (log_to_filename != NULL)
    {
        qof_log_init_filename_special(log_to_filename);
    }
    else
    {
        /* initialize logging to our file. */
        gchar *tracefilename;
        tracefilename = g_build_filename(g_get_tmp_dir(), "gnucash.trace",
                                         (gchar *)NULL);
        qof_log_init_filename(tracefilename);
        g_free(tracefilename);
    }

    // set a reasonable default.
    qof_log_set_default(QOF_LOG_WARNING);

    gnc_log_default();

    if (gnc_is_debugging())
    {
        qof_log_set_level("", QOF_LOG_INFO);
        qof_log_set_level("qof", QOF_LOG_INFO);
        qof_log_set_level("gnc", QOF_LOG_INFO);
    }

    {
        gchar *log_config_filename;
        log_config_filename = gnc_build_dotgnucash_path("log.conf");
        if (g_file_test(log_config_filename, G_FILE_TEST_EXISTS))
            qof_log_parse_log_config(log_config_filename);
        g_free(log_config_filename);
    }

    if (log_flags != NULL)
    {
        int i = 0;
        for (; log_flags[i] != NULL; i++)
        {
            QofLogLevel level;
            gchar **parts = NULL;

            gchar *log_opt = log_flags[i];
            parts = g_strsplit(log_opt, "=", 2);
            if (parts == NULL || parts[0] == NULL || parts[1] == NULL)
            {
                g_warning("string [%s] not parseable", log_opt);
                continue;
            }

            level = qof_log_level_from_string(parts[1]);
            qof_log_set_level(parts[0], level);
            g_strfreev(parts);
        }
    }
}

int
main(int argc, char ** argv)
{
#if !defined(G_THREADS_ENABLED) || defined(G_THREADS_IMPL_NONE)
#    error "No GLib thread implementation available!"
#endif
    g_thread_init(NULL);

#ifdef ENABLE_BINRELOC
    {
        GError *binreloc_error = NULL;
        if (!gnc_gbr_init(&binreloc_error))
        {
            g_print("main: Error on gnc_gbr_init: %s\n", binreloc_error->message);
            g_error_free(binreloc_error);
        }
    }
#else
    g_message("main: binreloc relocation support was disabled at configure time.\n");
#endif

    /* This should be called before gettext is initialized
     * The user may have configured a different language via
     * the environment file.
     */
    environment_override();

    #ifdef HAVE_GETTEXT
    {
        gchar *localedir = gnc_path_get_localedir();
        /* setlocale(LC_ALL, ""); is already called by gtk_set_locale()
           via gtk_init(). */
        bindtextdomain(GETTEXT_PACKAGE, localedir);
        textdomain(GETTEXT_PACKAGE);
        bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
        g_free(localedir);
    }
#endif

    qof_log_init();
    qof_log_set_default(QOF_LOG_INFO);

    gnucash_command_line(&argc, argv);
    gnc_print_unstable_message();
    gnc_log_init();

    gnc_module_system_init();

    if (add_quotes_file)
    {
        gchar *prefix = gnc_path_get_prefix ();
        gchar *pkgsysconfdir = gnc_path_get_pkgsysconfdir ();
        gchar *pkgdatadir = gnc_path_get_pkgdatadir ();
        gchar *pkglibdir = gnc_path_get_pkglibdir ();
        /* This option needs to run without a display, so we can't
           initialize any GUI libraries.  */
        gnome_program_init(
            "gnucash", VERSION, LIBGNOME_MODULE,
            argc, argv,
            GNOME_PARAM_APP_PREFIX, prefix,
            GNOME_PARAM_APP_SYSCONFDIR, pkgsysconfdir,
            GNOME_PARAM_APP_DATADIR, pkgdatadir,
            GNOME_PARAM_APP_LIBDIR, pkglibdir,
            GNOME_PARAM_NONE);
        g_free (prefix);
        g_free (pkgsysconfdir);
        g_free (pkgdatadir);
        g_free (pkglibdir);
        scm_boot_guile(argc, argv, inner_main_add_price_quotes, 0);
        exit(0);  /* never reached */
    }

    gnc_gnome_init (argc, argv, VERSION);
    gnc_gui_init();
    scm_boot_guile(argc, argv, inner_main, 0);
    exit(0); /* never reached */
}
