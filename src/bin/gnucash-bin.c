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
#include "glib.h"
#include "gnc-module.h"
#include "gnc-path.h"
#include "binreloc.h"
#include "gnc-locale-utils.h"
#include "core-utils/gnc-version.h"
#include "gnc-engine.h"
#include "gnc-environment.h"
#include "gnc-filepath-utils.h"
#include "gnc-ui-util.h"
#include "gnc-file.h"
#include "gnc-hooks.h"
#include "top-level.h"
#include "gfec.h"
#include "gnc-commodity.h"
#include "gnc-prefs.h"
#include "gnc-prefs-utils.h"
#include "gnc-gsettings.h"
#include "gnc-report.h"
#include "gnc-main-window.h"
#include "gnc-splash.h"
#include "gnc-gnome-utils.h"
#include "gnc-plugin-file-history.h"
#include "dialog-new-user.h"
#include "gnc-session.h"
#include "engine-helpers-guile.h"
#include "swig-runtime.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#ifdef HAVE_GETTEXT
#  include <libintl.h>
#  include <locale.h>
#endif

#ifdef MAC_INTEGRATION
#  include <Foundation/Foundation.h>
#endif

/* GNUCASH_SCM is defined whenever we're building from an svn/svk/git/bzr tree */
#ifdef GNUCASH_SCM
static int is_development_version = TRUE;
#else
static int is_development_version = FALSE;
#define GNUCASH_SCM ""
#endif

/* Command-line option variables */
static int          gnucash_show_version = 0;
static int          debugging        = 0;
static int          extra            = 0;
static gchar      **log_flags        = NULL;
static gchar       *log_to_filename  = NULL;
static int          nofile           = 0;
static const gchar *gsettings_prefix = NULL;
static const char  *add_quotes_file  = NULL;
static char        *namespace_regexp = NULL;
static const char  *file_to_load     = NULL;
static gchar      **args_remaining   = NULL;

static GOptionEntry options[] =
{
    {
        "version", 'v', 0, G_OPTION_ARG_NONE, &gnucash_show_version,
        N_("Show GnuCash version"), NULL
    },

    {
        "debug", '\0', 0, G_OPTION_ARG_NONE, &debugging,
        N_("Enable debugging mode: increasing logging to provide deep detail."), NULL
    },

    {
        "extra", '\0', 0, G_OPTION_ARG_NONE, &extra,
        N_("Enable extra/development/debugging features."), NULL
    },

    {
        "log", '\0', 0, G_OPTION_ARG_STRING_ARRAY, &log_flags,
        N_("Log level overrides, of the form \"log.ger.path={debug,info,warn,crit,error}\""),
        NULL
    },

    {
        "logto", '\0', 0, G_OPTION_ARG_STRING, &log_to_filename,
        N_("File to log into; defaults to \"/tmp/gnucash.trace\"; can be \"stderr\" or \"stdout\"."),
        NULL
    },

    {
        "nofile", '\0', 0, G_OPTION_ARG_NONE, &nofile,
        N_("Do not load the last file opened"), NULL
    },
    {
        "gsettings-prefix", '\0', 0, G_OPTION_ARG_STRING, &gsettings_prefix,
        N_("Set the prefix for gsettings schemas for gsettings queries. This can be useful to have a different settings tree while debugging."),
        /* Translators: Argument description for autohelp; see
           http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
        N_("GSETTINGSPREFIX")
    },
    {
        "add-price-quotes", '\0', 0, G_OPTION_ARG_STRING, &add_quotes_file,
        N_("Add price quotes to given GnuCash datafile"),
        /* Translators: Argument description for autohelp; see
           http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
        N_("FILE")
    },
    {
        "namespace", '\0', 0, G_OPTION_ARG_STRING, &namespace_regexp,
        N_("Regular expression determining which namespace commodities will be retrieved"),
        /* Translators: Argument description for autohelp; see
           http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
        N_("REGEXP")
    },
    {
        G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY, &args_remaining, NULL, N_("[datafile]") },
    { NULL }
};

static void
gnc_print_unstable_message(void)
{
    if (!is_development_version) return;

    g_print("\n\n%s\n%s\n%s\n%s\n",
            _("This is a development version. It may or may not work."),
            _("Report bugs and other problems to gnucash-devel@gnucash.org"),
            _("You can also lookup and file bug reports at http://bugzilla.gnome.org"),
            _("To find the last stable version, please refer to http://www.gnucash.org"));
}

#ifdef MAC_INTEGRATION
static void
set_mac_locale()
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSUserDefaults *defs = [NSUserDefaults standardUserDefaults];
    NSArray *languages = [defs objectForKey: @"AppleLanguages"];
    const gchar *langs = NULL;
    NSLocale *locale = [NSLocale currentLocale];
    NSString *locale_str;
    @try
    {
	locale_str = [[[locale objectForKey: NSLocaleLanguageCode]
		       stringByAppendingString: @"_"]
		      stringByAppendingString:
		      [locale objectForKey: NSLocaleCountryCode]];
    }
    @catch (NSException *err)
    {
	PWARN("Locale detection raised error %s: %s. "
	      "Check that your locale settings in "
	      "System Preferences>Languages & Text are set correctly.",
	      [[err name] UTF8String], [[err reason] UTF8String]);
	locale_str = @"_";
    }
/* If we didn't get a valid current locale, the string will be just "_" */
    if ([locale_str isEqualToString: @"_"])
	locale_str = @"en_US";

    if (!setlocale(LC_ALL, [locale_str UTF8String]))
    {
	NSArray *all_locales = [NSLocale availableLocaleIdentifiers];
	NSEnumerator *locale_iter = [all_locales objectEnumerator];
	NSString *this_locale, *new_locale = nil;
	NSString *lang = [locale objectForKey: NSLocaleLanguageCode];
	PWARN("Apple Locale is set to a value %s not supported"
	      " by the C runtime", [locale_str UTF8String]);
	while ((this_locale = (NSString*)[locale_iter nextObject]))
	    if ([[[NSLocale componentsFromLocaleIdentifier: this_locale]
		  objectForKey: NSLocaleLanguageCode]
		 isEqualToString: lang] &&
		setlocale (LC_ALL, [this_locale UTF8String]))
	    {
		new_locale = this_locale;
		break;
	    }
	if (new_locale)
	    locale_str = new_locale;
	else
	{
	    locale_str = @"en_US";
	    setlocale(LC_ALL, [locale_str UTF8String]);
	}
	PWARN("Using %s instead.", [locale_str UTF8String]);
    }
    if (g_getenv("LANG") == NULL)
	g_setenv("LANG", [locale_str UTF8String], TRUE);
/* If the currency doesn't match the base locale, we need to find a locale that does match, because setlocale won't know what to do with just a currency identifier. */
    if (![[locale objectForKey: NSLocaleCurrencyCode] isEqualToString:
	  [[[NSLocale alloc] initWithLocaleIdentifier: locale_str] objectForKey: NSLocaleCurrencyCode]]) {
	NSArray *all_locales = [NSLocale availableLocaleIdentifiers];
	NSEnumerator *locale_iter = [all_locales objectEnumerator];
	NSString *this_locale;
	NSString *currency = [locale objectForKey: NSLocaleCurrencyCode];
	NSString *money_locale = nil;
	while ((this_locale = (NSString*)[locale_iter nextObject]))
	{
	    NSLocale *templocale = [[NSLocale alloc]
				    initWithLocaleIdentifier: this_locale];
	    if ([[templocale objectForKey: NSLocaleCurrencyCode]
		 isEqualToString: currency])
	    {
		money_locale = this_locale;
		[templocale release];
		break;
	    }
	    [templocale release];
	}
	if (money_locale)
	    setlocale(LC_MONETARY, [money_locale UTF8String]);
    }
/* Now call gnc_localeconv() to force creation of the app locale
 * before another call to setlocale messes it up. */
    gnc_localeconv ();
/* Process the language list.
 *
 * Language subgroups (e.g., US English) are reported in the form
 * "ll-SS" (e.g. again, "en-US"), not what gettext wants. We convert
 * those to old-style locales, which is easy for most cases. There are
 * two where it isn't, though: Simplified Chinese (zh-Hans) and
 * traditional Chinese (zh-Hant), which are normally assigned the
 * locales zh_CN and zh_TW, respectively. Those are handled
 * specially.*/
    if ([languages count] > 0) {
	NSEnumerator *lang_iter = [languages objectEnumerator];
	NSString *this_lang;
	NSArray *elements;
	NSArray *new_languages = [NSArray array];
	while ((this_lang = [lang_iter nextObject])) {
	    this_lang = [this_lang stringByTrimmingCharactersInSet:
			 [NSCharacterSet characterSetWithCharactersInString:
			  @"\""]];
	    elements = [this_lang componentsSeparatedByString: @"-"];
	    if ([elements count] > 1) {
		if ([[elements objectAtIndex: 0] isEqualToString: @"zh"]) {
		    if ([[elements objectAtIndex: 1] isEqualToString: @"Hans"])
			this_lang = @"zh_CN";
		    else
			this_lang = @"zh_TW";
		}
		else
		  this_lang = [elements componentsJoinedByString: @"_"];
	    }
	    new_languages = [new_languages arrayByAddingObject: this_lang];
/* If it's an English language, add the "C" locale after it so that
 * any messages can default to it */
	    if ( [[elements objectAtIndex: 0] isEqualToString: @"en"])
		new_languages = [new_languages arrayByAddingObject: @"C"];

	}
	langs = [[new_languages componentsJoinedByString:@":"] UTF8String];
    }
    if (langs && strlen(langs) > 0)
	g_setenv("LANGUAGE", langs, TRUE);
    [pool drain];
}
#endif /* MAC_INTEGRATION */

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
    gchar *system_config_dir;
    gchar *system_config;

    if (is_system_config_loaded) return;

    update_message("loading system configuration");
    system_config_dir = gnc_path_get_pkgsysconfdir();
    system_config = g_build_filename(system_config_dir, "config", NULL);
    is_system_config_loaded = gfec_try_load(system_config);
    g_free(system_config_dir);
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
        SAVED_REPORTS_FILE, SAVED_REPORTS_FILE_OLD_REV, NULL
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

/* Parse command line options, using GOption interface.
 * We can't let gtk_init_with_args do it because it fails
 * before parsing any arguments if the GUI can't be initialized.
 */
static void
gnc_parse_command_line(int *argc, char ***argv)
{

    GError *error = NULL;
    GOptionContext *context = g_option_context_new (_("- GnuCash personal and small business finance management"));

    g_option_context_add_main_entries (context, options, GETTEXT_PACKAGE);
    g_option_context_add_group (context, gtk_get_option_group(FALSE));
    if (!g_option_context_parse (context, argc, argv, &error))
    {
        g_printerr (_("%s\nRun '%s --help' to see a full list of available command line options.\n"),
                    error->message, *argv[0]);
        g_error_free (error);
        exit (1);
    }
    g_option_context_free (context);

    if (gnucash_show_version)
    {
        gchar *fixed_message;

        if (is_development_version)
        {
            fixed_message = g_strdup_printf(_("GnuCash %s development version"), VERSION);

            /* Translators: 1st %s is a fixed message, which is translated independently;
                            2nd %s is the scm type (svn/svk/git/bzr);
                            3rd %s is the scm revision number;
                            4th %s is the build date */
            g_print ( _("%s\nThis copy was built from %s rev %s on %s."),
                      fixed_message, GNUCASH_SCM, GNUCASH_SCM_REV,
                      GNUCASH_BUILD_DATE );
        }
        else
        {
            fixed_message = g_strdup_printf(_("GnuCash %s"), VERSION);

            /* Translators: 1st %s is a fixed message, which is translated independently;
                            2nd %s is the scm (svn/svk/git/bzr) revision number;
                            3rd %s is the build date */
            g_print ( _("%s\nThis copy was built from rev %s on %s."),
                      fixed_message, GNUCASH_SCM_REV, GNUCASH_BUILD_DATE );
        }
        g_print("\n");
        g_free (fixed_message);
        exit(0);
    }

    gnc_prefs_set_debugging(debugging);
    gnc_prefs_set_extra(extra);

    if (gsettings_prefix)
        gnc_gsettings_set_prefix(g_strdup(gsettings_prefix));

    if (namespace_regexp)
        gnc_prefs_set_namespace_regexp(namespace_regexp);

    if (args_remaining)
        file_to_load = args_remaining[0];
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
        { "gnucash/import-export/csv-import", 0, TRUE },
        { "gnucash/import-export/csv-export", 0, TRUE },
        { "gnucash/import-export/log-replay", 0, TRUE },
        { "gnucash/import-export/aqbanking", 0, TRUE },
        { "gnucash/report/report-system", 0, FALSE },
        { "gnucash/report/stylesheets", 0, FALSE },
        { "gnucash/report/standard-reports", 0, FALSE },
        { "gnucash/report/utility-reports", 0, FALSE },
        { "gnucash/report/locale-specific/us", 0, FALSE },
        { "gnucash/report/report-gnome", 0, FALSE },
        { "gnucash/business-gnome", 0, TRUE },
        { "gnucash/gtkmm", 0, TRUE },
        { "gnucash/python", 0, TRUE },
        { "gnucash/plugins/bi_import", 0, TRUE},
        { "gnucash/plugins/customer_import", 0, TRUE},
    };

    /* module initializations go here */
    len = sizeof(modules) / sizeof(*modules);
    for (i = 0; i < len; i++)
    {
        DEBUG("Loading module %s started", modules[i].name);
        gnc_update_splash_screen(modules[i].name, GNC_SPLASH_PERCENTAGE_UNKNOWN);
        if (modules[i].optional)
            gnc_module_load_optional(modules[i].name, modules[i].version);
        else
            gnc_module_load(modules[i].name, modules[i].version);
        DEBUG("Loading module %s finished", modules[i].name);
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

    /* Don't load the modules since the stylesheet module crashes if the
       GUI is not initialized */
#ifdef PRICE_QUOTES_NEED_MODULES
    load_gnucash_modules();
#endif
    gnc_prefs_init ();
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

    qof_session_begin(session, add_quotes_file, FALSE, FALSE, FALSE);
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

    /* GnuCash switched to gsettings to store its preferences in version 2.5.6
     * Migrate the user's preferences from gconf if needed */
    gnc_gsettings_migrate_from_gconf();

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

    /* Install Price Quote Sources */
    gnc_update_splash_screen(_("Checking Finance::Quote..."), GNC_SPLASH_PERCENTAGE_UNKNOWN);
    scm_c_use_module("gnucash price-quotes");
    scm_c_eval_string("(gnc:price-quotes-install-sources)");

    gnc_hook_run(HOOK_STARTUP, NULL);

    if (!nofile && (fn = get_file_to_load()))
    {
        gnc_update_splash_screen(_("Loading data..."), GNC_SPLASH_PERCENTAGE_UNKNOWN);
        gnc_file_open_file(fn, /*open_readonly*/ FALSE);
        g_free(fn);
    }
    else if (gnc_prefs_get_bool(GNC_PREFS_GROUP_NEW_USER, GNC_PREF_FIRST_STARTUP))
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

    if (gnc_prefs_is_debugging_enabled())
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
    gchar *sys_locale = NULL;
#if !defined(G_THREADS_ENABLED) || defined(G_THREADS_IMPL_NONE)
#    error "No GLib thread implementation available!"
#endif
#ifndef HAVE_GLIB_2_32 /* Automatic after GLib 2-32 */
    g_thread_init(NULL);
#endif
#ifdef ENABLE_BINRELOC
    {
        GError *binreloc_error = NULL;
        if (!gnc_gbr_init(&binreloc_error))
        {
            g_print("main: Error on gnc_gbr_init: %s\n", binreloc_error->message);
            g_error_free(binreloc_error);
        }
    }
#endif

    /* This should be called before gettext is initialized
     * The user may have configured a different language via
     * the environment file.
     */
#ifdef MAC_INTEGRATION
    set_mac_locale();
#endif
    gnc_environment_setup();
#ifndef MAC_INTEGRATION /* setlocale already done */
    sys_locale = g_strdup (setlocale (LC_ALL, ""));
    if (!sys_locale)
      {
        g_print ("The locale defined in the environment isn't supported. "
                 "Falling back to the 'C' (US English) locale\n");
        g_setenv ("LC_ALL", "C", TRUE);
        setlocale (LC_ALL, "C");
      }
#endif
#ifdef HAVE_GETTEXT
    {
        gchar *localedir = gnc_path_get_localedir();
        bindtextdomain(GETTEXT_PACKAGE, localedir);
        textdomain(GETTEXT_PACKAGE);
        bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
        g_free(localedir);
    }
#endif
    
    gnc_parse_command_line(&argc, &argv);
    gnc_print_unstable_message();

    gnc_log_init();

#ifndef MAC_INTEGRATION
    /* Write some locale details to the log to simplify debugging
     * To be on the safe side, only do this if not on OS X,
     * to avoid unintentionally messing up the locale settings */
    PINFO ("System locale returned %s", sys_locale ? sys_locale : "(null)");
    PINFO ("Effective locale set to %s.", setlocale (LC_ALL, ""));
    g_free (sys_locale);
#endif

    /* If asked via a command line parameter, fetch quotes only */
    if (add_quotes_file)
    {
        /* First initialize the module system, even though gtk hasn't been initialized. */
        gnc_module_system_init();
        scm_boot_guile(argc, argv, inner_main_add_price_quotes, 0);
        exit(0);  /* never reached */
    }

    /* We need to initialize gtk before looking up all modules */
    gnc_gtk_add_rc_file ();
    if(!gtk_init_check (&argc, &argv))
    {
        g_printerr(_("%s\nRun '%s --help' to see a full list of available command line options.\n"),
                   _("Error: could not initialize graphical user interface and option add-price-quotes was not set.\n"
                     "       Perhaps you need to set the $DISPLAY environment variable ?"),
                   argv[0]);
        return 1;
    }

    /* Now the module files are looked up, which might cause some library
    initialization to be run, hence gtk must be initialized beforehand. */
    gnc_module_system_init();

    gnc_gui_init();
    scm_boot_guile(argc, argv, inner_main, 0);
    exit(0); /* never reached */
}
