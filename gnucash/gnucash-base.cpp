/*
 * gnucash-base.cpp -- Basic application object for gnucash binaries
 *
 * Copyright (C) 2020 Geert Janssens <geert@kobaltwit.be>
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
#include <config.h>

#include <libguile.h>
#include <glib/gi18n.h>
#include <binreloc.h>
#include <gnc-engine.h>
#include <guile-mappings.h>
#ifdef __MINGW32__
#include <Windows.h>
#include <fcntl.h>
#endif

#include "gnucash-base.hpp"

extern "C" {
#include <gfec.h>
#include <gnc-environment.h>
#include <gnc-filepath-utils.h>
#include <gnc-path.h>
#include <gnc-prefs.h>
#include <gnc-gsettings.h>
#include <gnc-report.h>
#include <gnc-splash.h>
#include <gnc-version.h>
}

#include <boost/locale.hpp>
#include <iostream>

namespace bl = boost::locale;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/* Change the following to have a console window attached to GnuCash
 * for displaying stdout and stderr on Windows.
 */
#define __MSWIN_CONSOLE__ 0

#include <libintl.h>
#include <locale.h>

#ifdef MAC_INTEGRATION
#  include <Foundation/Foundation.h>
#endif

/* GNC_VCS is defined whenever we're building from an svn/svk/git/bzr tree */
#ifdef GNC_VCS
static int is_development_version = TRUE;
#else
static int is_development_version = FALSE;
#define GNC_VCS ""
#endif



static gchar *userdata_migration_msg = NULL;

static void
gnc_print_unstable_message(void)
{
    if (!is_development_version) return;

    std::cerr << bl::translate ("This is a development version. It may or may not work.") << "\n"
              << bl::translate ("Report bugs and other problems to gnucash-devel@gnucash.org") << "\n"
              /* Translators: {1} will be replaced with a URL*/
              << bl::format (bl::translate ("You can also lookup and file bug reports at {1}")) % PACKAGE_BUGREPORT << "\n"
              /* Translators: {1} will be replaced with a URL*/
              << bl::format (bl::translate ("To find the last stable version, please refer to {1}")) % PACKAGE_URL << "\n";
}

#ifdef MAC_INTEGRATION
static void
mac_set_currency_locale(NSLocale *locale, NSString *locale_str)
{
    /* If the currency doesn't match the base locale, we need to find a locale that does match, because setlocale won't know what to do with just a currency identifier. */
    NSLocale *cur_locale = [ [NSLocale alloc] initWithLocaleIdentifier: locale_str];
    if (![ [locale objectForKey: NSLocaleCurrencyCode] isEqualToString:
	  [cur_locale objectForKey: NSLocaleCurrencyCode] ])
    {
        NSArray *all_locales = [NSLocale availableLocaleIdentifiers];
        NSEnumerator *locale_iter = [all_locales objectEnumerator];
        NSString *this_locale;
        NSString *currency = [locale objectForKey: NSLocaleCurrencyCode];
        NSString *money_locale = nil;
        while ((this_locale = (NSString*)[locale_iter nextObject]))
        {
            NSLocale *templocale = [ [NSLocale alloc]
                                    initWithLocaleIdentifier: this_locale];
            if ([ [templocale objectForKey: NSLocaleCurrencyCode]
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
    [cur_locale release];
}
/* The locale that we got from AppKit isn't a supported POSIX one, so we need to
 * find something close. First see if we can find another locale for the
 * country; failing that, try the language. Ultimately fall back on en_US.
 */
static NSString*
mac_find_close_country(NSString *locale_str, NSString *country_str,
                       NSString *lang_str)
{
    NSArray *all_locales = [NSLocale availableLocaleIdentifiers];
    NSEnumerator *locale_iter = [all_locales objectEnumerator];
    NSString *this_locale, *new_locale = nil;
    PWARN("Apple Locale is set to a value %s not supported"
          " by the C runtime", [locale_str UTF8String]);
    while ((this_locale = (NSString*)[locale_iter nextObject]))
        if ([ [ [NSLocale componentsFromLocaleIdentifier: this_locale]
              objectForKey: NSLocaleCountryCode]
             isEqualToString: country_str] &&
            setlocale (LC_ALL, [this_locale UTF8String]))
        {
            new_locale = this_locale;
            break;
        }
    if (!new_locale)
        while ((this_locale = (NSString*)[locale_iter nextObject]))
            if ([ [ [NSLocale componentsFromLocaleIdentifier: this_locale]
                  objectForKey: NSLocaleLanguageCode]
                 isEqualToString: lang_str] &&
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
    return locale_str;
}

/* Language subgroups (e.g., US English) are reported in the form "ll-SS"
 * (e.g. again, "en-US"), not what gettext wants. We convert those to
 * old-style locales, which is easy for most cases. There are two where it
 * isn't, though: Simplified Chinese (zh-Hans) and traditional Chinese
 * (zh-Hant), which are normally assigned the locales zh_CN and zh_TW,
 * respectively. Those are handled specially.
 */
static NSString*
mac_convert_complex_language(NSString* this_lang)
{
    NSArray *elements = [this_lang componentsSeparatedByString: @"-"];
    if ([elements count] == 1)
        return this_lang;
    if ([ [elements objectAtIndex: 0] isEqualToString: @"zh"]) {
        if ([ [elements objectAtIndex: 1] isEqualToString: @"Hans"])
            this_lang = @"zh_CN";
        else
            this_lang = @"zh_TW";
    }
    else
        this_lang = [elements componentsJoinedByString: @"_"];
    return this_lang;
}

static void
mac_set_languages(NSArray* languages, NSString *lang_str)
{
    /* Process the language list. */

    const gchar *langs = NULL;
    NSEnumerator *lang_iter = [languages objectEnumerator];
    NSArray *new_languages = [NSArray array];
    NSString *this_lang = NULL;
    NSRange not_found = {NSNotFound, 0};
    while ((this_lang = [lang_iter nextObject])) {
        this_lang = [this_lang stringByTrimmingCharactersInSet:
                     [NSCharacterSet characterSetWithCharactersInString: @"\""] ];
        this_lang = mac_convert_complex_language(this_lang);
        new_languages = [new_languages arrayByAddingObject: this_lang];
/* If it's an English language, add the "C" locale after it so that
 * any messages can default to it */
        if (!NSEqualRanges([this_lang rangeOfString: @"en"], not_found))
            new_languages = [new_languages arrayByAddingObject: @"C"];
        if (![new_languages containsObject: lang_str]) {
            NSArray *temp_array = [NSArray arrayWithObject: lang_str];
            new_languages = [temp_array arrayByAddingObjectsFromArray: new_languages];
        }
        langs = [ [new_languages componentsJoinedByString:@":"] UTF8String];
    }
    if (langs && strlen(langs) > 0)
    {
        PWARN("Language list: %s", langs);
        g_setenv("LANGUAGE", langs, TRUE);
    }
}

static void
set_mac_locale()
{
    NSAutoreleasePool *pool = [ [NSAutoreleasePool alloc] init];
    NSUserDefaults *defs = [NSUserDefaults standardUserDefaults];
    NSLocale *locale = [NSLocale currentLocale];
    NSString *lang_str, *country_str, *locale_str;
    NSArray *languages = [ [defs arrayForKey: @"AppleLanguages"] retain];
    @try
    {
        lang_str = [locale objectForKey: NSLocaleLanguageCode];
        country_str = [locale objectForKey: NSLocaleCountryCode];
	locale_str = [ [lang_str stringByAppendingString: @"_"]
		      stringByAppendingString: country_str];
    }
    @catch (NSException *err)
    {
	PWARN("Locale detection raised error %s: %s. "
	      "Check that your locale settings in "
	      "System Preferences>Languages & Text are set correctly.",
	      [ [err name] UTF8String], [ [err reason] UTF8String]);
	locale_str = @"_";
    }
/* If we didn't get a valid current locale, the string will be just "_" */
    if ([locale_str isEqualToString: @"_"])
	locale_str = @"en_US";

    lang_str = mac_convert_complex_language(lang_str);
    if (!setlocale(LC_ALL, [locale_str UTF8String]))
        locale_str =  mac_find_close_country(locale_str, country_str, lang_str);
    if (g_getenv("LANG") == NULL)
	g_setenv("LANG", [locale_str UTF8String], TRUE);
    mac_set_currency_locale(locale, locale_str);
/* Now call gnc_localeconv() to force creation of the app locale
 * before another call to setlocale messes it up. */
    gnc_localeconv ();
    /* Process the languages, including the one from the Apple locale. */
    if ([languages count] > 0)
        mac_set_languages(languages, lang_str);
    else
        g_setenv("LANGUAGE", [lang_str UTF8String], TRUE);
    [languages release];
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
        filename = gnc_build_userdata_path(fns[i]);
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
    /* Don't continue adding to this list. When 3.0 rolls around bump
       the 2.4 files off the list. */
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
    {
        gchar *config_filename;
        config_filename = g_build_filename (gnc_userconfig_dir (),
                                                "config-user.scm", (char *)NULL);
        gfec_try_load(config_filename);
        g_free(config_filename);
    }

    update_message("loading saved reports");
    try_load_config_array(saved_report_files);
    update_message("loading stylesheets");
    try_load_config_array(stylesheet_files);
}


static void
gnc_log_init (gchar **log_flags, gchar *log_to_filename)
{
    if (log_to_filename != NULL)
        qof_log_init_filename_special(log_to_filename);
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
        log_config_filename = g_build_filename (gnc_userconfig_dir (),
                                                "log.conf", (char *)NULL);
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

#ifdef __MINGW32__
/* If one of the Unix locale variables LC_ALL, LC_MESSAGES, or LANG is
 * set in the environment check to see if it's a valid locale and if
 * it is set both the Windows and POSIX locales to that. If not
 * retrieve the Windows locale and set POSIX to match.
 */
static void
set_win32_thread_locale()
{
    WCHAR lpLocaleName[LOCALE_NAME_MAX_LENGTH];
    char *locale = NULL;

    if (((locale = getenv ("LC_ALL")) != NULL && locale[0] != '\0') ||
      ((locale = getenv ("LC_MESSAGES")) != NULL && locale[0] != '\0') ||
      ((locale = getenv ("LANG")) != NULL && locale[0] != '\0'))
    {
	gunichar2* wlocale = NULL;
	int len = 0;
	len = strchr(locale, '.') - locale;
	locale[2] = '-';
	wlocale = g_utf8_to_utf16 (locale, len, NULL, NULL, NULL);
	if (IsValidLocaleName(wlocale))
	{
	    LCID lcid = LocaleNameToLCID(wlocale, LOCALE_ALLOW_NEUTRAL_NAMES);
	    SetThreadLocale(lcid);
	    locale[2] = '_';
	    setlocale (LC_ALL, locale);
	    sys_locale = locale;
	    g_free(wlocale);
	    return;
	}
	g_free(locale);
	g_free(wlocale);
    }
    if (GetUserDefaultLocaleName(lpLocaleName, LOCALE_NAME_MAX_LENGTH))
    {
	sys_locale = g_utf16_to_utf8((gunichar2*)lpLocaleName,
				     LOCALE_NAME_MAX_LENGTH,
				     NULL, NULL, NULL);
	sys_locale[2] = '_';
	setlocale (LC_ALL, sys_locale);
	return;
    }
}
#endif

/* Creates a console window on MSWindows to display stdout and stderr
 * when __MSWIN_CONSOLE__ is defined at the top of the file.
 *
 * Useful for displaying the diagnostics printed before logging is
 * started and if logging is redirected with --logto=stderr.
 */
static void
redirect_stdout (void)
{
#if defined __MINGW32__ && __MSWIN_CONSOLE__
    static const WORD MAX_CONSOLE_LINES = 500;
   int hConHandle;
    long lStdHandle;
    CONSOLE_SCREEN_BUFFER_INFO coninfo;
    FILE *fp;

    // allocate a console for this app
    AllocConsole();

    // set the screen buffer to be big enough to let us scroll text
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &coninfo);
    coninfo.dwSize.Y = MAX_CONSOLE_LINES;
    SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), coninfo.dwSize);

    // redirect unbuffered STDOUT to the console
    lStdHandle = (long)GetStdHandle(STD_OUTPUT_HANDLE);
    hConHandle = _open_osfhandle(lStdHandle, _O_TEXT);
    fp = _fdopen( hConHandle, "w" );
    *stdout = *fp;
    setvbuf( stdout, NULL, _IONBF, 0 );

    // redirect unbuffered STDIN to the console
    lStdHandle = (long)GetStdHandle(STD_INPUT_HANDLE);
    hConHandle = _open_osfhandle(lStdHandle, _O_TEXT);
    fp = _fdopen( hConHandle, "r" );
    *stdin = *fp;
    setvbuf( stdin, NULL, _IONBF, 0 );

    // redirect unbuffered STDERR to the console
    lStdHandle = (long)GetStdHandle(STD_ERROR_HANDLE);
    hConHandle = _open_osfhandle(lStdHandle, _O_TEXT);
    fp = _fdopen( hConHandle, "w" );
    *stderr = *fp;
    setvbuf( stderr, NULL, _IONBF, 0 );
#endif
}

Gnucash::Base::Base ()
{
    #if !defined(G_THREADS_ENABLED) || defined(G_THREADS_IMPL_NONE)
    #    error "No GLib thread implementation available!"
    #endif
    #ifdef ENABLE_BINRELOC
    {
        GError *binreloc_error = NULL;
        if (!gnc_gbr_init(&binreloc_error))
        {
            std::cerr << "main: Error on gnc_gbr_init: " << binreloc_error->message << "\n";
            g_error_free(binreloc_error);
        }
    }
    #endif
    redirect_stdout ();

    /* This should be called before gettext is initialized
     * The user may have configured a different language via
     * the environment file.
     */
    #ifdef MAC_INTEGRATION
    set_mac_locale();
    #elif defined __MINGW32__
    set_win32_thread_locale();
    #endif
    gnc_environment_setup();
    #if ! defined MAC_INTEGRATION && ! defined __MINGW32__/* setlocale already done */
    sys_locale = g_strdup (setlocale (LC_ALL, ""));
    if (!sys_locale)
    {
        std::cerr << "The locale defined in the environment isn't supported. "
                  << "Falling back to the 'C' (US English) locale\n";
        g_setenv ("LC_ALL", "C", TRUE);
        setlocale (LC_ALL, "C");
    }
    #endif

    auto localedir = gnc_path_get_localedir ();
    bindtextdomain(PROJECT_NAME, localedir);
    bindtextdomain("iso_4217", localedir); // For win32 to find currency name translations
    bind_textdomain_codeset("iso_4217", "UTF-8");
    textdomain(PROJECT_NAME);
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8");
    g_free(localedir);
}

/* Parse command line options, using GOption interface.
 * We can't let gtk_init_with_args do it because it fails
 * before parsing any arguments if the GUI can't be initialized.
 */
void
Gnucash::Base::parse_command_line (int *argc, char ***argv)
{
#ifdef __MINGW64__
    wchar_t *tmp_log_to_filename = NULL;
#else
    char *tmp_log_to_filename = NULL;
#endif

    GOptionEntry options[] =
    {
        {
            "version", 'v', 0, G_OPTION_ARG_NONE, &gnucash_show_version,
            N_("Show GnuCash version"), NULL
        },

        {
            "debug", '\0', 0, G_OPTION_ARG_NONE, &debugging,
            N_("Enable debugging mode: provide deep detail in the logs.\nThis is equivalent to: --log \"=info\" --log \"qof=info\" --log \"gnc=info\""), NULL
        },

        {
            "extra", '\0', 0, G_OPTION_ARG_NONE, &extra,
            N_("Enable extra/development/debugging features."), NULL
        },

        {
            "log", '\0', 0, G_OPTION_ARG_STRING_ARRAY, &log_flags,
            N_("Log level overrides, of the form \"modulename={debug,info,warn,crit,error}\"\nExamples: \"--log qof=debug\" or \"--log gnc.backend.file.sx=info\"\nThis can be invoked multiple times."),
            NULL
        },

        {
            "logto", '\0', 0, G_OPTION_ARG_STRING, &tmp_log_to_filename,
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
             *         http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            N_("GSETTINGSPREFIX")
        },
        {
            "add-price-quotes", '\0', 0, G_OPTION_ARG_STRING, &add_quotes_file,
            N_("Add price quotes to given GnuCash datafile"),
            /* Translators: Argument description for autohelp; see
             *         http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            N_("FILE")
        },
        {
            "namespace", '\0', 0, G_OPTION_ARG_STRING, &namespace_regexp,
            N_("Regular expression determining which namespace commodities will be retrieved"),
            /* Translators: Argument description for autohelp; see
             *         http://developer.gnome.org/doc/API/2.0/glib/glib-Commandline-option-parser.html */
            N_("REGEXP")
        },
        {
            G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY, &args_remaining, NULL, N_("[datafile]") },
            { NULL }
    };

    GError *error = NULL;
    GOptionContext *context = g_option_context_new (_("- GnuCash, accounting for personal and small business finance"));

    g_option_context_add_main_entries (context, options, PROJECT_NAME);
    g_option_context_add_group (context, gtk_get_option_group(FALSE));
    if (!g_option_context_parse (context, argc, argv, &error))
    {
        std::cerr << error->message << "\n"
                  << bl::format (bl::translate ("Run '{1} --help' to see a full list of available command line options.")) % *argv[0]
                  << "\n";
        g_error_free (error);
        exit (1);
    }
    g_option_context_free (context);

    if (tmp_log_to_filename != NULL)
    {
#ifdef __MINGW64__
        log_to_filename = g_utf16_to_utf8(tmp_log_to_filename, -1, NULL, NULL, NULL);
        g_free (tmp_log_to_filename);
#else
        log_to_filename = tmp_log_to_filename;
#endif
    }

    if (gnucash_show_version)
    {
        bl::format rel_fmt (bl::translate ("GnuCash {1}"));
        bl::format dev_fmt (bl::translate ("GnuCash {1} development version"));

        if (is_development_version)
            std::cout << dev_fmt % gnc_version () << "\n";
        else
            std::cout << rel_fmt % gnc_version () << "\n";

        std::cout << bl::translate ("Build ID") << ": " << gnc_build_id () << "\n";
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

const char*
Gnucash::Base::get_file_to_load (void)
{
    return file_to_load;
}

int
Gnucash::Base::get_no_file (void)
{
    return nofile;
}

const char*
Gnucash::Base::get_quotes_file (void)
{
    return add_quotes_file;
}

void
Gnucash::Base::start (void)
{
    gnc_print_unstable_message();

    /* Make sure gnucash' user data directory is properly set up
     *       This must be done before any guile code is called as that would
     *       fail the migration message */
    userdata_migration_msg = gnc_filepath_init();
    if (userdata_migration_msg)
        g_print("\n\n%s\n", userdata_migration_msg);

    gnc_log_init (log_flags, log_to_filename);
    gnc_engine_init (0, NULL);

    /* Write some locale details to the log to simplify debugging */
    PINFO ("System locale returned %s", sys_locale ? sys_locale : "(null)");
    PINFO ("Effective locale set to %s.", setlocale (LC_ALL, NULL));
    g_free (sys_locale);
    sys_locale = NULL;
}
