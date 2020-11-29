/*
 * gnucash-core-app.cpp -- Basic application object for gnucash binaries
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
#include <guile-mappings.h>
#ifdef __MINGW32__
extern "C" void set_win32_thread_locale(char**);
#include <Windows.h>
#include <fcntl.h>
#endif

#include "gnucash-core-app.hpp"

extern "C" {
#include <glib/gi18n.h>
#include <binreloc.h>
#include <gnc-engine.h>
#include <gfec.h>
#include <gnc-environment.h>
#include <gnc-filepath-utils.h>
#include <gnc-locale-utils.h>
#include <gnc-path.h>
#include <gnc-prefs.h>
#include <gnc-gsettings.h>
#include <gnc-report.h>
#include <gnc-splash.h>
#include <gnc-version.h>
}

#include <boost/algorithm/string.hpp>
#include <boost/locale.hpp>
#include <iostream>
#include <string>
#include <vector>

namespace bl = boost::locale;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#include <libintl.h>
#include <locale.h>
#include <gnc-locale-utils.hpp>

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
    while ((this_locale = [(NSString*)[locale_iter nextObject] stringByAppendingString: @".UTF-8"]))
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
        locale_str = @"en_US.UTF-8";
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
	locale_str = [[[lang_str stringByAppendingString: @"_"]
		      stringByAppendingString: country_str]
                      stringByAppendingString: @".UTF-8"];
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
	locale_str = @"en_US.UTF-8";

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
    if (g_getenv("LANGUAGE") == NULL)
    {
        if ([languages count] > 0)
            mac_set_languages(languages, lang_str);
        else
            g_setenv("LANGUAGE", [lang_str UTF8String], TRUE);
    }
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
    system_config = g_build_filename(system_config_dir, "config", nullptr);
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
gnc_log_init (const std::vector <std::string> log_flags,
              const boost::optional <std::string> &log_to_filename)
{
    if (log_to_filename && !log_to_filename->empty())
    {
#ifdef __MINGW64__
        auto *utf8_filename = g_utf16_to_utf8 (log_to_filename->c_str(), -1, NULL, NULL, NULL);
#else
        auto utf8_filename = log_to_filename->c_str();
#endif

        qof_log_init_filename_special (utf8_filename);

#ifdef __MINGW64__
        g_free (utf8_filename);
#endif
    }
    else
    {
        /* initialize logging to our file. */
        auto tracefilename = g_build_filename (g_get_tmp_dir(), "gnucash.trace",
                                               (gchar *)NULL);
        qof_log_init_filename (tracefilename);
        g_free (tracefilename);
    }

    // set a reasonable default.
    qof_log_set_default(QOF_LOG_WARNING);
    gnc_log_default();

    if (gnc_prefs_is_debugging_enabled())
    {
        qof_log_set_level ("", QOF_LOG_INFO);
        qof_log_set_level ("qof", QOF_LOG_INFO);
        qof_log_set_level ("gnc", QOF_LOG_INFO);
    }

    auto log_config_filename = g_build_filename (gnc_userconfig_dir (),
                                                 "log.conf", (char *)NULL);
    if (g_file_test (log_config_filename, G_FILE_TEST_EXISTS))
        qof_log_parse_log_config (log_config_filename);
    g_free (log_config_filename);

    for (auto log_flag : log_flags)
    {
        if (log_flag.empty () ||
            log_flag[0] == '=' ||
            log_flag[log_flag.length () - 1] == '=')
        {
            g_warning ("string [%s] not parseable", log_flag.c_str());
            continue;
        }

        std::vector<std::string> split_flag;
        boost::split (split_flag, log_flag, [](char c){return c == '=';});

        auto level = qof_log_level_from_string (split_flag[1].c_str());
        qof_log_set_level (split_flag[0].c_str(), level);
    }
}

Gnucash::CoreApp::CoreApp ()
{
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

    /* This should be called before gettext is initialized
     * The user may have configured a different language via
     * the environment file.
     */
    #ifdef MAC_INTEGRATION
    set_mac_locale();
    #elif defined __MINGW32__
    set_win32_thread_locale(&sys_locale);
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

    gnc_init_boost_locale (localedir);
    std::cerr.imbue (gnc_get_boost_locale());
    g_free(localedir);
}

Gnucash::CoreApp::CoreApp (const char* app_name)
{

    CoreApp();

    m_app_name = std::string(app_name);

    // Now that gettext is properly initialized, set our help tagline.
    m_tagline = bl::translate("- GnuCash, accounting for personal and small business finance").str(gnc_get_boost_locale());
    m_opt_desc_display = std::make_unique<bpo::options_description>
        ((bl::format (bl::gettext ("{1} [options] [datafile]")) % m_app_name).str() + std::string(" ") + m_tagline);
    add_common_program_options();
}


/* Parse command line options, using GOption interface.
 * We can't let gtk_init_with_args do it because it fails
 * before parsing any arguments if the GUI can't be initialized.
 */
void
Gnucash::CoreApp::parse_command_line (int argc, char **argv)
{
    try
    {
    bpo::store (bpo::command_line_parser (argc, argv).
        options (m_opt_desc_all).positional(m_pos_opt_desc).run(), m_opt_map);
    bpo::notify (m_opt_map);
    }
    catch (std::exception &e)
    {
        std::cerr << e.what() << "\n\n";
        std::cerr << *m_opt_desc_display.get() << "\n";

        exit(1);
    }

    if (m_show_version)
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

    if (m_show_help)
    {
        std::cout << *m_opt_desc_display.get() << "\n";
        exit(0);
    }

    gnc_prefs_set_debugging (m_debug);
    gnc_prefs_set_extra (m_extra);

    if (m_gsettings_prefix)
        gnc_gsettings_set_prefix (m_gsettings_prefix->c_str());
}

/* Define command line options common to all gnucash binaries. */
void
Gnucash::CoreApp::add_common_program_options (void)
{
    bpo::options_description common_options(_("Common Options"));
    common_options.add_options()
        ("help,h", bpo::bool_switch (&m_show_help),
         _("Show this help message"))
        ("version,v", bpo::bool_switch (&m_show_version),
         _("Show GnuCash version"))
        ("debug", bpo::bool_switch (&m_debug),
         _("Enable debugging mode: provide deep detail in the logs.\nThis is equivalent to: --log \"=info\" --log \"qof=info\" --log \"gnc=info\""))
        ("extra", bpo::bool_switch(&m_extra),
         _("Enable extra/development/debugging features."))
        ("log", bpo::value (&m_log_flags),
         _("Log level overrides, of the form \"modulename={debug,info,warn,crit,error}\"\nExamples: \"--log qof=debug\" or \"--log gnc.backend.file.sx=info\"\nThis can be invoked multiple times."))
        ("logto", bpo::value (&m_log_to_filename),
         _("File to log into; defaults to \"/tmp/gnucash.trace\"; can be \"stderr\" or \"stdout\"."))
        ("gsettings-prefix", bpo::value (&m_gsettings_prefix),
         _("Set the prefix for gsettings schemas for gsettings queries. This can be useful to have a different settings tree while debugging."));

    bpo::options_description hidden_options(_("Hidden Options"));
    hidden_options.add_options()
        ("input-file", bpo::value (&m_file_to_load),
         _("[datafile]"));

        m_pos_opt_desc.add("input-file", -1);

        m_opt_desc_all.add (common_options);
        m_opt_desc_all.add (hidden_options);

        m_opt_desc_display->add (common_options);
}

void
Gnucash::CoreApp::start (void)
{
    gnc_print_unstable_message();

    /* Make sure gnucash' user data directory is properly set up
     *       This must be done before any guile code is called as that would
     *       fail the migration message */
    userdata_migration_msg = gnc_filepath_init();
    if (userdata_migration_msg)
        g_print("\n\n%s\n", userdata_migration_msg);

    gnc_log_init (m_log_flags, m_log_to_filename);
    gnc_engine_init (0, NULL);

    /* Write some locale details to the log to simplify debugging */
    PINFO ("System locale returned %s", sys_locale ? sys_locale : "(null)");
    PINFO ("Effective locale set to %s.", setlocale (LC_ALL, NULL));
    g_free (sys_locale);
    sys_locale = NULL;
}
