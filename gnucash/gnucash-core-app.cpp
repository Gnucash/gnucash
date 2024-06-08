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
#include <Windows.h>
#include <fcntl.h>
#endif

#include "gnucash-core-app.hpp"

#include <glib/gi18n.h>
#include <binreloc.h>
#include <gnc-engine.h>
#include <gfec.h>
#include <gnc-environment.h>
#include <gnc-filepath-utils.h>
#include <gnc-locale-utils.h>
#include <gnc-path.h>
#include <gnc-prefs.h>
#include <gnc-version.h>
#include "gnucash-locale-platform.h"

#include <boost/algorithm/string.hpp>
#include <boost/locale.hpp>
#include <iostream>
#include <string>
#include <vector>
#include <gnc-report.h>

namespace bl = boost::locale;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#include <libintl.h>
#include <locale.h>
#include <gnc-locale-utils.hpp>

/* GNC_VCS is defined whenever we're building from a git work tree */
#ifdef GNC_VCS
constexpr int is_development_version = TRUE;
#else
constexpr int is_development_version = FALSE;
#define GNC_VCS ""
#endif

static gchar *userdata_migration_msg = NULL;

static void
gnc_print_unstable_message(void)
{
    if (!is_development_version) return;

    std::cerr << _("This is a development version. It may or may not work.") << "\n"
              << _("Report bugs and other problems to gnucash-devel@gnucash.org") << "\n"
              /* Translators: {1} will be replaced with an URL*/
              << bl::format (std::string{_("You can also lookup and file bug reports at {1}")}) % PACKAGE_BUGREPORT << "\n"
              /* Translators: {1} will be replaced with an URL*/
              << bl::format (std::string{_("To find the last stable version, please refer to {1}")}) % PACKAGE_URL << "\n";
}

void
Gnucash::gnc_load_scm_config (MessageCb update_message_cb)
{
    static auto is_system_config_loaded = false;
    if (!is_system_config_loaded)
    {
        /* Translators: Guile is the programming language of the reports */
        auto msg = _("Loading system wide Guile extensions…");
        update_message_cb (msg);
        auto system_config_dir = gnc_path_get_pkgsysconfdir ();
        auto system_config = g_build_filename (system_config_dir, "config", nullptr);
        is_system_config_loaded = gfec_try_load (system_config);
        g_free (system_config_dir);
        g_free (system_config);
    }

    static auto is_user_config_loaded = false;
    if (!is_user_config_loaded)
    {
        auto msg = _("Loading user specific Guile extensions…");
        update_message_cb (msg);
        auto config_filename = g_build_filename (gnc_userconfig_dir (), "config-user.scm", nullptr);
        is_user_config_loaded = gfec_try_load (config_filename);
        g_free (config_filename);
    }
}

static void
gnc_log_init (const std::vector <std::string> log_flags,
              const boost::optional <std::string> &log_to_filename)
{
    if (log_to_filename && !log_to_filename->empty())
    {
        auto utf8_filename = log_to_filename->c_str();
        qof_log_init_filename_special (utf8_filename);
    }
    else
    {
        /* initialize logging to our file. */
        auto tracefilename = g_build_filename (g_get_tmp_dir(), "gnucash.trace",
                                               (gchar *)NULL);
        qof_log_init_filename (tracefilename);
        g_free (tracefilename);
    }

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

Gnucash::CoreApp::CoreApp (const char* app_name) : m_app_name {app_name}
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
    gnc_environment_setup();
    #if defined MAC_INTEGRATION || defined __MINGW32__
    sys_locale = set_platform_locale();
    #endif
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
    std::cout.imbue (gnc_get_boost_locale());
    g_free(localedir);

    // Now that gettext is properly initialized, set our help tagline.
    m_tagline = _("- GnuCash, accounting for personal and small business finance");
    m_opt_desc_display = std::make_unique<bpo::options_description>
        ((bl::format (std::string{_("{1} [options] [datafile]")}) % m_app_name).str() + std::string(" ") + m_tagline);
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
        std::cerr << *m_opt_desc_display.get() << std::endl;

        exit(1);
    }

    if (m_show_paths)
    {
        std::cout << _("GnuCash Paths") << '\n';
        for (const auto& ep : gnc_list_all_paths ())
        {
            std::cout << ep.env_name << ": " << ep.env_path;
            if (ep.modifiable)
                std::cout << ' ' << _("(user modifiable)");
            std::cout << '\n';
        }
        exit (0);
    }

    if (m_show_version)
    {
        bl::format rel_fmt (std::string{_("GnuCash {1}")});
        bl::format dev_fmt (std::string{_("GnuCash {1} development version")});

        if (is_development_version)
            std::cout << dev_fmt % gnc_version () << "\n";
        else
            std::cout << rel_fmt % gnc_version () << "\n";

        std::cout << _("Build ID") << ": " << gnc_build_id () << "\n";
        exit(0);
    }

    if (m_show_help)
    {
        std::cout << *m_opt_desc_display.get() << std::endl;
        exit(0);
    }

    gnc_prefs_set_debugging (m_debug);
    gnc_prefs_set_extra (m_extra);
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
        ("paths", bpo::bool_switch(&m_show_paths),
         _("Show paths"))
        ("logto", bpo::value (&m_log_to_filename),
         _("File to log into; defaults to \"/tmp/gnucash.trace\"; can be \"stderr\" or \"stdout\"."));

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
