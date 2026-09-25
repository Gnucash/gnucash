/*
 * gnucash.cpp -- The program entry point for GnuCash
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
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

#include "gnucash-core-app.hpp"

#include <gtk/gtk.h>
#ifdef __MINGW32__
#include <Windows.h>
#include <fcntl.h>
#endif

#include "gnucash-commands.hpp"
#include "gnucash-guile-bootstrap.h"

#include <glib/gi18n.h>
#include <dialog-new-user.h>
#include <gfec.h>
#include <gnc-engine.h> // For define GNC_MOD_GUI
#include <gnc-file.h>
#include <gnc-filepath-utils.h>
#include <gnc-gnome-utils.h>
#include <gnc-hooks.h>
#include <gnc-module.h>
#include <gnc-path.h>
#include <gnc-plugin-bi-import.h>
#include <gnc-plugin-csv-export.h>
#include <gnc-plugin-csv-import.h>
#include <gnc-plugin-customer-import.h>
#include <gnc-plugin-file-history.h>
#include <gnc-plugin-log-replay.h>
#include <gnc-plugin-qif-import.h>
#include <gnc-plugin-report-system.h>
#include <gnc-prefs.h>
#include <gnc-prefs-utils.h>
#include <gnc-session.h>
#include <gnc-splash.h>
#include <gnc-ui.h>
#include <gnucash-register.h>
#include <search-core-type.h>
#include <top-level.h>

#include <boost/locale.hpp>
#include <boost/optional.hpp>
#ifdef __MINGW32__
#include <boost/nowide/args.hpp>
#endif
#include <iostream>
#include <string>
#include <vector>
#include <gnc-report.h>
#include <gnc-locale-utils.hpp>
#include <gnc-quotes.hpp>

namespace bl = boost::locale;

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

static void
load_gnucash_plugins()
{
    gnc_plugin_bi_import_create_plugin ();
    gnc_plugin_csv_export_create_plugin ();
    gnc_plugin_csv_import_create_plugin();
    gnc_plugin_customer_import_create_plugin ();
    gnc_plugin_qif_import_create_plugin ();
    gnc_plugin_log_replay_create_plugin ();
}

static void
load_gnucash_modules()
{
    struct
    {
        const gchar * name;
        int version;
        gboolean optional;
    } modules[] =
    {
        { "gnucash/import-export/ofx", 0, TRUE },
        { "gnucash/import-export/aqbanking", 0, TRUE },
        { "gnucash/python", 0, TRUE },
    };

    /* module initializations go here */
    int len = sizeof(modules) / sizeof(*modules);
    for (int i = 0; i < len; i++)
    {
        DEBUG("Loading module %s started", modules[i].name);
        gnc_update_splash_screen(modules[i].name, GNC_SPLASH_PERCENTAGE_UNKNOWN);
        if (modules[i].optional)
            gnc_module_load_optional(modules[i].name, modules[i].version);
        else
            gnc_module_load(modules[i].name, modules[i].version);
        DEBUG("Loading module %s finished", modules[i].name);
    }
}

static char *
get_file_to_load (const char* file_to_load, bool nofile)
{
    if (file_to_load && *file_to_load != '\0')
        return g_strdup(file_to_load);
    if (nofile)
        return nullptr;
    /* Note history will always return a valid (possibly empty) string */
    return gnc_history_get_last();
}

extern SCM scm_init_sw_gnome_module(void);

struct t_file_spec {
    int nofile;
    const char *file_to_load;
    std::string *pending_open_file;
};

static void
scm_run_gnucash (void *data, [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    auto user_file_spec = static_cast<t_file_spec*>(data);

    scm_c_eval_string("(debug-set! stack 200000)");

    auto main_mod = scm_c_resolve_module("gnucash utilities");
    scm_set_current_module(main_mod);
    scm_c_use_module("gnucash app-utils");

    gnc_gnome_utils_init();
    gnc_search_core_initialize ();
    gnc_hook_add_dangler(HOOK_UI_SHUTDOWN, (GFunc)gnc_search_core_finalize, NULL, NULL);
    gnucash_register_add_cell_types ();
    gnc_report_init ();

    load_gnucash_plugins();
    load_gnucash_modules();

    /* Load the scm config files before starting up the gui. This ensures that
     * custom reports have been read into memory before the Reports
     * menu is created. */
    Gnucash::gnc_load_scm_config ([](const gchar *msg)
    {
        gnc_update_splash_screen (msg, GNC_SPLASH_PERCENTAGE_UNKNOWN);
        PINFO ("%s", msg);
    });

    /* Setting-up the report menu must come after the module
     loading but before the gui initialization. */
    gnc_plugin_report_system_new();

    /* TODO: After some more guile-extraction, this should happen even
       before booting guile.  */
    gnc_main_gui_init();

    gnc_hook_add_dangler(HOOK_UI_SHUTDOWN, (GFunc)gnc_file_quit, NULL, NULL);

    /* Install Price Quote Sources */
    try
    {
        const auto checking = _("Checking Finance::Quote…");
        gnc_update_splash_screen (checking, GNC_SPLASH_PERCENTAGE_UNKNOWN);
        GncQuotes quotes;
        auto found = (bl::format (std::string{_("Found Finance::Quote version {1}.")}) % quotes.version()).str();
        auto quote_sources = quotes.sources();
        gnc_quote_source_set_fq_installed (quotes.version().c_str(), quote_sources);
        gnc_update_splash_screen (found.c_str(), GNC_SPLASH_PERCENTAGE_UNKNOWN);
    }
    catch (const GncQuoteException& err)
    {
        auto msg = _("Unable to load Finance::Quote.");
        PINFO ("Attempt to load Finance::Quote returned this error message:\n");
        PINFO ("%s", err.what());
        gnc_update_splash_screen (msg, GNC_SPLASH_PERCENTAGE_UNKNOWN);
    }

    gnc_hook_run(HOOK_STARTUP, NULL);

    char* fn = nullptr;
    auto requested_file = user_file_spec->file_to_load;
    auto from_open_event = (!requested_file || !*requested_file) &&
                           !user_file_spec->pending_open_file->empty ();
    if (from_open_event)
        requested_file = user_file_spec->pending_open_file->c_str ();
    if ((fn = get_file_to_load (requested_file, user_file_spec->nofile)) && *fn )
    {
        if (from_open_event)
            user_file_spec->pending_open_file->clear ();
        auto msg = _("Loading data…");
        gnc_update_splash_screen (msg, GNC_SPLASH_PERCENTAGE_UNKNOWN);
        gnc_file_open_file(nullptr, fn, /*open_readonly*/ FALSE);
        g_free(fn);
    }
    else if (gnc_prefs_get_bool(GNC_PREFS_GROUP_NEW_USER, GNC_PREF_FIRST_STARTUP))
    {
        g_free(fn); /* fn could be an empty string ("") */
        gnc_destroy_splash_screen();
        gnc_ui_new_user_dialog();
    }

    /* Ensure temporary preferences are temporary */
    gnc_prefs_reset_group (GNC_PREFS_GROUP_WARNINGS_TEMP);

    gnc_destroy_splash_screen();
    gnc_main_window_show_all_windows();

    gnc_hook_run(HOOK_UI_POST_STARTUP, NULL);
    gnc_ui_start_event_loop();
    return;
}

namespace Gnucash {

    class Gnucash : public CoreApp
    {
    public:
        Gnucash (const char* app_name);
        CommandLineResult parse_command_line (int argc, char **argv);
        int start (int argc, char **argv);
        int run (int argc, char **argv);
        void activate (void);
        int command_line (GApplicationCommandLine *command_line);
        void open (GFile **files, gint n_files);

    private:
        void configure_program_options (void);

        bool m_nofile = false;
        bool m_started = false;
        bool m_starting = false;
        int m_exit_status = 0;
        int m_argc = 0;
        char **m_argv = nullptr;
        std::string m_pending_open_file;
        std::vector<std::string> m_pending_extra_files;
    };

}

Gnucash::Gnucash::Gnucash (const char *app_name) : Gnucash::CoreApp (app_name)
{
    configure_program_options();
}


Gnucash::CommandLineResult
Gnucash::Gnucash::parse_command_line (int argc, char **argv)
{
    return Gnucash::CoreApp::parse_command_line (argc, argv);
}

// Define command line options specific to gnucash.
void
Gnucash::Gnucash::configure_program_options (void)
{

    bpo::options_description app_options(_("Application Options"));
    app_options.add_options()
    ("nofile", bpo::bool_switch (&m_nofile),
     _("Do not load the last file opened"));

    m_opt_desc_display->add (app_options);
    m_opt_desc_all.add (app_options);
}

int
Gnucash::Gnucash::start (int argc, char **argv)
{
    Gnucash::CoreApp::start();

    /* Now the module files are looked up, which might cause some library
     initialization to be run, hence gtk must be initialized beforehand. */
    gnc_module_system_init();

    gnc_gui_init();

    auto user_file_spec = t_file_spec {
        m_nofile,
        m_file_to_load ? m_file_to_load->c_str() : "",
        &m_pending_open_file};
    scm_run_gnucash (&user_file_spec, argc, argv);

    return 0;
}

void
Gnucash::Gnucash::activate (void)
{
    if (m_started)
    {
        gnc_main_window_show_all_windows ();
        return;
    }

    m_started = true;
    m_starting = true;
    m_exit_status = start (m_argc, m_argv);
    m_starting = false;
    if (m_exit_status == 0)
    {
        /* A command-line file takes precedence at startup. Finder files that
         * arrived alongside it, and additional open requests, follow it. */
        if (!m_pending_open_file.empty ())
            m_pending_extra_files.insert (m_pending_extra_files.begin (),
                                          m_pending_open_file);
        for (const auto& uri : m_pending_extra_files)
            gnc_file_open_file (gnc_ui_get_main_window (nullptr), uri.c_str (),
                                /*open_readonly*/ FALSE);
    }
    m_pending_open_file.clear ();
    m_pending_extra_files.clear ();
    auto application = g_application_get_default ();
    if (m_exit_status == 0)
    {
        /* Keep the application alive after its last window is closed until
         * GnuCash has completed the asynchronous save-and-shutdown path. */
        if (application)
            g_application_hold (application);
    }
    else
    {
        if (application)
            g_application_quit (application);
    }
}

static void
on_application_activate ([[maybe_unused]] GtkApplication *application, gpointer user_data)
{
    static_cast<Gnucash::Gnucash*>(user_data)->activate ();
}

void
Gnucash::Gnucash::open (GFile **files, gint n_files)
{
    for (gint i = 0; i < n_files; ++i)
    {
        auto uri = g_file_get_uri (files[i]);
        if (!m_started || m_starting)
        {
            if (m_pending_open_file.empty ())
                m_pending_open_file = uri;
            else
                m_pending_extra_files.emplace_back (uri);
        }
        else
            gnc_file_open_file (gnc_ui_get_main_window (nullptr), uri,
                                /*open_readonly*/ FALSE);
        g_free (uri);
    }

    if (!m_started)
        activate ();
}

static void
on_application_open ([[maybe_unused]] GApplication *application, GFile **files,
                     gint n_files, [[maybe_unused]] const char *hint,
                     gpointer user_data)
{
    static_cast<Gnucash::Gnucash*>(user_data)->open (files, n_files);
}

#ifdef MAC_INTEGRATION
static void
on_macos_application_action (GSimpleAction *action, GVariant *parameter,
                             gpointer user_data)
{
    auto application = GTK_APPLICATION (user_data);
    auto active_window = gtk_application_get_active_window (application);
    auto main_window = gnc_ui_get_main_window (active_window
                                               ? GTK_WIDGET (active_window)
                                               : nullptr);
    const auto action_name = g_action_get_name (G_ACTION (action));
    const char *window_action = nullptr;

    (void)parameter;
    if (g_str_equal (action_name, "quit"))
        window_action = "FileQuitAction";
    else if (g_str_equal (action_name, "preferences"))
        window_action = "EditPreferencesAction";
    else if (g_str_equal (action_name, "about"))
        window_action = "HelpAboutAction";

    if (main_window && window_action)
        g_action_group_activate_action (G_ACTION_GROUP (main_window),
                                        window_action, nullptr);
    else if (g_str_equal (action_name, "quit"))
        g_application_quit (G_APPLICATION (application));
}

static void
on_macos_application_startup (GApplication *application, gpointer user_data)
{
    static const GActionEntry actions[] =
    {
        { "about", on_macos_application_action, nullptr, nullptr, nullptr },
        { "preferences", on_macos_application_action, nullptr, nullptr, nullptr },
        { "quit", on_macos_application_action, nullptr, nullptr, nullptr },
    };
    const char *quit_accels[] = { "<Meta>q", nullptr };
    const char *preferences_accels[] = { "<Meta>comma", nullptr };

    (void)user_data;
    g_action_map_add_action_entries (G_ACTION_MAP (application), actions,
                                     G_N_ELEMENTS (actions), application);
    gtk_application_set_accels_for_action (GTK_APPLICATION (application),
                                           "app.quit", quit_accels);
    gtk_application_set_accels_for_action (GTK_APPLICATION (application),
                                           "app.preferences",
                                           preferences_accels);
}
#endif

static int
on_application_command_line ([[maybe_unused]] GApplication *application,
                             GApplicationCommandLine *command_line,
                             gpointer user_data)
{
    return static_cast<Gnucash::Gnucash*>(user_data)->command_line (command_line);
}

int
Gnucash::Gnucash::command_line (GApplicationCommandLine *command_line)
{
    gint argc = 0;
    auto argv = g_application_command_line_get_arguments (command_line, &argc);

    if (!m_started)
    {
        m_argc = argc;
        m_argv = argv;
        activate ();
        m_argc = 0;
        m_argv = nullptr;
        g_strfreev (argv);
        return m_exit_status;
    }

    /* GApplication forwards later invocations to this process. GnuCash has a
     * single active book, so it can only forward one positional data file to
     * the existing file-opening path. Do not silently treat unsupported
     * invocations as activation requests. */
    if (argc == 1)
    {
        if (!m_starting)
            activate ();
    }
    else if (argc == 2 && argv[1][0] != '-')
    {
        auto file = g_application_command_line_create_file_for_arg (
            command_line, argv[1]);
        auto filename = g_file_get_uri (file);

        auto open_result = GNC_FILE_OPEN_QUEUED;
        if (m_starting)
            m_pending_extra_files.emplace_back (filename);
        else
            open_result = gnc_file_open_file (gnc_ui_get_main_window (nullptr),
                                               filename,
                                               /*open_readonly*/ FALSE);
        g_free (filename);
        g_object_unref (file);
        if (open_result == GNC_FILE_OPEN_QUEUED)
            g_application_command_line_print (
                command_line, "%s\n",
                _("GnuCash is busy completing another file operation. "
                  "The requested file has been queued."));
        else if (open_result == GNC_FILE_OPEN_REJECTED)
        {
            g_application_command_line_printerr (
                command_line, "%s\n",
                _("GnuCash is shutting down. "
                  "The requested file was not opened."));
            g_strfreev (argv);
            return 1;
        }
    }
    else
    {
        g_application_command_line_printerr (
            command_line, "%s\n",
            _("A separate GnuCash instance is already running. "
              "Open files one at a time."));
        g_strfreev (argv);
        return 1;
    }

    g_strfreev (argv);
    return 0;
}

int
Gnucash::Gnucash::run (int argc, char **argv)
{
    /* Parse in the invoking process before GApplication forwards the
     * command line. Informational options and parser errors must be written
     * to that process's stdout/stderr, even if another instance owns the
     * application name. */
    auto parse_result = parse_command_line (argc, argv);
    if (parse_result != CommandLineResult::Run)
        return parse_result == CommandLineResult::ExitSuccess ? 0 : 1;

    auto gtk_application = gtk_application_new ("org.gnucash.GnuCash",
                                                static_cast<GApplicationFlags> (
                                                    G_APPLICATION_HANDLES_COMMAND_LINE |
                                                    G_APPLICATION_HANDLES_OPEN));
#ifdef MAC_INTEGRATION
    g_signal_connect (gtk_application, "startup",
                      G_CALLBACK (on_macos_application_startup), nullptr);
#endif
    g_signal_connect (gtk_application, "activate", G_CALLBACK (on_application_activate), this);
    g_signal_connect (gtk_application, "open", G_CALLBACK (on_application_open), this);
    g_signal_connect (gtk_application, "command-line",
                      G_CALLBACK (on_application_command_line), this);

    auto status = g_application_run (G_APPLICATION (gtk_application), argc, argv);

    if (m_started)
    {
        gnc_ui_stop_event_loop ();
        gnc_hook_remove_dangler (HOOK_UI_SHUTDOWN, (GFunc)gnc_file_quit);
    }
    g_object_unref (gtk_application);

    if (m_started)
        gnc_shutdown (status == 0 ? m_exit_status : status);

    return status == 0 ? m_exit_status : status;
}

static int
run_gnucash_application (int argc, char **argv, void *user_data)
{
    return static_cast<Gnucash::Gnucash *> (user_data)->run (argc, argv);
}

int
main (int argc, char **argv)
{
    Gnucash::Gnucash application (PROJECT_NAME);
#ifdef __MINGW32__
    boost::nowide::args a(argc, argv); // Fix arguments - make them UTF-8
#endif
    gnc_run_with_guile (argc, argv, run_gnucash_application, &application);
}
