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
#ifdef __MINGW32__
#include <Windows.h>
#include <fcntl.h>
#endif

#include "gnucash-commands.hpp"
#include "gnucash-core-app.hpp"

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
#include <gnucash-register.h>
#include <search-core-type.h>
#include <top-level.h>

#include <boost/locale.hpp>
#include <boost/optional.hpp>
#ifdef __MINGW32__
#include <boost/nowide/args.hpp>
#endif
#include <iostream>
#include <gnc-report.h>
#include <gnc-locale-utils.hpp>
#include <gnc-quotes.hpp>

namespace bl = boost::locale;

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;
static gchar *userdata_migration_msg = NULL;

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
get_file_to_load (const char* file_to_load)
{
    if (file_to_load && *file_to_load != '\0')
        return g_strdup(file_to_load);
    else
        /* Note history will always return a valid (possibly empty) string */
        return gnc_history_get_last();
}

extern SCM scm_init_sw_gnome_module(void);

struct t_file_spec {
    int nofile;
    const char *file_to_load;
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
    if (!user_file_spec->nofile && (fn = get_file_to_load (user_file_spec->file_to_load)) && *fn )
    {
        auto msg = _("Loading data…");
        gnc_update_splash_screen (msg, GNC_SPLASH_PERCENTAGE_UNKNOWN);
        gnc_file_open_file(gnc_get_splash_screen(), fn, /*open_readonly*/ FALSE);
        g_free(fn);
    }
    else if (gnc_prefs_get_bool(GNC_PREFS_GROUP_NEW_USER, GNC_PREF_FIRST_STARTUP))
    {
        g_free(fn); /* fn could be an empty string ("") */
        gnc_destroy_splash_screen();
        gnc_ui_new_user_dialog();
    }

    if (userdata_migration_msg)
    {
        GtkWidget *dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
                                                   GTK_MESSAGE_INFO,
                                                   GTK_BUTTONS_OK,
                                                   "%s",
                                                   userdata_migration_msg);
        gnc_destroy_splash_screen();
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy (dialog);
        g_free (userdata_migration_msg);
    }
    /* Ensure temporary preferences are temporary */
    gnc_prefs_reset_group (GNC_PREFS_GROUP_WARNINGS_TEMP);

    gnc_destroy_splash_screen();
    gnc_main_window_show_all_windows();

    gnc_hook_run(HOOK_UI_POST_STARTUP, NULL);
    gnc_ui_start_event_loop();
    gnc_hook_remove_dangler(HOOK_UI_SHUTDOWN, (GFunc)gnc_file_quit);

    gnc_shutdown(0);
    return;
}

namespace Gnucash {

    class Gnucash : public CoreApp
    {
    public:
        Gnucash (const char* app_name);
        void parse_command_line (int argc, char **argv);
        int start (int argc, char **argv);

    private:
        void configure_program_options (void);

        bool m_nofile = false;
    };

}

Gnucash::Gnucash::Gnucash (const char *app_name) : Gnucash::CoreApp (app_name)
{
    configure_program_options();
}


void
Gnucash::Gnucash::parse_command_line (int argc, char **argv)
{
    Gnucash::CoreApp::parse_command_line (argc, argv);
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
Gnucash::Gnucash::start ([[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    Gnucash::CoreApp::start();

    /* Now the module files are looked up, which might cause some library
     initialization to be run, hence gtk must be initialized beforehand. */
    gnc_module_system_init();

    gnc_gui_init();

    auto user_file_spec = t_file_spec {
        m_nofile,
        m_file_to_load ? m_file_to_load->c_str() : ""};
    scm_boot_guile (argc, argv, scm_run_gnucash, &user_file_spec);

    return 0;
}

int
main(int argc, char ** argv)
{
    Gnucash::Gnucash application (PROJECT_NAME);
#ifdef __MINGW32__
    boost::nowide::args a(argc, argv); // Fix arguments - make them UTF-8
#endif
    /* We need to initialize gtk before looking up all modules */
    if(!gtk_init_check (&argc, &argv))
    {
        std::cerr << bl::format (std::string{("Run '{1} --help' to see a full list of available command line options.")}) % *argv[0]
        << "\n"
        // Translators: Do not translate $DISPLAY! It is an environment variable for X11
        << _("Error: could not initialize graphical user interface and option add-price-quotes was not set.\n"
        "Perhaps you need to set the $DISPLAY environment variable?");
        return 1;
    }

    application.parse_command_line (argc, argv);
    return application.start (argc, argv);
}
