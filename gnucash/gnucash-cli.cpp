/*
 * gnucash-cli.cpp -- The command line entry point for GnuCash
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

#ifdef __MINGW32__
#include <Windows.h>
#include <fcntl.h>
#endif

#include "gnucash-commands.hpp"
#include "gnucash-core-app.hpp"

extern "C" {
#include <glib/gi18n.h>
#include <gnc-engine.h>
#include <gnc-prefs.h>
}

#include <boost/locale.hpp>
#include <boost/optional.hpp>
#ifdef __MINGW32__
#include <boost/nowide/args.hpp>
#endif
#include <iostream>
#include <gnc-quotes.hpp>

namespace bl = boost::locale;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

namespace Gnucash {

    class GnucashCli : public CoreApp
    {
    public:
        GnucashCli (const char* app_name);
        void parse_command_line (int argc, char **argv);
        int start (int argc, char **argv);
    private:
        void configure_program_options (void);

        boost::optional <std::string> m_quotes_cmd;
        boost::optional <std::string> m_namespace;

        boost::optional <std::string> m_report_cmd;
        boost::optional <std::string> m_report_name;
        boost::optional <std::string> m_export_type;
        boost::optional <std::string> m_output_file;
    };

}

Gnucash::GnucashCli::GnucashCli (const char *app_name) : Gnucash::CoreApp (app_name)
{
    configure_program_options();
}

void
Gnucash::GnucashCli::parse_command_line (int argc, char **argv)
{
    Gnucash::CoreApp::parse_command_line (argc, argv);

    if (!m_log_to_filename || m_log_to_filename->empty())
        m_log_to_filename = "stderr";

    if (m_namespace)
        gnc_prefs_set_namespace_regexp (m_namespace->c_str());
}

// Define command line options specific to gnucash-cli.
void
Gnucash::GnucashCli::configure_program_options (void)
{
    bpo::options_description quotes_options(_("Price Quotes Retrieval Options"));
    quotes_options.add_options()
    ("quotes,Q", bpo::value (&m_quotes_cmd),
     _("Execute price quote related commands. The following commands are supported.\n\n"
       "  info: \tShow Finance::Quote version and exposed quote sources.\n"
       "  get: \tFetch current quotes for all foreign currencies and stocks in the given GnuCash datafile.\n"))
    ("namespace", bpo::value (&m_namespace),
     _("Regular expression determining which namespace commodities will be retrieved for"));
    m_opt_desc_display->add (quotes_options);
    m_opt_desc_all.add (quotes_options);

    bpo::options_description report_options(_("Report Generation Options"));
    report_options.add_options()
    ("report,R", bpo::value (&m_report_cmd),
     _("Execute report related commands. The following commands are supported.\n\n"
     "  list: \tLists available reports.\n"
     "  show: \tDescribe the options modified in the named report. A datafile \
may be specified to describe some saved options.\n"
     "  run: \tRun the named report in the given GnuCash datafile.\n"))
    ("name", bpo::value (&m_report_name),
     _("Name of the report to run\n"))
    ("export-type", bpo::value (&m_export_type),
     _("Specify export type\n"))
    ("output-file", bpo::value (&m_output_file),
     _("Output file for report\n"));
    m_opt_desc_display->add (report_options);
    m_opt_desc_all.add (report_options);

}

int
Gnucash::GnucashCli::start ([[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    Gnucash::CoreApp::start();

    if (m_quotes_cmd)
    {
        if (*m_quotes_cmd == "info")
        {
            auto quotes = gnc_get_quotes_instance();
            if (quotes.cmd_result() == 0)
            {
                std::cout << bl::format (bl::translate ("Found Finance::Quote version {1}.")) % quotes.version() << std::endl;
                std::cout << bl::translate ("Finance::Quote sources: ");
                for (auto source : quotes.sources())
                    std::cout << source << " ";
                std::cout << std::endl;
                return 0;
            }
            else
            {
                std::cerr << bl::translate ("Finance::Quote isn't "
                                            "installed properly.") << "\n";
                std::cerr << bl::translate ("Error message:") << std::endl;
                std::cerr << quotes.error_msg() << std::endl;
                return 1;
            }
        }
        else if (*m_quotes_cmd == "get")
        {

            if (!m_file_to_load || m_file_to_load->empty())
            {
                std::cerr << bl::translate("Missing data file parameter") << "\n\n"
                << *m_opt_desc_display.get();
                return 1;
            }
            else
                return Gnucash::add_quotes (m_file_to_load);
        }
        else
        {
            std::cerr << bl::format (bl::translate("Unknown quotes command '{1}'")) % *m_quotes_cmd << "\n\n"
                      << *m_opt_desc_display.get();
            return 1;
        }
    }

    if (m_report_cmd)
    {
        if (*m_report_cmd == "run")
        {
            if (!m_file_to_load || m_file_to_load->empty())
            {
                std::cerr << bl::translate("Missing data file parameter") << "\n\n"
                          << *m_opt_desc_display.get();
                return 1;
            }
            else
                return Gnucash::run_report(m_file_to_load, m_report_name,
                                           m_export_type, m_output_file);
        }

        // The command "list" does *not* test&pass the m_file_to_load
        // argument because the reports are global rather than
        // per-file objects. In the future, saved reports may be saved
        // into the datafile, therefore one will need to be specified
        // for loading.
        else if (*m_report_cmd == "list")
                return Gnucash::report_list ();

        // The command "show" does test&pass the m_file_to_load
        // argument, and will attempt to load datafile prior to
        // describing report. If loading fails, it will continue
        // showing report options.
        else if (*m_report_cmd == "show")
            if (!m_report_name || m_report_name->empty())
            {
                std::cerr << bl::translate("Missing --name parameter") << "\n\n"
                          << *m_opt_desc_display.get();
                return 1;
            }
            else
                return Gnucash::report_show (m_file_to_load, m_report_name);
        else
        {
            std::cerr << bl::format (bl::translate("Unknown report command '{1}'")) % *m_report_cmd << "\n\n"
                      << *m_opt_desc_display.get();
            return 1;
        }
    }

    std::cerr << bl::translate("Missing command or option") << "\n\n"
              << *m_opt_desc_display.get();

    return 1;
}

int
main(int argc, char **argv)
{
    Gnucash::GnucashCli application (argv[0]);
#ifdef __MINGW32__
    boost::nowide::args a(argc, argv); // Fix arguments - make them UTF-8
#endif
    application.parse_command_line (argc, argv);
    application.start (argc, argv);

    return 0;
}
