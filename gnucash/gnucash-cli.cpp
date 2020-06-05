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
#include <iostream>

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

        boost::optional <std::string> m_run_report;
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
     _("Run price quote related commands. Currently only one command is supported.\n\n"
       "  get: \tFetch current quotes for all foreign currencies and stocks in the given GnuCash datafile.\n"))
    ("namespace", bpo::value (&m_namespace),
     _("Regular expression determining which namespace commodities will be retrieved for"));
    m_opt_desc->add (quotes_options);

    bpo::options_description report_options(_("Report Generation Options"));
    report_options.add_options()
    ("run-report", bpo::value (&m_run_report),
     _("Runs a report\n"))
    ("export-type", bpo::value (&m_export_type),
     _("Specify export type\n"))
    ("output-file", bpo::value (&m_output_file),
     _("Output file for report\n"));
    m_opt_desc->add (report_options);

}

int
Gnucash::GnucashCli::start ([[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    Gnucash::CoreApp::start();

    if (m_quotes_cmd)
    {
        if (*m_quotes_cmd != "get")
        {
            std::cerr << bl::format (bl::translate("Unknown quotes command '{1}'")) % *m_quotes_cmd << "\n\n"
            << *m_opt_desc.get();
            return 1;
        }

        if (!m_file_to_load || m_file_to_load->empty())
        {
            std::cerr << bl::translate("Missing data file parameter") << "\n\n"
                      << *m_opt_desc.get();
            return 1;
        }
        else
            return Gnucash::add_quotes (*m_file_to_load);
    }

    if (m_run_report && !m_run_report->empty())
    {
        if (!m_file_to_load || m_file_to_load->empty())
        {
            std::cerr << bl::translate("Missing data file parameter") << "\n\n"
                      << *m_opt_desc.get();
            return 1;
        }
        else
            return Gnucash::run_report(*m_file_to_load, m_run_report,
                                       m_export_type, m_output_file);
    }
    return 1;
}

int
main(int argc, char **argv)
{
    Gnucash::GnucashCli application (argv[0]);

    application.parse_command_line (argc, argv);
    application.start (argc, argv);

    return 0;
}
