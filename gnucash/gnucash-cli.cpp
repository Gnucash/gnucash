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

        bool m_add_quotes;
        std::string m_run_report;
        std::string m_export_type;
        std::string m_output_file;
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

    if (m_log_to_filename.empty())
        m_log_to_filename.assign("stderr");

    m_add_quotes = m_opt_map["add-price-quotes"].as<bool>();

    if (m_opt_map.count ("namespace"))
        gnc_prefs_set_namespace_regexp(m_opt_map["namespace"].
        as<std::string>().c_str());

    if (m_opt_map.count ("run-report"))
        m_run_report = m_opt_map["run-report"].as<std::string>();

    if (m_opt_map.count ("export-type"))
        m_export_type = m_opt_map["export-type"].as<std::string>();

    if (m_opt_map.count ("output-file"))
        m_output_file = m_opt_map["output-file"].as<std::string>();
}

// Define command line options specific to gnucash-cli.
void
Gnucash::GnucashCli::configure_program_options (void)
{
    bpo::options_description quotes_options(_("Price Quotes Retrieval Options"));
    quotes_options.add_options()
    ("add-price-quotes", bpo::bool_switch(),
     _("Add price quotes to given GnuCash datafile."))
    ("namespace", bpo::value<std::string>(),
     _("Regular expression determining which namespace commodities will be retrieved"));
    m_opt_desc->add (quotes_options);

    bpo::options_description report_options(_("Report Generation Options"));
    report_options.add_options()
    ("run-report", bpo::value<std::string>(),
     _("Runs a report\n"))
    ("export-type", bpo::value<std::string>(),
     _("Specify export type\n"))
    ("output-file", bpo::value<std::string>(),
     _("Output file for report\n"));
    m_opt_desc->add (report_options);

}

int
Gnucash::GnucashCli::start ([[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    Gnucash::CoreApp::start();

    if (m_add_quotes)
    {
        if (m_file_to_load.empty())
        {
            std::cerr << bl::translate("Missing data file parameter") << "\n\n"
                      << *m_opt_desc.get();
            return 1;
        }
        else
            return Gnucash::add_quotes (m_file_to_load);
    }

    if (!m_run_report.empty())
    {
        if (m_file_to_load.empty())
        {
            std::cerr << bl::translate("Missing data file parameter") << "\n\n"
                      << *m_opt_desc.get();
            return 1;
        }
        else
            return Gnucash::run_report(m_file_to_load, m_run_report,
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
