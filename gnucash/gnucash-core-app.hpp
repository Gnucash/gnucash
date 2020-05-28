/*
 * gnucash-core-app.hpp -- Core application object for gnucash binaries
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

#ifndef GNUCASH_CORE_APP_HPP
#define GNUCASH_CORE_APP_HPP

#include <boost/program_options.hpp>
#include <string>
#include <vector>

namespace Gnucash {

namespace bpo = boost::program_options;

class CoreApp
{
public:
    CoreApp ();
    CoreApp (const char* app_name);

    void parse_command_line (int *argc, char ***argv);
    void start (void);

    const char *get_file_to_load (void);
    int get_no_file (void);

protected:
    int gtk_show_help = 0;
    std::string m_app_name;
    std::string tagline;

    std::unique_ptr<bpo::options_description> m_opt_desc;
    bpo::variables_map m_opt_map;
    bpo::positional_options_description m_pos_opt_desc;

private:
    void add_common_program_options (void);

    /* Command-line option variables */
    int gnucash_show_version = 0;
    int debugging = 0;
    int extra = 0;
    char **log_flags;
    char *log_to_filename = nullptr;
    int nofile = 0;
    const char *gsettings_prefix = nullptr;
    const char *add_quotes_file = nullptr;
    char *namespace_regexp = nullptr;
    const char *file_to_load = nullptr;
    char **args_remaining = nullptr;
    char *sys_locale = nullptr;
};

}
#endif
