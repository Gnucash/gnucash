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

#ifdef __MINGW32__
// Avoid cmath missing function decl.
#undef _GLIBCXX_USE_C99_MATH_TR1
#if (__GNUC__ > 14) || (__GNUC__ == 14 && __GNUC_MINOR__ >= 1)
#undef _GLIBCXX_USE_C99_MATH_FUNCS
#endif
#endif

#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <string>
#include <vector>

namespace Gnucash {

namespace bpo = boost::program_options;

class CoreApp
{
public:
    CoreApp (const char* app_name);

    void parse_command_line (int argc, char **argv);
    void start (void);

protected:
    std::string m_app_name;
    std::string m_tagline;
    boost::optional <std::string> m_log_to_filename;
    boost::optional <std::string> m_file_to_load;

    bpo::options_description m_opt_desc_all;
    std::unique_ptr<bpo::options_description> m_opt_desc_display;
    bpo::variables_map m_opt_map;
    bpo::positional_options_description m_pos_opt_desc;

private:
    void add_common_program_options (void);

    /* Command-line option variables */
    bool m_show_help = false;
    bool m_show_version = false;
    bool m_show_paths = false;
    bool m_debug = false;
    bool m_extra = false;
    boost::optional <std::string> m_gsettings_prefix;
    std::vector <std::string> m_log_flags;

    char *sys_locale = nullptr;
};

using MessageCb = std::function<void(const char*)>;

void gnc_load_scm_config (MessageCb update_message);
}
#endif
