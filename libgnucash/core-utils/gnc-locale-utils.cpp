/********************************************************************\
 * gnc-locale-utils.cpp -- provide a default locale for C++         *
 * Copyright (C) 2019 John Ralls <jralls@ceridwen.us                *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
#include <glib.h>
#include <clocale>
#include <boost/locale.hpp>
#include "gnc-locale-utils.hpp"
#include <config.h>

/** Cache the UI locale
 *
 * We don't want to set the default locale because we need
 * std::locale::classic for consistency in stored data. We also don't
 * want to call std::locale("") more than once, it's slow... and we
 * don't want to call it on MinGW32 at all because that supports only
 * std::locale::classic and throws if you try to construct anything
 * else. Boost::locale doesn't support std::locale::facet required by
 * boost::datetime (go figure).
 *
 * @return a copy of std::locale::classic() on MinGW32 and a copy of
 * the cached result of std::locale("") otherwise.
 */
const std::locale&
gnc_get_locale()
{
#ifdef __MINGW32__
    return std::locale::classic(); // Nothing else supported.
#else
    static std::locale cached;
    static bool tried_already = false;
    if (!tried_already)
    {
	tried_already = true;
	try
	{
	    cached = std::locale("");
	}
	catch (const std::runtime_error& err)
	{
	    char* locale = g_strdup(setlocale(LC_ALL, ""));

	    g_log(G_LOG_DOMAIN, G_LOG_LEVEL_WARNING,
		  "Failed to create C++ default locale from "
		  "%s because %s. Using the 'C' locale for C++.",
		  locale, err.what());
	    g_free(locale);
	    cached = std::locale::classic();
	}
    }
    return cached;
#endif
}


static std::locale boost_cached;
static bool tried_boost_already = false;

void
gnc_init_boost_locale (const std::string& messages_path)
{
    if (!tried_boost_already)
    {
        tried_boost_already = true;

        try
        {
            boost::locale::generator gen;
            if (!messages_path.empty())
                gen.add_messages_path(messages_path);
            else
                g_log(G_LOG_DOMAIN, G_LOG_LEVEL_WARNING,
                      "Attempt to initialize boost_locale without a message_path. "
                      "If message catalogs are not installed in the system's default locations "
                      "user interface strings will not be translated.");
            gen.add_messages_domain(PROJECT_NAME);
            boost_cached = gen ("");
        }
        catch (const std::runtime_error& err)
        {
            const char* locale = setlocale(LC_ALL, "");

            g_log(G_LOG_DOMAIN, G_LOG_LEVEL_WARNING,
                  "Failed to create C++ default locale from"
                  "%s because %s. Using the 'C' locale for C++.",
                  locale, err.what());
            boost_cached = std::locale::classic();
        }
    }
}



const std::locale&
gnc_get_boost_locale()
{
    return boost_cached;
}


