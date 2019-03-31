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
extern "C"
{
#include <glib.h>
}
#include <clocale>
#include <boost/locale.hpp>
#include "gnc-locale-utils.hpp"

/* This function addresses two separate problems: First, if we set
 * std::locale::global then all streams automagically imbue
 * themselves with it and we have to re-imbue all of the backends and
 * logging streams with std::locale::classic() so that data and log
 * files aren't localized. Second, calling std::locale("") is slow,
 * so we want to do it only once. Worse, the standard C++ library in
 * Mingw64 chokes on at least some Microsoft-style locale strings
 * (e.g. "Spanish_Spain") but libc's setlocale(LC_ALL, NULL) emits
 * them even if originally fed a Unix-style locale ("es_ES").
 *
 * The solution is this function which caches the setlocale() locale
 * the first time it's called and which uses a boost::locale
 * generator, which does know what to do with (sometimes adjusted)
 * Microsoft locale strings.
 */
const std::locale&
gnc_get_locale()
{
  static std::locale cached;
  static bool tried_already = false;
  if (!tried_already)
  {
    boost::locale::generator gen;
    tried_already = true;
      try
      {
	cached = std::locale("");
      }
      catch (const std::runtime_error& err)
      {
#ifdef __MINGW32__
	  char* locale = g_win32_getlocale();
#else
	  char* locale = g_strdup(setlocale(LC_ALL, ""));
#endif

	  g_log(G_LOG_DOMAIN, G_LOG_LEVEL_WARNING,
		"Failed to create C++ default locale from"
		"%s because %s. Using the 'C' locale for C++.",
		locale, err.what());
	  g_free(locale);
	  cached = std::locale::classic();
      }
  }
  return cached;
}


