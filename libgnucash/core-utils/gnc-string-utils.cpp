/********************************************************************\
 * gnc-string-utils.cpp -- string and list utility functions        *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (C) 2026 Brent McBride <mcbridebt@hotmail.com>         *
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
 *                                                                  *
\********************************************************************/

#include <config.h>

#include <cstring>
#include <string_view>

#include <boost/locale/collator.hpp>
#include <boost/locale/conversion.hpp>
#include <boost/locale/encoding.hpp>
#include <boost/locale/info.hpp>

#include "gnc-locale-utils.hpp"
#include "gnc-string-utils.h"

#ifdef G_OS_WIN32
#include <windows.h>
#endif

int
safe_utf8_collate (const char * da, const char * db)
{
    if (da && !(*da))
        da = nullptr;
    if (db && !(*db))
        db = nullptr;

    if (da && db)
    {
        auto const& coll{std::use_facet<boost::locale::collator<char>>(
            gnc_get_boost_locale())};
        return coll.compare(boost::locale::collate_level::quaternary, da, db);
    }
    if (da)
        return 1;
    if (db)
        return -1;
    return 0;
}

/********************************************************************
 * The following definitions are from gutf8.c, for use by
 * gnc_utf8_validate().  These are all verbatim copies, except for
 * UNICODE_VALID() which has been modified to look for the strict
 * subset of UTF-8 that is valid XML text.
 */

#define UTF8_COMPUTE(Char, Mask, Len)					      \
  if (Char < 128)							      \
    {									      \
      Len = 1;								      \
      Mask = 0x7f;							      \
    }									      \
  else if ((Char & 0xe0) == 0xc0)					      \
    {									      \
      Len = 2;								      \
      Mask = 0x1f;							      \
    }									      \
  else if ((Char & 0xf0) == 0xe0)					      \
    {									      \
      Len = 3;								      \
      Mask = 0x0f;							      \
    }									      \
  else if ((Char & 0xf8) == 0xf0)					      \
    {									      \
      Len = 4;								      \
      Mask = 0x07;							      \
    }									      \
  else if ((Char & 0xfc) == 0xf8)					      \
    {									      \
      Len = 5;								      \
      Mask = 0x03;							      \
    }									      \
  else if ((Char & 0xfe) == 0xfc)					      \
    {									      \
      Len = 6;								      \
      Mask = 0x01;							      \
    }									      \
  else									      \
    Len = -1;

#define UTF8_LENGTH(Char)              \
  ((Char) < 0x80 ? 1 :                 \
   ((Char) < 0x800 ? 2 :               \
    ((Char) < 0x10000 ? 3 :            \
     ((Char) < 0x200000 ? 4 :          \
      ((Char) < 0x4000000 ? 5 : 6)))))


#define UTF8_GET(Result, Chars, Count, Mask, Len)			      \
  (Result) = (Chars)[0] & (Mask);					      \
  for ((Count) = 1; (Count) < (Len); ++(Count))				      \
    {									      \
      if (((Chars)[(Count)] & 0xc0) != 0x80)				      \
	{								      \
	  (Result) = -1;						      \
	  break;							      \
	}								      \
      (Result) <<= 6;							      \
      (Result) |= ((Chars)[(Count)] & 0x3f);				      \
    }

#define UNICODE_VALID(Char)                   \
    ((Char) < 0x110000 &&			      \
     (((Char) & 0xFFFFF800) != 0xD800) &&	      \
     ((Char) < 0xFDD0 || (Char) > 0xFDEF) &&	      \
     ((Char) >= 0x20 || (Char) == 0x09 || (Char) == 0x0A || (Char) == 0x0D) && \
     ((Char) & 0xFFFE) != 0xFFFE)

gboolean
gnc_utf8_validate(const gchar  *str,
                  gssize        max_len,
                  const gchar **end)
{

    g_return_val_if_fail (str != nullptr, false);

    if (end)
        *end = str;

    const gchar *p = str;

    while ((max_len < 0 || (p - str) < max_len) && *p)
    {
        int i, mask = 0, len;
        gunichar result;
        unsigned char c = static_cast<unsigned char>(*p);

        UTF8_COMPUTE (c, mask, len);

        if (len == -1)
            break;

        /* check that the expected number of bytes exists in str */
        if (max_len >= 0 &&
                ((max_len - (p - str)) < len))
            break;

        UTF8_GET (result, p, i, mask, len);

        if (UTF8_LENGTH (result) != len) /* Check for overlong UTF-8 */
            break;

        if (result == static_cast<gunichar>(-1))
            break;

        if (!UNICODE_VALID (result))
            break;

        p += len;
    }

    if (end)
        *end = p;

    /* See that we covered the entire length if a length was
     * passed in, or that we ended on a nul if not
     */
    if (max_len >= 0 &&
            p != (str + max_len))
        return false;
    else if (max_len < 0 &&
             *p != '\0')
        return false;
    else
        return true;
}

void
gnc_utf8_strip_invalid (gchar *str)
{
    gchar *end;

    g_return_if_fail(str);

    if (gnc_utf8_validate(str, -1, const_cast<const gchar **>(&end)))
        return;

    g_warning("Invalid utf8 string: %s", str);
    do
    {
        int len = strlen(end);
        memmove(end, end + 1, len);	/* shuffle the remainder one byte */
    }
    while (!gnc_utf8_validate(str, -1, const_cast<const gchar **>(&end)));
}

gchar *
gnc_utf8_strip_invalid_strdup(const gchar* str)
{
    gchar *result = g_strdup (str);
    gnc_utf8_strip_invalid (result);
    return result;
}

void
gnc_utf8_strip_invalid_and_controls (gchar *str)
{
    const char *controls = "\b\f\n\r\t\v";
    g_return_if_fail (str != nullptr && strlen (str) > 0);
    gnc_utf8_strip_invalid (str); /* First fix the UTF-8 */
    for (gchar *c = str + strlen (str) - 1; c != str; --c)
    {
        bool line_control = (static_cast<unsigned char>(*c) < 0x20);
        if (line_control || strchr(controls, *c) != nullptr)
            *c = ' '; /*replace controls with a single space. */
    }
}

gchar *
gnc_locale_from_utf8(const gchar* str)
{
    g_return_val_if_fail (str != nullptr, nullptr);

    // Convert from UTF-8 to the encoding used in the current locale.
    auto const& info{std::use_facet<boost::locale::info> (
        gnc_get_boost_locale ())};
    if (info.utf8 ())
        return g_strdup (str);
    try
    {
        auto locale_str = boost::locale::conv::from_utf<char> (
            str, info.encoding (), boost::locale::conv::stop);
        return g_strdup (locale_str.c_str ());
    }
    catch (const std::exception& err)
    {
        g_warning ("gnc_locale_from_utf8 failed: %s", err.what ());
        return nullptr;
    }
}

gchar *
gnc_locale_to_utf8(const gchar* str)
{
    g_return_val_if_fail (str != nullptr, nullptr);

    // Convert to UTF-8 from the encoding used in the current locale.
    auto const& info{std::use_facet<boost::locale::info> (
        gnc_get_boost_locale ())};
    if (info.utf8 ())
        return g_strdup (str);
    try
    {
        auto utf8_str = boost::locale::conv::to_utf<char> (
            str, info.encoding (), boost::locale::conv::stop);
        return g_strdup (utf8_str.c_str ());
    }
    catch (const std::exception& err)
    {
        g_warning ("gnc_locale_to_utf8 failed: %s", err.what ());
        return nullptr;
    }
}

GList*
gnc_g_list_map(GList* list, GncGMapFunc fn, gpointer user_data)
{
    GList *rtn = nullptr;
    for (; list != nullptr; list = list->next)
    {
        rtn = g_list_prepend (rtn, (*fn)(list->data, user_data));
    }
    return g_list_reverse (rtn);
}

void
gnc_g_list_cut(GList **list, GList *cut_point)
{
    if (list == nullptr || *list == nullptr)
        return;

    // if it's the first element.
    if (cut_point->prev == nullptr)
    {
        *list = nullptr;
        return;
    }

    cut_point->prev->next = nullptr;
    cut_point->prev = nullptr;
}

static bool
utf8_strstr(char **needle, char *haystack)
{
    auto tmp{boost::locale::normalize (*needle, boost::locale::norm_nfc,
                                       gnc_get_boost_locale ())};
    if (haystack && *haystack &&
        std::string_view{haystack}.find (tmp) != std::string_view::npos)
        return false;

    *needle = g_strdup (tmp.c_str ()); //so that haystack is already normalized
    return true;
}

static gchar *
stringjoin_internal (GList *list_of_strings, const gchar *sep, bool testdups)
{
    gint seplen = sep ? strlen (sep) : 0;
    gint length = -seplen;
    gchar *retval, *p;

    for (GList *n = list_of_strings; n; n = n->next)
    {
        gchar *str = static_cast<gchar*>(n->data);
        if (str && *str)
            length += strlen (str) + seplen;
    }

    if (length <= 0)
        return nullptr;

    p = retval = static_cast<gchar*>(g_malloc0 (length * sizeof (gchar) + 1));
    for (GList *n = list_of_strings; n; n = n->next)
    {
        gchar *str = static_cast<gchar*>(n->data);
        if (!str || !str[0])
            continue;
        if (!testdups || utf8_strstr (&str, retval))
        {
            if (sep && (p != retval))
                p = g_stpcpy (p, sep);
            p = g_stpcpy (p, str);
            if (testdups)
                g_free (str);
        }
    }

    return retval;
}

gchar *
gnc_g_list_stringjoin (GList *list_of_strings, const gchar *sep)
{
    return stringjoin_internal (list_of_strings, sep, false);
}

gchar *
gnc_g_list_stringjoin_nodups (GList *list_of_strings, const gchar *sep)
{
    return stringjoin_internal (list_of_strings, sep, true);
}

gint
gnc_list_length_cmp (const GList *list, size_t len)
{
    for (GList *lst = const_cast<GList*>(list);;
         lst = g_list_next (lst), len--)
    {
        if (!lst) return (len ? -1 : 0);
        if (!len) return 1;
    }
}
