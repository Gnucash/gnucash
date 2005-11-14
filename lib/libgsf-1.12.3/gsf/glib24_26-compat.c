/** jsled, 2005-11-08: copied from glib-2.6.6 to support libgsf compilation
    against glib-2.4.14. **/

#include <gsf/glib24_26-compat.h>

/* gstdio.c - wrappers for C library functions
 *
 * Copyright 2004 Tor Lillqvist
 *
 * GLib is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * GLib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with GLib; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "config.h"

//#include <stdio.h>
#include <string.h>
//#include <stdlib.h>

/** ------------------------------------------------------------ **/
/* START gconvert.c  */

#include <glib/gunicode.h>
#include <glib/gconvert.h>
#include <glib/gthread.h>

typedef struct _GFilenameCharsetCache GFilenameCharsetCache;

struct _GFilenameCharsetCache {
  gboolean is_utf8;
  gchar *charset;
  gchar **filename_charsets;
};

/**
 * g_get_filename_charsets:
 * @charsets: return location for the %NULL-terminated list of encoding names
 *
 * Determines the preferred character sets used for filenames.
 * The first character set from the @charsets is the filename encoding, the
 * subsequent character sets are used when trying to generate a displayable
 * representation of a filename, see g_filename_display_name().
 *
 * On Unix, the character sets are determined by consulting the
 * environment variables <envar>G_FILENAME_ENCODING</envar> and
 * <envar>G_BROKEN_FILENAMES</envar>. On Windows, the character set
 * used in the GLib API is always UTF-8 and said environment variables
 * have no effect.
 *
 * <envar>G_FILENAME_ENCODING</envar> may be set to a comma-separated list 
 * of character set names. The special token "@locale" is taken to mean the 
 * character set for the current locale. If <envar>G_FILENAME_ENCODING</envar> 
 * is not set, but <envar>G_BROKEN_FILENAMES</envar> is, the character set of 
 * the current locale is taken as the filename encoding. If neither environment
 * variable is set, UTF-8 is taken as the filename encoding, but the character
 * set of the current locale is also put in the list of encodings.
 *
 * The returned @charsets belong to GLib and must not be freed.
 *
 * Note that on Unix, regardless of the locale character set or
 * <envar>G_FILENAME_ENCODING</envar> value, the actual file names present on a
 * system might be in any random encoding or just gibberish.
 *
 * Return value: %TRUE if the filename encoding is UTF-8.
 * 
 * Since: 2.6
 */
gboolean
g_get_filename_charsets (G_CONST_RETURN gchar ***filename_charsets) 
{
  static const gchar *charsets[] = {
    "UTF-8",
    NULL
  };

#ifdef G_OS_WIN32
  /* On Windows GLib pretends that the filename charset is UTF-8 */
  if (filename_charsets)
    *filename_charsets = charsets;

  return TRUE;
#else
  gboolean result;

  /* Cygwin works like before */
  result = g_get_charset (&(charsets[0]));

  if (filename_charsets)
    *filename_charsets = charsets;

  return result;
#endif
}

static gchar *
make_valid_utf8 (const gchar *name)
{
  GString *string;
  const gchar *remainder, *invalid;
  gint remaining_bytes, valid_bytes;
  
  string = NULL;
  remainder = name;
  remaining_bytes = strlen (name);
  
  while (remaining_bytes != 0) 
    {
      if (g_utf8_validate (remainder, remaining_bytes, &invalid)) 
	break;
      valid_bytes = invalid - remainder;
    
      if (string == NULL) 
	string = g_string_sized_new (remaining_bytes);

      g_string_append_len (string, remainder, valid_bytes);
      g_string_append_c (string, '?');
      
      remaining_bytes -= valid_bytes + 1;
      remainder = invalid + 1;
    }
  
  if (string == NULL)
    return g_strdup (name);
  
  g_string_append (string, remainder);
  g_string_append (string, " (invalid encoding)");

  g_assert (g_utf8_validate (string->str, -1, NULL));
  
  return g_string_free (string, FALSE);
}

/**
 * g_filename_display_name:
 * @filename: a pathname hopefully in the GLib file name encoding
 * 
 * Converts a filename into a valid UTF-8 string. The 
 * conversion is not necessarily reversible, so you 
 * should keep the original around and use the return
 * value of this function only for display purposes.
 * Unlike g_filename_to_utf8(), the result is guaranteed 
 * to be non-NULL even if the filename actually isn't in the GLib
 * file name encoding.
 *
 * If you know the whole pathname of the file you should use
 * g_filename_display_basename(), since that allows location-based
 * translation of filenames.
 *
 * Return value: a newly allocated string containing
 *   a rendition of the filename in valid UTF-8
 *
 * Since: 2.6
 **/
gchar *
g_filename_display_name (const gchar *filename)
{
  gint i;
  const gchar **charsets;
  gchar *display_name = NULL;
  gboolean is_utf8;
 
  is_utf8 = g_get_filename_charsets (&charsets);

  if (is_utf8)
    {
      if (g_utf8_validate (filename, -1, NULL))
	display_name = g_strdup (filename);
    }
  
  if (!display_name)
    {
      /* Try to convert from the filename charsets to UTF-8.
       * Skip the first charset if it is UTF-8.
       */
      for (i = is_utf8 ? 1 : 0; charsets[i]; i++)
	{
	  display_name = g_convert (filename, -1, "UTF-8", charsets[i], 
				    NULL, NULL, NULL);

	  if (display_name)
	    break;
	}
    }
  
  /* if all conversions failed, we replace invalid UTF-8
   * by a question mark
   */
  if (!display_name) 
    display_name = make_valid_utf8 (filename);

  return display_name;
}
