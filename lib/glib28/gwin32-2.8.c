/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1998  Peter Mattis, Spencer Kimball and Josh MacDonald
 * Copyright (C) 1998-1999  Tor Lillqvist
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/* Contains all #includes, but otherwise only relevant differences between
 * GLib 2.6 and GLib 2.8 */

#include "config.h"

#include <glibconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>
#include <errno.h>

#define STRICT			/* Strict typing, please */
#include <windows.h>
#undef STRICT
#ifndef G_WITH_CYGWIN
#include <direct.h>
#endif
#include <errno.h>
#include <ctype.h>
#ifdef _MSC_VER
#  include <io.h>
#endif /* _MSC_VER */

#include <glib.h>


/**
 * g_win32_locale_filename_from_utf8:
 * @utf8filename: a UTF-8 encoded filename.
 *
 * Converts a filename from UTF-8 to the system codepage.
 *
 * On NT-based Windows, on NTFS file systems, file names are in
 * Unicode. It is quite possible that Unicode file names contain
 * characters not representable in the system codepage. (For instance,
 * Greek or Cyrillic characters on Western European or US Windows
 * installations, or various less common CJK characters on CJK Windows
 * installations.)
 *
 * In such a case, and if the filename refers to an existing file, and
 * the file system stores alternate short (8.3) names for directory
 * entries, the short form of the filename is returned. Note that the
 * "short" name might in fact be longer than the Unicode name if the
 * Unicode name has very short pathname components containing
 * non-ASCII characters. If no system codepage name for the file is
 * possible, %NULL is returned.
 *
 * The return value is dynamically allocated and should be freed with
 * g_free() when no longer needed.
 *
 * Return value: The converted filename, or %NULL on conversion
 * failure and lack of short names.
 *
 * Since: 2.8
 */
gchar *
g_win32_locale_filename_from_utf8 (const gchar *utf8filename)
{
  gchar *retval = g_locale_from_utf8 (utf8filename, -1, NULL, NULL, NULL);

  if (retval == NULL && G_WIN32_HAVE_WIDECHAR_API ())
    {
      /* Conversion failed, so convert to wide chars, check if there
       * is a 8.3 version, and use that.
       */
      wchar_t *wname = g_utf8_to_utf16 (utf8filename, -1, NULL, NULL, NULL);
      if (wname != NULL)
	{
	  wchar_t wshortname[MAX_PATH + 1];
	  if (GetShortPathNameW (wname, wshortname, G_N_ELEMENTS (wshortname)))
	    {
	      gchar *tem = g_utf16_to_utf8 (wshortname, -1, NULL, NULL, NULL);
	      retval = g_locale_from_utf8 (tem, -1, NULL, NULL, NULL);
	      g_free (tem);
	    }
	  g_free (wname);
	}
    }
  return retval;
}
