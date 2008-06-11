/********************************************************************\
 * gnc-glib-utils.c -- utility functions based on glib functions    *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
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

#include "config.h"
#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>

#include "gnc-glib-utils.h"

#ifdef G_OS_WIN32
#include <windows.h>
#endif

int
safe_utf8_collate (const char * da, const char * db)
{
  if (da && !(*da))
    da = NULL;
  if (db && !(*db))
    db = NULL;

  if (da && db)
    return g_utf8_collate(da, db);
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

  const gchar *p;

  g_return_val_if_fail (str != NULL, FALSE);
  
  if (end)
    *end = str;
  
  p = str;
  
  while ((max_len < 0 || (p - str) < max_len) && *p)
    {
      int i, mask = 0, len;
      gunichar result;
      unsigned char c = (unsigned char) *p;
      
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

      if (result == (gunichar)-1)
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
    return FALSE;
  else if (max_len < 0 &&
           *p != '\0')
    return FALSE;
  else
    return TRUE;
}

void
gnc_utf8_strip_invalid (gchar *str)
{
  gchar *end;
  gint len;

  if (gnc_utf8_validate(str, -1, (const gchar **)&end))
    return;

  g_warning("Invalid utf8 string: %s", str);
  do {
    len = strlen(end);
    memmove(end, end+1, len);	/* shuffle the remainder one byte */
  } while (!gnc_utf8_validate(str, -1, (const gchar **)&end));
}

gchar *
gnc_utf8_strip_invalid_strdup(const gchar* str)
{
  gchar *result = g_strdup (str);
  gnc_utf8_strip_invalid (result);
  return result;
}

gchar *
gnc_locale_from_utf8(const gchar* str)
{
  gchar *   locale_str;
  gsize     bytes_written = 0;
  GError *  err = NULL;

  /* Convert from UTF-8 to the encoding used in the current locale. */
  locale_str = g_locale_from_utf8(str, -1, NULL, &bytes_written, &err);
  if (err) {
    g_warning("g_locale_from_utf8 failed: %s", err->message);
    g_error_free(err);
  }

  return locale_str;
}

gchar *
gnc_locale_to_utf8(const gchar* str)
{
  gchar *   utf8_str;
  gsize     bytes_written = 0;
  GError *  err = NULL;

  /* Convert to UTF-8 from the encoding used in the current locale. */
  utf8_str = g_locale_to_utf8(str, -1, NULL, &bytes_written, &err);
  if (err) {
    g_warning("g_locale_to_utf8 failed: %s", err->message);
    g_error_free(err);
  }

  return utf8_str;
}

GList*
gnc_g_list_map(GList* list, GncGMapFunc fn, gpointer user_data)
{
  GList *rtn = NULL;
  for (; list != NULL; list = list->next)
  {
    rtn = g_list_append(rtn, (*fn)(list->data, user_data));
  }
  return rtn;
}

void
gnc_g_list_cut(GList **list, GList *cut_point)
{
  if (list == NULL || *list == NULL)
    return;

  // if it's the first element.
  if (cut_point->prev == NULL)
  {
    *list = NULL;
    return;
  }

  cut_point->prev->next = NULL;
  cut_point->prev = NULL;
}

void
gnc_scm_log_warn(const gchar *msg)
{
    g_log("gnc.scm", G_LOG_LEVEL_WARNING, "%s", msg);
}

void
gnc_scm_log_error(const gchar *msg)
{
    g_log("gnc.scm", G_LOG_LEVEL_CRITICAL, "%s", msg);
}

void
gnc_scm_log_msg(const gchar *msg)
{
    g_log("gnc.scm", G_LOG_LEVEL_MESSAGE, "%s", msg);
}

void
gnc_scm_log_debug(const gchar *msg)
{
    g_log("gnc.scm", G_LOG_LEVEL_DEBUG, "%s", msg);
}

void gnc_gpid_kill(GPid pid)
{
#ifdef G_OS_WIN32
    if (!TerminateProcess((HANDLE) pid, 0)) {
        gchar *msg = g_win32_error_message(GetLastError());
        g_warning("Could not kill child process: %s", msg ? msg : "(null)");
        g_free(msg);
    }
#else /* !G_OS_WIN32 */
    if (kill(pid, SIGKILL)) {
        g_warning("Could not kill child process: %s", g_strerror(errno));
    }
#endif /* G_OS_WIN32 */
}
