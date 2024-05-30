/*
 * gnucash-locale-windows.c -- Windows specific locale handling
 *
 * Copyright (C) 2020 John Ralls <jralls@ceridwen.us>
 * Copyright (C) 2021 Geert Janssens <geert@kobaltwit.be>
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

#include <Windows.h>
#include <fcntl.h>
#include <glib/gi18n.h>
#include "gnucash-locale-platform.h"

static void
rectify_utf(const char* envvar, const char* locale,
            const char* dot, const char* rest)
{
  char *new_locale;
  if (rest && *rest)
    new_locale = g_strdup_printf ("%s.UTF-%s@%s",
                                  locale, dot + 3, rest);
  else
    new_locale = g_strdup_printf ("%s.UTF-%s", locale, dot + 3);

  _putenv_s (envvar, new_locale);
  g_free(new_locale);
}

static void
rectify_iso(const char* envvar, const char* locale,
            const char* dot, const char* rest)
{

  char *eefn = strstr (dot, "8859");

  if (!(eefn && *eefn))
    return;

  char* isonum = (*(eefn + 4) == '-') ? eefn + 5 : eefn + 4;

  if (*isonum == '\0')
    return;

  char *new_locale;
  if (rest && *rest)
    new_locale = g_strdup_printf ("%s.ISO-8859-%s@%s", locale, isonum, rest);
  else
    new_locale = g_strdup_printf ("%s.ISO-8859-%s", locale, isonum);

  _putenv_s (envvar, new_locale);
  g_free (new_locale);
}

static void
rectify_environment_charset(const char* envvar)
{
  if (!(envvar && *envvar))
    return;
  if (strcmp (envvar, "LANG") && strncmp (envvar, "LC_", 3))
    return;
  char* saveptr;
  char* varval = getenv (envvar);
  char* locale = strtok_r (varval, ".", &saveptr);
  char* dot = strtok_r (NULL, "@", &saveptr);

  if (!dot) //strsep didn't find a .
    return;

  char* rest = strtok_r (NULL, "@", &saveptr);

  if ((strncasecmp (dot, "utf", 3) == 0 || strncasecmp (dot, "ucs", 3) == 0) &&
      dot[3] != '-')
    return rectify_utf (envvar, locale, dot, rest);

  if (strncasecmp (dot, "iso", 3) == 0 && strlen (dot) >= 8 &&
      dot[3] != '-' && dot[8] != '-')
    return rectify_iso (envvar, locale, dot, rest);

  //Otherwise do nothing
}

/* If one of the Unix locale variables LC_ALL, LC_MESSAGES, or LANG is
 * set in the environment check to see if it's a valid locale and if
 * it is set both the Windows and POSIX locales to that. If not
 * retrieve the Windows locale and set POSIX to match.
 */
char *
set_platform_locale(void)
{
    WCHAR lpLocaleName[LOCALE_NAME_MAX_LENGTH];
    char *locale = NULL;
    /* Prevent Bug 799320 by ensuring that the localization
       environment variables of interest have well-formed UTF or
       ISO-8859 specifiers for GnuLib's interpretation of well-formed,
       which means having minuses in the right places. This only
       protects agains missing hyphens, it won't help if you specify
       utf42 as a charset when you meant utf32.
     */
    rectify_environment_charset ("LANG");
    rectify_environment_charset ("LC_ALL");
    rectify_environment_charset ("LC_MESSAGES");
    rectify_environment_charset ("LC_CTYPE");

    if (((locale = getenv ("LC_ALL")) != NULL && locale[0] != '\0') ||
      ((locale = getenv ("LC_MESSAGES")) != NULL && locale[0] != '\0') ||
      ((locale = getenv ("LANG")) != NULL && locale[0] != '\0'))
    {
        gunichar2* wlocale = NULL;
        int len = 0;
        len = strchr(locale, '.') - locale;
        locale[2] = '-';
        wlocale = g_utf8_to_utf16 (locale, len, NULL, NULL, NULL);
        if (IsValidLocaleName(wlocale))
        {
            LCID lcid = LocaleNameToLCID(wlocale, LOCALE_ALLOW_NEUTRAL_NAMES);
            SetThreadLocale(lcid);
            locale[2] = '_';
            setlocale (LC_ALL, locale);
            g_free(wlocale);
            return g_strdup (locale);
        }
        g_free(locale);
        g_free(wlocale);
    }
    if (GetUserDefaultLocaleName(lpLocaleName, LOCALE_NAME_MAX_LENGTH))
    {
        locale = g_utf16_to_utf8((gunichar2*)lpLocaleName,
                                 LOCALE_NAME_MAX_LENGTH,
                                 NULL, NULL, NULL);
        (locale)[2] = '_';
        setlocale (LC_ALL, locale);
        return locale;
    }
    return g_strdup("C");
}
