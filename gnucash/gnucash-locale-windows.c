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
#include <locale.h>
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
  const char* environment_value = getenv (envvar);
  if (!environment_value || !*environment_value)
    return;
  char* varval = g_strdup (environment_value);
  char* locale = strtok_r (varval, ".", &saveptr);
  char* dot = strtok_r (NULL, "@", &saveptr);

  if (!dot) //strsep didn't find a .
  {
    g_free (varval);
    return;
  }

  char* rest = strtok_r (NULL, "@", &saveptr);

  if ((strncasecmp (dot, "utf", 3) == 0 || strncasecmp (dot, "ucs", 3) == 0) &&
      dot[3] != '-')
    rectify_utf (envvar, locale, dot, rest);

  else if (strncasecmp (dot, "iso", 3) == 0 && strlen (dot) >= 8 &&
           dot[3] != '-' && dot[8] != '-')
    rectify_iso (envvar, locale, dot, rest);

  g_free (varval);
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

    const char *environment_locale = NULL;
    if (((environment_locale = getenv ("LC_ALL")) != NULL && environment_locale[0] != '\0') ||
        ((environment_locale = getenv ("LC_MESSAGES")) != NULL && environment_locale[0] != '\0') ||
        ((environment_locale = getenv ("LANG")) != NULL && environment_locale[0] != '\0'))
    {
        gchar *dot;
        gchar *locale_name;
        gunichar2 *wlocale;

        locale = g_strdup (environment_locale);
        if (g_strcmp0 (locale, "C") == 0 || g_strcmp0 (locale, "POSIX") == 0)
        {
            setlocale (LC_ALL, locale);
            return locale;
        }

        dot = strchr (locale, '.');
        locale_name = g_strndup (locale, dot ? (gsize)(dot - locale) : strlen (locale));
        g_strdelimit (locale_name, "_", '-');
        wlocale = g_utf8_to_utf16 (locale_name, -1, NULL, NULL, NULL);
        if (wlocale && IsValidLocaleName (wlocale))
        {
            LCID lcid = LocaleNameToLCID (wlocale, LOCALE_ALLOW_NEUTRAL_NAMES);
            SetThreadLocale (lcid);
            setlocale (LC_ALL, locale);
            g_free (wlocale);
            g_free (locale_name);
            return locale;
        }
        g_free (wlocale);
        g_free (locale_name);
        g_free (locale);
    }
    if (GetUserDefaultLocaleName (lpLocaleName, LOCALE_NAME_MAX_LENGTH))
    {
        locale = g_utf16_to_utf8 ((gunichar2*)lpLocaleName, -1, NULL, NULL, NULL);
        if (locale)
        {
            if (strlen (locale) > 2 && locale[2] == '-')
                locale[2] = '_';
            setlocale (LC_ALL, locale);
            return locale;
        }
    }
    return g_strdup ("C");
}

void
set_platform_ctype_to_acp (void)
{
    // The C locale is always aligned with ACP. Use _wsetlocale because
    // libintl's setlocale replacement refuses "C" when the ACP is UTF-8.
    _wsetlocale (LC_CTYPE, L"C");
}
