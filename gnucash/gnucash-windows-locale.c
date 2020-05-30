/*
 * gnucash-core-app.cpp -- Basic application object for gnucash binaries
 *
 * Copyright (C) 2020 John Ralls <jralls@ceridwen.us>
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

//sacrificial prototype
void set_win32_thread_locale(char **sys_locale);

/* If one of the Unix locale variables LC_ALL, LC_MESSAGES, or LANG is
 * set in the environment check to see if it's a valid locale and if
 * it is set both the Windows and POSIX locales to that. If not
 * retrieve the Windows locale and set POSIX to match.
 */
void
set_win32_thread_locale(char **sys_locale)
{
    WCHAR lpLocaleName[LOCALE_NAME_MAX_LENGTH];
    char *locale = NULL;

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
	    *sys_locale = g_strdup (locale);
	    g_free(wlocale);
	    return;
	}
	g_free(locale);
	g_free(wlocale);
    }
    if (GetUserDefaultLocaleName(lpLocaleName, LOCALE_NAME_MAX_LENGTH))
    {
	*sys_locale = g_utf16_to_utf8((gunichar2*)lpLocaleName,
				     LOCALE_NAME_MAX_LENGTH,
				     NULL, NULL, NULL);
	(*sys_locale)[2] = '_';
	setlocale (LC_ALL, *sys_locale);
	return;
    }
}
