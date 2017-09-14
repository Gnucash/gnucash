/* strptime.h
 *
 * $Id$
 *
 * Ethereal - Network traffic analyzer
 * By Gerald Combs <gerald@ethereal.com>
 * Copyright 1998 Gerald Combs
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 */

#ifndef __STRPTIME_H__
#define __STRPTIME_H__

/*
 * Version of "strptime()", for the benefit of OSes that don't have it.
 */
extern char *strptime(const char *, const char *, struct tm *);

#if defined(OS_WIN32) || defined(G_OS_WIN32)
extern char *get_win32_locale_string(int lctype);
extern char *translate_win32_picture(const char *);
#endif

#endif
