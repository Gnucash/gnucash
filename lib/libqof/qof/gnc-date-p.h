/* 
 * gnc-date-p.h
 *
 * Copyright (C) 2007 Andreas Koehler <andi5.py@gmx.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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

#ifndef __GNC_DATE_P_H__
#define __GNC_DATE_P_H__

#include "gnc-date.h"

/** Convert a given date/time format from UTF-8 to an encoding suitable for the
 *  strftime system call.
 *
 *  @param utf8_format Date/time format specification in UTF-8.
 *
 *  @return A newly allocated string on success, or NULL otherwise.
 */
gchar *qof_time_format_from_utf8(const gchar *utf8_format);

/** Convert a result of a call to strftime back to UTF-8.
 *
 *  @param locale_string The result of a call to strftime.
 *
 *  @return A newly allocated string on success, or NULL otherwise.
 */
gchar *qof_formatted_time_to_utf8(const gchar *locale_string);

#endif /* __GNC_DATE_P_H__ */
