/********************************************************************\
 * gnc-locale-utils.h -- Locale manipulation functions		    *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
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
#ifndef GNC_LOCALE_UTILS_H
#define GNC_LOCALE_UTILS_H

#include <glib.h>
#include <locale.h>

/* The gnc_localeconv() subroutine returns an lconv structure
 * containing locale information. If no locale is set, the structure
 * is given default (en_US) values.  */
struct lconv * gnc_localeconv (void);


/* Returns the default ISO currency string of the current locale. */
const char * gnc_locale_default_iso_currency_code (void);

/* Returns the number of decimal place to print in the current locale */
int gnc_locale_decimal_places (void);

/** Temporarily change locale, pushing the old one onto a stack
 * Currently, this has no effect on gnc_localeconv.  i.e., after the
 * first call to gnc_localeconv, subsequent calls will return the same
 * information.
 *
 * WARNING: Be careful to maintain the correct nesting order of pushes
 * or pops; otherwise, the localization results might be
 * interesting. Note that the stack does not keep track of which
 * category a locale was pushed from, so careless use will alse
 * produce interesting results.
 *
 * @param category: The locale category (e.g. LC_ALL, LC_NUMERIC) to push onto
 * @param locale: The new locale to set
 */
void gnc_push_locale (int category, const char *locale);

/** Restore the last-pushed locale.
 * @param category: The locale category to restore the locale to.
 */
void gnc_pop_locale (int category);


#endif /* GNC_LOCALE_UTILS_H */
