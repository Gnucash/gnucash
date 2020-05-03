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

#endif /* GNC_LOCALE_UTILS_H */
