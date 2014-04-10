/********************************************************************\
 * gnc-euro.h -- utilities for EURO currency                        *
 *                                                                  *
 * Copyright (C) 2000 Herbert Thoma                                 *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#ifndef GNC_EURO_H
#define GNC_EURO_H

#include <glib.h>

#include "gnc-commodity.h"
#include "qof.h"

gboolean    gnc_is_euro_currency (const gnc_commodity * currency);
gboolean    gnc_is_euro_currency_code (const char *code);
gnc_numeric gnc_convert_to_euro (const gnc_commodity * currency,
                                 gnc_numeric value);
gnc_numeric gnc_convert_from_euro (const gnc_commodity * currency,
                                   gnc_numeric value);
gnc_numeric gnc_euro_currency_get_rate (const gnc_commodity *currency);

gnc_commodity * gnc_get_euro (void);

#endif  /* EURO_UTILS_H */
