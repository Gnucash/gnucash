/********************************************************************\
 * Commodity-matcher.h -- Commodity selection and matching functions* 
 * for import modules.                                              *
 *                                                                  *
 *                        (GnuCash)                                 *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/
/**@file
 * \brief Generic Commodity matching functionnality
 */
#ifndef COMMODITY_MATCHER_H
#define COMMODITY_MATCHER_H

#include "gnc-commodity.h"

/* The gnc_import_select_commodity():

  Must be called with a string containing a unique identifier for the
  commodity.  If an commodity with a matching exchange_code is
  found, the function immediately returns with a pointer to that
  commodity.  Otherwise, the user may be prompted to select a GnuCash
  account or create a new one (in both cases, the exchange_code is written
  written to the commodity's exchange_code field, overwriting anything that
  was there before.

  Params:

    char * exchange_code: The string containing the code for which you
    want a matching commodity.  A CUISP code or similar UNIQUE code.
    The stock ticker is NOT appropriate, unless you have no other option.

    char auto_create: If 0, if the exchange_code value in unknown,
    the function returns NULL, otherwise, the user will be asked to 
    create a new account.

    char * default_fullname: A human-readable description of the commodity, such
    as the stock name.  Can be NULL. If it is not NULL, it will be shown
    to the user when selecting a commodity.  It will also be used as
    the default if a new commodity is created.

     char * default_mnemonic:  Usually the stock ticker or similar. Can be NULL.
     If it is not NULL, it will be shown
    to the user when selecting a commodity.  It will also be used as
    the default if a new commodity is created.


  Return: A pointer to the found or created commodity, or NULL if no
  account was found or created.

*/
gnc_commodity * gnc_import_select_commodity(char * exchange_code,
				    char auto_create,
				    char * default_fullname,
				    char * default_mnemonic);

#endif
