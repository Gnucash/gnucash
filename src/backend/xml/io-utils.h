/********************************************************************\
 * io-utils.h -- api for gnucash file i/o                           *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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

#ifndef IO_UTILS_H
#define IO_UTILS_H

#include <stdio.h>

#include "io-gncxml-v2.h"
#include "qof.h"

void write_account_tree(FILE *out, Account *root, sixtp_gdv2 *gd);
void write_accounts(FILE *out, QofBook *book, sixtp_gdv2 *gd);
void write_book_parts(FILE *out, QofBook *book);
void write_commodities(FILE *out, QofBook *book, sixtp_gdv2 *gd);

void write_emacs_trailer(FILE *out);


#endif /* IO_UTILS_H */
