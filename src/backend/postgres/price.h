/********************************************************************\
 * price.h -- implements price & commodity handling for pg backend  *
 * Copyright (c) 2000, 2001 Linas Vepstas  <linas@linas.org>        *
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


#ifndef POSTGRES_PRICE_H
#define POSTGRES_PRICE_H

#include "PostgresBackend.h"

void pgendGetAllCommodities (PGBackend *be);
void pgendGetCommodity (PGBackend *be, const char * unique_name);
void pgendStorePriceDB (PGBackend *be, QofBook *book);
void pgendStorePriceDBNoLock (PGBackend *be, QofBook *book);
void pgendGetAllPricesInBook (PGBackend *be, QofBook *);
void pgendPriceFind (QofBackend *bend, gpointer olook);


void pgend_price_begin_edit (QofBackend * bend, GNCPrice *pr);
void pgend_price_commit_edit (QofBackend * bend, GNCPrice *pr);

#endif /* POSTGRES_PRICE_H */
