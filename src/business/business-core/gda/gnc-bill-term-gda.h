/*
 * gnc-bill-term-gda.h -- billing term gda backend
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

/** @file gnc-bill-term-gda.h
 *  @brief load and save accounts data to SQL via libgda
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database via libgda
 */

#ifndef GNC_BILLTERM_GDA_H
#define GNC_BILLTERM_GDA_H

#include "gncBillTerm.h"

#define CT_BILLTERMREF "billterm"

void gnc_billterm_gda_initialize( void );
void gnc_gda_save_billterm( QofInstance* inst, GncGdaBackend* be );

#endif /* GNC_BILLTERM_GDA_H */
