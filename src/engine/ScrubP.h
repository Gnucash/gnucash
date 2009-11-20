/********************************************************************\
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

/** @file ScrubP.h
 *
 * This is the *private* header for the scrub routines.
 * No one outside of the engine should ever include this file.
 *
 * Copyright (C) 2003, Linas Vepstas <linas@linas.org> 
 */

#ifndef XACC_SCRUB_P_H
#define XACC_SCRUB_P_H

#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"

/* Utility to make account by name.  Not for public use. */
Account * xaccScrubUtilityGetOrMakeAccount (Account *root, 
       gnc_commodity * currency, const char *accname,
       GNCAccountType acctype, gboolean placeholder);


#endif /* XACC_SCRUB_P_H */
