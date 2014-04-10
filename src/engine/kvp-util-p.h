/********************************************************************\
 * kvp_util.h -- misc odd-job kvp utils                             *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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
 *                                                                  *
\********************************************************************/

#ifndef XACC_KVP_UTIL_P_H
#define XACC_KVP_UTIL_P_H

#include "config.h"

#include "GNCId.h"
#include "kvp_frame.h"

/* PRIVATE FILE 
 * -- these routines are private to the engine. The should not be used 
 *    outside of the engine.
 */

/* 
 * The gnc_kvp_gemini() routine:
 * mark the guid and date of the copy, using kvp.  The info will be
 * placed in /gemini/ncopies, /gemini/<n>/acct_guid, /gemini/<n>/book_guid,
 * /gemini/<n>/date, where <n> = ncopies-1.
 */

void gnc_kvp_gemini (kvp_frame *, const GUID *, const GUID *, time_t);

#endif /* XACC_KVP_UTIL_P_H */
