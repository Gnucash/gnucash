/********************************************************************\
 * kvp_util.h -- misc odd-job kvp utils                             *
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

#include "guid.h"
#include "kvp_frame.h"

/* @file kvp_util.h
 * @breif misc odd-job kvp utils engine-private routines
 * @author Copyright (C) 2001, 2003 Linas Vepstas <linas@linas.org>               *
 * @note PRIVATE FILE 
 * -- these routines are private to the engine. The should not be used 
 *    outside of the engine.
 */

/** The gnc_kvp_array() routine is used to maintain a list of pointers
 *  in a kvp tree.
 *  The thing being pointed at is uniquely identified by its GUID. 
 *  This routine is typically used to create a linked list, and/or
 *  a collection of pointers to objects that are 'related' to each 
 *  other in some way.
 *
 *  The var-args should be pairs of strings (const char *) followed by
 *  the corresponding GUID pointer (const GUID *).  Terminate the varargs
 *  with a NULL as the last string argument.
 *
 *  The actual 'pointer' is stored in an array in a subdirectory
 *  of the directory 'path'.
 *  The size of the array is in /ncopies.  The pointer is stored in 
 *  /<n>/<name> where <n> = ncopies -1,  <name> was passed as an argument.
 *  In addition, the date is logged.  Thus, for example:
 *  gnc_kvp_array (kvp, "foo", secs, "acct_guid", aguid, 
 *                                   "book_guid", bguid, NULL);
 *  will increment /foo/ncopies, and will store aguid in 
 *  /foo/<n>/acct_guid and bguid in /foo/<n>/book_guid, where <n> = ncopies-1
 */

void gnc_kvp_array (KvpFrame *kvp_root, const char *path, time_t secs, 
                     const char *first_name, ...);

/* Equivalent to gnc_kvp_array(kvp_root, "gemini", secs, firstname, ...); */
void gnc_kvp_gemini (KvpFrame *kvp_root, time_t secs, 
                     const char *first_name, ...);

#endif /* XACC_KVP_UTIL_P_H */
