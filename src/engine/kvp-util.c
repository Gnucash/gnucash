/********************************************************************\
 * kvp_util.c -- misc odd-job kvp utils                             *
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

#include "config.h"

#include <glib.h>
#include <stdio.h>

#include "kvp_frame.h"
#include "kvp-util-p.h"

void gnc_kvp_gemini (kvp_frame *, const GUID *, const GUID *, time_t);

/* ================================================================ */
/* mark the guid and date of the copy, using kvp.  The info will be
 * places in /gemini/ncopies, /gemini/<n>/acct_guid, /gemini/<n>/book_guid,
 * /gemini/<n>/date, where <n> = ncopies-1.
 */

void 
gnc_kvp_gemini (kvp_frame *kvp_root, const GUID *acct_guid, 
        const GUID *book_guid, time_t secs)
{
   char buff[80];
   kvp_frame *cwd, *pwd;
   kvp_value *v_ncopies, *vvv;
   gint64 ncopies = 0;
   Timespec ts;

   if (!kvp_root) return;

   /* cwd == 'current working directory' */
   pwd = kvp_frame_get_frame (kvp_root, "gemini");
   if (!pwd) return;  /* error: can't ever happen */

   /* find, increment, store number of copies */
   v_ncopies = kvp_frame_get_slot (pwd, "ncopies");
   if (v_ncopies)
   { 
      ncopies = kvp_value_get_gint64 (v_ncopies);
   }

   ncopies ++;
   v_ncopies = kvp_value_new_gint64 (ncopies);
   kvp_frame_set_slot_nc (pwd, "ncopies", v_ncopies);

   /* OK, now create subdirectory and put the actual data */
   --ncopies;
   sprintf (buff, "%lld", ncopies);
   cwd = kvp_frame_new();
   kvp_frame_set_slot_nc(pwd, buff, kvp_value_new_frame(cwd));

   if (acct_guid)
   {
      vvv = kvp_value_new_guid (acct_guid);
      kvp_frame_set_slot_nc (cwd, "acct_guid", vvv);
   }

   if (book_guid)
   {
      vvv = kvp_value_new_guid (book_guid);
      kvp_frame_set_slot_nc (cwd, "book_guid", vvv);
   }

   ts.tv_sec = secs;
   ts.tv_nsec = 0;
   vvv = kvp_value_new_timespec (ts);
   kvp_frame_set_slot_nc (cwd, "date", vvv);
}

/* ================================================================ */

