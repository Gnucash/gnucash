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
#include "gnc-engine-util.h"

#include "kvp_frame.h"
#include "kvp-util.h"
#include "kvp-util-p.h"

/* ================================================================ */
 
void 
gnc_kvp_gemini (KvpFrame *kvp_root, time_t secs, const char *first_name, ...)
{
   va_list ap;
   char buff[80];
   KvpFrame *cwd, *pwd;
   KvpValue *v_ncopies;
   gint64 ncopies = 0;
   Timespec ts;
   const char *name;

   if (!kvp_root) return;
   if (!first_name) return;

   /* cwd == 'current working directory' */
   pwd = kvp_frame_get_frame (kvp_root, "gemini", NULL);
   if (!pwd) return;  /* error: can't ever happen */

   /* Find, increment, store number of copies */
   v_ncopies = kvp_frame_get_slot (pwd, "ncopies");
   if (v_ncopies)
   { 
      ncopies = kvp_value_get_gint64 (v_ncopies);
   }

   ncopies ++;
   kvp_frame_set_gint64 (pwd, "ncopies", ncopies);

   /* OK, now create subdirectory and put the actual data */
   --ncopies;
   sprintf (buff, GNC_SCANF_LLD, (long long int) ncopies);
   cwd = kvp_frame_new();
   kvp_frame_set_slot_nc(pwd, buff, kvp_value_new_frame_nc(cwd));

   /* Record the time */
   ts.tv_sec = secs;
   ts.tv_nsec = 0;
   kvp_frame_set_timespec (cwd, "date", ts);

   /* Loop over the args */
   va_start (ap, first_name);
   name = first_name;

   while (name)
   {
      const GUID *guid;
      guid = va_arg (ap, const GUID *);

      kvp_frame_set_guid (cwd, name, guid);

      name = va_arg (ap, const char *);
   }

   va_end (ap);
}

/* ================================================================ */
/*
 * See header for docs.
 */

static void
kv_pair_helper(gpointer key, gpointer val, gpointer user_data)
{
  GSList **result = (GSList **) user_data;
  GHashTableKVPair *kvp = g_new(GHashTableKVPair, 1);

  kvp->key = key;
  kvp->value = val;
  *result = g_slist_prepend(*result, kvp);
}

GSList *
g_hash_table_key_value_pairs(GHashTable *table)
{
  GSList *result_list = NULL;
  g_hash_table_foreach(table, kv_pair_helper, &result_list);
  return result_list;
}

void
g_hash_table_kv_pair_free_gfunc(gpointer data, gpointer user_data)
{
  GHashTableKVPair *kvp = (GHashTableKVPair *) data;
  g_free(kvp);
}

/*======================== END OF FILE =============================*/
