/********************************************************************\
 * qofinstance.c -- handler for fields common to all objects        *
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

/*
 * Object instance holds many common fields that most
 * gnucash objects use.
 * 
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 */

#include "gnc-trace.h"

#include "kvp-util-p.h"
#include "qofbook.h"
#include "qofbook-p.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofinstance.h"

static short module = MOD_ENGINE;

/* ========================================================== */

void 
qof_instance_init (QofInstance *inst, QofBook *book)
{
   inst->book = book;
	inst->kvp_data = kvp_frame_new();
	inst->editlevel = 0;
	inst->do_free = FALSE;
	inst->dirty = FALSE;

   qof_entity_guid_new (qof_book_get_entity_table (book),&inst->guid);
}

void 
qof_instance_release (QofInstance *inst)
{
   kvp_frame_delete (inst->kvp_data);
   qof_entity_remove (inst->book->entity_table, &inst->guid);
	inst->editlevel = 0;
	inst->do_free = FALSE;
   inst->dirty = FALSE;
}

const GUID *
qof_instance_get_guid (QofInstance *inst)
{
   if (!inst) return NULL;
   return &inst->guid;
}

QofBook * 
qof_instance_get_book (QofInstance *inst)
{
   if (!inst) return NULL;
   return inst->book;
}

KvpFrame* 
qof_instance_get_slots (QofInstance *inst)
{
  if (!inst) return NULL;
  return inst->kvp_data;
}

gboolean
qof_instance_is_dirty (QofInstance *inst)
{
  if (!inst) return FALSE;
  return inst->dirty;
}

/* ========================================================== */

void
qof_instance_gemini (QofInstance *to, QofInstance *from)
{
  time_t now;
  now = time(0);

  /* Make a note of where the copy came from */
  gnc_kvp_bag_add (to->kvp_data, "gemini", now,
                                  "inst_guid", &from->guid,
                                  "book_guid", &from->book->guid,
                                  NULL);
  gnc_kvp_bag_add (from->kvp_data, "gemini", now,
                                  "inst_guid", &to->guid,
                                  "book_guid", &to->book->guid,
                                  NULL);

  to->dirty = TRUE;
}

QofInstance *
qof_instance_lookup_twin (QofInstance *src, QofBook *target_book)
{
   QofIdType etype;
   KvpFrame *fr;
   GUID * twin_guid;
   QofInstance * twin;
                                                                                
   if (!src || !target_book) return NULL;
   ENTER (" ");

   fr = gnc_kvp_bag_find_by_guid (src->kvp_data, "gemini",
                    "book_guid", &src->guid);
                                                                                
   twin_guid = kvp_frame_get_guid (fr, "inst_guid");

   etype = qof_entity_type (src->book->entity_table, &src->guid);

   twin = qof_entity_lookup (target_book->entity_table, twin_guid, etype);

   LEAVE (" found twin=%p", twin);
   return twin;
}

/* ========================== END OF FILE ======================= */
