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
#include "qofinstance-p.h"

static short module = MOD_ENGINE;

/* ========================================================== */

void 
qof_instance_init (QofInstance *inst, QofIdType type, QofBook *book)
{
	QofCollection *col;

	inst->book = book;
	inst->kvp_data = kvp_frame_new();
	inst->last_update.tv_sec = 0;
	inst->last_update.tv_nsec = -1;
	inst->editlevel = 0;
	inst->do_free = FALSE;
	inst->dirty = FALSE;

	col = qof_book_get_collection (book, type);
	qof_entity_init (&inst->entity, type, col);
}

void 
qof_instance_release (QofInstance *inst)
{
	kvp_frame_delete (inst->kvp_data);
	inst->editlevel = 0;
	inst->do_free = FALSE;
	inst->dirty = FALSE;
	qof_entity_release (&inst->entity);
}

const GUID *
qof_instance_get_guid (QofInstance *inst)
{
	if (!inst) return NULL;
	return &inst->entity.guid;
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

Timespec
qof_instance_get_last_update (QofInstance *inst)
{
	if (!inst) 
	{
		Timespec ts = {0,-1}; 
		return ts;
	}
	return inst->last_update;
}

int 
qof_instance_version_cmp (QofInstance *left, QofInstance *right)
{
	if (!left && !right) return 0;
	if (!left) return -1;
	if (!right) return +1;
	if (left->last_update.tv_sec < right->last_update.tv_sec) return -1;
	if (left->last_update.tv_sec > right->last_update.tv_sec) return +1;
	if (left->last_update.tv_nsec < right->last_update.tv_nsec) return -1;
	if (left->last_update.tv_nsec > right->last_update.tv_nsec) return +1;
	return 0;
}

gboolean
qof_instance_is_dirty (QofInstance *inst)
{
	if (!inst) return FALSE;
	return inst->dirty;
}

/* ========================================================== */
/* setters */

void
qof_instance_mark_clean (QofInstance *inst)
{
  if(!inst) return;
  inst->dirty = FALSE;
}

void 
qof_instance_set_slots (QofInstance *inst, KvpFrame *frm)
{
  if (!inst) return;
  if (inst->kvp_data && (inst->kvp_data != frm))
  {
    kvp_frame_delete(inst->kvp_data);
  }

  inst->dirty = TRUE;
  inst->kvp_data = frm;
}

void
qof_instance_set_last_update (QofInstance *inst, Timespec ts)
{
  if (!inst) return;
  inst->last_update = ts;
}

/* ========================================================== */

void
qof_instance_gemini (QofInstance *to, QofInstance *from)
{
  time_t now;

  /* Books must differ for a gemini to be meaningful */
  if (!from || !to || (from->book == to->book)) return;

  now = time(0);

  /* Make a note of where the copy came from */
  gnc_kvp_bag_add (to->kvp_data, "gemini", now,
                                  "inst_guid", &from->entity.guid,
                                  "book_guid", &from->book->inst.entity.guid,
                                  NULL);
  gnc_kvp_bag_add (from->kvp_data, "gemini", now,
                                  "inst_guid", &to->entity.guid,
                                  "book_guid", &to->book->inst.entity.guid,
                                  NULL);

  to->dirty = TRUE;
}

QofInstance *
qof_instance_lookup_twin (QofInstance *src, QofBook *target_book)
{
	QofCollection *col;
	KvpFrame *fr;
	GUID * twin_guid;
	QofInstance * twin;

	if (!src || !target_book) return NULL;
	ENTER (" ");

	fr = gnc_kvp_bag_find_by_guid (src->kvp_data, "gemini",
	                             "book_guid", &target_book->inst.entity.guid);

	twin_guid = kvp_frame_get_guid (fr, "inst_guid");

	col = qof_book_get_collection (target_book, src->entity.e_type);
	twin = (QofInstance *) qof_collection_lookup_entity (col, twin_guid);

	LEAVE (" found twin=%p", twin);
	return twin;
}

/* ========================== END OF FILE ======================= */
