/*
 * qofinstance.c
 *
 * Object instance holds many common fields that most
 * gnucash objects use.
 * 
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 */

#include "qofbook.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofinstance.h"

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

