/*
 * qofinstance.c
 *
 * Object instance holds many common fields that most
 * gnucash objects use.
 * 
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 */

#include "kvp-util-p.h"
#include "qofbook.h"
#include "qofbook-p.h"
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

