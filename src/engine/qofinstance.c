/*
 * qofinstance.c
 *
 * Object instance holds many common fields that most
 * gnucash objects use.
 * 
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 */

#include "qofinstance.h"

void 
qof_instance_init (QofInstance *inst)
{
	inst->kvp_data = kvp_frame_new();
}

void 
qof_instance_release (QofInstance *inst)
{
   kvp_frame_delete (inst->kvp_data);
}

KvpFrame* 
qof_instance_get_slots (QofInstance *inst)
{
  if (!inst) return NULL;
  return inst->kvp_data;
}

