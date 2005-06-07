/*
 * gnc-hooks.c -- helpers for using Glib hook functions
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include <glib.h>
#include <stdio.h>
#include "gnc-hooks.h"

GHashTable* all_hook_lists = NULL;

static GHookList *
gnc_lookup_hook_list (const gchar *name)
{
  GHookList *hook_list;

  //printf("Enter %s: name %s\n", __FUNCTION__, name);
  if (all_hook_lists == NULL) {
    //printf("Leave %s: no hook lists\n", __FUNCTION__);
    return NULL;
  }

  hook_list = g_hash_table_lookup(all_hook_lists, name);
  //printf("Leave %s: hook list %p\n", __FUNCTION__, hook_list);
  return(hook_list);
}

static GHookList *
gnc_create_hook_list (const gchar *name)
{
  GHookList *hook_list;

  //printf("Enter %s: name %s\n", __FUNCTION__, name);
  if (all_hook_lists == NULL)
    all_hook_lists = g_hash_table_new(g_str_hash, g_str_equal);

  hook_list = g_hash_table_lookup(all_hook_lists, name);
  if (hook_list) {
    //printf("Leave %s: list %s(%p) already exists\n\n", __FUNCTION__, name, hook_list);
    return(hook_list);
  }

  hook_list = g_malloc(sizeof(GHookList));
  g_hook_list_init(hook_list, sizeof(GHook));
  g_hash_table_insert(all_hook_lists, (gchar *)name, hook_list);
  //printf("Leave %s: created list %s(%p)\n", __FUNCTION__, name, hook_list);
  return(hook_list);
}

void
gnc_add_to_c_hook (const gchar *name,
		   GHookFunc callback)
{
  GHookList *hook_list;
  GHook *hook;

  //printf("Enter %s: list %s, function %p\n\n", __FUNCTION__, name, callback);
  hook_list = gnc_create_hook_list(name);
  hook = g_hook_alloc(hook_list);
  hook->func = callback;
  g_hook_append(hook_list, hook);
  //printf("Leave %s:  \n", __FUNCTION__);
}

static gboolean
hook_remove_runner (GHook *hook,
		    gpointer data)
{
  return(hook->func == data);
}


void
gnc_remove_from_c_hook (const gchar *name,
			GHookFunc callback)
{
  GHookList *hook_list;
  GHook *hook;

  //printf("Enter %s: name %s, function %p\n\n", __FUNCTION__, name, callback);
  hook_list = gnc_lookup_hook_list(name);
  if (hook_list == NULL) {
    //printf("Leave %s: Unknown hook list %s\n", __FUNCTION__, name);
    return;
  }

  hook = g_hook_find(hook_list, TRUE, hook_remove_runner, callback);
  if (hook == NULL) {
    //printf("Leave %s: Hook %p not found in %s\n", __FUNCTION__, callback, name);
    return;
  }

  g_hook_unref(hook_list, hook);
  //printf("Leave %s: Removed %p from %s\n", __FUNCTION__ , hook, name);
}


static void
call_hook (GHook *hook, gpointer data)
{
  //printf("Enter %s: hook %p (func %p), data %p\n", __FUNCTION__, hook, hook->func, data);
  ((GHookFunc)hook->func)(data);
  //printf("Leave %s:  \n", __FUNCTION__);
}

void
gnc_run_c_hook (const gchar *name, gpointer data)
{
  GHookList *hook_list;

  //printf("Enter %s: list %s, data %p\n", __FUNCTION__, name, data);
  hook_list = gnc_lookup_hook_list(name);
  if (!hook_list) {
    //printf("Leave %s: No such hook list\n", __FUNCTION__);
    return;
  }
  g_hook_list_marshal(hook_list, TRUE, call_hook, data);
  //printf("Leave %s:  \n", __FUNCTION__);
}
