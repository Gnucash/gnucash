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

static GHashTable* gnc_hooks_list = NULL;
static gboolean gnc_hooks_initialized = FALSE;

typedef struct {
  gchar		*desc;
  GHookList	*c_danglers;
  GHookList	*scm_danglers;
} GncHook;

const gchar *
gnc_hook_create (const gchar *name, const gchar *desc)
{
  GncHook *hook_list;

  //printf("Enter %s: name %s\n", __FUNCTION__, name);
  if (gnc_hooks_list == NULL) {
    gnc_hooks_list = g_hash_table_new(g_str_hash, g_str_equal);

    /* If we're not initialized then initialize now */
    if (!gnc_hooks_initialized)
      gnc_hooks_init();
  }

  hook_list = g_hash_table_lookup(gnc_hooks_list, name);
  if (hook_list) {
    //printf("Leave %s: list %s(%p) already exists\n\n", __FUNCTION__, name, hook_list);
    return(name);
  }

  hook_list = g_new0(GncHook, 1);
  hook_list->desc = g_strdup(desc);
  hook_list->c_danglers = g_malloc(sizeof(GHookList));
  g_hook_list_init(hook_list->c_danglers, sizeof(GHook));
  hook_list->scm_danglers = g_malloc(sizeof(GHookList));
  g_hook_list_init(hook_list->scm_danglers, sizeof(GHook));
  g_hash_table_insert(gnc_hooks_list, (gchar *)name, hook_list);
  //printf("Leave %s: created list %s(%p)\n", __FUNCTION__, name, hook_list);

  return name;
}

static GncHook *
gnc_hook_lookup (const gchar *name)
{
  GncHook *hook;

  //printf("Enter %s: name %s\n", __FUNCTION__, name);
  if (gnc_hooks_list == NULL) {
    //printf("Leave %s: no hook lists\n", __FUNCTION__);
    gnc_hooks_init();
  }

  hook = g_hash_table_lookup(gnc_hooks_list, name);
  //printf("Leave %s: hook list %p\n", __FUNCTION__, hook_list);
  return(hook);
}

const gchar *
gnc_hook_get_description(const gchar *name)
{
  GncHook *hook;

  hook = gnc_hook_lookup(name);
  if (!hook) return "";
  return hook->desc;
}

void
gnc_hook_add_dangler (const gchar *name, GFunc callback, gpointer cb_arg)
{
  GncHook *gnc_hook;
  GHook *hook;

  //printf("Enter %s: list %s, function %p\n\n", __FUNCTION__, name, callback);
  gnc_hook = gnc_hook_lookup(name);
  g_return_if_fail(gnc_hook != NULL);
  hook = g_hook_alloc(gnc_hook->c_danglers);
  hook->func = callback;
  hook->data = cb_arg;
  hook->destroy = NULL;
  g_hook_append(gnc_hook->c_danglers, hook);
  //printf("Leave %s:  \n", __FUNCTION__);
}

static gboolean
hook_remove_runner (GHook *hook, gpointer data)
{
  return(hook->func == data);
}

void
gnc_hook_remove_dangler (const gchar *name, GFunc callback)
{
  GncHook *gnc_hook;
  GHook *hook;

  //printf("Enter %s: name %s, function %p\n\n", __FUNCTION__, name, callback);
  gnc_hook = gnc_hook_lookup(name);
  if (gnc_hook == NULL) {
    //printf("Leave %s: Unknown hook list %s\n", __FUNCTION__, name);
    return;
  }

  hook = g_hook_find(gnc_hook->c_danglers, TRUE, hook_remove_runner, callback);
  if (hook == NULL) {
    //printf("Leave %s: Hook %p not found in %s\n", __FUNCTION__, callback, name);
    return;
  }

  g_hook_unref(gnc_hook->c_danglers, hook);
  //printf("Leave %s: Removed %p from %s\n", __FUNCTION__ , hook, name);
}


static void
call_c_hook (GHook *hook, gpointer data)
{
  //printf("Enter %s: hook %p (func %p), data %p\n", __FUNCTION__, hook, hook->func, data);
  ((GFunc)hook->func)(data, hook->data);
  //printf("Leave %s:  \n", __FUNCTION__);
}

static void
call_scm_hook (GHook *hook, gpointer data)
{
  // XXX.  Implement me.
}

void
gnc_hook_run (const gchar *name, gpointer data)
{
  GncHook *hook;

  //printf("Enter %s: list %s, data %p\n", __FUNCTION__, name, data);
  hook = gnc_hook_lookup(name);
  if (!hook) {
    //printf("Leave %s: No such hook list\n", __FUNCTION__);
    return;
  }
  g_hook_list_marshal(hook->c_danglers, TRUE, call_c_hook, data);
  g_hook_list_marshal(hook->scm_danglers, TRUE, call_scm_hook, data);
  //printf("Leave %s:  \n", __FUNCTION__);
}

void
gnc_hooks_init(void)
{
  if (gnc_hooks_initialized)
    return;

  gnc_hooks_initialized = TRUE;

  gnc_hook_create(HOOK_STARTUP,
		  "Functions to run at startup.  Hook args: ()");
  gnc_hook_create(HOOK_SHUTDOWN,
		  "Functions to run at guile shutdown.  Hook args: ()");
  gnc_hook_create(HOOK_UI_STARTUP,
		  "Functions to run when the ui comes up.  Hook args: ()");
  gnc_hook_create(HOOK_UI_POST_STARTUP,
		  "Functions to run after the ui comes up.  Hook args: ()");
  gnc_hook_create(HOOK_UI_SHUTDOWN,
		  "Functions to run at ui shutdown.  Hook args: ()");
  gnc_hook_create(HOOK_NEW_BOOK,
		  "Run after a new (empty) book is opened, before the"
		  " book-opened-hook. Hook args: ()");
  gnc_hook_create(HOOK_REPORT,
		  "Run just before the reports are pushed into the menus."
		  "  Hook args: ()");

  gnc_hook_create(HOOK_BOOK_OPENED,
		  "Run after book open.  Hook args: <gnc:Session*>.");
  gnc_hook_create(HOOK_BOOK_CLOSED,
		  "Run before file close.  Hook args: <gnc:Session*>");
}
