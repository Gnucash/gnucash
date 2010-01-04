/*
 * gnc-hooks.c -- helpers for using Glib hook functions
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
 *                    Derek Atkins <derek@ihtfp.com>
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <glib.h>
#include <stdio.h>
#include <libguile.h>
#include <guile-mappings.h>
#include "swig-runtime.h"
#include "gnc-hooks.h"
#include "gnc-hooks-scm.h"
#include "gnc-engine.h"

static QofLogModule log_module = GNC_MOD_ENGINE; 

static GHashTable* gnc_hooks_list = NULL;
static gboolean gnc_hooks_initialized = FALSE;

typedef struct {
  gchar		*desc;
  GHookList	*c_danglers;
  GHookList	*scm_danglers;
  gint           num_args;
} GncHook;

typedef struct {
  SCM		proc;
} GncScmDangler;

gchar *
gnc_hook_create (const gchar *name, gint num_args, const gchar *desc)
{
  GncHook *hook_list;

  g_return_val_if_fail(name != NULL, NULL);
  g_return_val_if_fail(num_args <= 1, NULL);
  g_return_val_if_fail(desc != NULL, NULL);

  ENTER("name %s", name);
  if (gnc_hooks_list == NULL) {
    gnc_hooks_list = g_hash_table_new(g_str_hash, g_str_equal);

    /* If we're not initialized then initialize now */
    if (!gnc_hooks_initialized)
      gnc_hooks_init();
  }

  hook_list = g_hash_table_lookup(gnc_hooks_list, name);
  if (hook_list) {
    LEAVE("List %s(%p) already exists", name, hook_list);
    return((gchar*)name);
  }

  hook_list = g_new0(GncHook, 1);
  hook_list->desc = g_strdup(desc);
  hook_list->c_danglers = g_malloc(sizeof(GHookList));
  g_hook_list_init(hook_list->c_danglers, sizeof(GHook));
  hook_list->scm_danglers = g_malloc(sizeof(GHookList));
  hook_list->num_args = num_args;
  g_hook_list_init(hook_list->scm_danglers, sizeof(GHook));
  g_hash_table_insert(gnc_hooks_list, (gchar *)name, hook_list);

  LEAVE("created list %s(%p)", name, hook_list);
  return (gchar *)name;
}

static GncHook *
gnc_hook_lookup (const gchar *name)
{
  GncHook *hook;

  ENTER("name %s", name);
  if (gnc_hooks_list == NULL) {
    PINFO("no hook lists");
    gnc_hooks_init();
  }

  hook = g_hash_table_lookup(gnc_hooks_list, name);
  LEAVE("hook list %p", hook);
  return(hook);
}

gchar *
gnc_hook_get_description(const gchar *name)
{
  GncHook *hook;
  ENTER("name %s", name);

  hook = gnc_hook_lookup(name);
  if (!hook) {
    LEAVE("No hook found");
    return "";
  }

  LEAVE("desc: %s", hook->desc);
  return (gchar*) hook->desc;
}

void
gnc_hook_add_dangler (const gchar *name, GFunc callback, gpointer cb_arg)
{
  GncHook *gnc_hook;
  GHook *hook;

  ENTER("list %s, function %p, cbarg %p", name, callback, cb_arg);
  gnc_hook = gnc_hook_lookup(name);
  g_return_if_fail(gnc_hook != NULL);
  hook = g_hook_alloc(gnc_hook->c_danglers);
  hook->func = callback;
  hook->data = cb_arg;
  hook->destroy = NULL;
  g_hook_append(gnc_hook->c_danglers, hook);
  LEAVE("");
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

  ENTER("name %s, function %p", name, callback);
  gnc_hook = gnc_hook_lookup(name);
  if (gnc_hook == NULL) {
    LEAVE("Unknown hook list %s", name);
    return;
  }

  hook = g_hook_find(gnc_hook->c_danglers, TRUE, hook_remove_runner, callback);
  if (hook == NULL) {
    LEAVE("Hook %p not found in %s", callback, name);
    return;
  }

  g_hook_destroy_link(gnc_hook->c_danglers, hook);
  LEAVE("Removed %p from %s", hook, name);
}

static void
delete_scm_hook (gpointer data)
{
  GncScmDangler *scm = data;
  scm_gc_unprotect_object(scm->proc);
  g_free(scm);
}

static void
call_scm_hook (GHook *hook, gpointer data)
{
  GncScmDangler *scm = hook->data;

  ENTER("hook %p, data %p, cbarg %p", hook, data, hook->data);

  scm_call_0 (scm->proc);

  LEAVE("");
}

static void
call_scm_hook_1 (GHook *hook, gpointer data)
{
  GncScmDangler *scm = hook->data;

  ENTER("hook %p, data %p, cbarg %p", hook, data, hook->data);

  // XXX: FIXME: We really should make sure this is a session!!! */
  scm_call_1 (scm->proc,
              SWIG_NewPointerObj(data, SWIG_TypeQuery("_p_QofSession"), 0));

  LEAVE("");
}

void
gnc_hook_add_scm_dangler (const gchar *name, SCM proc)
{
  GncHook *gnc_hook;
  GHook *hook;
  GncScmDangler *scm;

  ENTER("list %s, proc ???", name);
  gnc_hook = gnc_hook_lookup(name);
  g_return_if_fail(gnc_hook != NULL);
  scm = g_new0(GncScmDangler, 1);
  scm_gc_protect_object(proc);
  scm->proc = proc;
  hook = g_hook_alloc(gnc_hook->scm_danglers);
  hook->func = call_scm_hook;
  hook->data = scm;
  hook->destroy = delete_scm_hook;
  g_hook_append(gnc_hook->scm_danglers, hook);
  LEAVE("");
}

static gboolean
hook_remove_scm_runner (GHook *hook, gpointer data)
{
  GncScmDangler *scm1 = data;
  GncScmDangler *scm2 = hook->data;
  SCM res;

  res = scm_equal_p(scm1->proc, scm2->proc);
  return(scm_is_true(res));
}

void
gnc_hook_del_scm_dangler (const gchar *name, SCM proc)
{
  GncHook *gnc_hook;
  GHook *hook;
  GncScmDangler scm;

  ENTER("name %s, proc ???", name);

  scm.proc = proc;
  gnc_hook = gnc_hook_lookup(name);
  if (gnc_hook == NULL) {
    LEAVE("Unknown hook list %s", name);
    return;
  }

  hook = g_hook_find(gnc_hook->scm_danglers, TRUE, hook_remove_scm_runner, &scm);
  if (hook == NULL) {
    LEAVE("Hook dangler not found");
    return;
  }

  g_hook_destroy_link(gnc_hook->scm_danglers, hook);
  LEAVE("Removed dangler from %s", name);
}

static void
call_c_hook (GHook *hook, gpointer data)
{
  ENTER("hook %p (func %p), data %p, cbarg %p", hook, hook->func, data,
	hook->data);
  ((GFunc)hook->func)(data, hook->data);
  LEAVE("");
}

void
gnc_hook_run (const gchar *name, gpointer data)
{
  GncHook *hook;

  ENTER("list %s, data %p", (name == NULL ? "(null)" : name), data);
  hook = gnc_hook_lookup(name);
  if (!hook) {
    LEAVE("No such hook list");
    return;
  }
  g_hook_list_marshal(hook->c_danglers, TRUE, call_c_hook, data);
  if (hook->num_args == 0)
    g_hook_list_marshal(hook->scm_danglers, TRUE, call_scm_hook, data);
  else
    g_hook_list_marshal(hook->scm_danglers, TRUE, call_scm_hook_1, data);
  LEAVE("");
}

void
gnc_hooks_init(void)
{
  ENTER("");

  if (gnc_hooks_initialized) {
    LEAVE("Hooks already initialized");
    return;
  }

  gnc_hooks_initialized = TRUE;

  gnc_hook_create(HOOK_STARTUP, 0,
		  "Functions to run at startup.  Hook args: ()");
  gnc_hook_create(HOOK_SHUTDOWN, 0,
		  "Functions to run at guile shutdown.  Hook args: ()");
  gnc_hook_create(HOOK_UI_STARTUP, 0,
		  "Functions to run when the ui comes up.  Hook args: ()");
  gnc_hook_create(HOOK_UI_POST_STARTUP, 0,
		  "Functions to run after the ui comes up.  Hook args: ()");
  gnc_hook_create(HOOK_UI_SHUTDOWN, 0,
		  "Functions to run at ui shutdown.  Hook args: ()");
  gnc_hook_create(HOOK_NEW_BOOK, 0,
		  "Run after a new (empty) book is opened, before the"
		  " book-opened-hook. Hook args: ()");
  gnc_hook_create(HOOK_REPORT, 0,
		  "Run just before the reports are pushed into the menus."
		  "  Hook args: ()");
  gnc_hook_create(HOOK_CURRENCY_CHANGED, 0,
		  "Functions to run when the user changes currency settings.  Hook args: ()");
  gnc_hook_create(HOOK_SAVE_OPTIONS, 0,
		  "Functions to run when saving options.  Hook args: ()");
  gnc_hook_create(HOOK_ADD_EXTENSION, 0,
		  "Functions to run when the extensions menu is created."
		  "  Hook args: ()");

  gnc_hook_create(HOOK_BOOK_OPENED, 1,
		  "Run after book open.  Hook args: <gnc:Session*>.");
  gnc_hook_create(HOOK_BOOK_CLOSED, 1,
		  "Run before file close.  Hook args: <gnc:Session*>");
  gnc_hook_create(HOOK_BOOK_SAVED, 1,
		  "Run after file saved.  Hook args: <gnc:Session*>");

  LEAVE("");
}
