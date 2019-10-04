/*
 * gnc-hooks-scm.c -- helpers for using Glib hook functions in guile
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

#include <config.h>

#include <glib.h>
#include <stdio.h>
#include "gnc-hooks.h"
#include "gnc-hooks-scm.h"
#include "gnc-engine.h"
#include <libguile.h>
#include "swig-runtime.h"
#include <guile-mappings.h>

static QofLogModule log_module = GNC_MOD_ENGINE;

typedef struct
{
    SCM proc;
    int num_args;
} GncScmDangler;


static void
delete_scm_hook (gpointer data)
{
    GncScmDangler *scm = data;
    scm_gc_unprotect_object(scm->proc);
    g_free(scm);
}

static void
scm_hook_cb (gpointer data, GncScmDangler *scm)
{
    ENTER("data %p, cbarg %p", data, scm);

    if (scm->num_args == 0)
        scm_call_0 (scm->proc);
    else
    {
        // XXX: FIXME: We really should make sure this is a session!!! */
        scm_call_1 (scm->proc,
            SWIG_NewPointerObj(data, SWIG_TypeQuery("_p_QofSession"), 0));
    }

    LEAVE("");
}

void
gnc_hook_add_scm_dangler (const gchar *name, SCM proc)
{
    GHook *hook;
    GncScmDangler *scm;
    int num_args;

    ENTER("list %s, proc ???", name);
    num_args = gnc_hook_num_args(name);
    g_return_if_fail(num_args >= 0);
    scm = g_new0(GncScmDangler, 1);
    scm_gc_protect_object(proc);
    scm->proc = proc;
    scm->num_args = num_args;
    gnc_hook_add_dangler(name, (GFunc)scm_hook_cb,
                         (GDestroyNotify) delete_scm_hook, scm);
    LEAVE("");
}
