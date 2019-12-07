/********************************************************************\
 * glib-guile.c -- glib helper functions for guile                  *
 * Copyright (C) 2000 Linas Vepstas                                 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>         *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include <config.h>

#include <errno.h>
#include <string.h>
#include <glib.h>

#ifdef __MINGW32__
#define _GL_UNISTD_H //Deflect poisonous define of close in Guile's GnuLib
#endif
#include <libguile.h>
#ifdef HAVE_UNISTD_H
# ifdef close
#  undef close
# endif
# include <unistd.h>
#else
# include <io.h>
# define close _close
#endif

#include <libguile.h>
#include "swig-runtime.h"
#include "guile-mappings.h"
#include "gnc-glib-utils.h"
#include "gnc-guile-utils.h"
#include "glib-guile.h"

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <winsock.h>
#include <windows.h>
#endif

#include "qof.h"
#include "gnc-engine-guile.h"


#define UNUSED_VAR     __attribute__ ((unused))

/* This static indicates the debugging module this .o belongs to.  */
static QofLogModule UNUSED_VAR log_module = GNC_MOD_GUILE;

static SCM
glist_to_scm_list_helper(GList *glist, swig_type_info *wct)
{
    SCM list = SCM_EOL;
    GList *node;

    for (node = glist; node; node = node->next)
        list = scm_cons(SWIG_NewPointerObj(node->data, wct, 0), list);

    return scm_reverse (list);
}

SCM
gnc_glist_to_scm_list(GList *glist, gchar *wct)
{
    swig_type_info *stype = SWIG_TypeQuery(wct);
    g_return_val_if_fail(stype, SCM_UNDEFINED);
    return glist_to_scm_list_helper(glist, stype);
}

GList *
gnc_scm_list_to_glist(SCM rest)
{
    GList *result = NULL;
    SCM scm_item;

    SWIG_GetModule(NULL); /* Work-around for SWIG bug. */
    SCM_ASSERT(scm_is_list(rest), rest, SCM_ARG1, "gnc_scm_list_to_glist");

    while (!scm_is_null(rest))
    {
        void *item;

        scm_item = SCM_CAR(rest);
        rest = SCM_CDR(rest);

        if (scm_item == SCM_BOOL_F)
        {
            result = g_list_prepend(result, NULL);
        }
        else
        {
            if (!SWIG_IsPointer(scm_item))
                scm_misc_error("gnc_scm_list_to_glist",
                               "Item in list not a wcp.", scm_item);

            item = (void *)SWIG_PointerAddress(scm_item);
            result = g_list_prepend(result, item);
        }
    }

    return g_list_reverse(result);
}

/********************************************************************
 * gnc_glist_string_to_scm
 * i.e. (glist-of (<gw:mchars> calee-owned) callee-owned)
 * or equivalently
 * i.e. (glist-of (<gw:gchars> calee-owned) callee-owned)
 ********************************************************************/
SCM
gnc_glist_string_to_scm(GList *glist)
{
    SCM list = SCM_EOL;
    GList *node;

    for (node = glist; node; node = node->next)
    {
        if (node->data)
            list = scm_cons (scm_from_utf8_string(node->data), list);
        else
            list = scm_cons (SCM_BOOL_F, list);
    }

    return scm_reverse (list);
}




/********************************************************************
 * gnc_scm_to_glist_string
 * i.e. (glist-of (<gw:mchars> callee-owned) callee-owned)
 * or equivalently
 * i.e. (glist-of (<gw:gchars> callee-owned) callee-owned)
 ********************************************************************/

GList *
gnc_scm_to_glist_string(SCM list)
{
    GList *glist = NULL;

    while (!scm_is_null (list))
    {
        if (scm_is_string(SCM_CAR(list)))
        {
            gchar * str;

            str = gnc_scm_to_utf8_string (SCM_CAR(list));
            if (str)
                glist = g_list_prepend (glist, g_strdup (str));
            g_free (str);
        }
        list = SCM_CDR (list);
    }

    return g_list_reverse (glist);
}

GSList *
gnc_scm_to_gslist_string(SCM list)
{
    GSList *gslist = NULL;

    while (!scm_is_null (list))
    {
        if (scm_is_string(SCM_CAR(list)))
        {
            gchar * str;

            str = gnc_scm_to_utf8_string (SCM_CAR(list));
            if (str)
                gslist = g_slist_prepend (gslist, g_strdup (str));
            g_free (str);
        }
        list = SCM_CDR (list);
    }

    return g_slist_reverse (gslist);
}

/********************************************************************
 * gnc_glist_string_p
 ********************************************************************/

int
gnc_glist_string_p(SCM list)
{
    return scm_is_list(list);
}

struct _Process
{
    GPid pid;
    gint fd_stdin;
    gint fd_stdout;
    gint fd_stderr;
    gboolean dead;
    gboolean detached;
};

static void
on_child_exit (GPid pid, gint status, gpointer data)
{
    Process *proc = data;
    g_return_if_fail (proc && proc->pid == pid);

    g_spawn_close_pid (proc->pid);

    /* free if the process is both dead and detached */
    if (!proc->detached)
        proc->dead = TRUE;
    else
        g_free (proc);
}

Process *
gnc_spawn_process_async (GList *argl, const gboolean search_path)
{
    gboolean retval;
    Process *proc;
    GList *l_iter;
    guint argc;
    gchar **argv, **v_iter;
    GSpawnFlags flags;
    GError *error = NULL;

    proc = g_new0 (Process, 1);

    argc = g_list_length (argl);
    argv = g_malloc ((argc + 1) * sizeof(gchar*));

    for (l_iter = argl, v_iter = argv; l_iter; l_iter = l_iter->next, v_iter++)
    {
        *v_iter = (gchar*) l_iter->data;
    }
    *v_iter = NULL;
    g_list_free (argl);

    flags = G_SPAWN_DO_NOT_REAP_CHILD;
    if (search_path)
        flags |= G_SPAWN_SEARCH_PATH;

    retval = g_spawn_async_with_pipes (
        NULL, argv, NULL, flags, NULL, NULL, &proc->pid,
        &proc->fd_stdin, &proc->fd_stdout, &proc->fd_stderr, &error);

    if (retval)
    {
        g_child_watch_add (proc->pid, on_child_exit, proc);
    }
    else
    {
        g_warning ("Could not spawn %s: %s", *argv ? *argv : "(null)",
                   error->message ? error->message : "(null)");
        g_free (proc);
        proc = NULL;
    }
    g_strfreev (argv);

    return proc;
}

gint
gnc_process_get_fd (const Process *proc, const gint std_fd)
{
    const gint *retptr = NULL;
    g_return_val_if_fail (proc, -1);

    if (std_fd == 0)
        retptr = &proc->fd_stdin;
    else if (std_fd == 1)
        retptr = &proc->fd_stdout;
    else if (std_fd == 2)
        retptr = &proc->fd_stderr;
    else
        g_return_val_if_reached (-1);

    if (*retptr == -1)
        g_warning ("Pipe to child's file descriptor %d is -1", std_fd);
    return *retptr;
}

void
gnc_detach_process (Process *proc, const gboolean kill_it)
{
    g_return_if_fail (proc && proc->pid);

    errno = 0;
    close (proc->fd_stdin);
    if (errno)
    {
        g_message ("Close of child's stdin (%d) failed: %s", proc->fd_stdin,
                   g_strerror (errno));
        errno = 0;
    }
    close (proc->fd_stdout);
    if (errno)
    {
        g_message ("Close of child's stdout (%d) failed: %s", proc->fd_stdout,
                   g_strerror(errno));
        errno = 0;
    }
    close (proc->fd_stderr);
    if (errno)
    {
        g_message ("Close of child's stderr (%d) failed: %s", proc->fd_stderr,
                   g_strerror(errno));
        errno = 0;
    }

    if (kill_it && !proc->dead)
    {
        /* give it a chance to die */
        while (g_main_context_iteration (NULL, FALSE) && !proc->dead)
            ;
        if (!proc->dead)
            gnc_gpid_kill (proc->pid);
    }

    /* free if the process is both dead and detached */
    if (!proc->dead)
        proc->detached = TRUE;
    else
        g_free (proc);
}
