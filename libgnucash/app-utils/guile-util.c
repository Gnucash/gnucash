/********************************************************************\
 * guile-util.c -- utility functions for using guile for GnuCash    *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
 * Copyright (C) 2017 Aaron Laws                                    *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <config.h>

#include "swig-runtime.h"
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <winsock.h>
#include <windows.h>
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>
#ifdef __MINGW32__
#define _GL_UNISTD_H //Deflect poisonous define of close in Guile's GnuLib
#endif
#include <libguile.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
# ifdef close
#  undef close
# endif
# include <unistd.h>
#else
# include <io.h>
# define close _close
#endif
#ifndef HAVE_STRPTIME
#    include "strptime.h"
#endif

#include "qof.h"
#include "engine-helpers-guile.h"
#include "glib-helpers.h"
#include "gnc-glib-utils.h"
#include "gnc-guile-utils.h"
#include "gnc-prefs.h"
#include "guile-util.h"
#include "guile-mappings.h"

#define UNUSED_VAR     __attribute__ ((unused))

/* This static indicates the debugging module this .o belongs to.  */
static QofLogModule UNUSED_VAR log_module = GNC_MOD_GUILE;

struct _getters
{
    SCM debit_string;
    SCM credit_string;
} getters;

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
initialize_scm_functions()
{
    static gboolean scm_funcs_inited = FALSE;

    if (scm_funcs_inited)
        return;

    getters.debit_string = scm_c_eval_string("gnc:get-debit-string");
    getters.credit_string = scm_c_eval_string("gnc:get-credit-string");

    scm_funcs_inited = TRUE;
}


/********************************************************************\
 * gnc_get_debit_string                                             *
 *   return a debit string for a given account type                 *
 *                                                                  *
 * Args: account_type - type of account to get debit string for     *
 * Return: g_malloc'd debit string or NULL                          *
\********************************************************************/
char *
gnc_get_debit_string(GNCAccountType account_type)
{
    SCM result;
    SCM arg;

    initialize_scm_functions();

    if (gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_ACCOUNTING_LABELS))
        return g_strdup(_("Debit"));

    if ((account_type < ACCT_TYPE_NONE) || (account_type >= NUM_ACCOUNT_TYPES))
        account_type = ACCT_TYPE_NONE;

    arg = scm_from_long (account_type);

    result = scm_call_1(getters.debit_string, arg);
    if (!scm_is_string(result))
        return NULL;

    return scm_to_utf8_string(result);
}


/************************************************************************\
 * gnc_get_credit_string                                                *
 *   return a credit string for a given account type                    *
 *                                                                      *
 * Args: account_type - type of account to get credit string for        *
 * Return: g_malloc'd credit string or NULL, must be freed with g_free  *
\************************************************************************/
char *
gnc_get_credit_string(GNCAccountType account_type)
{
    SCM result;
    SCM arg;

    initialize_scm_functions();

    if (gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_ACCOUNTING_LABELS))
        return g_strdup(_("Credit"));

    if ((account_type < ACCT_TYPE_NONE) || (account_type >= NUM_ACCOUNT_TYPES))
        account_type = ACCT_TYPE_NONE;

    arg = scm_from_long (account_type);

    result = scm_call_1(getters.credit_string, arg);
    if (!scm_is_string(result))
        return NULL;

    return gnc_scm_to_utf8_string(result);
}


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
        g_warning ("Pipe to childs file descriptor %d is -1", std_fd);
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
        g_message ("Close of childs stdin (%d) failed: %s", proc->fd_stdin,
                   g_strerror (errno));
        errno = 0;
    }
    close (proc->fd_stdout);
    if (errno)
    {
        g_message ("Close of childs stdout (%d) failed: %s", proc->fd_stdout,
                   g_strerror(errno));
        errno = 0;
    }
    close (proc->fd_stderr);
    if (errno)
    {
        g_message ("Close of childs stderr (%d) failed: %s", proc->fd_stderr,
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


time64
gnc_parse_time_to_time64 (const gchar *s, const gchar *format)
{
    struct tm tm;

    g_return_val_if_fail(s && format, -1);

    if (!strptime(s, format, &tm))
        return -1;

    return gnc_mktime(&tm);
}
