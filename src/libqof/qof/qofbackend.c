/********************************************************************\
 * qofbackend.c -- utility routines for dealing with the data backend  *
 * Copyright (C) 2000 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2004-5 Neil Williams <linux@codehelp.co.uk>        *
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

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <regex.h>
#include <glib.h>
#include <gmodule.h>
#include <errno.h>
#include "qof.h"
#include "qofbackend-p.h"

static QofLogModule log_module = QOF_MOD_BACKEND;

#define QOF_CONFIG_DESC    "desc"
#define QOF_CONFIG_TIP     "tip"

/* *******************************************************************\
 * error handling                                                   *
\********************************************************************/

void
qof_backend_set_error (QofBackend *be, QofBackendError err)
{
    if (!be) return;

    /* use stack-push semantics. Only the earliest error counts */
    if (ERR_BACKEND_NO_ERR != be->last_err) return;
    be->last_err = err;
}

QofBackendError
qof_backend_get_error (QofBackend *be)
{
    QofBackendError err;
    if (!be) return ERR_BACKEND_NO_BACKEND;

    /* use 'stack-pop' semantics */
    err = be->last_err;
    be->last_err = ERR_BACKEND_NO_ERR;
    return err;
}

void
qof_backend_set_message (QofBackend *be, const char *format, ...)
{
    va_list args;
    char * buffer;

    if (!be) return;

    /* If there's already something here, free it */
    if (be->error_msg) g_free(be->error_msg);

    if (!format)
    {
        be->error_msg = NULL;
        return;
    }

    va_start(args, format);
    buffer = (char *)g_strdup_vprintf(format, args);
    va_end(args);

    be->error_msg = buffer;
}

char *
qof_backend_get_message (QofBackend *be)
{
    char * msg;

    if (!be) return g_strdup("ERR_BACKEND_NO_BACKEND");
    if (!be->error_msg) return NULL;

    /*
     * Just return the contents of the error_msg and then set it to
     * NULL. This is necessary, because the Backends don't seem to
     * have a destroy_backend function to take care of freeing stuff
     * up. The calling function should free the copy.
     * Also, this is consistent with the qof_backend_get_error() popping.
     */

    msg = be->error_msg;
    be->error_msg = NULL;
    return msg;
}

/***********************************************************************/
/* Get a clean backend */
void
qof_backend_init(QofBackend *be)
{
    be->session_begin = NULL;
    be->session_end = NULL;
    be->destroy_backend = NULL;

    be->load = NULL;

    be->begin = NULL;
    be->commit = NULL;
    be->rollback = NULL;

    be->compile_query = NULL;
    be->free_query = NULL;
    be->run_query = NULL;

    be->sync = NULL;
    be->safe_sync = NULL;
    be->load_config = NULL;

    be->events_pending = NULL;
    be->process_events = NULL;

    be->last_err = ERR_BACKEND_NO_ERR;
    if (be->error_msg) g_free (be->error_msg);
    be->error_msg = NULL;
    be->percentage = NULL;
    be->backend_configuration = kvp_frame_new();

    /* to be removed */
    be->price_lookup = NULL;
    be->export_fn = NULL;
}

void
qof_backend_destroy(QofBackend *be)
{
    g_free(be->error_msg);
    be->error_msg = NULL;
    kvp_frame_delete(be->backend_configuration);
    be->backend_configuration = NULL;
}

void
qof_backend_run_begin(QofBackend *be, QofInstance *inst)
{
    if (!be || !inst)
    {
        return;
    }
    if (!be->begin)
    {
        return;
    }
    (be->begin) (be, inst);
}

gboolean
qof_backend_begin_exists(const QofBackend *be)
{
    if (be->begin)
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

void
qof_backend_run_commit(QofBackend *be, QofInstance *inst)
{
    if (!be || !inst)
    {
        return;
    }
    if (!be->commit)
    {
        return;
    }
    (be->commit) (be, inst);
}


gboolean
qof_backend_commit_exists(const QofBackend *be)
{
    if (!be)
    {
        return FALSE;
    }
    if (be->commit)
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

static GSList* backend_module_list = NULL;

gboolean
qof_load_backend_library (const char *directory, const char* module_name)
{
    gchar *fullpath;
    GModule *backend;
    void (*module_init_func) (void);

    g_return_val_if_fail(g_module_supported(), FALSE);
    fullpath = g_module_build_path(directory, module_name);
    backend = g_module_open(fullpath, G_MODULE_BIND_LAZY);
    g_free(fullpath);
    if (!backend)
    {
        g_message ("%s: %s\n", PACKAGE, g_module_error ());
        return FALSE;
    }
    if (g_module_symbol(backend, "qof_backend_module_init",
                        (gpointer)&module_init_func))
        module_init_func();

    g_module_make_resident(backend);
    backend_module_list = g_slist_prepend( backend_module_list, backend );
    return TRUE;
}

void
qof_finalize_backend_libraries(void)
{
    GSList* node;
    GModule* backend;
    void (*module_finalize_func) (void);

    for (node = backend_module_list; node != NULL; node = node->next)
    {
        backend = (GModule*)node->data;

        if (g_module_symbol(backend, "qof_backend_module_finalize",
                            (gpointer)&module_finalize_func))
            module_finalize_func();

    }
}

/************************* END OF FILE ********************************/
