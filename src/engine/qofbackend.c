/********************************************************************\
 * qofbackend.c -- utility routines for dealing with the data backend  *
 * Copyright (C) 2000 Linas Vepstas <linas@linas.org>               *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"
#include <stdarg.h>
#include <glib.h>

#include "qofbackend.h"
#include "qofbackend-p.h"

/* static short module = MOD_ENGINE; */

/********************************************************************\
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
  
   va_start(args, format);
   buffer = (char *)g_strdup_vprintf(format, args);
   va_end(args);

   /* If there's already something here, free it */
   if (be->error_msg) g_free(be->error_msg);

   be->error_msg = buffer;
}

/* This should always return a valid char * */
char *
qof_backend_get_message (QofBackend *be) 
{
   char * msg;
   
   if (!be) return g_strdup("ERR_BACKEND_NO_BACKEND");
   if (!be->error_msg) return NULL;

   /* 
    * Just return the contents of the error_msg and then set it to
    * NULL.  This is necessary, because the Backends don't seem to
    * have a destroy_backend function to take care if freeing stuff
    * up.  The calling function should free the copy.
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

    be->events_pending = NULL;
    be->process_events = NULL;

    be->last_err = ERR_BACKEND_NO_ERR;
    if (be->error_msg) g_free (be->error_msg);
    be->error_msg = NULL;
    be->percentage = NULL;

    /* XXX remove these */
    be->price_lookup = NULL;
    be->export = NULL;
}

/************************* END OF FILE ********************************/
