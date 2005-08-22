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

#define _GNU_SOURCE
#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <regex.h>
#include <glib.h>
#include <dlfcn.h>
#include <sys/stat.h>
#include <errno.h>
#include "qofbackend-p.h"

/* static short module = MOD_BACKEND; */

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
  
   /* If there's already something here, free it */
   if (be->error_msg) g_free(be->error_msg);

   if (!format) {
       be->error_msg = NULL;
       return;
   }

   va_start(args, format);
   buffer = (char *)g_strdup_vprintf(format, args);
   va_end(args);

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
	be->load_config = NULL;

    be->events_pending = NULL;
    be->process_events = NULL;

    be->last_err = ERR_BACKEND_NO_ERR;
    if (be->error_msg) g_free (be->error_msg);
    be->error_msg = NULL;
    be->percentage = NULL;
	be->backend_configuration = kvp_frame_new();

#ifdef GNUCASH_MAJOR_VERSION
    /* XXX remove these */
    be->fullpath = NULL;
    be->price_lookup = NULL;
    be->export = NULL;
#endif
}

void
qof_backend_run_begin(QofBackend *be, QofInstance *inst)
{
	if(!be | !inst) { return; }
	if(!be->begin) { return; }
	(be->begin) (be, inst);
}

gboolean
qof_backend_begin_exists(QofBackend *be)
{
	if(be->begin) { return TRUE; }
	else { return FALSE; }
}

void
qof_backend_run_commit(QofBackend *be, QofInstance *inst)
{
	if(!be | !inst) { return; }
	if(!be->commit) { return; }
	(be->commit) (be, inst);
}

void
qof_backend_load_config(QofBackend *be, KvpFrame *config)
{
	if(!be | !config) { return; }
	if(!be->load_config) { return; }
	(be->load_config) (be, config);
}

KvpFrame*
qof_backend_get_config(QofBackend *be)
{
	if(!be) { return NULL; }
	if(!be->get_config) { return NULL; }
	return (be->get_config) (be);
}

gboolean
qof_backend_commit_exists(QofBackend *be)
{
	if(!be) { return FALSE; }
	if(be->commit) { return TRUE; }
	else { return FALSE; }
}

gboolean
qof_backend_check_type (QofBackend *be, const char *path)
{
	if(!be) { return FALSE; }
	if(!be->check_data_type) { return FALSE; }
	return (gboolean)(be->check_data_type) (be, path);
}

void 
qof_begin_edit(QofInstance *inst)
{
  QofBackend * be;

  if (!inst) { return; }
  inst->editlevel++;
  if (1 < inst->editlevel) return;
  if (0 >= inst->editlevel) { inst->editlevel = 1; }
  be = qof_book_get_backend (inst->book);
    if (be && qof_backend_begin_exists(be)) {
     qof_backend_run_begin(be, inst);
  } else { inst->dirty = TRUE; }
}

void qof_commit_edit(QofInstance *inst)
{
  QofBackend * be;

  if (!inst) return;
  inst->editlevel--;
  if (0 < inst->editlevel) { return; }
  if ((-1 == inst->editlevel) && inst->dirty)
  {
    be = qof_book_get_backend ((inst)->book);
    if (be && qof_backend_begin_exists(be)) {
     qof_backend_run_begin(be, inst);
    }
    inst->editlevel = 0;
  }
  if (0 > inst->editlevel) { inst->editlevel = 0; }
}

#define STR_DLNAME     "dlname="
#define STR_LIBDIR     "libdir="

gboolean
qof_load_backend_library (const char* filename, const char* init_fcn)
{
	FILE *filehandle;
	void (*initfn) (void);
	void *dl_hand = NULL;
#ifndef HAVE_GETLINE
	char lineptr[1024];
#else
	char *lineptr;
#endif
	const char * err_str;
	gchar *dlname, *libdirpath, *tempstr;
	int errors;
	guint n, end;
	struct stat sbuf;

	errors = 0;
	dlname = NULL;
	tempstr = NULL;
	libdirpath = NULL;
	g_return_val_if_fail((filename || init_fcn), FALSE); 
	g_return_val_if_fail(g_str_has_suffix (filename, ".la"), FALSE);
	/* now we have a filename ending in .la, see if we can open it. */
	n = (guint)strlen(STR_DLNAME);
	g_return_val_if_fail((stat(filename, &sbuf) <0), FALSE);
	filehandle = fopen(filename, "r");
	g_return_val_if_fail((filehandle), FALSE);
#ifndef HAVE_GETLINE
	while (NULL != (fgets(lineptr, sizeof(lineptr), filehandle)))
#else
	lineptr = NULL;
	while (0 < getline(&lineptr, &n, filehandle))
#endif
	{
		if (strncmp (lineptr, STR_DLNAME, n - 1) == 0)
		{
			/* obtain substring from dlname='.*'\n
			 where .* matches the library .so|.dylib name
			 allowing for single quotes, if used. */
			tempstr = g_strdup(lineptr + n);
			if(tempstr[0] == '\'') { tempstr++; }
			dlname = g_strndup(tempstr, strlen(tempstr) - 1);
			end = strlen(dlname);
			if(dlname[end-1] == '\'') 
			{ 
				tempstr = g_strndup(dlname, end - 1);
				dlname = tempstr;
			}
		}
		/* now get the path, just in case */
		n = (guint)strlen(STR_LIBDIR);
		if (strncmp (lineptr, STR_LIBDIR, n - 1) == 0)
		{
			tempstr = g_strdup(lineptr + n);
			if(tempstr[0] == '\'') { tempstr++; }
			libdirpath = g_strndup(tempstr, strlen(tempstr) - 1);
			end = strlen(libdirpath);
			if(libdirpath[end-1] == '\'')
			{
				tempstr = g_strndup(libdirpath, end - 1);
				libdirpath = tempstr;
			}
			break;
		}
	}
	fclose(filehandle);
	tempstr = g_strconcat(libdirpath, "/", dlname, NULL);
	dlname = tempstr;
	g_free(libdirpath);
	dl_hand = dlopen (dlname, RTLD_LAZY);
	if (NULL == dl_hand)
	{
		err_str = dlerror();
		g_message ("Can't load backend, %s\n", err_str);
		return FALSE;
	}
	initfn = dlsym (dl_hand, init_fcn);
	if (initfn) { (*initfn)(); }
	else
	{
		err_str = dlerror();
		g_message("Can't find %s:%s, %s\n", dlname, init_fcn, err_str);
		return FALSE;
	}
	g_free(dlname);
	return TRUE;
}

/************************* END OF FILE ********************************/
