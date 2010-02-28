/********************************************************************
 * gnc-engine.c  -- top-level initialization for GnuCash Engine     *
 * Copyright 2000 Bill Gribble <grib@billgribble.com>               *
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
 ********************************************************************/

#include "config.h"

#include <glib.h>
#include "gnc-engine.h"
#include "qof.h"
#include "cashobjects.h"
#include "AccountP.h"
#include "SX-book-p.h"
#include "gnc-budget.h"
#include "TransactionP.h"
#include "gnc-commodity.h"
#include "gnc-pricedb-p.h"

/** gnc file backend library name */
#define GNC_LIB_NAME "gncmod-backend-xml"

/* gnc-backend-file location */
#include "gnc-path.h"

static GList * engine_init_hooks = NULL;
static int engine_is_initialized = 0;

EngineCommitErrorCallback g_error_cb;
gpointer g_error_cb_data;

// static QofLogModule log_module = GNC_MOD_ENGINE;

/* GnuCash version functions */
unsigned int
gnucash_major_version (void)
{
  return GNUCASH_MAJOR_VERSION;
}

unsigned int
gnucash_minor_version (void)
{
  return GNUCASH_MINOR_VERSION;
}

unsigned int
gnucash_micro_version (void)
{
  return GNUCASH_MICRO_VERSION;
}

/********************************************************************
 * gnc_engine_init
 * initialize backend, load any necessary databases, etc. 
 ********************************************************************/

void 
gnc_engine_init(int argc, char ** argv)
{
  static struct {
    const gchar* lib;
    gboolean required;
  } libs[] = {
#if defined( HAVE_DBI_DBI_H )
    { "gncmod-backend-dbi", TRUE },
#endif
    { "gncmod-backend-xml", TRUE },
    { NULL, FALSE } }, *lib;
  gnc_engine_init_hook_t hook;
  GList * cur;
  gchar *pkglibdir;

  if (1 == engine_is_initialized) return;

  /* initialize QOF */
  qof_init();
  qof_set_alt_dirty_mode(TRUE);

  /* Now register our core types */
  cashobjects_register();

  pkglibdir = gnc_path_get_pkglibdir ();
  for (lib = libs; lib->lib ; lib++)
  {
      if (qof_load_backend_library(pkglibdir, lib->lib))
      {
          engine_is_initialized = 1;
      }
      else
      {
          g_warning("failed to load %s from %s\n", lib->lib, pkglibdir);
          /* If this is a required library, stop now! */
          if (lib->required)
          {
              g_critical("required library %s not found.\n", lib->lib);
          }
      }
  }
  g_free (pkglibdir);

  /* call any engine hooks */
  for (cur = engine_init_hooks; cur; cur = cur->next)
  {
    hook = (gnc_engine_init_hook_t)cur->data;

    if (hook)
      (*hook)(argc, argv);
  }
}

/********************************************************************
 * gnc_engine_shutdown
 * shutdown backend, destroy any global data, etc.
 ********************************************************************/

void
gnc_engine_shutdown (void)
{
  qof_log_shutdown();
  qof_close();
  engine_is_initialized = 0;
}

/********************************************************************
 * gnc_engine_add_init_hook
 * add a startup hook 
 ********************************************************************/

void
gnc_engine_add_init_hook(gnc_engine_init_hook_t h) {
  engine_init_hooks = g_list_append(engine_init_hooks, (gpointer)h);
}

gboolean
gnc_engine_is_initialized (void)
{
    return (engine_is_initialized == 1) ? TRUE : FALSE;
}

/* replicate old gnc-trace enum behaviour
 *
 * these are only here as a convenience, they could be
 * initialised elsewhere as appropriate.
 * */
void gnc_log_default(void)
{
	qof_log_set_default(QOF_LOG_WARNING);
    qof_log_set_level(GNC_MOD_ROOT, QOF_LOG_WARNING);
	qof_log_set_level(GNC_MOD_TEST, QOF_LOG_DEBUG);
}

void
gnc_engine_add_commit_error_callback( EngineCommitErrorCallback cb, gpointer data )
{
    g_error_cb = cb;
    g_error_cb_data = data;
}

void
gnc_engine_signal_commit_error( QofBackendError errcode )
{
    if( g_error_cb != NULL ) {
	(*g_error_cb)( g_error_cb_data, errcode );
    }
}
