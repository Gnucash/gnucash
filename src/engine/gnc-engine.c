/********************************************************************
 * gnc-engine.c  -- top-level initialization for Gnucash Engine     *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#include "config.h"

#include <glib.h>

#include "GNCIdP.h"
#include "QueryNewP.h" 
#include "gncObjectP.h"
#include "gnc-engine.h"

static GList * engine_init_hooks = NULL;
static int engine_is_initialized = 0;
GCache * gnc_string_cache = NULL;


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
  gnc_engine_init_hook_t hook;
  GList                  * cur;

  if (1 == engine_is_initialized) return;
  engine_is_initialized = 1;

  /* initialize the string cache */
  gnc_engine_get_string_cache();
  
  xaccGUIDInit ();
  gncObjectInitialize ();
  gncQueryNewInit ();

  /* call any engine hooks */
  for (cur = engine_init_hooks; cur; cur = cur->next)
  {
    hook = (gnc_engine_init_hook_t)cur->data;

    if (hook)
      (*hook)(argc, argv);
  }
}

GCache*
gnc_engine_get_string_cache(void)
{
    if(!gnc_string_cache) 
    {
        gnc_string_cache = g_cache_new(
            (GCacheNewFunc) g_strdup, g_free,
            (GCacheDupFunc) g_strdup, g_free, g_str_hash, 
            g_str_hash, g_str_equal);
    }
    return gnc_string_cache;
}

/********************************************************************
 * gnc_engine_shutdown
 * shutdown backend, destroy any global data, etc.
 ********************************************************************/

void
gnc_engine_shutdown (void)
{
  gncQueryNewShutdown ();

  g_cache_destroy (gnc_string_cache);
  gnc_string_cache = NULL;

  gncObjectShutdown ();
  xaccGUIDShutdown ();
}

/********************************************************************
 * gnc_engine_add_init_hook
 * add a startup hook 
 ********************************************************************/

void
gnc_engine_add_init_hook(gnc_engine_init_hook_t h) {
  engine_init_hooks = g_list_append(engine_init_hooks, (gpointer)h);
}
