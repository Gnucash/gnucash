/********************************************************************\
 * gnc-gnome-utils.c -- utility functions for gnome for GnuCash     *
 * Copyright (C) 2001 Linux Developers Group                        *
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

#include <gnome.h>
#include <guile/gh.h>

#ifdef GTKHTML_HAVE_GCONF
#include <gconf/gconf.h>
#endif

#ifdef USE_GUPPI
#include "gnc-html-guppi.h"
#endif

#include "argv-list-converters.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"


static char**
gnc_scm2argv (SCM scm, int prelen, const char **prependargv)
{
  /* FIXME: when we drop support older guiles, drop the (char *) coercion. */
  return gnc_scheme_list_to_nulltermcharpp (prelen, prependargv, scm);
}

static SCM
gnc_argv2scm (int len, const char **rest)
{
  return gnc_argvarr_to_scheme_list (len, rest);
}

static const char *default_argv[] = {"", 0};

static const struct poptOption nullPoptTable[] = {
  { NULL, 0, 0, NULL, 0 }
};

SCM
gnc_gnome_init (const char * arg0,
                const char * progname,
                const char * version,
                SCM command_line)
{
  poptContext returnedPoptContext;
  int restargc;
  char **restargv;
  char **restargv2;
  SCM ret = command_line;

  if (arg0)
    default_argv[0] = arg0;

  restargv = gnc_scm2argv (command_line, 1, default_argv);
  if (!restargv)
  {
    restargv = g_new (char*, 2);
    restargv[0] = g_strdup (default_argv[0]);
    restargv[1] = NULL;
  }

  restargc = argv_length (restargv);

  gnome_init_with_popt_table (progname, version, restargc, restargv,
                              nullPoptTable, 0, &returnedPoptContext);

  restargv2 = (char**) poptGetArgs (returnedPoptContext);
  ret = gnc_argv2scm (argv_length (restargv2), (const char**)restargv2);

#ifdef GTKHTML_HAVE_GCONF
  {
    GError * gerror;

    if (!gconf_init (restargc, restargv, &gerror))
      g_error_free (gerror);
  }
#endif

  /* this must come after using the poptGetArgs return value */
  poptFreeContext (returnedPoptContext);
  gnc_free_argv (restargv);

  /* initialization required for gtkhtml */
  gdk_rgb_init ();    
  gtk_widget_set_default_colormap (gdk_rgb_get_cmap ());
  gtk_widget_set_default_visual (gdk_rgb_get_visual ());
    
#ifdef USE_GUPPI    
  /* initialize guppi handling in gnc-html */
  gnc_html_guppi_init ();
#endif

  return ret;
}

void
gnc_gnome_shutdown (void)
{
#ifdef USE_GUPPI    
  gnc_html_guppi_shutdown();
#endif
}
