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
#include <libguile.h>

#ifdef GTKHTML_HAVE_GCONF
#include <gconf/gconf.h>
#endif

#ifdef USE_GUPPI
#include "gnc-html-guppi.h"
#endif

#include "argv-list-converters.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"
#include "gnc-trace.h"

#include <libgnomeui/gnome-window-icon.h>
#include <gnc-dir.h>

static short module = MOD_GUI;
static GnomeProgram *gnucash_program = NULL;

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
  poptContext returnedPoptContext; /* owned by the library */
  int restargc;
  char *fullname;
  char **restargv;
  char **restargv2;
  SCM ret = command_line;
  GError *error = NULL;
  GValue value = { 0, };

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

  gnucash_program =
    gnome_program_init("gnucash", version, LIBGNOMEUI_MODULE,
		       restargc, restargv,
		       GNOME_PARAM_POPT_TABLE, nullPoptTable,
		       GNOME_PROGRAM_STANDARD_PROPERTIES,
		       GNOME_PARAM_NONE);

  g_value_init(&value, G_TYPE_POINTER);
  g_object_get_property (G_OBJECT (gnucash_program),
			 GNOME_PARAM_POPT_CONTEXT, &value);
  returnedPoptContext = g_value_get_pointer (&value);
  restargv2 = (char**) poptGetArgs (returnedPoptContext);
  ret = gnc_argv2scm (argv_length (restargv2), (const char**)restargv2);

#ifdef GTKHTML_HAVE_GCONF
  {
    if (!gconf_init (restargc, restargv, &error))
      g_error_free (error);
  }
#endif

  gnc_free_argv (restargv);

  /* initialization required for gtkhtml */
  gdk_rgb_init ();    
  gtk_widget_set_default_colormap (gdk_rgb_get_cmap ());
  gtk_widget_set_default_visual (gdk_rgb_get_visual ());

  /* use custom icon */
  fullname = gnome_program_locate_file (gnucash_program,
					GNOME_FILE_DOMAIN_APP_PIXMAP,
					"gnucash-icon.png", TRUE, NULL);
  if (fullname) {
    gtk_window_set_default_icon_from_file (fullname, &error);
    g_free(fullname);
    if (error) {
      PERR ("Could not set default icon: %s", error->message);
      g_error_free (error);
    }
  } else {
    PERR ("Could not locate pixmap file %s", "gnucash-icon.png");
  }

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

char *
gnc_gnome_locate_file (const char *name)
{
  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  fullname = gnome_program_locate_file (gnucash_program,
					GNOME_FILE_DOMAIN_APP_PIXMAP,
					name, TRUE, NULL);
  if (fullname == NULL) {
    PERR ("Could not locate file %s", name);
    return NULL;
  }

  return fullname;
}

void
gnc_gnome_help (const char *file_name, const char *anchor)
{
  GError *error = NULL;

  DEBUG ("Attempting to opening help file %s", file_name);
  if (gnome_help_display (file_name, anchor, &error))
    return;

  g_assert(error != NULL);
  PERR ("%s", error->message);
  g_error_free(error);
}

/********************************************************************\
 * gnc_gnome_get_pixmap                                             *
 *   returns a GnomePixmap widget given a pixmap filename           *
 *                                                                  *
 * Args: none                                                       *
 * Returns: GnomePixmap widget or NULL if there was a problem       *
 \*******************************************************************/
GtkWidget *
gnc_gnome_get_pixmap (const char *name)
{
  GtkWidget *pixmap;
  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  fullname = gnome_program_locate_file (gnucash_program,
					GNOME_FILE_DOMAIN_APP_PIXMAP,
					name, TRUE, NULL);
  if (fullname == NULL) {
    PERR ("Could not locate pixmap file %s", name);
    return NULL;
  }

  DEBUG ("Loading pixmap file %s", fullname);
  pixmap = gnome_pixmap_new_from_file (fullname);
  if (pixmap == NULL) {
    PERR ("Could not load pixmap");
  }
  g_free (fullname);

  return pixmap;
}

/********************************************************************\
 * gnc_gnome_get_gdkpixbuf                                          *
 *   returns a GdkImlibImage object given a pixmap filename         *
 *                                                                  *
 * Args: none                                                       *
 * Returns: GdkPixbuf or NULL if there was a problem                *
 \*******************************************************************/
GdkPixbuf *
gnc_gnome_get_gdkpixbuf (const char *name)
{
  GdkPixbuf *pixbuf;
  GError *error = NULL;
  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  fullname = gnome_program_locate_file (gnucash_program,
					GNOME_FILE_DOMAIN_APP_PIXMAP,
					name, TRUE, NULL);

  if (fullname == NULL) {
    PERR ("Could not locate pixbuf file %s", name);
    return NULL;
  }

  DEBUG ("Loading pixbuf file %s", fullname);
  pixbuf = gdk_pixbuf_new_from_file (fullname, &error);
  if (error != NULL) {
    g_assert (pixbuf == NULL);
    PERR ("Could not load pixbuf: %s", error->message);
    g_error_free (error);
  }
  g_free (fullname);

  return pixbuf;
}


