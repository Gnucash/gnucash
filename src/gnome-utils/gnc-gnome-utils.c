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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <libguile.h>
#include <gconf/gconf.h>

#include "gnc-html-graph-gog.h"

#include "argv-list-converters.h"
#include "druid-gconf-setup.h"
#include "gnc-gconf-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"
#include "gnc-engine.h"
#include "gnc-ui.h"

#include <gnc-dir.h>

static QofLogModule log_module = GNC_MOD_GUI;
static GnomeProgram *gnucash_program = NULL;

static const struct poptOption nullPoptTable[] = {
  { NULL, 0, 0, NULL, 0 }
};

char *
gnc_gnome_locate_pixmap (const char *name)
{
  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  fullname = gnome_program_locate_file (gnucash_program,
					GNOME_FILE_DOMAIN_APP_PIXMAP,
					name, TRUE, NULL);
  if (fullname == NULL) {
    PERR ("Could not locate pixmap/pixbuf file %s", name);
    return NULL;
  }

  return fullname;
}

char *
gnc_gnome_locate_data_file (const char *name)
{
  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  fullname = gnome_program_locate_file (gnucash_program,
					GNOME_FILE_DOMAIN_APP_DATADIR,
					name, TRUE, NULL);

  if (fullname == NULL) {
    PERR ("Could not locate file %s", name);
    return NULL;
  }

  return fullname;
}

char *
gnc_gnome_locate_ui_file (const char *name)
{
  char *partial;
  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  partial = g_strdup_printf("ui/%s", name);
  fullname = gnc_gnome_locate_data_file(partial);
  g_free(partial);

  return fullname;
}

static void
gnc_gtk_add_rc_file (void)
{
  const gchar *var;
  gchar *str;

  var = g_get_home_dir ();
  if (var) {
    str = g_build_filename (var, ".gtkrc-2.0.gnucash", (char *)NULL);
    gtk_rc_add_default_file (str);
    g_free (str);
  }
}

void
gnc_gnome_init (int argc, char **argv, const char * version)
{
  char *fullname;
  GError *error = NULL;

  gnc_gtk_add_rc_file();
  gnucash_program = gnome_program_init(
      "gnucash", version, LIBGNOMEUI_MODULE,
      argc, argv, GNOME_PARAM_POPT_TABLE, nullPoptTable,
      GNOME_PROGRAM_STANDARD_PROPERTIES, GNOME_PARAM_NONE);

  /* initialization required for gtkhtml */
  gtk_widget_set_default_colormap (gdk_rgb_get_colormap ());

  /* use custom icon */
  fullname = gnc_gnome_locate_pixmap ("gnucash-icon.png");
  if (fullname) {
    gtk_window_set_default_icon_from_file (fullname, &error);
    g_free(fullname);
    if (error) {
      PERR ("Could not set default icon: %s", error->message);
      g_error_free (error);
    }
  }

  druid_gconf_install_check_schemas();

  return;
}

void
gnc_gnome_shutdown (void)
{
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
 *   returns a GtkWidget given a pixmap filename                    *
 *                                                                  *
 * Args: none                                                       *
 * Returns: GtkWidget or NULL if there was a problem                *
 \*******************************************************************/
GtkWidget *
gnc_gnome_get_pixmap (const char *name)
{
  GtkWidget *pixmap;
  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  fullname = gnc_gnome_locate_pixmap (name);
  if (fullname == NULL)
    return NULL;

  DEBUG ("Loading pixmap file %s", fullname);

  pixmap = gtk_image_new_from_file (fullname);
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

  fullname = gnc_gnome_locate_pixmap (name);
  if (fullname == NULL)
    return NULL;

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


/*  shutdown gnucash.  This function will call the Scheme side of
 *  GnuCash to initiate an orderly shutdown, and when that has
 *  finished it will exit the program.
 */
void
gnc_shutdown (int exit_status)
{
  /*SCM scm_shutdown = gnc_scm_lookup("gnucash bootstrap", "gnc:shutdown");*/
  SCM scm_shutdown = scm_c_eval_string("gnc:shutdown");

  if(scm_procedure_p(scm_shutdown) != SCM_BOOL_F)
  {
    SCM scm_exit_code = scm_long2num(exit_status);    
    scm_call_1(scm_shutdown, scm_exit_code);
  }
  else
  {
    /* Either guile is not running, or for some reason we
       can't find gnc:shutdown. Either way, just exit. */
    g_warning("couldn't find gnc:shutdown -- exiting anyway.");
    exit(exit_status);
  }
}
