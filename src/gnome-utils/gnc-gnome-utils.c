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
#include <glib/gi18n.h>
#include <libguile.h>
#include <gconf/gconf.h>
#ifdef HAVE_X11_XLIB_H
# include <X11/Xlib.h>
#endif
#include <libxml/xmlIO.h>

//#include "gnc-html-graph-gog.h"

#include "druid-gconf-setup.h"
#include "gnc-gconf-utils.h"
#include "gnc-gnome-utils.h"
//#include "gnc-html.h"
#include "gnc-engine.h"
#include "gnc-path.h"
#include "gnc-ui.h"
#include "gnc-file.h"
#include "gnc-hooks.h"
#include "gnc-filepath-utils.h"
#include "gnc-menu-extensions.h"
#include "gnc-component-manager.h"
#include "gnc-splash.h"
#include "gnc-window.h"
#include "gnc-icons.h"
#include "dialog-options.h"
#include "dialog-commodity.h"
#include "dialog-totd.h"
#include "gnc-ui-util.h"
#include "gnc-session.h"
#ifdef G_OS_WIN32
#    include "gnc-help-utils.h"
#endif

static QofLogModule log_module = GNC_MOD_GUI;
static GnomeProgram *gnucash_program = NULL;
static int gnome_is_running = FALSE;
static int gnome_is_terminating = FALSE;
static int gnome_is_initialized = FALSE;


#define ACCEL_MAP_NAME "accelerator-map"

static void
gnc_global_options_help_cb (GNCOptionWin *win, gpointer dat)
{
  gnc_gnome_help (HF_HELP, HL_GLOBPREFS);
}

static void
gnc_commodity_help_cb (void)
{
  gnc_gnome_help (HF_HELP, HL_COMMODITY);
}

/* gnc_configure_date_format
 *    sets dateFormat to the current value on the scheme side
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void 
gnc_configure_date_format (void)
{
  char *format_code = gnc_gconf_get_string(GCONF_GENERAL, 
                                           KEY_DATE_FORMAT, NULL);

  QofDateFormat df;

  if (format_code == NULL)
    format_code = g_strdup("locale");
  if (*format_code == '\0') {
    g_free(format_code);
    format_code = g_strdup("locale");
  }

  if (gnc_date_string_to_dateformat(format_code, &df))
  {
    PERR("Incorrect date format code");
    if (format_code != NULL)
      free(format_code);
    return;
  }

  qof_date_format_set(df);

  if (format_code != NULL)
    free(format_code);
}

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
gnc_gnome_locate_file (GnomeFileDomain domain, const char *name)
{
  char *fullname;

  g_return_val_if_fail(name, NULL);
  fullname = gnome_program_locate_file(gnucash_program,
                                       domain, name, TRUE, NULL);
  if (!fullname)
      PERR ("Could not locate file %s", name);
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
  GError *error = NULL;
  gchar *prefix = gnc_path_get_prefix ();
  gchar *pkgsysconfdir = gnc_path_get_pkgsysconfdir ();
  gchar *pkgdatadir = gnc_path_get_pkgdatadir ();
  gchar *pkglibdir = gnc_path_get_pkglibdir ();

  gnc_gtk_add_rc_file();
  gnucash_program = gnome_program_init(
      "gnucash", version, LIBGNOMEUI_MODULE,
      argc, argv,
      GNOME_PARAM_APP_PREFIX, prefix,
      GNOME_PARAM_APP_SYSCONFDIR, pkgsysconfdir,
      GNOME_PARAM_APP_DATADIR, pkgdatadir,
      GNOME_PARAM_APP_LIBDIR, pkglibdir,
      GNOME_PARAM_NONE);
  g_free (prefix);
  g_free (pkgsysconfdir);
  g_free (pkgdatadir);
  g_free (pkglibdir);

#ifdef G_OS_WIN32
  /* workaround for bug #421792 */
  xmlCleanupInputCallbacks();
#endif

  /* initialization required for gtkhtml */
  gtk_widget_set_default_colormap (gdk_rgb_get_colormap ());

  /* use custom icon */
  {
    int idx;
    char *icon_filenames[] = {"gnucash-icon-16x16.png",
                              "gnucash-icon-32x32.png",
                              "gnucash-icon-48x48.png",
                              NULL};
    GList *icons = NULL;
    char *fullname, *name_iter;

    for (idx = 0; icon_filenames[idx] != NULL; idx++) {
      GdkPixbuf *buf = NULL;

      fullname = gnc_gnome_locate_pixmap(icon_filenames[idx]);
      if (fullname == NULL) {
        g_warning("couldn't find icon file [%s]", icon_filenames[idx]);
        continue;
      }
        
      buf = gnc_gnome_get_gdkpixbuf(fullname);
      if (buf == NULL)
      {
        g_warning("error loading image from [%s]", fullname);
        g_free(fullname);
        continue;
      }
      g_free(fullname);
      icons = g_list_append(icons, buf);
    }

    gtk_window_set_default_icon_list(icons);
    g_list_foreach(icons, (GFunc)g_object_unref, NULL);
    g_list_free(icons);
  }

  druid_gconf_install_check_schemas();

  return;
}

#ifndef G_OS_WIN32
void
gnc_gnome_help (const char *file_name, const char *anchor)
{
  GError *error = NULL;

  DEBUG ("Attempting to opening help file %s", file_name);
  if (gnome_help_display (file_name, anchor, &error))
    return;

  g_assert(error != NULL);
  {
    const gchar *message =
      _("GnuCash could not find the files for the help documentation.  "
	"This is likely because the 'gnucash-docs' package is not installed.");
    gnc_error_dialog(NULL, "%s", message);
  }
  PERR ("%s", error->message);
  g_error_free(error);
}

#else /* G_OS_WIN32 */
void
gnc_gnome_help (const char *file_name, const char *anchor)
{
  const gchar * const *lang;
  gchar *pkgdatadir, *fullpath, *found = NULL;
  
  pkgdatadir = gnc_path_get_pkgdatadir ();
  for (lang=g_get_language_names (); *lang; lang++) {
    fullpath = g_build_filename (pkgdatadir, "help", *lang, file_name,
                                 (gchar*) NULL);
    if (g_file_test (fullpath, G_FILE_TEST_IS_REGULAR)) {
      found = g_strdup (fullpath);
      g_free (fullpath);
      break;
    }
    g_free (fullpath);
  }
  g_free (pkgdatadir);

  if (!found) {
    const gchar *message =
      _("GnuCash could not find the files for the help documentation.");
    gnc_error_dialog (NULL, message);
  } else {
    gnc_show_htmlhelp (found, anchor);
  }
  g_free (found);
}
#endif

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

static gboolean
gnc_ui_check_events (gpointer not_used)
{
  QofSession *session;
  gboolean force;

  if (gtk_main_level() != 1)
    return TRUE;

  if (!gnc_current_session_exist())
    return TRUE;
  session = gnc_get_current_session ();

  if (gnc_gui_refresh_suspended ())
    return TRUE;

  if (!qof_session_events_pending (session))
    return TRUE;

  gnc_suspend_gui_refresh ();

  force = qof_session_process_events (session);

  gnc_resume_gui_refresh ();

  if (force)
    gnc_gui_refresh_all ();

  return TRUE;
}

#ifdef HAVE_X11_XLIB_H
static int
gnc_x_error (Display *display, XErrorEvent *error)
{
  if (error->error_code)
  {
    char buf[64];

    XGetErrorText (display, error->error_code, buf, 63);

    g_warning ("X-ERROR **: %s\n  serial %ld error_code %d "
               "request_code %d minor_code %d\n", 
               buf, 
               error->serial, 
               error->error_code, 
               error->request_code,
               error->minor_code);
  }

  return 0;
}
#endif

int
gnc_ui_start_event_loop (void)
{
  guint id;

  gnome_is_running = TRUE;

  id = g_timeout_add_full (G_PRIORITY_DEFAULT_IDLE, 10000, /* 10 secs */
                           gnc_ui_check_events, NULL, NULL);

#ifdef HAVE_X11_XLIB_H
  XSetErrorHandler (gnc_x_error);
#endif

  /* Enter gnome event loop */
  gtk_main ();

  g_source_remove (id);

  gnome_is_running = FALSE;
  gnome_is_terminating = FALSE;

  return 0;
}

GncMainWindow *
gnc_gui_init(void)
{
    static GncMainWindow *main_window;
    gchar *map;

    if (gnome_is_initialized) {
        return main_window;
    }

    if (gnc_gconf_get_bool(GCONF_GENERAL, "show_splash_screen", NULL))
        gnc_gui_init_splash();

    gnome_is_initialized = TRUE;

    gnc_ui_util_init();
    gnc_configure_date_format();

    gnc_gconf_general_register_cb(
        KEY_DATE_FORMAT, (GncGconfGeneralCb)gnc_configure_date_format, NULL);
    gnc_gconf_general_register_any_cb(
        (GncGconfGeneralAnyCb)gnc_gui_refresh_all, NULL);

    gnc_ui_commodity_set_help_callback (gnc_commodity_help_cb);
    gnc_file_set_shutdown_callback (gnc_shutdown);

    gnc_options_dialog_set_global_help_cb (gnc_global_options_help_cb, NULL);

    main_window = gnc_main_window_new ();
    // Bug#350993:
    // gtk_widget_show (GTK_WIDGET (main_window));
    gnc_window_set_progressbar_window (GNC_WINDOW(main_window));

    map = gnc_build_dotgnucash_path(ACCEL_MAP_NAME);
    gtk_accel_map_load(map);
    g_free(map);

    gnc_load_stock_icons();
    gnc_totd_dialog(GTK_WINDOW(main_window), TRUE);

    return main_window;
}

gboolean
gnucash_ui_is_running(void)
{
  return gnome_is_running;
}

static void
gnc_gui_destroy (void)
{
  if (!gnome_is_initialized)
    return;

  gnc_extensions_shutdown ();
}

static void
gnc_gui_shutdown (void)
{
  gchar *map;

  if (gnome_is_running && !gnome_is_terminating)
  {
    gnome_is_terminating = TRUE;

    map = gnc_build_dotgnucash_path(ACCEL_MAP_NAME);
    gtk_accel_map_save(map);
    g_free(map);

    gtk_main_quit();
  }
}

/*  shutdown gnucash.  This function will initiate an orderly
 *  shutdown, and when that has finished it will exit the program.
 */
void
gnc_shutdown (int exit_status)
{
    if (gnucash_ui_is_running()) {
        if (!gnome_is_terminating) {
            if (gnc_file_query_save(FALSE)) {
                gnc_hook_run(HOOK_UI_SHUTDOWN, NULL);
                gnc_gui_shutdown();
            }
        }
    } else {
        gnc_gui_destroy();
        gnc_hook_run(HOOK_SHUTDOWN, NULL);
        gnc_engine_shutdown();
        exit(exit_status);
    }   
}

