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

#include <config.h>

#include <glib/gi18n.h>
#include <libxml/xmlIO.h>

#include "gnc-prefs-utils.h"
#include "gnc-prefs.h"
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
#include "gnc-uri-utils.h"
#include "gnc-session.h"
#include "qofbookslots.h"
#ifdef G_OS_WIN32
#include <windows.h>
#include "gnc-help-utils.h"
#endif
#ifdef MAC_INTEGRATION
#import <Cocoa/Cocoa.h>
#endif

static QofLogModule log_module = GNC_MOD_GUI;
static int gnome_is_running = FALSE;
static int gnome_is_terminating = FALSE;
static int gnome_is_initialized = FALSE;


#define ACCEL_MAP_NAME "accelerator-map"

static void gnc_book_options_help_cb (GNCOptionWin *win, gpointer dat);

static void
gnc_global_options_help_cb (GNCOptionWin *win, gpointer dat)
{
    gnc_gnome_help (HF_HELP, HL_GLOBPREFS);
}

static void
gnc_book_options_help_cb (GNCOptionWin *win, gpointer dat)
{
    gnc_gnome_help (HF_HELP, HL_BOOK_OPTIONS);
}

void
gnc_options_dialog_set_book_options_help_cb (GNCOptionWin *win)
{
    gnc_options_dialog_set_help_cb(win,
                                (GNCOptionWinCallback)gnc_book_options_help_cb,
                                NULL);
}

void
gnc_options_dialog_set_new_book_option_values (GNCOptionDB *odb)
{
    GNCOption *num_source_option;
    GtkWidget *num_source_is_split_action_button;
    gboolean num_source_is_split_action;

    if (!odb) return;
    num_source_is_split_action = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL,
                                                    GNC_PREF_NUM_SOURCE);
    if (num_source_is_split_action)
    {
        num_source_option = gnc_option_db_get_option_by_name(odb,
                                                 OPTION_SECTION_ACCOUNTS,
                                                 OPTION_NAME_NUM_FIELD_SOURCE);
        num_source_is_split_action_button =
                                gnc_option_get_gtk_widget (num_source_option);
        gtk_toggle_button_set_active
                    (GTK_TOGGLE_BUTTON (num_source_is_split_action_button),
                        num_source_is_split_action);
    }
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
    QofDateFormat df = gnc_prefs_get_int(GNC_PREFS_GROUP_GENERAL,
                                         GNC_PREF_DATE_FORMAT);

    /* Only a subset of the qof date formats is currently
     * supported for date entry.
     */
    if (df > QOF_DATE_FORMAT_LOCALE)
    {
        PERR("Incorrect date format");
        return;
    }

    qof_date_format_set(df);
}

/* gnc_configure_date_completion
 *    sets dateCompletion to the current value on the scheme side.
 *    QOF_DATE_COMPLETION_THISYEAR: use current year
 *    QOF_DATE_COMPLETION_SLIDING: use a sliding 12-month window
 *    backmonths 0-11: windows starts this many months before current month
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_date_completion (void)
{
    QofDateCompletion dc = QOF_DATE_COMPLETION_THISYEAR;
    int backmonths = gnc_prefs_get_float(GNC_PREFS_GROUP_GENERAL,
                                         GNC_PREF_DATE_BACKMONTHS);

    if (backmonths < 0)
        backmonths = 0;
    else if (backmonths > 11)
        backmonths = 11;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_DATE_COMPL_SLIDING))
        dc = QOF_DATE_COMPLETION_SLIDING;

    qof_date_completion_set(dc, backmonths);
}

/* This function was copied from GTK3.22 as it was only introduced in
 * version 3.16 */
#if !GTK_CHECK_VERSION(3,16,0)
static void
gtk_css_provider_load_from_resource (GtkCssProvider *css_provider,
                                     const gchar *resource_path)
{
  GFile *file;
  gchar *uri, *escaped;

  g_return_if_fail (GTK_IS_CSS_PROVIDER (css_provider));
  g_return_if_fail (resource_path != NULL);

  escaped = g_uri_escape_string (resource_path,
                  G_URI_RESERVED_CHARS_ALLOWED_IN_PATH, FALSE);
  uri = g_strconcat ("resource://", escaped, NULL);
  g_free (escaped);

  file = g_file_new_for_uri (uri);
  g_free (uri);

  gtk_css_provider_load_from_file (css_provider, file, NULL);

  g_object_unref (file);
}
#endif

void
gnc_add_css_file (void)
{
    GtkCssProvider *provider_user, *provider_app, *provider_fallback;
    GdkDisplay *display;
    GdkScreen *screen;
    const gchar *var;
    GError *error = 0;

    provider_user = gtk_css_provider_new ();
    provider_app = gtk_css_provider_new ();
    provider_fallback = gtk_css_provider_new ();
    display = gdk_display_get_default ();
    screen = gdk_display_get_default_screen (display);

    gtk_style_context_add_provider_for_screen (screen, GTK_STYLE_PROVIDER (provider_fallback), GTK_STYLE_PROVIDER_PRIORITY_FALLBACK);
    gtk_style_context_add_provider_for_screen (screen, GTK_STYLE_PROVIDER (provider_app), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    gtk_style_context_add_provider_for_screen (screen, GTK_STYLE_PROVIDER (provider_user), GTK_STYLE_PROVIDER_PRIORITY_USER);

#if GTK_CHECK_VERSION(3,20,0)
    gtk_css_provider_load_from_resource (provider_app, "/org/gnucash/gnucash-320.css");
#else
    gtk_css_provider_load_from_resource (provider_app, "/org/gnucash/gnucash-310.css");
#endif
    gtk_css_provider_load_from_resource (provider_fallback,  "/org/gnucash/gnucash-fallback-310.css");

    var = gnc_userconfig_dir ();
    if (var)
    {
        gchar *str;
        str = g_build_filename (var, "gtk-3.0.css", (char *)NULL);
        gtk_css_provider_load_from_path (provider_user, str, &error);
        g_free (str);
    }
    g_object_unref (provider_user);
    g_object_unref (provider_app);
    g_object_unref (provider_fallback);
}

#ifdef MAC_INTEGRATION

/* Don't be alarmed if this function looks strange to you: It's
 * written in Objective-C, the native language of the OSX Cocoa
 * toolkit.
 */
void
gnc_gnome_help (const char *dir, const char *detail)
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSString *subdir = [NSString stringWithUTF8String: dir];
    NSString *tag, *subdirectory;
    NSURL *url = NULL;

    if (detail)
        tag  = [NSString stringWithUTF8String: detail];
    else if ([subdir compare: @HF_HELP] == NSOrderedSame)
        tag = @"help";
    else if ([subdir compare: @HF_GUIDE] == NSOrderedSame)
        tag = @"index";
    else
    {
        PWARN("gnc_gnome_help called with unknown subdirectory %s", dir);
        return;
    }

    if (![[NSBundle mainBundle] bundleIdentifier])
    {
        /* If bundleIdentifier is NULL, then we're running from the
         * commandline and must construct a file path to the resource. We can
         * still get the resource path, but it will point to the "bin"
         * directory so we chop that off, break up what's left into pieces,
         * add some more pieces, and put it all back together again. Then,
         * because the gettext way of handling localizations is different from
         * OSX's, we have to figure out which translation to use. */
        NSArray *components = [NSArray arrayWithObjects: @"share", @"doc", @"gnucash-docs", nil ];
        NSString *prefix = [[[NSBundle mainBundle] resourcePath]
                            stringByDeletingLastPathComponent];
        NSArray *prefix_comps = [[prefix pathComponents]
                                 arrayByAddingObjectsFromArray: components];
        NSString *docs_dir = [NSString pathWithComponents: prefix_comps];
        NSArray *languages = [[NSUserDefaults standardUserDefaults]
                              objectForKey: @"AppleLanguages"];
        BOOL dir;
        subdir = [[[subdir lowercaseString] componentsSeparatedByString: @" "]
                  componentsJoinedByString: @"-"];
        if (![[NSFileManager defaultManager] fileExistsAtPath: docs_dir])
        {
            const gchar *message =
                _("GnuCash could not find the files for the help documentation. "
                  "This is likely because the 'gnucash-docs' package is not installed");
            gnc_error_dialog(NULL, "%s at %s", message, [docs_dir UTF8String]);
            [pool release];
            return;
        }
        if ([languages count] > 0)
        {
            NSEnumerator *lang_iter = [languages objectEnumerator];
            NSString *path;
            NSString *this_lang;
            while ((this_lang = [lang_iter nextObject]))
            {
                NSArray *elements;
                unsigned int paths;
                NSString *completed_path = [NSString alloc];
                this_lang = [this_lang stringByTrimmingCharactersInSet:
                             [NSCharacterSet characterSetWithCharactersInString:
                              @"\""]];
                elements = [this_lang componentsSeparatedByString: @"-"];
                this_lang = [elements objectAtIndex: 0];
                path = [docs_dir stringByAppendingPathComponent: this_lang];
                paths = [path completePathIntoString: &completed_path
                         caseSensitive: FALSE
                         matchesIntoArray: NULL filterTypes: NULL];
                if (paths > 1 &&
                    [[NSFileManager defaultManager]
                     fileExistsAtPath: completed_path
                     isDirectory: &dir])
                    if (dir)
                    {
                        @try
                        {
                            url = [NSURL fileURLWithPath:
                                   [[[completed_path
                                      stringByAppendingPathComponent: subdir]
                                     stringByAppendingPathComponent: tag]
                                    stringByAppendingPathExtension: @"html"]];
                        }
                        @catch (NSException *e)
                        {
                            PWARN("fileURLWithPath threw %s: %s",
                                  [[e name] UTF8String], [[e reason] UTF8String]);
                            return;
                        }
                        break;
                    }
                if ([this_lang compare: @"en"] == NSOrderedSame)
                    break; /* Special case, forces use of "C" locale */
            }
        }
        if (!url)
        {
            @try
            {
                url = [NSURL
                       fileURLWithPath: [[[[docs_dir
                                            stringByAppendingPathComponent: @"C"]
                                           stringByAppendingPathComponent: subdir]
                                          stringByAppendingPathComponent: tag]
                                         stringByAppendingPathExtension: @"html"]];
            }
            @catch (NSException *e)
            {
                PWARN("fileURLWithPath threw %s: %s",
                      [[e name] UTF8String], [[e reason] UTF8String]);
                return;
            }
        }
    }
    /* It's a lot easier in a bundle! OSX finds the best translation for us. */
    else
    {
        @try
        {
            url = [NSURL fileURLWithPath: [[NSBundle mainBundle]
                                           pathForResource: tag
                                           ofType: @"html"
                                           inDirectory: subdir ]];
        }
        @catch (NSException *e)
        {
            PWARN("fileURLWithPath threw %s: %s",
                  [[e name] UTF8String], [[e reason] UTF8String]);
            return;
        }
    }
    /* Now just open the URL in the default app for opening URLs */
    if (url)
        [[NSWorkspace sharedWorkspace] openURL: url];
    else
    {
        const gchar *message =
            _("GnuCash could not find the files for the help documentation. "
              "This is likely because the 'gnucash-docs' package is not installed.");
        gnc_error_dialog(NULL, "%s", message);
    }
    [pool release];
}
#elif defined G_OS_WIN32 /* G_OS_WIN32 */
void
gnc_gnome_help (const char *file_name, const char *anchor)
{
    const gchar * const *lang;
    gchar *pkgdatadir, *fullpath, *found = NULL;

    pkgdatadir = gnc_path_get_pkgdatadir ();
    for (lang = g_get_language_names (); *lang; lang++)
    {
        fullpath = g_build_filename (pkgdatadir, "help", *lang, file_name,
                                     (gchar*) NULL);
        if (g_file_test (fullpath, G_FILE_TEST_IS_REGULAR))
        {
            found = g_strdup (fullpath);
            g_free (fullpath);
            break;
        }
        g_free (fullpath);
    }
    g_free (pkgdatadir);

    if (!found)
    {
        const gchar *message =
            _("GnuCash could not find the files for the help documentation.");
        gnc_error_dialog (NULL, message);
    }
    else
    {
        gnc_show_htmlhelp (found, anchor);
    }
    g_free (found);
}
#else
void
gnc_gnome_help (const char *file_name, const char *anchor)
{
    GError *error = NULL;
    gchar *uri = NULL;
    gboolean success;

    if (anchor)
        uri = g_strconcat ("ghelp:", file_name, "?", anchor, NULL);
    else
        uri = g_strconcat ("ghelp:", file_name, NULL);

    DEBUG ("Attempting to opening help uri %s", uri);
#if GTK_CHECK_VERSION(3,22,0)
    success = gtk_show_uri_on_window (NULL, uri, gtk_get_current_event_time (), &error);
#else
    success = gtk_show_uri (NULL, uri, gtk_get_current_event_time (), &error);
#endif
    g_free (uri);
    if (success)
        return;

    g_assert(error != NULL);
    {
        const gchar *message =
            _("GnuCash could not find the files for the help documentation. "
              "This is likely because the 'gnucash-docs' package is not installed.");
        gnc_error_dialog(NULL, "%s", message);
    }
    PERR ("%s", error->message);
    g_error_free(error);
}


#endif

#ifdef MAC_INTEGRATION

/* Don't be alarmed if this function looks strange to you: It's
 * written in Objective-C, the native language of the OSX Cocoa
 * toolkit.
 */
void
gnc_launch_assoc (const char *uri)
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSString *uri_str = [NSString stringWithUTF8String: uri];
    NSURL *url = [[[NSURL alloc] initWithString: uri_str] autorelease];
    const gchar *message =
        _("GnuCash could not find the associated file.");

    if (url)
    {
        [[NSWorkspace sharedWorkspace] openURL: url];
        [pool release];
        return;
    }

    gnc_error_dialog(NULL, "%s", message);

    [pool release];
    return;
}
#elif defined G_OS_WIN32 /* G_OS_WIN32 */
void
gnc_launch_assoc (const char *uri)
{
    wchar_t *winuri = NULL;
    /* ShellExecuteW open doesn't decode http escapes if it's passed a
     * file URI so we have to do it. */
    if (gnc_uri_is_file_uri (uri))
    {
        gchar *uri_ue = g_uri_unescape_string (uri, NULL);
        gchar *filename = gnc_uri_get_path (uri_ue);
        winuri = (wchar_t *)g_utf8_to_utf16(filename, -1, NULL, NULL, NULL);
        g_free (filename);
        g_free (uri_ue);
    }
    else
        winuri = (wchar_t *)g_utf8_to_utf16(uri, -1, NULL, NULL, NULL);

    if (winuri)
    {
        wchar_t *wincmd = (wchar_t *)g_utf8_to_utf16("open", -1,
                                 NULL, NULL, NULL);
        if ((INT_PTR)ShellExecuteW(NULL, wincmd, winuri,
                       NULL, NULL, SW_SHOWNORMAL) <= 32)
        {
            const gchar *message =
            _("GnuCash could not find the associated file");
            gnc_error_dialog(NULL, "%s: %s", message, uri);
        }
        g_free (wincmd);
        g_free (winuri);
    }
}

#else
void
gnc_launch_assoc (const char *uri)
{
    GError *error = NULL;
    gboolean success;

    if (!uri)
        return;

    DEBUG ("Attempting to open uri %s", uri);
#if GTK_CHECK_VERSION(3,22,0)
    success = gtk_show_uri_on_window (NULL, uri, gtk_get_current_event_time (), &error);
#else
    success = gtk_show_uri (NULL, uri, gtk_get_current_event_time (), &error);
#endif
    if (success)
        return;

    g_assert(error != NULL);
    {
        const gchar *message =
            _("GnuCash could not open the associated URI:");
        gnc_error_dialog(NULL, "%s\n%s", message, uri);
    }
    PERR ("%s", error->message);
    g_error_free(error);
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

    fullname = gnc_filepath_locate_pixmap (name);
    if (fullname == NULL)
        return NULL;

    DEBUG ("Loading pixmap file %s", fullname);

    pixmap = gtk_image_new_from_file (fullname);
    if (pixmap == NULL)
    {
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

    fullname = gnc_filepath_locate_pixmap (name);
    if (fullname == NULL)
        return NULL;

    DEBUG ("Loading pixbuf file %s", fullname);
    pixbuf = gdk_pixbuf_new_from_file (fullname, &error);
    if (error != NULL)
    {
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


int
gnc_ui_start_event_loop (void)
{
    guint id;

    gnome_is_running = TRUE;

    id = g_timeout_add_full (G_PRIORITY_DEFAULT_IDLE, 10000, /* 10 secs */
                             gnc_ui_check_events, NULL, NULL);

    scm_call_1(scm_c_eval_string("gnc:set-ui-status"), SCM_BOOL_T);

    /* Enter gnome event loop */
    gtk_main ();

    g_source_remove (id);

    scm_call_1(scm_c_eval_string("gnc:set-ui-status"), SCM_BOOL_F);

    gnome_is_running = FALSE;
    gnome_is_terminating = FALSE;

    return 0;
}

GncMainWindow *
gnc_gui_init(void)
{
    static GncMainWindow *main_window;
    gchar *map;
#ifdef MAC_INTEGRATION
    gchar *data_dir;
#endif

    ENTER ("");

    if (gnome_is_initialized)
        return main_window;

    /* use custom icon */
    gnc_load_app_icons();
    gtk_window_set_default_icon_name(GNC_ICON_APP);

    g_set_application_name(PACKAGE_NAME);

    gnc_prefs_init();
    gnc_show_splash_screen();

    gnome_is_initialized = TRUE;

    gnc_ui_util_init();
    gnc_configure_date_format();
    gnc_configure_date_completion();

    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_DATE_FORMAT,
                           gnc_configure_date_format,
                           NULL);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_DATE_COMPL_THISYEAR,
                           gnc_configure_date_completion,
                           NULL);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_DATE_COMPL_SLIDING,
                           gnc_configure_date_completion,
                           NULL);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_DATE_BACKMONTHS,
                           gnc_configure_date_completion,
                           NULL);
    gnc_prefs_register_group_cb (GNC_PREFS_GROUP_GENERAL,
                                gnc_gui_refresh_all,
                                NULL);

    gnc_ui_commodity_set_help_callback (gnc_commodity_help_cb);
    gnc_file_set_shutdown_callback (gnc_shutdown);

    gnc_options_dialog_set_global_help_cb (gnc_global_options_help_cb, NULL);

    main_window = gnc_main_window_new ();
    // Bug#350993:
    // gtk_widget_show (GTK_WIDGET (main_window));
    gnc_window_set_progressbar_window (GNC_WINDOW(main_window));

#ifdef MAC_INTEGRATION
    map = gnc_build_userdata_path(ACCEL_MAP_NAME);
    if (!g_file_test (map, G_FILE_TEST_EXISTS))
    {
        g_free (map);
        data_dir = gnc_path_get_pkgdatadir();
        map = g_build_filename(data_dir, "ui", "osx_accel_map", NULL);
        g_free(data_dir);
    }
#else
    map = gnc_build_userdata_path(ACCEL_MAP_NAME);
#endif /* MAC_INTEGRATION */
    gtk_accel_map_load(map);
    g_free(map);

    /* Load css configuration file */
    gnc_add_css_file ();

    gnc_totd_dialog (gnc_get_splash_screen (), TRUE);

    LEAVE ("");
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
        map = gnc_build_userdata_path(ACCEL_MAP_NAME);
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
    if (gnucash_ui_is_running())
    {
        if (!gnome_is_terminating)
        {
            if (gnc_file_query_save (gnc_ui_get_main_window (NULL), FALSE))
            {
                gnc_hook_run(HOOK_UI_SHUTDOWN, NULL);
                gnc_gui_shutdown();
            }
        }
    }
    else
    {
        gnc_gui_destroy();
        gnc_hook_run(HOOK_SHUTDOWN, NULL);
        gnc_engine_shutdown();
        exit(exit_status);
    }
}

