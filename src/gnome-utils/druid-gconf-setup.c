/*
 * druid-gconf-setup.c  -- install gconf keys where they can be found.
 *
 * Copyright (c) 2005 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup Druids
    @{ */
/** @addtogroup GConfDruid Setup Druid for GConf
    @{ */
/** @file druid-gconf-setup.c
    @brief Check for gconf.  Help user set up if needed.
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>

#include "dialog-utils.h"
#include "druid-gconf-setup.h"
#include "druid-utils.h"
#include "gnc-path.h"
#include "gnc-gconf-utils.h"
#include "gnc-gui-query.h"
#include "gnc-gnome-utils.h"
#include "gnc-ui.h"

#define WHO_DOES		"who_does"
#define WHO_GNUCASH		1
#define WHO_USER		2
#define WHO_ALREADY_DONE	3

#define HOW			"how"
#define HOW_UPDATE		1
#define HOW_INSTALL		2

#define SCRIPT_NAME "update-gnucash-gconf"

#define PATH_STRING1 "xml:readwrite:$(HOME)/.gconf\n"
#define PATH_STRING2 "xml:readonly:%s\n"


/********************
 * Declarations
 ********************/


gboolean druid_gconf_delete_event (GtkWidget *window, GdkEvent *event, gpointer user_data);
void     druid_gconf_cancel (GnomeDruid *druid, gpointer user_data);
void     druid_gconf_choose_page_prepare (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
gboolean druid_gconf_choose_page_next (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
void     druid_gconf_update_page_prepare (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
gboolean druid_gconf_update_page_next (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
void     druid_gconf_install_page_prepare (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
gboolean druid_gconf_install_page_next (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
gboolean druid_gconf_install_page_back (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
void     druid_gconf_finish_page_prepare (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
gboolean druid_gconf_finish_page_back (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);
void     druid_gconf_finish_page_finish (GnomeDruidPage *druidpage, GnomeDruid *druid, gpointer user_data);


/********************
 * Work Functions
 ********************/


/** This function is called to create/update the users ~/.gconf.path
 *  file to point at the location of the gnucash schema files.  It
 *  should add two lines to the file.  The first points at the user's
 *  local .gconf directory and the second points at gnucash.  If this
 *  isn't done then all the gnucash keys end up being interpreted as
 *  read-only keys and the user isn't allowed to change anything.
 *
 *  This function first checks to file to see if the ~/.gconf line
 *  already exists in the path file.  If so, it won't add it a second
 *  time.  The function then adds the one or two needed lines to the
 *  end of the file.  Any errors are reported to the caller.
 *
 *  @param error This argument points to a location where error
 *  information can be stored.  It is updated if there is a problem
 *  executing the command.
 *
 *  @return This functions returns TRUE if all the steps needed to
 *  update the users ~/.gconf.path file were able to complete
 *  successfully. It returns FALSE otherwise.
 */
static gboolean
druid_gconf_update_path (GError **error)
{
    gchar *path_filename, *data_filename;
    gchar *contents, **lines, *line;
    gboolean found_user_dir = FALSE;
    FILE *output;
    gchar *gconfdir;

    data_filename = g_build_filename(g_get_home_dir(), ".gconf", (char *)NULL);
    path_filename = g_build_filename(g_get_home_dir(), ".gconf.path", (char *)NULL);
    if (g_file_test(path_filename, G_FILE_TEST_EXISTS))
    {
        if (!g_file_get_contents(path_filename, &contents, NULL, error))
        {
            g_free(path_filename);
            g_free(data_filename);
            return FALSE;
        }

        lines = g_strsplit_set(contents, "\r\n", -1);
        for (line = *lines; line; line++)
        {
            if (line[0] == '#')
                continue;
            if ((strstr(line, "$(HOME)/.gconf") == 0) ||
                    (strstr(line, "~/.gconf") == 0) ||
                    (strstr(line, data_filename)))
            {
                found_user_dir = TRUE;
                break;
            }
        }
        g_strfreev(lines);
    }

    output = g_fopen(path_filename, "a");
    if (output == NULL)
    {
        *error = g_error_new (G_FILE_ERROR,
                              g_file_error_from_errno(errno),
                              "Error opening file %s for writing.",
                              path_filename);
        g_free(path_filename);
        g_free(data_filename);
        return FALSE;
    }

    fprintf(output, "\n######## The following lines were added by GnuCash. ########\n");
    if (!found_user_dir)
        fprintf(output, PATH_STRING1);
    gconfdir = gnc_path_get_gconfdir (TRUE);
    fprintf(output, PATH_STRING2, gconfdir);
    g_free (gconfdir);
    fprintf(output,   "############## End of lines added by GnuCash. ##############\n");
    if (fclose(output) != 0)
    {
        *error = g_error_new (G_FILE_ERROR,
                              g_file_error_from_errno(errno),
                              "Error closing file %s.",
                              path_filename);
        g_free(path_filename);
        g_free(data_filename);
        return  FALSE;
    }

    g_free(path_filename);
    g_free(data_filename);
    return TRUE;
}


/** This function is called to install the gnucash gconf schemas into
 *  the users local .goncf directory.  It spawns a process to run a
 *  shell script that is installed with gnucash.  Any errors are
 *  reported to the caller.
 *
 *  @param error This argument points to a location where error
 *  information can be stored.  It is updated if there is a problem
 *  executing the command.
 *
 *  @return This functions returns TRUE if the command completed
 *  successfully, FALSE otherwise.  Note that this is based on whether
 *  the script could be found, the script's exit code, etc., not on
 *  whether any individual command in the script was successful.
 */
static gboolean
druid_gconf_install_keys (GError **error)
{
    return g_spawn_command_line_sync(SCRIPT_NAME, NULL, NULL, NULL, error);
}

/********************
 * Common Callbacks
 ********************/


/** This function is called when the window manager close button is
 *  clicked on any page of the druid.  It destroys the dialog and
 *  kills gnucash.
 */
gboolean
druid_gconf_delete_event (GtkWidget *window,
                          GdkEvent *event,
                          gpointer user_data)
{
    gtk_widget_destroy(GTK_WIDGET(window));
    exit(40);
}


/** This function is called when the Cancel button is clicked on any
 *  page of the druid.  It destroys the dialog and kills gnucash.
 */
void
druid_gconf_cancel (GnomeDruid *druid,
                    gpointer user_data)
{
    GtkWidget *window;

    window = gnc_glade_lookup_widget(GTK_WIDGET(druid), "GConf Install Druid");
    gtk_widget_destroy(GTK_WIDGET(window));
    exit(41);
}


/********************
 * Choose Page
 ********************/


/** This function is called before the Choose page is presented to the
 *  user.  Its sole purpose is to change the background color of the
 *  GtkTextView widgets to match the color of the druid.
 */
void
druid_gconf_choose_page_prepare (GnomeDruidPage *druidpage,
                                 GnomeDruid *druid,
                                 gpointer user_data)
{
}


/** This function is called when the Next button is clicked on the
 *  Choose page of the druid.  It save the user selection on the
 *  dialog widget, and uses that selection to determines whether to go
 *  to the the "Update Path" or "Install" page.
 */
gboolean
druid_gconf_choose_page_next (GnomeDruidPage *druidpage,
                              GnomeDruid *druid,
                              gpointer user_data)
{
    GtkWidget *page, *button;

    button = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "update_path");
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button)))
    {
        page = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "update_page");
        g_object_set_data(G_OBJECT(druid), HOW, GINT_TO_POINTER(HOW_UPDATE));
    }
    else
    {
        page = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "install_page");
        g_object_set_data(G_OBJECT(druid), HOW, GINT_TO_POINTER(HOW_INSTALL));
    }

    gnome_druid_set_page(druid, GNOME_DRUID_PAGE(page));
    return TRUE;
}


/********************
 * Update Page
 ********************/


/** This function is called before the Choose page is presented to the
 *  user.  Its changes the background color of the GtkTextView widgets
 *  to match the color of the druid, and fills in the text of one of
 *  the textview widgets based upon the installed path of gnucash.
 */
void
druid_gconf_update_page_prepare (GnomeDruidPage *druidpage,
                                 GnomeDruid *druid,
                                 gpointer user_data)
{
    GtkTextBuffer *textbuffer;
    GtkWidget *textview;
    gchar *msg;
    gchar *gconfdir = gnc_path_get_gconfdir (TRUE);

    textview = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "update_text");
    textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
    msg = g_strdup_printf(PATH_STRING1 PATH_STRING2, gconfdir);
    gtk_text_buffer_set_text(textbuffer, msg, -1);
    g_free (gconfdir);
}


/** This function is called when the Next button is clicked on the
 *  Update Path page of the druid.  It save the user selection on the
 *  dialog widget, and skips to the Finish page.
 */
gboolean
druid_gconf_update_page_next (GnomeDruidPage *druidpage,
                              GnomeDruid *druid,
                              gpointer user_data)
{
    GtkWidget *page, *button1, *button2;

    button1 = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "program1");
    button2 = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "user1");
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button1)))
    {
        g_object_set_data(G_OBJECT(druid), WHO_DOES, GINT_TO_POINTER(WHO_GNUCASH));
    }
    else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button2)))
    {
        g_object_set_data(G_OBJECT(druid), WHO_DOES, GINT_TO_POINTER(WHO_USER));
    }
    else
    {
        g_object_set_data(G_OBJECT(druid), WHO_DOES, GINT_TO_POINTER(WHO_ALREADY_DONE));
    }

    page = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "finish_page");
    gnome_druid_set_page(druid, GNOME_DRUID_PAGE(page));
    return TRUE;
}

/********************
 * Install Page
 ********************/

/** This function is called before the Install page is presented to the
 *  user.  Its sole purpose is to change the background color of the
 *  GtkTextView widgets to match the color of the druid.
 */
void
druid_gconf_install_page_prepare (GnomeDruidPage *druidpage,
                                  GnomeDruid *druid,
                                  gpointer user_data)
{
    GtkTextBuffer *textbuffer;
    GtkWidget *textview;

    textview = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "install_text");
    textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
    gtk_text_buffer_set_text(textbuffer, SCRIPT_NAME, -1);
}


/** This function is called when the Next button is clicked on the
 *  Install page of the druid.  It save the user selection on the
 *  dialog widget, and moves to the Finish page.
 */
gboolean
druid_gconf_install_page_next (GnomeDruidPage *druidpage,
                               GnomeDruid *druid,
                               gpointer user_data)
{
    GtkWidget *page, *button1, *button2;

    button1 = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "program2");
    button2 = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "user2");
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button1)))
    {
        g_object_set_data(G_OBJECT(druid), WHO_DOES, GINT_TO_POINTER(WHO_GNUCASH));
    }
    else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button2)))
    {
        g_object_set_data(G_OBJECT(druid), WHO_DOES, GINT_TO_POINTER(WHO_USER));
    }
    else
    {
        g_object_set_data(G_OBJECT(druid), WHO_DOES, GINT_TO_POINTER(WHO_ALREADY_DONE));
    }

    page = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "finish_page");
    gnome_druid_set_page(druid, GNOME_DRUID_PAGE(page));
    return TRUE;
}


/** This function is called when the Back button is clicked on the
 *  Install page of the druid.  It skips back to the Choose page.
 */
gboolean
druid_gconf_install_page_back (GnomeDruidPage *druidpage,
                               GnomeDruid *druid,
                               gpointer user_data)
{
    GtkWidget *page;

    page = gnc_glade_lookup_widget(GTK_WIDGET(druidpage), "choose_page");
    gnome_druid_set_page(druid, GNOME_DRUID_PAGE(page));
    return TRUE;
}


/********************
 * Finish Page
 ********************/


/** This function is called before the Finish page is presented to the
 *  user.  Its determines which of four messages will be presented to
 *  the user based upon their previous selections.
 */
void
druid_gconf_finish_page_prepare (GnomeDruidPage *druidpage,
                                 GnomeDruid *druid,
                                 gpointer user_data)
{
    gint who, how;
    gchar *text;
    const gchar *pgm_path =
        _("When you click Apply, GnuCash will modify your ~/.gconf.path file "
          "and restart the gconf backend.");
    const gchar *pgm_install =
        _("When you click Apply, GnuCash will install the gconf data into your "
          "local ~/.gconf file and restart the gconf backend.  The %s script "
          "must be found in your search path for this to work correctly.");
    const gchar *user_path =
        _("You have chosen to correct the problem by yourself.  When you click "
          "Apply, GnuCash will exit.  Please correct the problem and restart "
          "the gconf backend with the command 'gconftool-2 --shutdown' before "
          "restarting GnuCash.  If you have not already done so, you can click "
          "the Back button and copy the necessary text from the dialog.");
    const gchar *user_install =
        _("You have chosen to correct the problem by yourself.  When you "
          "click Apply, GnuCash will exit.  Please run the %s script which "
          "will install the configuration data and restart the gconf backend.");
    const gchar *user_did =
        _("You have already corrected the problem and restarted the gconf "
          "backend with the command 'gconftool-2 --shutdown'.  When you click "
          "Apply, GnuCash will continue loading.");

    who = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(druid), WHO_DOES));
    switch (who)
    {
    case WHO_ALREADY_DONE:
        gnome_druid_page_edge_set_text(GNOME_DRUID_PAGE_EDGE(druidpage),
                                       user_did);
        break;

    case WHO_USER:
        how = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(druid), HOW));
        if (how == HOW_INSTALL)
        {
            text = g_strdup_printf(user_install, SCRIPT_NAME);
            gnome_druid_page_edge_set_text(GNOME_DRUID_PAGE_EDGE(druidpage), text);
            g_free(text);
        }
        else
        {
            gnome_druid_page_edge_set_text(GNOME_DRUID_PAGE_EDGE(druidpage),
                                           user_path);
        }
        break;

    default:
        how = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(druid), HOW));
        if (how == HOW_INSTALL)
        {
            text = g_strdup_printf(pgm_install, SCRIPT_NAME);
            gnome_druid_page_edge_set_text(GNOME_DRUID_PAGE_EDGE(druidpage), text);
            g_free(text);
        }
        else
        {
            gnome_druid_page_edge_set_text(GNOME_DRUID_PAGE_EDGE(druidpage),
                                           pgm_path);
        }
        break;
    }
}


/** This function is called when the Back button is clicked on the
 *  Finish page of the druid.  It determines whether to go back the
 *  the "Update Path" or "Install" pages.
 */
gboolean
druid_gconf_finish_page_back (GnomeDruidPage *druidpage,
                              GnomeDruid *druid,
                              gpointer user_data)
{
    return druid_gconf_choose_page_next(druidpage, druid, user_data);
}


/** This function is called when the Apply button is clicked on the
 *  Finish Page end of the druid.  It determines whether or not there
 *  is any work to be performed by Gnucash, and if so it calls other
 *  functions to carry out the work.
 */
void
druid_gconf_finish_page_finish (GnomeDruidPage *druidpage,
                                GnomeDruid *druid,
                                gpointer user_data)
{
    GtkWidget *window;
    gint value, value2;
    GError *error = NULL;
    gboolean keep_going = TRUE;

    /* What to do... what to do... */
    value = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(druid), WHO_DOES));
    switch (value)
    {
    case WHO_ALREADY_DONE:
        break;

    case WHO_USER:
        keep_going = FALSE;
        break;

    default:
        value2 = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(druid), HOW));
        switch (value2)
        {
        case HOW_INSTALL:
            if (!druid_gconf_install_keys(&error))
            {
                keep_going = FALSE;
                gnc_error_dialog(NULL, "%s", error->message);
                g_error_free(error);
            }
            break;

        default:
            if (!druid_gconf_update_path(&error))
            {
                keep_going = FALSE;
                gnc_error_dialog(NULL, "%s", error->message);
                g_error_free(error);
            }
            break;
        }
        break;
    }

    window = gnc_glade_lookup_widget(GTK_WIDGET(druid), "GConf Install Druid");
    gtk_widget_destroy(GTK_WIDGET(window));
    if (keep_going)
    {
        gtk_main_quit();
    }
    else
    {
        exit(42);
    }
}


/********************
 * Druid Creation
 ********************/

static void
druid_gconf_fix_textview_color (GtkWidget *window)
{
    GdkColor *color;
    GtkWidget *widget;
    gint i;
    const gchar *names[] =
    {
        "textview1",
        "textview2",
        "textview3",
        "textview4",
        "textview5",
        "textview6",
        NULL
    };

    widget = gnc_glade_lookup_widget(window, "choose_page");
    color = &GNOME_DRUID_PAGE_STANDARD(widget)->contents_background;

    for (i = 0; names[i]; i++)
    {
        widget = gnc_glade_lookup_widget(widget, names[i]);
        gtk_widget_modify_base(widget, GTK_STATE_INSENSITIVE, color);
    }
}

/** This function build and presents the druid that presents the user
 *  with the two methods of making the gconf schemas visible, and
 *  learns whether gnucash should do the work or the user will do the
 *  work.  This function then blocks in a call to gtk_main(), while
 *  all of the work happens in callback functions.  Once the dialog is
 *  finished, this function kills off any existing gconf daemon so
 *  that the changes will be noticed upon daemon restart.
 */
static void
gnc_gnome_install_gconf_schemas (void)
{
    GladeXML *xml;
    GtkWidget *window;
    GError *error = NULL;

    xml = gnc_glade_xml_new ("druid-gconf-setup.glade", "GConf Install Druid");
    glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, NULL);
    window = glade_xml_get_widget (xml, "GConf Install Druid");
    druid_gconf_fix_textview_color(window);
    gtk_widget_show_all(window);

    /* This won't return until the dialog is finished */
    gtk_main();

    /* Kill the backend daemon. When it restarts it will find our changes */
    if (!g_spawn_command_line_sync("gconftool-2 --shutdown", NULL, NULL,
                                   NULL, &error))
    {
        gnc_warning_dialog(NULL, "%s", error->message);
        g_error_free(error);
    }
}


/*  This routine checks to see if GnuCash's gconf schemas are visible
 *  to the user.  The schemas typically should be visible, as rpm and
 *  deb installs will put the schemas in the default system location.
 *  For things like network installs or developers, this function will
 *  present a warning dialog that asks the user whether to setup
 *  gconf, continue without the schemas, or quit.  If the user chooses
 *  to set up the schemas, this function will invoke a druid to walk
 *  the user through making the schemas visible.
 */
void
druid_gconf_install_check_schemas (void)
{
    GladeXML *xml;
    GtkWidget *dialog;
    gboolean done = FALSE;
    gint response;

    if (gnc_gconf_schemas_found())
    {
        gnc_gconf_unset_dir(GCONF_WARNINGS_TEMP, NULL);
        return;
    }

#ifdef G_OS_WIN32
    {
        /* automatically update the search path on windows */
        GError *error = NULL;
        if (!druid_gconf_update_path (&error))
        {
            gnc_error_dialog (NULL, error->message);
            g_error_free (error);
            exit(42);
        }
        else
        {
            if (!g_spawn_command_line_sync("gconftool-2 --shutdown", NULL, NULL,
                                           NULL, &error))
            {
                gnc_warning_dialog(NULL, error->message);
                g_error_free(error);
            }
            return;
        }
    }
#endif /* G_OS_WIN32 */

    xml = gnc_glade_xml_new ("druid-gconf-setup.glade", "GConf Query");
    dialog = glade_xml_get_widget (xml, "GConf Query");
    do
    {
        response = gtk_dialog_run(GTK_DIALOG(dialog));

        switch (response)
        {
        case GTK_RESPONSE_CANCEL:
        default:
            gnc_shutdown(42);
            /* never returns */

        case GTK_RESPONSE_NO:
            /* User wants to run without setting up gconf */
            done = TRUE;
            break;

        case GTK_RESPONSE_ACCEPT:
            gtk_widget_hide(dialog);
            gnc_gnome_install_gconf_schemas();
            done = TRUE;
            break;

        case GTK_RESPONSE_HELP:
            gnc_gnome_help(HF_HELP, HL_GCONF);
            break;
        }
    }
    while (!done);

    gtk_widget_destroy(dialog);
}

/** @} */
/** @} */
