/*
 * assistant-gconf-setup.c  -- install gconf keys where they can be found.
 *
 * Copyright (c) 2005 David Hampton <hampton@employees.org>
 * Copyright (c) 2011 Robert Fewell
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

/** @addtogroup Assistants
    @{ */
/** @addtogroup GConfAssistant Setup Assistant for GConf
    @{ */
/** @file assistant-gconf-setup.c
    @brief Check for gconf.  Help user set up if needed.
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>

#include "dialog-utils.h"
#include "assistant-gconf-setup.h"
#include "assistant-utils.h"
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

typedef struct
{
    GtkWidget *dialog;
    GtkWidget *assistant;
    GtkWidget *update_path;
    GtkWidget *install_data;
    GtkWidget *program1;
    GtkWidget *user1;
    GtkWidget *update_text;
    GtkWidget *program2;
    GtkWidget *user2;
    GtkWidget *install_text;
    GtkWidget *finish_page;

} gconf_data;


/********************
 * Declarations
 ********************/
void assistant_gconf_prepare (GtkAssistant *assistant, GtkWidget *page, gconf_data  *data);
void assistant_gconf_cancel (GtkAssistant *gtkassistant, gpointer user_data);
void assistant_gconf_finish (GtkAssistant *assistant, gpointer user_data);

void assistant_gconf_update_cb (GtkToggleButton *button, gpointer user_data);
void assistant_gconf_install_cb (GtkToggleButton *button, gpointer user_data);

void assistant_gconf_update_prep (GtkAssistant *assistant, gpointer user_data);
void assistant_gconf_step_prep (GtkAssistant *assistant, gpointer user_data);
void assistant_gconf_install_prep (GtkAssistant *assistant, gpointer user_data);
void assistant_gconf_finish_prep (GtkAssistant *assistant, gpointer user_data);


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
assistant_gconf_update_path (GError **error)
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
assistant_gconf_install_keys (GError **error)
{
    return g_spawn_command_line_sync(SCRIPT_NAME, NULL, NULL, NULL, error);
}


/********************
 * Update Page
 *******************/

/** This function is called before the Update page is presented to the
 *  user. It gets the active button from the Method page and uses this
 *  to either add the path strings or jump to the install page.
 */
void
assistant_gconf_update_prep (GtkAssistant *assistant, gpointer user_data)
{
    gconf_data *data = user_data;
    GtkTextBuffer *textbuffer;
    GtkWidget *textview;
    gchar *msg;
    gchar *gconfdir = gnc_path_get_gconfdir (TRUE);

    gint num = gtk_assistant_get_current_page (assistant);

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(data->update_path)))
    {
        g_object_set_data(G_OBJECT(assistant), HOW, GINT_TO_POINTER(HOW_UPDATE));

        textview = data->update_text;
        textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
        msg = g_strdup_printf(PATH_STRING1 PATH_STRING2, gconfdir);
        gtk_text_buffer_set_text(textbuffer, msg, -1);
        g_free (gconfdir);
    }
    else
    {
        g_object_set_data(G_OBJECT(assistant), HOW, GINT_TO_POINTER(HOW_INSTALL));
        gtk_assistant_set_current_page (assistant, num + 2);
    }
}


/* Call back for update radio buttons */
void
assistant_gconf_update_cb (GtkToggleButton *button, gpointer user_data)
{
    gconf_data *data = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(data->dialog);

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(data->program1)))
    {
        g_object_set_data(G_OBJECT(assistant), WHO_DOES, GINT_TO_POINTER(WHO_GNUCASH));
    }
    else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(data->user1)))
    {
        g_object_set_data(G_OBJECT(assistant), WHO_DOES, GINT_TO_POINTER(WHO_USER));
    }
    else
    {
        g_object_set_data(G_OBJECT(assistant), WHO_DOES, GINT_TO_POINTER(WHO_ALREADY_DONE));
    }
}


/* Call back for Install radio buttons */
void
assistant_gconf_install_cb (GtkToggleButton *button, gpointer user_data)
{
    gconf_data *data = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(data->dialog);

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(data->program2)))
    {
        g_object_set_data(G_OBJECT(assistant), WHO_DOES, GINT_TO_POINTER(WHO_GNUCASH));
    }
    else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(data->user2)))
    {
        g_object_set_data(G_OBJECT(assistant), WHO_DOES, GINT_TO_POINTER(WHO_USER));
    }
    else
    {
        g_object_set_data(G_OBJECT(assistant), WHO_DOES, GINT_TO_POINTER(WHO_ALREADY_DONE));
    }
}


/********************
 * Step over Page
 ********************/

/** This function is called before the Step over page is presented to the
 *  user. It allows the jumping of the install page.
 */
void
assistant_gconf_step_prep (GtkAssistant *assistant, gpointer user_data)
{
    gint num = gtk_assistant_get_current_page (assistant);
    gtk_assistant_set_current_page (assistant, num + 2);
}


/********************
 * Install Page
 ********************/

/** This function is called before the Install page is presented to the
 *  user. It populates the install_text with the script name.
 */
void
assistant_gconf_install_prep (GtkAssistant *assistant, gpointer user_data)
{
    gconf_data *data = user_data;
    GtkTextBuffer *textbuffer;
    GtkWidget *textview;

    textview = data->install_text;
    textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
    gtk_text_buffer_set_text(textbuffer, SCRIPT_NAME, -1);
}


/********************
 * Finish Page
 ********************/

/** This function is called before the finish page is presented to the
 *  user. It populates the page with text based on previous page options
 *  taken.
 */
void
assistant_gconf_finish_prep (GtkAssistant *assistant, gpointer user_data)
{
    gconf_data *data = user_data;
    gint who, how;
    gchar *text;

    const gchar *pgm_path =
        _("When you click Apply, GnuCash will modify your ~/.gconf.path file "
          "and restart the gconf backend. There will be a short delay before "
          "GnuCash is loaded.");
    const gchar *pgm_install =
        _("When you click Apply, GnuCash will install the gconf data into your "
          "local ~/.gconf file and restart the gconf backend. The %s script "
          "must be found in your search path for this to work correctly.");
    const gchar *user_path =
        _("You have chosen to correct the problem by yourself. When you click "
          "Apply, GnuCash will exit. Please correct the problem and restart "
          "the gconf backend with the command 'gconftool-2 --shutdown' before "
          "restarting GnuCash. If you have not already done so, you can click "
          "the Back button and copy the necessary text from the dialog.");
    const gchar *user_install =
        _("You have chosen to correct the problem by yourself. When you "
          "click Apply, GnuCash will exit. Please run the %s script which "
          "will install the configuration data and restart the gconf backend.");
    const gchar *user_did =
        _("You have already corrected the problem and restarted the gconf "
          "backend with the command 'gconftool-2 --shutdown'. When you click "
          "Apply, there will be a short delay before GnuCash is loaded.");

    who = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(assistant), WHO_DOES));
    switch (who)
    {
    case WHO_ALREADY_DONE:
        gtk_label_set_text(GTK_LABEL(data->finish_page), user_did);
        break;

    case WHO_USER:
        how = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(assistant), HOW));
        if (how == HOW_INSTALL)
        {
            text = g_strdup_printf(user_install, SCRIPT_NAME);
            gtk_label_set_text(GTK_LABEL(data->finish_page), text);
            g_free(text);
        }
        else
        {
            gtk_label_set_text(GTK_LABEL(data->finish_page), user_path);
        }
        break;

    default:
        how = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(assistant), HOW));
        if (how == HOW_INSTALL)
        {
            text = g_strdup_printf(pgm_install, SCRIPT_NAME);
            gtk_label_set_text(GTK_LABEL(data->finish_page), text);
            g_free(text);
        }
        else
        {
            gtk_label_set_text(GTK_LABEL(data->finish_page), pgm_path);
        }
        break;
    }
}


/** This function is called when the Apply button is clicked on the
 *  Finish Page of the assistant.  It determines whether or not there
 *  is any work to be performed by Gnucash, and if so it calls other
 *  functions to carry out the work.
 */
void
assistant_gconf_finish (GtkAssistant *assistant, gpointer user_data)
{
    gconf_data *data = user_data;
    gint value, value2;
    GError *error = NULL;
    gboolean keep_going = TRUE;

    /* What to do... what to do... */
    value = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(assistant), WHO_DOES));
    switch (value)
    {
    case WHO_ALREADY_DONE:
        break;

    case WHO_USER:
        keep_going = FALSE;
        break;

    default:
        value2 = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(assistant), HOW));
        switch (value2)
        {
        case HOW_INSTALL:
            if (!assistant_gconf_install_keys(&error))
            {
                keep_going = FALSE;
                gnc_error_dialog(NULL, "%s", error->message);
                g_error_free(error);
            }
            break;

        default:
            if (!assistant_gconf_update_path(&error))
            {
                keep_going = FALSE;
                gnc_error_dialog(NULL, "%s", error->message);
                g_error_free(error);
            }
            break;
        }
        break;
    }
    /* Hide the Assistant */
    gtk_widget_hide(GTK_WIDGET(data->dialog));

    if (keep_going)
    {
        gtk_main_quit();
    }
    else
    {
        exit(42);
    }
}


/*************************************
 * Assistant Creation and call backs
 ************************************/
void
assistant_gconf_prepare (GtkAssistant  *assistant, GtkWidget *page,
                         gconf_data  *data)
{
    switch (gtk_assistant_get_current_page(assistant))
    {
    case 2:
        /* Current page is update search path */
        assistant_gconf_update_prep(assistant, data);
        break;
    case 3:
        /* Current page is a step page */
        assistant_gconf_step_prep(assistant, data);
        break;
    case 4:
        /* Current page is install page */
        assistant_gconf_install_prep(assistant, data);
        break;
    case 5:
        /* Current page is finish page */
        assistant_gconf_finish_prep(assistant, data);
        break;
    }
}


/** This function is called when the Cancel button is clicked on any
 *  page of the assistant.  It destroys the dialog and kills gnucash.
 */
void
assistant_gconf_cancel (GtkAssistant *gtkassistant,
                        gpointer user_data)
{
    gconf_data *data = user_data;
    gtk_widget_destroy(GTK_WIDGET(data->dialog));
    exit(41);
}


/** This function build and presents the assistant that presents the user
 *  with the two methods of making the gconf schemas visible, and
 *  learns whether gnucash should do the work or the user will do the
 *  work.  This function then blocks in a call to gtk_main(), while
 *  all of the work happens in callback functions.  Once the dialog is
 *  finished, this function kills off any existing gconf daemon so
 *  that the changes will be noticed upon daemon restart.
 */
static GtkWidget *
gnc_gnome_install_gconf_schemas (void)
{
    gconf_data *data;
    GtkBuilder *builder;
    GtkWidget *dialog;
    GError *error = NULL;


    data = g_new0 (gconf_data, 1);
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "textbuffer1");
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "textbuffer2");
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "textbuffer3");
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "textbuffer4");
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "textbuffer5");
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "textbuffer6");
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "textbuffer7");
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "textbuffer8");
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "GConf Setup Assistant");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "GConf Setup Assistant"));
    data->dialog = dialog;

    /* Set the colors for the assistant */
    gnc_assistant_set_colors (GTK_ASSISTANT (data->dialog));

    /* Enable buttons on all pages. */
    gtk_assistant_set_page_complete (GTK_ASSISTANT (dialog),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "start_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (dialog),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "choose_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (dialog),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "update_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (dialog),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "step_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (dialog),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "install_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (dialog),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "finish_page")),
                                     TRUE);

    /* Choose page */
    data->update_path = GTK_WIDGET(gtk_builder_get_object (builder, "update_path"));

    /* Update page */
    data->program1 = GTK_WIDGET(gtk_builder_get_object (builder, "program1"));
    data->user1 = GTK_WIDGET(gtk_builder_get_object (builder, "user1"));
    data->update_text = GTK_WIDGET(gtk_builder_get_object (builder, "update_text"));

    /* Install page */
    data->program2 = GTK_WIDGET(gtk_builder_get_object (builder, "program2"));
    data->user2 = GTK_WIDGET(gtk_builder_get_object (builder, "user2"));
    data->install_text = GTK_WIDGET(gtk_builder_get_object (builder, "install_text"));

    /* Finish page  */
    data->finish_page = GTK_WIDGET(gtk_builder_get_object (builder, "finish_page"));

    gtk_builder_connect_signals(builder, data);
    g_object_unref(G_OBJECT(builder));

    gtk_widget_show_all(dialog);

    /* This won't return until the dialog is finished */
    gtk_main();

    /* Destroy the Assistant dialog */
    gtk_widget_destroy(GTK_WIDGET(data->dialog));

    /* Kill the backend daemon. When it restarts it will find our changes */
    if (!g_spawn_command_line_sync("gconftool-2 --shutdown", NULL, NULL,
                                   NULL, &error))
    {
        gnc_warning_dialog(NULL, "%s", error->message);
        g_error_free(error);
    }

    return dialog;
}


/*  This routine checks to see if GnuCash's gconf schemas are visible
 *  to the user.  The schemas typically should be visible, as rpm and
 *  deb installs will put the schemas in the default system location.
 *  For things like network installs or developers, this function will
 *  present a warning dialog that asks the user whether to setup
 *  gconf, continue without the schemas, or quit.  If the user chooses
 *  to set up the schemas, this function will invoke an assistant to
 *  walk the user through making the schemas visible.
 */
void
assistant_gconf_install_check_schemas (void)
{
    GtkBuilder *builder;
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
        if (!assistant_gconf_update_path (&error))
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

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "assistant-gconf-setup.glade", "GConf Query");

    if (!builder)
    {
        gnc_error_dialog(NULL, "The glade UI files were not found. Your installation is incomplete and cannot be run.");
        exit(-1); /* quit immediately */
    }
    g_assert(builder);
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "GConf Query"));
    g_assert(dialog);
    do
    {
        response = gtk_dialog_run(GTK_DIALOG(dialog));

        switch (response)
        {
        case GTK_RESPONSE_CANCEL:
        default:
            exit(42);
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

    g_object_unref(G_OBJECT(builder));
    gtk_widget_destroy(dialog);
}

/** @} */
/** @} */
