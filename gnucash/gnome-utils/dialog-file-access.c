/********************************************************************\
 * dialog-file-access.c -- dialog for opening a file or making a    *
 *                        connection to a libdbi database           *
 *                                                                  *
 * Copyright (C) 2009 Phil Longstaff (plongstaff@rogers.com)        *
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
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-uri-utils.h"
#include "dialog-utils.h"
#include "dialog-file-access.h"
#include "gnc-file.h"
#include "gnc-plugin-file-history.h"
#include "gnc-session.h"

static QofLogModule log_module = GNC_MOD_GUI;

#define DEFAULT_HOST "localhost"
#define DEFAULT_DATABASE "gnucash"
#define FILE_ACCESS_OPEN    0
#define FILE_ACCESS_SAVE_AS 1
#define FILE_ACCESS_EXPORT  2

typedef struct FileAccessWindow
{
    /* Parts of the dialog */
    int type;

    GtkWidget           *dialog;
    GtkWidget           *frame_file;
    GtkWidget           *frame_database;
    GtkWidget           *readonly_checkbutton;
    GtkFileChooser      *fileChooser;
    gchar               *starting_dir;
    GtkComboBoxText     *cb_uri_type;
    GtkEntry            *tf_host;
    GtkEntry            *tf_database;
    GtkEntry            *tf_username;
    GtkEntry            *tf_password;
} FileAccessWindow;

void gnc_ui_file_access_file_activated_cb( GtkFileChooser *chooser,
        FileAccessWindow *faw );
void gnc_ui_file_access_response_cb( GtkDialog *, gint, GtkDialog * );
static void cb_uri_type_changed_cb( GtkComboBoxText* cb );

static gchar*
geturl( FileAccessWindow* faw )
{
    gchar* url = NULL;
    const gchar* host;
    const gchar* database;
    const gchar* username;
    const gchar* password;
    const gchar* type;
    const gchar* file;
    const gchar* path;

    host = gtk_entry_get_text( faw->tf_host );
    database = gtk_entry_get_text( faw->tf_database );
    username = gtk_entry_get_text( faw->tf_username );
    password = gtk_entry_get_text( faw->tf_password );
    file = gtk_file_chooser_get_filename( faw->fileChooser );

    type = gtk_combo_box_text_get_active_text(faw->cb_uri_type );
    if ( gnc_uri_is_file_protocol( type ) )
    {
        if ( file == NULL ) /* file protocol was chosen but no filename was set */
            return NULL;
        else                /* file protocol was chosen with filename set */
            path = file;
    }
    else                    /* db protocol was chosen */
        path = database;

    url = gnc_uri_create_uri (type, host, 0, username, password, path);

    return url;
}

void
gnc_ui_file_access_file_activated_cb( GtkFileChooser *chooser, FileAccessWindow *faw )
{
    g_return_if_fail( chooser != NULL );

    gnc_ui_file_access_response_cb( GTK_DIALOG(faw->dialog), GTK_RESPONSE_OK, NULL );
}

void
gnc_ui_file_access_response_cb(GtkDialog *dialog, gint response, GtkDialog *unused)
{
    FileAccessWindow* faw;
    gchar* url;

    g_return_if_fail( dialog != NULL );

    faw = g_object_get_data( G_OBJECT(dialog), "FileAccessWindow" );
    g_return_if_fail( faw != NULL );

    switch ( response )
    {
    case GTK_RESPONSE_HELP:
        gnc_gnome_help( HF_HELP, HL_GLOBPREFS );
        break;

    case GTK_RESPONSE_OK:
        url = geturl( faw );
        if ( url == NULL )
        {
            return;
        }
        if (g_str_has_prefix (url, "file://"))
        {
            if ( g_file_test( g_filename_from_uri( url, NULL, NULL ),
                              G_FILE_TEST_IS_DIR ))
            {
                gtk_file_chooser_set_current_folder_uri( faw->fileChooser, url );
                return;
            }
        }
        if ( faw->type == FILE_ACCESS_OPEN )
        {
            gboolean open_readonly = faw->readonly_checkbutton
                                     ? gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(faw->readonly_checkbutton))
                                     : FALSE;
            gnc_file_open_file (gnc_ui_get_main_window (GTK_WIDGET (dialog)), url, open_readonly);
        }
        else if ( faw->type == FILE_ACCESS_SAVE_AS )
        {
            gnc_file_do_save_as (gnc_ui_get_main_window (GTK_WIDGET (dialog)), url);
        }
        else if ( faw->type == FILE_ACCESS_EXPORT )
        {
            gnc_file_do_export (gnc_ui_get_main_window (GTK_WIDGET (dialog)), url);
        }
        break;

    case GTK_RESPONSE_CANCEL:
        break;

    default:
        PERR( "Invalid response" );
        break;
    }

    if ( response != GTK_RESPONSE_HELP )
    {
        gtk_widget_destroy( GTK_WIDGET(dialog) );
    }
}

/* Activate the file chooser and deactivate the db selection fields */
static void
set_widget_sensitivity( FileAccessWindow* faw, gboolean is_file_based_uri )
{
    if (is_file_based_uri)
    {
        gtk_widget_show(faw->frame_file);
        gtk_widget_hide(faw->frame_database);
        gtk_file_chooser_set_current_folder(faw->fileChooser, faw->starting_dir);
    }
    else
    {
        gtk_widget_show(faw->frame_database);
        gtk_widget_hide(faw->frame_file);
    }
//    gtk_widget_set_sensitive( faw->frame_file, is_file_based_uri );
//	gtk_widget_set_sensitive( faw->frame_database, !is_file_based_uri );
}

static void
set_widget_sensitivity_for_uri_type( FileAccessWindow* faw, const gchar* uri_type )
{
    if ( strcmp( uri_type, "file" ) == 0 || strcmp( uri_type, "xml" ) == 0
            || strcmp( uri_type, "sqlite3" ) == 0 )
    {
        set_widget_sensitivity( faw, /* is_file_based_uri */ TRUE );
    }
    else if ( strcmp( uri_type, "mysql" ) == 0 || strcmp( uri_type, "postgres" ) == 0 )
    {
        set_widget_sensitivity( faw, /* is_file_based_uri */ FALSE );
    }
    else
    {
        g_assert( FALSE );
    }
}

static void
cb_uri_type_changed_cb( GtkComboBoxText* cb )
{
    GtkWidget* dialog;
    FileAccessWindow* faw;
    const gchar* type;

    g_return_if_fail( cb != NULL );

    dialog = gtk_widget_get_toplevel( GTK_WIDGET(cb) );
    g_return_if_fail( dialog != NULL );
    faw = g_object_get_data( G_OBJECT(dialog), "FileAccessWindow" );
    g_return_if_fail( faw != NULL );

    type = gtk_combo_box_text_get_active_text( cb );
    set_widget_sensitivity_for_uri_type( faw, type );
}

static const char*
get_default_database( void )
{
    const gchar* default_db;

    default_db = g_getenv( "GNC_DEFAULT_DATABASE" );
    if ( default_db == NULL )
    {
        default_db = DEFAULT_DATABASE;
    }

    return default_db;
}

static void
gnc_ui_file_access (GtkWindow *parent, int type)
{
    FileAccessWindow *faw;
    GtkBuilder* builder;
    GtkButton* op;
    GtkWidget* file_chooser;
    GtkFileChooserWidget* fileChooser;
    GtkFileChooserAction fileChooserAction = GTK_FILE_CHOOSER_ACTION_OPEN;
    GList* list;
    GList* node;
    GtkWidget* uri_type_container;
    gboolean need_access_method_file = FALSE;
    gboolean need_access_method_mysql = FALSE;
    gboolean need_access_method_postgres = FALSE;
    gboolean need_access_method_sqlite3 = FALSE;
    gboolean need_access_method_xml = FALSE;
    gint access_method_index = -1;
    gint active_access_method_index = -1;
    const gchar* default_db;
    const gchar *button_label = NULL;
    const gchar *settings_section = NULL;
    gchar *last;

    g_return_if_fail( type == FILE_ACCESS_OPEN || type == FILE_ACCESS_SAVE_AS || type == FILE_ACCESS_EXPORT );

    faw = g_new0(FileAccessWindow, 1);
    g_return_if_fail( faw != NULL );

    faw->type = type;
    faw->starting_dir = NULL;

    /* Open the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-file-access.glade", "file_access_dialog" );
    faw->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "file_access_dialog" ));
    gtk_window_set_transient_for (GTK_WINDOW (faw->dialog), parent);
    g_object_set_data_full( G_OBJECT(faw->dialog), "FileAccessWindow", faw, g_free );

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(faw->dialog), "GncFileAccessDialog");

    faw->frame_file = GTK_WIDGET(gtk_builder_get_object (builder, "frame_file" ));
    faw->frame_database = GTK_WIDGET(gtk_builder_get_object (builder, "frame_database" ));
    faw->readonly_checkbutton = GTK_WIDGET(gtk_builder_get_object (builder, "readonly_checkbutton"));
    faw->tf_host = GTK_ENTRY(gtk_builder_get_object (builder, "tf_host" ));
    gtk_entry_set_text( faw->tf_host, DEFAULT_HOST );
    faw->tf_database = GTK_ENTRY(gtk_builder_get_object (builder, "tf_database" ));
    default_db = get_default_database();
    gtk_entry_set_text( faw->tf_database, default_db );
    faw->tf_username = GTK_ENTRY(gtk_builder_get_object (builder, "tf_username" ));
    faw->tf_password = GTK_ENTRY(gtk_builder_get_object (builder, "tf_password" ));

    switch ( type )
    {
    case FILE_ACCESS_OPEN:
        gtk_window_set_title(GTK_WINDOW(faw->dialog), _("Open..."));
        button_label = _("_Open");
        fileChooserAction = GTK_FILE_CHOOSER_ACTION_OPEN;
        settings_section = GNC_PREFS_GROUP_OPEN_SAVE;
        break;

    case FILE_ACCESS_SAVE_AS:
        gtk_window_set_title(GTK_WINDOW(faw->dialog), _("Save As..."));
        button_label = _("_Save As");
        fileChooserAction = GTK_FILE_CHOOSER_ACTION_SAVE;
        settings_section = GNC_PREFS_GROUP_OPEN_SAVE;
        gtk_widget_destroy(faw->readonly_checkbutton);
        faw->readonly_checkbutton = NULL;
        break;

    case FILE_ACCESS_EXPORT:
        gtk_window_set_title(GTK_WINDOW(faw->dialog), _("Export"));
        button_label = _("_Save As");
        fileChooserAction = GTK_FILE_CHOOSER_ACTION_SAVE;
        settings_section = GNC_PREFS_GROUP_EXPORT;
        gtk_widget_destroy(faw->readonly_checkbutton);
        faw->readonly_checkbutton = NULL;
        break;
    }

    op = GTK_BUTTON(gtk_builder_get_object (builder, "pb_op" ));
    if ( op != NULL )
        gtk_button_set_label( op, button_label );

    file_chooser = GTK_WIDGET(gtk_builder_get_object (builder, "file_chooser" ));
    fileChooser = GTK_FILE_CHOOSER_WIDGET(gtk_file_chooser_widget_new( fileChooserAction ));
    faw->fileChooser = GTK_FILE_CHOOSER(fileChooser);
    gtk_box_pack_start( GTK_BOX(file_chooser), GTK_WIDGET(fileChooser), TRUE, TRUE, 6 );

    /* Set the default directory */
    if (type == FILE_ACCESS_OPEN || type == FILE_ACCESS_SAVE_AS)
    {
        last = gnc_history_get_last();
        if ( last && gnc_uri_is_file_uri ( last ) )
        {
            gchar *filepath = gnc_uri_get_path ( last );
            faw->starting_dir = g_path_get_dirname( filepath );
            g_free ( filepath );
        }
    }
    if (!faw->starting_dir)
        faw->starting_dir = gnc_get_default_directory(settings_section);
    gtk_file_chooser_set_current_folder(faw->fileChooser, faw->starting_dir);

    g_object_connect( G_OBJECT(faw->fileChooser), "signal::file-activated",
                      gnc_ui_file_access_file_activated_cb, faw, NULL );

    uri_type_container = GTK_WIDGET(gtk_builder_get_object (builder, "vb_uri_type_container" ));
    faw->cb_uri_type = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
    gtk_container_add( GTK_CONTAINER(uri_type_container), GTK_WIDGET(faw->cb_uri_type) );
    gtk_box_set_child_packing( GTK_BOX(uri_type_container), GTK_WIDGET(faw->cb_uri_type),
                               /*expand*/TRUE, /*fill*/FALSE, /*padding*/0, GTK_PACK_START );
    g_object_connect( G_OBJECT(faw->cb_uri_type),
                      "signal::changed", cb_uri_type_changed_cb, NULL,
                      NULL );

    /* Autoconnect signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, faw);

    /* See what qof backends are available and add appropriate ones to the combo box */
    list = qof_backend_get_registered_access_method_list();
    for ( node = list; node != NULL; node = node->next )
    {
        const gchar* access_method = node->data;

        /* For the different access methods, "mysql" and "postgres" are added if available.  Access
        methods "xml" and "sqlite3" are compressed to "file" if opening a file, but when saving a file,
        both access methods are added. */
        if ( strcmp( access_method, "mysql" ) == 0 )
        {
            need_access_method_mysql = TRUE;
        }
        else if ( strcmp( access_method, "postgres" ) == 0 )
        {
            need_access_method_postgres = TRUE;
        }
        else if ( strcmp( access_method, "xml" ) == 0 )
        {
            if ( type == FILE_ACCESS_OPEN )
            {
                need_access_method_file = TRUE;
            }
            else
            {
                need_access_method_xml = TRUE;
            }
        }
        else if ( strcmp( access_method, "sqlite3" ) == 0 )
        {
            if ( type == FILE_ACCESS_OPEN )
            {
                need_access_method_file = TRUE;
            }
            else
            {
                need_access_method_sqlite3 = TRUE;
            }
        }
    }
    g_list_free(list);

    /* Now that the set of access methods has been ascertained, add them to the list, and set the
    default. */
    access_method_index = -1;
    if ( need_access_method_file )
    {
        gtk_combo_box_text_append_text( faw->cb_uri_type, "file" );
        active_access_method_index = ++access_method_index;
    }
    if ( need_access_method_mysql )
    {
        gtk_combo_box_text_append_text( faw->cb_uri_type, "mysql" );
        ++access_method_index;
    }
    if ( need_access_method_postgres )
    {
        gtk_combo_box_text_append_text( faw->cb_uri_type, "postgres" );
        ++access_method_index;
    }
    if ( need_access_method_sqlite3 )
    {
        gtk_combo_box_text_append_text( faw->cb_uri_type, "sqlite3" );
        active_access_method_index = ++access_method_index;
    }
    if ( need_access_method_xml )
    {
        gtk_combo_box_text_append_text( faw->cb_uri_type, "xml" );
        ++access_method_index;

        // Set XML as default if it is offered (which mean we are in
        // the "Save As" dialog)
        active_access_method_index = access_method_index;
    }
    g_assert( active_access_method_index >= 0 );

    g_object_unref(G_OBJECT(builder));

    /* Run the dialog */
    gtk_widget_show_all( faw->dialog );

    /* Hide the frame that's not required for the active access method so either only
     * the File or only the Database frame are presented. */
    gtk_combo_box_set_active(GTK_COMBO_BOX(faw->cb_uri_type), active_access_method_index );
    set_widget_sensitivity_for_uri_type( faw, gtk_combo_box_text_get_active_text( faw->cb_uri_type ));
}

void
gnc_ui_file_access_for_open (GtkWindow *parent)
{
    gnc_ui_file_access (parent, FILE_ACCESS_OPEN);
}


void
gnc_ui_file_access_for_save_as (GtkWindow *parent)
{
    gnc_ui_file_access (parent, FILE_ACCESS_SAVE_AS);
}


void
gnc_ui_file_access_for_export (GtkWindow *parent)
{
    gnc_ui_file_access (parent, FILE_ACCESS_EXPORT);
}
