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
#include "gnc-gtk-utils.h"
#include "dialog-file-access.h"
#include "gnc-file.h"
#include "gnc-plugin-file-history.h"
#include "gnc-session.h"


/* MariaDB/MySQL/Postgres optimize localhost to a unix socket but
 * flatpak won't connect to unix sockets without gymnastics default to
 * the localhost IP to force a network connection.
 */
#define DEFAULT_HOST "127.0.0.1"
#define DEFAULT_DATABASE PROJECT_NAME
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
    GtkWidget           *file_select_button;
    GtkWidget           *file_name_label;
    gchar               *file_name;
    gchar               *starting_dir;
    GtkDropDown         *cb_uri_type;
    GtkEntry            *tf_host;
    GtkEntry            *tf_database;
    GtkEntry            *tf_username;
    GtkEntry            *tf_password;
    GtkEntry            *tf_port;
} FileAccessWindow;

static void file_access_accept_clicked_cb (GtkButton *button,
                                           FileAccessWindow *faw);
static void file_access_cancel_clicked_cb (GtkButton *button,
                                           FileAccessWindow *faw);
static void cb_uri_type_changed_cb (GtkDropDown *drop_down, GParamSpec *pspec,
                                        gpointer user_data);
static void port_insert_text_cb( GtkEditable *editable, const gchar *text,
                                 gint length, gint *position, gpointer data );

static const gchar*
get_active_uri_type (FileAccessWindow *faw)
{
    GObject *item;

    item = gtk_drop_down_get_selected_item (faw->cb_uri_type);
    if (!item)
        return NULL;
    return gtk_string_object_get_string (GTK_STRING_OBJECT (item));
}

static gchar*
geturl( FileAccessWindow* faw )
{
    gchar* url = NULL;
    const gchar* host = NULL;
    const gchar* username = NULL;
    const gchar* password = NULL;
    const gchar* type;
    gchar* path = NULL;
    gint32 port = 0;
    const gchar* port_text = NULL;

    type = get_active_uri_type (faw);
    if (!type)
        return NULL;

    if (gnc_uri_is_file_scheme (type))
    {
        path = g_strdup (faw->file_name);
        if ( !path ) /* file protocol was chosen but no filename was set */
        {
            return NULL;
        }
    }
    else                    /* db protocol was chosen */
    {
        host = gtk_editable_get_text (GTK_EDITABLE (faw->tf_host));
        path = g_strdup (gtk_editable_get_text (GTK_EDITABLE (faw->tf_database)));
        username = gtk_editable_get_text (GTK_EDITABLE (faw->tf_username));
        password = gtk_editable_get_text (GTK_EDITABLE (faw->tf_password));
    }

    g_assert (faw->tf_port != NULL);
    port_text = gtk_editable_get_text (GTK_EDITABLE (faw->tf_port));
    if (port_text && *port_text)
        port = atoi (port_text) & 0xffff;

    url = gnc_uri_create_uri (type, host, port, username, password, path);

    g_free (path);

    return url;
}

typedef struct
{
    GWeakRef dialog;
} FileAccessFileDialogData;

static void
file_access_file_dialog_data_free (FileAccessFileDialogData *data)
{
    g_weak_ref_clear (&data->dialog);
    g_free (data);
}

static gboolean
file_access_set_selected_file (FileAccessWindow *faw, GFile *file)
{
    gchar *path = g_file_get_path (file);
    gchar *directory;

    if (!path || g_file_test (path, G_FILE_TEST_IS_DIR))
    {
        g_free (path);
        return FALSE;
    }

    directory = g_path_get_dirname (path);
    g_free (faw->file_name);
    faw->file_name = path;
    g_free (faw->starting_dir);
    faw->starting_dir = directory;
    gtk_label_set_text (GTK_LABEL (faw->file_name_label), faw->file_name);
    return TRUE;
}

static void
file_access_file_dialog_finished (GObject *source, GAsyncResult *result,
                                  gpointer user_data)
{
    FileAccessFileDialogData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file;
    GtkWidget *dialog;
    FileAccessWindow *faw = NULL;

    file = gnc_file_dialog_request_finish (request, result, &error);
    dialog = g_weak_ref_get (&data->dialog);
    if (dialog)
        faw = g_object_get_data (G_OBJECT (dialog), "FileAccessWindow");

    if (file && faw)
    {
        if (!file_access_set_selected_file (faw, file))
            gnc_error_dialog (GTK_WINDOW (dialog), "%s",
                              _("Please select a file, not a folder."));
    }
    else if (dialog && error &&
             !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
    {
        gnc_error_dialog (GTK_WINDOW (dialog), "%s", error->message);
    }

    g_clear_object (&file);
    g_clear_error (&error);
    g_clear_object (&dialog);
    file_access_file_dialog_data_free (data);
}

static void
file_access_choose_file_cb (GtkButton *button, gpointer user_data)
{
    FileAccessWindow *faw = user_data;
    FileAccessFileDialogData *data;
    GncFileDialogRequest *request;
    GNCFileDialogType type;

    switch (faw->type)
    {
    case FILE_ACCESS_OPEN:
        type = GNC_FILE_DIALOG_OPEN;
        break;
    case FILE_ACCESS_SAVE_AS:
        type = GNC_FILE_DIALOG_SAVE;
        break;
    case FILE_ACCESS_EXPORT:
        type = GNC_FILE_DIALOG_EXPORT;
        break;
    default:
        g_assert_not_reached ();
    }

    data = g_new0 (FileAccessFileDialogData, 1);
    g_weak_ref_init (&data->dialog, faw->dialog);
    request = gnc_file_dialog_request_new (
        GTK_WINDOW (faw->dialog), _("Select GnuCash File"),
        gnc_file_dialog_get_datafile_filters (), faw->starting_dir, type);
    if (type == GNC_FILE_DIALOG_OPEN)
        gnc_file_dialog_request_open_async (request, NULL,
                                            file_access_file_dialog_finished,
                                            data);
    else
        gnc_file_dialog_request_save_async (request, NULL,
                                            file_access_file_dialog_finished,
                                            data);
    g_object_unref (request);

    (void)button;
}

static void
file_access_accept_clicked_cb (GtkButton *button, FileAccessWindow *faw)
{
    gchar *url;

    g_return_if_fail (faw != NULL);

    url = geturl (faw);
    if (!url)
        return;
    if (g_str_has_prefix (url, "file://"))
    {
        gchar *path = gnc_uri_get_path (url);
        gboolean is_directory = path && g_file_test (path, G_FILE_TEST_IS_DIR);

        g_free (path);
        if (is_directory)
        {
            gnc_error_dialog (GTK_WINDOW (faw->dialog), "%s",
                              _("Please select a file, not a folder."));
            g_free (url);
            return;
        }
    }

    GtkWindow *parent = gtk_window_get_transient_for (GTK_WINDOW (faw->dialog));
    if (!parent)
        parent = gnc_ui_get_main_window (GTK_WIDGET (faw->dialog));

    switch (faw->type)
    {
    case FILE_ACCESS_OPEN:
        gnc_file_open_file (parent, url,
                            faw->readonly_checkbutton &&
                            gtk_check_button_get_active (
                                GTK_CHECK_BUTTON (faw->readonly_checkbutton)));
        break;
    case FILE_ACCESS_SAVE_AS:
        gnc_file_do_save_as (parent, url);
        break;
    case FILE_ACCESS_EXPORT:
        gnc_file_do_export (parent, url);
        break;
    default:
        g_assert_not_reached ();
    }

    g_free (url);
    gtk_window_destroy (GTK_WINDOW (faw->dialog));
    (void)button;
}

static void
file_access_cancel_clicked_cb (GtkButton *button, FileAccessWindow *faw)
{
    g_return_if_fail (faw != NULL);

    gtk_window_destroy (GTK_WINDOW (faw->dialog));
    (void)button;
}
/* Activate the file chooser and deactivate the db selection fields */
static void
set_widget_sensitivity( FileAccessWindow* faw, gboolean is_file_based_uri )
{
    if (is_file_based_uri)
    {
        gtk_widget_set_visible (faw->frame_file, TRUE);
        gtk_widget_set_visible (faw->frame_database, FALSE);
    }
    else
    {
        gtk_widget_set_visible (faw->frame_database, TRUE);
        gtk_widget_set_visible (faw->frame_file, FALSE);
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
        gtk_entry_set_placeholder_text( faw->tf_port,
            strcmp( uri_type, "mysql" ) == 0 ? _("Default: 3306") : _("Default: 5432") );
    }
    else
    {
        g_assert( FALSE );
    }
}

static void
port_insert_text_cb( GtkEditable *editable, const gchar *text, gint length,
                     gint *position, gpointer data )
{
    for ( gint i = 0; i < length; i++ )
    {
        if ( !g_ascii_isdigit( text[i] ) )
        {
            g_signal_stop_emission_by_name( G_OBJECT(editable), "insert-text" );
            return;
        }
    }
}

static void
cb_uri_type_changed_cb (GtkDropDown *drop_down, GParamSpec *pspec,
                        gpointer user_data)
{
    GtkRoot *root;
    FileAccessWindow *faw;
    const gchar *type;

    g_return_if_fail (GTK_IS_DROP_DOWN (drop_down));

    root = gtk_widget_get_root (GTK_WIDGET (drop_down));
    g_return_if_fail (GTK_IS_WINDOW (root));
    faw = g_object_get_data (G_OBJECT (root), "FileAccessWindow");
    g_return_if_fail (faw != NULL);

    type = get_active_uri_type (faw);
    if (type)
        set_widget_sensitivity_for_uri_type (faw, type);
    (void)pspec;
    (void)user_data;
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
free_file_access_window (FileAccessWindow *faw)
{
    g_free (faw->file_name);
    g_free (faw->starting_dir);
    g_free (faw);
}

static void
gnc_ui_file_access (GtkWindow *parent, int type)
{
    FileAccessWindow *faw;
    GtkBuilder* builder;
    GtkButton* op;
    GtkButton* cancel;
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
    g_object_set_data_full (G_OBJECT(faw->dialog), "FileAccessWindow", faw,
                            (GDestroyNotify)free_file_access_window);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(faw->dialog), "gnc-id-file-access");

    faw->frame_file = GTK_WIDGET(gtk_builder_get_object (builder, "frame_file" ));
    faw->frame_database = GTK_WIDGET(gtk_builder_get_object (builder, "frame_database" ));
    faw->readonly_checkbutton = GTK_WIDGET(gtk_builder_get_object (builder, "readonly_checkbutton"));
    faw->tf_host = GTK_ENTRY(gtk_builder_get_object (builder, "tf_host" ));
    gtk_editable_set_text (GTK_EDITABLE (faw->tf_host), DEFAULT_HOST);
    faw->tf_database = GTK_ENTRY(gtk_builder_get_object (builder, "tf_database" ));
    default_db = get_default_database();
    gtk_editable_set_text (GTK_EDITABLE (faw->tf_database), default_db);
    faw->tf_username = GTK_ENTRY(gtk_builder_get_object (builder, "tf_username" ));
    faw->tf_password = GTK_ENTRY(gtk_builder_get_object (builder, "tf_password" ));
    faw->tf_port = GTK_ENTRY(gtk_builder_get_object (builder, "tf_port" ));
    g_signal_connect( G_OBJECT(faw->tf_port), "insert-text",
                      G_CALLBACK(port_insert_text_cb), NULL );
    gtk_entry_set_max_length( faw->tf_port, 5 );

    switch ( type )
    {
    case FILE_ACCESS_OPEN:
        gtk_window_set_title(GTK_WINDOW(faw->dialog), _("Open…"));
        button_label = _("_Open");
        settings_section = GNC_PREFS_GROUP_OPEN_SAVE;
        break;

    case FILE_ACCESS_SAVE_AS:
        gtk_window_set_title(GTK_WINDOW(faw->dialog), _("Save As…"));
        button_label = _("_Save As");
        settings_section = GNC_PREFS_GROUP_OPEN_SAVE;
        gtk_widget_unparent (faw->readonly_checkbutton);
        faw->readonly_checkbutton = NULL;
        break;

    case FILE_ACCESS_EXPORT:
        gtk_window_set_title(GTK_WINDOW(faw->dialog), _("Export"));
        button_label = _("_Save As");
        settings_section = GNC_PREFS_GROUP_EXPORT;
        gtk_widget_unparent (faw->readonly_checkbutton);
        faw->readonly_checkbutton = NULL;
        break;
    }

    op = GTK_BUTTON(gtk_builder_get_object (builder, "pb_op" ));
    cancel = GTK_BUTTON(gtk_builder_get_object (builder, "cancel_button" ));
    if (op)
    {
        gtk_button_set_label (op, button_label);
        gtk_window_set_default_widget (GTK_WINDOW (faw->dialog),
                                       GTK_WIDGET (op));
        g_signal_connect (op, "clicked",
                          G_CALLBACK (file_access_accept_clicked_cb), faw);
    }
    if (cancel)
        g_signal_connect (cancel, "clicked",
                          G_CALLBACK (file_access_cancel_clicked_cb), faw);

    faw->file_select_button = GTK_WIDGET (gtk_builder_get_object (
        builder, "file_select_button"));
    faw->file_name_label = GTK_WIDGET (gtk_builder_get_object (
        builder, "file_name_label"));
    g_signal_connect (faw->file_select_button, "clicked",
                      G_CALLBACK (file_access_choose_file_cb), faw);

    /* Set the default directory */
    if (type == FILE_ACCESS_OPEN || type == FILE_ACCESS_SAVE_AS)
    {
        last = gnc_history_get_last();
        if ( last && *last && gnc_uri_targets_local_fs (last))
        {
            gchar *filepath = gnc_uri_get_path ( last );
            faw->starting_dir = g_path_get_dirname( filepath );
            g_free ( filepath );
        }
        g_free (last);
    }
    if (!faw->starting_dir)
        faw->starting_dir = gnc_get_default_directory(settings_section);

    uri_type_container = GTK_WIDGET(gtk_builder_get_object (builder, "vb_uri_type_container" ));
    faw->cb_uri_type = gnc_gtk_drop_down_new (
        G_LIST_MODEL (gtk_string_list_new (NULL)), NULL);
    gnc_box_append_full (GTK_BOX(uri_type_container), GTK_WIDGET (faw->cb_uri_type),
                         TRUE, FALSE, 0);
    g_signal_connect (faw->cb_uri_type, "notify::selected",
                      G_CALLBACK (cb_uri_type_changed_cb), NULL);

    /* Autoconnect signals */
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, faw);

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
        gtk_string_list_append (GTK_STRING_LIST (gtk_drop_down_get_model (faw->cb_uri_type)), "file" );
        active_access_method_index = ++access_method_index;
    }
    if ( need_access_method_mysql )
    {
        gtk_string_list_append (GTK_STRING_LIST (gtk_drop_down_get_model (faw->cb_uri_type)), "mysql" );
        ++access_method_index;
    }
    if ( need_access_method_postgres )
    {
        gtk_string_list_append (GTK_STRING_LIST (gtk_drop_down_get_model (faw->cb_uri_type)), "postgres" );
        ++access_method_index;
    }
    if ( need_access_method_sqlite3 )
    {
        gtk_string_list_append (GTK_STRING_LIST (gtk_drop_down_get_model (faw->cb_uri_type)), "sqlite3" );
        active_access_method_index = ++access_method_index;
    }
    if ( need_access_method_xml )
    {
        gtk_string_list_append (GTK_STRING_LIST (gtk_drop_down_get_model (faw->cb_uri_type)), "xml" );
        ++access_method_index;

        // Set XML as default if it is offered (which mean we are in
        // the "Save As" dialog)
        active_access_method_index = access_method_index;
    }
    g_assert( active_access_method_index >= 0 );

    g_object_unref(G_OBJECT(builder));

    /* Run the dialog */
    gtk_widget_set_visible (faw->dialog, TRUE);

    /* Hide the frame that's not required for the active access method so either only
     * the File or only the Database frame are presented. */
    gtk_drop_down_set_selected (faw->cb_uri_type, active_access_method_index);
    set_widget_sensitivity_for_uri_type (faw, get_active_uri_type (faw));
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
