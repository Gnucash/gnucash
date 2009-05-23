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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glade/glade.h>

#include "gnc-ui.h"
#include "dialog-utils.h"
#include "dialog-file-access.h"
#include "gnc-file.h"
#include "gnc-session.h"

static QofLogModule log_module = GNC_MOD_GUI;

#define DEFAULT_HOST "localhost"
#define DEFAULT_DATABASE "gnucash"
#define FILE_ACCESS_OPEN    0
#define FILE_ACCESS_SAVE_AS 1

void gnc_ui_file_access_response_cb( GtkDialog *, gint, GtkDialog * );
static void cb_uri_type_changed_cb( GtkComboBox* cb );

typedef struct FileAccessWindow
{
  /* Parts of the dialog */
  int type;

  GtkWidget* dialog;
  GtkWidget* frame_file;
  GtkWidget* frame_database;
  GtkFileChooser* fileChooser;
  GtkComboBox* cb_uri_type;
  GtkEntry* tf_host;
  GtkEntry* tf_database;
  GtkEntry* tf_username;
  GtkEntry* tf_password;
} FileAccessWindow;

static gchar*
geturl( FileAccessWindow* faw )
{
	gchar* url;
	const gchar* host;
	const gchar* database;
	const gchar* username;
	const gchar* password;
	const gchar* type;
	const gchar* file;

	host = gtk_entry_get_text( faw->tf_host );
	database = gtk_entry_get_text( faw->tf_database );
	username = gtk_entry_get_text( faw->tf_username );
	password = gtk_entry_get_text( faw->tf_password );
	file = gtk_file_chooser_get_filename( faw->fileChooser );

	type = gtk_combo_box_get_active_text( faw->cb_uri_type );
	if( strcmp( type, "xml" ) == 0 ) {
		type = "xml";
		url = g_strdup_printf( "%s://%s", type, file );
	} else if( strcmp( type, "sqlite3" ) == 0 ) {
		type = "sqlite3";
		url = g_strdup_printf( "%s://%s", type, file );
	} else if( strcmp( type, "mysql" ) == 0 ) {
		type = "mysql";
		url = g_strdup_printf( "%s://%s:%s:%s:%s",
							type, host, database, username, password );
	} else {
		g_assert( strcmp( type, "postgres" ) == 0 );
		type = "postgres";
		url = g_strdup_printf( "%s://%s:%s:%s:%s",
							type, host, database, username, password );
	}

	return url;
}

void
gnc_ui_file_access_response_cb(GtkDialog *dialog, gint response, GtkDialog *unused)
{
    FileAccessWindow* faw;
	gchar* url;

    g_return_if_fail( dialog != NULL );

    faw = g_object_get_data( G_OBJECT(dialog), "FileAccessWindow" );
    g_return_if_fail( faw != NULL );

    switch( response ) {
    case GTK_RESPONSE_HELP:
        gnc_gnome_help( HF_HELP, HL_GLOBPREFS );
        break;
  
	case GTK_RESPONSE_OK:
		url = geturl( faw );
		if( faw->type == FILE_ACCESS_OPEN ) {
			gnc_file_open_file( url );
		} else if( faw->type == FILE_ACCESS_SAVE_AS ) {
			gnc_file_do_save_as( url );
		}
		break;

	case GTK_RESPONSE_CANCEL:
		break;

    default:
        PERR( "Invalid response" );
        break;
    }

	if( response != GTK_RESPONSE_HELP ) {
        gtk_widget_destroy( GTK_WIDGET(dialog) );
	}
}

/* Activate the file chooser and deactivate the db selection fields */
static void
set_widget_sensitivity( FileAccessWindow* faw, gboolean is_file_based_uri )
{
    gtk_widget_set_sensitive( faw->frame_file, is_file_based_uri );
	gtk_widget_set_sensitive( faw->frame_database, !is_file_based_uri );
}

static void
set_widget_sensitivity_for_uri_type( FileAccessWindow* faw, const gchar* uri_type )
{
	if( strcmp( uri_type, "xml" ) == 0 || strcmp( uri_type, "sqlite3" ) == 0 ) {
		set_widget_sensitivity( faw, /* is_file_based_uri */ TRUE );
	} else if( strcmp( uri_type, "mysql" ) == 0 || strcmp( uri_type, "postgres" ) == 0 ) {
		set_widget_sensitivity( faw, /* is_file_based_uri */ FALSE );
	} else {
		g_assert( FALSE );
	}
}

static void
cb_uri_type_changed_cb( GtkComboBox* cb )
{
	GtkWidget* dialog;
    FileAccessWindow* faw;
	const gchar* type;

	g_return_if_fail( cb != NULL );

	dialog = gtk_widget_get_toplevel( GTK_WIDGET(cb) );
	g_return_if_fail( dialog != NULL );
    faw = g_object_get_data( G_OBJECT(dialog), "FileAccessWindow" );
    g_return_if_fail( faw != NULL );

	type = gtk_combo_box_get_active_text( cb );
	set_widget_sensitivity_for_uri_type( faw, type );
}

static void
gnc_ui_file_access( int type )
{
    FileAccessWindow *faw;
    GladeXML* xml;
    GtkWidget* box;
	GList* ds_node;
	GtkButton* op;
	GtkWidget* align;
	GtkFileChooserWidget* fileChooser;
	GtkFileChooserAction fileChooserAction = GTK_FILE_CHOOSER_ACTION_OPEN;
	GList* list;
	GList* node;
	GtkWidget* uri_type_container;

	g_return_if_fail( type == FILE_ACCESS_OPEN || type == FILE_ACCESS_SAVE_AS );

    faw = g_new0(FileAccessWindow, 1);
    g_return_if_fail( faw != NULL );

	faw->type = type;

    /* Open the dialog */
    xml = gnc_glade_xml_new( "dialog-file-access.glade", "File Access" );
    faw->dialog = glade_xml_get_widget( xml, "File Access" );
    g_object_set_data_full( G_OBJECT(faw->dialog), "FileAccessWindow", faw,
			 				g_free );

	faw->frame_file = glade_xml_get_widget( xml, "frame_file" );
	faw->frame_database = glade_xml_get_widget( xml, "frame_database" );
    faw->tf_host = GTK_ENTRY(glade_xml_get_widget( xml, "tf_host" ));
	gtk_entry_set_text( faw->tf_host, DEFAULT_HOST );
    faw->tf_database = GTK_ENTRY(glade_xml_get_widget( xml, "tf_database" ));
	gtk_entry_set_text( faw->tf_database, DEFAULT_DATABASE );
    faw->tf_username = GTK_ENTRY(glade_xml_get_widget( xml, "tf_username" ));
    faw->tf_password = GTK_ENTRY(glade_xml_get_widget( xml, "tf_password" ));
	op = GTK_BUTTON(glade_xml_get_widget( xml, "pb_op" ));
	if( op != NULL ) {
		switch( type ) {
		case FILE_ACCESS_OPEN:
		    gtk_button_set_label( op, "gtk-open" );
			fileChooserAction = GTK_FILE_CHOOSER_ACTION_OPEN;
			break;

		case FILE_ACCESS_SAVE_AS:
		    gtk_button_set_label( op, "gtk-save-as" );
			fileChooserAction = GTK_FILE_CHOOSER_ACTION_SAVE;
			break;
		}
		gtk_button_set_use_stock( op, TRUE );
	}
	align = glade_xml_get_widget( xml, "alignment_file_chooser" );
	fileChooser = GTK_FILE_CHOOSER_WIDGET(gtk_file_chooser_widget_new( fileChooserAction ));
	faw->fileChooser = GTK_FILE_CHOOSER(fileChooser);
	gtk_container_add( GTK_CONTAINER(align), GTK_WIDGET(fileChooser) );

	uri_type_container = glade_xml_get_widget( xml, "vb_uri_type_container" );
	faw->cb_uri_type = GTK_COMBO_BOX(gtk_combo_box_new_text());
	gtk_container_add( GTK_CONTAINER(uri_type_container), GTK_WIDGET(faw->cb_uri_type) );
	gtk_box_set_child_packing( GTK_BOX(uri_type_container), GTK_WIDGET(faw->cb_uri_type), 
								/*expand*/TRUE, /*fill*/FALSE, /*padding*/0, GTK_PACK_START );
	g_object_connect( G_OBJECT(faw->cb_uri_type),
					"signal::changed", cb_uri_type_changed_cb, NULL,
					NULL );

    /* Autoconnect signals */
    glade_xml_signal_autoconnect_full( xml, gnc_glade_autoconnect_full_func,
				    					faw->dialog );

	/* See what qof backends are available and add appropriate ones to the combo box */
	list = qof_backend_get_registered_access_method_list();
	for( node = list; node != NULL; node = node->next ) {
		const gchar* access_method = node->data;
		if( strcmp( access_method, "xml" ) == 0 ||
			strcmp( access_method, "sqlite3" ) == 0 ||
			strcmp( access_method, "mysql" ) == 0 ||
			strcmp( access_method, "postgres" ) == 0 ) {
			gtk_combo_box_append_text( faw->cb_uri_type, access_method );
		}
	}
	g_list_free(list);
	gtk_combo_box_set_active( faw->cb_uri_type, 0 );
	set_widget_sensitivity_for_uri_type( faw, gtk_combo_box_get_active_text( faw->cb_uri_type ) );

    /* Clean up the xml data structure when the dialog is destroyed */
    g_object_set_data_full( G_OBJECT(faw->dialog), "dialog-file-access.glade",
			 				xml, g_object_unref );

    /* Run the dialog */
    gtk_widget_show_all( faw->dialog );
}

void
gnc_ui_file_access_for_open( void )
{
	gnc_ui_file_access( FILE_ACCESS_OPEN );
}


void
gnc_ui_file_access_for_save_as( void )
{
	gnc_ui_file_access( FILE_ACCESS_SAVE_AS );
}
