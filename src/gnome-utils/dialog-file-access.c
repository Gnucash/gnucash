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
void gnc_ui_file_access_rb_xml_clicked_cb( GtkToggleButton* tb );
void gnc_ui_file_access_rb_sqlite3_clicked_cb( GtkToggleButton* tb );
void gnc_ui_file_access_rb_mysql_clicked_cb( GtkToggleButton* tb );
void gnc_ui_file_access_rb_pgsql_clicked_cb( GtkToggleButton* tb );

typedef struct FileAccessWindow
{
  /* Parts of the dialog */
  int type;

  GtkWidget* dialog;
  GtkWidget* frame_file;
  GtkWidget* frame_database;
  GtkFileChooser* fileChooser;
  GtkWidget* rb_xml;
  GtkWidget* rb_sqlite3;
  GtkWidget* rb_mysql;
  GtkWidget* rb_pgsql;
  GtkWidget* tf_host;
  GtkWidget* tf_database;
  GtkWidget* tf_username;
  GtkWidget* tf_password;
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

	host = gtk_entry_get_text( GTK_ENTRY(faw->tf_host) );
	database = gtk_entry_get_text( GTK_ENTRY(faw->tf_database) );
	username = gtk_entry_get_text( GTK_ENTRY(faw->tf_username) );
	password = gtk_entry_get_text( GTK_ENTRY(faw->tf_password) );
	file = gtk_file_chooser_get_filename( faw->fileChooser );

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(faw->rb_xml) ) ) {
		type = "xml";
		url = g_strdup_printf( "%s://%s", type, file );
	} else if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(faw->rb_sqlite3) ) ) {
		type = "sqlite3";
		url = g_strdup_printf( "%s://%s", type, file );
	} else if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(faw->rb_mysql) ) ) {
		type = "mysql";
		url = g_strdup_printf( "%s://%s:%s:%s:%s",
							type, host, database, username, password );
	} else {
		g_assert( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(faw->rb_pgsql) ) );
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
on_rb_filetype_clicked( FileAccessWindow* faw )
{
    gtk_widget_set_sensitive( faw->frame_file, TRUE );
	gtk_widget_set_sensitive( faw->frame_database, FALSE );
}

void
gnc_ui_file_access_rb_xml_clicked_cb( GtkToggleButton* tb )
{
	GtkWidget* dialog;
    FileAccessWindow* faw;

	g_return_if_fail( tb != NULL );

	if( gtk_toggle_button_get_active( tb ) ) {
		dialog = gtk_widget_get_toplevel( GTK_WIDGET(tb) );
		g_return_if_fail( dialog != NULL );
    	faw = g_object_get_data( G_OBJECT(dialog), "FileAccessWindow" );
    	g_return_if_fail( faw != NULL );

    	on_rb_filetype_clicked( faw );
	}
}

void
gnc_ui_file_access_rb_sqlite3_clicked_cb( GtkToggleButton* tb )
{
	GtkWidget* dialog;
    FileAccessWindow* faw;

	if( gtk_toggle_button_get_active( tb ) ) {
		dialog = gtk_widget_get_toplevel( GTK_WIDGET(tb) );
		g_return_if_fail( dialog != NULL );
    	faw = g_object_get_data( G_OBJECT(dialog), "FileAccessWindow" );
    	g_return_if_fail( faw != NULL );

    	on_rb_filetype_clicked( faw );
	}
}

/* Deactivate the file chooser and activate the db selection fields */
static void
on_rb_databasetype_clicked( FileAccessWindow* faw )
{
    gtk_widget_set_sensitive( faw->frame_file, FALSE );
	gtk_widget_set_sensitive( faw->frame_database, TRUE );
}

void
gnc_ui_file_access_rb_mysql_clicked_cb( GtkToggleButton* tb )
{
	GtkWidget* dialog;
    FileAccessWindow* faw;

	if( gtk_toggle_button_get_active( tb ) ) {
		g_return_if_fail( tb != NULL );
		dialog = gtk_widget_get_toplevel( GTK_WIDGET(tb) );
		g_return_if_fail( dialog != NULL );
    	faw = g_object_get_data( G_OBJECT(dialog), "FileAccessWindow" );
    	g_return_if_fail( faw != NULL );

    	on_rb_databasetype_clicked( faw );
	}
}

void
gnc_ui_file_access_rb_pgsql_clicked_cb( GtkToggleButton* tb )
{
	GtkWidget* dialog;
    FileAccessWindow* faw;

	if( gtk_toggle_button_get_active( tb ) ) {
		g_return_if_fail( tb != NULL );
		dialog = gtk_widget_get_toplevel( GTK_WIDGET(tb) );
		g_return_if_fail( dialog != NULL );
    	faw = g_object_get_data( G_OBJECT(dialog), "FileAccessWindow" );
    	g_return_if_fail( faw != NULL );

    	on_rb_databasetype_clicked( faw );
	}
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
	GtkFileChooserAction fileChooserAction;
	GList* list;
	GList* node;

	g_return_if_fail( type == FILE_ACCESS_OPEN || type == FILE_ACCESS_SAVE_AS );

    faw = g_new0(FileAccessWindow, 1);
    g_return_if_fail( faw != NULL );

	faw->type = type;

    /* Open the dialog */
    xml = gnc_glade_xml_new( "dialog-file-access.glade", "File Access" );
    faw->dialog = glade_xml_get_widget( xml, "File Access" );

	faw->frame_file = glade_xml_get_widget( xml, "frame_file" );
	faw->frame_database = glade_xml_get_widget( xml, "frame_database" );
    faw->rb_xml = glade_xml_get_widget( xml, "rb_xml" );
    faw->rb_sqlite3 = glade_xml_get_widget( xml, "rb_sqlite3" );
    faw->rb_mysql = glade_xml_get_widget( xml, "rb_mysql" );
    faw->rb_pgsql = glade_xml_get_widget( xml, "rb_pgsql" );
    faw->tf_host = glade_xml_get_widget( xml, "tf_host" );
	gtk_entry_set_text( GTK_ENTRY(faw->tf_host), DEFAULT_HOST );
    faw->tf_database = glade_xml_get_widget( xml, "tf_database" );
	gtk_entry_set_text( GTK_ENTRY(faw->tf_database), DEFAULT_DATABASE );
    faw->tf_username = glade_xml_get_widget( xml, "tf_username" );
    faw->tf_password = glade_xml_get_widget( xml, "tf_password" );
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

    /* Autoconnect signals */
    glade_xml_signal_autoconnect_full( xml, gnc_glade_autoconnect_full_func,
				    					faw->dialog );

	/* See what qof backends are available, and disable sqlite3, mysql and postgres if not
	available */
	gtk_widget_set_sensitive( faw->rb_sqlite3, FALSE );
	gtk_widget_set_sensitive( faw->rb_mysql, FALSE );
	gtk_widget_set_sensitive( faw->rb_pgsql, FALSE );
	list = qof_backend_get_registered_access_method_list();
	for( node = list; node != NULL; node = node->next ) {
		const gchar* access_method = node->data;
		if( strcmp( access_method, "sqlite3" ) == 0 ) {
			gtk_widget_set_sensitive( faw->rb_sqlite3, TRUE );
		} else if( strcmp( access_method, "mysql" ) == 0 ) {
			gtk_widget_set_sensitive( faw->rb_mysql, TRUE );
		} else if( strcmp( access_method, "postgres" ) == 0 ) {
			gtk_widget_set_sensitive( faw->rb_pgsql, TRUE );
		}
	}
	g_list_free(list);

    /* Clean up the xml data structure when the dialog is destroyed */
    g_object_set_data_full( G_OBJECT(faw->dialog), "dialog-file-access.glade",
			 				xml, g_object_unref );
    g_object_set_data_full( G_OBJECT(faw->dialog), "FileAccessWindow", faw,
			 				g_free );

    /* Run the dialog */
    gtk_widget_show_all( faw->dialog );
	on_rb_filetype_clicked( faw );
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
