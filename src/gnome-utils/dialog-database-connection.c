/********************************************************************\
 * dialog-database-connection.c -- dialog for opening a connection  *
 *                        to a libgda database, either predefined   *
 *                        in ~/.libgda/config or explicit using     *
 *                        provider and database.                    *
 *                                                                  *
 * Copyright (C) 2007-8 Phil Longstaff (plongstaff@rogers.com)      *
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
#include "dialog-database-connection.h"
#include "gnc-file.h"
#include "gnc-session.h"

static QofLogModule log_module = GNC_MOD_GUI;

void gnc_database_connection_response_cb( GtkDialog *, gint, GtkDialog * );
#define PB_LOAD_RESPONSE 1000
#define PB_SAVE_RESPONSE 1001

struct DatabaseConnectionWindow
{
  /* Parts of the dialog */
  GtkWidget* dialog;
  GtkWidget* rb_mysql;
  GtkWidget* rb_postgresql;
  GtkWidget* tf_host;
  GtkWidget* tf_database;
  GtkWidget* tf_username;
  GtkWidget* tf_password;
};

static gchar*
geturl( struct DatabaseConnectionWindow* dcw )
{
	gchar* url;
	const gchar* host;
	const gchar* database;
	const gchar* username;
	const gchar* password;
	const gchar* type;

	host = gtk_entry_get_text( GTK_ENTRY(dcw->tf_host) );
	database = gtk_entry_get_text( GTK_ENTRY(dcw->tf_database) );
	username = gtk_entry_get_text( GTK_ENTRY(dcw->tf_username) );
	password = gtk_entry_get_text( GTK_ENTRY(dcw->tf_password) );

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(dcw->rb_mysql) ) ) {
		type = "mysql";
	} else {
		type = "postgres";
	}
	url = g_strdup_printf( "%s://%s:%s:%s:%s",
							type, host, database, username, password );

	return url;
}

void
gnc_database_connection_response_cb(GtkDialog *dialog, gint response, GtkDialog *unused)
{
    struct DatabaseConnectionWindow* dcw;

    g_return_if_fail( dialog != NULL );

    dcw = g_object_get_data( G_OBJECT(dialog), "DatabaseConnectionWindow" );
    g_return_if_fail(dcw);

    switch( response ) {
    case GTK_RESPONSE_HELP:
        gnc_gnome_help( HF_HELP, HL_GLOBPREFS );
        break;
  
    case PB_LOAD_RESPONSE:
		gnc_file_open_file( geturl( dcw ) );
		break;

    case PB_SAVE_RESPONSE:
		gnc_file_do_save_as( geturl( dcw ) );
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

void gnc_ui_database_connection( void )
{
    struct DatabaseConnectionWindow *dcw;
    GladeXML* xml;
    GtkWidget* box;
	GList* ds_node;

    dcw = g_new0(struct DatabaseConnectionWindow, 1);
    g_return_if_fail(dcw);

    /* Open the dialog */
    xml = gnc_glade_xml_new( "dialog-database-connection.glade", "Database Connection" );
    dcw->dialog = glade_xml_get_widget( xml, "Database Connection" );

    dcw->rb_mysql = glade_xml_get_widget( xml, "rb_mysql" );
    dcw->rb_postgresql = glade_xml_get_widget( xml, "rb_postgresql" );
    dcw->tf_host = glade_xml_get_widget( xml, "tf_host" );
	gtk_entry_set_text( GTK_ENTRY(dcw->tf_host), "localhost" );
    dcw->tf_database = glade_xml_get_widget( xml, "tf_database" );
	gtk_entry_set_text( GTK_ENTRY(dcw->tf_database), "gnucash" );
    dcw->tf_username = glade_xml_get_widget( xml, "tf_username" );
    dcw->tf_password = glade_xml_get_widget( xml, "tf_password" );

    /* Autoconnect signals */
    glade_xml_signal_autoconnect_full( xml, gnc_glade_autoconnect_full_func,
				    					dcw->dialog );

    /* Clean up the xml data structure when the dialog is destroyed */
    g_object_set_data_full( G_OBJECT(dcw->dialog), "dialog-database-connection.glade",
			 				xml, g_object_unref );
    g_object_set_data_full( G_OBJECT(dcw->dialog), "DatabaseConnectionWindow", dcw,
			 				g_free );

    /* Run the dialog */
    gtk_widget_show_all( dcw->dialog );
}

