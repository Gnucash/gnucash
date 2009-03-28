/********************************************************************
 * gnc-html_factory.c -- Factory to create HTML component           *
 *                                                                  *
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>        *
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
 ********************************************************************/

#include "config.h"

#include <gtk/gtk.h>

#include "gnc-html.h"
#include "gnc-html-gtkhtml.h"
#include "gnc-html-webkit.h"
#include "gnc-html-gtkmozembed.h"
#include "qoflog.h"
#include "gnc-engine.h"

#include "gnc-html-factory.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

GncHtml* gnc_html_factory_create_html( void )
{
	const gchar* html_type = g_getenv( "GNC_HTML" );

	if( html_type == NULL ) {
		return gnc_html_gtkhtml_new();
	} else if( strcmp( html_type, "webkit" ) == 0 ) {
		return gnc_html_webkit_new();
	} else if( strcmp( html_type, "gtkhtml" ) == 0 ) {
		return gnc_html_gtkhtml_new();
	} else if( strcmp( html_type, "gtkmozembed" ) == 0 ) {
		return gnc_html_gtkmozembed_new();
	} else {
		return gnc_html_gtkhtml_new();
	}
}
