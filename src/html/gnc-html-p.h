/********************************************************************
 * gnc-html-p.h -- display html with gnc special tags               *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#ifndef GNC_HTML_P_H
#define GNC_HTML_P_H

struct _GncHtmlPrivate {
	GtkWidget* parent;				/* window this html goes into */
	GtkWidget* container;			/* parent of the gtkhtml widget */
	gchar* current_link;			/* link under mouse pointer */

	URLType base_type;				/* base of URL (path - filename) */
	gchar* base_location;

	GHashTable* request_info;		/* hash uri to GList of GtkHTMLStream * */

	/* callbacks */
	GncHTMLUrltypeCB urltype_cb;	/* is this type OK for this instance? */
	GncHTMLLoadCB load_cb;
	GncHTMLFlyoverCB flyover_cb;
	GncHTMLButtonCB button_cb;

	gpointer flyover_cb_data;
	gpointer load_cb_data;
	gpointer button_cb_data;

	gnc_html_history * history;
};

#endif
