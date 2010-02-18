/********************************************************************
 * gnc-html-extras.h -- display html with gnc special tags          *
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
\********************************************************************/

#ifndef GNC_HTML_EXTRAS_H
#define GNC_HTML_EXTRAS_H

// This file is needed so that these definitions can be separately included in the
// gnc-html.i file.  The full gnc-html.h file can't be parsed because of the new
// use of GObject

typedef gchar* URLType;

#define URL_TYPE_FILE	"file"
#define URL_TYPE_JUMP	"jump"
#define URL_TYPE_HTTP	"http"
#define URL_TYPE_FTP	"ftp"
#define URL_TYPE_SECURE	"secure"
#define URL_TYPE_REGISTER	"register"   /* for gnucash register popups */
#define URL_TYPE_ACCTTREE	"accttree"   /* for account tree windows */
#define URL_TYPE_REPORT	"report"     /* for gnucash report popups */
#define URL_TYPE_OPTIONS	"options"    /* for editing report options */
#define URL_TYPE_SCHEME	"scheme"     /* for scheme code evaluation */
#define URL_TYPE_HELP	"help"       /* for a gnucash help window */
#define URL_TYPE_XMLDATA	"xmldata"    /* links to gnucash XML data files */
#define URL_TYPE_PRICE	"price"      /* for price editor popups */
#define URL_TYPE_OTHER	"other"
#define URL_TYPE_BUDGET "budget"

gchar* gnc_build_url( URLType type, const gchar* location, const gchar* label );

gboolean gnc_html_engine_supports_css( void );

#endif
