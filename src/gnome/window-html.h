/********************************************************************\
 * window-html.h -- an html window for gnucash.                     *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
 * Copyright (C) 2000 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 *   (gtkhtml port)                                                 *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef __WINDOW_HTML_H__
#define __WINDOW_HTML_H__

#include <gnome.h>
#include <gtkhtml/gtkhtml.h>

typedef struct _HTMLWindow HTMLWindow;
typedef struct _HTMLData   HTMLData;
typedef void *             HTMLUserData;
typedef enum { URL_TYPE_FILE, URL_TYPE_JUMP, 
               URL_TYPE_HTTP, URL_TYPE_FTP, 
               URL_TYPE_SECURE, URL_TYPE_OTHER } URLType;

typedef void (*HTMLDestroyUserDataFunc)(HTMLUserData);

typedef HTMLData * (*HTMLAnchorCB)(URLType url_type, char * location, 
                                   char * label, HTMLUserData user_data);

typedef void (*HTMLJumpCB)(HTMLUserData user_data, char ** location, 
                           char ** label);

HTMLData     * gnc_html_data_new(const char * title, 
                                 HTMLUserData user_data,
                                 HTMLDestroyUserDataFunc destroy,
                                 GnomeUIInfo * user_buttons,
                                 int num_user_buttons);

HTMLWindow   * gnc_html_window_new(HTMLAnchorCB anchor_cb, HTMLJumpCB jump_cb);
void         gnc_html_window_destroy(HTMLWindow *hw);
void         gnc_html_load(HTMLWindow *hw);
void         gnc_html_print(HTMLWindow * hw);

URLType      gnc_html_parse_url(HTMLWindow * html, const gchar * url, 
                                char ** location, char ** label);

HTMLUserData gnc_html_window_user_data(HTMLWindow *hw);
GtkWidget    * gnc_html_window_get_window(HTMLWindow *hw);


void htmlWindow(GtkWidget * parent, HTMLWindow ** hwp, HTMLData * data);

/* for showing URLS with an external browser */
void gnc_url_show(URLType type, char * location, char * label);


#endif
