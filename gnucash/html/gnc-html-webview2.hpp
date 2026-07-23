/********************************************************************
 * gnc-html-webview2.hpp -- display html with gnc special tags        *
 * Copyright (C) 2026 John Ralls <jralls@ceridwen.us>                *
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

/* GncHtml backend that hosts Microsoft Edge WebView2 as a native child
 * window parented to a plain GTK widget's own natively-backed HWND
 * (see gdk_window_ensure_native()). Windows-only;
 */

#ifndef GNC_HTML_WEBVIEW2_H
#define GNC_HTML_WEBVIEW2_H

#include <glib-object.h>
#include "gnc-html.h"

G_BEGIN_DECLS

#define GNC_TYPE_HTML_WEBVIEW2       (gnc_html_webview2_get_type())
#define GNC_HTML_WEBVIEW2(o)         (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_HTML_WEBVIEW2, GncHtmlWebview2))
#define GNC_HTML_WEBVIEW2_CLASS(k)   (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_HTML_WEBVIEW2, GncHtmlWebview2Class))
#define GNC_IS_HTML_WEBVIEW2(o)      (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_HTML_WEBVIEW2))
#define GNC_IS_HTML_WEBVIEW2_CLASS(k)   (G_TYPE_CHECK_CLASS_TYPE((k), GNC_TYPE_HTML_WEBVIEW2))
#define GNC_HTML_WEBVIEW2_GET_CLASS(o)  (G_TYPE_INSTANCE_GET_CLASS((o), GNC_TYPE_HTML_WEBVIEW2, GncHtmlWebview2Class))

struct GncHtmlWebview2Private;

struct GncHtmlWebview2
{
    GncHtml parent_instance;

    /*< private >*/
    GncHtmlWebview2Private* priv;
};

struct GncHtmlWebview2Class
{
    GncHtmlClass parent_class;
};

GType gnc_html_webview2_get_type( void );
GncHtml* gnc_html_webview2_new( void ) NOEXCEPT;

G_END_DECLS

#endif // GNC_HTML_WEBVIEW2_H
