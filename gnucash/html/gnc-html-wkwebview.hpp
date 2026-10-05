/********************************************************************
 * gnc-html-wkwebview.hpp -- display html with gnc special tags      *
 * Copyright (C) 2026 Christopher Lam                               *
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

/* GncHtml backend that hosts a WKWebView -- the system WebKit view that
 * Safari itself uses -- as a native NSView subview of a plain GTK
 * widget's own natively-backed GdkQuartzView (see
 * gdk_window_ensure_native() and gdk_quartz_window_get_nsview()).
 * macOS-only, and only with a GTK built against the Quartz GDK backend.
 */

#ifndef GNC_HTML_WKWEBVIEW_H
#define GNC_HTML_WKWEBVIEW_H

#include <glib-object.h>
#include "gnc-html.h"

G_BEGIN_DECLS

#define GNC_TYPE_HTML_WKWEBVIEW      (gnc_html_wkwebview_get_type())
#define GNC_HTML_WKWEBVIEW(o)        (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_HTML_WKWEBVIEW, GncHtmlWkwebview))
#define GNC_HTML_WKWEBVIEW_CLASS(k)  (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_HTML_WKWEBVIEW, GncHtmlWkwebviewClass))
#define GNC_IS_HTML_WKWEBVIEW(o)     (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_HTML_WKWEBVIEW))
#define GNC_IS_HTML_WKWEBVIEW_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE((k), GNC_TYPE_HTML_WKWEBVIEW))
#define GNC_HTML_WKWEBVIEW_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS((o), GNC_TYPE_HTML_WKWEBVIEW, GncHtmlWkwebviewClass))

struct GncHtmlWkwebviewPrivate;

struct GncHtmlWkwebview
{
    GncHtml parent_instance;

    /*< private >*/
    GncHtmlWkwebviewPrivate* priv;
};

struct GncHtmlWkwebviewClass
{
    GncHtmlClass parent_class;
};

GType gnc_html_wkwebview_get_type( void );
GncHtml* gnc_html_wkwebview_new( void ) NOEXCEPT;

G_END_DECLS

#endif // GNC_HTML_WKWEBVIEW_H
