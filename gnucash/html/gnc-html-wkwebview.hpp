/********************************************************************
 * gnc-html-wkwebview.hpp -- display reports with Apple WKWebView  *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#ifndef GNC_HTML_WKWEBVIEW_HPP
#define GNC_HTML_WKWEBVIEW_HPP

#include <glib-object.h>

#include "gnc-html.h"

G_BEGIN_DECLS

#define GNC_TYPE_HTML_WKWEBVIEW       (gnc_html_wkwebview_get_type ())
#define GNC_HTML_WKWEBVIEW(o)         (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_HTML_WKWEBVIEW, GncHtmlWKWebView))
#define GNC_IS_HTML_WKWEBVIEW(o)      (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_HTML_WKWEBVIEW))

struct GncHtmlWKWebViewPrivate;

struct GncHtmlWKWebView
{
    GncHtml parent_instance;

    /*< private >*/
    GncHtmlWKWebViewPrivate *priv;
};

struct GncHtmlWKWebViewClass
{
    GncHtmlClass parent_class;
};

GType gnc_html_wkwebview_get_type (void);
GncHtml *gnc_html_wkwebview_new (void) NOEXCEPT;

G_END_DECLS

#endif /* GNC_HTML_WKWEBVIEW_HPP */
