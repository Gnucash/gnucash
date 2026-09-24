/********************************************************************
 * gnc-html-webview2-visibility.hpp -- WebView2 host visibility     *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#ifndef GNC_HTML_WEBVIEW2_VISIBILITY_HPP
#define GNC_HTML_WEBVIEW2_VISIBILITY_HPP

#include <gtk/gtk.h>

inline gboolean
gnc_html_webview2_host_is_mapped (GtkWidget *host)
{
    return host && gtk_widget_get_mapped (host);
}

#endif
