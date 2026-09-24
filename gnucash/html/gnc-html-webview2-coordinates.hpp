/********************************************************************
 * gnc-html-webview2-coordinates.hpp -- WebView2 point helpers     *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#ifndef GNC_HTML_WEBVIEW2_COORDINATES_HPP
#define GNC_HTML_WEBVIEW2_COORDINATES_HPP

#include <cmath>

struct GncHtmlWebView2ControllerPoint
{
    long x;
    long y;
};

inline GncHtmlWebView2ControllerPoint
gnc_html_webview2_controller_point_from_surface (double surface_x, double surface_y,
                                                  double scale_factor)
{
    return {std::lround (surface_x * scale_factor),
            std::lround (surface_y * scale_factor)};
}

#endif
