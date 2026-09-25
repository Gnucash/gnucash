/********************************************************************
 * gnc-html-webview2-loader-state.hpp -- WebView2 loader lifetime *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#ifndef GNC_HTML_WEBVIEW2_LOADER_STATE_HPP
#define GNC_HTML_WEBVIEW2_LOADER_STATE_HPP

#include <glib.h>

class GncHtmlWebView2LoaderState final
{
public:
    using ReleaseFunc = void (*) (gpointer);

    GncHtmlWebView2LoaderState (gpointer module, ReleaseFunc release) :
        module_ (module), release_ (release) {}

    ~GncHtmlWebView2LoaderState ()
    {
        if (module_ && release_)
            release_ (module_);
    }

    gpointer module () const { return module_; }

private:
    gpointer module_ = nullptr;
    ReleaseFunc release_ = nullptr;
};

#endif /* GNC_HTML_WEBVIEW2_LOADER_STATE_HPP */
