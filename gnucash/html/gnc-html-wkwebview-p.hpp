/********************************************************************
 * gnc-html-wkwebview-p.hpp -- display html with gnc special tags    *
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

#ifndef GNC_HTML_WKWEBVIEW_P_H
#define GNC_HTML_WKWEBVIEW_P_H

#include "gnc-html-p.h"
#include "gnc-html-support.hpp"

/* The Objective-C types are only nameable when this header is pulled
 * into an Objective-C++ translation unit. Everywhere else -- SWIG's
 * dependency scan, the installed-headers check, an ordinary C++ build
 * that happens to include it -- they degrade to void*, so the header
 * still parses. */
#ifdef __OBJC__
@class WKWebView;
@class GncWkWebViewDelegate;
using GncWkWebViewRef = WKWebView*;
using GncWkWebViewDelegateRef = GncWkWebViewDelegate*;
#else
using GncWkWebViewRef = void*;
using GncWkWebViewDelegateRef = void*;
#endif

struct GncHtmlWkwebviewPrivate
{
    GncHtmlPrivate base;

    /* Plain GTK widget forced to own a private, natively-backed
     * GdkWindow (see gdk_window_ensure_native()), whose GdkQuartzView
     * becomes the WKWebView's superview. The WKWebView's frame is kept
     * in sync with the widget's allocation, so it paints itself exactly
     * over this widget -- not literally Cairo content, but visually
     * indistinguishable from it. */
    GtkWidget* socket;

    /* Retained manually: this file is compiled without ARC, because the
     * private struct lives in g_realloc()ed memory that ARC is not
     * allowed to manage. */
    GncWkWebViewRef web_view;
    GncWkWebViewDelegateRef delegate;

    /* Set immediately before a load that originates from our own code
     * (showing a report, an error page, ...), so the navigation-policy
     * delegate can tell those apart from the user clicking a link
     * inside the rendered page. */
    gboolean navigating_internally;

    gboolean disposed;

    gchar* html_string;     /* last html shown; used for export/print */

    /* Anchor the next show_data() should scroll to, handed over by
     * load_to_stream(). It becomes the fragment of the temporary
     * file's URI, which is the only point at which WKWebView will act
     * on it. */
    gchar* anchor;

    /* The WKWebView exists from realize onwards, but a load issued
     * before then -- gnc_html_show_url() is routinely called straight
     * after construction -- has nowhere to go yet, so it is stashed
     * here and replayed once the view is up. Heap-allocated because
     * this struct is g_realloc()ed rather than constructed. */
    gnc::html::PendingNavigation* pending;
};

#endif
