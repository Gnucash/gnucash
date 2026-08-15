/********************************************************************
 * gnc-html-webview2-p.hpp -- display html with gnc special tags      *
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

#ifndef GNC_HTML_WEBVIEW2_P_H
#define GNC_HTML_WEBVIEW2_P_H

#include "gnc-html-p.h"

#include <windows.h>
#include <WebView2.h>

enum GncWebview2PendingKind
{
    PENDING_NONE,
    PENDING_URI,
    PENDING_STRING
};

struct GncHtmlWebview2Private
{
    GncHtmlPrivate base;

    /* Plain GTK widget forced to own a private, natively-backed HWND
     * (see gdk_window_ensure_native()). The WebView2 controller is
     * parented directly to that HWND and its bounds are kept in sync
     * with the widget's allocation, so it paints itself as a child
     * window exactly covering this widget -- not literally Cairo
     * content, but visually indistinguishable from it. */
    GtkWidget* socket;

    ICoreWebView2Environment* environment;
    ICoreWebView2Controller* controller;
    ICoreWebView2* webview;
    EventRegistrationToken nav_starting_token;
    gboolean has_nav_starting_token;
    EventRegistrationToken new_window_token;
    gboolean has_new_window_token;

    /* Only available on newer WebView2 runtimes (ICoreWebView2_11);
     * nullptr if the QueryInterface for it failed, in which case the
     * context menu is simply never customized. */
    ICoreWebView2_11* webview11;
    EventRegistrationToken context_menu_token;
    gboolean has_context_menu_token;

    gboolean environment_creating;

    /* Set immediately before a Navigate()/NavigateToString() call that
     * originates from our own code (loading a report, showing an error
     * page, etc.), so the NavigationStarting handler can tell those
     * apart from the user clicking a link inside the rendered page. */
    gboolean navigating_internally;

    gboolean disposed;

    gchar* html_string; /* last html shown; used for export/print */

    /* Environment/controller creation is asynchronous, but callers
     * routinely call gnc_html_show_url()/gnc_html_show_data()
     * synchronously right after construction (e.g. to load the initial
     * report). Stash the most recent such request here and replay it
     * once the controller becomes ready. */
    GncWebview2PendingKind pending_kind;
    gchar* pending_payload;
};

#endif
