/********************************************************************
 * test-gnc-html-webview2-loader-state.cpp -- loader lifetime test *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <glib.h>

#include <memory>

#include "gnc-html-webview2-loader-state.hpp"

static guint releases = 0;

static void
record_release (gpointer)
{
    ++releases;
}

static void
test_loader_is_held_by_async_callback (void)
{
    releases = 0;
    auto owner = std::make_shared<GncHtmlWebView2LoaderState> (
        GINT_TO_POINTER (1), record_release);
    auto callback = owner;

    owner.reset ();
    g_assert_cmpuint (releases, ==, 0);

    callback.reset ();
    g_assert_cmpuint (releases, ==, 1);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    g_test_add_func ("/html/webview2-loader/async-callback-lease",
                     test_loader_is_held_by_async_callback);
    return g_test_run ();
}
