/********************************************************************
 * test-gnc-html-webview2-clipboard.cpp -- WebView2 clipboard tests*
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <config.h>

#include <glib.h>

#include "gnc-html-webview2-clipboard.hpp"

static void
test_no_selection_does_not_produce_content ()
{
    auto selection = gnc_html_webview2_decode_clipboard_selection ("null");
    g_assert_true (selection.result == GncHtmlWebView2ClipboardResult::no_selection);
    g_assert_true (selection.text.empty ());
    g_assert_true (selection.html.empty ());
    g_assert_null (gnc_html_webview2_clipboard_content_provider (selection));
}

static void
test_selected_text_and_html_are_decoded ()
{
    auto selection = gnc_html_webview2_decode_clipboard_selection (
        "\"4772c3b6c39f65:3c7374726f6e673e4772c3b6c39f653c2f7374726f6e673e3c62723e3c656d3e4d6f6e61743c2f656d3e\"");
    g_assert_true (selection.result == GncHtmlWebView2ClipboardResult::selected_content);
    g_assert_cmpstr (selection.text.c_str (), ==, "Größe");
    g_assert_cmpstr (selection.html.c_str (), ==,
                     "<strong>Größe</strong><br><em>Monat</em>");
}

static void
test_content_provider_advertises_plain_and_html ()
{
    auto selection = gnc_html_webview2_decode_clipboard_selection (
        "\"4772c3b6c39f65:3c7374726f6e673e4772c3b6c39f653c2f7374726f6e673e3c62723e3c656d3e4d6f6e61743c2f656d3e\"");
    auto provider = gnc_html_webview2_clipboard_content_provider (selection);
    g_assert_nonnull (provider);
    auto formats = gdk_content_provider_ref_formats (provider);
    g_assert_true (gdk_content_formats_contain_mime_type (formats, "text/plain"));
    g_assert_true (gdk_content_formats_contain_mime_type (formats, "text/html"));
    gdk_content_formats_unref (formats);
    g_object_unref (provider);
}

static void
test_malformed_result_does_not_produce_content ()
{
    auto selection = gnc_html_webview2_decode_clipboard_selection ("\"476:3c623e\"");
    g_assert_true (selection.result == GncHtmlWebView2ClipboardResult::invalid_result);
    g_assert_true (selection.text.empty ());
    g_assert_true (selection.html.empty ());
    g_assert_null (gnc_html_webview2_clipboard_content_provider (selection));
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    g_test_add_func ("/html/webview2-clipboard/no-selection",
                     test_no_selection_does_not_produce_content);
    g_test_add_func ("/html/webview2-clipboard/selection", test_selected_text_and_html_are_decoded);
    g_test_add_func ("/html/webview2-clipboard/mime-types",
                     test_content_provider_advertises_plain_and_html);
    g_test_add_func ("/html/webview2-clipboard/malformed-result",
                     test_malformed_result_does_not_produce_content);
    return g_test_run ();
}
