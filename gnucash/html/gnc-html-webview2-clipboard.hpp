/********************************************************************
 * gnc-html-webview2-clipboard.hpp -- WebView2 clipboard helpers    *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#ifndef GNC_HTML_WEBVIEW2_CLIPBOARD_HPP
#define GNC_HTML_WEBVIEW2_CLIPBOARD_HPP

#include <gdk/gdk.h>

#include <string>
#include <string_view>
#include <utility>

enum class GncHtmlWebView2ClipboardResult
{
    no_selection,
    invalid_result,
    selected_content,
};

struct GncHtmlWebView2ClipboardSelection
{
    GncHtmlWebView2ClipboardResult result;
    std::string text;
    std::string html;
};

inline bool
gnc_html_webview2_decode_clipboard_field (std::string_view encoded, std::string& decoded)
{
    if (encoded.empty () || encoded.size () % 2 != 0)
        return false;

    decoded.clear ();
    decoded.reserve (encoded.size () / 2);
    for (std::size_t index = 0; index < encoded.size (); index += 2)
    {
        const auto high = g_ascii_xdigit_value (encoded[index]);
        const auto low = g_ascii_xdigit_value (encoded[index + 1]);
        if (high < 0 || low < 0)
            return false;
        const auto character = static_cast<char> ((high << 4) | low);
        if (!character)
            return false;
        decoded.push_back (character);
    }
    return g_utf8_validate (decoded.data (), decoded.size (), nullptr);
}

inline GncHtmlWebView2ClipboardSelection
gnc_html_webview2_decode_clipboard_selection (std::string_view result)
{
    if (result == "null")
        return {GncHtmlWebView2ClipboardResult::no_selection, {}, {}};

    if (result.size () < 7 || result.front () != '"' || result.back () != '"')
        return {GncHtmlWebView2ClipboardResult::invalid_result, {}, {}};

    const auto fields = result.substr (1, result.size () - 2);
    const auto delimiter = fields.find (':');
    if (delimiter == std::string_view::npos ||
        fields.find (':', delimiter + 1) != std::string_view::npos)
        return {GncHtmlWebView2ClipboardResult::invalid_result, {}, {}};

    GncHtmlWebView2ClipboardSelection selection {
        GncHtmlWebView2ClipboardResult::invalid_result, {}, {}};
    if (!gnc_html_webview2_decode_clipboard_field (fields.substr (0, delimiter),
                                                    selection.text) ||
        !gnc_html_webview2_decode_clipboard_field (fields.substr (delimiter + 1),
                                                    selection.html))
        return selection;

    selection.result = GncHtmlWebView2ClipboardResult::selected_content;
    return selection;
}

inline GdkContentProvider *
gnc_html_webview2_clipboard_content_provider (const GncHtmlWebView2ClipboardSelection& selection)
{
    if (selection.result != GncHtmlWebView2ClipboardResult::selected_content)
        return nullptr;

    auto plain_bytes = g_bytes_new (selection.text.data (), selection.text.size ());
    auto html_bytes = g_bytes_new (selection.html.data (), selection.html.size ());
    auto plain_provider = gdk_content_provider_new_for_bytes ("text/plain", plain_bytes);
    auto html_provider = gdk_content_provider_new_for_bytes ("text/html", html_bytes);
    g_bytes_unref (plain_bytes);
    g_bytes_unref (html_bytes);
    if (!plain_provider || !html_provider)
    {
        g_clear_object (&plain_provider);
        g_clear_object (&html_provider);
        return nullptr;
    }

    /* new_union takes ownership of the input provider references. */
    GdkContentProvider *providers[] = {plain_provider, html_provider};
    return gdk_content_provider_new_union (providers, G_N_ELEMENTS (providers));
}

#endif
