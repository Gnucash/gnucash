/********************************************************************
 * test-gnc-html-url-policy.cpp -- Report navigation policy tests   *
 *                                                                  *
 * Copyright 2026 GnuCash Contributors                              *
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
 ********************************************************************/

#include <gtk/gtk.h>

#include "gnc-html.h"

typedef struct
{
    GncHtml parent_instance;
    guint cancel_calls;
    gchar *type;
    gchar *location;
    gchar *label;
    gboolean new_window;
} TestHtml;

typedef struct
{
    GncHtmlClass parent_class;
} TestHtmlClass;

GType test_html_get_type (void);

G_DEFINE_TYPE (TestHtml, test_html, GNC_TYPE_HTML)

static void
test_html_show_url (GncHtml *html, URLType type, const gchar *location,
                    const gchar *label, gboolean new_window)
{
    auto test_html = reinterpret_cast<TestHtml *> (html);

    g_free (test_html->type);
    g_free (test_html->location);
    g_free (test_html->label);
    test_html->type = g_strdup (type);
    test_html->location = g_strdup (location);
    test_html->label = g_strdup (label);
    test_html->new_window = new_window;
}

static void
test_html_cancel (GncHtml *html)
{
    auto test_html = reinterpret_cast<TestHtml *> (html);

    test_html->cancel_calls++;
}

static void
test_html_dispose (GObject *object)
{
    auto test_html = reinterpret_cast<TestHtml *> (object);

    g_clear_pointer (&test_html->type, g_free);
    g_clear_pointer (&test_html->location, g_free);
    g_clear_pointer (&test_html->label, g_free);

    G_OBJECT_CLASS (test_html_parent_class)->dispose (object);
}

static void
test_html_class_init (TestHtmlClass *klass)
{
    auto object_class = G_OBJECT_CLASS (klass);
    auto html_class = GNC_HTML_CLASS (klass);

    object_class->dispose = test_html_dispose;
    html_class->show_url = test_html_show_url;
    html_class->cancel = test_html_cancel;
}

static void
test_html_init (TestHtml *)
{
}
static void
test_internal_url_types (void)
{
    const URLType types[] =
    {
        URL_TYPE_REGISTER,
        URL_TYPE_ACCTTREE,
        URL_TYPE_REPORT,
        URL_TYPE_OPTIONS,
        URL_TYPE_SCHEME,
        URL_TYPE_HELP,
        URL_TYPE_XMLDATA,
        URL_TYPE_PRICE,
        URL_TYPE_BUDGET,
    };

    for (guint index = 0; index < G_N_ELEMENTS (types); index++)
        g_assert_true (gnc_html_urltype_is_internal (types[index]));
}

static void
test_renderer_url_types_are_not_internal (void)
{
    const URLType types[] =
    {
        URL_TYPE_FILE,
        URL_TYPE_JUMP,
        URL_TYPE_HTTP,
        URL_TYPE_FTP,
        URL_TYPE_SECURE,
        URL_TYPE_OTHER,
        "mailto",
        "javascript",
    };

    for (guint index = 0; index < G_N_ELEMENTS (types); index++)
        g_assert_false (gnc_html_urltype_is_internal (types[index]));
}

static void
test_internal_url_dispatches_through_controller (void)
{
    auto html = GNC_HTML (g_object_new (test_html_get_type (), nullptr));
    auto test_html = reinterpret_cast<TestHtml *> (html);

    g_assert_null (gnc_html_get_widget (html));
    gnc_html_initialize ();
    g_assert_true (gnc_html_handle_internal_url (html, "gnc-report:income#details", TRUE));
    g_assert_cmpstr (test_html->type, ==, URL_TYPE_REPORT);
    g_assert_cmpstr (test_html->location, ==, "income");
    g_assert_cmpstr (test_html->label, ==, "details");
    g_assert_true (test_html->new_window);

    g_assert_false (gnc_html_handle_internal_url (html, "https://example.invalid", FALSE));
    g_assert_cmpstr (test_html->location, ==, "income");

    g_assert_false (gnc_html_handle_internal_url (html, "gnc-report:", FALSE));
    g_assert_cmpstr (test_html->location, ==, "income");

    gnc_html_cancel (html);
    g_assert_cmpuint (test_html->cancel_calls, ==, 1);

    gnc_html_destroy (html);
}
int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/html/url-policy/internal", test_internal_url_types);
    g_test_add_func ("/html/url-policy/renderer",
                     test_renderer_url_types_are_not_internal);
    g_test_add_func ("/html/url-policy/internal-dispatch",
                     test_internal_url_dispatches_through_controller);

    return g_test_run ();
}
