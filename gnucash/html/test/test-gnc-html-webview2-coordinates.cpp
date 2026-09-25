/********************************************************************
 * test-gnc-html-webview2-coordinates.cpp -- coordinate tests      *
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

#include "gnc-html-webview2-coordinates.hpp"

static void
test_widget_and_surface_coordinates_reach_the_same_controller_point ()
{
    constexpr double widget_x = 12.0;
    constexpr double widget_y = 34.0;
    constexpr double widget_surface_x = 100.0;
    constexpr double widget_surface_y = 200.0;
    constexpr int scale_factor = 2;

    const auto click = gnc_html_webview2_controller_point_from_surface (
        widget_surface_x + widget_x, widget_surface_y + widget_y, scale_factor);
    const auto motion = gnc_html_webview2_controller_point_from_surface (
        widget_surface_x + widget_x, widget_surface_y + widget_y, scale_factor);
    const auto wheel = gnc_html_webview2_controller_point_from_surface (
        112.0, 234.0, scale_factor);

    g_assert_cmpint (click.x, ==, 224);
    g_assert_cmpint (click.y, ==, 468);
    g_assert_cmpint (motion.x, ==, click.x);
    g_assert_cmpint (motion.y, ==, click.y);
    g_assert_cmpint (wheel.x, ==, click.x);
    g_assert_cmpint (wheel.y, ==, click.y);
}

static void
test_fractional_scale_and_negative_coordinates_are_rounded_to_pixels ()
{
    const auto scale_1 = gnc_html_webview2_controller_point_from_surface (10.4, -10.5, 1.0);
    const auto scale_125 = gnc_html_webview2_controller_point_from_surface (10.0, -10.0, 1.25);
    const auto scale_15 = gnc_html_webview2_controller_point_from_surface (1.0, -1.0, 1.5);
    const auto scale_2 = gnc_html_webview2_controller_point_from_surface (5.25, -5.25, 2.0);

    g_assert_cmpint (scale_1.x, ==, 10);
    g_assert_cmpint (scale_1.y, ==, -11);
    g_assert_cmpint (scale_125.x, ==, 13);
    g_assert_cmpint (scale_125.y, ==, -13);
    g_assert_cmpint (scale_15.x, ==, 2);
    g_assert_cmpint (scale_15.y, ==, -2);
    g_assert_cmpint (scale_2.x, ==, 11);
    g_assert_cmpint (scale_2.y, ==, -11);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    g_test_add_func ("/html/webview2-coordinates/widget-and-surface",
                     test_widget_and_surface_coordinates_reach_the_same_controller_point);
    g_test_add_func ("/html/webview2-coordinates/fractional-scale",
                     test_fractional_scale_and_negative_coordinates_are_rounded_to_pixels);
    return g_test_run ();
}
