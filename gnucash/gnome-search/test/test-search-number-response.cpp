/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <cstdlib>
#include "qof.h"
#include "gnc-amount-edit.h"
#include "gnc-exp-parser.h"

/* These C headers do not yet provide C++ linkage guards. */
extern "C"
{
#include "search-double.h"
#include "search-int64.h"
#include "search-numeric.h"
}

static gboolean display_available;

static GtkWidget *
find_amount (GtkWidget *widget)
{
    if (GNC_IS_AMOUNT_EDIT (widget))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_amount (GTK_WIDGET (node->data));
    g_list_free (children);
    return found;
}

static GtkWidget *
find_notice (GtkWidget *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == GTK_WINDOW (parent))
        {
            g_assert_null (found);
            found = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return found;
}

static void
test_validation (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto mode = GPOINTER_TO_INT (data);
    GNCSearchCoreType *core = nullptr;
    switch (mode % 3)
    {
    case 0: core = GNC_SEARCH_CORE_TYPE (gnc_search_double_new ()); break;
    case 1: core = GNC_SEARCH_CORE_TYPE (gnc_search_int64_new ()); break;
    case 2: core = GNC_SEARCH_CORE_TYPE (gnc_search_numeric_new ()); break;
    }
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gnc_search_core_type_pass_parent (core, parent);
    auto widget = gnc_search_core_type_get_widget (core);
    gtk_container_add (GTK_CONTAINER (parent), widget);
    auto amount = find_amount (widget);
    g_assert_true (GNC_IS_AMOUNT_EDIT (amount));
    auto entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (amount));
    gtk_entry_set_text (GTK_ENTRY (entry), "1");
    g_assert_true (gnc_search_core_type_validate (core));
    g_assert_null (find_notice (parent));
    gtk_entry_set_text (GTK_ENTRY (entry), "(");
    g_assert_false (gnc_search_core_type_validate (core));
    auto notice = find_notice (parent);
    g_assert_nonnull (notice);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (notice)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (notice)));
    /* The response does not retain the validator or its input storage. */
    gtk_widget_destroy (widget);
    g_object_unref (core);
    if (mode >= 3)
        gtk_widget_destroy (parent);
    else
    {
        gtk_dialog_response (GTK_DIALOG (notice), GTK_RESPONSE_CLOSE);
        g_assert_null (find_notice (parent));
        gtk_widget_destroy (parent);
    }
}

static int
run_tests (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    /* No saved user variables are needed for these literal test expressions. */
    gnc_exp_parser_real_init (FALSE);
    g_test_add_data_func ("/gnome-search/number/double-close", GINT_TO_POINTER (0), test_validation);
    g_test_add_data_func ("/gnome-search/number/int64-close", GINT_TO_POINTER (1), test_validation);
    g_test_add_data_func ("/gnome-search/number/numeric-close", GINT_TO_POINTER (2), test_validation);
    g_test_add_data_func ("/gnome-search/number/double-parent", GINT_TO_POINTER (3), test_validation);
    g_test_add_data_func ("/gnome-search/number/int64-parent", GINT_TO_POINTER (4), test_validation);
    g_test_add_data_func ("/gnome-search/number/numeric-parent", GINT_TO_POINTER (5), test_validation);
    auto result = g_test_run ();
    gnc_exp_parser_shutdown ();
    qof_close ();
    return result;
}

static void
guile_main ([[maybe_unused]] void *data, int argc, char **argv)
{
    std::exit (run_tests (argc, argv));
}

int
main (int argc, char **argv)
{
    scm_boot_guile (argc, argv, guile_main, nullptr);
    return 0;
}
