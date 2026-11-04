/*
 * Currency search widget integration test.
 * Copyright (C) 2026 Étienne Beaulé
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 */

#include <config.h>
#include <memory>
#include <gtest/gtest.h>
#include <gtk/gtk.h>

#include "gnc-currency-edit.h"
#include "gnc-engine.h"
#include "gnc-session.h"
#include "gncInvoice.h"
#include "search-currency.h"
#include "search-core-utils.h"

static bool s_have_display;

namespace
{
struct WidgetDeleter
{
    void operator() (GtkWidget *widget) const
    {
        gtk_widget_destroy (widget);
        g_object_unref (widget);
    }
};

using WidgetPtr = std::unique_ptr<GtkWidget, WidgetDeleter>;
using SearchPtr = std::unique_ptr<GNCSearchCoreType, decltype (&g_object_unref)>;

WidgetPtr
make_widget (GNCSearchCoreType& search)
{
    auto widget{gnc_search_core_type_get_widget (&search)};
    g_object_ref_sink (widget);
    return WidgetPtr{widget};
}

struct Controls
{
    GtkComboBox *m_comparison{};
    GNCCurrencyEdit *m_picker{};
};

Controls
get_controls (GtkWidget& widget)
{
    Controls controls;
    gtk_container_foreach (GTK_CONTAINER (&widget),
        [](GtkWidget *child, gpointer data)
        {
            auto& controls{*static_cast<Controls*> (data)};
            if (GNC_IS_CURRENCY_EDIT (child))
                controls.m_picker = GNC_CURRENCY_EDIT (child);
            else if (GTK_IS_COMBO_BOX (child))
                controls.m_comparison = GTK_COMBO_BOX (child);
        }, &controls);
    return controls;
}

class CurrencySearchTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        if (!s_have_display)
            GTEST_SKIP () << "Currency picker requires a GTK display";
        m_book = qof_session_get_book (gnc_get_current_session ());
        auto table{gnc_commodity_table_get_table (m_book)};
        m_usd = gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY, "USD");
        m_cad = gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY, "CAD");
        ASSERT_NE (m_usd, nullptr);
        ASSERT_NE (m_cad, nullptr);
        m_usd_invoice = gncInvoiceCreate (m_book);
        m_cad_invoice = gncInvoiceCreate (m_book);
        gncInvoiceSetCurrency (m_usd_invoice, m_usd);
        gncInvoiceSetCurrency (m_cad_invoice, m_cad);
        m_search.reset (gnc_search_core_type_new_type_name (CURRENCY_MATCH_TYPE));
        m_widget = make_widget (*m_search);
        m_controls = get_controls (*m_widget);
        ASSERT_NE (m_controls.m_picker, nullptr);
        ASSERT_NE (m_controls.m_comparison, nullptr);
    }

    void TearDown () override
    {
        m_widget.reset ();
        m_search.reset ();
        gnc_clear_current_session ();
    }

    void assert_match (GNCSearchCoreType& search, GncInvoice& expected)
    {
        auto predicate{gnc_search_core_type_get_predicate (&search)};
        ASSERT_NE (predicate, nullptr);
        std::unique_ptr<QofQuery, decltype (&qof_query_destroy)> query{
            qof_query_create_for (GNC_ID_INVOICE), qof_query_destroy};
        qof_query_set_book (query.get (), m_book);
        qof_query_add_term (query.get (), qof_query_build_param_list (INVOICE_CURRENCY, nullptr),
                            predicate, QOF_QUERY_AND);
        auto results{qof_query_run (query.get ())};
        ASSERT_EQ (g_list_length (results), 1u);
        EXPECT_EQ (results->data, &expected);
    }

    QofBook *m_book{};
    gnc_commodity *m_usd{}, *m_cad{};
    GncInvoice *m_usd_invoice{}, *m_cad_invoice{};
    SearchPtr m_search{nullptr, g_object_unref};
    WidgetPtr m_widget;
    Controls m_controls;
};

TEST_F (CurrencySearchTest, SelectionComparisonAndCopy)
{
    EXPECT_EQ (gnc_search_core_type_get_predicate (m_search.get ()), nullptr);
    gnc_currency_edit_set_currency (m_controls.m_picker, m_usd);
    assert_match (*m_search, *m_usd_invoice);
    gnc_combo_box_search_set_active (m_controls.m_comparison, QOF_COMPARE_NEQ);
    assert_match (*m_search, *m_cad_invoice);

    SearchPtr copy{gnc_search_core_type_clone (m_search.get ()), g_object_unref};
    auto copy_widget{make_widget (*copy)};
    auto controls{get_controls (*copy_widget)};
    ASSERT_NE (controls.m_picker, nullptr);
    gnc_currency_edit_set_currency (m_controls.m_picker, m_cad);
    assert_match (*m_search, *m_usd_invoice);
    assert_match (*copy, *m_cad_invoice);

    gtk_combo_box_set_active (GTK_COMBO_BOX (controls.m_picker), -1);
    EXPECT_EQ (gnc_search_core_type_get_predicate (copy.get ()), nullptr);
}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    s_have_display = gtk_init_check (&argc, &argv);
    gnc_engine_init_static (argc, argv);
    gnc_search_core_initialize ();
    auto result{RUN_ALL_TESTS ()};
    gnc_search_core_finalize ();
    gnc_engine_shutdown ();
    return result;
}
