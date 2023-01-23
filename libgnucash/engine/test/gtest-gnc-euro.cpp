/********************************************************************
 * gtest-gnc-euro.cpp -- unit tests for Euro currency functions     *
 * Copyright 2022 John Ralls <jralls@ceridwen.us>                   *
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

#include <gtest/gtest.h>
#include "../gnc-numeric.hpp"
#include <config.h>
#include "../gnc-euro.h"
#include "../gnc-commodity.h"
#include "../gnc-session.h"

class Currencies : public ::testing::Test
{
protected:
    gnc_commodity_table* m_table;
    Currencies() : m_table{gnc_commodity_table_new()}
    {
        QofBook* book = qof_session_get_book (gnc_get_current_session ());
        qof_book_set_data (book, GNC_COMMODITY_TABLE, m_table);

        if (!gnc_commodity_table_add_default_data(m_table, book))
            exit(-1);
    }

    ~Currencies() { gnc_commodity_table_destroy(m_table); }
};

static const char* currency{"CURRENCY"};

TEST_F(Currencies, is_euro_currency)
{
    auto usd = gnc_commodity_table_lookup(m_table, currency, "USD");
    auto eur = gnc_commodity_table_lookup(m_table, currency, "EUR");
    auto pte = gnc_commodity_table_lookup(m_table, currency, "PTE");

    ASSERT_NE(eur, nullptr);
    ASSERT_NE(usd, nullptr);
    ASSERT_NE(pte, nullptr);

    EXPECT_TRUE(gnc_is_euro_currency(pte));
    EXPECT_TRUE(gnc_is_euro_currency(eur));
    EXPECT_FALSE(gnc_is_euro_currency(usd));
}

TEST_F(Currencies, convert_to_euro)
{
    gnc_numeric value{314159, 100};
    gnc_numeric cyp_eur_amount{536776, 100}; // calc gets 5367.76
    auto cyp{gnc_commodity_table_lookup(m_table, currency, "CYP")};
    ASSERT_NE(cyp, nullptr);
    auto amount{gnc_convert_to_euro(cyp, value)};
    EXPECT_EQ(cyp_eur_amount.num, amount.num);
    EXPECT_EQ(cyp_eur_amount.denom, amount.denom);
    EXPECT_TRUE(gnc_numeric_equal(cyp_eur_amount, amount));

    gnc_numeric grd_eur_amount{922, 100};
    auto grd{gnc_commodity_table_lookup(m_table, currency, "GRD")};
    amount = gnc_convert_to_euro(grd, value);
    EXPECT_EQ(grd_eur_amount.num, amount.num);
    EXPECT_EQ(grd_eur_amount.denom, amount.denom);
    EXPECT_TRUE(gnc_numeric_equal(grd_eur_amount, amount));

    gnc_numeric itl_eur_amount{162, 100};
    auto itl{gnc_commodity_table_lookup(m_table, currency, "ITL")};
    amount = gnc_convert_to_euro(itl, value);
    EXPECT_EQ(itl_eur_amount.num, amount.num);
    EXPECT_EQ(itl_eur_amount.denom, amount.denom);
    EXPECT_TRUE(gnc_numeric_equal(itl_eur_amount, amount));

    gnc_numeric nlg_eur_amount{142559, 100};
    auto nlg{gnc_commodity_table_lookup(m_table, currency, "NLG")};
    amount = gnc_convert_to_euro(nlg, value);
    EXPECT_EQ(nlg_eur_amount.num, amount.num);
    EXPECT_EQ(nlg_eur_amount.denom, amount.denom);
    EXPECT_TRUE(gnc_numeric_equal(nlg_eur_amount, amount));

    auto eur{gnc_commodity_table_lookup(m_table, currency, "EUR")};
    ASSERT_NE(eur, nullptr);
    EXPECT_TRUE(gnc_numeric_equal(value,
                               gnc_convert_to_euro(eur, value)));
}

TEST_F(Currencies, convert_from_euro)
{
    gnc_numeric value{314159, 100}; //.787564 * 3141.59 = 2472.20
    gnc_numeric eur_iep_amount{247419, 100};
    auto iep{gnc_commodity_table_lookup(m_table, currency, "IEP")};
    ASSERT_NE(iep, nullptr);
    auto amount{gnc_convert_from_euro(iep, value)};
    EXPECT_EQ(eur_iep_amount.num, amount.num);
    EXPECT_EQ(eur_iep_amount.denom, amount.denom);
    EXPECT_TRUE(gnc_numeric_equal(eur_iep_amount, amount));

    gnc_numeric eur_itl_amount{6082966, 1};
    auto itl{gnc_commodity_table_lookup(m_table, currency, "ITL")};
    amount = gnc_convert_from_euro(itl, value);
    EXPECT_EQ(eur_itl_amount.num, amount.num);
    EXPECT_EQ(eur_itl_amount.denom, amount.denom);
    EXPECT_TRUE(gnc_numeric_equal(eur_itl_amount, amount));

    gnc_numeric eur_fim_amount{1867905, 100};
    auto fim{gnc_commodity_table_lookup(m_table, currency, "FIM")};
    EXPECT_NE(fim, nullptr);
    amount = gnc_convert_from_euro(fim, value);
    EXPECT_EQ(eur_fim_amount.num, amount.num);
    EXPECT_EQ(eur_fim_amount.denom, amount.denom);
    EXPECT_TRUE(gnc_numeric_equal(eur_fim_amount, amount));

    gnc_numeric eur_dm_amount{614442, 100};
    auto dm{gnc_commodity_table_lookup(m_table, currency, "DEM")};
    EXPECT_NE(dm, nullptr);
    amount = gnc_convert_from_euro(dm, value);
    EXPECT_EQ(eur_dm_amount.num, amount.num);
    EXPECT_EQ(eur_dm_amount.denom, amount.denom);
    EXPECT_TRUE(gnc_numeric_equal(eur_dm_amount, amount));

    auto eur{gnc_commodity_table_lookup(m_table, currency, "EUR")};
    EXPECT_TRUE(gnc_numeric_equal(value,
                               gnc_convert_from_euro(eur, value)));
}

TEST_F(Currencies, euro_currency_get_rate)
{
    auto ats{gnc_commodity_table_lookup(m_table, currency, "ATS")};
    ASSERT_NE(ats, nullptr);
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{137603, 10000},
                                  gnc_euro_currency_get_rate(ats)));

    auto bef{gnc_commodity_table_lookup(m_table, currency, "BEF")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{403399, 10000},
                                  gnc_euro_currency_get_rate(bef)));

    auto cyp{gnc_commodity_table_lookup(m_table, currency, "CYP")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{585274, 1000000},
                                  gnc_euro_currency_get_rate(cyp)));

    auto dem{gnc_commodity_table_lookup(m_table, currency, "DEM")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{195583, 100000},
                                  gnc_euro_currency_get_rate(dem)));

    auto eek{gnc_commodity_table_lookup(m_table, currency, "EEK")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{156466, 10000},
                                  gnc_euro_currency_get_rate(eek)));

    auto esp{gnc_commodity_table_lookup(m_table, currency, "ESP")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{166386, 1000},
                                  gnc_euro_currency_get_rate(esp)));

    auto eur{gnc_commodity_table_lookup(m_table, currency, "EUR")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{100000, 100000},
                                  gnc_euro_currency_get_rate(eur)));

    auto fim{gnc_commodity_table_lookup(m_table, currency, "FIM")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{594573, 100000},
                                  gnc_euro_currency_get_rate(fim)));


    auto frf{gnc_commodity_table_lookup(m_table, currency, "FRF")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{655957, 100000},
                                  gnc_euro_currency_get_rate(frf)));

    auto grd{gnc_commodity_table_lookup(m_table, currency, "GRD")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{340750, 1000},
                                  gnc_euro_currency_get_rate(grd)));


    auto hrk{gnc_commodity_table_lookup(m_table, currency, "HRK")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{753450, 100000},
                                  gnc_euro_currency_get_rate(hrk)));

    auto iep{gnc_commodity_table_lookup(m_table, currency, "IEP")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{787564, 1000000},
                                  gnc_euro_currency_get_rate(iep)));

    auto itl{gnc_commodity_table_lookup(m_table, currency, "ITL")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{193627, 100},
                                  gnc_euro_currency_get_rate(itl)));

    auto luf{gnc_commodity_table_lookup(m_table, currency, "LUF")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{403399, 10000},
                                  gnc_euro_currency_get_rate(luf)));

    auto lvl{gnc_commodity_table_lookup(m_table, currency, "LVL")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{702804, 1000000},
                                  gnc_euro_currency_get_rate(lvl)));

    auto mtl{gnc_commodity_table_lookup(m_table, currency, "MTL")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{429300, 1000000},
                                  gnc_euro_currency_get_rate(mtl)));

    auto nlg{gnc_commodity_table_lookup(m_table, currency, "NLG")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{220371, 100000},
                                  gnc_euro_currency_get_rate(nlg)));

    auto pte{gnc_commodity_table_lookup(m_table, currency, "PTE")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{200482, 1000},
                                  gnc_euro_currency_get_rate(pte)));

    auto sit{gnc_commodity_table_lookup(m_table, currency, "SIT")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{239640, 1000},
                                  gnc_euro_currency_get_rate(sit)));

    auto skk{gnc_commodity_table_lookup(m_table, currency, "SKK")};
    EXPECT_TRUE(gnc_numeric_equal(gnc_numeric{301260, 10000},
                                  gnc_euro_currency_get_rate(skk)));

}
