/********************************************************************\
 * gtest-cashobjects.cpp -- Unit tests for cashobjects.cpp          *
 *                                                                  *
 * Copyright 2024 GnuCash team                                      *
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
 *                                                                  *
\********************************************************************/

#include <config.h>
#include <gtest/gtest.h>
#include "../cashobjects.h"
#include "../qof.h"
#include "../gnc-engine.h"
#include "../gncInvoice.h"
#include "../gncCustomer.h"
#include "../gncVendor.h"
#include "../gncJob.h"
#include "../gncBillTerm.h"
#include "../gncEmployee.h"
#include "../gncEntry.h"
#include "../gncOrder.h"
#include "../gncOwner.h"
#include "../gncTaxTable.h"

class CashObjectsTest : public ::testing::Test
{
protected:
    void SetUp() override
    {
        qof_init();
    }

    void TearDown() override
    {
        qof_close();
    }
};

TEST_F(CashObjectsTest, test_registration)
{
    EXPECT_TRUE(cashobjects_register());

    // Check engine objects
    EXPECT_NE(qof_object_lookup(GNC_ID_ACCOUNT), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_TRANS), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_SPLIT), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_COMMODITY), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_LOT), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_PRICE), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_BUDGET), nullptr);

    // Check business objects
    EXPECT_NE(qof_object_lookup(GNC_ID_INVOICE), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_CUSTOMER), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_VENDOR), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_JOB), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_BILLTERM), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_EMPLOYEE), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_ENTRY), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_ORDER), nullptr);
    EXPECT_NE(qof_object_lookup(GNC_ID_TAXTABLE), nullptr);
}
