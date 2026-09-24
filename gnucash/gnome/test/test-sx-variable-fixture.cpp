/*
 * test-sx-variable-fixture.cpp -- scheduled transaction variable test data
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <memory>

#include "test-sx-variable-fixture.h"

#include "SX-ttinfo.hpp"
#include "SchedXaction.hpp"
#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-session.h"
#include "test-engine-stuff.h"

static Account *
create_fixture_account (QofBook *book, Account *root, gnc_commodity *currency,
                        const gchar *name)
{
    Account *account = xaccMallocAccount (book);

    xaccAccountBeginEdit (account);
    xaccAccountSetName (account, name);
    xaccAccountSetType (account, ACCT_TYPE_BANK);
    xaccAccountSetCommodity (account, currency);
    gnc_account_append_child (root, account);
    xaccAccountCommitEdit (account);
    return account;
}

extern "C" SchedXaction *
add_daily_sx_with_variable (const gchar *name, const GDate *start)
{
    QofBook *book = qof_session_get_book (gnc_get_current_session ());
    SchedXaction *sx = add_daily_sx (name, start, NULL, NULL);
    gnc_commodity *currency = gnc_commodity_table_lookup (
        gnc_commodity_table_get_table (book), GNC_COMMODITY_NS_CURRENCY, "USD");
    Account *root = gnc_book_get_root_account (book);
    Account *debit_account;
    Account *credit_account;
    auto transaction = std::make_shared<TTInfo> ();
    auto debit_split = std::make_shared<TTSplitInfo> ();
    auto credit_split = std::make_shared<TTSplitInfo> ();

    g_assert_nonnull (currency);
    if (!root)
        root = gnc_account_create_root (book);
    debit_account = create_fixture_account (book, root, currency, "SLR debit");
    credit_account = create_fixture_account (book, root, currency, "SLR credit");
    transaction->set_currency (currency);

    debit_split->set_account (debit_account);
    debit_split->set_debit_formula ("a");
    transaction->append_template_split (debit_split);

    credit_split->set_account (credit_account);
    credit_split->set_credit_formula ("a");
    transaction->append_template_split (credit_split);

    xaccSchedXactionSetTemplateTrans (sx, { transaction }, book);
    return sx;
}
