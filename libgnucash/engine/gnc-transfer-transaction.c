/* gnc-transfer-transaction.c -- Create a two-account transfer transaction.
 *
 * Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 */

#include <config.h>

#include "gnc-transfer-transaction.h"
#include "qofbook.h"

Transaction *
gnc_transfer_transaction_create (const GncTransferTransactionInfo *info)
{
    Transaction *trans;
    Split *from_split;
    Split *to_split;

    trans = xaccMallocTransaction (info->book);
    xaccTransBeginEdit (trans);

    xaccTransSetCurrency (trans, info->from_commodity);
    xaccTransSetDatePostedSecsNormalized (trans, info->date);
    xaccTransSetDescription (trans, info->description);

    from_split = xaccMallocSplit (info->book);
    xaccTransAppendSplit (trans, from_split);
    to_split = xaccMallocSplit (info->book);
    xaccTransAppendSplit (trans, to_split);

    xaccAccountBeginEdit (info->from_account);
    xaccAccountInsertSplit (info->from_account, from_split);
    xaccAccountBeginEdit (info->to_account);
    xaccAccountInsertSplit (info->to_account, to_split);

    xaccSplitSetBaseValue (from_split, gnc_numeric_neg (info->amount),
                           info->from_commodity);
    xaccSplitSetBaseValue (to_split, info->amount, info->from_commodity);
    xaccSplitSetBaseValue (to_split, info->to_amount, info->to_commodity);

    if (info->number)
    {
        if (qof_book_use_split_action_for_num_field (info->book))
            xaccSplitSetAction (from_split, info->number);
        else
            xaccTransSetNum (trans, info->number);
    }
    xaccTransSetNotes (trans, info->notes);
    xaccSplitSetMemo (from_split, info->memo);
    xaccSplitSetMemo (to_split, info->memo);

    xaccTransCommitEdit (trans);
    xaccAccountCommitEdit (info->from_account);
    xaccAccountCommitEdit (info->to_account);
    return trans;
}
