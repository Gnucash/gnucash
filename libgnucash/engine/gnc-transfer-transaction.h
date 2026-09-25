/* gnc-transfer-transaction.h -- Create a two-account transfer transaction.
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

#ifndef GNC_TRANSFER_TRANSACTION_H
#define GNC_TRANSFER_TRANSACTION_H

#include "Account.h"
#include "Transaction.h"

G_BEGIN_DECLS

typedef struct
{
    QofBook *book;
    Account *from_account;
    Account *to_account;
    gnc_commodity *from_commodity;
    gnc_commodity *to_commodity;
    time64 date;
    gnc_numeric amount;
    gnc_numeric to_amount;
    const char *number;
    const char *description;
    const char *notes;
    const char *memo;
} GncTransferTransactionInfo;

/* The caller validates the accounts and amounts before invoking this function.
 * Returns a committed transaction owned by info->book. The account edits and
 * the transaction edit are committed before the caller is notified.
 */
Transaction *gnc_transfer_transaction_create (const GncTransferTransactionInfo *info);

G_END_DECLS

#endif
