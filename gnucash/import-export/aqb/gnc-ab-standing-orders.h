/*
 * gnc-ab-standing-orders.h -- AqBanking standing-order synchronization
 * Copyright 2026 copystring
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#ifndef GNC_AB_STANDING_ORDERS_H
#define GNC_AB_STANDING_ORDERS_H

#include <glib.h>

#include "Account.h"

typedef struct AB_IMEXPORTER_CONTEXT AB_IMEXPORTER_CONTEXT;

typedef struct
{
    guint received;
    guint created;
    guint updated;
    guint disabled;
    guint skipped;
    /* Borrowed SchedXaction pointers; the caller owns only the list. */
    GList *to_edit;
} GncABStandingOrderSyncResult;

G_BEGIN_DECLS

/** Synchronize a complete AqBanking standing-order snapshot into a book.
 *
 * This entry point has no GUI or live-bank dependency so that callers and
 * tests can provide a synthetic AqBanking response.
 */
GncABStandingOrderSyncResult gnc_ab_import_standing_orders (
    AB_IMEXPORTER_CONTEXT *context, Account *default_acc);

G_END_DECLS

#endif /* GNC_AB_STANDING_ORDERS_H */
