/*
 * gncEntryLedgerModel.h -- Model of GncEntry Manipulation Widget
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ENTRY_LEDGER_MODEL_H
#define GNC_ENTRY_LEDGER_MODEL_H

#include "gncEntryLedger.h"

TableModel * gnc_entry_ledger_model_new (GncEntryLedgerType type);

#endif /* GNC_ENTRY_LEDGER_MODEL_H */
