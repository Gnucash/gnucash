/*
 * sie4-export.h -- SIE4 export writer interface
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#ifndef GNC_SIE4_EXPORT_H
#define GNC_SIE4_EXPORT_H

#include <glib.h>

#include "qof.h"

G_BEGIN_DECLS

/* All string fields are borrowed for the duration of gnc_sie4_export().
 * current_* dates describe #RAR 0 and previous_* dates describe #RAR -1. */
typedef struct
{
    const gchar *file_name;
    const gchar *company_name;
    const gchar *file_id;
    const gchar *organization_number;
    const gchar *contact;
    const gchar *street_address;
    const gchar *postal_address;
    const gchar *phone;
    const gchar *account_plan;
    const gchar *currency_code;
    const gchar *voucher_series;
    time64 current_start;
    time64 current_end;
    time64 previous_start;
    time64 previous_end;
    gboolean include_business_dimensions;
    gboolean include_zero_balances;
    gboolean use_transaction_numbers;
} GncSie4ExportSettings;

typedef struct
{
    guint generated_voucher_numbers;
} GncSie4ExportResult;

gboolean gnc_sie4_export (const GncSie4ExportSettings *settings,
                          GncSie4ExportResult *result,
                          GError **error);

G_END_DECLS

#endif /* GNC_SIE4_EXPORT_H */
