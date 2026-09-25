/*
 * dialog-ab-daterange.h --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file aqbanking/dialog-ab-daterange.h
 * @brief Dialog for date range entry
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef DIALOG_DATERANGE_H
#define DIALOG_DATERANGE_H

#include <gtk/gtk.h>

#include "qof.h"

G_BEGIN_DECLS

typedef struct
{
    time64 from_date;
    gboolean last_retrieval_date;
    gboolean first_possible_date;
    time64 to_date;
    gboolean to_now;
} GncABDateRange;

/**
 * gnc_ab_enter_daterange_async:
 * @parent: (nullable): widget to use as the transient parent
 * @heading: (nullable): descriptive text at the top
 * @initial: initial range values
 * @cancellable: (nullable): operation cancellation token
 * @callback: (nullable): completion callback
 * @user_data: data passed to @callback
 *
 * Presents the date-range dialog without entering a nested main loop.
 */
void gnc_ab_enter_daterange_async (GtkWidget *parent,
                                   const char *heading,
                                   const GncABDateRange *initial,
                                   GCancellable *cancellable,
                                   GAsyncReadyCallback callback,
                                   gpointer user_data);

/**
 * gnc_ab_enter_daterange_finish:
 * @result: asynchronous result returned by gnc_ab_enter_daterange_async()
 * @range: (out): selected range
 * @error: (out) (nullable): operation error
 *
 * Returns %TRUE if the user accepted the dialog. Cancelling it returns %FALSE
 * without an error and leaves @range unchanged.
 */
gboolean gnc_ab_enter_daterange_finish (GAsyncResult *result,
                                        GncABDateRange *range,
                                        GError **error);

G_END_DECLS

#endif /* DIALOG_DATERANGE_H */
/** @} */
/** @} */
