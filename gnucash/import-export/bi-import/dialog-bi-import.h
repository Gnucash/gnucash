/*
 * dialog-bi-import.h -- Invoice Importer core functions
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
 * @addtogroup Tools
 * @{
 * @file dialog-bi-import.h
 * @brief core import functions for invoice import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 * @author Mike Evans <mikee@saxicola.co.uk>
 * @author Rob Laan <rob.laan@chello.nl>
 */

#ifndef GNC_PLUGIN_BI_IMPORT_H
#define GNC_PLUGIN_BI_IMPORT_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

// model
enum bi_import_model_columns
{
    ID, DATE_OPENED, OWNER_ID, BILLING_ID, NOTES, // invoice settings
    DATE, DESC, ACTION, ACCOUNT, QUANTITY, PRICE, DISC_TYPE, DISC_HOW, DISCOUNT, TAXABLE, TAXINCLUDED, TAX_TABLE, // entry settings
    DATE_POSTED, DUE_DATE, ACCOUNT_POSTED, MEMO_POSTED, ACCU_SPLITS, // autopost settings
    N_COLUMNS
};

enum _bi_import_result
{
    RESULT_OK,
    RESULT_OPEN_FAILED,
    RESULT_ERROR_IN_REGEXP,
};
typedef enum _bi_import_result bi_import_result;

struct _bi_import_stats
{
    int n_imported, n_ignored;
    GString *ignored_lines;
};
typedef struct _bi_import_stats bi_import_stats;

/* A row is a plain GObject whose values are owned by the import model. */
GObject *gnc_bi_import_row_new (void);
const gchar *gnc_bi_import_row_get (GObject *row, guint column);
gchar *gnc_bi_import_row_dup (GObject *row, guint column);
void gnc_bi_import_row_set (GObject *row, guint column, const gchar *value);


bi_import_result
gnc_bi_import_read_file (const gchar *filename, const gchar *parser_regexp, GListStore *store, guint max_rows, bi_import_stats *stats);

void
gnc_bi_import_fix_bis (GListStore *store, guint *fixed, guint *deleted, GString *info, gchar *type);

/** Completion for gnc_bi_import_create_bis_async.
 * @param completed TRUE when the import ran; FALSE after cancellation, destroyed
 *                  parent, or an invalid/shutting-down book.
 * @param info Borrowed processing details, valid only for the callback.
 */
typedef void (*GncBiImportCreateCallback) (gboolean completed,
                                           guint n_invoices_created,
                                           guint n_invoices_updated,
                                           guint n_rows_ignored,
                                           const gchar *info,
                                           gpointer user_data);

/**
 * Continue validated import rows without a nested event loop. Ownership of
 * @a info transfers to the request. @a parent protects the pending decision;
 * @a open_parent remains the owner of any invoice tabs opened after import.
 */
void
gnc_bi_import_create_bis_async (GListStore *store, QofBook *book,
                                const gchar *type, const gchar *open_mode,
                                guint n_rows_ignored, GString *info,
                                GtkWindow *parent, GtkWindow *open_parent,
                                GncBiImportCreateCallback completed,
                                gpointer user_data);


G_END_DECLS

#endif /* GNC_PLUGIN_BI_IMPORT_H */

/** @} */
