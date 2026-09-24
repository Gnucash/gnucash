/*
 * dialog-bi-import.c -- Invoice importer Core functions
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
 * @internal
 * @file dialog-bi-import.c
 * @brief core import functions for invoice import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 * @author Rob Laan <rob.laan@chello.nl>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib/gi18n.h>
#include <regex.h>
#include <glib.h>
#include <glib/gstdio.h>

#include "gnc-string-utils.h"
#include "gnc-date.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gncAddress.h"
#include "gncVendorP.h"
#include "gncVendor.h"
#include "gncEntry.h"
#include "gnc-prefs.h"

#include "gnc-exp-parser.h"

// query
#include "Query.h"
#include "qof.h"
#include "gncIDSearch.h"
#include "dialog-bi-import.h"
#include "dialog-bi-import-helper.h"

// To open the invoices for editing
#include "gnc-plugin-page-invoice.h"
#include "dialog-invoice.h"
#include "business-gnome-utils.h"

// This helper macro takes a regexp match and fills an import row.
#define FILL_IN_HELPER(match_name,column) \
            temp = g_match_info_fetch_named (match_info, match_name); \
            if (temp) \
            { \
                g_strstrip( temp ); \
                gnc_bi_import_row_set (row, column, temp); \
                g_free (temp); \
            } else gnc_bi_import_row_set (row, column, "");

static QofLogModule log_module = G_LOG_DOMAIN; //G_LOG_BUSINESS;
static char * un_escape(char *str);

#define BI_IMPORT_ROW_VALUES "bi-import-row-values"

GObject *
gnc_bi_import_row_new (void)
{
    GObject *row = g_object_new (G_TYPE_OBJECT, NULL);
    gchar **values = g_new0 (gchar *, N_COLUMNS + 1);

    g_object_set_data_full (row, BI_IMPORT_ROW_VALUES, values,
                            (GDestroyNotify)g_strfreev);
    return row;
}

const gchar *
gnc_bi_import_row_get (GObject *row, guint column)
{
    gchar **values;

    g_return_val_if_fail (G_IS_OBJECT (row), "");
    g_return_val_if_fail (column < N_COLUMNS, "");
    values = g_object_get_data (row, BI_IMPORT_ROW_VALUES);
    return values[column] ? values[column] : "";
}

gchar *
gnc_bi_import_row_dup (GObject *row, guint column)
{
    return g_strdup (gnc_bi_import_row_get (row, column));
}

void
gnc_bi_import_row_set (GObject *row, guint column, const gchar *value)
{
    gchar **values;

    g_return_if_fail (G_IS_OBJECT (row));
    g_return_if_fail (column < N_COLUMNS);
    values = g_object_get_data (row, BI_IMPORT_ROW_VALUES);
    g_free (values[column]);
    values[column] = g_strdup (value ? value : "");
}

/** \brief Imports a csv file with invoice data into a GListStore.

 Opens the csv file and attempts to match each row with the regular
 expression provided in parser_regexp. This is a regular expression
 that matches each field of the import row and the user selected field
 separators (, or ;), optionally with the fields enclosed in quotes.

 If the match is successful, the fields of the import row are transferred to
 a row in the GListStore store. If the match is not successful, the
 row is ignored. Maintains information about number of rows imported,
 the number of rows ignored, and the actual ignored rows.

 @param filename      The csv filename to read
 @param parser_regexp The regular expression with which to match the import rows
 @param store         To store the matched data
 @param max_rows      The maximum number of rows to import; use 0 for no maximum.
 @param stats         Return information about matched and non-matched rows. Use NULL if the information is not required.

 */

bi_import_result
gnc_bi_import_read_file (const gchar * filename, const gchar * parser_regexp,
                         GListStore * store, guint max_rows,
                         bi_import_stats * stats)
{
    // some statistics
    bi_import_stats stats_fallback;
    FILE *f;

    // regexp
    char *line = NULL;
    gchar *line_utf8 = NULL;
    gchar *temp = NULL;
    GMatchInfo *match_info;
    GError *err;
    GRegex *regexpat;

    f = g_fopen (filename, "rt");
    if (!f)
    {
        //gnc_error_dialog (NULL, _("File %s cannot be opened."), filename );
        return RESULT_OPEN_FAILED;
    }

    // set up statistics
    if (!stats)
        stats = &stats_fallback;

    // compile the regular expression and check for errors
    err = NULL;
    regexpat =
        g_regex_new (parser_regexp, G_REGEX_EXTENDED | G_REGEX_OPTIMIZE | G_REGEX_DUPNAMES, 0, &err);
    if (err != NULL)
    {
        gchar *errmsg;

        errmsg = g_strdup_printf (_("Error in regular expression '%s':\n%s"),
                                  parser_regexp, err->message);
        g_error_free (err);
        err = NULL;

        gnc_error_dialog (NULL, "%s", errmsg);
        g_free (errmsg);
        errmsg = 0;

        fclose (f);
        return RESULT_ERROR_IN_REGEXP;
    }

    // start the import
    stats->n_imported = 0;
    stats->n_ignored = 0;
    stats->ignored_lines = g_string_new (NULL);
#define buffer_size 1000
    line = g_malloc0 (buffer_size);
    while (!feof (f)
            && ((max_rows == 0)
                || (stats->n_imported + stats->n_ignored < max_rows)))
    {
        int l;
        // read one line
        if (!fgets (line, buffer_size, f))
            break;			// eof
        // now strip the '\n' from the end of the line
        l = strlen (line);
        if ((l > 0) && (line[l - 1] == '\n'))
            line[l - 1] = 0;

        // if the line doesn't conform to UTF-8, try a default charcter set
        // conversion based on locale
        if (g_utf8_validate(line, -1, NULL))
            line_utf8 = line;
        else
            line_utf8 = g_locale_to_utf8 (line, -1, NULL, NULL, NULL);

        // Remove the potential XML-prohibited codepoints from the UTF-8 compliant string
        gnc_utf8_strip_invalid(line_utf8);

        // parse the line
        match_info = NULL;	// it seems, that in contrast to documentation, match_info is not always set -> g_match_info_free will segfault
        if (g_regex_match (regexpat, line_utf8, 0, &match_info))
        {
            // match found
            stats->n_imported++;

            // fill in the values
            GObject *row = gnc_bi_import_row_new ();
            FILL_IN_HELPER ("id", ID); /* FIXME: Should "id" be translated? I don't think so. */
            FILL_IN_HELPER ("date_opened", DATE_OPENED);
            FILL_IN_HELPER ("owner_id", OWNER_ID);
            FILL_IN_HELPER ("billing_id", BILLING_ID);
            FILL_IN_HELPER ("notes", NOTES);

            FILL_IN_HELPER ("date", DATE);
            FILL_IN_HELPER ("desc", DESC);
            FILL_IN_HELPER ("action", ACTION);
            FILL_IN_HELPER ("account", ACCOUNT);
            FILL_IN_HELPER ("quantity", QUANTITY);
            FILL_IN_HELPER ("price", PRICE);
            FILL_IN_HELPER ("disc_type", DISC_TYPE);
            FILL_IN_HELPER ("disc_how", DISC_HOW);
            FILL_IN_HELPER ("discount", DISCOUNT);
            FILL_IN_HELPER ("taxable", TAXABLE);
            FILL_IN_HELPER ("taxincluded", TAXINCLUDED);
            FILL_IN_HELPER ("tax_table", TAX_TABLE);

            FILL_IN_HELPER ("date_posted", DATE_POSTED);
            FILL_IN_HELPER ("due_date", DUE_DATE);
            FILL_IN_HELPER ("account_posted", ACCOUNT_POSTED);
            FILL_IN_HELPER ("memo_posted", MEMO_POSTED);
            FILL_IN_HELPER ("accu_splits", ACCU_SPLITS);
            g_list_store_append (store, row);
            g_object_unref (row);
        }
        else
        {
            // ignore line
            stats->n_ignored++;
            g_string_append (stats->ignored_lines, line_utf8);
            g_string_append_c (stats->ignored_lines, '\n');
        }

        g_match_info_free (match_info);
        if (line_utf8 != line)
            g_free (line_utf8);
    }
    g_free (line);
    line = 0;

    g_regex_unref (regexpat);
    regexpat = 0;
    fclose (f);

    if (stats == &stats_fallback)
        // stats are not requested -> free the string
        g_string_free (stats->ignored_lines, TRUE);

    return RESULT_OK;
}


/** \brief Adjusts and validates invoice import data.

 Replaces missing or invalid data with defaults:
 - if quantity is not set, default to 1
 - if date_opened is not set or invalid, default to today
 - if date is not set or invalid, default to date_opened
 - if due date is not set or invalid, default to date_posted

 Validates the import data; any error causes all rows of the same invoice
 to be deleted from the import data:
 - id is not set, and there is no previous id
 - owner_id is not set, or customer/vendor does not exist
 - date_posted is not valid
 - account_posted does not exist
 - account posted is not the applicable type, A/P or A/R
 - price is not set
 - account does not exist

 Adjustment and validation for header fields is only done for the first row of an invoice,
 which is assumed to hold the header data for all items of the same invoice.
 Currency related validation is done in subsqequent processing by gnc_bi_import_create_bis.

 @param store Holds the rows of invoice import data
 @param n_rows_fixed Increased for every data row that is adjusted in this function
 @param n_rows_ignored Increased for every data row that is deleted in this function
 @param info Updated with the error messages from this function
 @param type The type of the import data, BILL or INVOICE

 */

void
gnc_bi_import_fix_bis (GListStore *store, guint *n_rows_fixed,
                       guint *n_rows_ignored, GString *info, gchar *type)
{
    gboolean row_fixed, on_first_row_of_invoice, ignore_invoice;
    gchar *invoice_date_opened = NULL;
    GString *running_id;
    Account *acc = NULL;
    guint dummy;
    guint position = 0, first_row_of_invoice = 0;
    gint row_number = 1, fixed_for_invoice = 0, invoice_line = 0;
    const gchar *date_format_string = qof_date_format_get_string (qof_date_format_get ());

    DEBUG ("date_format_string: %s", date_format_string);
    if (!n_rows_fixed)
        n_rows_fixed = &dummy;
    if (!n_rows_ignored)
        n_rows_ignored = &dummy;

    *n_rows_fixed = 0;
    *n_rows_ignored = 0;
    running_id = g_string_new ("");
    ignore_invoice = FALSE;
    on_first_row_of_invoice = TRUE;
    g_string_append_printf (info, _("Validation…\n"));

    while (position < g_list_model_get_n_items (G_LIST_MODEL (store)))
    {
        GObject *current = g_list_model_get_item (G_LIST_MODEL (store), position);
        gchar *id = gnc_bi_import_row_dup (current, ID);
        gchar *date = gnc_bi_import_row_dup (current, DATE);
        gchar *account = gnc_bi_import_row_dup (current, ACCOUNT);
        gchar *quantity = gnc_bi_import_row_dup (current, QUANTITY);
        gchar *price = gnc_bi_import_row_dup (current, PRICE);
        gboolean has_next;
        gchar *next_id = NULL;

        ++invoice_line;
        row_fixed = FALSE;

        if (on_first_row_of_invoice)
        {
            gchar *date_opened = gnc_bi_import_row_dup (current, DATE_OPENED);
            gchar *date_posted = gnc_bi_import_row_dup (current, DATE_POSTED);
            gchar *due_date = gnc_bi_import_row_dup (current, DUE_DATE);
            gchar *account_posted = gnc_bi_import_row_dup (current, ACCOUNT_POSTED);
            gchar *owner_id = gnc_bi_import_row_dup (current, OWNER_ID);

            g_string_assign (running_id, id);
            first_row_of_invoice = position;

            if (*id == '\0')
            {
                ignore_invoice = TRUE;
                g_string_append_printf (info,
                                        _("Row %d: no invoice ID in first row of import file.\n"),
                                        row_number);
            }
            if (*owner_id == '\0')
            {
                ignore_invoice = TRUE;
                g_string_append_printf (info, _("Row %d, invoice %s/%u: owner not set.\n"),
                                        row_number, id, invoice_line);
            }
            if (g_ascii_strcasecmp (type, "BILL") == 0 &&
                !gnc_search_vendor_on_id (gnc_get_current_book (), owner_id))
            {
                ignore_invoice = TRUE;
                g_string_append_printf (info,
                                        _("Row %d, invoice %s/%u: vendor %s does not exist.\n"),
                                        row_number, id, invoice_line, owner_id);
            }
            else if (g_ascii_strcasecmp (type, "INVOICE") == 0 &&
                     !gnc_search_customer_on_id (gnc_get_current_book (), owner_id))
            {
                ignore_invoice = TRUE;
                g_string_append_printf (info,
                                        _("Row %d, invoice %s/%u: customer %s does not exist.\n"),
                                        row_number, id, invoice_line, owner_id);
            }

            if (*date_posted != '\0')
            {
                if (!isDateValid (date_posted))
                {
                    ignore_invoice = TRUE;
                    g_string_append_printf (info,
                                            _("Row %d, invoice %s/%u: %s is not a valid posting date.\n"),
                                            row_number, id, invoice_line, date_posted);
                    if (!isDateValid (due_date))
                        g_string_append_printf (info,
                                                _("Row %d, invoice %s/%u: %s is not a valid due date.\n"),
                                                row_number, id, invoice_line, due_date);
                }
                else if (!isDateValid (due_date))
                {
                    gnc_bi_import_row_set (current, DUE_DATE, date_posted);
                    row_fixed = TRUE;
                }

                acc = gnc_account_lookup_for_register (gnc_get_current_root_account (),
                                                       account_posted);
                if (!acc)
                {
                    ignore_invoice = TRUE;
                    g_string_append_printf (info,
                                            _("Row %d, invoice %s/%u: account %s does not exist.\n"),
                                            row_number, id, invoice_line, account_posted);
                }
                else if ((g_ascii_strcasecmp (type, "BILL") == 0 &&
                          xaccAccountGetType (acc) != ACCT_TYPE_PAYABLE) ||
                         (g_ascii_strcasecmp (type, "INVOICE") == 0 &&
                          xaccAccountGetType (acc) != ACCT_TYPE_RECEIVABLE))
                {
                    ignore_invoice = TRUE;
                    g_string_append_printf (info,
                                            g_ascii_strcasecmp (type, "BILL") == 0 ?
                                            _("Row %d, invoice %s/%u: account %s is not of type Accounts Payable.\n") :
                                            _("Row %d, invoice %s/%u: account %s is not of type Accounts Receivable.\n"),
                                            row_number, id, invoice_line, account_posted);
                }
            }

            g_free (invoice_date_opened);
            if (!isDateValid (date_opened))
            {
                gchar today[20];
                GDate current_date;

                g_date_clear (&current_date, 1);
                gnc_gdate_set_today (&current_date);
                g_date_strftime (today, sizeof today, date_format_string, &current_date);
                gnc_bi_import_row_set (current, DATE_OPENED, today);
                invoice_date_opened = g_strdup (today);
                row_fixed = TRUE;
            }
            else
                invoice_date_opened = g_strdup (date_opened);

            g_free (date_opened);
            g_free (date_posted);
            g_free (due_date);
            g_free (account_posted);
            g_free (owner_id);
        }

        if (*price == '\0')
        {
            ignore_invoice = TRUE;
            g_string_append_printf (info, _("Row %d, invoice %s/%u: price not set.\n"),
                                    row_number, running_id->str, invoice_line);
        }
        acc = gnc_account_lookup_for_register (gnc_get_current_root_account (), account);
        if (!acc)
        {
            ignore_invoice = TRUE;
            g_string_append_printf (info,
                                    _("Row %d, invoice %s/%u: account %s does not exist.\n"),
                                    row_number, running_id->str, invoice_line, account);
        }
        if (!ignore_invoice)
        {
            if (*quantity == '\0')
            {
                gnc_bi_import_row_set (current, QUANTITY, "1");
                row_fixed = TRUE;
            }
            if (!isDateValid (date))
            {
                gnc_bi_import_row_set (current, DATE, invoice_date_opened);
                row_fixed = TRUE;
            }
        }
        if (row_fixed)
            ++fixed_for_invoice;

        has_next = position + 1 < g_list_model_get_n_items (G_LIST_MODEL (store));
        if (has_next)
        {
            GObject *next = g_list_model_get_item (G_LIST_MODEL (store), position + 1);
            next_id = gnc_bi_import_row_dup (next, ID);
            if (*next_id == '\0')
            {
                gnc_bi_import_row_set (next, ID, running_id->str);
                g_free (next_id);
                next_id = g_strdup (running_id->str);
            }
            g_object_unref (next);
        }

        if (!has_next || g_strcmp0 (next_id, running_id->str) != 0)
        {
            if (ignore_invoice)
            {
                while (first_row_of_invoice < g_list_model_get_n_items (G_LIST_MODEL (store)))
                {
                    GObject *candidate = g_list_model_get_item (G_LIST_MODEL (store),
                                                                first_row_of_invoice);
                    gboolean same_invoice = g_strcmp0 (gnc_bi_import_row_get (candidate, ID),
                                                        running_id->str) == 0;
                    g_object_unref (candidate);
                    if (!same_invoice)
                        break;
                    g_list_store_remove (store, first_row_of_invoice);
                    (*n_rows_ignored)++;
                }
                if (*running_id->str)
                    g_string_append_printf (info,
                                            _("Error(s) in invoice %s, all rows of this invoice ignored.\n"),
                                            running_id->str);
                else
                    g_string_append_printf (info,
                                            _("Error(s) in invoice without id, all rows of this invoice ignored.\n"));
                fixed_for_invoice = 0;
                ignore_invoice = FALSE;
                position = first_row_of_invoice;
            }
            else
                position++;

            on_first_row_of_invoice = TRUE;
            *n_rows_fixed += fixed_for_invoice;
            fixed_for_invoice = 0;
            invoice_line = 0;
            g_clear_pointer (&invoice_date_opened, g_free);
        }
        else
        {
            on_first_row_of_invoice = FALSE;
            position++;
        }

        g_free (next_id);
        g_free (id);
        g_free (date);
        g_free (account);
        g_free (quantity);
        g_free (price);
        g_object_unref (current);
        row_number++;
    }

    g_free (invoice_date_opened);
    g_string_free (running_id, TRUE);
}


/** \brief Creates and updates invoices from validated import data.

 Loops through the import data to create and update invoices.
 The first data row for an invoice is assumed to hold the header data.

 If an invoice already exists, the user is asked, once per import,
 to confirm that invoices should be updated.
 If not confirmed, any rows for existing invoices are ignored.
 If confirmed, entries are added to existing invoices.
 Posted invoices, however, are never updated.

 If the field date_posted is set, the system will
 attempt to also post the invoice. The system will not
 post the invoice if the entries of the invoice hold different currencies,
 or if the currency of the invoice differs from the currency of the account_posted.

 As per user selection, the system displays tabs for either all affected invoices,
 all affected invoices not yet posted, or no invoices at all.

 */

static void
bi_import_create_bis_process (GListStore *store, QofBook *book,
                              guint *n_invoices_created,
                              guint *n_invoices_updated,
                              guint *n_rows_ignored,
                              const gchar *type, const gchar *open_mode,
                              GString *info, GtkWindow *parent,
                              gboolean update_existing)
{
    gboolean valid, on_first_row_of_invoice, invoice_posted;
    guint position = 0, first_row_of_invoice;
    GObject *current;
    gchar *id = NULL, *date_opened = NULL, *owner_id = NULL, *billing_id = NULL, *notes = NULL;
    gchar *date = NULL, *desc = NULL, *action = NULL, *account = NULL, *quantity = NULL,
          *price = NULL, *disc_type = NULL, *disc_how = NULL, *discount = NULL, *taxable = NULL,
          *taxincluded = NULL, *tax_table = NULL;
    gchar *date_posted = NULL, *due_date = NULL, *account_posted = NULL, *memo_posted = NULL,
          *accumulatesplits = NULL;
    guint dummy;
    GncInvoice *invoice;
    GncEntry *entry;
    gint day, month, year;
    gnc_numeric value;
    GncOwner *owner;
    Account *acc = NULL;
    time64 today;
    InvoiceWindow *iw;
    GString *running_id;

    // these arguments are needed
    g_return_if_fail (store && book);
    // logic of this function only works for bills or invoices
    g_return_if_fail ((g_ascii_strcasecmp (type, "INVOICE") == 0) ||
            (g_ascii_strcasecmp (type, "BILL") == 0));

    // allow to call this function without statistics
    if (!n_invoices_created)
        n_invoices_created = &dummy;
    if (!n_invoices_updated)
        n_invoices_updated = &dummy;
    *n_invoices_created = 0;
    *n_invoices_updated = 0;

    invoice = NULL;
    on_first_row_of_invoice = TRUE;
    running_id = g_string_new("");

    g_string_append_printf (info, "\n%s\n", _("Processing…") );

    valid = g_list_model_get_n_items (G_LIST_MODEL (store)) > 0;
    while (valid)
    {
        // Walk through the list, reading each row
        current = g_list_model_get_item (G_LIST_MODEL (store), position);
        id = gnc_bi_import_row_dup (current, ID);
        date_opened = gnc_bi_import_row_dup (current, DATE_OPENED);
        date_posted = gnc_bi_import_row_dup (current, DATE_POSTED);
        due_date = gnc_bi_import_row_dup (current, DUE_DATE);
        account_posted = gnc_bi_import_row_dup (current, ACCOUNT_POSTED);
        memo_posted = gnc_bi_import_row_dup (current, MEMO_POSTED);
        accumulatesplits = gnc_bi_import_row_dup (current, ACCU_SPLITS);
        owner_id = gnc_bi_import_row_dup (current, OWNER_ID);
        billing_id = gnc_bi_import_row_dup (current, BILLING_ID);
        notes = gnc_bi_import_row_dup (current, NOTES);
        date = gnc_bi_import_row_dup (current, DATE);
        desc = gnc_bi_import_row_dup (current, DESC);
        action = gnc_bi_import_row_dup (current, ACTION);
        account = gnc_bi_import_row_dup (current, ACCOUNT);
        quantity = gnc_bi_import_row_dup (current, QUANTITY);
        price = gnc_bi_import_row_dup (current, PRICE);
        disc_type = gnc_bi_import_row_dup (current, DISC_TYPE);
        disc_how = gnc_bi_import_row_dup (current, DISC_HOW);
        discount = gnc_bi_import_row_dup (current, DISCOUNT);
        taxable = gnc_bi_import_row_dup (current, TAXABLE);
        taxincluded = gnc_bi_import_row_dup (current, TAXINCLUDED);
        tax_table = gnc_bi_import_row_dup (current, TAX_TABLE);

        if (on_first_row_of_invoice)
        {
            g_string_assign(running_id, id);
            first_row_of_invoice = position;

            if (g_ascii_strcasecmp (type, "BILL") == 0)
                invoice = gnc_search_bill_on_id (book, id);
            else if (g_ascii_strcasecmp (type, "INVOICE") == 0)
                invoice = gnc_search_invoice_on_id (book, id);
            DEBUG( "Existing %s ID: %s\n", type, gncInvoiceGetID(invoice));

            // If the search is empty then there is no existing invoice so make a new one
            if (invoice == NULL)
            {
                 DEBUG( "Creating a new : %s\n", type );
                // new invoice
                invoice = gncInvoiceCreate (book);
                /* Protect against thrashing the DB and trying to write the invoice
                 * record prematurely */
                gncInvoiceBeginEdit (invoice);
                gncInvoiceSetID (invoice, id);
                owner = gncOwnerNew ();
                if (g_ascii_strcasecmp (type, "BILL") == 0)
                    gncOwnerInitVendor (owner,
                                        gnc_search_vendor_on_id (book, owner_id));
                else if (g_ascii_strcasecmp (type, "INVOICE") == 0)
                    gncOwnerInitCustomer (owner,
                                          gnc_search_customer_on_id (book, owner_id));
                gncInvoiceSetOwner (invoice, owner);
                gncInvoiceSetCurrency (invoice, gncOwnerGetCurrency (owner));	// Set the invoice currency based on the owner
                qof_scan_date (date_opened, &day, &month, &year);
                gncInvoiceSetDateOpened (invoice,
                                         gnc_dmy2time64 (day, month, year));
                gncInvoiceSetBillingID (invoice, billing_id ? billing_id : "");
                notes = un_escape(notes);
                gncInvoiceSetNotes (invoice, notes ? notes : "");
                gncInvoiceSetActive (invoice, TRUE);
                //if (g_ascii_strcasecmp(type,"INVOICE"))gncInvoiceSetBillTo( invoice, billto );
                (*n_invoices_created)++;
                g_string_append_printf (info, _("Invoice %s created.\n"),id);

                gncInvoiceCommitEdit (invoice);
            }
            else			// Dealing with an existing invoice.
            {
                if (!update_existing)
                {
                    // If the user does not want to update existing invoices, ignore all rows of the invoice.
                    g_string_append_printf (info,_("Invoice %s not updated because it already exists.\n"),id);
                    while (valid && g_strcmp0 (id, running_id->str) == 0)
                    {
                        (*n_rows_ignored)++;
                        position++;
                        valid = position < g_list_model_get_n_items (G_LIST_MODEL (store));
                        g_free (id);
                        id = NULL;
                        if (valid)
                        {
                            GObject *next = g_list_model_get_item (G_LIST_MODEL (store), position);
                            id = gnc_bi_import_row_dup (next, ID);
                            g_object_unref (next);
                        }
                    }
                    on_first_row_of_invoice = TRUE;
                    goto next_row;
                }

                if (gncInvoiceIsPosted (invoice))
                {
                    // If the invoice is already posted, ignore all rows of the invoice.
                    g_string_append_printf (info,_("Invoice %s not updated because it is already posted.\n"),id);
                    while (valid && g_strcmp0 (id, running_id->str) == 0)
                    {
                        (*n_rows_ignored)++;
                        position++;
                        valid = position < g_list_model_get_n_items (G_LIST_MODEL (store));
                        g_free (id);
                        id = NULL;
                        if (valid)
                        {
                            GObject *next = g_list_model_get_item (G_LIST_MODEL (store), position);
                            id = gnc_bi_import_row_dup (next, ID);
                            g_object_unref (next);
                        }
                    }
                    on_first_row_of_invoice = TRUE;
                    goto next_row;
                }

                (*n_invoices_updated)++;
                g_string_append_printf (info, _("Invoice %s updated.\n"),id);
            }
        }

        // Add entry to invoice/bill
        entry = gncEntryCreate (book);
        gncEntryBeginEdit(entry);
        qof_scan_date (date, &day, &month, &year);
        {
            GDate *date = g_date_new_dmy(day, month, year);
            gncEntrySetDateGDate (entry, date);
            g_date_free (date);
        }
        today = gnc_time (NULL);
        gncEntrySetDateEntered(entry, today);
        // Remove escaped quotes
        desc = un_escape(desc);
        notes = un_escape(notes);
        gncEntrySetDescription (entry, desc);
        gncEntrySetAction (entry, action);
        value = gnc_numeric_zero();
        gnc_exp_parser_parse (quantity, &value, NULL);
        gncEntrySetQuantity (entry, value);
        acc = gnc_account_lookup_for_register (gnc_get_current_root_account (),
                                               account);

        if (g_ascii_strcasecmp (type, "BILL") == 0)
        {
            gncEntrySetBillAccount (entry, acc);
            value = gnc_numeric_zero();
            gnc_exp_parser_parse (price, &value, NULL);
            gncEntrySetBillPrice (entry, value);
            gncEntrySetBillTaxable (entry, text2bool (taxable));
            gncEntrySetBillTaxIncluded (entry, text2bool (taxincluded));
            gncEntrySetBillTaxTable (entry, gncTaxTableLookupByName (book, tax_table));
            gncBillAddEntry (invoice, entry);
        }
        else if (g_ascii_strcasecmp (type, "INVOICE") == 0)
        {
            gncEntrySetNotes (entry, notes);
            gncEntrySetInvAccount (entry, acc);
            value = gnc_numeric_zero();
            gnc_exp_parser_parse (price, &value, NULL);
            gncEntrySetInvPrice (entry, value);
            gncEntrySetInvTaxable (entry, text2bool (taxable));
            gncEntrySetInvTaxIncluded (entry, text2bool (taxincluded));
            gncEntrySetInvTaxTable (entry, gncTaxTableLookupByName (book, tax_table));
            value = gnc_numeric_zero();
            gnc_exp_parser_parse (discount, &value, NULL);
            gncEntrySetInvDiscount (entry, value);
            gncEntrySetInvDiscountType (entry, text2disc_type (disc_type));
            gncEntrySetInvDiscountHow (entry, text2disc_how (disc_how));
            gncInvoiceAddEntry (invoice, entry);
        }
        gncEntryCommitEdit(entry);
        g_free (id);
        id = NULL;
        position++;
        valid = position < g_list_model_get_n_items (G_LIST_MODEL (store));
        // handle auto posting of invoices

        if (valid)
        {
            GObject *next = g_list_model_get_item (G_LIST_MODEL (store), position);
            id = gnc_bi_import_row_dup (next, ID);
            g_object_unref (next);
        }

        if (g_strcmp0 (id, running_id->str) == 0) // The next row is for the same invoice.
        {
            on_first_row_of_invoice = FALSE;
        }
        else // The next row is for a new invoice; try to post the invoice.
        {
            // Use posting values from the first row of this invoice.
            GObject *first = g_list_model_get_item (G_LIST_MODEL (store),
                                                     first_row_of_invoice);
            g_free (id);
            g_free (date_posted);
            g_free (due_date);
            g_free (account_posted);
            g_free (memo_posted);
            g_free (accumulatesplits);
            id = gnc_bi_import_row_dup (first, ID);
            date_posted = gnc_bi_import_row_dup (first, DATE_POSTED);
            due_date = gnc_bi_import_row_dup (first, DUE_DATE);
            account_posted = gnc_bi_import_row_dup (first, ACCOUNT_POSTED);
            memo_posted = gnc_bi_import_row_dup (first, MEMO_POSTED);
            accumulatesplits = gnc_bi_import_row_dup (first, ACCU_SPLITS);
            g_object_unref (first);
            invoice_posted = FALSE;

            if (strlen(date_posted) != 0)
            {
                // autopost this invoice
                GHashTable *foreign_currs;
                gboolean auto_pay;
                time64 p_date, d_date;
                guint curr_count;
                gboolean scan_date_r;
                scan_date_r = qof_scan_date (date_posted, &day, &month, &year);
                DEBUG("Invoice %s is marked to be posted because...", id);
                DEBUG("qof_scan_date = %d", scan_date_r);
                if (g_ascii_strcasecmp (type, "INVOICE") == 0)
                    auto_pay = gnc_prefs_get_bool (GNC_PREFS_GROUP_INVOICE, GNC_PREF_AUTO_PAY);
                else
                    auto_pay = gnc_prefs_get_bool (GNC_PREFS_GROUP_BILL, GNC_PREF_AUTO_PAY);
                // Do we have any foreign currencies to deal with?
                foreign_currs = gncInvoiceGetForeignCurrencies (invoice);
                curr_count = g_hash_table_size (foreign_currs);
                DEBUG("curr_count = %d",curr_count);
                // Only auto-post if there's a single currency involved
                if(curr_count == 0)
                {
                    acc = gnc_account_lookup_for_register
                          (gnc_get_current_root_account (), account_posted);
                    // Check if the currencies match
                    if(gncInvoiceGetCurrency(invoice) == gnc_account_get_currency_or_parent(acc))
                    {
                        qof_scan_date (date_posted, &day, &month, &year);
                        p_date = gnc_dmy2time64 (day, month, year);
                        qof_scan_date (due_date, &day, &month, &year);
                        d_date = gnc_dmy2time64 (day, month, year);
                        gncInvoicePostToAccount (invoice, acc, p_date, d_date,
                                             memo_posted,
                                             text2bool (accumulatesplits),
                                             auto_pay);
                        PWARN("Invoice %s posted",id);
                        invoice_posted = TRUE;
                        g_string_append_printf (info, _("Invoice %s posted.\n"),id);
                    }
                    else // No match! Don't post it.
                    {
                        PWARN("Invoice %s NOT posted because currencies don't match", id);
                        g_string_append_printf (info,_("Invoice %s NOT posted because currencies don't match.\n"), id);
                    }
                }
                else
                {
                    PWARN("Invoice %s NOT posted because it requires currency conversion.",id);
                    g_string_append_printf (info,_("Invoice %s NOT posted because it requires currency conversion.\n"),id);
                }
                g_hash_table_unref (foreign_currs);
            }
            else
            {
                PWARN("Invoice %s is NOT marked for posting",id);
            }

            // open new bill / invoice in a tab, if requested
            if (g_ascii_strcasecmp(open_mode, "ALL") == 0
                    || (g_ascii_strcasecmp(open_mode, "NOT_POSTED") == 0
                        && !invoice_posted))
            {
                iw =  gnc_ui_invoice_edit (parent, invoice);
                gnc_plugin_page_invoice_new (iw);
            }

            // The next row will be for a new invoice.
            on_first_row_of_invoice = TRUE;
        }

next_row:
        g_free (id);
        g_free (date_opened);
        g_free (owner_id);
        g_free (billing_id);
        g_free (notes);
        g_free (date);
        g_free (desc);
        g_free (action);
        g_free (account);
        g_free (quantity);
        g_free (price);
        g_free (disc_type);
        g_free (disc_how);
        g_free (discount);
        g_free (taxable);
        g_free (taxincluded);
        g_free (tax_table);
        g_free (date_posted);
        g_free (due_date);
        g_free (account_posted);
        g_free (memo_posted);
        g_free (accumulatesplits);
        g_object_unref (current);
    }

    if (*n_invoices_updated + *n_invoices_created == 0)
        g_string_append_printf (info, _("Nothing to process.\n"));

    g_string_free (running_id, TRUE);
}

typedef struct
{
    GListStore *store;
    QofBook *book;
    gchar *type;
    gchar *open_mode;
    GString *info;
    guint n_invoices_created;
    guint n_invoices_updated;
    guint n_rows_ignored;
    GWeakRef parent;
    GWeakRef open_parent;
    gboolean has_parent;
    gboolean has_open_parent;
    GCancellable *cancellable;
    gulong parent_destroy_handler;
    GncBiImportCreateCallback completed;
    gpointer user_data;
    gboolean completed_once;
} BiImportCreateRequest;

static void
bi_import_create_request_free (BiImportCreateRequest *request)
{
    GtkWindow *parent;

    if (!request)
        return;

    parent = g_weak_ref_get (&request->parent);
    if (parent && request->parent_destroy_handler)
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_weak_ref_clear (&request->open_parent);
    g_clear_object (&request->store);
    g_clear_object (&request->cancellable);
    g_clear_pointer (&request->type, g_free);
    g_clear_pointer (&request->open_mode, g_free);
    if (request->info)
        g_string_free (request->info, TRUE);
    g_free (request);
}

static gboolean
bi_import_create_request_context_valid (BiImportCreateRequest *request,
                                        GtkWindow **open_parent)
{
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    GtkWindow *opened_parent = g_weak_ref_get (&request->open_parent);
    gboolean valid;

    valid = request->book && request->book == gnc_get_current_book () &&
            !qof_book_shutting_down (request->book) &&
            (!request->has_parent || parent != NULL) &&
            (!request->has_open_parent || opened_parent != NULL);
    g_clear_object (&parent);
    if (!valid)
    {
        g_clear_object (&opened_parent);
        return FALSE;
    }

    if (open_parent)
        *open_parent = opened_parent;
    else
        g_clear_object (&opened_parent);
    return TRUE;
}

static void
bi_import_create_request_complete (BiImportCreateRequest *request,
                                   gboolean continue_import,
                                   gboolean update_existing)
{
    GtkWindow *open_parent = NULL;
    gboolean completed = FALSE;

    if (!request || request->completed_once)
        return;

    request->completed_once = TRUE;
    if (continue_import &&
        bi_import_create_request_context_valid (request, &open_parent))
    {
        bi_import_create_bis_process (request->store, request->book,
                                      &request->n_invoices_created,
                                      &request->n_invoices_updated,
                                      &request->n_rows_ignored,
                                      request->type, request->open_mode,
                                      request->info, open_parent,
                                      update_existing);
        completed = TRUE;
    }
    g_clear_object (&open_parent);

    request->completed (completed, request->n_invoices_created,
                        request->n_invoices_updated,
                        request->n_rows_ignored,
                        request->info ? request->info->str : "",
                        request->user_data);
    bi_import_create_request_free (request);
}

static void
bi_import_create_parent_destroyed (GtkWidget *parent,
                                   BiImportCreateRequest *request)
{
    (void)parent;
    request->parent_destroy_handler = 0;
    g_cancellable_cancel (request->cancellable);
}

static void
bi_import_create_existing_finished (GObject *source, GAsyncResult *result,
                                    gpointer user_data)
{
    BiImportCreateRequest *request = user_data;
    GError *error = NULL;
    gint response;

    response = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source), result,
                                               &error);
    if (error)
    {
        g_clear_error (&error);
        bi_import_create_request_complete (request, FALSE, FALSE);
        return;
    }

    if (response == 0)
        bi_import_create_request_complete (request, TRUE, FALSE);
    else if (response == 1)
        bi_import_create_request_complete (request, TRUE, TRUE);
    else
        bi_import_create_request_complete (request, FALSE, FALSE);
}

static gboolean
bi_import_has_existing_invoice (GListStore *store, QofBook *book,
                                const gchar *type)
{
    guint position;
    guint count = g_list_model_get_n_items (G_LIST_MODEL (store));

    for (position = 0; position < count; position++)
    {
        GObject *row = g_list_model_get_item (G_LIST_MODEL (store), position);
        gchar *id = gnc_bi_import_row_dup (row, ID);
        GncInvoice *invoice = NULL;

        if (*id)
        {
            if (g_ascii_strcasecmp (type, "BILL") == 0)
                invoice = gnc_search_bill_on_id (book, id);
            else
                invoice = gnc_search_invoice_on_id (book, id);
        }
        g_free (id);
        g_object_unref (row);
        if (invoice)
            return TRUE;
    }
    return FALSE;
}

void
gnc_bi_import_create_bis_async (GListStore *store, QofBook *book,
                                const gchar *type, const gchar *open_mode,
                                guint n_rows_ignored, GString *info,
                                GtkWindow *parent, GtkWindow *open_parent,
                                GncBiImportCreateCallback completed,
                                gpointer user_data)
{
    BiImportCreateRequest *request;

    g_return_if_fail (G_IS_LIST_STORE (store));
    g_return_if_fail (book != NULL);
    g_return_if_fail (info != NULL);
    g_return_if_fail (completed != NULL);
    g_return_if_fail (g_ascii_strcasecmp (type, "INVOICE") == 0 ||
                      g_ascii_strcasecmp (type, "BILL") == 0);

    request = g_new0 (BiImportCreateRequest, 1);
    request->store = g_object_ref (store);
    request->book = book;
    request->type = g_strdup (type);
    request->open_mode = g_strdup (open_mode);
    request->info = info;
    request->n_rows_ignored = n_rows_ignored;
    request->completed = completed;
    request->user_data = user_data;
    request->has_parent = parent != NULL;
    request->has_open_parent = open_parent != NULL;
    request->cancellable = g_cancellable_new ();
    g_weak_ref_init (&request->parent, parent);
    g_weak_ref_init (&request->open_parent, open_parent);
    if (parent)
        request->parent_destroy_handler = g_signal_connect (
            parent, "destroy", G_CALLBACK (bi_import_create_parent_destroyed), request);

    if (!bi_import_create_request_context_valid (request, NULL))
    {
        bi_import_create_request_complete (request, FALSE, FALSE);
        return;
    }

    if (!bi_import_has_existing_invoice (store, book, type))
    {
        bi_import_create_request_complete (request, TRUE, TRUE);
        return;
    }

    {
        const gchar *buttons[] = { _("No"), _("Yes"), NULL };
        GtkAlertDialog *dialog = gtk_alert_dialog_new (
            "%s", _("Do you want to update existing bills/invoices?"));
        GtkWindow *alert_parent = g_weak_ref_get (&request->parent);

        gtk_alert_dialog_set_buttons (dialog, buttons);
        gtk_alert_dialog_set_cancel_button (dialog, -1);
        gtk_alert_dialog_set_default_button (dialog, 0);
        gtk_alert_dialog_choose (dialog, alert_parent, request->cancellable,
                                 bi_import_create_existing_finished, request);
        g_clear_object (&alert_parent);
        g_object_unref (dialog);
    }
}

/* Change any escaped quotes ("") to (")
 * @param char* String to be modified
 * @return char* Modified string.
*/
static char*
un_escape(char *str)
{
    gchar quote = '"';
    gchar *newStr = NULL, *tmpstr = str;
    int n = strlen (str), i;
    newStr = g_malloc (n + 1);
    memset (newStr, 0, n + 1);

    for (i = 0; *tmpstr != '\0'; ++i, ++tmpstr)
    {
        newStr[i] = *tmpstr == quote ? *(++tmpstr) : *(tmpstr);
        if (*tmpstr == '\0')
            break;
    }
    g_free (str);
    return newStr;
}
