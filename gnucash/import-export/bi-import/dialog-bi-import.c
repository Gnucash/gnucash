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
 * @author Mike Evans <mikee@saxicola.co.uk>
 * @author Rob Laan <rob.laan@chello.nl>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib/gi18n.h>
#include <regex.h>
#include <glib.h>
#include <glib/gstdio.h>

#include "gnc-date.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
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

// this helper macro takes a regexp match and fills the model
#define FILL_IN_HELPER(match_name,column) \
            temp = g_match_info_fetch_named (match_info, match_name); \
            if (temp) \
            { \
                g_strstrip( temp ); \
                gtk_list_store_set (store, &iter, column, temp, -1); \
                g_free (temp); \
            }

static QofLogModule log_module = G_LOG_DOMAIN; //G_LOG_BUSINESS;
static char * un_escape(char *str);

/** \brief Imports a csv file with invoice data into a GtkListStore.
 
 Opens the csv file and attempts to match each row with the regular
 expression provided in parser_regexp. This is a regular expression
 that matches each field of the import row and the user selected field
 separators (, or ;), optionally with the fields enclosed in quotes.
 
 If the match is successful, the fields of the import row are transferred to
 a row in the GtkListStore store. If the match is not successful, the
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
                         GtkListStore * store, guint max_rows,
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

    // model
    GtkTreeIter iter;

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
        GtkWidget *dialog;
        gchar *errmsg;

        errmsg = g_strdup_printf (_("Error in regular expression '%s':\n%s"),
                                  parser_regexp, err->message);
        g_error_free (err);
        err = NULL;

        dialog = gtk_message_dialog_new (NULL,
                                         GTK_DIALOG_MODAL,
                                         GTK_MESSAGE_ERROR,
                                         GTK_BUTTONS_OK, "%s", errmsg);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
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

        // convert line from locale into utf8
        line_utf8 = g_locale_to_utf8 (line, -1, NULL, NULL, NULL);

        // parse the line
        match_info = NULL;	// it seems, that in contrast to documentation, match_info is not always set -> g_match_info_free will segfault
        if (g_regex_match (regexpat, line_utf8, 0, &match_info))
        {
            // match found
            stats->n_imported++;

            // fill in the values
            gtk_list_store_append (store, &iter);
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
        }
        else
        {
            // ignore line
            stats->n_ignored++;
            g_string_append (stats->ignored_lines, line_utf8);
            g_string_append_c (stats->ignored_lines, '\n');
        }

        g_match_info_free (match_info);
        match_info = 0;
        g_free (line_utf8);
        line_utf8 = 0;
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
gnc_bi_import_fix_bis (GtkListStore * store, guint * n_rows_fixed, guint * n_rows_ignored,
                       GString * info, gchar *type)
{
    GtkTreeIter iter, first_row_of_invoice;
    gboolean valid, row_fixed, on_first_row_of_invoice, ignore_invoice;
    gchar *id = NULL, *date_opened = NULL, *date_posted = NULL, *due_date = NULL, *account_posted = NULL,
        *owner_id = NULL, *date = NULL, *account = NULL, *quantity = NULL, *price = NULL;
    GString *running_id;
    Account *acc = NULL;
    guint dummy;
    gint row = 1, fixed_for_invoice = 0, invoice_line = 0;
    const gchar* date_format_string = qof_date_format_get_string (qof_date_format_get()); // Get the user set date format string

    DEBUG("date_format_string: %s",date_format_string);
    // allow the call to this function with only GtkListeStore* specified
    if (!n_rows_fixed)
        n_rows_fixed = &dummy;
    if (!n_rows_ignored)
        n_rows_ignored = &dummy;

    *n_rows_fixed = 0;
    *n_rows_ignored = 0;
    
    // Init control variables
    running_id = g_string_new("");
    ignore_invoice = FALSE;
    on_first_row_of_invoice = TRUE;
    
    g_string_append_printf (info, _("Validation...\n") );

    // Walk through the list, reading each row.
    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (store), &iter);
    while (valid)
    {
        ++invoice_line;
        row_fixed = FALSE;

        //  If this is a row for a new invoice id, validate header values.
        if (on_first_row_of_invoice)
        {
            gtk_tree_model_get (GTK_TREE_MODEL (store), &iter,
                                ID, &id,
                                DATE_OPENED, &date_opened,
                                DATE_POSTED, &date_posted,
                                DUE_DATE, &due_date,
                                ACCOUNT_POSTED, &account_posted,
                                OWNER_ID, &owner_id, -1);
            
            g_string_assign (running_id, id);
            first_row_of_invoice = iter;
            
            // Validate the invoice id.
            if (strlen (id) == 0)
            {
                // If there was an earlier valid id, then it replaces an empty id when the next row is read at the end of the loop.
                // So an empty id error can only happen on the first row of an import file.
                ignore_invoice = TRUE;
                g_string_append_printf (info,
                                        _("Row %d: no invoice ID in first row of import file.\n"), row);
            }

            // Validate customer or vendor.
            if (strlen (owner_id) == 0)
            {
                ignore_invoice = TRUE;
                g_string_append_printf (info,
                                        _("Row %d, invoice %s/%u: owner not set.\n"),
                                        row, id, invoice_line);
            }
            // Verify that customer or vendor exists.
            if (g_ascii_strcasecmp (type, "BILL") == 0)
            {
                if (!gnc_search_vendor_on_id
                    (gnc_get_current_book (), owner_id))
                {
                    // Vendor not found.
                    ignore_invoice = TRUE;
                    g_string_append_printf (info,
                                            _("Row %d, invoice %s/%u: vendor %s does not exist.\n"),
                                            row, id, invoice_line, owner_id);
                }
            }
            else if (g_ascii_strcasecmp (type, "INVOICE") == 0)
            {
                if (!gnc_search_customer_on_id
                    (gnc_get_current_book (), owner_id))
                {
                    // Customer not found.
                    ignore_invoice = TRUE;
                    g_string_append_printf (info,
                                            _("Row %d, invoice %s/%u: customer %s does not exist.\n"),
                                            row, id, invoice_line, owner_id);
                }
            }
            
            if (strlen(date_posted) != 0)
            {
                // Validate the date posted and due date.
                if (!isDateValid(date_posted))
                {
                    // Invalid date posted in first row of invoice, ignore the invoice
                    ignore_invoice = TRUE;
                    g_string_append_printf (info,
                                            _("Row %d, invoice %s/%u: %s is not a valid posting date.\n"),
                                            row, id, invoice_line, date_posted);
                    
                    // Verify the due date.
                    if (!isDateValid(due_date))
                    {
                        // Invalid due date in first row of invoice, without valid posting date to substitute.
                        g_string_append_printf (info,
                                                _("Row %d, invoice %s/%u: %s is not a valid due date.\n"),
                                                row, id, invoice_line, due_date);
                    }
                }
                else
                {
                    // Verify the due date.
                    if (!isDateValid(due_date))
                    {
                        // Fix this by using the date posted.
                        gtk_list_store_set (store, &iter, DUE_DATE,
                                            date_posted, -1);
                        row_fixed = TRUE;
                    }
                }
                
            // Validate account posted.
            // Account should exists, and should be of type A/R for invoices, A/P for bills.
                 acc = gnc_account_lookup_for_register
                (gnc_get_current_root_account (), account_posted);
                if (acc == NULL)
                {
                    ignore_invoice = TRUE;
                    g_string_append_printf (info,
                                            _("Row %d, invoice %s/%u: account %s does not exist.\n"),
                                            row, id, invoice_line, account_posted);
                }
                else
                {
                    if (g_ascii_strcasecmp (type, "BILL") == 0)
                    {
                        
                        if (xaccAccountGetType (acc) != ACCT_TYPE_PAYABLE)
                        {
                            ignore_invoice = TRUE;
                            g_string_append_printf (info,
                                                    _("Row %d, invoice %s/%u: account %s is not of type Accounts Payable.\n"),
                                                    row, id, invoice_line, account_posted);
                        }
                    }
                    else if (g_ascii_strcasecmp (type, "INVOICE") == 0)
                    {
                        if (xaccAccountGetType (acc) != ACCT_TYPE_RECEIVABLE)
                        {
                            ignore_invoice = TRUE;
                            g_string_append_printf (info,
                                                    _("Row %d, invoice %s/%u: account %s is not of type Accounts Receivable.\n"),
                                                    row, id, invoice_line, account_posted);
                        }
                    }
                }
            }
            
            // Verify the date opened.
            if(!isDateValid(date_opened))
            {
                // Fix this by using the current date.
                gchar temp[20];
                GDate date;
                g_date_clear (&date, 1);
                gnc_gdate_set_today (&date);
                g_date_strftime (temp, 20, date_format_string, &date);    // Create a user specified date string.
                gtk_list_store_set (store, &iter, DATE_OPENED,
                                    temp, -1);
                row_fixed = TRUE;
            }
        }
        
        // Validate and fix item data for each row.
        
        // Get item data.
        gtk_tree_model_get (GTK_TREE_MODEL (store), &iter,
                            DATE, &date,
                            ACCOUNT, &account,
                            QUANTITY, &quantity,
                            PRICE, &price, -1);

        
        // Validate the price.
        if (strlen (price) == 0)
        {
            // No valid price, delete the row
            ignore_invoice = TRUE;
            g_string_append_printf (info,
                                    _("Row %d, invoice %s/%u: price not set.\n"),
                                    row, id, invoice_line);
        }

        // Validate the account
        acc = gnc_account_lookup_for_register (gnc_get_current_root_account (),
                                               account);
        if (acc == NULL)
        {
            ignore_invoice = TRUE;
            g_string_append_printf (info,
                                    _("Row %d, invoice %s/%u: account %s does not exist.\n"),
                                    row, id, invoice_line, account);
        }
        
        // Fix item data.
        if (!ignore_invoice)
        {
            
            // Verify the quantity.
            if (strlen (quantity) == 0)
            {
                // The quantity is not set, default to 1.
                gtk_list_store_set (store, &iter, QUANTITY, "1", -1);
                row_fixed = TRUE;
            }
            
            // Verify the item date
            if(!isDateValid(date))
            {
                // Invalid item date, replace with date opened
                gtk_list_store_set (store, &iter, DATE,
                                    date_opened, -1);
                row_fixed = TRUE;
            }

        }
        if (row_fixed) ++fixed_for_invoice;
        
        // Get the next row and its id.
        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter);
        if (valid) gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, ID, &id, -1);

        
        // If the id of the next row is blank, it takes the id of the previous row.
        if (valid && strlen(id) == 0)
        {
            strcpy( id, running_id->str);
            gtk_list_store_set (store, &iter, ID, id, -1);
        }
        
        // If this row was the last row of the invoice...
        if (!valid || (valid && g_strcmp0 (id, running_id->str) != 0))
        {
            // If invoice should be ignored, remove all rows of this invoice.
            if (ignore_invoice)
            {
                iter = first_row_of_invoice;
                do
                {
                    (*n_rows_ignored)++;
                    valid = gtk_list_store_remove (store, &iter);
                    if (valid) gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, ID, &id, -1);
                }
                while (valid && (g_strcmp0 (id, running_id->str) == 0));
                
                if (running_id->len != 0)
                {
                    g_string_append_printf (info,
                                            _("Error(s) in invoice %s, all rows of this invoice ignored.\n"),
                                            running_id->str);
                }
                else
                {
                    g_string_append_printf (info,
                                            _("Error(s) in invoice without id, all rows of this invoice ignored.\n"));
                }
                    
                // Fixes for ignored invoices don't count in the statistics.
                fixed_for_invoice = 0;
                
                ignore_invoice = FALSE;
            }
            
            on_first_row_of_invoice = TRUE;
            (*n_rows_fixed) += fixed_for_invoice;
            fixed_for_invoice = 0;
            invoice_line = 0;
            
            g_free (id);
            g_free (date_opened);
            g_free (date_posted);
            g_free (due_date);
            g_free (account_posted);
            g_free (owner_id);
        }
        else on_first_row_of_invoice = FALSE;
        
        g_free (date);
        g_free (account);
        g_free (quantity);
        g_free (price);

        row++;
    }
    
    // Deallocate strings.
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

void
gnc_bi_import_create_bis (GtkListStore * store, QofBook * book,
                          guint * n_invoices_created,
                          guint * n_invoices_updated,
                          guint * n_rows_ignored,
                          gchar * type, gchar * open_mode, GString * info,
                          GtkWindow *parent)
{
    gboolean valid, on_first_row_of_invoice, invoice_posted;
    GtkTreeIter iter, first_row_of_invoice;
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
    enum update {YES = GTK_RESPONSE_YES, NO = GTK_RESPONSE_NO, NOT_ASKED = GTK_RESPONSE_NONE} update;
    GtkWidget *dialog;
    time64 today;
    InvoiceWindow *iw;
    gint64 denom = 0;
    gnc_commodity *currency;
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
    update = NOT_ASKED;
    on_first_row_of_invoice = TRUE;
    running_id = g_string_new("");
    
    g_string_append_printf (info, _("\nProcessing...\n") );
    
    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (store), &iter);
    while (valid)
    {
        // Walk through the list, reading each row
        gtk_tree_model_get (GTK_TREE_MODEL (store), &iter,
                            ID, &id,
                            DATE_OPENED, &date_opened,
                            DATE_POSTED, &date_posted,       // if autoposting requested
                            DUE_DATE, &due_date,             // if autoposting requested
                            ACCOUNT_POSTED, &account_posted, // if autoposting requested
                            MEMO_POSTED, &memo_posted,       // if autoposting requested
                            ACCU_SPLITS, &accumulatesplits,  // if autoposting requested
                            OWNER_ID, &owner_id,
                            BILLING_ID, &billing_id,
                            NOTES, &notes,
                            DATE, &date,
                            DESC, &desc,
                            ACTION, &action,
                            ACCOUNT, &account,
                            QUANTITY, &quantity,
                            PRICE, &price,
                            DISC_TYPE, &disc_type,
                            DISC_HOW, &disc_how,
                            DISCOUNT, &discount,
                            TAXABLE, &taxable,
                            TAXINCLUDED, &taxincluded,
                            TAX_TABLE, &tax_table, -1);
        
        if (on_first_row_of_invoice)
        {
            g_string_assign(running_id, id);
            first_row_of_invoice = iter;
        
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
                // For the first existing invoice in the import file,
                // ask the user to confirm update of existing invoices.
                if (update == NOT_ASKED)
                {
                    dialog = gtk_message_dialog_new (parent,
                                                     GTK_DIALOG_MODAL,
                                                     GTK_MESSAGE_ERROR,
                                                     GTK_BUTTONS_YES_NO,
                                                     "%s",
                                                     _("Do you want to update existing bills/invoices?"));
                    update = gtk_dialog_run (GTK_DIALOG (dialog));
                    gtk_widget_destroy (dialog);
                }
                
                if (update == NO)
                {
                    // If the user does not want to update existing invoices, ignore all rows of the invoice.
                    g_string_append_printf (info,_("Invoice %s not updated because it already exists.\n"),id);
                    while (valid && g_strcmp0 (id, running_id->str) == 0)
                    {
                        (*n_rows_ignored)++;
                        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter);
                        if (valid)
                            gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, ID, &id, -1);
                    }
                    on_first_row_of_invoice = TRUE;
                    continue;
                }
                
                if (gncInvoiceIsPosted (invoice))
                {
                    // If the invoice is already posted, ignore all rows of the invoice.
                    g_string_append_printf (info,_("Invoice %s not updated because it is already posted.\n"),id);
                    while (valid && g_strcmp0 (id, running_id->str) == 0)
                    {
                        (*n_rows_ignored)++;
                        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter);
                        if (valid)
                            gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, ID, &id, -1);
                    }
                    on_first_row_of_invoice = TRUE;
                    continue;
                }

                (*n_invoices_updated)++;
                g_string_append_printf (info, _("Invoice %s updated.\n"),id);
            }
        }

        // Add entry to invoice/bill
        entry = gncEntryCreate (book);
        gncEntryBeginEdit(entry);
        currency = gncInvoiceGetCurrency(invoice);
        if (currency) denom = gnc_commodity_get_fraction(currency);
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
        // Need to set the denom appropriately else we get stupid rounding errors.
        value = gnc_numeric_convert (value, denom * 100, GNC_HOW_RND_NEVER);
        gncEntrySetQuantity (entry, value);
        acc = gnc_account_lookup_for_register (gnc_get_current_root_account (),
                                               account);

        if (g_ascii_strcasecmp (type, "BILL") == 0)
        {
            gncEntrySetBillAccount (entry, acc);
            value = gnc_numeric_zero();
            gnc_exp_parser_parse (price, &value, NULL);
            value = gnc_numeric_convert (value, denom * 100, GNC_HOW_RND_NEVER);
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
            value = gnc_numeric_convert (value, denom * 100, GNC_HOW_RND_NEVER);
            gncEntrySetInvPrice (entry, value);
            gncEntrySetInvTaxable (entry, text2bool (taxable));
            gncEntrySetInvTaxIncluded (entry, text2bool (taxincluded));
            gncEntrySetInvTaxTable (entry, gncTaxTableLookupByName (book, tax_table));
            value = gnc_numeric_zero();
            gnc_exp_parser_parse (discount, &value, NULL);
            value = gnc_numeric_convert (value, denom * 100, GNC_HOW_RND_NEVER);
            gncEntrySetInvDiscount (entry, value);
            gncEntrySetInvDiscountType (entry, text2disc_type (disc_type));
            gncEntrySetInvDiscountHow (entry, text2disc_how (disc_how));
            gncInvoiceAddEntry (invoice, entry);
        }
        gncEntryCommitEdit(entry);
        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter);
        // handle auto posting of invoices
       
        if (valid)
            gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, ID, &id, -1);
        else
            id = NULL;
        
        if (g_strcmp0 (id, running_id->str) == 0) // The next row is for the same invoice.
        {
            on_first_row_of_invoice = FALSE;
        }
        else // The next row is for a new invoice; try to post the invoice.
        {
            // Use posting values from the first row of this invoice.
            gtk_tree_model_get (GTK_TREE_MODEL (store), &first_row_of_invoice,
                                ID, &id,
                                DATE_POSTED, &date_posted,
                                DUE_DATE, &due_date,
                                ACCOUNT_POSTED, &account_posted,
                                MEMO_POSTED, &memo_posted,
                                ACCU_SPLITS, &accumulatesplits, -1);
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
    }
    
    if (*n_invoices_updated + *n_invoices_created == 0)
        g_string_append_printf (info, _("Nothing to process.\n"));
    
    // cleanup
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
    
    g_string_free (running_id, TRUE);
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
