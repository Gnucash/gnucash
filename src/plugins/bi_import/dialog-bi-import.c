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
 * @todo Create an option to import a pre-formed regex when it is present
 * to enable the use of custom output csv formats.
 * @todo Open the newly created invoice(es).
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib/gi18n.h>
#include <regex.h>
#include <glib.h>
#include <glib/gstdio.h>

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
#include <gnc-gdate-utils.h>

// To open the invoices for editing
#include "business/business-gnome/gnc-plugin-page-invoice.h"
#include "business/business-gnome/dialog-invoice.h"
#include "business/business-gnome/business-gnome-utils.h"


//#ifdef HAVE_GLIB_2_14
// glib >= 2.14.0
// perl regular expressions are available

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

bi_import_result
gnc_bi_import_read_file (const gchar * filename, const gchar * parser_regexp,
                         GtkListStore * store, guint max_rows,
                         bi_import_stats * stats)
{
    // some statistics
    bi_import_stats stats_fallback;
    FILE *f;

    // regexp
    char *line;
    gchar *line_utf8, *temp;
    GMatchInfo *match_info;
    GError *err;
    GRegex *regexpat;

    // model
    GtkTreeIter iter;

    f = g_fopen (filename, "rt");
    if (!f)
    {
        //gnc_error_dialog( 0, _("File %s cannot be opened."), filename );
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
        match_info = NULL;	// it seems, that in contrast to documentation, match_info is not alsways set -> g_match_info_free will segfault
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


//! \brief try to fix some common errors in the csv representation of invoices
//! * corrects the date format
//! * corrects ambigous values in multi line invoices
//! * ensures customer exists
//! * if quantity is unset, set to 1
//! * if price is unset, delete row
void
gnc_bi_import_fix_bis (GtkListStore * store, guint * fixed, guint * deleted,
                       GString * info, gchar *type)
{
    GtkTreeIter iter;
    gboolean valid, row_deleted, row_fixed;
    gchar *id, *date_opened, *date_posted, *due_date, *owner_id, *date, *quantity, *price;
    GString *prev_id, *prev_date_opened, *prev_date_posted, *prev_owner_id, *prev_date;	// needed to fix multi line invoices
    guint dummy;
    gint row = 1;
    const gchar* date_format_string = qof_date_format_get_string (qof_date_format_get()); // Get the user set date format string
    
    
    //date_format_string = qof_date_format_get_string (qof_date_format_get());	

    DEBUG("date_format_string: %s",date_format_string);
    // allow the call to this function with only GtkListeStore* specified
    if (!fixed)
        fixed = &dummy;
    if (!deleted)
        deleted = &dummy;

    *fixed = 0;
    *deleted = 0;

    // init strings
    prev_id = g_string_new ("");
    prev_date_opened = g_string_new ("");
    prev_date_posted = g_string_new ("");
    prev_owner_id = g_string_new ("");
    prev_date = g_string_new ("");

    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (store), &iter);
    while (valid)
    {
        row_deleted = FALSE;
        row_fixed = FALSE;

        // Walk through the list, reading each row
        gtk_tree_model_get (GTK_TREE_MODEL (store), &iter,
                            ID, &id,
                            DATE_OPENED, &date_opened,
                            DATE_POSTED, &date_posted,
                            DUE_DATE, &due_date,
                            OWNER_ID, &owner_id,
                            DATE, &date,
                            QUANTITY, &quantity, PRICE, &price, -1);

        if (strlen (price) == 0)
        {
            // invalid row (no price given)
            // no fix possible -> delete row
            valid = gtk_list_store_remove (store, &iter);
            row_deleted = TRUE;
            g_string_append_printf (info,
                                    _("ROW %d DELETED, PRICE_NOT_SET: id=%s\n"),
                                    row, id);
        }
        // TODO: QTY get set to 1 later if field is empty.  Delete this section?
        else if (strlen (quantity) == 0)
        {
            // invalid row (no quantity given)
            // no fix possible -> delete row
            valid = gtk_list_store_remove (store, &iter);
            row_deleted = TRUE;
            g_string_append_printf (info, _("ROW %d DELETED, QTY_NOT_SET: id=%s\n"),
                                    row, id);
        }
        else
        {   // TODO: If id is empty get the next one in the series.  Bug 731105 
            if (strlen (id) == 0)
            {
                // no invoice id specified
                if (prev_id->len == 0)
                {
                    // cannot fix -> delete row
                    valid = gtk_list_store_remove (store, &iter);
                    row_deleted = TRUE;
                    g_string_append_printf (info,
                                            _("ROW %d DELETED, ID_NOT_SET\n"), row);
                }
                else
                {
                    // this is a fixable multi line invoice
                    gtk_list_store_set (store, &iter, ID, prev_id->str, -1);
                    row_fixed = TRUE;
                }
            }
            else
            {
                // remember invoice id (to be able to fix multi line invoices)
                g_string_assign (prev_id, id);
                // new invoice => reset all other fixable entries
                g_string_assign (prev_date_opened, "");
                g_string_assign (prev_date_posted, "");
                g_string_assign (prev_owner_id, "");
                g_string_assign (prev_date, "");
            }
        }

        if (!row_deleted)
        {
            // the row is valid (price and id are valid)

            if(!isDateValid(date_opened))
            {
                if (prev_date_opened->len == 0)
                {
                    // fix this by using the current date
                    gchar temp[20];
                    GDate date;
                    g_date_clear (&date, 1);
                    gnc_gdate_set_today (&date);
                    g_date_strftime (temp, 20, date_format_string, &date);	// Create a user specified date string.
                    g_string_assign (prev_date_opened, temp);
                }
                // fix this by using the previous date_opened value (multi line invoice)
                gtk_list_store_set (store, &iter, DATE_OPENED,
                                    prev_date_opened->str, -1);
                row_fixed = TRUE;
            }
            else
            {
                // remember date_opened (to be able to fix multi line invoices)
                g_string_assign (prev_date_opened, date_opened);
            }

            // date_opened is valid

             if(!isDateValid(date_posted))
             {
                if (prev_date_posted->len == 0)
                {
                    // this invoice will have to get posted manually
                }
                else
                {
                    // multi line invoice => fix it
                    gtk_list_store_set (store, &iter, DATE_POSTED,
                                        prev_date_posted->str, -1);
                    row_fixed = TRUE;
                }
            }
            else
            {
                // remember date_opened (to be able to fix multi line invoices)
                g_string_assign (prev_date_posted, date_posted);
            }

            // date_posted is valid
            /*
            // Check if due date is valid.  Set it to date_posted if not valid or missing.
            if(!isDateValid(due_date))
            {
                gtk_list_store_set (store, &iter, DUE_DATE,
                                        date_posted, -1);
                row_fixed = TRUE;
                
            }
            
            // due_date is valid
            */
            if (strlen (quantity) == 0)
            {
                // quantity is unset => set to 1
                gtk_list_store_set (store, &iter, QUANTITY, "1", -1);
                row_fixed = TRUE;
            }
            

            // quantity is valid

            if (strlen (owner_id) == 0)
            {
                if (prev_owner_id->len == 0)
                {
                    // no customer given and not fixable => delete row
                    valid = gtk_list_store_remove (store, &iter);
                    row_deleted = TRUE;
                    g_string_append_printf (info,
                                            _("ROW %d DELETED, OWNER_NOT_SET: id=%s\n"),
                                            row, id);
                }
                else
                {
                    gtk_list_store_set (store, &iter, owner_id,
                                        prev_owner_id->str, -1);
                    row_fixed = TRUE;
                }
            }
            else
            {
                // remember owner_id
                g_string_assign (prev_owner_id, owner_id);
            }
            if (g_ascii_strcasecmp (type, "BILL") == 0)
            {
                // BILL: check, if vendor exists
                if (!gnc_search_vendor_on_id
                        (gnc_get_current_book (), prev_owner_id->str))
                {
                    // vendor not found => delete row
                    valid = gtk_list_store_remove (store, &iter);
                    row_deleted = TRUE;
                    g_string_append_printf (info,
                                            _("ROW %d DELETED, VENDOR_DOES_NOT_EXIST: id=%s\n"),
                                            row, id);
                }
            }
            else if (g_ascii_strcasecmp (type, "INVOICE") == 0)
            {
                // INVOICE: check, if customer exists
                if (!gnc_search_customer_on_id
                        (gnc_get_current_book (), prev_owner_id->str))
                {
                    // customer not found => delete row
                    valid = gtk_list_store_remove (store, &iter);
                    row_deleted = TRUE;
                    g_string_append_printf (info,
                                            _("ROW %d DELETED, CUSTOMER_DOES_NOT_EXIST: id=%s\n"),
                                            row, id);
                }
            }

            // owner_id is valid
        }

        g_free (id);
        g_free (date_opened);
        g_free (date_posted);
        g_free (owner_id);
        g_free (date);
        g_free (quantity);
        g_free (price);
        if (row_deleted)
        {
            (*deleted)++;
            // reset all remembered values
            g_string_assign (prev_id, "");
            g_string_assign (prev_date_opened, "");
            g_string_assign (prev_date_posted, "");
            g_string_assign (prev_owner_id, "");
            g_string_assign (prev_date, "");
        }
        else if (row_fixed)
            (*fixed)++;

        if (!row_deleted)
            valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter);

        row++;
    }

    // deallocate strings
    g_string_free (prev_id, TRUE);
    g_string_free (prev_date_opened, TRUE);
    g_string_free (prev_date_posted, TRUE);
    g_string_free (prev_owner_id, TRUE);
    g_string_free (prev_date, TRUE);

    if (info && (info->len > 0))
    {
        g_string_prepend (info, "\n\n");
        g_string_prepend (info, _("These rows were deleted:"));
    }
}


/***********************************************************************
 * @todo Maybe invoice checking should be done in gnc_bi_import_fix_bis (...)
 * rather than in here?  But that is more concerned with ensuring the csv is consistent.
 * @param GtkListStore *store
 * @param guint *n_invoices_created
 * @param guint *n_invoices_updated
 * @return void
 ***********************************************************************/
void
gnc_bi_import_create_bis (GtkListStore * store, QofBook * book,
                          guint * n_invoices_created,
                          guint * n_invoices_updated,
                          gchar * type, gchar * open_mode, GString * info)
{
    gboolean valid;
    GtkTreeIter iter;
    gchar *id, *date_opened, *owner_id, *billing_id, *notes;
    gchar *date, *desc, *action, *account, *quantity, *price, *disc_type,
          *disc_how, *discount, *taxable, *taxincluded, *tax_table;
    gchar *date_posted, *due_date, *account_posted, *memo_posted,
          *accumulatesplits;
    guint dummy;
    GncInvoice *invoice;
    GncEntry *entry;
    gint day, month, year;
    gnc_numeric value;
    GncOwner *owner;
    Account *acc;
    enum update {YES = GTK_RESPONSE_YES, NO = GTK_RESPONSE_NO} update;
    GtkWidget *dialog;
    Timespec today;
    InvoiceWindow *iw;
    gchar *new_id = NULL;
    gint64 denom = 0;
    gnc_commodity *currency;
    
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
    update = NO;

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

        // TODO:  Assign a new invoice number if one is absent.  BUT we don't want to assign a new invoice for every line!!
        // so we'd have to flag this up somehow or add an option in the import GUI.  The former implies that we make
        // an assumption about what the importer (person) wants to do.  It seems reasonable that a CSV file full of items with
        // If an invoice exists then we add to it in this current schema.
        // no predefined invoice number is a new invoice that's in need of a new number.
        // This was  not designed to satisfy the need for repeat invoices however, so maybe we need a another method for this, after all
        // It should be easier to copy an invoice with a new ID than to go through all this malarky.
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
            if (strlen (date_opened) != 0)	// If a date is specified in CSV
            {
                // FIXME: Must check for the return value of qof_scan_date!
                qof_scan_date (date_opened, &day, &month, &year);
                gncInvoiceSetDateOpened (invoice,
                                         gnc_dmy2timespec (day, month, year));
            }
            else			// If no date in CSV
            {
                time64 now = gnc_time (NULL);
                Timespec now_timespec;
                timespecFromTime64 (&now_timespec, now);
                gncInvoiceSetDateOpened (invoice, now_timespec);
            }
            gncInvoiceSetBillingID (invoice, billing_id ? billing_id : "");
            gncInvoiceSetNotes (invoice, notes ? notes : "");
            gncInvoiceSetActive (invoice, TRUE);
            //if (g_ascii_strcasecmp(type,"INVOICE"))gncInvoiceSetBillTo( invoice, billto );
            (*n_invoices_created)++;
            update = YES;

            // open new bill / invoice in a tab, if requested
            if (g_ascii_strcasecmp(open_mode, "ALL") == 0
                    || (g_ascii_strcasecmp(open_mode, "NOT_POSTED") == 0
                        && strlen(date_posted) == 0))
            {
                iw =  gnc_ui_invoice_edit (invoice);
                gnc_plugin_page_invoice_new (iw);
            }
            gncInvoiceCommitEdit (invoice);
        }
// I want to warn the user that an existing billvoice exists, but not every
// time.
// An import can contain many lines usually referring to the same invoice.
// NB: Posted invoices are NEVER updated.
        else			// if invoice exists
        {
            if (gncInvoiceIsPosted (invoice))	// Is it already posted?
            {
                valid =
                    gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter);
                continue;		// If already posted then never import
            }
            if (update != YES)	// Pop up a dialog to ask if updates are the expected action
            {
                dialog = gtk_message_dialog_new (NULL,
                                                 GTK_DIALOG_MODAL,
                                                 GTK_MESSAGE_ERROR,
                                                 GTK_BUTTONS_YES_NO,
                                                 "%s",
                                                 _("Are you sure you have bills/invoices to update?"));
                update = gtk_dialog_run (GTK_DIALOG (dialog));
                gtk_widget_destroy (dialog);
                if (update == NO)
                {
                    // Cleanup and leave
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
                    return;
                }
            }
            (*n_invoices_updated)++;
        }


        // add entry to invoice/bill
        entry = gncEntryCreate (book);
        gncEntryBeginEdit(entry);
        currency = gncInvoiceGetCurrency(invoice);
        if (currency) denom = gnc_commodity_get_fraction(currency);
        // FIXME: Must check for the return value of qof_scan_date!
        qof_scan_date (date, &day, &month, &year);
        {
            GDate *date = g_date_new_dmy(day, month, year);
            gncEntrySetDateGDate (entry, date);
            g_date_free (date);
        }
        timespecFromTime64 (&today, gnc_time (NULL));	// set today to the current date
        gncEntrySetDateEntered (entry, today);
        gncEntrySetDescription (entry, desc);
        gncEntrySetAction (entry, action);

        gnc_exp_parser_parse (quantity, &value, NULL);
        gncEntrySetQuantity (entry, value);
        acc = gnc_account_lookup_for_register (gnc_get_current_root_account (),
                                               account);
        
        if (g_ascii_strcasecmp (type, "BILL") == 0)
        {
            gncEntrySetBillAccount (entry, acc);
            gnc_exp_parser_parse (price, &value, NULL);
            gncEntrySetBillPrice (entry, value);
            gncEntrySetBillTaxable (entry, text2bool (taxable));
            gncEntrySetBillTaxIncluded (entry, text2bool (taxincluded));
            gncEntrySetBillTaxTable (entry, gncTaxTableLookupByName (book, tax_table));
            gncEntryCommitEdit(entry);
            gncBillAddEntry (invoice, entry);
        }
        else if (g_ascii_strcasecmp (type, "INVOICE") == 0)
        {
            gncEntrySetNotes (entry, notes);
            gncEntrySetInvAccount (entry, acc);
            gnc_exp_parser_parse (price, &value, NULL);
            gncEntrySetInvPrice (entry, value);
            gncEntrySetInvTaxable (entry, text2bool (taxable));
            gncEntrySetInvTaxIncluded (entry, text2bool (taxincluded));
            gncEntrySetInvTaxTable (entry, gncTaxTableLookupByName (book, tax_table));
            gnc_exp_parser_parse (discount, &value, NULL);
            gncEntrySetInvDiscount (entry, value);
            gncEntrySetInvDiscountType (entry, text2disc_type (disc_type));
            gncEntrySetInvDiscountHow (entry, text2disc_how (disc_how));
            gncEntryCommitEdit(entry);
            gncInvoiceAddEntry (invoice, entry);
        }
        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter);

        // handle auto posting of invoices
    
        
        if (valid)
            gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, ID, &new_id, -1);
        if (g_strcmp0 (id, new_id) != 0)
        {
            // the next invoice id is different => try to autopost this invoice
            if (qof_scan_date (date_posted, &day, &month, &year))
            {
                // autopost this invoice
                gboolean auto_pay;
                Timespec d1, d2;

                if (g_ascii_strcasecmp (type, "INVOICE") == 0)
                    auto_pay = gnc_prefs_get_bool (GNC_PREFS_GROUP_INVOICE, GNC_PREF_AUTO_PAY);
                else
                    auto_pay = gnc_prefs_get_bool (GNC_PREFS_GROUP_BILL, GNC_PREF_AUTO_PAY);

                d1 = gnc_dmy2timespec (day, month, year);
                // FIXME: Must check for the return value of qof_scan_date!
                qof_scan_date (due_date, &day, &month, &year);	// obtains the due date, or leaves it at date_posted
                d2 = gnc_dmy2timespec (day, month, year);
                acc = gnc_account_lookup_for_register
                      (gnc_get_current_root_account (), account_posted);
                gncInvoicePostToAccount (invoice, acc, &d1, &d2,
                                         memo_posted,
                                         text2bool (accumulatesplits),
                                         auto_pay);
            }

        }
        
        
    }
    // cleanup
    g_free (new_id);
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
    
}
