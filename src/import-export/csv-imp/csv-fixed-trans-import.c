/*******************************************************************\
 * csv-fixed-trans-import.c -- Fixed Format Transaction importing   *
 *                                           from file.             *
 *                                                                  *
 * Copyright (C) 2014 Robert Fewell                                 *
 *                                                                  *
 * Based on code from bi_import written by Sebastian Held  and      *
 * Mike Evans.                                                      *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include "gnc-ui-util.h"
#include <regex.h>
#include "Account.h"
#include "gnc-component-manager.h"
#include "csv-fixed-trans-import.h"
#include "gnc-csv-model.h"
#include "gnc-ui-util.h"
#include "engine-helpers.h"
#include "gnc-gdate-utils.h"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_ASSISTANT;


/* This helper function takes a regexp match and fills the model */
static void
fill_model_with_match (GMatchInfo *match_info,
               const gchar *match_name,
               GtkListStore *store,
               GtkTreeIter *iterptr,
               gint column)
{
    gchar *temp;

    if (!match_info || !match_name)
        return;

    temp = g_match_info_fetch_named (match_info, match_name);
    if (temp)
    {
        g_strstrip (temp);
        if (g_str_has_prefix (temp, "\""))
        {
            if (strlen (temp) >= 2)
            {
                gchar *toptail = g_strndup (temp + 1, strlen (temp)-2);
                gchar **parts = g_strsplit (toptail, "\"\"", -1);
                temp = g_strjoinv ("\"", parts);
                g_strfreev (parts);
                g_free (toptail);
            }
        }
        gtk_list_store_set (store, iterptr, column, temp, -1);
        g_free (temp);
    }
}

/*******************************************************
 * csv_fixed_trans_import_read_file
 *
 * Parse the file for a correctly formated file
 *******************************************************/
csv_fixed_trans_import_result
csv_fixed_trans_import_read_file (const gchar *filename, const gchar *parser_regexp,
                      GtkListStore *store, guint max_rows)
{
    gchar      *locale_cont, *contents;
    GMatchInfo *match_info = NULL;
    GRegex     *regexpat = NULL;
    GError     *err;
    gint       row = 0;
    gboolean   match_found = FALSE;

    // model
    GtkTreeIter iter;

    if (!g_file_get_contents (filename, &locale_cont, NULL, NULL))
    {
        //gnc_error_dialog( 0, _("File %s cannot be opened."), filename);
        return RESULT_OPEN_FAILED;
    }

    contents = g_locale_to_utf8 (locale_cont, -1, NULL, NULL, NULL);
    g_free (locale_cont);

    // compile the regular expression and check for errors
    err = NULL;
    regexpat =
        g_regex_new (parser_regexp, G_REGEX_OPTIMIZE, 0, &err);
    if (err != NULL)
    {
        GtkWidget *dialog;
        gchar *errmsg;

        errmsg = g_strdup_printf (_("Error in regular expression '%s':\n%s"),
                                  parser_regexp, err->message);
        g_error_free (err);

        dialog = gtk_message_dialog_new (NULL,
                                         GTK_DIALOG_MODAL,
                                         GTK_MESSAGE_ERROR,
                                         GTK_BUTTONS_OK, "%s", errmsg);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        g_free (errmsg);
        g_free (contents);

        return RESULT_ERROR_IN_REGEXP;
    }

    g_regex_match (regexpat, contents, 0, &match_info);
    while (g_match_info_matches (match_info))
    {
        match_found = TRUE;
        // fill in the values
        gtk_list_store_append (store, &iter);
        fill_model_with_match (match_info, "date", store, &iter, FTDATE);
        fill_model_with_match (match_info, "type", store, &iter, FTTYPE);
        fill_model_with_match (match_info, "sdate", store, &iter, FTSDATE);
        fill_model_with_match (match_info, "acct_name", store, &iter, FTACCT_NAME);
        fill_model_with_match (match_info, "number", store, &iter, FTNUMBER);
        fill_model_with_match (match_info, "description", store, &iter, FTDESCRIPTION);
        fill_model_with_match (match_info, "notes", store, &iter, FTNOTES);
        fill_model_with_match (match_info, "memo", store, &iter, FTMEMO);
        fill_model_with_match (match_info, "full_cat_name", store, &iter, FTFULL_CAT_NAME);
        fill_model_with_match (match_info, "cat_name", store, &iter, FTCAT_NAME);
        fill_model_with_match (match_info, "row_type", store, &iter, FTRTYPE);
        fill_model_with_match (match_info, "action", store, &iter, FTACTION);
        fill_model_with_match (match_info, "reconcile", store, &iter, FTRECONCILE);
        fill_model_with_match (match_info, "amount_with_sym", store, &iter, FTAMOUNT_WITH_SYM);
        fill_model_with_match (match_info, "commoditym", store, &iter, FTCOMMODITYM);
        fill_model_with_match (match_info, "commodityn", store, &iter, FTCOMMODITYN);
        fill_model_with_match (match_info, "amount_num", store, &iter, FTAMOUNT_NUM);
        fill_model_with_match (match_info, "rate", store, &iter, FTRATE);
        gtk_list_store_set (store, &iter, FTROW_COLOR, NULL, -1);

        row++;
        if (row == max_rows)
            break;
        g_match_info_next (match_info, &err);
    }

    g_match_info_free (match_info);
    g_regex_unref (regexpat);
    g_free (contents);

    if (err != NULL)
    {
        g_printerr ("Error while matching: %s\n", err->message);
        g_error_free (err);
    }

    if (match_found == TRUE)
        return MATCH_FOUND;
    else
        return RESULT_OK;
}


/*******************************************************
 * save_error_text
 *
 * Add error text to existing errors
 *******************************************************/
static void
save_error_text (CsvFTImportInfo *info, gint row, gchar *etext)
{
    gchar *current_error_text;
    gchar *text;

    current_error_text = g_strdup (info->error);

    if (!g_strcmp0 (info->error, "") == 0)
        g_free (info->error);

    text = g_strdup_printf (gettext("Row %u, %s\n"), row + 1, etext);
    info->error = g_strconcat (current_error_text, text, NULL);
    g_free (text);
    g_free (current_error_text);
}


/*******************************************************
 * parse_number_string
 *
 * Parse the number string and return a gnc_number
 *******************************************************/
static gboolean
parse_number_string (CsvFTImportInfo *info, const gchar *num_string, gnc_numeric *ret_num)
{
    char       *endptr, *str_num;
    gboolean    valid = FALSE;
    gchar      *result = NULL;
    gnc_numeric num;

    str_num = g_strdup (num_string);

    /* Currency format */
    switch (info->currency_format)
    {
        case 0:
            /* Currency locale */
            valid = xaccParseAmount (str_num, TRUE, &num, &endptr);
            break;
        case 1:
            /* Currency decimal period */
            valid = xaccParseAmountExtended (str_num, TRUE, '-', '.', ',', "\003\003", "$+", &num, &endptr);
            break;
        case 2:
            /* Currency decimal comma */
            valid = xaccParseAmountExtended (str_num, TRUE, '-', ',', '.', "\003\003", "$+", &num, &endptr);
            break;
    }

    // xaccParseAmountExtended does not recognise trailing '-' numbers, lets scan string for any
    result = g_strrstr (num_string, "-");
    if ((result != NULL) && !gnc_numeric_negative_p (num))
        num = gnc_numeric_neg (num);

    *ret_num = num;
    g_free (str_num);

    return valid;
}


/*******************************************************
 * csv_fixed_trans_test_one_line
 *
 * Test a Transaction for valid date and split for valid number
 *******************************************************/
gboolean
csv_fixed_trans_test_one_line (CsvFTImportInfo *info)
{
    gboolean     valid;
    GtkTreeIter  iter;
    gchar       *date, *row_type, *amount_num;
    gnc_numeric  amount;
    gint         row;
    gboolean     trans_found = FALSE;
    gboolean     split_found = FALSE;
    gboolean     date_ok = FALSE;
    gboolean     num_ok = FALSE;

    row = info->header_rows;
    valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(info->store), &iter, NULL, row);

    while (valid)
    {
        /* Walk through the list, reading each row */
        gtk_tree_model_get (GTK_TREE_MODEL (info->store), &iter,
                            FTDATE, &date,
                            FTRTYPE, &row_type,
                            FTAMOUNT_NUM, &amount_num, -1);

        if (g_strcmp0 (row_type, "T") == 0) // We have the Transaction line
        {
            if (parse_date (date, info->date_format) == -1) // invalid date
                date_ok = FALSE;
            else
                date_ok = TRUE;
            trans_found = TRUE;
        }

        if (g_strcmp0 (row_type, "S") == 0) // We have the split line
        {
            if (g_strcmp0 (amount_num, "") != 0) // test for valid to number
            {
                if (parse_number_string (info, amount_num, &amount))
                    num_ok = TRUE;
                else
                    num_ok = FALSE;
            }
            else
                num_ok = FALSE;

            split_found = TRUE;
        }

    /* free resources */
    g_free (date);
    g_free (row_type);
    g_free (amount_num);

    if ((trans_found == TRUE) && (split_found == TRUE))
        break;

    valid = gtk_tree_model_iter_next (GTK_TREE_MODEL(info->store), &iter);
    row++;
    }

    if ((date_ok == TRUE) && (num_ok == TRUE))
       return TRUE;
    else
       return FALSE;
}


/*******************************************************
 * check_for_existing_trans
 *
 * Check if the Transaction allready exists
 *******************************************************/
static gboolean
check_for_existing_trans (CsvFTImportInfo *info, Account *acc, Transaction *trans, gnc_numeric value)
{
    Query       *q;
    GSList      *p1, *p2;
    GList       *splits;
    QofBook     *book;
    GDate        tdate;
    time64       time_val_start;
    time64       time_val_end;
    gboolean     ret = FALSE;
    const gchar *notes, *desc, *num;

    q = qof_query_create_for (GNC_ID_SPLIT);
    book = gnc_get_current_book();
    qof_query_set_book (q, book);

    /* Sort by transaction date */
    p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    p1 = g_slist_prepend (p1, SPLIT_TRANS);
    p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
    qof_query_set_sort_order (q, p1, p2, NULL);

    xaccQueryAddSingleAccountMatch (q, acc, QOF_QUERY_AND);

    tdate = xaccTransGetDatePostedGDate (trans);
    time_val_start = gnc_time64_get_day_start_gdate (&tdate);
    time_val_end = gnc_time64_get_day_end_gdate (&tdate);
    xaccQueryAddDateMatchTT (q, TRUE, time_val_start, TRUE, time_val_end, QOF_QUERY_AND);

    desc = xaccTransGetDescription (trans) ? xaccTransGetDescription (trans) : "" ;
    notes = xaccTransGetNotes (trans) ? xaccTransGetNotes (trans) : "" ;
    num = xaccTransGetNum (trans) ? xaccTransGetNum (trans) : "" ;

    xaccQueryAddNumberMatch (q, num, TRUE, FALSE, QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    xaccQueryAddDescriptionMatch (q, desc, TRUE, FALSE, QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    xaccQueryAddNotesMatch (q, notes, TRUE, FALSE, QOF_COMPARE_EQUAL, QOF_QUERY_AND);

    if (gnc_numeric_negative_p (value))
        xaccQueryAddValueMatch (q, value, QOF_NUMERIC_MATCH_CREDIT,
                               QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    else
        xaccQueryAddValueMatch (q, value, QOF_NUMERIC_MATCH_DEBIT,
                               QOF_COMPARE_EQUAL, QOF_QUERY_AND);

    // The returned list is managed internally by QofQuery, Do not free.
    splits = qof_query_run (q);

    if (g_list_length (splits) != 0)
        ret = TRUE;

    // Compare transaction types, for A/Payable/Recievable
    if (g_list_length (splits) == 1)
    {
        Split *split;
        GList *first_split = g_list_first (splits);
        split = first_split->data; 
        if (xaccTransGetTxnType (trans) == xaccTransGetTxnType (xaccSplitGetParent (split)))
            ret = TRUE;
        else
            ret = FALSE;
    }

    qof_query_destroy (q);
    return ret;
}


/*******************************************************
 * csv_fixed_trans_import
 *
 * Parse the liststore for new Transactions
 *******************************************************/
void
csv_fixed_trans_import (CsvFTImportInfo *info)
{
    QofBook             *book;
    Account             *root;
    gboolean             valid;
    GtkTreeIter          iter;
    gchar               *date, *type, *sdate, *acct_name, *number, *description, *notes, *memo;
    gchar               *full_cat_name, *cat_name, *row_type, *action, *reconcile;
    gchar               *amount_with_sym, *commoditym, *commodityn, *amount_num, *rate;
    gchar               *void_reason;
    gboolean             void_trans = FALSE;
    gnc_commodity       *trans_commodity, *split_commodity;
    gnc_commodity_table *table;
    Transaction         *new_trans = NULL, *prev_trans = NULL;
    gboolean             split_check = FALSE;
    gdouble              row, trow, max_row;
    GtkProgressBar      *progress;
    gdouble              percent = 0.0;

    ENTER("");
    book = gnc_get_current_book();
    root = gnc_book_get_root_account (book);
    table = gnc_commodity_table_get_table (book);

    info->num_new = 0;
    info->num_duplicates = 0;
    void_reason = g_strdup (" ");

    /* Move to the first valid entry in store */
    row = info->header_rows;
    valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(info->store), &iter, NULL, row);

    max_row = gtk_tree_model_iter_n_children (GTK_TREE_MODEL(info->store), NULL);

    progress = GTK_PROGRESS_BAR(info->progressbar);

    gnc_suspend_gui_refresh ();

    while (valid)
    {
        gchar *message = g_strdup_printf (gettext("%.0f%% Complete"), percent * 100);
        percent = (row / max_row);
        gtk_progress_bar_set_fraction (progress, percent);
        gtk_progress_bar_set_text (progress, message);
        g_free (message);

        while (gtk_events_pending ())
           gtk_main_iteration ();

        /* Walk through the list, reading each row */
        gtk_tree_model_get (GTK_TREE_MODEL(info->store), &iter,
                            FTDATE, &date,
                            FTTYPE, &type,
                            FTSDATE, &sdate,
                            FTACCT_NAME, &acct_name,
                            FTNUMBER, &number,
                            FTDESCRIPTION, &description,
                            FTNOTES, &notes,
                            FTMEMO, &memo,
                            FTFULL_CAT_NAME, &full_cat_name,
                            FTCAT_NAME, &cat_name,
                            FTRTYPE, &row_type,
                            FTACTION, &action,
                            FTRECONCILE, &reconcile,
                            FTAMOUNT_WITH_SYM, &amount_with_sym,
                            FTCOMMODITYM, &commoditym,
                            FTCOMMODITYN, &commodityn,
                            FTAMOUNT_NUM, &amount_num,
                            FTRATE, &rate, -1);

        if (g_strcmp0 (row_type, "T") == 0) // We have the Transaction line
        {
            gboolean trans_error = FALSE;

            new_trans = NULL; // Reset new_trans to NULL
            trow = row; //record the row the Transaction was on

            if (parse_date (date, info->date_format) == -1) // invalid date
            {
                save_error_text (info, row, _("Date is invalid for Transaction"));
                trans_error = TRUE;
            }

            trans_commodity = gnc_commodity_table_lookup (table, commodityn, commoditym);
            if (!trans_commodity) // invalid commodity
            {
                save_error_text (info, row, _("Commodity is invalid for Transaction"));
                trans_error = TRUE;
            }

            if (trans_error == FALSE) // Create Transaction
            {
                new_trans = xaccMallocTransaction (book);

                if (prev_trans != NULL)
                {
                    if (void_trans) // Test to Void Transaction
                        xaccTransVoid (prev_trans, void_reason);
                    xaccTransCommitEdit (prev_trans); // commit the previous transaction
                    info->num_new = info->num_new + 1;
                    void_trans = FALSE;
                }

                xaccTransBeginEdit (new_trans);
                xaccTransSetCurrency (new_trans, trans_commodity);
                xaccTransSetDatePostedSecsNormalized (new_trans, parse_date (date, info->date_format));
                if (!g_strcmp0 (description, "") == 0)
                    xaccTransSetDescription (new_trans, description);
                if (!g_strcmp0 (notes, "") == 0)
                    xaccTransSetNotes (new_trans, notes);
                if (!g_strcmp0 (number, "") == 0)
                    xaccTransSetNum (new_trans, number);

                if (g_strcmp0 (type, "I") == 0) // Invoice Transaction Type
                {
                    Timespec ts;
                    timespecFromTime64 (&ts, parse_date (sdate, info->date_format));
                    xaccTransSetTxnType (new_trans, 'I');
                    xaccTransSetDateDueTS (new_trans, &ts);
                }
                if (g_strcmp0 (type, "P") == 0) // Payment Transaction Type
                    xaccTransSetTxnType (new_trans, 'P');
                if (g_strcmp0 (type, "L") == 0) // Lot Link Transaction Type
                    xaccTransSetTxnType (new_trans, 'L');
 
                prev_trans = new_trans;

                split_check = FALSE; // Reset split_check
            }
        }

        if ((g_strcmp0 (row_type, "S") == 0) && (new_trans != NULL)) // We have a Split line
        {
            Account    *acct;
            Split      *split;
            gnc_numeric amount, value, price_rate;
            char        rec;
            char       *endptr, *str_num, *str_rate;
            gboolean    split_error = FALSE;
            gboolean    num_error = FALSE;
            gboolean    rate_error = FALSE;
            gboolean    type_deb;

            split_commodity = gnc_commodity_table_lookup (table, commodityn, commoditym);

            acct = gnc_account_lookup_by_full_name (root, full_cat_name);
            if (!acct) // invalid account
            {
                save_error_text (info, row, _("Account is invalid for Split"));
                split_error = TRUE;
            }

            if (!split_commodity) // invalid commodity
            {
                save_error_text (info, row, _("Commodity is invalid for Split"));
                split_error = TRUE;
            }

            if (!gnc_commodity_equal (split_commodity, xaccAccountGetCommodity (acct))) // non matching commodity
            {
                save_error_text (info, row, _("Commodity does not match account for Split"));
                split_error = TRUE;
            }

            // Lets get some numbers
            num_error = parse_number_string (info, amount_num, &amount);
            rate_error = parse_number_string (info, rate, &price_rate);

            if (!num_error) // invalid amount
            {
                save_error_text (info, row, _("Numeric Amount is invalid for Split"));
                split_error = TRUE;
            }

            if (!rate_error) // invalid rate/price
            {
                save_error_text (info, row, _("Numeric Rate/Price is invalid for Split"));
                split_error = TRUE;
            }

            if (split_error == FALSE)
                value = gnc_numeric_mul (amount, price_rate, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);

            // Check to see if Transaction allready exists
            if ((acct != NULL) && (split_check == FALSE)) // we only check first split
            {
                gboolean ret_error = check_for_existing_trans (info, acct, new_trans, value);
                if (ret_error)
                {
                    const gchar *desc, *notes;
                    gchar       *error_text;

                    desc = xaccTransGetDescription (new_trans) ? xaccTransGetDescription (new_trans) : "" ;
                    notes = xaccTransGetNotes (new_trans) ? xaccTransGetNotes (new_trans) : "" ;

                    error_text = g_strdup_printf (gettext (
                                "Transaction already Exists, Description was '%s' and Notes was '%s', Skipping"),
                                 desc, notes);

                    save_error_text (info, trow, error_text);
                    info->num_duplicates = info->num_duplicates + 1;
                    g_free (error_text);
                    split_error = TRUE;
                }
                split_check = TRUE;
            }

            if (split_error == FALSE)
            {
                split = xaccMallocSplit (book);
                xaccSplitSetAccount (split, acct);
                xaccSplitSetParent (split, new_trans);
                if (!g_strcmp0 (memo, "") == 0)
                    xaccSplitSetMemo (split, memo);
                if (!g_strcmp0 (action, "") == 0)
                    xaccSplitSetAction (split, action);

                if (g_strcmp0 (reconcile, _("n")) == 0) // Normal
                    rec = 'n';
                else if (g_strcmp0 (reconcile, _("c")) == 0) // Cleared
                    rec = 'c';
                else if (g_strcmp0 (reconcile, _("y")) == 0) // Reconciled
                {
                    Timespec ts;
                    timespecFromTime64 (&ts, parse_date (sdate, info->date_format));
                    xaccSplitSetDateReconciledTS (split, &ts);
                    rec = 'y';
                }
                else if (g_strcmp0 (reconcile, _("f")) == 0) // Frozen
                    rec = 'f';
                else if (g_strcmp0 (reconcile, _("v")) == 0) // Void 
                {
                    g_free (void_reason);
                    void_reason = g_strdup (notes);
                    void_trans = TRUE;
                    rec = 'v';
                }
                else /* default: */
                {
                    save_error_text (info, row, _("Unrecognised Reconcile flag, using default for Split"));
                    rec = 'n';
                }
                xaccSplitSetReconcile (split, rec);

                // Set Amount and Value entries
                xaccSplitSetAmount (split, amount);
                xaccSplitSetValue (split, value);
            }
            else
            {
                 // We have an Error, lets destroy the Started Transaction
                 xaccTransDestroy (new_trans);
                 xaccTransCommitEdit (new_trans);
                 new_trans = NULL;
                 prev_trans = NULL;
            }
        }

        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL(info->store), &iter);
        row++;

        /* free resources */
        g_free (date);
        g_free (type);
        g_free (sdate);
        g_free (acct_name);
        g_free (number);
        g_free (description);
        g_free (notes);
        g_free (memo);
        g_free (full_cat_name);
        g_free (cat_name);
        g_free (row_type);
        g_free (action);
        g_free (reconcile);
        g_free (amount_with_sym);
        g_free (commoditym);
        g_free (commodityn);
        g_free (amount_num);
        g_free (rate);
    }

    if (new_trans != NULL)
    {
        if (void_trans) // Test to Void Transaction
            xaccTransVoid (new_trans, void_reason);
        xaccTransCommitEdit (new_trans); // commit the last new Transaction
        info->num_new = info->num_new + 1;
    }
    g_free (void_reason);

    gnc_resume_gui_refresh ();

    LEAVE("");
}
