/*******************************************************************\
 * csv-actions-export.c -- Export Transactions to a file       *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
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
/** @file csv-transactions-export.c
    @brief CSV Export Transactions
    @author Copyright (c) 2012 Robert Fewell
*/
#include "config.h"

#include <glib/gstdio.h>
#include <stdbool.h>

#include "gnc-commodity.h"
#include "gnc-ui-util.h"
#include "Query.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "qofbookslots.h"

#include "csv-transactions-export.h"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/* CSV spec requires CRLF line endings. Tweak the end-of-line string so this
 * true for each platform */
#ifdef G_OS_WIN32
# define EOLSTR "\n"
#else
# define EOLSTR "\r\n"
#endif


/*******************************************************************/

/*******************************************************
 * write_line_to_file
 *
 * write a text string to a file pointer, return true if
 * successful.
 *******************************************************/
static
bool write_line_to_file (FILE *fh, char * line)
{
    DEBUG("Account String: %s", line);

    /* Write account line */
    int len = strlen (line);
    int written = fwrite (line, 1, len, fh);

    return (written == len);
}


/*******************************************************
 * csv_txn_test_field_string
 *
 * Test the field string for ," and new lines
 *******************************************************/
static
gchar *csv_txn_test_field_string (CsvExportInfo *info, const gchar *string_in)
{
    /* Check for " and then "" them */
    gchar **parts = g_strsplit (string_in, "\"", -1);
    gchar *string_parts = g_strjoinv ("\"\"", parts);
    g_strfreev (parts);

    /* Check for separator string and \n and " in field,
       if so quote field if not already quoted */
    bool need_quote = !g_strrstr (string_parts, info->separator_str) ||
                      !g_strrstr (string_parts, "\n") ||
                      !g_strrstr (string_parts, "\"");

    gchar *string_out;
    if (!info->use_quotes && need_quote)
        string_out = g_strconcat ("\"", string_parts, "\"", NULL);
    else
        string_out = g_strdup (string_parts);

    g_free (string_parts);
    return string_out;
}

/******************** Helper functions *********************/

// Transaction Date
static gchar*
add_date (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    gchar *date = qof_print_date (xaccTransGetDate (trans));
    gchar *result = g_strconcat (so_far, info->end_sep, date, info->mid_sep, NULL);
    g_free (date);
    g_free (so_far);
    return result;
}


// Transaction GUID
static gchar*
add_guid (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    gchar *guid = guid_to_string (xaccTransGetGUID (trans));
    gchar *result = g_strconcat (so_far, guid, info->mid_sep, NULL);
    g_free (guid);
    g_free (so_far);
    return result;
}

// Reconcile Date
static gchar*
add_reconcile_date (gchar *so_far, Split *split, CsvExportInfo *info)
{
    gchar *result;
    if (xaccSplitGetReconcile (split) == YREC)
    {
        time64 t = xaccSplitGetDateReconciled (split);
        char str_rec_date[MAX_DATE_LENGTH + 1];
        memset (str_rec_date, 0, sizeof(str_rec_date));
        qof_print_date_buff (str_rec_date, MAX_DATE_LENGTH, t);
        result = g_strconcat (so_far, str_rec_date, info->mid_sep, NULL);
    }
    else
        result = g_strconcat (so_far, info->mid_sep, NULL);

    g_free (so_far);
    return result;
}

// Account Name short or Long
static gchar*
add_account_name (gchar *so_far, Split *split, bool full, CsvExportInfo *info)
{
    Account *account = xaccSplitGetAccount (split);
    gchar *name = NULL;
    if (full)
        name = gnc_account_get_full_name (account);
    else
        name = g_strdup (xaccAccountGetName (account));
    gchar *conv = csv_txn_test_field_string (info, name);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (name);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Number
static gchar*
add_number (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *num = xaccTransGetNum (trans);
    num = num ? num : "";
    gchar *conv = csv_txn_test_field_string (info, num);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Description
static gchar*
add_description (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *desc = xaccTransGetDescription (trans);
    desc = desc ? desc : "";
    gchar *conv = csv_txn_test_field_string (info, desc);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Notes
static gchar*
add_notes (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *notes = xaccTransGetNotes (trans);
    notes = notes ? notes : "" ;
    gchar *conv = csv_txn_test_field_string (info, notes);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Void reason
static gchar*
add_void_reason (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *void_reason = xaccTransGetVoidReason (trans);
    void_reason = void_reason ? void_reason : "";
    gchar *conv = csv_txn_test_field_string (info, void_reason);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Memo
static gchar*
add_memo (gchar *so_far, Split *split, CsvExportInfo *info)
{
    const gchar *memo = xaccSplitGetMemo (split);
    memo = memo ? memo : "";
    gchar *conv = csv_txn_test_field_string (info, memo);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Full Category Path or Not
static gchar*
add_category (gchar *so_far, Split *split, bool full, CsvExportInfo *info)
{
    gchar *cat;
    if (full)
        cat = xaccSplitGetCorrAccountFullName (split);
    else
        cat = g_strdup(xaccSplitGetCorrAccountName (split));

    gchar *conv = csv_txn_test_field_string (info, cat);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (cat);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Action
static gchar*
add_action (gchar *so_far, Split *split, CsvExportInfo *info)
{
    const gchar *action = xaccSplitGetAction (split);
    gchar *conv = csv_txn_test_field_string (info, action);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Reconcile
static gchar*
add_reconcile (gchar *so_far, Split *split, CsvExportInfo *info)
{
    const gchar *recon = gnc_get_reconcile_str (xaccSplitGetReconcile (split));
    gchar *conv = csv_txn_test_field_string (info, recon);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Transaction commodity
static gchar*
add_commodity (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *comm_m = gnc_commodity_get_unique_name (xaccTransGetCurrency (trans));
    gchar *conv = csv_txn_test_field_string (info, comm_m);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Amount with Symbol or not
static gchar*
add_amount (gchar *so_far, Split *split, bool t_void, bool symbol, CsvExportInfo *info)
{
    const gchar *amt;
    if (t_void)
        amt = xaccPrintAmount (xaccSplitVoidFormerAmount (split), gnc_split_amount_print_info (split, symbol));
    else
        amt = xaccPrintAmount (xaccSplitGetAmount (split), gnc_split_amount_print_info (split, symbol));
    gchar *conv = csv_txn_test_field_string (info, amt);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Value with Symbol or not
static gchar*
add_value (gchar *so_far, Split *split, bool t_void, bool symbol, CsvExportInfo *info)
{
    Transaction *trans = xaccSplitGetParent(split);
    gnc_commodity *tcurr = xaccTransGetCurrency (trans);
    GNCPrintAmountInfo pai = gnc_commodity_print_info (tcurr, symbol);
    const gchar *amt;
    if (t_void)
        amt = xaccPrintAmount (xaccSplitVoidFormerValue (split), pai);
    else
        amt = xaccPrintAmount (xaccSplitGetValue (split), pai);
    gchar *conv = csv_txn_test_field_string (info, amt);
    gchar *result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Share Price / Conversion factor
static gchar*
add_rate (gchar *so_far, Split *split, bool t_void, CsvExportInfo *info)
{
    gnc_commodity *curr = xaccAccountGetCommodity (xaccSplitGetAccount (split));
    const gchar *amt;
    if (t_void)
        amt = xaccPrintAmount (gnc_numeric_zero(), gnc_default_price_print_info (curr));
    else
        amt = xaccPrintAmount (xaccSplitGetSharePrice (split), gnc_default_price_print_info (curr));
    gchar *conv = csv_txn_test_field_string (info, amt);
    gchar *result = g_strconcat (so_far, conv, info->end_sep, EOLSTR, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Share Price / Conversion factor
static gchar*
add_price (gchar *so_far, Split *split, bool t_void, CsvExportInfo *info)
{
    gnc_commodity *curr = xaccAccountGetCommodity (xaccSplitGetAccount (split));
    const gchar *string_amount;
    if (t_void)
    {
        gnc_numeric cf = gnc_numeric_div (xaccSplitVoidFormerValue (split), xaccSplitVoidFormerAmount (split), GNC_DENOM_AUTO,
                                                   GNC_HOW_DENOM_SIGFIGS(6) | GNC_HOW_RND_ROUND_HALF_UP);
        string_amount = xaccPrintAmount (cf, gnc_default_price_print_info (curr));
    }
    else
        string_amount = xaccPrintAmount (xaccSplitGetSharePrice (split), gnc_default_price_print_info (curr));

    gchar *conv = csv_txn_test_field_string (info, string_amount);
    gchar *result = g_strconcat (so_far, conv, info->end_sep, EOLSTR, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

/******************************************************************************/

static gchar*
make_simple_trans_line (Transaction *trans, Split *split, CsvExportInfo *info)
{
    bool t_void = xaccTransGetVoidStatus (trans);

    gchar *exp_line = g_strdup("");
    exp_line = add_date (exp_line, trans, info);
    exp_line = add_account_name (exp_line, split, true, info);
    exp_line = add_number (exp_line, trans, info);
    exp_line = add_description (exp_line, trans, info);
    exp_line = add_category (exp_line, split, true, info);
    exp_line = add_reconcile (exp_line, split, info);
    exp_line = add_amount (exp_line, split, t_void, true, info);
    exp_line = add_amount (exp_line, split, t_void, false, info);
    exp_line = add_value (exp_line, split, t_void, true, info);
    exp_line = add_value (exp_line, split, t_void, false, info);
    exp_line = add_rate (exp_line, split, t_void, info);
    return exp_line;
}

static gchar*
make_complex_trans_line (Transaction *trans, Split *split, CsvExportInfo *info)
{
    // Transaction fields
    gchar *exp_line = g_strdup("");
    exp_line = add_date (exp_line, trans, info);
    exp_line = add_guid (exp_line, trans, info);
    exp_line = add_number (exp_line, trans, info);
    exp_line = add_description (exp_line, trans, info);
    exp_line = add_notes (exp_line, trans, info);
    exp_line = add_commodity (exp_line, trans, info);
    exp_line = add_void_reason (exp_line, trans, info);
    bool t_void = xaccTransGetVoidStatus (trans);

    //Split fields
    exp_line = add_action (exp_line, split, info);
    exp_line = add_memo (exp_line, split, info);
    exp_line = add_account_name (exp_line, split, true, info);
    exp_line = add_account_name (exp_line, split, false, info);
    exp_line = add_amount (exp_line, split, t_void, true, info);
    exp_line = add_amount (exp_line, split, t_void, false, info);
    exp_line = add_value (exp_line, split, t_void, true, info);
    exp_line = add_value (exp_line, split, t_void, false, info);
    exp_line = add_reconcile (exp_line, split, info);
    exp_line = add_reconcile_date (exp_line, split, info);
    exp_line = add_price (exp_line, split, t_void, info);

    return exp_line;
}


/*******************************************************
 * account_splits
 *
 * gather the splits / transactions for an account and
 * send them to a file
 *******************************************************/
static
void account_splits (CsvExportInfo *info, Account *acc, FILE *fh )
{
    bool is_trading_acct = acc && (xaccAccountGetType (acc) == ACCT_TYPE_TRADING);

    // Setup the query for normal transaction export
    if (info->export_type == XML_EXPORT_TRANS)
    {
        info->query = qof_query_create_for (GNC_ID_SPLIT);
        QofBook *book = gnc_get_current_book();
        qof_query_set_book (info->query, book);

        /* Sort by transaction date */
        GSList *p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
        p1 = g_slist_prepend (p1, SPLIT_TRANS);
        GSList *p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
        qof_query_set_sort_order (info->query, p1, p2, NULL);

        xaccQueryAddSingleAccountMatch (info->query, acc, QOF_QUERY_AND);
        xaccQueryAddDateMatchTT (info->query, true, info->csvd.start_time, true, info->csvd.end_time, QOF_QUERY_AND);
    }

    /* Run the query */
    GList *trans_list = NULL;
    for (GList *splits = qof_query_run (info->query); splits; splits = splits->next)
    {
        Split *split = splits->data;

        // Look for trans already exported in trans_list
        Transaction *trans = xaccSplitGetParent (split);
        if (g_list_find (trans_list, trans))
            continue;

        // Look for blank split
        Account *split_acc = xaccSplitGetAccount (split);
        if (!split_acc)
            continue;

        // Only export trading splits when exporting a trading account
        if (!is_trading_acct &&
            (xaccAccountGetType (split_acc) == ACCT_TYPE_TRADING))
            continue;

        if (info->simple_layout)
        {
            // Write line in simple layout, equivalent to a single line register view
            gchar *line = make_simple_trans_line (trans, split, info);
            info->failed = !write_line_to_file (fh, line);
            g_free (line);
            if (info->failed)
                break;

            continue;
        }

        // Write complex Transaction Line.
        gchar *line = make_complex_trans_line (trans, split, info);
        info->failed = !write_line_to_file (fh, line);
        g_free (line);
        if (info->failed)
            break;

        /* Loop through the list of splits for the Transaction */
        for (GList *node = xaccTransGetSplitList (trans); node; node = node->next)
        {
            Split *t_split = node->data;

            // base split is already written on the trans_line
            if (split == t_split)
                continue;

            // Only export trading splits if exporting a trading account
            Account *tsplit_acc = xaccSplitGetAccount (t_split);
            if (!is_trading_acct &&
                (xaccAccountGetType (tsplit_acc) == ACCT_TYPE_TRADING))
                continue;

            // Write complex Split Line.
            line = make_complex_trans_line (trans, t_split, info);
            info->failed = !write_line_to_file (fh, line);
            g_free (line);
            if (info->failed)
                break;
        }
        trans_list = g_list_prepend (trans_list, trans);
    }

    if (info->export_type == XML_EXPORT_TRANS)
        qof_query_destroy (info->query);
    g_list_free (trans_list);
}


/*******************************************************
 * csv_transactions_export
 *
 * write a list of transactions to a text file
 *******************************************************/
void csv_transactions_export (CsvExportInfo *info)
{
    ENTER("");
    DEBUG("File name is : %s", info->file_name);

    info->failed = false;

    /* Set up separators */
    if (info->use_quotes)
    {
        info->end_sep = "\"";
        info->mid_sep = g_strconcat ("\"", info->separator_str, "\"", NULL);
    }
    else
    {
        info->end_sep = "";
        info->mid_sep = g_strconcat (info->separator_str, NULL);
    }

    /* Open File for writing */
    FILE *fh = g_fopen (info->file_name, "w" );
    if (!fh)
    {
        info->failed = true;
        return;
    }

    gchar *header;
    bool num_action = qof_book_use_split_action_for_num_field (gnc_get_current_book());
    /* Header string */
    if (info->simple_layout)
    {
        header = g_strconcat (info->end_sep,
                        /* Translators: The following symbols will build the *
                        * header line of exported CSV files:                */
                                _("Date"), info->mid_sep, _("Account Name"),
                                info->mid_sep, (num_action ? _("Transaction Number") : _("Number")),
                                info->mid_sep, _("Description"), info->mid_sep, _("Full Category Path"),
                                info->mid_sep, _("Reconcile"),
                                info->mid_sep, _("Amount With Sym"), info->mid_sep, _("Amount Num."),
                                info->mid_sep, _("Value With Sym"), info->mid_sep, _("Value Num."),
                                info->mid_sep, _("Rate/Price"),
                                info->end_sep, EOLSTR, NULL);
    }
    else
    {
        header = g_strconcat (info->end_sep, _("Date"), info->mid_sep, _("Transaction ID"),
                                info->mid_sep, (num_action ? _("Transaction Number") : _("Number")),
                                info->mid_sep, _("Description"), info->mid_sep, _("Notes"),
                                info->mid_sep, _("Commodity/Currency"), info->mid_sep, _("Void Reason"),
                                info->mid_sep, (num_action ? _("Number/Action") : _("Action")), info->mid_sep, _("Memo"),
                                info->mid_sep, _("Full Account Name"), info->mid_sep, _("Account Name"),
                                info->mid_sep, _("Amount With Sym"), info->mid_sep, _("Amount Num."),
                                info->mid_sep, _("Value With Sym"), info->mid_sep, _("Value Num."),
                                info->mid_sep, _("Reconcile"), info->mid_sep, _("Reconcile Date"), info->mid_sep, _("Rate/Price"),
                                info->end_sep, EOLSTR, NULL);
    }
    DEBUG("Header String: %s", header);

    /* Write header line */
    info->failed = !write_line_to_file (fh, header);
    g_free (header);
    if (info->failed)
        return;

    /* Go through list of accounts */
    for (GList *ptr = info->csva.account_list; ptr; ptr = g_list_next(ptr))
    {
        Account *acc = ptr->data;
        DEBUG("Account being processed is : %s", xaccAccountGetName (acc));
        account_splits (info, acc, fh);
    }

    fclose (fh);
    LEAVE("");
}

