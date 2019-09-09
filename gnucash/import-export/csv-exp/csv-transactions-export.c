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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

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
 * write a text string to a file pointer, return TRUE if
 * successful.
 *******************************************************/
static
gboolean write_line_to_file (FILE *fh, char * line)
{
    int len, written;
    DEBUG("Account String: %s", line);

    /* Write account line */
    len = strlen (line);
    written = fwrite (line, 1, len, fh);

    if (written != len)
        return FALSE;
    else
        return TRUE;
}


/*******************************************************
 * csv_txn_test_field_string
 *
 * Test the field string for ," and new lines
 *******************************************************/
static
gchar *csv_txn_test_field_string (CsvExportInfo *info, const gchar *string_in)
{
    gboolean need_quote = FALSE;
    gchar **parts;
    gchar *string_parts;
    gchar *string_out;

    /* Check for " and then "" them */
    parts = g_strsplit (string_in, "\"", -1);
    string_parts = g_strjoinv ("\"\"", parts);
    g_strfreev (parts);

    /* Check for separator string and \n and " in field,
       if so quote field if not already quoted */
    if (g_strrstr (string_parts, info->separator_str) != NULL)
        need_quote = TRUE;
    if (g_strrstr (string_parts, "\n") != NULL)
        need_quote = TRUE;
    if (g_strrstr (string_parts, "\"") != NULL)
        need_quote = TRUE;

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
    gchar *result;
    gchar *guid;

    guid = guid_to_string (xaccTransGetGUID (trans));
    result = g_strconcat (so_far, guid, info->mid_sep, NULL);
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
add_account_name (gchar *so_far, Split *split, gboolean full, CsvExportInfo *info)
{
    gchar       *name = NULL;
    gchar       *conv;
    gchar       *result;

    Account     *account = xaccSplitGetAccount (split);
    if (full)
        name = gnc_account_get_full_name (account);
    else
        name = g_strdup (xaccAccountGetName (account));
    conv = csv_txn_test_field_string (info, name);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (name);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Number
static gchar*
add_number (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *num;
    gchar       *conv;
    gchar       *result;

    num = xaccTransGetNum (trans) ? xaccTransGetNum (trans) : "" ;
    conv = csv_txn_test_field_string (info, num);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Description
static gchar*
add_description (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *desc;
    gchar       *conv;
    gchar       *result;

    desc = xaccTransGetDescription (trans) ? xaccTransGetDescription (trans) : "" ;
    conv = csv_txn_test_field_string (info, desc);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Notes
static gchar*
add_notes (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *notes;
    gchar       *conv;
    gchar       *result;

    notes = xaccTransGetNotes (trans) ? xaccTransGetNotes (trans) : "" ;
    conv = csv_txn_test_field_string (info, notes);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Void reason
static gchar*
add_void_reason (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    gchar       *result;

    if (xaccTransGetVoidStatus (trans))
    {
        const gchar *void_reason = xaccTransGetVoidReason (trans);
        gchar *conv = csv_txn_test_field_string (info, void_reason);
        result = g_strconcat (so_far, conv, info->mid_sep, NULL);
        g_free (conv);
    }
    else
        result = g_strconcat (so_far, info->mid_sep, NULL);

    g_free (so_far);
    return result;
}

// Memo
static gchar*
add_memo (gchar *so_far, Split *split, CsvExportInfo *info)
{
    const gchar *memo;
    gchar       *conv;
    gchar       *result;

    memo = xaccSplitGetMemo (split) ? xaccSplitGetMemo (split) : "" ;
    conv = csv_txn_test_field_string (info, memo);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Full Category Path or Not
static gchar*
add_category (gchar *so_far, Split *split, gboolean full, CsvExportInfo *info)
{
    gchar       *cat;
    gchar       *conv;
    gchar       *result;

    if (full)
        cat = xaccSplitGetCorrAccountFullName (split);
    else
        cat = g_strdup(xaccSplitGetCorrAccountName (split));

    conv = csv_txn_test_field_string (info, cat);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
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
    const gchar *recon;
    gchar       *conv;
    gchar       *result;

    recon = gnc_get_reconcile_str (xaccSplitGetReconcile (split));
    conv = csv_txn_test_field_string (info, recon);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Transaction commodity
static gchar*
add_commodity (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    const gchar *comm_m;
    gchar       *conv;
    gchar       *result;

    comm_m = gnc_commodity_get_unique_name (xaccTransGetCurrency (trans));

    conv = csv_txn_test_field_string (info, comm_m);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Amount with Symbol or not
static gchar*
add_amount (gchar *so_far, Split *split, gboolean t_void, gboolean symbol, CsvExportInfo *info)
{
    const gchar *amt;
    gchar       *conv;
    gchar       *result;

    if (t_void)
        amt = xaccPrintAmount (xaccSplitVoidFormerAmount (split), gnc_split_amount_print_info (split, symbol));
    else
        amt = xaccPrintAmount (xaccSplitGetAmount (split), gnc_split_amount_print_info (split, symbol));
    conv = csv_txn_test_field_string (info, amt);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Share Price / Conversion factor
static gchar*
add_rate (gchar *so_far, Split *split, gboolean t_void, CsvExportInfo *info)
{
    const gchar *amt;
    gchar       *conv;
    gchar       *result;

    if (t_void)
        amt = xaccPrintAmount (gnc_numeric_zero(), gnc_split_amount_print_info (split, FALSE));
    else
        amt = xaccPrintAmount (xaccSplitGetSharePrice (split), gnc_split_amount_print_info (split, FALSE));

    conv = csv_txn_test_field_string (info, amt);
    result = g_strconcat (so_far, conv, info->end_sep, EOLSTR, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Share Price / Conversion factor
static gchar*
add_price (gchar *so_far, Split *split, gboolean t_void, CsvExportInfo *info)
{
    const gchar *string_amount;
    gchar       *conv;
    gchar       *result;

    if (t_void)
    {
        gnc_numeric cf = gnc_numeric_div (xaccSplitVoidFormerValue (split), xaccSplitVoidFormerAmount (split), GNC_DENOM_AUTO,
                                                   GNC_HOW_DENOM_SIGFIGS(6) | GNC_HOW_RND_ROUND_HALF_UP);
        string_amount = xaccPrintAmount (cf, gnc_split_amount_print_info (split, FALSE));
    }
    else
        string_amount = xaccPrintAmount (xaccSplitGetSharePrice (split), gnc_split_amount_print_info (split, FALSE));

    conv = csv_txn_test_field_string (info, string_amount);
    result = g_strconcat (so_far, conv, info->end_sep, EOLSTR, NULL);
    g_free (conv);
    return result;
}

/******************************************************************************/

static gchar*
make_simple_trans_line (Account *acc, Transaction *trans, Split *split, CsvExportInfo *info)
{
    gboolean t_void = xaccTransGetVoidStatus (trans);

    gchar *exp_line = g_strdup("");
    exp_line = add_date (exp_line, trans, info);
    exp_line = add_account_name (exp_line, split, TRUE, info);
    exp_line = add_number (exp_line, trans, info);
    exp_line = add_description (exp_line, trans, info);
    exp_line = add_category (exp_line, split, TRUE, info);
    exp_line = add_reconcile (exp_line, split, info);
    exp_line = add_amount (exp_line, split, t_void, TRUE, info);
    exp_line = add_amount (exp_line, split, t_void, FALSE, info);
    exp_line = add_rate (exp_line, split, t_void, info);
    return exp_line;
}

static gchar*
make_split_part (gchar* exp_line, Split *split, gboolean t_void, CsvExportInfo *info)
{
    exp_line = add_action (exp_line, split, info);
    exp_line = add_memo (exp_line, split, info);
    exp_line = add_account_name (exp_line, split, TRUE, info);
    exp_line = add_account_name (exp_line, split, FALSE, info);
    exp_line = add_amount (exp_line, split, t_void, TRUE, info);
    exp_line = add_amount (exp_line, split, t_void, FALSE, info);
    exp_line = add_reconcile (exp_line, split, info);
    exp_line = add_reconcile_date (exp_line, split, info);
    exp_line = add_price (exp_line, split, t_void, info);
    return exp_line;
}

static gchar*
make_complex_trans_line (Account *acc, Transaction *trans, Split *split, CsvExportInfo *info)
{
    gchar *exp_line = g_strdup("");
    exp_line = add_date (exp_line, trans, info);
    exp_line = add_guid (exp_line, trans, info);
    exp_line = add_number (exp_line, trans, info);
    exp_line = add_description (exp_line, trans, info);
    exp_line = add_notes (exp_line, trans, info);
    exp_line = add_commodity (exp_line, trans, info);
    exp_line = add_void_reason (exp_line, trans, info);
    return make_split_part (exp_line, split, xaccTransGetVoidStatus (trans), info);
}

static gchar*
make_complex_split_line (Transaction *trans, Split *split, CsvExportInfo *info)
{
    /* Pure split lines don't have any transaction information,
     * so start with empty fields for all transaction columns.
     */
    gchar *result = g_strconcat (info->end_sep, info->mid_sep, info->mid_sep, info->mid_sep,
            info->mid_sep, info->mid_sep, info->mid_sep, info->mid_sep, NULL);
    return make_split_part (result, split, xaccTransGetVoidStatus (trans), info);
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
    GSList  *p1, *p2;
    GList   *splits;
    QofBook *book;

    // Setup the query for normal transaction export
    if (info->export_type == XML_EXPORT_TRANS)
    {
        info->query = qof_query_create_for (GNC_ID_SPLIT);
        book = gnc_get_current_book();
        qof_query_set_book (info->query, book);

        /* Sort by transaction date */
        p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
        p1 = g_slist_prepend (p1, SPLIT_TRANS);
        p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
        qof_query_set_sort_order (info->query, p1, p2, NULL);

        xaccQueryAddSingleAccountMatch (info->query, acc, QOF_QUERY_AND);
        xaccQueryAddDateMatchTT (info->query, TRUE, info->csvd.start_time, TRUE, info->csvd.end_time, QOF_QUERY_AND);
    }

    /* Run the query */
    for (splits = qof_query_run (info->query); splits; splits = splits->next)
    {
        Split       *split;
        Transaction *trans;
        SplitList   *s_list;
        GList       *node;
        Split       *t_split;
        int          nSplits;
        int          cnt;
        gchar       *line;

        split = splits->data;
        trans = xaccSplitGetParent (split);
        nSplits = xaccTransCountSplits (trans);
        s_list = xaccTransGetSplitList (trans);

        // Look for trans already exported in trans_list
        if (g_list_find (info->trans_list, trans) != NULL)
            continue;

        // Look for blank split
        if (xaccSplitGetAccount (split) == NULL)
            continue;

        // This will be a simple layout equivalent to a single line register view.
        if (info->simple_layout)
        {
            line = make_simple_trans_line (acc, trans, split, info);

            /* Write to file */
            if (!write_line_to_file (fh, line))
            {
                info->failed = TRUE;
                break;
            }
            g_free (line);
            continue;
        }

        // Complex Transaction Line.
        line = make_complex_trans_line (acc, trans, split, info);

        /* Write to file */
        if (!write_line_to_file (fh, line))
        {
            info->failed = TRUE;
            break;
        }
        g_free (line);

        /* Loop through the list of splits for the Transaction */
        node = s_list;
        cnt = 0;
        while ((cnt < nSplits) && (info->failed == FALSE))
        {
            t_split = node->data;

            // base split is already written on the trans_line
            if (split != t_split)
            {
            // Complex Split Line.
                line = make_complex_split_line (trans, t_split, info);

                if (!write_line_to_file (fh, line))
                    info->failed = TRUE;

                g_free (line);
            }

            cnt++;
            node = node->next;
        }
        info->trans_list = g_list_prepend (info->trans_list, trans); // add trans to trans_list
    }
    if (info->export_type == XML_EXPORT_TRANS)
        qof_query_destroy (info->query);
    g_list_free (splits);
}


/*******************************************************
 * csv_transactions_export
 *
 * write a list of transactions to a text file
 *******************************************************/
void csv_transactions_export (CsvExportInfo *info)
{
    FILE    *fh;
    Account *acc;
    GList   *ptr;
    gboolean num_action = qof_book_use_split_action_for_num_field (gnc_get_current_book());

    ENTER("");
    DEBUG("File name is : %s", info->file_name);

    info->failed = FALSE;

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
    fh = g_fopen (info->file_name, "w" );
    if (fh != NULL)
    {
        gchar *header;
        int i;

        /* Header string */
        if (info->simple_layout)
        {
            header = g_strconcat (info->end_sep,
                         /* Translators: The following symbols will build the *
                          * header line of exported CSV files:                */
                                  _("Date"), info->mid_sep, _("Account Name"),
                                  info->mid_sep, (num_action ? _("Transaction Number") : _("Number")),
                                  info->mid_sep, _("Description"), info->mid_sep, _("Full Category Path"),
                                  info->mid_sep, _("Reconcile"), info->mid_sep, _("Amount With Sym"),
                                  info->mid_sep, _("Amount Num."), info->mid_sep, _("Rate/Price"),
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
                                  info->mid_sep, _("Reconcile"), info->mid_sep, _("Reconcile Date"), info->mid_sep, _("Rate/Price"),
                                  info->end_sep, EOLSTR, NULL);
        }
        DEBUG("Header String: %s", header);

        /* Write header line */
        if (!write_line_to_file (fh, header))
        {
            info->failed = TRUE;
            g_free (header);
            return;
        }
        g_free (header);

        if (info->export_type == XML_EXPORT_TRANS)
        {
            /* Go through list of accounts */
            for (ptr = info->csva.account_list, i = 0; ptr; ptr = g_list_next(ptr), i++)
            {
                acc = ptr->data;
                DEBUG("Account being processed is : %s", xaccAccountGetName (acc));
                account_splits (info, acc, fh);
            }
        }
        else
            account_splits (info, info->account, fh);

        g_list_free (info->trans_list); // free trans_list
    }
    else
        info->failed = TRUE;
    if (fh)
        fclose (fh);
    LEAVE("");
}

