/*******************************************************************\
 * csv-transactions-export.c -- Export Transactions to a file       *
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


enum GncCsvLineType {TRANS_SIMPLE,
                     TRANS_COMPLEX,
                     SPLIT_LINE};

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
       if so quote field if not allready quoted */
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

// Transaction line starts with Date
static gchar*
begin_trans_string (Transaction *trans, CsvExportInfo *info)
{
    gchar *date = qof_print_date (xaccTransGetDate (trans));
    gchar *result = g_strconcat (info->end_sep, date, info->mid_sep, NULL);
    g_free (date);
    return result;
}


// Split line start
static gchar*
begin_split_string (Transaction *trans, Split *split, gboolean t_void, CsvExportInfo *info)
{
    const gchar *str_rec_date;
    const gchar *start;
    gchar       *conv;
    gchar       *result;
    Timespec     ts = {0,0};

    if (xaccSplitGetReconcile (split) == YREC)
    {
        xaccSplitGetDateReconciledTS (split, &ts);
        str_rec_date = gnc_print_date (ts);
    }
    else
        str_rec_date = "";

    if (t_void)
    {
        start = xaccTransGetVoidReason (trans) ? xaccTransGetVoidReason (trans) : "" ;
        conv = csv_txn_test_field_string (info, start);
        result = g_strconcat (info->end_sep, info->mid_sep, info->mid_sep, str_rec_date,
                              info->mid_sep, info->mid_sep, info->mid_sep, info->mid_sep, conv, info->mid_sep, NULL);
        g_free (conv);
    }
    else
         result = g_strconcat (info->end_sep, info->mid_sep, info->mid_sep, str_rec_date,
                               info->mid_sep, info->mid_sep, info->mid_sep, info->mid_sep, info->mid_sep, NULL);

    return result;
}


// Transaction Type
static gchar*
add_type (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    gchar       *result;
    char         type;
    static char  ss[2];

    type = xaccTransGetTxnType (trans);

    if (type == TXN_TYPE_NONE)
        type = ' ';
    ss[0] = type;
    ss[1] = '\0';
    result = g_strconcat (so_far, ss, info->mid_sep, NULL);
    g_free (so_far);
    return result;
}

// Second Date
static gchar*
add_second_date (gchar *so_far, Transaction *trans, CsvExportInfo *info)
{
    gchar       *result;
    const gchar *second_date;
    char         type;
    Timespec     ts = {0,0};

    type = xaccTransGetTxnType (trans);

    if (type == TXN_TYPE_INVOICE)
    {
        xaccTransGetDateDueTS (trans, &ts);
        second_date = gnc_print_date (ts);
        result = g_strconcat (so_far, second_date, info->mid_sep, NULL);
    }
    else
        result = g_strconcat (so_far, info->mid_sep, NULL);

    g_free (so_far);
    return result;
}

// Account Name short or Long
static gchar*
add_account_name (gchar *so_far, Account *acc, Split *split, gboolean full, CsvExportInfo *info)
{
    gchar       *name = NULL;
    gchar       *conv;
    gchar       *result;
    Account     *account = NULL;

    if (split == NULL)
    {
        if (acc == NULL)
            name = g_strdup (" ");
        else
            account = acc;
    }
    else
        account = xaccSplitGetAccount (split);

    if (account != NULL)
    {
        if (full)
            name = gnc_account_get_full_name (account);
        else
            name = g_strdup (xaccAccountGetName (account));
    }
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

// Line Type
static gchar*
add_line_type (gchar *so_far, gint line_type, CsvExportInfo *info)
{
    gchar *result;

    if (line_type == SPLIT_LINE)
        result = g_strconcat (so_far, "S", info->mid_sep, NULL);
    else
        result = g_strconcat (so_far, "T", info->mid_sep, NULL);

    g_free (so_far);
    return result;
}

// Action
static gchar*
add_action (gchar *so_far, Split *split, gint line_type, CsvExportInfo *info)
{
    const gchar *action;
    gchar       *conv;
    gchar       *result;

    if ((line_type == TRANS_COMPLEX)||(line_type == TRANS_SIMPLE))
        result = g_strconcat (so_far, "", info->mid_sep, NULL);
    else
    {
        action = xaccSplitGetAction (split);
        conv = csv_txn_test_field_string (info, action);
        result = g_strconcat (so_far, conv, info->mid_sep, NULL);
        g_free (conv);
    }
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

// Commodity Mnemonic
static gchar*
add_comm_mnemonic (gchar *so_far, Transaction *trans, Split *split, CsvExportInfo *info)
{
    const gchar *comm_m;
    gchar       *conv;
    gchar       *result;

    if (split == NULL)
        comm_m = gnc_commodity_get_mnemonic (xaccTransGetCurrency (trans));
    else
        comm_m = gnc_commodity_get_mnemonic (xaccAccountGetCommodity (xaccSplitGetAccount(split)));

    conv = csv_txn_test_field_string (info, comm_m);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Commodity Namespace
static gchar*
add_comm_namespace (gchar *so_far, Transaction *trans, Split *split, CsvExportInfo *info)
{
    const gchar *comm_n;
    gchar       *conv;
    gchar       *result;

    if (split == NULL)
        comm_n = gnc_commodity_get_namespace (xaccTransGetCurrency (trans));
    else
        comm_n = gnc_commodity_get_namespace (xaccAccountGetCommodity (xaccSplitGetAccount(split)));

    conv = csv_txn_test_field_string (info, comm_n);
    result = g_strconcat (so_far, conv, info->mid_sep, NULL);
    g_free (conv);
    g_free (so_far);
    return result;
}

// Amount with Symbol or not
static gchar*
add_amount (gchar *so_far, Split *split, gboolean t_void, gboolean symbol, gint line_type, CsvExportInfo *info)
{
    const gchar *amt;
    gchar       *conv;
    gchar       *result;

    if (line_type == TRANS_COMPLEX)
        result = g_strconcat (so_far, "", info->mid_sep, NULL);
    else
    {
        if (symbol)
        {
            if (t_void)
                amt = xaccPrintAmount (gnc_numeric_zero(), gnc_split_amount_print_info (split, TRUE));
            else
                amt = xaccPrintAmount (xaccSplitGetAmount (split), gnc_split_amount_print_info (split, TRUE));
        }
        else
        {
            if (t_void)
                amt = xaccPrintAmount (xaccSplitVoidFormerAmount (split), gnc_split_amount_print_info (split, FALSE));
            else
                amt = xaccPrintAmount (xaccSplitGetAmount (split), gnc_split_amount_print_info (split, FALSE));
        }
        conv = csv_txn_test_field_string (info, amt);
        result = g_strconcat (so_far, conv, info->mid_sep, NULL);
        g_free (conv);
    }
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

// Transaction End of Line
static gchar*
add_trans_eol (gchar *so_far, CsvExportInfo *info)
{
    gchar *result = g_strconcat (so_far, "", info->mid_sep, "", info->end_sep, EOLSTR, NULL);

    g_free (so_far);
    return result;
}

/******************************************************************************/

static gchar*
make_simple_trans_line (Account *acc, Transaction *trans, Split *split, CsvExportInfo *info)
{
    gboolean t_void = xaccTransGetVoidStatus (trans);

    gchar *result = begin_trans_string (trans, info);
    result = add_account_name (result, acc, NULL, TRUE, info);
    result = add_number (result, trans, info);
    result = add_description (result, trans, info);
    result = add_category (result, split, TRUE, info);
    result = add_reconcile (result, split, info);
    result = add_amount (result, split, t_void, TRUE, TRANS_SIMPLE, info);
    result = add_amount (result, split, t_void, FALSE, TRANS_SIMPLE, info);
    result = add_rate (result, split, t_void, info);
    return result;
}

static gchar*
make_complex_trans_line (Account *acc, Transaction *trans, Split *split, CsvExportInfo *info)
{
    gboolean t_void = xaccTransGetVoidStatus (trans);

    gchar *result = begin_trans_string (trans, info);
    result = add_type (result, trans, info);
    result = add_second_date (result, trans, info);
    result = add_account_name (result, acc, NULL, FALSE, info);
    result = add_number (result, trans, info);
    result = add_description (result, trans, info);
    result = add_notes (result, trans, info);
    result = add_memo (result, split, info);
    result = add_category (result, split, TRUE, info);
    result = add_category (result, split, FALSE, info);
    result = add_line_type (result, TRANS_COMPLEX, info);
    result = add_action (result,split, TRANS_COMPLEX, info);
    result = add_reconcile (result, split, info);
    result = add_amount (result, split, t_void, TRUE, TRANS_COMPLEX, info);
    result = add_comm_mnemonic (result, trans, NULL, info);
    result = add_comm_namespace (result, trans, NULL, info);
    result = add_trans_eol (result, info);
    return result;
}

static gchar*
make_complex_split_line (Transaction *trans, Split *split, CsvExportInfo *info)
{
    gboolean t_void = xaccTransGetVoidStatus (trans);

    gchar *result = begin_split_string (trans, split, t_void, info);
    result = add_memo (result, split, info);
    result = add_account_name (result, NULL, split, TRUE, info);
    result = add_account_name (result, NULL, split, FALSE, info);
    result = add_line_type (result, SPLIT_LINE, info);
    result = add_action (result,split, SPLIT_LINE, info);
    result = add_reconcile (result, split, info);
    result = add_amount (result, split, t_void, TRUE, SPLIT_LINE, info);
    result = add_comm_mnemonic (result, trans, split, info);
    result = add_comm_namespace (result, trans, split, info);
    result = add_amount (result, split, t_void, FALSE, SPLIT_LINE, info);
    result = add_price (result, split, t_void, info);
    return result;
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

            // Complex Split Line.
            line = make_complex_split_line (trans, t_split, info);

            if (!write_line_to_file (fh, line))
                info->failed = TRUE;

            g_free (line);

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
            header = g_strconcat (info->end_sep, _("Date"), info->mid_sep, _("Account Name"),
                                  info->mid_sep, (num_action ? _("Transaction Number") : _("Number")),
                                  info->mid_sep, _("Description"), info->mid_sep, _("Full Category Path"),
                                  info->mid_sep, _("Reconcile"), info->mid_sep, _("Amount With Sym"),
                                  info->mid_sep, _("Amount Num."), info->mid_sep, _("Rate/Price"),
                                  info->end_sep, EOLSTR, NULL);
        }
        else
        {
            header = g_strconcat (info->end_sep, _("Date"), info->mid_sep, _("Transaction Type"), info->mid_sep, _("Second Date"),
                                  info->mid_sep, _("Account Name"), info->mid_sep, (num_action ? _("Transaction Number") : _("Number")),
                                  info->mid_sep, _("Description"), info->mid_sep, _("Notes"), info->mid_sep, _("Memo"),
                                  info->mid_sep, _("Full Category Path"), info->mid_sep, _("Category"), info->mid_sep, _("Row Type"),
                                  info->mid_sep, (num_action ? _("Number/Action") : _("Action")),
                                  info->mid_sep, _("Reconcile"), info->mid_sep, _("Amount With Sym"),
                                  info->mid_sep, _("Commodity Mnemonic"), info->mid_sep, _("Commodity Namespace"),
                                  info->mid_sep, _("Amount Num."), info->mid_sep, _("Rate/Price"),
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

