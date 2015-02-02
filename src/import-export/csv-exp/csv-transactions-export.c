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


/*******************************************************************/

/*******************************************************
 * write_line_to_file
 *
 * write a text string to a file pointer, return TRUE if
 * successfull.
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


/*******************************************************
 * account_splits
 *
 * gather the splits / transactions for an account and
 * send them to a file
 *******************************************************/
static
void account_splits (CsvExportInfo *info, Account *acc, FILE *fh )
{
    Query   *q;
    GSList  *p1, *p2;
    GList   *splits;
    QofBook *book;

    gchar   *end_sep;
    gchar   *mid_sep;

    q = qof_query_create_for (GNC_ID_SPLIT);
    book = gnc_get_current_book();
    qof_query_set_book (q, book);

    /* Set up separators */
    if (info->use_quotes)
    {
        end_sep = "\"";
        mid_sep = g_strconcat ("\"", info->separator_str, "\"", NULL);
    }
    else
    {
        end_sep = "";
        mid_sep = g_strconcat (info->separator_str, NULL);
    }

    /* Sort by transaction date */
    p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    p1 = g_slist_prepend (p1, SPLIT_TRANS);
    p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
    qof_query_set_sort_order (q, p1, p2, NULL);

    xaccQueryAddSingleAccountMatch (q, acc, QOF_QUERY_AND);
    xaccQueryAddDateMatchTT (q, TRUE, info->csvd.start_time, TRUE, info->csvd.end_time, QOF_QUERY_AND);

    /* Run the query */
    for (splits = qof_query_run (q); splits; splits = splits->next)
    {
        Split       *split;
        Transaction *trans;
        SplitList   *s_list;
        GList       *node;
        Split       *t_split;
        int          nSplits;
        int          cnt;
        gchar       *part1;
        gchar       *part2;
        gchar       *date;
        const gchar *currentSel;
        const gchar *split_amount;
        gchar       *str_temp = NULL;
        gchar       *full_path = NULL;
        Timespec     ts = {0,0};
        char         type;
        static char  ss[2];

        split = splits->data;
        trans = xaccSplitGetParent (split);
        nSplits = xaccTransCountSplits (trans);
        s_list = xaccTransGetSplitList (trans);
        type = xaccTransGetTxnType (trans);

        // Look for trans already exported in trans_list
        if (g_list_find (info->trans_list, trans) != NULL)
            continue;

        /* Date */
        date = qof_print_date (xaccTransGetDate (trans));
        part1 = g_strconcat (end_sep, date, mid_sep, NULL);
        g_free (date);
        /* Transaction Type */
        if (type == TXN_TYPE_NONE)
            type = ' ';
        ss[0] = type;
        ss[1] = '\0';
        part2 = g_strconcat (part1, ss, mid_sep, NULL);
        g_free (part1);
        /* Second Date */
        if (type == TXN_TYPE_INVOICE)
        {
            xaccTransGetDateDueTS (trans, &ts);
            currentSel = gnc_print_date (ts);
            part1 = g_strconcat (part2, currentSel, mid_sep, NULL);
            g_free (part2);
        }
        else
        {
            part1 = g_strconcat (part2, mid_sep, NULL);
            g_free (part2);
        }
        /* Name */
        currentSel = xaccAccountGetName (acc);
        str_temp = csv_txn_test_field_string (info, currentSel);
        part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
        g_free (str_temp);
        g_free (part1);
        /* Number */
        currentSel = xaccTransGetNum (trans) ? xaccTransGetNum (trans) : "" ;
        str_temp = csv_txn_test_field_string (info, currentSel);
        part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
        g_free (str_temp);
        g_free (part2);
        /* Description */
        currentSel = xaccTransGetDescription (trans) ? xaccTransGetDescription (trans) : "" ;
        str_temp = csv_txn_test_field_string (info, currentSel);
        part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
        g_free (str_temp);
        g_free (part1);
        /* Notes */
        currentSel = xaccTransGetNotes (trans) ? xaccTransGetNotes (trans) : "" ;
        str_temp = csv_txn_test_field_string (info, currentSel);
        part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
        g_free (str_temp);
        g_free (part2);
        /* Memo */
        currentSel = xaccSplitGetMemo (split) ? xaccSplitGetMemo (split) : "" ;
        str_temp = csv_txn_test_field_string (info, currentSel);
        part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
        g_free (str_temp);
        g_free (part1);
        /* Full Category Path */
        full_path = xaccSplitGetCorrAccountFullName (split);
        str_temp = csv_txn_test_field_string (info, full_path);
        part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
        g_free (full_path);
        g_free (str_temp);
        g_free (part2);
        part2 = g_strconcat (part1, NULL);
        g_free (part1);
        /* Category */
        currentSel = xaccSplitGetCorrAccountName (split);
        str_temp = csv_txn_test_field_string (info, currentSel);
        part1 = g_strconcat (part2, str_temp, mid_sep, "T", mid_sep, "", mid_sep, NULL);
        g_free (str_temp);
        g_free (part2);

        part2 = g_strconcat (part1, NULL);
        g_free (part1);

        /* Reconcile and Amount with Symbol */
        currentSel = gnc_get_reconcile_str (xaccSplitGetReconcile (split));
        part1 = g_strconcat (part2, currentSel, mid_sep, "", mid_sep, NULL);
        g_free (part2);

        /* Commodity Mnemonic */
        currentSel = gnc_commodity_get_mnemonic (xaccTransGetCurrency (trans));
        str_temp = csv_txn_test_field_string (info, currentSel);
        part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
        g_free (str_temp);
        g_free (part1);

        /* Commodity Namespace */
        currentSel = gnc_commodity_get_namespace (xaccTransGetCurrency (trans));
        str_temp = csv_txn_test_field_string (info, currentSel);
        part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
        g_free (str_temp);
        g_free (part2);

        /* Amount Number Only and Rate/Price */
        part2 = g_strconcat (part1, "", mid_sep, "", end_sep, EOLSTR, NULL);
        g_free (part1);

        /* Write to file */
        if (!write_line_to_file (fh, part2))
        {
            info->failed = TRUE;
            break;
        }
        g_free (part2);

        /* Loop through the list of splits for the Transcation */
        node = s_list;
        cnt = 0;
        while ((cnt < nSplits) && (info->failed == FALSE))
        {
            gchar *fullname = NULL;
            const gchar *str_rec_date;
            gboolean t_void = xaccTransGetVoidStatus (trans);
            t_split = node->data;


            if (xaccSplitGetReconcile (t_split) == YREC)
            {
                xaccSplitGetDateReconciledTS (t_split, &ts);
                str_rec_date = gnc_print_date (ts);
            }
            else
                str_rec_date = "";

            /* Start of line */
            if (t_void)
            {
                currentSel = xaccTransGetVoidReason (trans) ? xaccTransGetVoidReason (trans) : "" ;
                str_temp = csv_txn_test_field_string (info, currentSel);
                part1 = g_strconcat (end_sep, mid_sep, mid_sep, str_rec_date, mid_sep, mid_sep, mid_sep, mid_sep, str_temp, mid_sep, NULL);
                g_free (str_temp);
            }
            else
                part1 = g_strconcat (end_sep, mid_sep, mid_sep, str_rec_date, mid_sep, mid_sep, mid_sep, mid_sep, mid_sep, NULL);

            /* Memo */
            currentSel = xaccSplitGetMemo (t_split) ? xaccSplitGetMemo (t_split) : "" ;
            str_temp = csv_txn_test_field_string (info, currentSel);
            part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part1);

            /* Full Account */
            fullname = gnc_account_get_full_name (xaccSplitGetAccount (t_split));
            str_temp = csv_txn_test_field_string (info, fullname);
            part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (fullname);
            g_free (part2);

            part2 = g_strconcat (part1, NULL);
            g_free (part1);

            /* Account */
            currentSel = xaccAccountGetName (xaccSplitGetAccount (t_split));
            str_temp = csv_txn_test_field_string (info, currentSel);
            part1 = g_strconcat (part2, str_temp, mid_sep, "S", mid_sep, NULL);
            g_free (str_temp);
            g_free (part2);

            /* Action */
            currentSel = xaccSplitGetAction (t_split);
            str_temp = csv_txn_test_field_string (info, currentSel);
            part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part1);

            /* Reconcile */
            currentSel = gnc_get_reconcile_str (xaccSplitGetReconcile (t_split));
            part1 = g_strconcat (part2, currentSel, mid_sep, NULL);
            g_free (part2);

            /* Amount with Symbol */
            split_amount = xaccPrintAmount (xaccSplitGetAmount (t_split), gnc_split_amount_print_info (t_split, TRUE));
            str_temp = csv_txn_test_field_string (info, split_amount);
            part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part1);

            /* Commodity Mnemonic */
            currentSel = gnc_commodity_get_mnemonic (xaccAccountGetCommodity (xaccSplitGetAccount(t_split)));
            str_temp = csv_txn_test_field_string (info, currentSel);
            part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part2);

            /* Commodity Namespace */
            currentSel = gnc_commodity_get_namespace (xaccAccountGetCommodity (xaccSplitGetAccount(t_split)));
            str_temp = csv_txn_test_field_string (info, currentSel);
            part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part1);

            /* Amount Numbers only */
            if (t_void)
                split_amount = xaccPrintAmount (xaccSplitVoidFormerAmount (t_split), gnc_split_amount_print_info (t_split, FALSE));
            else
                split_amount = xaccPrintAmount (xaccSplitGetAmount (t_split), gnc_split_amount_print_info (t_split, FALSE));
            str_temp = csv_txn_test_field_string (info, split_amount);
            part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part2);

            /* Share Price / Conversion factor */
            if (t_void)
            {
                gnc_numeric cf = gnc_numeric_div (xaccSplitVoidFormerValue (t_split), xaccSplitVoidFormerAmount (t_split), GNC_DENOM_AUTO,
                                                   GNC_HOW_DENOM_SIGFIGS(6) | GNC_HOW_RND_ROUND_HALF_UP);
                split_amount = xaccPrintAmount (cf, gnc_split_amount_print_info (t_split, FALSE));
            }
            else
                split_amount = xaccPrintAmount (xaccSplitGetSharePrice (t_split), gnc_split_amount_print_info (t_split, FALSE));
            str_temp = csv_txn_test_field_string (info, split_amount);
            part2 = g_strconcat (part1, str_temp, end_sep, EOLSTR, NULL);
            g_free (str_temp);
            g_free (part1);

            if (!write_line_to_file (fh, part2))
                info->failed = TRUE;

            g_free (part2);
            cnt++;
            node = node->next;
        }
        info->trans_list = g_list_prepend (info->trans_list, trans); // add trans to trans_list
    }
    g_free (mid_sep);
    qof_query_destroy (q);
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
    gboolean num_action = qof_book_use_split_action_for_num_field(gnc_get_current_book());

    ENTER("");
    DEBUG("File name is : %s", info->file_name);

    info->failed = FALSE;

    /* Open File for writing */
    fh = g_fopen (info->file_name, "w" );
    if (fh != NULL)
    {
        gchar *header;
        gchar *end_sep;
        gchar *mid_sep;
        int i;

        /* Set up separators */
        if (info->use_quotes)
        {
            end_sep = "\"";
            mid_sep = g_strconcat ("\"", info->separator_str, "\"", NULL);
        }
        else
        {
            end_sep = "";
            mid_sep = g_strconcat (info->separator_str, NULL);
        }

        /* Header string */
        header = g_strconcat (end_sep, _("Date"), mid_sep, _("Transaction Type"), mid_sep, _("Second Date"),
                              mid_sep, _("Account Name"), mid_sep, (num_action ? _("Transaction Number") : _("Number")),
                              mid_sep, _("Description"), mid_sep, _("Notes"), mid_sep, _("Memo"),
                              mid_sep, _("Full Category Path"), mid_sep, _("Category"), mid_sep, _("Row Type"),
                              mid_sep, (num_action ? _("Number/Action") : _("Action")),
                              mid_sep, _("Reconcile"), mid_sep, _("Amount With Sym"),
                              mid_sep, _("Commodity Mnemonic"), mid_sep, _("Commodity Namespace"),
                              mid_sep, _("Amount Num."), mid_sep, _("Rate/Price"),
                              end_sep, EOLSTR, NULL);
        DEBUG("Header String: %s", header);

        /* Write header line */
        if (!write_line_to_file (fh, header))
        {
            info->failed = TRUE;
            g_free (mid_sep);
            g_free (header);
            return;
        }
        g_free (mid_sep);
        g_free (header);

        /* Go through list of accounts */
        for (ptr = info->csva.account_list, i = 0; ptr; ptr = g_list_next(ptr), i++)
        {
            acc = ptr->data;
            DEBUG("Account being processed is : %s", xaccAccountGetName (acc));
            account_splits (info, acc, fh);
        }
        g_list_free (info->trans_list); // free trans_list
    }
    else
        info->failed = TRUE;
    if (fh)
        fclose (fh);
    LEAVE("");
}

