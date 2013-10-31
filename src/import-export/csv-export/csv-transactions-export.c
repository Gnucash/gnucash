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

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

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
    len = strlen( line );
    written = fwrite( line, 1, len, fh );

    if ( written != len )
        return FALSE;
    else
        return TRUE;
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

    q = qof_query_create_for(GNC_ID_SPLIT);
    book = gnc_get_current_book();
    qof_query_set_book (q, book);

    /* Set up separators */
    if (info->use_quotes)
    {
        end_sep = "\"";
        mid_sep = g_strconcat ( "\"", info->separator_str, "\"", NULL);
    }
    else
    {
        end_sep = "";
        mid_sep = g_strconcat ( info->separator_str, NULL);
    }

    /* Sort by transaction date */
    p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    p1 = g_slist_prepend (p1, SPLIT_TRANS);
    p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
    qof_query_set_sort_order (q, p1, p2, NULL);

    xaccQueryAddSingleAccountMatch (q, acc, QOF_QUERY_AND);
    xaccQueryAddDateMatchTT (q, TRUE, info->csvd.start_time, TRUE, info->csvd.end_time, QOF_QUERY_AND);

    /* Run the query */
    for (splits = qof_query_run(q); splits; splits = splits->next)
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

        split = splits->data;
        trans = xaccSplitGetParent(split);
        nSplits = xaccTransCountSplits(trans);
        s_list = xaccTransGetSplitList(trans);

        /* Date */
        date = qof_print_date ( xaccTransGetDate(trans));
        part1 = g_strconcat ( end_sep, date, mid_sep, NULL);
        g_free(date);
        /* Name */
        currentSel = xaccAccountGetName(acc);
        part2 = g_strconcat ( part1, currentSel, mid_sep, NULL);
        g_free(part1);
        /* Number */
        currentSel = gnc_get_num_action(trans, NULL);
        part1 = g_strconcat ( part2, currentSel, mid_sep, NULL);
        g_free(part2);
        /* Description */
        currentSel = xaccTransGetDescription(trans);
        part2 = g_strconcat ( part1, currentSel, mid_sep, NULL);
        g_free(part1);
        /* Notes */
        currentSel = xaccTransGetNotes(trans);
        if (currentSel == NULL)
            part1 = g_strconcat ( part2, mid_sep, NULL);
        else
            part1 = g_strconcat ( part2, currentSel, mid_sep, NULL);
        g_free(part2);
        /* Memo */
        currentSel = xaccSplitGetMemo(split);
        part2 = g_strconcat ( part1, currentSel, mid_sep, NULL);
        g_free(part1);
        /* Category */
        currentSel = xaccSplitGetCorrAccountName(split);
        part1 = g_strconcat ( part2, currentSel, mid_sep, "T", mid_sep, NULL);
        g_free(part2);
        /* Action */
        currentSel =  gnc_get_num_action(NULL, split);
        part2 = g_strconcat ( part1, currentSel, mid_sep, NULL);
        g_free(part1);
        /* Reconcile */
        switch (xaccSplitGetReconcile (split))
        {
        case NREC:
            currentSel = "N";
            break;
        case CREC:
            currentSel = "C";
            break;
        case YREC:
            currentSel = "Y";
            break;
        case FREC:
            currentSel = "F";
            break;
        case VREC:
            currentSel = "V";
            break;
        default:
            currentSel = "N";
        }
        part1 = g_strconcat ( part2, currentSel, mid_sep, NULL);
        g_free(part2);
        /* To with Symbol */
        split_amount = xaccPrintAmount(xaccSplitGetAmount(split), gnc_split_amount_print_info(split, TRUE));
        part2 = g_strconcat ( part1, split_amount, mid_sep, NULL);
        g_free(part1);

        /* From with Symbol */
        part1 = g_strconcat ( part2, "", mid_sep, NULL);
        g_free(part2);

        /* To Number Only */
        split_amount = xaccPrintAmount(xaccSplitGetAmount(split), gnc_split_amount_print_info(split, FALSE));
        part2 = g_strconcat ( part1, split_amount, mid_sep, NULL);
        g_free(part1);

        /* From Number Only */
        part1 = g_strconcat ( part2, "", mid_sep, "", mid_sep, "", end_sep, "\n", NULL);
        g_free(part2);

        /* Write to file */
        if (!write_line_to_file(fh, part1))
        {
            info->failed = TRUE;
            break;
        }
        g_free(part1);

        /* Loop through the list of splits for the Transcation */
        node = s_list;
        cnt = 0;
        while ( (cnt < nSplits) && (info->failed == FALSE))
        {
            t_split = node->data;

            /* Start of line */
            part1 = g_strconcat ( end_sep, mid_sep, mid_sep, mid_sep, mid_sep, mid_sep, NULL);

            /* Memo */
            currentSel = xaccSplitGetMemo(t_split);
            part2 = g_strconcat ( part1, currentSel, mid_sep, NULL);
            g_free(part1);

            /* Account */
            currentSel = xaccAccountGetName( xaccSplitGetAccount(t_split));
            part1 = g_strconcat ( part2, currentSel, mid_sep, "S", mid_sep, NULL);
            g_free(part2);

            /* Action */
            currentSel = gnc_get_num_action(NULL, t_split);
            part2 = g_strconcat ( part1, currentSel, mid_sep, NULL);
            g_free(part1);

            /* Reconcile */
            switch (xaccSplitGetReconcile (split))
            {
            case NREC:
                currentSel = "N";
                break;
            case CREC:
                currentSel = "C";
                break;
            case YREC:
                currentSel = "Y";
                break;
            case FREC:
                currentSel = "F";
                break;
            case VREC:
                currentSel = "V";
                break;
            default:
                currentSel = "N";
            }
            part1 = g_strconcat ( part2, currentSel, mid_sep, NULL);
            g_free(part2);

            /* From / To with Symbol */
            split_amount = xaccPrintAmount(xaccSplitGetAmount(t_split), gnc_split_amount_print_info(t_split, TRUE));
            if (xaccSplitGetAccount(t_split) == acc)
                part2 = g_strconcat ( part1,  split_amount, mid_sep, mid_sep, NULL);
            else
                part2 = g_strconcat ( part1, mid_sep, split_amount, mid_sep, NULL);
            g_free(part1);

            /* From / To Numbers only */
            split_amount = xaccPrintAmount(xaccSplitGetAmount(t_split), gnc_split_amount_print_info(t_split, FALSE));
            if (xaccSplitGetAccount(t_split) == acc)
                part1 = g_strconcat ( part2,  split_amount, mid_sep, mid_sep, NULL);
            else
                part1 = g_strconcat ( part2, mid_sep, split_amount, mid_sep, NULL);
            g_free(part2);

            /* From / To - Share Price / Conversion factor */
            split_amount = xaccPrintAmount(xaccSplitGetSharePrice(t_split), gnc_split_amount_print_info(t_split, FALSE));
            if (xaccSplitGetAccount(t_split) == acc)
                part2 = g_strconcat ( part1,  split_amount, mid_sep, end_sep, "\n", NULL);
            else
                part2 = g_strconcat ( part1, mid_sep, split_amount, end_sep, "\n", NULL);
            g_free(part1);

            if (!write_line_to_file(fh, part2))
                info->failed = TRUE;

            g_free(part2);
            cnt++;
            node = node->next;
        }
    }
    g_free(mid_sep);
    qof_query_destroy (q);
    g_list_free( splits );
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
    fh = g_fopen( info->file_name, "w" );
    if ( fh != NULL )
    {
        gchar *header;
        gchar *end_sep;
        gchar *mid_sep;
        int i;

        /* Set up separators */
        if (info->use_quotes)
        {
            end_sep = "\"";
            mid_sep = g_strconcat ( "\"", info->separator_str, "\"", NULL);
        }
        else
        {
            end_sep = "";
            mid_sep = g_strconcat ( info->separator_str, NULL);
        }

        /* Header string */
        header = g_strconcat ( end_sep, _("Date"), mid_sep, _("Account Name"), mid_sep,
                               (num_action ? _("Transaction Number") : _("Number")),
                               mid_sep, _("Description"), mid_sep, _("Notes"),
                               mid_sep, _("Memo"), mid_sep, _("Category"), mid_sep,
                               _("Type"), mid_sep,
                               (num_action ? _("Number/Action") : _("Action")),
                               mid_sep, _("Reconcile"), mid_sep,
                               _("To With Sym"), mid_sep, _("From With Sym"), mid_sep,
                               _("To Num."), mid_sep, _("From Num."), mid_sep,
                               _("To Rate/Price"), mid_sep, _("From Rate/Price"),
                               end_sep, "\n", NULL);
        DEBUG("Header String: %s", header);

        /* Write header line */
        if (!write_line_to_file(fh, header))
        {
            info->failed = TRUE;
            g_free(mid_sep);
            g_free(header);
            return;
        }
        g_free(mid_sep);
        g_free(header);

        /* Go through list of accounts */
        for (ptr = info->csva.account_list, i = 0; ptr; ptr = g_list_next(ptr), i++)
        {
            acc = ptr->data;
            DEBUG("Account being processed is : %s", xaccAccountGetName(acc));
            account_splits (info, acc, fh);
        }
    }
    else
        info->failed = TRUE;
    if (fh)
        fclose (fh);
    LEAVE("");
}





