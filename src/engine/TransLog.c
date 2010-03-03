/********************************************************************\
 * TransLog.c -- the transaction logger                             *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <errno.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <string.h>

#include "Account.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "qof.h"

/*
 * Some design philosphy that I think would be good to keep in mind:
 * (0) Simplicity and foolproofness are the over-riding design points.
 *     This is supposed to be a fail-safe safety net.   We don't want
 *     our safety net to fail because of some whiz-bang shenanigans.
 *
 * (1) Try to keep the code simple.  Want to make it simple and obvious
 *     that we are recording everything that we need to record.
 *
 * (2) Keep the printed format human readable, for the same reasons.
 * (2.a) Keep the format, simple, flat, more or less unstructured,
 *       record oriented.  This will help parsing by perl scripts.
 *       No, using a perl script to analyze a file that's supposed to
 *       be human readable is not a contradication in terms -- that's
 *       exactly the point.
 * (2.b) Use tabs as a human friendly field separator; its also a
 *       character that does not (should not) appear naturally anywhere
 *       in the data, as it serves no formatting purpose in the current
 *       GUI design.  (hack alert -- this is not currently tested for
 *       or enforced, so this is a very unsafe assumption. Maybe
 *       urlencoding should be used.)
 * (2.c) Don't print redundant information in a single record. This
 *       would just confuse any potential user of this file.
 * (2.d) Saving space, being compact is not a priority, I don't think.
 *
 * (3) There are no compatibility requirements from release to release.
 *     Sounds OK to me to change the format of the output when needed.
 *
 * (-) print transaction start and end delimiters
 * (-) print a unique transaction id as a handy label for anyone
 *     who actually examines these logs.
 *     The C address pointer to the transaction struct should be fine,
 *     as it is simple and unique until the transaction is deleted ...
 *     and we log deletions, so that's OK.  Just note that the id
 *     for a deleted transaction might be recycled.
 * (-) print the current timestamp, so that if it is known that a bug
 *     occurred at a certain time, it can be located.
 * (-) hack alert -- something better than just the account name
 *     is needed for identifying the account.
 */
/* ------------------------------------------------------------------ */


static int gen_logs = 1;
static FILE * trans_log = NULL; /**< current log file handle */
static char * trans_log_name = NULL; /**< current log file name */
static char * log_base_name = NULL;

/********************************************************************\
\********************************************************************/

void xaccLogDisable (void)
{
    gen_logs = 0;
}
void xaccLogEnable  (void)
{
    gen_logs = 1;
}

/********************************************************************\
\********************************************************************/

void
xaccReopenLog (void)
{
    if (trans_log)
    {
        xaccCloseLog();
        xaccOpenLog();
    }
}


void
xaccLogSetBaseName (const char *basepath)
{
    if (!basepath) return;

    g_free (log_base_name);
    log_base_name = g_strdup (basepath);

    if (trans_log)
    {
        xaccCloseLog();
        xaccOpenLog();
    }
}


/*
 * See if the provided file name is that of the current log file.
 * Since the filename is generated with a time-stamp we can ignore the
 * directory path and avoid problems with worrying about any ".."
 * components in the path.
 */
gboolean
xaccFileIsCurrentLog (const gchar *name)
{
    gchar *base;
    gint result;

    if (!name || !trans_log_name)
        return FALSE;

    base = g_path_get_basename(name);
    result = (strcmp(base, trans_log_name) == 0);
    g_free(base);
    return result;
}

/********************************************************************\
\********************************************************************/

void
xaccOpenLog (void)
{
    char * filename;
    char * timestamp;

    if (!gen_logs) return;
    if (trans_log) return;

    if (!log_base_name) log_base_name = g_strdup ("translog");

    /* tag each filename with a timestamp */
    timestamp = xaccDateUtilGetStampNow ();

    filename = g_strconcat (log_base_name, ".", timestamp, ".log", NULL);

    trans_log = g_fopen (filename, "a");
    if (!trans_log)
    {
        int norr = errno;
        printf ("Error: xaccOpenLog(): cannot open journal \n"
		"\t %d %s\n", norr, g_strerror (norr) ? g_strerror (norr) : "");

        g_free (filename);
        g_free (timestamp);
        return;
    }

    /* Save the log file name */
    if (trans_log_name)
        g_free (trans_log_name);
    trans_log_name = g_path_get_basename(filename);

    g_free (filename);
    g_free (timestamp);

    /*  Note: this must match src/import-export/log-replay/gnc-log-replay.c */
    fprintf (trans_log, "mod\ttrans_guid\tsplit_guid\ttime_now\t"
             "date_entered\tdate_posted\t"
             "acc_guid\tacc_name\tnum\tdescription\t"
             "notes\tmemo\taction\treconciled\t"
             "amount\tvalue\tdate_reconciled\n");
    fprintf (trans_log, "-----------------\n");
}

/********************************************************************\
\********************************************************************/

void
xaccCloseLog (void)
{
    if (!trans_log) return;
    fflush (trans_log);
    fclose (trans_log);
    trans_log = NULL;
}

/********************************************************************\
\********************************************************************/

void
xaccTransWriteLog (Transaction *trans, char flag)
{
    GList *node;
    char trans_guid_str[GUID_ENCODING_LENGTH+1];
    char split_guid_str[GUID_ENCODING_LENGTH+1];
    const char *trans_notes;
    char dnow[100], dent[100], dpost[100], drecn[100];
    Timespec ts;

    if (!gen_logs) return;
    if (!trans_log) return;

    timespecFromTime_t(&ts, time(NULL));
    gnc_timespec_to_iso8601_buff (ts, dnow);

    timespecFromTime_t(&ts, trans->date_entered.tv_sec);
    gnc_timespec_to_iso8601_buff (ts, dent);

    timespecFromTime_t(&ts, trans->date_posted.tv_sec);
    gnc_timespec_to_iso8601_buff (ts, dpost);

    guid_to_string_buff (xaccTransGetGUID(trans), trans_guid_str);
    trans_notes = xaccTransGetNotes(trans);
    fprintf (trans_log, "===== START\n");

    for (node = trans->splits; node; node = node->next)
    {
        Split *split = node->data;
        const char * accname = "";
        char acc_guid_str[GUID_ENCODING_LENGTH+1];
        gnc_numeric amt, val;

        if (xaccSplitGetAccount(split))
        {
            accname = xaccAccountGetName (xaccSplitGetAccount(split));
            guid_to_string_buff(xaccAccountGetGUID(xaccSplitGetAccount(split)),
                                acc_guid_str);
        }
        else
        {
            acc_guid_str[0] = '\0';
        }

        timespecFromTime_t(&ts, split->date_reconciled.tv_sec);
        gnc_timespec_to_iso8601_buff (ts, drecn);

        guid_to_string_buff (xaccSplitGetGUID(split), split_guid_str);
        amt = xaccSplitGetAmount (split);
        val = xaccSplitGetValue (split);

        /* use tab-separated fields */
        fprintf (trans_log,
                 "%c\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t"
                 "%s\t%s\t%s\t%s\t%c\t%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT "\t%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT "\t%s\n",
                 flag,
                 trans_guid_str, split_guid_str,  /* trans+split make up unique id */
                 /* Note that the next three strings always exist,
                 		* so we don't need to test them. */
                 dnow,
                 dent,
                 dpost,
                 acc_guid_str,
                 accname ? accname : "",
                 trans->num ? trans->num : "",
                 trans->description ? trans->description : "",
                 trans_notes ? trans_notes : "",
                 split->memo ? split->memo : "",
                 split->action ? split->action : "",
                 split->reconciled,
                 gnc_numeric_num(amt),
                 gnc_numeric_denom(amt),
                 gnc_numeric_num(val),
                 gnc_numeric_denom(val),
                 /* The next string always exists. No need to test it. */
                 drecn);
    }

    fprintf (trans_log, "===== END\n");

    /* get data out to the disk */
    fflush (trans_log);
}

/********************************************************************\
\********************************************************************/

#if 0
/* open_memstream seems to give various distros fits
 * this has resulted in warfare on the mailing list.
 * I think the truce called required changing this to asprintf
 * this code is not currently used ...  so its ifdef out
 */

char *
xaccSplitAsString(Split *split, const char prefix[])
{
    char *result = NULL;
    size_t result_size;
    FILE *stream = open_memstream(&result, &result_size);
    const char *split_memo = xaccSplitGetMemo(split);
    const double split_value = gnc_numeric_to_double(xaccSplitGetValue(split));
    Account *split_dest = xaccSplitGetAccount(split);
    const char *dest_name =
        split_dest ? xaccAccountGetName(split_dest) : NULL;

    g_return_val_if_fail (stream, NULL);

    fputc('\n', stream);
    fputs(prefix, stream);
    fprintf(stream, "  %10.2f | %15s | %s",
            split_value,
            dest_name ? dest_name : "<no-account-name>",
            split_memo ? split_memo : "<no-split-memo>");
    fclose(stream);
    return(result);
}

static char *
xaccTransGetDateStr (Transaction *trans)
{
    char buf [MAX_DATE_LENGTH];
    struct tm *date;
    time_t secs;

    secs = xaccTransGetDate (trans);

    date = localtime (&secs);

    qof_print_date_buff(buf, date->tm_mday, date->tm_mon + 1, date->tm_year + 1900);

    return g_strdup (buf);
}

char *
xaccTransAsString(Transaction *txn, const char prefix[])
{
    char *result = NULL;
    size_t result_size;
    FILE *stream = open_memstream(&result, &result_size);
    time_t date = xaccTransGetDate(txn);
    const char *num = xaccTransGetNum(txn);
    const char *desc = xaccTransGetDescription(txn);
    const char *memo = xaccSplitGetMemo(xaccTransGetSplit(txn, 0));
    const double total = gnc_numeric_to_double(xaccSplitGetValue(xaccTransGetSplit(txn, 0)));

    g_return_val_if_fail (stream, NULL);

    fputs(prefix, stream);
    if (date)
    {
        char *datestr = xaccTransGetDateStr(txn);
        fprintf(stream, "%s", datestr);
        free(datestr);
    }
    else
    {
        fprintf(stream, "<no-date>");
    }
    fputc(' ', stream);
    if (num)
    {
        fputs(num, stream);
    }
    else
    {
        fprintf(stream, "<no-num>");
    }

    fputc('\n', stream);
    fputs(prefix, stream);
    if (desc)
    {
        fputs("  ", stream);
        fputs(desc, stream);
    }
    else
    {
        fprintf(stream, "<no-description>");
    }

    fputc('\n', stream);
    fputs(prefix, stream);
    if (memo)
    {
        fputs("  ", stream);
        fputs(memo, stream);
    }
    else
    {
        fprintf(stream, "<no-transaction-memo>");
    }

    {
        int split_count = xaccTransCountSplits(txn);
        int i;
        for (i = 1; i < split_count; i++)
        {
            Split *split = xaccTransGetSplit(txn, i);
            char *split_text = xaccSplitAsString(split, prefix);
            fputs(split_text, stream);
            free(split_text);
        }
    }
    fputc('\n', stream);

    fputs(prefix, stream);
    fprintf(stream, "  %10.2f -- Transaction total\n", total);
    fclose(stream);

    return(result);
}

#endif

/************************ END OF ************************************\
\************************* FILE *************************************/
