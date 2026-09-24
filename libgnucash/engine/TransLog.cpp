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

#include <config.h>
#ifdef __MINGW32__
#define __USE_MINGW_ANSI_STDIO 1
#endif
#include <errno.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <string>
#include <fstream>

#include "Account.h"
#include "Transaction.h"
#include "TransactionP.hpp"
#include "TransLog.h"
#include "qof.h"
#ifdef _MSC_VER
# define g_fopen fopen
#endif

static QofLogModule log_module = "gnc.translog";

/*
 * Some design philosophy that I think would be good to keep in mind:
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
static std::ofstream trans_log_stream; /**< current log file handle */
static std::string trans_log_name; /**< current log file name */
static std::string log_base_name;
static constexpr char trans_log_suppression_key[] =
    "gnc-transaction-log-suppression";

struct TransLogSuppressionState
{
    gatomicrefcount ref_count;
    guint token_count;
    gboolean attached;
};

struct GncTransLogSuppression
{
    TransLogSuppressionState *state;
};

static TransLogSuppressionState *trans_log_suppression_state_ref (
    TransLogSuppressionState *state)
{
    if (state)
        g_atomic_ref_count_inc (&state->ref_count);
    return state;
}

static void trans_log_suppression_state_unref (
    TransLogSuppressionState *state)
{
    if (state && g_atomic_ref_count_dec (&state->ref_count))
        g_free (state);
}

static void trans_log_suppression_book_finalizer (
    QofBook *, gpointer, gpointer data)
{
    auto state = static_cast<TransLogSuppressionState *> (data);
    if (!state)
        return;
    state->attached = FALSE;
    trans_log_suppression_state_unref (state);
}

GncTransLogSuppression *xaccTransLogSuppressForBook (QofBook *book)
{
    if (!book)
        return nullptr;
    auto suppression = g_new0 (GncTransLogSuppression, 1);
    if (!suppression)
        return nullptr;
    auto state = static_cast<TransLogSuppressionState *> (
        qof_book_get_data (book, trans_log_suppression_key));
    if (!state)
    {
        state = g_new0 (TransLogSuppressionState, 1);
        if (!state)
        {
            g_free (suppression);
            return nullptr;
        }
        g_atomic_ref_count_init (&state->ref_count);
        state->attached = TRUE;
        qof_book_set_data_fin (book, trans_log_suppression_key, state,
                               trans_log_suppression_book_finalizer);
    }
    suppression->state = trans_log_suppression_state_ref (state);
    ++state->token_count;
    return suppression;
}

void xaccTransLogSuppressionRelease (GncTransLogSuppression *suppression)
{
    if (!suppression)
        return;
    auto state = suppression->state;
    suppression->state = nullptr;
    if (state)
    {
        g_assert (state->token_count > 0);
        --state->token_count;
        trans_log_suppression_state_unref (state);
    }
    g_free (suppression);
}

gboolean xaccTransLogSuppressedForBook (const QofBook *book)
{
    auto state = book ? static_cast<TransLogSuppressionState *> (
        qof_book_get_data (const_cast<QofBook *> (book),
                           trans_log_suppression_key)) : nullptr;
    return state && state->attached && state->token_count > 0;
}


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
    if (trans_log_stream.is_open())
    {
        xaccCloseLog();
        xaccOpenLog();
    }
}


void
xaccLogSetBaseName (const char *basepath)
{
    if (!basepath) return;

    log_base_name = basepath;

    if (trans_log_stream.is_open())
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

    if (!name || trans_log_name.empty())
        return FALSE;

    base = g_path_get_basename(name);
    bool result = trans_log_name.compare(base) == 0;
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

    if (!gen_logs)
    {
	 PINFO ("Attempt to open disabled transaction log");
	 return;
    }
    if (trans_log_stream.is_open()) return;

    if (log_base_name.empty())
        log_base_name = "translog";

    /* tag each filename with a timestamp */
    timestamp = gnc_date_timestamp ();

    filename = g_strconcat (log_base_name.c_str(), ".", timestamp, ".log", nullptr);

    trans_log_stream.open(filename, std::ios::app);
    if (!trans_log_stream.is_open())
    {
        int norr = errno;
        printf ("Error: xaccOpenLog(): cannot open journal\n"
                "\t %d %s\n", norr, g_strerror (norr) ? g_strerror (norr) : "");

        g_free (filename);
        g_free (timestamp);
        return;
    }

    /* Save the log file name */
    auto tmpstr = g_path_get_basename(filename);
    trans_log_name = tmpstr;
    g_free (tmpstr);

    g_free (filename);
    g_free (timestamp);

    /*  Note: this must match src/import-export/log-replay/gnc-log-replay.c */
    trans_log_stream << "mod\ttrans_guid\tsplit_guid\ttime_now\t"
                     << "date_entered\tdate_posted\t"
                     << "acc_guid\tacc_name\tnum\tdescription\t"
                     << "notes\tmemo\taction\treconciled\t"
                     << "amount\tvalue\tdate_reconciled\n"
                     << "-----------------\n";
}

/********************************************************************\
\********************************************************************/

void
xaccCloseLog (void)
{
    if (!trans_log_stream.is_open()) return;
    trans_log_stream.flush();
    trans_log_stream.close();
}

/********************************************************************\
\********************************************************************/

void
xaccTransWriteLog (Transaction *trans, char flag)
{
    GList *node;
    char trans_guid_str[GUID_ENCODING_LENGTH + 1];
    char split_guid_str[GUID_ENCODING_LENGTH + 1];
    const char *trans_notes;
    char dnow[100], dent[100], dpost[100], drecn[100];
    if (xaccTransLogSuppressedForBook (xaccTransGetBook (trans)))
        return;


    if (!gen_logs)
    {
         PINFO ("Attempt to write disabled transaction log");
	 return;
    }
    if (!trans_log_stream.is_open()) return;

    gnc_time64_to_iso8601_buff (gnc_time(nullptr), dnow);
    gnc_time64_to_iso8601_buff (trans->date_entered, dent);
    gnc_time64_to_iso8601_buff (trans->date_posted, dpost);
    guid_to_string_buff (xaccTransGetGUID(trans), trans_guid_str);
    trans_notes = xaccTransGetNotes(trans);
    trans_log_stream << "===== START\n";

    for (node = trans->splits; node; node = node->next)
    {
        Split *split = GNC_SPLIT(node->data);
        const char * accname = "";
        char acc_guid_str[GUID_ENCODING_LENGTH + 1];
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

        gnc_time64_to_iso8601_buff (split->date_reconciled, drecn);

        guid_to_string_buff (xaccSplitGetGUID(split), split_guid_str);
        amt = xaccSplitGetAmount (split);
        val = xaccSplitGetValue (split);

        trans_log_stream << flag << '\t'
                         << trans_guid_str << '\t'
                         << split_guid_str << '\t'
                         << dnow << '\t'
                         << dent << '\t'
                         << dpost << '\t'
                         << acc_guid_str << '\t'
                         << (accname ? accname : "") << '\t'
                         << (trans->num ? trans->num : "") << '\t'
                         << (trans->description ? trans->description : "") << '\t'
                         << (trans_notes ? trans_notes : "") << '\t'
                         << (split->memo ? split->memo : "") << '\t'
                         << (split->action ? split->action : "") << '\t'
                         << split->reconciled << '\t'
                         << gnc_numeric_num(amt) << '/' << gnc_numeric_denom(amt) << '\t'
                         << gnc_numeric_num(val) << '/' << gnc_numeric_denom(val) << '\t'
                         << drecn << '\n';
    }

    trans_log_stream << "===== END" << std::endl;
}

/************************ END OF ************************************\
\************************* FILE *************************************/
