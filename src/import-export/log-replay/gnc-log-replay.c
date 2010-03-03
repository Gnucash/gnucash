/********************************************************************\
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
/** @addtogroup Import_Export
    @{ */
/** @internal
    @file gnc-log-replay.c
    @brief .log file replay code
    @author Copyright (c) 2003 Benoit Grégoire <bock@step.polymtl.ca>
*/
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <string.h>
#include <sys/time.h>
#include <libguile.h>
#include <errno.h>

#include "Account.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "Scrub.h"
#include "gnc-log-replay.h"
#include "gnc-file.h"
#include "qof.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"

#define GCONF_SECTION "dialogs/log_replay"

/* NW: If you want a new log_module, just define
a unique string either in gnc-engine.h or
locally.*/
/*static QofLogModule log_module = GNC_MOD_IMPORT;*/
static QofLogModule log_module = GNC_MOD_TEST;

/* fprintf (trans_log, "mod	guid	time_now	" \
   "date_entered	date_posted	" \
   "acc_guid	acc_name	num	description	" \
   "memo	action	reconciled	" \
   "amount	value date_reconciled\n");
   "%c\t%s/%s\t%s\t%s\t%s\t%s\t%s\t%s\t"
   "%s\t%s\t%s\t%c\t%lld/%lld\t%lld/%lld\t%s\n",
*/
#define STRING_FIELD_SIZE 256
typedef struct _split_record
{
    enum _enum_action {LOG_BEGIN_EDIT, LOG_ROLLBACK, LOG_COMMIT, LOG_DELETE} log_action;
    int log_action_present;
    GUID trans_guid;
    int trans_guid_present;
    GUID split_guid;
    int  split_guid_present;
    Timespec log_date;
    int log_date_present;
    Timespec date_entered;
    int date_entered_present;
    Timespec date_posted;
    int date_posted_present;
    GUID acc_guid;
    int acc_guid_present;
    char acc_name[STRING_FIELD_SIZE];
    int acc_name_present;
    char trans_num[STRING_FIELD_SIZE];
    int trans_num_present;
    char trans_descr[STRING_FIELD_SIZE];
    int trans_descr_present;
    char trans_notes[STRING_FIELD_SIZE];
    int trans_notes_present;
    char split_memo[STRING_FIELD_SIZE];
    int split_memo_present;
    char split_action[STRING_FIELD_SIZE];
    int split_action_present;
    char split_reconcile;
    int split_reconcile_present;
    gnc_numeric amount;
    int amount_present;
    gnc_numeric value;
    int value_present;
    Timespec date_reconciled;
    int date_reconciled_present;
} split_record;
/********************************************************************\
 * gnc_file_log_replay_import
 * Entry point
\********************************************************************/

static char *olds;
/* This version of strtok will only match SINGLE occurence of delim,
   returning a 0 length valid string between two consecutive ocurence of delim.
   It will also return a 0 length string instead of NULL when it reaches the end of s
*/
static char * my_strtok (s, delim)
char *s;
const char *delim;
{
    char *token;
    /*DEBUG("strtok(): Start...");*/
    if (s == NULL)
        s = olds;

    /* Scan leading delimiters.  */
    /*s += strspn (s, delim);*/ /*Don't do it, or we will loose count.*/
    if (*s == '\0')
    {
        olds = s;
        return s;
    }

    /* Find the end of the token.  */
    token = s;
    s = strpbrk (token, delim);
    if (s == NULL)
    {
        /* This token finishes the string.  */
        olds = strchr (token, '\0');
    }
    else
    {
        /* Terminate the token and make OLDS point past it.  */
        *s = '\0';
        olds = s + 1;
    }
    return token;
}

static split_record interpret_split_record( char *record_line)
{
    char * tok_ptr;
    split_record record;
    memset(&record, 0, sizeof(record));
    DEBUG("interpret_split_record(): Start...");
    if (strlen(tok_ptr = my_strtok(record_line, "\t")) != 0)
    {
        switch (tok_ptr[0])
        {
        case 'B':
            record.log_action = LOG_BEGIN_EDIT;
            break;
        case 'D':
            record.log_action = LOG_DELETE;
            break;
        case 'C':
            record.log_action = LOG_COMMIT;
            break;
        case 'R':
            record.log_action = LOG_ROLLBACK;
            break;
        }
        record.log_action_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        string_to_guid(tok_ptr, &(record.trans_guid));
        record.trans_guid_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        string_to_guid(tok_ptr, &(record.split_guid));
        record.split_guid_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        record.log_date = gnc_iso8601_to_timespec_gmt(tok_ptr);
        record.log_date_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        record.date_entered = gnc_iso8601_to_timespec_gmt(tok_ptr);
        record.date_entered_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        record.date_posted = gnc_iso8601_to_timespec_gmt(tok_ptr);
        record.date_posted_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        string_to_guid(tok_ptr, &(record.acc_guid));
        record.acc_guid_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        strncpy(record.acc_name, tok_ptr, STRING_FIELD_SIZE - 1);
        record.acc_name_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        strncpy(record.trans_num, tok_ptr, STRING_FIELD_SIZE - 1);
        record.trans_num_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        strncpy(record.trans_descr, tok_ptr, STRING_FIELD_SIZE - 1);
        record.trans_descr_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        strncpy(record.trans_notes, tok_ptr, STRING_FIELD_SIZE - 1);
        record.trans_notes_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        strncpy(record.split_memo, tok_ptr, STRING_FIELD_SIZE - 1);
        record.split_memo_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        strncpy(record.split_action, tok_ptr, STRING_FIELD_SIZE - 1);
        record.split_action_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        record.split_reconcile = tok_ptr[0];
        record.split_reconcile_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        string_to_gnc_numeric(tok_ptr, &(record.amount));
        record.amount_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        string_to_gnc_numeric(tok_ptr, &(record.value));
        record.value_present = TRUE;
    }
    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        record.date_reconciled = gnc_iso8601_to_timespec_gmt(tok_ptr);
        record.date_reconciled_present = TRUE;
    }

    if (strlen(tok_ptr = my_strtok(NULL, "\t")) != 0)
    {
        PERR("interpret_split_record():  Expected number of fields exceeded!");
    }
    DEBUG("interpret_split_record(): End");
    return record;
}

static void dump_split_record(split_record record)
{
    char * string_ptr = NULL;
    char string_buf[256];

    DEBUG("dump_split_record(): Start...");
    if (record.log_action_present)
    {
        switch (record.log_action)
        {
        case LOG_BEGIN_EDIT:
            DEBUG("Log action: LOG_BEGIN_EDIT");
            break;
        case LOG_DELETE:
            DEBUG("Log action: LOG_DELETE");
            break;
        case LOG_COMMIT:
            DEBUG("Log action: LOG_COMMIT");
            break;
        case LOG_ROLLBACK:
            DEBUG("Log action: LOG_ROLLBACK");
            break;
        }
    }
    if (record.trans_guid_present)
    {
        DEBUG("Transaction GUID: %s", guid_to_string (&(record.trans_guid)));
    }
    if (record.split_guid_present)
    {
        DEBUG("Split GUID: %s", guid_to_string (&(record.split_guid)));
    }
    if (record.log_date_present)
    {
        gnc_timespec_to_iso8601_buff (record.log_date, string_buf);
        DEBUG("Log entry date: %s", string_buf);
    }
    if (record.date_entered_present)
    {
        gnc_timespec_to_iso8601_buff (record.date_entered, string_buf);
        DEBUG("Date entered: %s", string_buf);
    }
    if (record.date_posted_present)
    {
        gnc_timespec_to_iso8601_buff (record.date_posted, string_buf);
        DEBUG("Date posted: %s", string_buf);
    }
    if (record.acc_guid_present)
    {
        DEBUG("Account GUID: %s", guid_to_string (&(record.acc_guid)));
    }
    if (record.acc_name_present)
    {
        DEBUG("Account name: %s", record.acc_name);
    }
    if (record.trans_num_present)
    {
        DEBUG("Transaction number: %s", record.trans_num);
    }
    if (record.trans_descr_present)
    {
        DEBUG("Transaction description: %s", record.trans_descr);
    }
    if (record.trans_notes_present)
    {
        DEBUG("Transaction notes: %s", record.trans_notes);
    }
    if (record.split_memo_present)
    {
        DEBUG("Split memo: %s", record.split_memo);
    }
    if (record.split_action_present)
    {
        DEBUG("Split action: %s", record.split_action);
    }
    if (record.split_reconcile_present)
    {
        DEBUG("Split reconcile: %c", record.split_reconcile);
    }
    if (record.amount_present)
    {
        string_ptr = gnc_numeric_to_string(record.amount);
        DEBUG("Record amount: %s", string_ptr);
        g_free(string_ptr);
    }
    if (record.value_present)
    {
        string_ptr = gnc_numeric_to_string(record.value);
        DEBUG("Record value: %s", string_ptr);
        g_free(string_ptr);
    }
    if (record.date_reconciled_present)
    {
        gnc_timespec_to_iso8601_buff (record.date_reconciled, string_buf);
        DEBUG("Reconciled date: %s", string_buf);
    }
}

/* File pointer must already be at the begining of a record */
static void  process_trans_record(  FILE *log_file)
{
    char read_buf[2048];
    char *read_retval;
    const char * record_end_str = "===== END";
    int first_record = TRUE;
    int record_ended = FALSE;
    int split_num = 0;
    split_record record;
    Transaction * trans = NULL;
    Split * split = NULL;
    Account * acct = NULL;
    QofBook * book = gnc_get_current_book();

    DEBUG("process_trans_record(): Begin...\n");

    while ( record_ended == FALSE)
    {
        read_retval = fgets(read_buf, sizeof(read_buf), log_file);
        if (read_retval != NULL && strncmp(record_end_str, read_buf, strlen(record_end_str)) != 0) /* If we are not at the end of the record */
        {
            split_num++;
            /*DEBUG("process_trans_record(): Line read: %s%s",read_buf ,"\n");*/
            record = interpret_split_record( read_buf);
            dump_split_record( record);
            if (record.log_action_present)
            {
                switch (record.log_action)
                {
                case LOG_BEGIN_EDIT:
                    DEBUG("process_trans_record():Ignoring log action: LOG_BEGIN_EDIT"); /*Do nothing, there is no point*/
                    break;
                case LOG_ROLLBACK:
                    DEBUG("process_trans_record():Ignoring log action: LOG_ROLLBACK");/*Do nothing, since we didn't do the begin_edit either*/
                    break;
                case LOG_DELETE:
                    DEBUG("process_trans_record(): Playing back LOG_DELETE");
                    if ((trans = xaccTransLookup (&(record.trans_guid), book)) != NULL
                            && first_record == TRUE)
                    {
                        xaccTransBeginEdit(trans);
                        xaccTransDestroy(trans);
                    }
                    else if (first_record == TRUE)
                    {
                        PERR("The transaction to delete was not found!");
                    }
                    break;
                case LOG_COMMIT:
                    DEBUG("process_trans_record(): Playing back LOG_COMMIT");
                    if (record.trans_guid_present == TRUE
                            && (trans = xaccTransLookupDirect (record.trans_guid, book)) != NULL
                            && first_record == TRUE)
                    {
                        DEBUG("process_trans_record(): Transaction to be edited was found");/*Destroy the current transaction, we will create a new one to replace it*/
                        xaccTransBeginEdit(trans);
                        xaccTransDestroy(trans);
                        xaccTransCommitEdit(trans);
                    }

                    if (record.trans_guid_present == TRUE
                            && first_record == TRUE)
                    {
                        DEBUG("process_trans_record(): Creating the new transaction");
                        trans = xaccMallocTransaction (book);
                        xaccTransBeginEdit(trans);
                        xaccTransSetGUID (trans, &(record.trans_guid));
                        /*Fill the transaction info*/
                        if (record.date_entered_present)
                        {
                            xaccTransSetDateEnteredTS(trans, &(record.date_entered));
                        }
                        if (record.date_posted_present)
                        {
                            xaccTransSetDatePostedTS(trans, &(record.date_posted));
                        }
                        if (record.trans_num_present)
                        {
                            xaccTransSetNum(trans, record.trans_num);
                        }
                        if (record.trans_descr_present)
                        {
                            xaccTransSetDescription(trans, record.trans_descr);
                        }
                        if (record.trans_notes_present)
                        {
                            xaccTransSetNotes(trans, record.trans_notes);
                        }
                    }
                    if (record.split_guid_present == TRUE) /*Fill the split info*/
                    {
                        split = xaccMallocSplit(book);
                        xaccSplitSetGUID (split, &(record.split_guid));
                        if (record.acc_guid_present)
                        {
                            acct = xaccAccountLookupDirect(record.acc_guid, book);
                            xaccAccountInsertSplit(acct, split);
                        }
                        xaccTransAppendSplit(trans, split);

                        if (record.split_memo_present)
                        {
                            xaccSplitSetMemo(split, record.split_memo);
                        }
                        if (record.split_action_present)
                        {
                            xaccSplitSetAction(split, record.split_action);
                        }
                        if (record.date_reconciled_present)
                        {
                            xaccSplitSetDateReconciledTS (split, &(record.date_reconciled));
                        }
                        if (record.split_reconcile_present)
                        {
                            xaccSplitSetReconcile(split, record.split_reconcile);
                        }

                        if (record.amount_present)
                        {
                            xaccSplitSetAmount(split, record.amount);
                        }
                        if (record.value_present)
                        {
                            xaccSplitSetValue(split, record.value);
                        }
                    }
                    first_record = FALSE;
                    break;
                }
            }
            else
            {
                PERR("Corrupted record");
            }
        }
        else /* The record ended */
        {
            record_ended = TRUE;
            DEBUG("process_trans_record(): Record ended\n");
            if (trans != NULL) /*If we played with a transaction, commit it here*/
            {
                xaccTransScrubCurrencyFromSplits(trans);
                xaccTransCommitEdit(trans);
            }
        }
    }
}

void gnc_file_log_replay (void)
{
    char *selected_filename;
    char *default_dir;
    char read_buf[256];
    char *read_retval;
    GtkFileFilter *filter;
    FILE *log_file;
    char * record_start_str = "===== START";
    /* NOTE: This string must match src/engine/TransLog.c (sans newline) */
    char * expected_header_orig = "mod\ttrans_guid\tsplit_guid\ttime_now\t"
                                  "date_entered\tdate_posted\tacc_guid\tacc_name\tnum\tdescription\t"
                                  "notes\tmemo\taction\treconciled\tamount\tvalue\tdate_reconciled";
    static char *expected_header = NULL;

    /* Use g_strdup_printf so we don't get accidental tab -> space conversion */
    if (!expected_header)
        expected_header = g_strdup(expected_header_orig);

    qof_log_set_level(GNC_MOD_IMPORT, QOF_LOG_DEBUG);
    ENTER(" ");

    default_dir = gnc_get_default_directory(GCONF_SECTION);

    filter = gtk_file_filter_new();
    gtk_file_filter_set_name(filter, "*.log");
    gtk_file_filter_add_pattern(filter, "*.[Ll][Oo][Gg]");
    selected_filename = gnc_file_dialog(_("Select a .log file to replay"),
                                        g_list_prepend(NULL, filter),
                                        default_dir,
                                        GNC_FILE_DIALOG_OPEN);
    g_free(default_dir);

    if (selected_filename != NULL)
    {
        /* Remember the directory as the default. */
        default_dir = g_path_get_dirname(selected_filename);
        gnc_set_default_directory(GCONF_SECTION, default_dir);
        g_free(default_dir);

        /*strncpy(file,selected_filename, 255);*/
        DEBUG("Filename found: %s", selected_filename);
        if (xaccFileIsCurrentLog(selected_filename))
        {
            g_warning("Cannot open the current log file: %s", selected_filename);
            gnc_error_dialog(NULL,
                             /* Translators: %s is the file name. */
                             _("Cannot open the current log file: %s"),
                             selected_filename);
        }
        else
        {
            DEBUG("Opening selected file");
            log_file = g_fopen(selected_filename, "r");
            if (!log_file || ferror(log_file) != 0)
            {
                int err = errno;
                perror("File open failed");
                gnc_error_dialog(NULL,
                                 /* Translation note:
                                  * First argument is the filename,
                                  * second argument is the error.
                                  */
                                 _("Failed to open log file: %s: %s"),
                                 selected_filename,
                                 strerror(err));
            }
            else
            {
                if ((read_retval = fgets(read_buf, sizeof(read_buf), log_file)) == NULL)
                {
                    DEBUG("Read error or EOF");
                    gnc_info_dialog(NULL, "%s",
                                    _("The log file you selected was empty."));
                }
                else
                {
                    if (strncmp(expected_header, read_buf, strlen(expected_header)) != 0)
                    {
                        PERR("File header not recognised:\n%s", read_buf);
                        PERR("Expected:\n%s", expected_header);
                        gnc_error_dialog(NULL, "%s",
                                         _("The log file you selected cannot be read.  "
                                           "The file header was not recognized."));
                    }
                    else
                    {
                        do
                        {
                            read_retval = fgets(read_buf, sizeof(read_buf), log_file);
                            /*DEBUG("Chunk read: %s",read_retval);*/
                            if (strncmp(record_start_str, read_buf, strlen(record_start_str)) == 0) /* If a record started */
                            {
                                process_trans_record(log_file);
                            }
                        }
                        while (feof(log_file) == 0);
                    }
                }
                fclose(log_file);
            }
        }
        g_free(selected_filename);
    }
    LEAVE("");
}


/** @} */
