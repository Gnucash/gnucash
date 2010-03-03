/********************************************************************\
 * guile-util.h -- utility functions for using guile for GnuCash    *
 * Copyright (C) 1999 Linas Vepstas                                 *
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

#ifndef GUILE_UTIL_H
#define GUILE_UTIL_H

#include <glib.h>
#include <libguile.h>

#include "qof.h"
#include "Account.h"


/* Helpful functions for calling functions that return
 * specific kinds of values. These functions do error
 * checking to verify the result is of the correct type. */
char * gnc_guile_call1_to_string(SCM func, SCM arg);
char * gnc_guile_call1_symbol_to_string(SCM func, SCM arg);
SCM    gnc_guile_call1_to_procedure(SCM func, SCM arg);
SCM    gnc_guile_call1_to_list(SCM func, SCM arg);
SCM    gnc_guile_list_ref(SCM list, int index);
SCM    gnc_guile_call1_to_vector(SCM func, SCM arg);

/* Don't use this to get hold of symbols that are considered private
 * to a given module unless the C code you're writing is considered
 * part of that module.  */
SCM    gnc_scm_lookup(const char *module, const char *symbol);

/* The next set of functions is for manipulating scheme
 * representations of splits and transactions. */
gboolean gnc_is_split_scm(SCM scm);
gboolean gnc_is_trans_scm(SCM scm);

SCM    gnc_copy_split(Split *split, gboolean use_cut_semantics);
void   gnc_copy_split_scm_onto_split(SCM split_scm, Split *split,
                                     QofBook *book);

void   gnc_split_scm_set_account(SCM split_scm, Account *account);
void   gnc_split_scm_set_memo(SCM split_scm, const char *memo);
void   gnc_split_scm_set_action(SCM split_scm, const char *action);
void   gnc_split_scm_set_reconcile_state(SCM split_scm, char reconcile_state);
void   gnc_split_scm_set_amount(SCM split_scm, gnc_numeric amount);
void   gnc_split_scm_set_value(SCM split_scm, gnc_numeric value);

char * gnc_split_scm_get_memo(SCM split_scm);
char * gnc_split_scm_get_action(SCM split_scm);
gnc_numeric gnc_split_scm_get_amount(SCM split_scm);
gnc_numeric gnc_split_scm_get_value(SCM split_scm);

SCM    gnc_copy_trans(Transaction *trans, gboolean use_cut_semantics);
void   gnc_copy_trans_scm_onto_trans(SCM trans_scm, Transaction *trans,
                                     gboolean do_commit, QofBook *book);
void   gnc_copy_trans_scm_onto_trans_swap_accounts(SCM trans_scm,
        Transaction *trans,
        const GUID *guid_1,
        const GUID *guid_2,
        gboolean do_commit,
        QofBook *book);

void   gnc_trans_scm_set_date(SCM trans_scm, Timespec *ts);
void   gnc_trans_scm_set_num(SCM trans_scm, const char *num);
void   gnc_trans_scm_set_description(SCM trans_scm, const char *description);
void   gnc_trans_scm_set_notes(SCM trans_scm, const char *notes);
void   gnc_trans_scm_append_split_scm(SCM trans_scm, SCM split_scm);

SCM    gnc_trans_scm_get_split_scm(SCM trans_scm, int index);
SCM    gnc_trans_scm_get_other_split_scm(SCM trans_scm, SCM split_scm);
int    gnc_trans_scm_get_num_splits(SCM trans_scm);

/* Two functions that return string synonyms for the terms 'debit' and
 * 'credit' as appropriate for the given account type and user preferences.
 * They should be g_freed when no longer needed. */
char * gnc_get_debit_string(GNCAccountType account_type);
char * gnc_get_credit_string(GNCAccountType account_type);

/** Clean up a scheme options string for use in a key/value file.
 *  This function removes all full line comments, removes all blank
 *  lines, and removes all leading/trailing white space.
 *
 *  @note: This function does not correctly handle comments that occur
 *  at the end of a line. Fortunately there aren't any such
 *  comments. */
gchar *gnc_guile_strip_comments (const gchar *text);

/** An opaque process structure returned by gnc_spawn_process_async. */
typedef struct _Process Process;

/** Wraps g_spawn_async_with_pipes minimally.  Use gnc_process_get_fd to access
 *  the file descriptors to the child.  To close them them and free the memory
 *  allocated for the process once it has exited, call gnc_detach_process.
 *
 *  @param argl A list of null-terminated strings used as arguments for spawning,
 *  i.e. "perl" "-w" "my-perl-script".  Will be freed inside.
 *
 *  @param search_path Determines whether the first element of argl will be
 *  looked for in the user's PATH.
 *
 *  @return A pointer to a structure representing the process or NULL on failure.
 */
Process *gnc_spawn_process_async(GList *argl, const gboolean search_path);

/** Accesses a given process structure and returns the file descriptor connected
 *  to the childs stdin, stdout or stderr.
 *
 *  @param proc A process structure returned by gnc_spawn_process_async.
 *
 *  @param std_fd 0, 1 or 2.
 *
 *  @return The file descriptor to write to the child on 0, or read from the
 *  childs output or error on 1 or 2, resp. */
gint gnc_process_get_fd(const Process *proc, const gint std_fd);

/** Close the file descriptors to a given process and declare it as detached.  If
 *  it is both dead and detached, the allocated memory will be freed.
 *
 *  @param proc A process structure returned by gnc_spawn_process_async.
 *
 *  @param kill_it If TRUE, kill the process. */
void gnc_detach_process(Process *proc, const gboolean kill_it);

#endif

/** Convert a time string to calendar time representation.  Combine strptime and
 *  mktime into a single function to avoid the need to wrap struct tm *.
 *
 *  @param s String representation of time.
 *
 *  @param format Format specification.
 *
 *  @return The time in seconds since unix epoch, or -1 on error */
time_t gnc_parse_time_to_timet(const gchar *s, const gchar *format);
