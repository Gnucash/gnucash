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
    @file import-backend.c
    @brief import-backend.c: Generic importer backend implementation (duplicate matching algorithm, action handling,  etc.)
    @author Copyright (C) 2002 Benoit Grégoire
    @author Christian Stimming
    @author Copyright (c) 2006 David Hampton <hampton@employees.org>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdlib.h>
#include <math.h>

#include <errno.h>

#include "import-backend.h"
#include "import-utilities.h"
#include "Account.h"
#include "Query.h"
#include "gnc-engine.h"
#include "engine-helpers.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"

#include <algorithm>

#define GNCIMPORT_DESC    "desc"
#define GNCIMPORT_MEMO    "memo"
#define GNCIMPORT_PAYEE   "payee"

/********************************************************************\
 *   Constants                                                      *
\********************************************************************/

static QofLogModule log_module = GNC_MOD_IMPORT;

/********************************************************************\
 *   Forward declared prototypes                                    *
\********************************************************************/

static void matchmap_store_destination(Account* base_acc,
                                       GNCImportTransInfo* trans_info,
                                       gboolean use_match);

static void trans_info_calculate_dest_amount (GNCImportTransInfo *info);


/********************************************************************\
 *               Structures passed between the functions            *
\********************************************************************/

struct _selected_match_info
{
    GNCImportMatchInfo *selected_match;
    gboolean selected_manually;
};

struct _transactioninfo
{
    Transaction * trans;
    Split * first_split;

    /* GList of GNCImportMatchInfo's, one for each possible duplicate match. */
    GList * match_list;
    GNCImportSelectedMatchInfo selected_match_info;

    GNCImportAction action;
    GNCImportAction previous_action;

    /* A list of tokenized strings to use for bayesian matching purposes */
    GList * match_tokens;

    /* In case of a single destination account it is stored here. */
    Account *dest_acc;
    gboolean dest_acc_selected_manually;

    /* Reference id to link gnc transaction to external object. E.g. aqbanking job id. */
    guint32 ref_id;

    /* When updating a matched transaction, append Description and Notes instead of replacing */
    gboolean append_text;

    /* Extra data we can use to build the balancing split. It may be passed on by the
     * code that calls the generic importer */
    gnc_numeric lsplit_price;
    char *lsplit_action;
    char *lsplit_memo;
    char lsplit_rec_state;
    time64 lsplit_rec_date;

    gnc_numeric lsplit_value;
    /* Amount for the balancing split. This may be passed by the import front-
     * ends or calculated. The latter is only possible when
     * the destination account is known and may require an exchange rate
     * if that account is not in the same commodity as the transaction. */
    gnc_numeric lsplit_amount;
    gboolean lsplit_amount_selected_manually;
};

/* Some simple getters and setters for the above data types. */

GList *
gnc_import_TransInfo_get_match_list (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->match_list;
}

void
gnc_import_TransInfo_set_match_list (GNCImportTransInfo *info, GList* match_list)
{
    g_assert (info);
    info->match_list = match_list;
    if (match_list)
        info->selected_match_info.selected_match = static_cast<GNCImportMatchInfo*>(match_list->data);
    else
    {
        info->selected_match_info.selected_match = nullptr;
        gnc_import_TransInfo_set_action (info, GNCImport_ADD);
    }
}

Transaction *
gnc_import_TransInfo_get_trans (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->trans;
}

gboolean
gnc_import_TransInfo_is_balanced (const GNCImportTransInfo *info)
{
    g_assert (info);
    return gnc_numeric_zero_p(xaccTransGetImbalanceValue(gnc_import_TransInfo_get_trans(info)));
}

Split *
gnc_import_TransInfo_get_fsplit (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->first_split;
}

GNCImportMatchInfo *
gnc_import_TransInfo_get_selected_match (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->selected_match_info.selected_match;
}

void
gnc_import_TransInfo_set_selected_match_info (GNCImportTransInfo *info,
        GNCImportMatchInfo *match,
        gboolean selected_manually)
{
    g_assert (info);
    info->selected_match_info.selected_match = match;
    info->selected_match_info.selected_manually = selected_manually;
}

gboolean
gnc_import_TransInfo_get_match_selected_manually (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->selected_match_info.selected_manually;
}

GNCImportAction
gnc_import_TransInfo_get_action (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->action;
}

void
gnc_import_TransInfo_set_action (GNCImportTransInfo *info,
                                 GNCImportAction action)
{
    g_assert (info);
    if (action != info->action)
    {
        info->previous_action = info->action;
        info->action = action;
    }
}

Account *
gnc_import_TransInfo_get_destacc (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->dest_acc;
}
void gnc_import_TransInfo_set_destacc (GNCImportTransInfo *info,
                                       Account *acc,
                                       gboolean selected_manually)
{
    g_assert (info);
    info->dest_acc = acc;
    info->dest_acc_selected_manually = selected_manually;

    /* Store the mapping to the other account in the MatchMap. */
    if (selected_manually)
        matchmap_store_destination (nullptr, info, false);

    trans_info_calculate_dest_amount (info);
}

gboolean
gnc_import_TransInfo_get_destacc_selected_manually (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->dest_acc_selected_manually;
}

guint32
gnc_import_TransInfo_get_ref_id (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->ref_id;
}

void
gnc_import_TransInfo_set_ref_id (GNCImportTransInfo *info,
                                 guint32 ref_id)
{
    g_assert (info);
    info->ref_id = ref_id;
}

gnc_numeric
gnc_import_TransInfo_get_price (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->lsplit_price;
}

void
gnc_import_TransInfo_set_price (GNCImportTransInfo *info,
                                 gnc_numeric lprice)
{
    g_assert (info);
    info->lsplit_price = lprice;
    /* if a valid price is explicitly set, assume the user wants to
     * use it to calculate balance split amount.
     * Ensure this gets recalculated */
    if (gnc_numeric_check (lprice) == 0)
    {
        info->lsplit_amount_selected_manually = false;
        trans_info_calculate_dest_amount(info);
    }
}

gnc_numeric
gnc_import_TransInfo_get_dest_amount (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->lsplit_amount;
}

gnc_numeric
gnc_import_TransInfo_get_dest_value (const GNCImportTransInfo *info)
{
    g_assert (info);
    return info->lsplit_value;
}

void
gnc_import_TransInfo_set_last_split_info (GNCImportTransInfo *info,
                                          GNCImportLastSplitInfo *lsplit)
{
    g_assert (info);
    if (lsplit)
    {
        info->lsplit_price = lsplit->price;
        info->lsplit_action = g_strdup(lsplit->action);
        info->lsplit_memo = g_strdup(lsplit->memo);
        if (gnc_numeric_check (lsplit->amount) == 0)
        {
            info->lsplit_amount = lsplit->amount;
            info->lsplit_amount_selected_manually = true;
        }
        info->dest_acc = lsplit->account;
        info->lsplit_rec_state = lsplit->rec_state;
        info->lsplit_rec_date = lsplit->rec_date;
    }
}

void
gnc_import_TransInfo_set_append_text (GNCImportTransInfo *info,
                                      gboolean append_text)
{
    g_assert (info);
    info->append_text = append_text;
}


Split *
gnc_import_MatchInfo_get_split (const GNCImportMatchInfo * info)
{
    g_assert (info);
    return info->split;
}

gint
gnc_import_MatchInfo_get_probability (const GNCImportMatchInfo * info)
{
    if (info)
        return info->probability;
    else
        return 0;
}

void gnc_import_TransInfo_delete (GNCImportTransInfo *info)
{
    if (info)
    {
        g_list_free (info->match_list);
        /*If the transaction exists and is still open, it must be destroyed*/
        if (xaccTransIsOpen(info->trans))
        {
            xaccTransDestroy(info->trans);
            xaccTransCommitEdit(info->trans);
        }
        g_list_free_full (info->match_tokens, g_free);
        g_free(info->lsplit_action);
        g_free(info->lsplit_memo);

        g_free(info);
    }
}

GdkPixbuf* gen_probability_pixbuf(gint score_original, GNCImportSettings *settings, GtkWidget * widget)
{
    constexpr gint height = 15;
    constexpr gint width_each_bar = 7;
    constexpr gint width_first_bar = 1;
    constexpr gint num_colors = 5;
    gchar * xpm[2 + num_colors + height];

    g_assert(settings);
    g_assert(widget);

    auto score = std::max (0, score_original);

    /* Add size definition to xpm */
    xpm[0] = g_strdup_printf("%d%s%d%s%d%s", (width_each_bar * score) + width_first_bar/*width*/, " ", height, " ", num_colors, " 1"/*characters per pixel*/);

    /* Define used colors */
    xpm[1] = g_strdup("  c None");
    xpm[2] = g_strdup("g c green");
    xpm[3] = g_strdup("y c yellow");
    xpm[4] = g_strdup("r c red");
    xpm[5] = g_strdup("b c black");

    auto add_threshold = gnc_import_Settings_get_add_threshold(settings);
    auto clear_threshold = gnc_import_Settings_get_clear_threshold(settings);
    for (int i = 0; i < height; i++)
    {
        xpm[num_colors+1+i] = g_new0(char, (width_each_bar * score) + width_first_bar + 1);
        for (int j = 0; j <= score; j++)
        {
            if (j == 0)
                strcat(xpm[num_colors+1+i], "b");
            else if (i == 0 || i == height - 1)
                strcat(xpm[num_colors+1+i], "bbbbbb ");
            else if (j <= add_threshold)
                strcat(xpm[num_colors+1+i], "brrrrb ");
            else if (j >= clear_threshold)
                strcat(xpm[num_colors+1+i], "bggggb ");
            else
                strcat(xpm[num_colors+1+i], "byyyyb ");
        }
    }

    auto retval = gdk_pixbuf_new_from_xpm_data((const gchar **)xpm);
    for (int i = 0; i <= num_colors + height; i++)
        g_free(xpm[i]);

    return retval;
}

/*************************************************************************
 * MatchMap related functions (storing and retrieving)
 */

/* Tokenize a string and append the tokens to an existing GList
 * (or an empty GList)
 */
static GList*
tokenize_string(GList* existing_tokens, const char *string)
{
    char **tokenized_strings = g_strsplit(string, " ", 0);
    char **stringpos = tokenized_strings;

    /* add each unique non-empty token to the token GList */
    while (stringpos && *stringpos)
    {
        if ((strlen(*stringpos) > 0) &&
            (!g_list_find_custom (existing_tokens, *stringpos, (GCompareFunc)g_strcmp0)))
                existing_tokens = g_list_prepend(existing_tokens, g_strdup(*stringpos));

        stringpos++;
    }

    /* free up the strings that g_strsplit() created */
    g_strfreev(tokenized_strings);

    return existing_tokens;
}

/* create and return a list of tokens for a given transaction info. */
static GList*
TransactionGetTokens(GNCImportTransInfo *info)
{

    g_return_val_if_fail (info, nullptr);
    if (info->match_tokens) return info->match_tokens;

    auto transaction = gnc_import_TransInfo_get_trans(info);
    g_assert(transaction);

    /* make tokens from the transaction description */
    auto text = xaccTransGetDescription(transaction);
    GList *tokens = nullptr;
    tokens = tokenize_string(tokens, text);

    /* The day of week the transaction occurred is a good indicator of
     * what account this transaction belongs in.  Get the date and convert
     * it to day of week as a token
     */
    auto transtime = xaccTransGetDate(transaction);
    auto tm_struct = gnc_gmtime(&transtime);
    char local_day_of_week[16];
    if (!qof_strftime(local_day_of_week, sizeof(local_day_of_week), "%A", tm_struct))
        PERR("TransactionGetTokens: error, strftime failed\n");
    gnc_tm_free (tm_struct);
    /* we cannot add a locally allocated string to this array, dup it so
     * it frees the same way the rest do
     */
    tokens = g_list_prepend(tokens, g_strdup(local_day_of_week));

    /* make tokens from the memo of each split of this transaction */
    for (GList *node=xaccTransGetSplitList (transaction); node; node=node->next)
    {
        text = xaccSplitGetMemo(static_cast<Split*>(node->data));
        tokens = tokenize_string(tokens, text);
    }

    info->match_tokens = tokens;
    return tokens;
}

/* searches using the GNCImportTransInfo through all existing transactions
 * if there is an exact match of the description and memo
 */
static Account *
matchmap_find_destination (Account *base_acc, GNCImportTransInfo *info)
{
    g_assert (info);
    auto orig_acc = (base_acc ? base_acc : xaccSplitGetAccount
                        (gnc_import_TransInfo_get_fsplit (info)));

    Account *result = nullptr;
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_IMPORT, GNC_PREF_USE_BAYES))
    {
        /* get the tokens for this transaction* */
        GList* tokens = TransactionGetTokens(info);

        /* try to find the destination account for this transaction from its tokens */
        result = gnc_account_imap_find_account_bayes(orig_acc, tokens);

    }
    else
        result = gnc_account_imap_find_account
                 (orig_acc, GNCIMPORT_DESC,
                  xaccTransGetDescription (gnc_import_TransInfo_get_trans (info)));

    return result;
}

/** Store the destination account from trans_info in the matchmap. If
    'use_match' is true, the destination account of the selected
    matching/duplicate transaction is used; otherwise, the stored
    destination_acc pointer is used. */
static void
matchmap_store_destination (Account *base_acc,
                            GNCImportTransInfo *trans_info,
                            gboolean use_match)
{
    g_assert (trans_info);

    /* This will store the destination account of the selected match if
       the reconcile match selected has only two splits. */
    Account *dest = nullptr;
    if (use_match)
        dest = xaccSplitGetAccount
            (xaccSplitGetOtherSplit
             (gnc_import_MatchInfo_get_split
              (gnc_import_TransInfo_get_selected_match (trans_info))));
    else
        dest = gnc_import_TransInfo_get_destacc (trans_info);
    if (!dest)
        return;

    auto orig_acc = (base_acc ? base_acc : xaccSplitGetAccount
                            (gnc_import_TransInfo_get_fsplit (trans_info)));

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_IMPORT, GNC_PREF_USE_BAYES))
    {
        /* tokenize this transaction */
        auto tokens = TransactionGetTokens(trans_info);

        /* add the tokens to the imap with the given destination account */
        gnc_account_imap_add_account_bayes(orig_acc, tokens, dest);
    }
    else
    {
        auto desc = xaccTransGetDescription
                            (gnc_import_TransInfo_get_trans (trans_info));
        auto memo = xaccSplitGetMemo
                            (gnc_import_TransInfo_get_fsplit (trans_info));

        if (desc && *desc)
            gnc_account_imap_add_account (orig_acc, GNCIMPORT_DESC, desc, dest);
        if (memo && *memo)
            gnc_account_imap_add_account (orig_acc, GNCIMPORT_MEMO, memo, dest);
    }
}



/** @brief The transaction matching heuristics are here.
 */
void split_find_match (GNCImportTransInfo * trans_info,
                       Split * split,
                       gint display_threshold,
                       gint date_threshold,
                       gint date_not_threshold,
                       double fuzzy_amount_difference)
{
    gint prob = 0;

    auto new_trans = gnc_import_TransInfo_get_trans (trans_info);
    auto new_trans_fsplit = gnc_import_TransInfo_get_fsplit (trans_info);

    /* Matching heuristics */

    /* Amount heuristics */
    auto downloaded_split_amount =
        gnc_numeric_to_double (xaccSplitGetAmount(new_trans_fsplit));
    /*DEBUG(" downloaded_split_amount=%f", downloaded_split_amount);*/
    auto match_split_amount = gnc_numeric_to_double(xaccSplitGetAmount(split));
    /*DEBUG(" match_split_amount=%f", match_split_amount);*/
    if (fabs(downloaded_split_amount - match_split_amount) < 1e-6)
        /* bug#347791: Double type shouldn't be compared for exact
            equality, so we're using fabs() instead. */
        /*if (gnc_numeric_equal(xaccSplitGetAmount
            (new_trans_fsplit),
            xaccSplitGetAmount(split)))
            -- gnc_numeric_equal is an expensive function call */
    {
        prob = prob + 3;
        /*DEBUG("heuristics:  probability + 3 (amount)");*/
    }
    else if (fabs (downloaded_split_amount - match_split_amount) <=
                fuzzy_amount_difference)
    {
        /* ATM fees are sometimes added directly in the transaction.
            So you withdraw 100$ and get charged 101,25$ in the same
            transaction */
        prob = prob + 2;
        /*DEBUG("heuristics:  probability + 2 (amount)");*/
    }
    else
    {
        /* If a transaction's amount doesn't match within the
            threshold, it's very unlikely to be the same transaction
            so we give it an extra -5 penalty */
        prob = prob - 5;
        /* DEBUG("heuristics:  probability - 1 (amount)"); */
    }

    /* Date heuristics */
    auto match_time = xaccTransGetDate (xaccSplitGetParent (split));
    auto download_time = xaccTransGetDate (new_trans);
    auto datediff_day = llabs(match_time - download_time) / 86400;
    /* Sorry, there are not really functions around at all that
                provide for less hacky calculation of days of date
                differences. Whatever. On the other hand, the difference
                calculation itself will work regardless of month/year
                turnarounds. */
    /*DEBUG("diff day %d", datediff_day);*/
    if (datediff_day == 0)
    {
        prob = prob + 3;
        /*DEBUG("heuristics:  probability + 3 (date)");*/
    }
    else if (datediff_day <= date_threshold)
    {
        prob = prob + 2;
        /*DEBUG("heuristics:  probability + 2 (date)");*/
    }
    else if (datediff_day > date_not_threshold)
    {
        /* Extra penalty if that split lies awfully far away from
            the given one. */
        prob = prob - 5;
        /*DEBUG("heuristics:  probability - 5 (date)"); */
        /* Changed 2005-02-21: Revert the hard-limiting behaviour
            back to the previous large penalty. (Changed 2004-11-27:
            The penalty is so high that we can forget about this
            split anyway and skip the rest of the tests.) */
    }

    /* Check if date and amount are identical */
    auto update_proposed = (prob < 6);

    /* Check number heuristics */
    auto new_trans_str = gnc_get_num_action(new_trans, new_trans_fsplit);
    if (new_trans_str && *new_trans_str)
    {
        char *endptr;
        auto conversion_ok = true;

        /* To distinguish success/failure after strtol call */
        errno = 0;
        auto new_trans_number = strtol(new_trans_str, &endptr, 10);
        /* Possible addressed problems: over/underflow, only non
                        numbers on string and string empty */
        conversion_ok = !(errno || endptr == new_trans_str);

        auto split_str = gnc_get_num_action (xaccSplitGetParent (split), split);
        errno = 0;
        auto split_number = strtol(split_str, &endptr, 10);
        conversion_ok =  !(errno || endptr == split_str);

        if ( (conversion_ok && (split_number == new_trans_number)) ||
                (g_strcmp0(new_trans_str, split_str) == 0) )
        {
            /* An exact match of the Check number gives a +4 */
            prob += 4;
            /*DEBUG("heuristics:  probability + 4 (Check number)");*/
        }
        else if (strlen(new_trans_str) > 0 && strlen(split_str) > 0)
        {
            /* If both number are not empty yet do not match, add a
                            little extra penalty */
            prob -= 2;
        }
    }

    /* Memo heuristics */
    auto memo = xaccSplitGetMemo(new_trans_fsplit);
    if (memo && *memo)
    {
        if (safe_strcasecmp(memo, xaccSplitGetMemo(split)) == 0)
        {
            /* An exact match of memo gives a +2 */
            prob = prob + 2;
            /* DEBUG("heuristics:  probability + 2 (memo)"); */
        }
        else if ((strncasecmp(memo, xaccSplitGetMemo(split),
                    strlen(xaccSplitGetMemo(split)) / 2) == 0))
        {
            /* Very primitive fuzzy match worth +1.  This matches the
                            first 50% of the strings to skip annoying transaction
                            number some banks seem to include in the memo but someone
                            should write something more sophisticated */
            prob = prob + 1;
            /*DEBUG("heuristics:  probability + 1 (memo)");	*/
        }
    }

    /* Description heuristics */
    auto descr = xaccTransGetDescription(new_trans);
    if (descr && *descr)
    {
        if (safe_strcasecmp(descr,
                xaccTransGetDescription(xaccSplitGetParent(split))) == 0)
        {
            /*An exact match of Description gives a +2 */
            prob = prob + 2;
            /*DEBUG("heuristics:  probability + 2 (description)");*/
        }
        else if ((strncasecmp(descr,
                    xaccTransGetDescription (xaccSplitGetParent(split)),
                    strlen(xaccTransGetDescription (new_trans)) / 2) == 0))
        {
            /* Very primitive fuzzy match worth +1.  This matches the
                            first 50% of the strings to skip annoying transaction
                            number some banks seem to include in the description but someone
                            should write something more sophisticated */
            prob = prob + 1;
            /*DEBUG("heuristics:  probability + 1 (description)");	*/
        }
    }

    /* Is the probability high enough? Otherwise do nothing and return. */
    if (prob < display_threshold)
        return;

    /* The probability is high enough, so allocate an object
                here. Allocating it only when it's actually being used is
                probably quite some performance gain. */
    auto match_info = g_new0(GNCImportMatchInfo, 1);

    match_info->probability = prob;
    match_info->update_proposed = update_proposed;
    match_info->split = split;
    match_info->trans = xaccSplitGetParent(split);


    /* Append that to the list. Do not use g_list_append because
                it is slow. The list is sorted afterwards anyway. */
    trans_info->match_list = g_list_prepend(trans_info->match_list, match_info);
}

/***********************************************************************
 */

static char*
maybe_append_string (const char* match_string, const char* imp_string)
{
    if (!(match_string && *match_string))
        return g_strdup(imp_string);

    if (!(imp_string && *imp_string))
        return nullptr;

    auto norm_match_string = g_utf8_normalize (match_string, -1, G_NORMALIZE_NFC);
    auto norm_imp_string = g_utf8_normalize (imp_string, -1, G_NORMALIZE_NFC);

    char *retval = nullptr;
    if (g_utf8_strlen (norm_imp_string, -1) > g_utf8_strlen (norm_match_string, -1) ||
         !strstr (norm_match_string, norm_imp_string))
        retval = g_strconcat(match_string, "|", imp_string, nullptr);

    g_free (norm_match_string);
    g_free (norm_imp_string);
    return retval;

}

/* Append or replace transaction description and notes
 * depending on the Append checkbox
 */
static void
update_desc_and_notes (const GNCImportTransInfo* trans_info)
{
    auto selected_match = gnc_import_TransInfo_get_selected_match (trans_info);
    auto imp_trans = gnc_import_TransInfo_get_trans (trans_info);

    if (trans_info->append_text)
    {
        auto match_trans = selected_match->trans;
        auto repl_str =
            maybe_append_string (xaccTransGetDescription(match_trans),
                                 xaccTransGetDescription(imp_trans));
        if (repl_str)
            xaccTransSetDescription(match_trans, repl_str);
        g_free (repl_str);

        repl_str =
            maybe_append_string (xaccTransGetNotes(match_trans),
                                 xaccTransGetNotes(imp_trans));
        if (repl_str)
            xaccTransSetNotes (match_trans, repl_str);
        g_free (repl_str);
    }
    else
    {
        xaccTransSetDescription (selected_match->trans,
                                 xaccTransGetDescription (imp_trans));
        xaccTransSetNotes (selected_match->trans,
                           xaccTransGetNotes (imp_trans));
    }
}

static void
process_reconcile(Account *base_acc,
                  GNCImportTransInfo *trans_info,
                  GNCImportMatchInfo *selected_match)
{
    /* Reconcile the matching transaction */
    /*DEBUG("BeginEdit selected_match")*/
    xaccTransBeginEdit(selected_match->trans);

    if (xaccSplitGetReconcile(selected_match->split) == NREC)
        xaccSplitSetReconcile(selected_match->split, CREC);

    /* Set reconcile date to today */
    xaccSplitSetDateReconciledSecs(selected_match->split, gnc_time (nullptr));

    /* Copy the online id to the reconciled transaction, so
     *      the match will be remembered */
    auto online_id = gnc_import_get_split_online_id(trans_info->first_split);
    if (online_id && *online_id)
        gnc_import_set_split_online_id(selected_match->split, online_id);

    g_free (online_id);

    /* Done editing. */
    /*DEBUG("CommitEdit selected_match")*/
    xaccTransCommitEdit(selected_match->trans);

    /* Store the mapping to the other account in the MatchMap. */
    matchmap_store_destination(base_acc, trans_info, true);

    /* Erase the downloaded transaction */
    xaccTransDestroy(trans_info->trans);
    /*DEBUG("CommitEdit trans")*/
    xaccTransCommitEdit(trans_info->trans);
    /* Very important: Make sure the freed transaction is not freed again! */
    trans_info->trans = nullptr;
}

/** /brief -- Processes one match
   according to its selected action.  */
gboolean
gnc_import_process_trans_item (Account *base_acc,
                               GNCImportTransInfo *trans_info)
{
    g_assert (trans_info);
    /*DEBUG("Iteration %d, action %d, split %s", i,
    	trans_info->action,
    	xaccTransGetDescription (gnc_import_TransInfo_get_trans
    	(trans_info)))*/
    switch (gnc_import_TransInfo_get_action (trans_info))
    {
    case GNCImport_SKIP:
        return false;
    case GNCImport_ADD:
        /* Transaction gets imported. */
        if (!gnc_import_TransInfo_is_balanced(trans_info)
                && gnc_import_TransInfo_get_destacc(trans_info))
        {
            /* Create the 'other' split. */
            auto trans = gnc_import_TransInfo_get_trans (trans_info);
            auto acct = gnc_import_TransInfo_get_destacc (trans_info);
            auto split = xaccMallocSplit (gnc_account_get_book (acct));
            xaccTransAppendSplit (trans, split);
            xaccAccountInsertSplit (acct, split);
            xaccSplitSetValue (split, trans_info->lsplit_value);
            if (!gnc_numeric_zero_p (trans_info->lsplit_amount))
                xaccSplitSetAmount (split, trans_info->lsplit_amount);
            else
            {
                /* Bad! user asked to create a balancing split in an account with
                 * different currency/commodit than the transaction but didn't provide
                 * an exchange rate.
                 * Continue anyway pretenting split is in transaction currency. */
                xaccSplitSetAmount (split, trans_info->lsplit_value);
                PWARN("Missing exchange rate while adding transaction '%s', will assume rate of 1",
                      xaccTransGetDescription (gnc_import_TransInfo_get_trans (trans_info)));
            }
        }

        xaccSplitSetReconcile(gnc_import_TransInfo_get_fsplit (trans_info), CREC);
        /*Set reconcile date to today*/
        xaccSplitSetDateReconciledSecs(gnc_import_TransInfo_get_fsplit (trans_info),
                                       gnc_time (nullptr));
        /* Done editing. */
        xaccTransCommitEdit(trans_info->trans);
        xaccTransRecordPrice(trans_info->trans, PRICE_SOURCE_SPLIT_IMPORT);
        return true;
    case GNCImport_UPDATE:
        {
            auto selected_match = gnc_import_TransInfo_get_selected_match(trans_info);

            /* If there is no selection, ignore this transaction. */
            if (!selected_match)
            {
                PWARN("No matching transaction to be cleared was chosen. Imported transaction will be ignored.");
                break;
            }

            /* Transaction gets not imported but the matching one gets
            updated and reconciled. */
            if (!gnc_import_MatchInfo_get_split(selected_match))
                PERR("The split I am trying to update and reconcile is nullptr, shouldn't happen!");
            else
            {
                /* Update and reconcile the matching transaction */
                /*DEBUG("BeginEdit selected_match")*/
                xaccTransBeginEdit(selected_match->trans);

                auto fsplit = gnc_import_TransInfo_get_fsplit(trans_info);
                xaccTransSetDatePostedSecsNormalized(selected_match->trans,
                                    xaccTransGetDate(xaccSplitGetParent(fsplit)));

                auto match_split_amount = xaccSplitGetAmount(selected_match->split);
                xaccSplitSetAmount(selected_match->split, xaccSplitGetAmount(fsplit));
                xaccSplitSetValue(selected_match->split, xaccSplitGetValue(fsplit));

                auto imbalance_value = gnc_import_TransInfo_get_dest_value(trans_info);
                auto other_split = xaccSplitGetOtherSplit(selected_match->split);
                if (!gnc_numeric_zero_p(imbalance_value) && other_split)
                {
                    if (xaccSplitGetReconcile(other_split) == NREC)
                    {
                        xaccSplitSetValue(other_split, imbalance_value);
                        auto new_amt = gnc_import_TransInfo_get_dest_value(trans_info);
                        if (gnc_numeric_zero_p(new_amt))
                        {
                            auto other_split_amount = xaccSplitGetAmount(other_split);
                            auto price = gnc_numeric_div(match_split_amount, other_split_amount,
                                                         GNC_DENOM_AUTO,
                                                         GNC_HOW_RND_ROUND_HALF_UP);

                            new_amt = gnc_numeric_mul(xaccSplitGetAmount(fsplit), price,
                                                      GNC_DENOM_AUTO,
                                                      GNC_HOW_RND_ROUND_HALF_UP);;
                        }
                        xaccSplitSetAmount(other_split, new_amt);
                    }
                    else
                    {
                        /* else GC will automatically insert a split to equity
                        to balance the transaction */
                        PWARN("Updated transaction '%s', but not other split.",
                              xaccTransGetDescription(selected_match->trans));
                    }
                }

                auto fs_memo = xaccSplitGetMemo (trans_info->first_split);
                if (fs_memo && *fs_memo)
                    xaccSplitSetMemo(selected_match->split, fs_memo);

                update_desc_and_notes(trans_info);

                /*DEBUG("CommitEdit selected_match")*/
                xaccTransCommitEdit(selected_match->trans);

                process_reconcile (base_acc, trans_info, selected_match);
            }
        }
        return true;
    case GNCImport_CLEAR:
        {
            auto selected_match = gnc_import_TransInfo_get_selected_match (trans_info);

            /* If there is no selection, ignore this transaction. */
            if (!selected_match)
            {
                PWARN("No matching translaction to be cleared was chosen. Imported transaction will be ignored.");
                break;
            }

            /* Transaction gets not imported but the matching one gets
            reconciled. */
            if (!gnc_import_MatchInfo_get_split (selected_match))
                PERR("The split I am trying to reconcile is nullptr, shouldn't happen!");
            else
            {
                /* Reconcile the matching transaction */
                process_reconcile(base_acc, trans_info, selected_match);
            }
        }
        return true;
    default:
        DEBUG("Invalid GNCImportAction for this imported transaction.");
        break;
    }
    /*DEBUG("End");*/
    return false;
}

static GHashTable*
hash_account_online_ids (Account *account)
{
     auto acct_hash = g_hash_table_new_full
          (g_str_hash, g_str_equal, g_free, nullptr);
     for (GList *n = xaccAccountGetSplitList (account) ; n; n = n->next)
     {
        auto id = gnc_import_get_split_online_id (static_cast<Split *>(n->data));
        if (id && *id)
            g_hash_table_insert (acct_hash, (void*) id, GINT_TO_POINTER (1));
     }
     return acct_hash;
}

/** Checks whether the given transaction's online_id already exists in
  its parent account. */
gboolean gnc_import_exists_online_id (Transaction *trans, GHashTable* acct_id_hash)
{

    /* Look for an online_id in the first split */
    auto source_split = xaccTransGetSplit(trans, 0);
    g_assert(source_split);

    auto source_online_id = gnc_import_get_split_online_id (source_split);

    // No online id, no point in continuing. We'd crash if we tried.
    if (!source_online_id)
        return false;

    // Create a hash per account of a hash of all split IDs. Then the
    // test below will be fast if we have many transactions to import.
    auto dest_acct = xaccSplitGetAccount (source_split);

    auto online_id_hash = static_cast<GHashTable*>(g_hash_table_lookup (acct_id_hash, dest_acct));

    if (!online_id_hash)
    {
        online_id_hash = hash_account_online_ids (dest_acct);
        g_hash_table_insert (acct_id_hash, dest_acct, online_id_hash);
    }

    auto online_id_exists = g_hash_table_contains (online_id_hash, source_online_id);
    
    /* If it does, abort the process for this transaction, since it is
       already in the system. */
    if (online_id_exists)
    {
        DEBUG("%s", "Transaction with same online ID exists, destroying current transaction");
        xaccTransDestroy(trans);
        xaccTransCommitEdit(trans);
    }
    g_free (source_online_id);
    return online_id_exists;
}


/* ******************************************************************
 */

/* Calculate lsplit_amount based on knowledge gathered so far
 * If insufficient info is available (eg multi currency transaction with missing
 * exchange rate provided), set amount to 0 */
static void trans_info_calculate_dest_amount (GNCImportTransInfo *info)
{
    info->lsplit_value = gnc_numeric_neg (xaccTransGetImbalanceValue (info->trans));
    if (!info->lsplit_amount_selected_manually)
        info->lsplit_amount = {0, 1};

    if (info->dest_acc)
    {
        auto tcurr = xaccTransGetCurrency(info->trans);
        auto dcurr = xaccAccountGetCommodity(info->dest_acc);

        if (gnc_numeric_zero_p(info->lsplit_value))
            return;

        if (gnc_commodity_equiv(tcurr, dcurr))
            info->lsplit_amount = info->lsplit_value;
        else if (info->lsplit_amount_selected_manually &&
                 gnc_numeric_check(info->lsplit_amount) == 0)
        {
            /* Nothing to do, user has provided amount already */
        }
        else if (gnc_numeric_check(info->lsplit_price) == 0)
        {
            /* We are in a multi currency situation and have a valid price
             * Reminder: value = amount * price => amount = value / price */
            gnc_numeric inv_price = gnc_numeric_invert (info->lsplit_price);
            info->lsplit_amount = gnc_numeric_mul (info->lsplit_value,
                                                   inv_price,
                                                   GNC_DENOM_AUTO,
                                                   GNC_HOW_RND_ROUND_HALF_UP);
        }
    }
}

/** Create a new object of GNCImportTransInfo here. */
GNCImportTransInfo *
gnc_import_TransInfo_new (Transaction *trans, Account *base_acc)
{
    g_assert (trans);

    auto t_info = g_new0(GNCImportTransInfo, 1);

    t_info->trans = trans;
    /* Only use first split, the source split */
    auto split = xaccTransGetSplit(trans, 0);
    g_assert(split);
    t_info->first_split = split;

    /* Try to find a previously selected destination account
       string match for the ADD action */
    gnc_import_TransInfo_set_destacc (t_info,
                                      matchmap_find_destination (base_acc, t_info),
                                      false);

    return t_info;
}


/** compare_probability() is used by g_list_sort to sort by probability */
static gint compare_probability (gconstpointer a,
                                 gconstpointer b)
{
    return(((GNCImportMatchInfo *)b)->probability -
           ((GNCImportMatchInfo *)a)->probability);
}

/** Iterates through all splits of trans_info's originating account
 * match list. Sorts the resulting list and sets the selected_match
 * and action fields in the trans_info.
 */
void
gnc_import_TransInfo_init_matches (GNCImportTransInfo *trans_info,
                                   GNCImportSettings *settings)
{
    g_assert (trans_info);

    if (trans_info->match_list)
    {
        trans_info->match_list = g_list_sort(trans_info->match_list,
                                             compare_probability);
        auto best_match = static_cast<GNCImportMatchInfo*>(g_list_nth_data(trans_info->match_list, 0));
        gnc_import_TransInfo_set_selected_match_info (trans_info, best_match, false);
        if (best_match &&
                best_match->probability >= gnc_import_Settings_get_clear_threshold(settings))
        {
            if (gnc_import_Settings_get_action_update_enabled(settings) &&
                best_match->update_proposed)
                trans_info->action = GNCImport_UPDATE;
            else
                trans_info->action = GNCImport_CLEAR;
        }
        else if (!best_match ||
                 best_match->probability <= gnc_import_Settings_get_add_threshold(settings))
            trans_info->action = GNCImport_ADD;
        else if (gnc_import_Settings_get_action_skip_enabled(settings))
            trans_info->action = GNCImport_SKIP;
        else if (gnc_import_Settings_get_action_update_enabled(settings))
            trans_info->action = GNCImport_UPDATE;
        else
            trans_info->action = GNCImport_ADD;
    }
    else
        trans_info->action = GNCImport_ADD;


    trans_info->previous_action = trans_info->action;
}


/** @} */
