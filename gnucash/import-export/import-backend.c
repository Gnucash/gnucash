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

#define GNCIMPORT_DESC    "desc"
#define GNCIMPORT_MEMO    "memo"
#define GNCIMPORT_PAYEE    "payee"

/********************************************************************\
 *   Constants                                                      *
\********************************************************************/

static QofLogModule log_module = GNC_MOD_IMPORT;

/********************************************************************\
 *   Constants, should ideally be defined a user preference dialog  *
\********************************************************************/

static const int MATCH_DATE_THRESHOLD = 4; /*within 4 days*/
static const int MATCH_DATE_NOT_THRESHOLD = 14;

/********************************************************************\
 *   Forward declared prototypes                                    *
\********************************************************************/

static void
matchmap_store_destination (GncImportMatchMap *matchmap,
                            GNCImportTransInfo *trans_info,
                            gboolean use_match);


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
    {
        info->selected_match_info.selected_match = match_list->data;
    }
    else
    {
        info->selected_match_info.selected_match = NULL;
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
    /* Assume that the importer won't create a transaction that involves two or more
       currencies and no non-currency commodity.  In that case can use the simpler
       value imbalance check. */
    if (gnc_numeric_zero_p(xaccTransGetImbalanceValue(gnc_import_TransInfo_get_trans(info))))
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
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
    {
        matchmap_store_destination (NULL, info, FALSE);
    }
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
    {
        return info->probability;
    }
    else
    {
        return 0;
    }
}

void gnc_import_TransInfo_delete (GNCImportTransInfo *info)
{
    if (info)
    {
        g_list_free (info->match_list);
        /*If the transaction exists and is still open, it must be destroyed*/
        if (info->trans && xaccTransIsOpen(info->trans))
        {
            xaccTransDestroy(info->trans);
            xaccTransCommitEdit(info->trans);
        }
        if (info->match_tokens)
        {
            GList *node;

            for (node = info->match_tokens; node; node = node->next)
                g_free (node->data);

            g_list_free (info->match_tokens);
        }
        g_free(info);
    }
}

GdkPixbuf* gen_probability_pixbuf(gint score_original, GNCImportSettings *settings, GtkWidget * widget)
{
    GdkPixbuf* retval = NULL;
    gint i, j;
    gint score;
    const gint height = 15;
    const gint width_each_bar = 7;
    gchar * green_bar = ("bggggb ");
    gchar * yellow_bar = ("byyyyb ");
    gchar * red_bar = ("brrrrb ");
    gchar * black_bar = ("bbbbbb ");
    const gint width_first_bar = 1;
    gchar * black_first_bar = ("b");
    const gint num_colors = 5;
    gchar * size_str;
    gchar * none_color_str = g_strdup_printf("  c None");
    gchar * green_color_str = g_strdup_printf("g c green");
    gchar * yellow_color_str = g_strdup_printf("y c yellow");
    gchar * red_color_str = g_strdup_printf("r c red");
    gchar * black_color_str = g_strdup_printf("b c black");
    gchar * xpm[2+num_colors+height];
    gint add_threshold, clear_threshold;

    g_assert(settings);
    g_assert(widget);
    if (score_original < 0)
    {
        score = 0;
    }
    else
    {
        score = score_original;
    }
    size_str = g_strdup_printf("%d%s%d%s%d%s", (width_each_bar * score) + width_first_bar/*width*/, " ", height, " ", num_colors, " 1"/*characters per pixel*/);

    /*DEBUG("Begin");*/
    xpm[0] = size_str;
    xpm[1] = none_color_str;
    xpm[2] = green_color_str;
    xpm[3] = yellow_color_str;
    xpm[4] = red_color_str;
    xpm[5] = black_color_str;
    add_threshold = gnc_import_Settings_get_add_threshold(settings);
    clear_threshold = gnc_import_Settings_get_clear_threshold(settings);

    for (i = 0; i < height; i++)
    {
        xpm[num_colors+1+i] = g_new0(char, (width_each_bar * score) + width_first_bar + 1);
        for (j = 0; j <= score; j++)
        {
            if (i == 0 || i == height - 1)
            {
                if (j == 0)
                {
                    strcat(xpm[num_colors+1+i], black_first_bar);
                }
                else
                {
                    strcat(xpm[num_colors+1+i], black_bar);
                }
            }
            else
            {
                if (j == 0)
                {
                    strcat(xpm[num_colors+1+i], black_first_bar);
                }
                else if (j <= add_threshold)
                {
                    strcat(xpm[num_colors+1+i], red_bar);
                }
                else if (j >= clear_threshold)
                {
                    strcat(xpm[num_colors+1+i], green_bar);
                }
                else
                {
                    strcat(xpm[num_colors+1+i], yellow_bar);
                }
            }
        }
    }

    retval =  gdk_pixbuf_new_from_xpm_data((const gchar **)xpm);
    for (i = 0; i <= num_colors + height; i++)
    {
        /*DEBUG("free_loop i=%d%s%s",i,": ",xpm[i]);*/
        g_free(xpm[i]);
    }

    return retval;
}

/*************************************************************************
 * MatchMap- related functions (storing and retrieving)
 */

/* Tokenize a string and append to an existing GList(or an empty GList)
 * the tokens
 */
static GList*
tokenize_string(GList* existing_tokens, const char *string)
{
    char **tokenized_strings; /* array of strings returned by g_strsplit() */
    char **stringpos;

    tokenized_strings = g_strsplit(string, " ", 0);
    stringpos = tokenized_strings;

    /* add each token to the token GList */
    while (stringpos && *stringpos)
    {
        if (strlen(*stringpos) > 0)
        {
            /* check for duplicated tokens */
            gboolean duplicated = FALSE;
            for (GList* token = existing_tokens; token != NULL; token = token->next)
            {
                if (g_strcmp0(token->data, *stringpos) == 0)
                {
                    duplicated = TRUE;
                    break;
                }
            }
            if (duplicated == FALSE)
            {
                /* prepend the char* to the token GList */
                existing_tokens = g_list_prepend(existing_tokens, g_strdup(*stringpos));
            }
        }

        /* then move to the next string */
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
    Transaction* transaction;
    GList* tokens;
    const char* text;
    time64 transtime;
    struct tm *tm_struct;
    char local_day_of_week[16];

    g_return_val_if_fail (info, NULL);
    if (info->match_tokens) return info->match_tokens;

    transaction = gnc_import_TransInfo_get_trans(info);
    g_assert(transaction);

    tokens = 0; /* start off with an empty list */

    /* make tokens from the transaction description */
    text = xaccTransGetDescription(transaction);
    tokens = tokenize_string(tokens, text);

    /* The day of week the transaction occurred is a good indicator of
     * what account this transaction belongs in.  Get the date and covert
     * it to day of week as a token
     */
    transtime = xaccTransGetDate(transaction);
    tm_struct = gnc_gmtime(&transtime);
    if (!qof_strftime(local_day_of_week, sizeof(local_day_of_week), "%A", tm_struct))
    {
        PERR("TransactionGetTokens: error, strftime failed\n");
    }
    gnc_tm_free (tm_struct);
    /* we cannot add a locally allocated string to this array, dup it so
     * it frees the same way the rest do
     */
    tokens = g_list_prepend(tokens, g_strdup(local_day_of_week));

    /* make tokens from the memo of each split of this transaction */
    for (GList *split=xaccTransGetSplitList (transaction); split; split=split->next)
    {
        text = xaccSplitGetMemo(split->data);
        tokens = tokenize_string(tokens, text);
    }

    /* remember the list of tokens for later.. */
    info->match_tokens = tokens;

    /* return the pointer to the GList */
    return tokens;
}
/* Destroy an import map. But all stored entries will still continue
 * to exist in the underlying kvp frame of the account.
 */
static void
gnc_imap_destroy (GncImportMatchMap *imap)
{
    if (!imap) return;
    g_free (imap);
}

/* searches using the GNCImportTransInfo through all existing transactions
 * if there is an exact match of the description and memo
 */
static Account *
matchmap_find_destination (GncImportMatchMap *matchmap, GNCImportTransInfo *info)
{
    GncImportMatchMap *tmp_map;
    Account *result;
    GList* tokens;
    gboolean useBayes;

    g_assert (info);
    tmp_map = ((matchmap != NULL) ? matchmap :
               gnc_account_imap_create_imap
               (xaccSplitGetAccount
                (gnc_import_TransInfo_get_fsplit (info))));

    useBayes = gnc_prefs_get_bool (GNC_PREFS_GROUP_IMPORT, GNC_PREF_USE_BAYES);
    if (useBayes)
    {
        /* get the tokens for this transaction* */
        tokens = TransactionGetTokens(info);

        /* try to find the destination account for this transaction from its tokens */
        result = gnc_account_imap_find_account_bayes(tmp_map, tokens);

    }
    else
    {
        /* old system of transaction to account matching */
        result = gnc_account_imap_find_account
                 (tmp_map, GNCIMPORT_DESC,
                  xaccTransGetDescription (gnc_import_TransInfo_get_trans (info)));
    }

    /* Disable matching by memo, until bayesian filtering is implemented.
     * It's currently unlikely to help, and has adverse effects,
     * causing false positives, since very often the type of the
     * transaction is stored there.

       if (result == NULL)
       result = gnc_account_imap_find_account
       (tmp_map, GNCIMPORT_MEMO,
       xaccSplitGetMemo (gnc_import_TransInfo_get_fsplit (info)));
    */

    if (matchmap == NULL)
        gnc_imap_destroy (tmp_map);

    return result;
}

/** Store the destination account from trans_info in the matchmap. If
    'use_match' is true, the destination account of the selected
    matching/duplicate transaction is used; otherwise, the stored
    destination_acc pointer is used. */
static void
matchmap_store_destination (GncImportMatchMap *matchmap,
                            GNCImportTransInfo *trans_info,
                            gboolean use_match)
{
    GncImportMatchMap *tmp_matchmap = NULL;
    Account *dest;
    const char *descr, *memo;
    GList *tokens;
    gboolean useBayes;

    g_assert (trans_info);

    /* This will store the destination account of the selected match if
       the reconcile match selected has only two splits.  Good idea
       Christian! */
    dest = ((use_match) ?
            xaccSplitGetAccount
            (xaccSplitGetOtherSplit
             (gnc_import_MatchInfo_get_split
              (gnc_import_TransInfo_get_selected_match (trans_info)))) :
            gnc_import_TransInfo_get_destacc (trans_info));
    if (dest == NULL)
        return;

    tmp_matchmap = ((matchmap != NULL) ?
                    matchmap :
                    gnc_account_imap_create_imap
                    (xaccSplitGetAccount
                     (gnc_import_TransInfo_get_fsplit (trans_info))));

    /* see what matching system we are currently using */
    useBayes = gnc_prefs_get_bool (GNC_PREFS_GROUP_IMPORT, GNC_PREF_USE_BAYES);
    if (useBayes)
    {
        /* tokenize this transaction */
        tokens = TransactionGetTokens(trans_info);

        /* add the tokens to the imap with the given destination account */
        gnc_account_imap_add_account_bayes(tmp_matchmap, tokens, dest);

    }
    else
    {
        /* old matching system */
        descr = xaccTransGetDescription
                (gnc_import_TransInfo_get_trans (trans_info));
        if (descr && (strlen (descr) > 0))
            gnc_account_imap_add_account (tmp_matchmap,
                                  GNCIMPORT_DESC,
                                  descr,
                                  dest);
        memo = xaccSplitGetMemo
               (gnc_import_TransInfo_get_fsplit (trans_info));
        if (memo && (strlen (memo) > 0))
            gnc_account_imap_add_account (tmp_matchmap,
                                  GNCIMPORT_MEMO,
                                  memo,
                                  dest);
    } /* if(useBayes) */

    if (matchmap == NULL)
        gnc_imap_destroy (tmp_matchmap);
}



/** @brief The transaction matching heuristics are here.
 */
void split_find_match (GNCImportTransInfo * trans_info,
                              Split * split,
                              gint display_threshold,
                              double fuzzy_amount_difference)
{
    /* DEBUG("Begin"); */

    /*Ignore the split if the transaction is open for edit, meaning it
      was just downloaded. */
    if (xaccTransIsOpen(xaccSplitGetParent(split)) == FALSE)
    {
        GNCImportMatchInfo * match_info;
        gint prob = 0;
        gboolean update_proposed;
        double downloaded_split_amount, match_split_amount;
        time64 match_time, download_time;
        int datediff_day;
        Transaction *new_trans = gnc_import_TransInfo_get_trans (trans_info);
        Split *new_trans_fsplit = gnc_import_TransInfo_get_fsplit (trans_info);

        /* Matching heuristics */

        /* Amount heuristics */
        downloaded_split_amount =
            gnc_numeric_to_double (xaccSplitGetAmount(new_trans_fsplit));
        /*DEBUG(" downloaded_split_amount=%f", downloaded_split_amount);*/
        match_split_amount = gnc_numeric_to_double(xaccSplitGetAmount(split));
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
        match_time = xaccTransGetDate (xaccSplitGetParent (split));
        download_time = xaccTransGetDate (new_trans);
        datediff_day = llabs(match_time - download_time) / 86400;
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
        else if (datediff_day <= MATCH_DATE_THRESHOLD)
        {
            prob = prob + 2;
            /*DEBUG("heuristics:  probability + 2 (date)");*/
        }
        else if (datediff_day > MATCH_DATE_NOT_THRESHOLD)
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
        update_proposed = (prob < 6);

        /* Check number heuristics */
        {
            const char *new_trans_str = gnc_get_num_action(new_trans, new_trans_fsplit);
            if (new_trans_str && strlen(new_trans_str) != 0)
            {
                long new_trans_number, split_number;
                const gchar *split_str;
                char *endptr;
                gboolean conversion_ok = TRUE;

                /* To distinguish success/failure after strtol call */
                errno = 0;
                new_trans_number = strtol(new_trans_str, &endptr, 10);
                /* Possible addressed problems: over/underflow, only non
                	     numbers on string and string empty */
                if (errno || endptr == new_trans_str)
                    conversion_ok = FALSE;

                split_str = gnc_get_num_action (xaccSplitGetParent (split), split);
                errno = 0;
                split_number = strtol(split_str, &endptr, 10);
                if (errno || endptr == split_str)
                    conversion_ok = FALSE;

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
        }

        /* Memo heuristics */
        {
            const char *memo = xaccSplitGetMemo(new_trans_fsplit);
            if (memo && strlen(memo) != 0)
            {
                if (safe_strcasecmp(memo, xaccSplitGetMemo(split)) == 0)
                {
                    /* An exact match of memo gives a +2 */
                    prob = prob + 2;
                    /* DEBUG("heuristics:  probability + 2 (memo)"); */
                }
                else if ((strncasecmp(memo, xaccSplitGetMemo(split),
                                      strlen(xaccSplitGetMemo(split)) / 2)
                          == 0))
                {
                    /* Very primitive fuzzy match worth +1.  This matches the
                    		 first 50% of the strings to skip annoying transaction
                    		 number some banks seem to include in the memo but someone
                    		 should write something more sophisticated */
                    prob = prob + 1;
                    /*DEBUG("heuristics:  probability + 1 (memo)");	*/
                }
            }
        }

        /* Description heuristics */
        {
            const char *descr = xaccTransGetDescription(new_trans);
            if (descr && strlen(descr) != 0)
            {
                if (safe_strcasecmp(descr,
                                    xaccTransGetDescription(xaccSplitGetParent(split)))
                        == 0)
                {
                    /*An exact match of Description gives a +2 */
                    prob = prob + 2;
                    /*DEBUG("heuristics:  probability + 2 (description)");*/
                }
                else if ((strncasecmp(descr,
                                      xaccTransGetDescription (xaccSplitGetParent(split)),
                                      strlen(xaccTransGetDescription (new_trans)) / 2)
                          == 0))
                {
                    /* Very primitive fuzzy match worth +1.  This matches the
                    		 first 50% of the strings to skip annoying transaction
                    		 number some banks seem to include in the memo but someone
                    		 should write something more sophisticated */
                    prob = prob + 1;
                    /*DEBUG("heuristics:  probability + 1 (description)");	*/
                }
            }
        }

        /* Is the probability high enough? Otherwise do nothing and return. */
        if (prob < display_threshold)
        {
            return;
        }

        /* The probability is high enough, so allocate an object
        	 here. Allocating it only when it's actually being used is
        	 probably quite some performance gain. */
        match_info = g_new0(GNCImportMatchInfo, 1);

        match_info->probability = prob;
        match_info->update_proposed = update_proposed;
        match_info->split = split;
        match_info->trans = xaccSplitGetParent(split);


        /* Append that to the list. Do not use g_list_append because
        	   it is slow. The list is sorted afterwards anyway. */
        trans_info->match_list =
            g_list_prepend(trans_info->match_list,
                           match_info);
    }
}/* end split_find_match */

/***********************************************************************
 */

/** /brief -- Processes one match
   according to its selected action.  */
gboolean
gnc_import_process_trans_item (GncImportMatchMap *matchmap,
                               GNCImportTransInfo *trans_info)
{
    Split * other_split;
    gnc_numeric imbalance_value;
    Transaction *trans;

    /* DEBUG("Begin"); */

    g_assert (trans_info);
    /*DEBUG("Iteration %d, action %d, split %s", i,
    	trans_info->action,
    	xaccTransGetDescription (gnc_import_TransInfo_get_trans
    	(trans_info)))*/
    switch (gnc_import_TransInfo_get_action (trans_info))
    {
    case GNCImport_SKIP:
        return FALSE;
    case GNCImport_ADD:
        /* Transaction gets imported. */

        /* Is the transaction not balanced and there is a non-NULL destination account? */
        if (gnc_import_TransInfo_is_balanced(trans_info) == FALSE
                && gnc_import_TransInfo_get_destacc(trans_info) != NULL)
        {
            /* Create the 'other' split. */
            Split *split =
                xaccMallocSplit
                (gnc_account_get_book
                 (gnc_import_TransInfo_get_destacc (trans_info)));
            xaccTransAppendSplit
            (gnc_import_TransInfo_get_trans (trans_info), split);
            xaccAccountInsertSplit
            (gnc_import_TransInfo_get_destacc (trans_info), split);
            /*xaccSplitSetBaseValue
              (split,
               gnc_numeric_neg(xaccTransGetImbalance
            	       (gnc_import_TransInfo_get_trans (trans_info))),
               xaccTransGetCurrency
               (gnc_import_TransInfo_get_trans (trans_info)));*/
            {
                /* This is a quick workaround for the bug described in
                		 http://lists.gnucash.org/pipermail/gnucash-devel/2003-August/009982.html
                       Assume that importers won't create transactions involving two or more
                       currencies so we can use xaccTransGetImbalanceValue. */
                imbalance_value =
                    gnc_numeric_neg (xaccTransGetImbalanceValue
                                     (gnc_import_TransInfo_get_trans (trans_info)));
                xaccSplitSetValue (split, imbalance_value);
                xaccSplitSetAmount (split, imbalance_value);
            }
            /*xaccSplitSetMemo (split, _("Auto-Balance split"));
              -- disabled due to popular request */
        }

        xaccSplitSetReconcile(gnc_import_TransInfo_get_fsplit (trans_info), CREC);
        /*Set reconcile date to today*/
        xaccSplitSetDateReconciledSecs(gnc_import_TransInfo_get_fsplit (trans_info),
                                       gnc_time (NULL));
        /* Done editing. */
        trans = gnc_import_TransInfo_get_trans (trans_info);
        xaccTransCommitEdit(trans);
        xaccTransRecordPrice(trans, PRICE_SOURCE_SPLIT_IMPORT);
        return TRUE;
    case GNCImport_UPDATE:
    {
        GNCImportMatchInfo *selected_match =
            gnc_import_TransInfo_get_selected_match(trans_info);

        /* If there is no selection, ignore this transaction. */
        if (!selected_match)
        {
            PWARN("No matching translaction to be cleared was chosen. Imported transaction will be ignored.");
            break;
        }

        /* Transaction gets not imported but the matching one gets
           updated and reconciled. */
        if (gnc_import_MatchInfo_get_split(selected_match) == NULL)
        {
            PERR("The split I am trying to update and reconcile is NULL, shouldn't happen!");
        }
        else
        {
            /* Update and reconcile the matching transaction */
            /*DEBUG("BeginEdit selected_match")*/
            xaccTransBeginEdit(selected_match->trans);

            xaccTransSetDatePostedSecsNormalized(selected_match->trans,
                                                 xaccTransGetDate(xaccSplitGetParent(
                                                         gnc_import_TransInfo_get_fsplit(trans_info))));

            xaccSplitSetAmount(selected_match->split,
                               xaccSplitGetAmount(
                                   gnc_import_TransInfo_get_fsplit(trans_info)));
            xaccSplitSetValue(selected_match->split,
                              xaccSplitGetValue(
                                  gnc_import_TransInfo_get_fsplit(trans_info)));

            imbalance_value = xaccTransGetImbalanceValue(
                                  gnc_import_TransInfo_get_trans(trans_info));
            other_split = xaccSplitGetOtherSplit(selected_match->split);
            if (!gnc_numeric_zero_p(imbalance_value) && other_split)
            {
                if (xaccSplitGetReconcile(other_split) == NREC)
                {
                    imbalance_value = gnc_numeric_neg(imbalance_value);
                    xaccSplitSetValue(other_split, imbalance_value);
                    xaccSplitSetAmount(other_split, imbalance_value);
                }
                /* else GC will automatically insert a split to equity
                   to balance the transaction */
            }

            xaccTransSetDescription(selected_match->trans,
                                    xaccTransGetDescription(
                                        gnc_import_TransInfo_get_trans(trans_info)));

            xaccTransSetNotes(selected_match->trans,
                                    xaccTransGetNotes(
                                        gnc_import_TransInfo_get_trans(trans_info)));

            if (xaccSplitGetReconcile(selected_match->split) == NREC)
            {
                xaccSplitSetReconcile(selected_match->split, CREC);
            }

            /* Set reconcile date to today */
            xaccSplitSetDateReconciledSecs(selected_match->split, gnc_time (NULL));

            /* Copy the online id to the reconciled transaction, so
               the match will be remembered */
            if (gnc_import_split_has_online_id(trans_info->first_split))
            {
                gnc_import_set_split_online_id(selected_match->split,
                                               gnc_import_get_split_online_id(trans_info->first_split));
            }

            /* Done editing. */
            /*DEBUG("CommitEdit selected_match")*/
            xaccTransCommitEdit(selected_match->trans);

            /* Store the mapping to the other account in the MatchMap. */
            matchmap_store_destination(matchmap, trans_info, TRUE);

            /* Erase the downloaded transaction */
            xaccTransDestroy(trans_info->trans);
            /*DEBUG("CommitEdit trans")*/
            xaccTransCommitEdit(trans_info->trans);
            /* Very important: Make sure the freed transaction is not freed again! */
            trans_info->trans = NULL;
        }
    }
    return TRUE;
    case GNCImport_CLEAR:
    {
        GNCImportMatchInfo *selected_match =
            gnc_import_TransInfo_get_selected_match (trans_info);

        /* If there is no selection, ignore this transaction. */
        if (!selected_match)
        {
            PWARN("No matching translaction to be cleared was chosen. Imported transaction will be ignored.");
            break;
        }

        /* Transaction gets not imported but the matching one gets
           reconciled. */
        if (gnc_import_MatchInfo_get_split (selected_match) == NULL)
        {
            PERR("The split I am trying to reconcile is NULL, shouldn't happen!");
        }
        else
        {
            /* Reconcile the matching transaction */
            /*DEBUG("BeginEdit selected_match")*/
            xaccTransBeginEdit(selected_match->trans);

            if (xaccSplitGetReconcile
                    (selected_match->split) == NREC)
                xaccSplitSetReconcile
                (selected_match->split, CREC);
            /* Set reconcile date to today */
            xaccSplitSetDateReconciledSecs
            (selected_match->split, gnc_time (NULL));

            /* Copy the online id to the reconciled transaction, so
            		 the match will be remembered */
            if (gnc_import_split_has_online_id(trans_info->first_split))
                gnc_import_set_split_online_id
                (selected_match->split,
                 gnc_import_get_split_online_id(trans_info->first_split));

            /* Done editing. */
            /*DEBUG("CommitEdit selected_match")*/
            xaccTransCommitEdit
            (selected_match->trans);

            /* Store the mapping to the other account in the MatchMap. */
            matchmap_store_destination (matchmap, trans_info, TRUE);

            /* Erase the downloaded transaction */
            xaccTransDestroy(trans_info->trans);
            /*DEBUG("CommitEdit trans")*/
            xaccTransCommitEdit(trans_info->trans);
            /* Very important: Make sure the freed transaction is not freed again! */
            trans_info->trans = NULL;
        }
    }
    return TRUE;
    default:
        DEBUG("Invalid GNCImportAction for this imported transaction.");
        break;
    }
    /*DEBUG("End");*/
    return FALSE;
}

/********************************************************************\
 * check_trans_online_id() Callback function used by
 * gnc_import_exists_online_id.  Takes pointers to transaction and split,
 * returns 0 if their online_ids  do NOT match, or if the split
 * belongs to the transaction
\********************************************************************/
static gint check_trans_online_id(Transaction *trans1, void *user_data)
{
    Account *account;
    Split *split1;
    Split *split2 = user_data;
    const gchar *online_id1;
    const gchar *online_id2;

    account = xaccSplitGetAccount(split2);
    split1 = xaccTransFindSplitByAccount(trans1, account);
    if (split1 == split2)
        return 0;

    /* hack - we really want to iterate over the _splits_ of the account
       instead of the transactions */
    g_assert(split1 != NULL);

    if (gnc_import_split_has_online_id(split1))
        online_id1 = gnc_import_get_split_online_id(split1);
    else
        online_id1 = gnc_import_get_trans_online_id(trans1);

    online_id2 = gnc_import_get_split_online_id(split2);

    if ((online_id1 == NULL) ||
            (online_id2 == NULL) ||
            (strcmp(online_id1, online_id2) != 0))
    {
        return 0;
    }
    else
    {
        /*printf("test_trans_online_id(): Duplicate found\n");*/
        return 1;
    }
}

static gint collect_trans_online_id(Transaction *trans, void *user_data)
{
    Split *split;
    GHashTable *id_hash = user_data;
    int i=0;
    
    const gchar* online_id = gnc_import_get_trans_online_id (trans);
    if (online_id)
        g_hash_table_add (id_hash, (void*) online_id);

    for (GList *splits = xaccTransGetSplitList (trans); splits; splits = splits->next)
    {
        if (gnc_import_split_has_online_id (splits->data))
            g_hash_table_add(id_hash, (void*) gnc_import_get_split_online_id (splits->data));
    }
    return 0;
}

/** Checks whether the given transaction's online_id already exists in
  its parent account. */
gboolean gnc_import_exists_online_id (Transaction *trans, GHashTable* acct_id_hash)
{
    gboolean online_id_exists = FALSE;
    Account *dest_acct;
    Split *source_split;

    /* Look for an online_id in the first split */
    source_split = xaccTransGetSplit(trans, 0);
    g_assert(source_split);

    // No online id, no point in continuing. We'd crash if we tried.
    if (!gnc_import_get_split_online_id (source_split))
        return FALSE;
    // Create a hash per account of a hash of all transactions IDs. Then the test below will be fast if
    // we have many transactions to import.
    dest_acct = xaccSplitGetAccount (source_split);
    if (!g_hash_table_contains (acct_id_hash, dest_acct))
    {
        GHashTable* new_hash = g_hash_table_new (g_str_hash, g_str_equal);
        g_hash_table_insert (acct_id_hash, dest_acct, new_hash);
        xaccAccountForEachTransaction (dest_acct, collect_trans_online_id, new_hash);
    }
    online_id_exists = g_hash_table_contains (g_hash_table_lookup (acct_id_hash, dest_acct),
                                              gnc_import_get_split_online_id (source_split));
    
    /* If it does, abort the process for this transaction, since it is
       already in the system. */
    if (online_id_exists == TRUE)
    {
        DEBUG("%s", "Transaction with same online ID exists, destroying current transaction");
        xaccTransDestroy(trans);
        xaccTransCommitEdit(trans);
    }
    return online_id_exists;
}


/* ******************************************************************
 */

/** Create a new object of GNCImportTransInfo here. */
GNCImportTransInfo *
gnc_import_TransInfo_new (Transaction *trans, GncImportMatchMap *matchmap)
{
    GNCImportTransInfo *transaction_info;
    Split *split;
    g_assert (trans);

    transaction_info = g_new0(GNCImportTransInfo, 1);

    transaction_info->trans = trans;
    /* Only use first split, the source split */
    split = xaccTransGetSplit(trans, 0);
    g_assert(split);
    transaction_info->first_split = split;

    /* Try to find a previously selected destination account
       string match for the ADD action */
    gnc_import_TransInfo_set_destacc (transaction_info,
                                      matchmap_find_destination (matchmap, transaction_info),
                                      FALSE);
    return transaction_info;
}


/** compare_probability() is used by g_list_sort to sort by probability */
static gint compare_probability (gconstpointer a,
                                 gconstpointer b)
{
    return(((GNCImportMatchInfo *)b)->probability -
           ((GNCImportMatchInfo *)a)->probability);
}

/** Iterates through all splits of the originating account of
 * trans_info. Sorts the resulting list and sets the selected_match
 * and action fields in the trans_info.
 */
void
gnc_import_TransInfo_init_matches (GNCImportTransInfo *trans_info,
                                   GNCImportSettings *settings)
{
    GNCImportMatchInfo * best_match = NULL;
    g_assert (trans_info);

    if (trans_info->match_list != NULL)
    {
        trans_info->match_list = g_list_sort(trans_info->match_list,
                                             compare_probability);
        best_match = g_list_nth_data(trans_info->match_list, 0);
        gnc_import_TransInfo_set_selected_match_info (trans_info,
                best_match,
                FALSE);
        if (best_match != NULL &&
                best_match->probability >= gnc_import_Settings_get_clear_threshold(settings))
        {
            trans_info->action = GNCImport_CLEAR;
        }
        else if (best_match == NULL ||
                 best_match->probability <= gnc_import_Settings_get_add_threshold(settings))
        {
            trans_info->action = GNCImport_ADD;
        }
        else if (gnc_import_Settings_get_action_skip_enabled(settings))
        {
            trans_info->action = GNCImport_SKIP;
        }
        else if (gnc_import_Settings_get_action_update_enabled(settings))
        {
            trans_info->action = GNCImport_UPDATE;
        }
        else
        {
            trans_info->action = GNCImport_ADD;
        }
    }
    else
    {
        trans_info->action = GNCImport_ADD;
    }
    if (best_match &&
            trans_info->action == GNCImport_CLEAR &&
            gnc_import_Settings_get_action_update_enabled(settings))
    {
        if (best_match->update_proposed)
        {
            trans_info->action = GNCImport_UPDATE;
        }
    }

    trans_info->previous_action = trans_info->action;
}


/* Try to automatch a transaction to a destination account if the */
/* transaction hasn't already been manually assigned to another account */
gboolean
gnc_import_TransInfo_refresh_destacc (GNCImportTransInfo *transaction_info,
                                      GncImportMatchMap *matchmap)
{
    Account *orig_destacc;
    Account *new_destacc = NULL;
    g_assert(transaction_info);

    orig_destacc = gnc_import_TransInfo_get_destacc(transaction_info);

    /* if we haven't manually selected a destination account for this transaction */
    if (gnc_import_TransInfo_get_destacc_selected_manually(transaction_info) == FALSE)
    {
        /* Try to find the destination account for this transaction based on prior ones */
        new_destacc = matchmap_find_destination(matchmap, transaction_info);
        gnc_import_TransInfo_set_destacc(transaction_info, new_destacc, FALSE);
    }
    else
    {
        new_destacc = orig_destacc;
    }

    /* account has changed */
    if (new_destacc != orig_destacc)
    {
        return TRUE;
    }
    else   /* account is the same */
    {
        return FALSE;
    }
}


/** @} */
