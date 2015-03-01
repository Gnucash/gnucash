/********************************************************************\
 * dialog-find-transactions2.c : locate transactions and show them   *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdio.h>

#include "gnc-ui-util.h"
#include "Query.h"
#include "qof.h"
#include "SX-book.h"
#include "Transaction.h"
#include "dialog-find-transactions2.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-register2.h"
#include "search-param.h"

#define GNC_PREFS_GROUP_SEARCH "dialogs.find"

struct _ftd_data
{
    QofQuery *		q;
    QofQuery *		ledger_q;
    GNCSearchWindow *	sw;
};

static void
do_find_cb (QofQuery *query, gpointer user_data, gpointer *result)
{
    struct _ftd_data *ftd = user_data;
    GNCLedgerDisplay2 *ledger;
    gboolean new_ledger = FALSE;
    GncPluginPage *page;

    ledger = gnc_ledger_display2_find_by_query (ftd->ledger_q);

    if (!ledger)
    {
        new_ledger = TRUE;
        ledger = gnc_ledger_display2_query (query, SEARCH_LEDGER2,
                                           REG2_STYLE_JOURNAL);
    }
    else
        gnc_ledger_display2_set_query (ledger, query);

    gnc_ledger_display2_refresh (ledger);

    if (new_ledger)
    {
        page = gnc_plugin_page_register2_new_ledger (ledger);
        gnc_main_window_open_page (NULL, page);
    }

    qof_query_destroy (ftd->q);

    gnc_search_dialog_destroy (ftd->sw);
}

static void
free_ftd_cb (gpointer user_data)
{
    struct _ftd_data *ftd = user_data;

    if (!ftd)
        return;

    g_free (ftd);
}

GNCSearchWindow *
gnc_ui_find_transactions_dialog_create2 (GNCLedgerDisplay2 * orig_ledg)
{
    QofIdType type = GNC_ID_SPLIT;
    struct _ftd_data *ftd;
    static GList *params = NULL;
    QofQuery *start_q, *show_q = NULL;
    gboolean num_action =
                qof_book_use_split_action_for_num_field(gnc_get_current_book());

    /* Build parameter list in reverse order */
    if (params == NULL)
    {
        params = gnc_search_param_prepend (params, N_("All Accounts"),
                                           ACCOUNT_MATCH_ALL_TYPE,
                                           type, SPLIT_TRANS, TRANS_SPLITLIST,
                                           SPLIT_ACCOUNT_GUID, NULL);
        params = gnc_search_param_prepend (params, N_("Account"), GNC_ID_ACCOUNT,
                                           type, SPLIT_ACCOUNT, QOF_PARAM_GUID,
                                           NULL);
        params = gnc_search_param_prepend (params, N_("Balanced"), NULL,
                                           type, SPLIT_TRANS, TRANS_IS_BALANCED,
                                           NULL);
        params = gnc_search_param_prepend (params, N_("Closing Entries"), NULL,
                                           type, SPLIT_TRANS, TRANS_IS_CLOSING,
                                           NULL);
        params = gnc_search_param_prepend (params, N_("Reconcile"), RECONCILED_MATCH_TYPE,
                                           type, SPLIT_RECONCILE, NULL);
        params = gnc_search_param_prepend (params, N_("Share Price"), NULL,
                                           type, SPLIT_SHARE_PRICE, NULL);
        params = gnc_search_param_prepend (params, N_("Shares"), NULL,
                                           type, SPLIT_AMOUNT, NULL);
        params = gnc_search_param_prepend (params, N_("Value"), NULL,
                                           type, SPLIT_VALUE, NULL);
        params = gnc_search_param_prepend (params, N_("Date Posted"), NULL,
                                           type, SPLIT_TRANS, TRANS_DATE_POSTED,
                                           NULL);
        params = gnc_search_param_prepend (params, (num_action
                                                    ? N_("Number/Action")
                                                    : N_("Action")), NULL,
                                           type, SPLIT_ACTION, NULL);
        params = gnc_search_param_prepend (params, (num_action
                                                    ? N_("Transaction Number")
                                                    : N_("Number")), NULL,
                                           type, SPLIT_TRANS, TRANS_NUM, NULL);
        {
            GList *params2 = NULL;
            params2 = gnc_search_param_prepend (params2, "", NULL,
                                               type, SPLIT_MEMO, NULL);
            params2 = gnc_search_param_prepend (params2, "", NULL,
                                               type, SPLIT_TRANS, TRANS_DESCRIPTION,
                                               NULL);
            params2 = gnc_search_param_prepend (params2, "", NULL,
                                               type, SPLIT_TRANS, TRANS_NOTES, NULL);
            params = gnc_search_param_prepend_compound (params, 
                                                        N_("Description, Notes, or Memo"),
                                                        params2,
                                                        GTK_JUSTIFY_LEFT, SEARCH_PARAM_ANY);
        }
        params = gnc_search_param_prepend (params, N_("Memo"), NULL,
                                           type, SPLIT_MEMO, NULL);
        params = gnc_search_param_prepend (params, N_("Notes"), NULL,
                                           type, SPLIT_TRANS, TRANS_NOTES, NULL);
        params = gnc_search_param_prepend (params, N_("Description"), NULL,
                                           type, SPLIT_TRANS, TRANS_DESCRIPTION,
                                           NULL);
    }
    else
    {
        GList *l;
        for (l = params; l; l = l->next)
        {
            GNCSearchParam *param = l->data;

            if (num_action)
            {
                if (strcmp (param->title, N_("Action")) == 0)
                    gnc_search_param_set_title (param, N_("Number/Action"));
                if (strcmp (param->title, N_("Number")) == 0)
                    gnc_search_param_set_title (param, N_("Transaction Number"));
            }
            else
            {
                if (strcmp (param->title, N_("Number/Action")) == 0)
                    gnc_search_param_set_title (param, N_("Action"));
                if (strcmp (param->title, N_("Transaction Number")) == 0)
                    gnc_search_param_set_title (param, N_("Number"));
            }
        }
    }

    ftd = g_new0 (struct _ftd_data, 1);

    if (orig_ledg)
    {
        ftd->ledger_q = gnc_ledger_display2_get_query (orig_ledg);
        start_q = show_q = qof_query_copy (ftd->ledger_q);
    }
    else
    {
        start_q = qof_query_create ();
        qof_query_set_book (start_q, gnc_get_current_book ());

        /* In lieu of not "mis-using" some portion of the infrastructure by writing
         * a bunch of new code, we just filter out the accounts of the template
         * transactions.  While these are in a seperate Account trees just for this
         * reason, the query engine makes no distinction between Account trees.
         * See Gnome Bug 86302.
         * 	-- jsled
         *
         * copied from gnc-ledger-display2.c:gnc_ledger_display2_gl()  -- warlord
         *
         * <jsled> Alternatively, you could look for a GNC_SX_ACCOUNT [SchedAction.h]
         * key in the KVP frame of the split.
         */
        {
            Account *tRoot;
            GList *al;

            tRoot = gnc_book_get_template_root( gnc_get_current_book() );
            al = gnc_account_get_descendants( tRoot );
            xaccQueryAddAccountMatch( start_q, al, QOF_GUID_MATCH_NONE, QOF_QUERY_AND );
            g_list_free (al);
            al = NULL;
            tRoot = NULL;
        }

        ftd->q = start_q;		/* save this to destroy it later */
    }

    ftd->sw = gnc_search_dialog_create (type, _("Find Transaction"),
                                        params, NULL, start_q, show_q,
                                        NULL, do_find_cb, NULL,
                                        ftd, free_ftd_cb, GNC_PREFS_GROUP_SEARCH, NULL);

    if (!ftd->sw)
    {
        free_ftd_cb (ftd);
        return NULL;
    }

    return ftd->sw;
}
