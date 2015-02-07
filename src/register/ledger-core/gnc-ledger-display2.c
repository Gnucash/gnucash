/********************************************************************\
 * gnc-ledger-display.c -- utilities for dealing with multiple      *
 *                         register/ledger windows in GnuCash       *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <time.h>

#include "Account.h"
#include "Query.h"
#include "qof.h"
#include "SX-book.h"
#include "Transaction.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-ledger-display2.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"

#include "split-register-control.h"
#include "split-register-model.h"

#include "gnc-tree-model-split-reg.h"


#define REGISTER_SINGLE_CM_CLASS     "register-single"
#define REGISTER_SUBACCOUNT_CM_CLASS "register-subaccount"
#define REGISTER_GL_CM_CLASS         "register-gl"
#define REGISTER_TEMPLATE_CM_CLASS   "register-template"

#define GNC_PREF_DOUBLE_LINE_MODE         "double-line-mode"
#define GNC_PREF_MAX_TRANS                "max-transactions"
#define GNC_PREF_DEFAULT_STYLE_LEDGER     "default-style-ledger"
#define GNC_PREF_DEFAULT_STYLE_AUTOLEDGER "default-style-autoledger"
#define GNC_PREF_DEFAULT_STYLE_JOURNAL    "default-style-journal"


struct gnc_ledger_display2
{
    GncGUID leader;

    Query *query;

    GNCLedgerDisplay2Type ld_type;

    GncTreeModelSplitReg *model; //FIXME Might get rid of this and use function to find.
    GncTreeViewSplitReg *view;

    gboolean refresh_ok;

    gboolean loading;
    gboolean use_double_line_default;

    GNCLedgerDisplay2Destroy destroy;
    GNCLedgerDisplay2GetParent get_parent;

    gpointer user_data;

    gint component_id;
};


/** GLOBALS *********************************************************/
static QofLogModule log_module = GNC_MOD_LEDGER;


/** Declarations ****************************************************/
static GNCLedgerDisplay2 *
gnc_ledger_display2_internal (Account *lead_account, Query *q,
                             GNCLedgerDisplay2Type ld_type,
                             SplitRegisterType2 reg_type,
                             SplitRegisterStyle2 style,
                             gboolean use_double_line,
                             gboolean is_template);

static void gnc_ledger_display2_refresh_internal (GNCLedgerDisplay2 *ld, GList *splits);

static void gnc_ledger_display2_refresh_cb (GncTreeModelSplitReg *model, gpointer item, gpointer user_data);

/** Implementations *************************************************/

Account *
gnc_ledger_display2_leader (GNCLedgerDisplay2 *ld)
{
    if (!ld)
        return NULL;

    return xaccAccountLookup (&ld->leader, gnc_get_current_book ());
}

GNCLedgerDisplay2Type
gnc_ledger_display2_type (GNCLedgerDisplay2 *ld)
{
    if (!ld)
        return -1;

    return ld->ld_type;
}

void
gnc_ledger_display2_set_user_data (GNCLedgerDisplay2 *ld, gpointer user_data)
{
    if (!ld)
        return;

    ld->user_data = user_data;
}

gpointer
gnc_ledger_display2_get_user_data (GNCLedgerDisplay2 *ld)
{
    if (!ld)
        return NULL;

    return ld->user_data;
}

void
gnc_ledger_display2_set_handlers (GNCLedgerDisplay2 *ld,
                                 GNCLedgerDisplay2Destroy destroy,
                                 GNCLedgerDisplay2GetParent get_parent)
{
    if (!ld)
        return;

    ld->destroy = destroy;
    ld->get_parent = get_parent;
}

GncTreeModelSplitReg *
gnc_ledger_display2_get_split_model_register (GNCLedgerDisplay2 *ld)
{
    if (!ld)
        return NULL;

    return ld->model;
}

Query *
gnc_ledger_display2_get_query (GNCLedgerDisplay2 *ld)
{
    if (!ld)
        return NULL;

    return ld->query;
}

static gboolean
find_by_leader (gpointer find_data, gpointer user_data)
{
    Account *account = find_data;
    GNCLedgerDisplay2 *ld = user_data;

    if (!account || !ld)
        return FALSE;

    return (account == gnc_ledger_display2_leader (ld));
}

static gboolean
find_by_query (gpointer find_data, gpointer user_data)
{
    Query *q = find_data;
    GNCLedgerDisplay2 *ld = user_data;

    if (!q || !ld)
        return FALSE;

    return ld->query == q;
}


static gboolean
find_by_reg (gpointer find_data, gpointer user_data)
{
    GncTreeModelSplitReg *model = find_data;
    GNCLedgerDisplay2 *ld = user_data;

    if (!model || !ld)
        return FALSE;

    return ld->model == model;
}

static SplitRegisterStyle2
gnc_get_default_register_style (GNCAccountType type)
{
    SplitRegisterStyle2 new_style = REG2_STYLE_LEDGER;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                            GNC_PREF_DEFAULT_STYLE_JOURNAL))
        new_style = REG2_STYLE_JOURNAL;
    else if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                 GNC_PREF_DEFAULT_STYLE_AUTOLEDGER))
        new_style = REG2_STYLE_AUTO_LEDGER;

    return new_style;
}

static gpointer
look_for_portfolio_cb (Account *account, gpointer data)
{
    return xaccAccountIsPriced(account) ? (gpointer) PORTFOLIO_LEDGER2 : NULL;
}

static SplitRegisterType2
gnc_get_reg_type (Account *leader, GNCLedgerDisplay2Type ld_type)
{
    GNCAccountType account_type;
    SplitRegisterType2 reg_type;

    if (ld_type == LD2_GL)
        return GENERAL_JOURNAL2;

    account_type = xaccAccountGetType (leader);

    if (ld_type == LD2_SINGLE)
    {
        switch (account_type)
        {
        case ACCT_TYPE_BANK:
            return BANK_REGISTER2;

        case ACCT_TYPE_CASH:
            return CASH_REGISTER2;

        case ACCT_TYPE_ASSET:
            return ASSET_REGISTER2;

        case ACCT_TYPE_CREDIT:
            return CREDIT_REGISTER2;

        case ACCT_TYPE_LIABILITY:
            return LIABILITY_REGISTER2;

        case ACCT_TYPE_PAYABLE:
            return PAYABLE_REGISTER2;

        case ACCT_TYPE_RECEIVABLE:
            return RECEIVABLE_REGISTER2;

        case ACCT_TYPE_STOCK:
        case ACCT_TYPE_MUTUAL:
            return STOCK_REGISTER2;

        case ACCT_TYPE_INCOME:
            return INCOME_REGISTER2;

        case ACCT_TYPE_EXPENSE:
            return EXPENSE_REGISTER2;

        case ACCT_TYPE_EQUITY:
            return EQUITY_REGISTER2;

        case ACCT_TYPE_CURRENCY:
            return CURRENCY_REGISTER2;

        case ACCT_TYPE_TRADING:
            return TRADING_REGISTER2;

        default:
            PERR ("unknown account type %d\n", account_type);
            return BANK_REGISTER2;
        }
    }

    if (ld_type != LD2_SUBACCOUNT)
    {
        PERR ("unknown ledger type %d\n", ld_type);
        return BANK_REGISTER2;
    }

    switch (account_type)
    {
    case ACCT_TYPE_BANK:
    case ACCT_TYPE_CASH:
    case ACCT_TYPE_ASSET:
    case ACCT_TYPE_CREDIT:
    case ACCT_TYPE_LIABILITY:
    case ACCT_TYPE_RECEIVABLE:
    case ACCT_TYPE_PAYABLE:
    {
        /* If any of the sub-accounts have ACCT_TYPE_STOCK or
         * ACCT_TYPE_MUTUAL types, then we must use the PORTFOLIO_LEDGER
         * ledger. Otherwise, a plain old GENERAL_JOURNAL will do. */
        gpointer ret;
        reg_type = GENERAL_JOURNAL2;

        ret = gnc_account_foreach_descendant_until(leader, look_for_portfolio_cb, NULL);
        if (ret) reg_type = PORTFOLIO_LEDGER2;
        break;
    }

    case ACCT_TYPE_STOCK:
    case ACCT_TYPE_MUTUAL:
    case ACCT_TYPE_CURRENCY:
        reg_type = PORTFOLIO_LEDGER2;
        break;

    case ACCT_TYPE_INCOME:
    case ACCT_TYPE_EXPENSE:
        reg_type = INCOME_LEDGER2;
        break;

    case ACCT_TYPE_EQUITY:
    case ACCT_TYPE_TRADING:
        reg_type = GENERAL_JOURNAL2;
        break;

    default:
        PERR ("unknown account type:%d", account_type);
        reg_type = GENERAL_JOURNAL2;
        break;
    }

    return reg_type;
}

/* Returns a boolean of whether this display should be single or double lined
 * mode by default */
gboolean
gnc_ledger_display2_default_double_line (GNCLedgerDisplay2 *gld)
{
    return (gld->use_double_line_default ||
            gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_DOUBLE_LINE_MODE));
}

/* Opens up a register window to display a single account */
GNCLedgerDisplay2 *
gnc_ledger_display2_simple (Account *account)
{
    SplitRegisterType2 reg_type;
    GNCAccountType acc_type = xaccAccountGetType (account);
    gboolean use_double_line;
    GNCLedgerDisplay2 *ld;

    ENTER("account=%p", account);

    switch (acc_type)
    {
    case ACCT_TYPE_PAYABLE:
    case ACCT_TYPE_RECEIVABLE:
        use_double_line = TRUE;
        break;
    default:
        use_double_line = FALSE;
        break;
    }

    reg_type = gnc_get_reg_type (account, LD2_SINGLE);

    ld = gnc_ledger_display2_internal (account, NULL, LD2_SINGLE, reg_type,
                                      gnc_get_default_register_style(acc_type),
                                      use_double_line, FALSE);
    LEAVE("%p", ld);
    return ld;
}

/* Opens up a register window to display an account, and all of its
 *   children, in the same window */
GNCLedgerDisplay2 *
gnc_ledger_display2_subaccounts (Account *account)
{
    SplitRegisterType2 reg_type;
    GNCLedgerDisplay2 *ld;

    ENTER("account=%p", account);

    reg_type = gnc_get_reg_type (account, LD2_SUBACCOUNT);

    ld = gnc_ledger_display2_internal (account, NULL, LD2_SUBACCOUNT,
                                      reg_type, REG2_STYLE_JOURNAL, FALSE,
                                      FALSE);
    LEAVE("%p", ld);
    return ld;
}

/* Opens up a general journal window. */
GNCLedgerDisplay2 *
gnc_ledger_display2_gl (void)
{
    Query *query;
    time64 start;
    struct tm tm;
    GNCLedgerDisplay2 *ld;

    ENTER(" ");

    query = qof_query_create_for (GNC_ID_SPLIT);

    qof_query_set_book (query, gnc_get_current_book());

    /* In lieu of not "mis-using" some portion of the infrastructure by writing
     * a bunch of new code, we just filter out the accounts of the template
     * transactions.  While these are in a seperate Account trees just for this
     * reason, the query engine makes no distinction between Account trees.
     * See Gnome Bug 86302.
     *         -- jsled */
    {
        Account *tRoot;
        GList *al;

        tRoot = gnc_book_get_template_root( gnc_get_current_book() );
        al = gnc_account_get_descendants( tRoot );
        xaccQueryAddAccountMatch( query, al, QOF_GUID_MATCH_NONE, QOF_QUERY_AND );
        g_list_free (al);
        al = NULL;
        tRoot = NULL;
    }

    gnc_tm_get_today_start(&tm);
    tm.tm_mon--; /* Default the register to the last month's worth of transactions. */
    start = gnc_mktime (&tm);
    xaccQueryAddDateMatchTT (query,
                             TRUE, start,
                             FALSE, 0,
                             QOF_QUERY_AND);

    ld = gnc_ledger_display2_internal (NULL, query, LD2_GL, GENERAL_JOURNAL2,
                                      REG2_STYLE_JOURNAL, FALSE, FALSE);
    LEAVE("%p", ld);
    return ld;
}

/**
 * @param id: The string version of the GncGUID of the context of template
 * transaction being edited in this template GL.  As used by scheduled
 * transactions, this is the GncGUID of the SX itself which is magically the
 * *name* of the (template) account which contains the transactions for this
 * scheduled transaction.  That's right.  The stringified GncGUID of the SX is
 * the name of the SX'es template account.
 **/
GNCLedgerDisplay2 *
gnc_ledger_display2_template_gl (char *id)
{
    QofBook *book;
    Query *q;
    GNCLedgerDisplay2 *ld;
    GncTreeModelSplitReg *model;
    Account *root, *acct;
    gboolean isTemplateModeTrue;

    ENTER("id=%s", id ? id : "(null)");

    acct = NULL;
    isTemplateModeTrue = TRUE;

    q = qof_query_create_for(GNC_ID_SPLIT);

    book = gnc_get_current_book ();
    qof_query_set_book (q, book);

    if ( id != NULL )
    {
        root = gnc_book_get_template_root (book);
        acct = gnc_account_lookup_by_name(root, id);
        g_assert( acct );
        xaccQueryAddSingleAccountMatch (q, acct, QOF_QUERY_AND);
    }

    ld = gnc_ledger_display2_internal (NULL, q, LD2_GL,
                                      SEARCH_LEDGER2,
                                      REG2_STYLE_JOURNAL,
                                      FALSE,
                                      isTemplateModeTrue);


    model = gnc_ledger_display2_get_split_model_register (ld);
    if ( acct )
    {
        gnc_tree_model_split_reg_set_template_account (model, acct);
    }

    LEAVE("%p", ld);
    return ld;
}

GtkWidget *
gnc_ledger_display2_get_parent( GNCLedgerDisplay2 *ld )
{
    if ( ld == NULL )
        return NULL;

    if ( ld->get_parent == NULL )
        return NULL;

    return ld->get_parent( ld );
}

static GtkWidget *
gnc_ledger_display2_parent (void *user_data)
{
    GNCLedgerDisplay2 *ld = user_data;
    return gnc_ledger_display2_get_parent( ld );
}

static void
gnc_ledger_display2_set_watches (GNCLedgerDisplay2 *ld, GList *splits)
{
    GList *node;

    gnc_gui_component_clear_watches (ld->component_id);

    gnc_gui_component_watch_entity_type (ld->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY
                                         | GNC_EVENT_ITEM_CHANGED);

    for (node = splits; node; node = node->next)
    {
        Split *split = node->data;
        Transaction *trans = xaccSplitGetParent (split);

        gnc_gui_component_watch_entity (ld->component_id,
                                        xaccTransGetGUID (trans),
                                        QOF_EVENT_MODIFY);
    }
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    GNCLedgerDisplay2 *ld = user_data;
    const EventInfo *info;
    gboolean has_leader;
    GList *splits;

    ENTER("changes=%p, user_data=%p", changes, user_data);

    if (ld->loading)
    {
        LEAVE("already loading");
        return;
    }

    has_leader = (ld->ld_type == LD2_SINGLE || ld->ld_type == LD2_SUBACCOUNT);

    if (has_leader)
    {
        Account *leader = gnc_ledger_display2_leader (ld);
        if (!leader)
        {
            gnc_close_gui_component (ld->component_id);
            LEAVE("no leader");
            return;
        }
    }

    if (changes && has_leader)
    {
        info = gnc_gui_get_entity_events (changes, &ld->leader);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (ld->component_id);
            LEAVE("destroy");
            return;
        }
    }

    /* Its not clear if we should re-run the query, or if we should
     * just use qof_query_last_run().  Its possible that the dates
     * changed, requiring a full new query.  Similar considerations
     * needed for multi-user mode.
     */
    splits = qof_query_run (ld->query);

//FIXME Not Needed ?    gnc_ledger_display2_set_watches (ld, splits);
//    gnc_ledger_display2_set_watches (ld, splits);

    //preference changes come this way
    gnc_ledger_display2_refresh_internal (ld, splits);

    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    GNCLedgerDisplay2 *ld = user_data;

    if (!ld)
        return;

    ENTER(" ");

    gnc_unregister_gui_component (ld->component_id);

    if (ld->destroy)
        ld->destroy (ld);

    gnc_tree_model_split_reg_destroy (ld->model);
    ld->model = NULL;
    ld->view = NULL;

    qof_query_destroy (ld->query);
    ld->query = NULL;

    LEAVE(" ");
    g_free (ld);
}

static void
gnc_ledger_display2_make_query (GNCLedgerDisplay2 *ld,
                               gint limit,
                               SplitRegisterType2 type)
{
    Account *leader;
    GList *accounts;

    if (!ld)
        return;

    switch (ld->ld_type)
    {
    case LD2_SINGLE:
    case LD2_SUBACCOUNT:
        break;

    case LD2_GL:
        return;

    default:
        PERR ("unknown ledger type: %d", ld->ld_type);
        return;
    }

    qof_query_destroy (ld->query);
    ld->query = qof_query_create_for(GNC_ID_SPLIT);

    /* This is a bit of a hack. The number of splits should be
     * configurable, or maybe we should go back a time range instead
     * of picking a number, or maybe we should be able to exclude
     * based on reconciled status. Anyway, this works for now. */
    if ((limit != 0) && (type != SEARCH_LEDGER2))
        qof_query_set_max_results (ld->query, limit);

    qof_query_set_book (ld->query, gnc_get_current_book());

    leader = gnc_ledger_display2_leader (ld);

    if (ld->ld_type == LD2_SUBACCOUNT)
        accounts = gnc_account_get_descendants (leader);
    else
        accounts = NULL;

    accounts = g_list_prepend (accounts, leader);

    xaccQueryAddAccountMatch (ld->query, accounts,
                              QOF_GUID_MATCH_ANY, QOF_QUERY_AND);

    g_list_free (accounts);
}

/* Opens up a ledger window for an arbitrary query. */
GNCLedgerDisplay2 *
gnc_ledger_display2_query (Query *query, SplitRegisterType2 type,
                          SplitRegisterStyle2 style)
{
    GNCLedgerDisplay2 *ld;

    ENTER("query=%p", query);

    ld = gnc_ledger_display2_internal (NULL, query, LD2_GL, type, style,
                                      FALSE, FALSE);
    LEAVE("%p", ld);
    return ld;
}

static GNCLedgerDisplay2 *
gnc_ledger_display2_internal (Account *lead_account, Query *q,
                             GNCLedgerDisplay2Type ld_type,
                             SplitRegisterType2 reg_type,
                             SplitRegisterStyle2 style,
                             gboolean use_double_line,
                             gboolean is_template )
{
    GNCLedgerDisplay2 *ld;
    gint limit;
    const char *klass;
    GList *splits;
    gboolean display_subaccounts = FALSE;
    gboolean is_gl = FALSE;

    switch (ld_type)
    {
    case LD2_SINGLE:
        klass = REGISTER_SINGLE_CM_CLASS;

        if (reg_type >= NUM_SINGLE_REGISTER_TYPES2)
        {
            PERR ("single-account register with wrong split register type");
            return NULL;
        }

        if (!lead_account)
        {
            PERR ("single-account register with no account specified");
            return NULL;
        }

        if (q)
        {
            PWARN ("single-account register with external query");
            q = NULL;
        }

        ld = gnc_find_first_gui_component (klass, find_by_leader, lead_account);
        if (ld)
            return ld;

        break;

    case LD2_SUBACCOUNT:
        klass = REGISTER_SUBACCOUNT_CM_CLASS;

        if (!lead_account)
        {
            PERR ("sub-account register with no lead account");
            return NULL;
        }

        if (q)
        {
            PWARN ("account register with external query");
            q = NULL;
        }

        ld = gnc_find_first_gui_component (klass, find_by_leader, lead_account);
        if (ld)
            return ld;

        display_subaccounts = TRUE;
        break;

    case LD2_GL:
        klass = REGISTER_GL_CM_CLASS;

        if (!q)
        {
            PWARN ("general journal with no query");
        }

        is_gl = TRUE;
        break;

    default:
        PERR ("bad ledger type: %d", ld_type);
        return NULL;

    }

    ld = g_new (GNCLedgerDisplay2, 1);

    ld->leader = *xaccAccountGetGUID (lead_account);
    ld->query = NULL;
    ld->ld_type = ld_type;
    ld->loading = FALSE;
    ld->refresh_ok = FALSE;
    ld->destroy = NULL;
    ld->get_parent = NULL;
    ld->user_data = NULL;

    limit = gnc_prefs_get_float(GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_MAX_TRANS);

    /* set up the query filter */
    if (q)
        ld->query = qof_query_copy (q);
    else
        gnc_ledger_display2_make_query (ld, limit, reg_type);

    ld->component_id = gnc_register_gui_component (klass,
                       refresh_handler,
                       close_handler, ld);

    /******************************************************************\
     * The main register window itself                                *
    \******************************************************************/

    ld->use_double_line_default = use_double_line;

    ld->model = gnc_tree_model_split_reg_new (reg_type, style, use_double_line, is_template);

    gnc_tree_model_split_reg_set_data (ld->model, ld, gnc_ledger_display2_parent);
    gnc_tree_model_split_reg_set_display (ld->model, display_subaccounts, is_gl);

    // This sets up a call back to reload after changes
    g_signal_connect (G_OBJECT (ld->model), "refresh_trans",
                      G_CALLBACK (gnc_ledger_display2_refresh_cb), ld );

//FIXME Not Needed ?    gnc_ledger_display2_set_watches (ld, splits);
//    gnc_ledger_display2_set_watches (ld, splits);

    // Populate the model with an empty split
    // An empty model could cause our gui callbacks to crash
    gnc_ledger_display2_refresh_internal (ld, NULL);

    return ld;
}

void
gnc_ledger_display2_set_split_view_register (GNCLedgerDisplay2 *ledger_display, GncTreeViewSplitReg *view)
{
    if (!ledger_display)
        return;

    ledger_display->view = view;
}

GncTreeViewSplitReg *
gnc_ledger_display2_get_split_view_register (GNCLedgerDisplay2 *ledger_display)
{
    if (!ledger_display)
        return NULL;

    return ledger_display->view;
}

void
gnc_ledger_display2_set_query (GNCLedgerDisplay2 *ledger_display, Query *q)
{
    if (!ledger_display || !q)
        return;

    g_return_if_fail (ledger_display->ld_type == LD2_GL);

    qof_query_destroy (ledger_display->query);
    ledger_display->query = qof_query_copy (q);
}

GNCLedgerDisplay2 *
gnc_ledger_display2_find_by_query (Query *q)
{
    GNCLedgerDisplay2 *ledger_display;
    GncTreeModelSplitReg *model;

    if (!q)
        return NULL;

    ledger_display = gnc_find_first_gui_component (REGISTER_GL_CM_CLASS, find_by_query, q);

    if (ledger_display)
    {
        model = ledger_display->model;
        // To get a new search page from a general journal, search register is a LD2_GL also.
        if (model->type == GENERAL_JOURNAL2)
            ledger_display = NULL;
    }
    return ledger_display;
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

static void
gnc_ledger_display2_refresh_internal (GNCLedgerDisplay2 *ld, GList *splits)
{
    GtkTreeModel *s_model, *model;

    if (!ld || ld->loading)
        return;

    if (!(ld->refresh_ok)) // We use this to test for the view available
    {
        ld->loading = TRUE;
        gnc_tree_model_split_reg_load (ld->model, splits, gnc_ledger_display2_leader (ld));
        ld->loading = FALSE;
    }
    else
    {
	/* This is used for the reloading of registers to refresh them and to update the search_ledger */
        ld->loading = TRUE;

	s_model = gtk_tree_view_get_model (GTK_TREE_VIEW (ld->view)); // this is the sort model
        model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (s_model)); // this is the model

        g_object_ref (s_model);
        g_object_ref (model);

        gnc_tree_view_split_reg_block_selection (ld->view, TRUE); // This blocks the tree selection
        gtk_tree_view_set_model (GTK_TREE_VIEW (ld->view), NULL); // Detach sort model from view
        gnc_tree_model_split_reg_load (ld->model, splits, gnc_ledger_display2_leader (ld)); //reload splits
        gtk_tree_view_set_model (GTK_TREE_VIEW (ld->view), GTK_TREE_MODEL (s_model)); // Re-attach sort model to view
        gnc_tree_view_split_reg_block_selection (ld->view, FALSE); // This unblocks the tree selection

        g_object_unref (model);
        g_object_unref (s_model);

        /* Set the default selection start position */
        gnc_tree_view_split_reg_default_selection (ld->view);

        ld->loading = FALSE;
    }
}

void
gnc_ledger_display2_refilter (GNCLedgerDisplay2 *ld)
{
    ENTER("ld=%p", ld);

    /* Set the default selection start position and refilter */
    gnc_tree_view_split_reg_default_selection (ld->view);

    LEAVE(" ");
}

void
gnc_ledger_display2_refresh_sched (GNCLedgerDisplay2 *ld, GList *splits)
{
    ENTER("ld=%p", ld);

    if (!ld)
    {
        LEAVE("no display");
        return;
    }

    if (ld->loading)
    {
        LEAVE("already loading");
        return;
    }
    gnc_ledger_display2_refresh_internal (ld, splits);
    LEAVE(" ");
}

void
gnc_ledger_display2_refresh (GNCLedgerDisplay2 *ld)
{
    ENTER("ld=%p", ld);

    if (!ld)
    {
        LEAVE("no display");
        return;
    }

    if (ld->loading)
    {
        LEAVE("already loading");
        return;
    }

    // Update the query before refresh
    gnc_tree_model_split_reg_update_query (ld->model, ld->query);
    gnc_ledger_display2_refresh_internal (ld, qof_query_run (ld->query));
    LEAVE(" ");
}

void
gnc_ledger_display2_refresh_by_split_register (GncTreeModelSplitReg *model)
{
    GNCLedgerDisplay2 *ld;

    if (!model)
        return;

    ld = gnc_find_first_gui_component (REGISTER_SINGLE_CM_CLASS,
                                       find_by_reg, model);
    if (ld)
    {
        gnc_ledger_display2_refresh (ld);
        return;
    }

    ld = gnc_find_first_gui_component (REGISTER_SUBACCOUNT_CM_CLASS,
                                       find_by_reg, model);
    if (ld)
    {
        gnc_ledger_display2_refresh (ld);
        return;
    }

    ld = gnc_find_first_gui_component (REGISTER_GL_CM_CLASS,
                                       find_by_reg, model);
    if (ld)
    {
        gnc_ledger_display2_refresh (ld);
        return;
    }

    ld = gnc_find_first_gui_component (REGISTER_TEMPLATE_CM_CLASS,
                                       find_by_reg, model);
    if (ld)
    {
        gnc_ledger_display2_refresh (ld);
    }
}

void
gnc_ledger_display2_set_split_view_refresh (GNCLedgerDisplay2 *ld, gboolean ok)
{
    if (!ld)
        return;

    ld->refresh_ok = ok;
}

/* This is used to reload after any changes made */
static void
gnc_ledger_display2_refresh_cb (GncTreeModelSplitReg *model, gpointer item, gpointer user_data)
{
    GNCLedgerDisplay2 *ld = user_data;

    /* Refresh the view when idle */
    g_idle_add ((GSourceFunc)gnc_ledger_display2_refresh, ld);
}

void
gnc_ledger_display2_close (GNCLedgerDisplay2 *ld)
{
    if (!ld)
        return;

    gnc_close_gui_component (ld->component_id);
}
