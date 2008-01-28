/* TODO:
 - date entry
 - autocomplete
 - expansion policy - easy
 - action field for trans
 - replace has_rate with price visibility test
 - non-anchored register

test basis mode debcred edit for blanktrans

   QUESTIONS
 - some bug with a zero amount in xfer dialog?
 - Is there any need to register with CM?
 - basic mode cred/deb edit - allow edit of trans with >2 splits?

   Done-ish
 - keynav
   - don't commit blank split just because of tab-through
 - cancel commit - navigation aborts to trans row, not prev split row
 - basic mode account field edit - done?
 - stock register - done?
 - reorderable columns? solved by gconf
 - column selection? solved by gconf

BUGS:
  - blank split sometimes appears before other splits
  - Gtk-CRITICAL **: _gtk_rbtree_reorder: assertion `tree->root->count == 
      length' failed

*/
/********************************************************************\
 * gnc-tree-view-transaction.c -- GtkTreeView implementation to     *
 *                        display Transactions in a GtkTreeView.    *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>
#include <gdk/gdkkeysyms.h>

#include "gnc-date.h"
#include "gnc-tree-view.h"
#include "gnc-tree-model-account.h"
#include "gnc-tree-model-transaction.h"
#include "gnc-tree-view-transaction.h"
//#include "gnctreemodelsort.h"

#include "Account.h"
#include "Transaction.h"
#include "TransactionP.h" // FIXME
#include "Scrub.h"
#include "gnc-gconf-utils.h"
#include "gnc-component-manager.h"
#include "gnc-icons.h"
#include "gnc-ui-util.h"
#include "gnc-exp-parser.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-gui-query.h"

#define SPLIT_TRANS_STR _("-- Split Transaction --")

/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_view_transaction_class_init(
    GncTreeViewTransactionClass *klass);
static void gnc_tree_view_transaction_init (GncTreeViewTransaction *view);
static void gnc_tree_view_transaction_finalize (GObject *object);
static void gnc_tree_view_transaction_dispose (GObject *object);

static void gtvt_edited_cb(GtkCellRendererText *cell, const gchar *path_string,
                          const gchar *new_text, gpointer _model);
static void start_edit(GtkCellRenderer *cr, GtkCellEditable *editable,
                       const gchar *path, gpointer user_data);
static void get_editable_start_editing_cb(
    GtkCellRenderer *cr, GtkCellEditable *editable,
    const gchar *path, gpointer user_data);

static gboolean
gtvt_key_press_cb(GtkWidget *treeview, GdkEventKey *event, gpointer userdata);

typedef enum {
    COL_DATE,
    COL_NUM,
    COL_DESCRIPTION,
    COL_ACCOUNT,
    COL_AMOUNT,
    COL_VALUE,
    COL_DEBIT,
    COL_CREDIT,
    COL_RECN,
    COL_BALANCE,
    COL_RATE,
    COL_TYPE,
    COL_NOTES,
} ViewCol;

typedef struct {
    ViewCol viewcol;
    gint modelcol;
    gchar *title;
    gchar *pref_name;
    gchar *sizer;
    int visibility_model_col;
    void (*edited_cb)(GtkCellRendererText *, const gchar *,
                      const gchar *, gpointer);
    void (*editing_started_cb)(GtkCellRenderer *, GtkCellEditable *,
                               const gchar *, gpointer);
    GtkTreeIterCompareFunc sort_fn;
} ColDef;

static ColDef all_tree_view_transaction_columns[] = {
    {COL_DATE, GNC_TREE_MODEL_TRANSACTION_COL_DATE,
     "Date", "date", "00/00/0000xxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtvt_edited_cb, NULL, gtmt_sort_by_date},
    {COL_NUM, GNC_TREE_MODEL_TRANSACTION_COL_NUM,
     "Num", "num", "0000xx",
     GNC_TREE_VIEW_COLUMN_COLOR_NONE,
     gtvt_edited_cb, get_editable_start_editing_cb, NULL},
    {COL_DESCRIPTION, GNC_TREE_MODEL_TRANSACTION_COL_DESCRIPTION,
     "Description", "description", "xxxxxxxxxxxxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtvt_edited_cb, get_editable_start_editing_cb, NULL},
    {COL_ACCOUNT, -1,
     "Transfer", "transfer", "xxxxxxxxxxxxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL /*FIXME?*/, start_edit, NULL},
    {COL_RECN, -1,
     "R", "recn", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtvt_edited_cb, NULL, NULL},
    {COL_AMOUNT, -1,
     "Amt", "amount", "xxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtvt_edited_cb, get_editable_start_editing_cb, NULL},
    {COL_VALUE, -1,
     "Val", "value", "xxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},
    {COL_DEBIT, -1,
     "Debit", "debit", "xxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtvt_edited_cb, get_editable_start_editing_cb,
     NULL},
    {COL_CREDIT, -1,
     "Credit", "credit", "xxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtvt_edited_cb, get_editable_start_editing_cb,
     NULL},
    {COL_BALANCE, -1,
     "Balance", "balance", "xxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},
    {COL_RATE, -1,
     "Price", "price", "xxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtvt_edited_cb, get_editable_start_editing_cb,
     NULL},
    {COL_TYPE, -1,
     "Type", "type", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},
    {COL_NOTES, -1,
     "Notes", "notes", "xxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtvt_edited_cb, get_editable_start_editing_cb, NULL},
};

struct GncTreeViewTransactionPrivate
{
    QofBook *book;
    Account *anchor;
    gnc_commodity *reg_comm;

    Split *dirty_split;
    Transaction *dirty_trans;

    gchar *acct_edit_path;  // remember which row's account we're editing
    GtkCellRenderer *temp_cr;

    gboolean has_rate;  /* if set, the transfer dialog will never automatically pop-up */
};


/************************************************************/
/*               g_object required functions                */
/************************************************************/

static GObjectClass *parent_class = NULL;

GType
gnc_tree_view_transaction_get_type (void)
{
    static GType gnc_tree_view_transaction_type = 0;

    if (gnc_tree_view_transaction_type == 0) {
        static const GTypeInfo ti = {
            sizeof (GncTreeViewTransactionClass),
            NULL, NULL,
            (GClassInitFunc) gnc_tree_view_transaction_class_init,
            NULL, NULL,
            sizeof (GncTreeViewTransaction),
            0,
            (GInstanceInitFunc) gnc_tree_view_transaction_init
        };

        gnc_tree_view_transaction_type = g_type_register_static (
            GNC_TYPE_TREE_VIEW, "GncTreeViewTransaction", &ti, 0);
    }

    return gnc_tree_view_transaction_type;
}

static void
gnc_tree_view_transaction_class_init (GncTreeViewTransactionClass *klass)
{
    GObjectClass *o_class;

    parent_class = g_type_class_peek_parent (klass);

    /* GObject signals */
    o_class = G_OBJECT_CLASS (klass);
    o_class->finalize = gnc_tree_view_transaction_finalize;
    o_class->dispose = gnc_tree_view_transaction_dispose;
}

static void
gnc_tree_view_transaction_init (GncTreeViewTransaction *view)
{
    view->priv = g_new0 (GncTreeViewTransactionPrivate, 1);
}

static void
gnc_tree_view_transaction_finalize (GObject *object)
{
    GncTreeViewTransaction *view;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_VIEW_TRANSACTION (object));

    view = GNC_TREE_VIEW_TRANSACTION (object);
    g_free (view->priv);

    if (G_OBJECT_CLASS(parent_class)->finalize)
        G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
gnc_tree_view_transaction_dispose (GObject *object)
{
    GncTreeViewTransaction *view;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_VIEW_TRANSACTION (object));

    view = GNC_TREE_VIEW_TRANSACTION (object);
    gnc_tree_view_set_model(GNC_TREE_VIEW(view), NULL);

    if (G_OBJECT_CLASS (parent_class)->dispose)
        G_OBJECT_CLASS (parent_class)->dispose(object);
}

static GncTreeModelTransaction *
get_trans_model_from_view(GncTreeViewTransaction *tv)
{
    GtkTreeModelSort *s_model = GTK_TREE_MODEL_SORT(
        gtk_tree_view_get_model(GTK_TREE_VIEW(tv)));
    return GNC_TREE_MODEL_TRANSACTION(gtk_tree_model_sort_get_model(s_model));
}

static gboolean
get_model_iter_from_view_string(GncTreeViewTransaction *tv,
                                const gchar *path_string, GtkTreeIter *iter)
{
    GtkTreeModelSort *s_model;
    GtkTreeIter s_iter;

    s_model = GTK_TREE_MODEL_SORT(gtk_tree_view_get_model(GTK_TREE_VIEW(tv)));
    if (!gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(s_model),
                                             &s_iter, path_string)) {
        iter = NULL;
        return FALSE;
    }
    gtk_tree_model_sort_convert_iter_to_child_iter(s_model, iter, &s_iter);
    return TRUE;
}

static gboolean
get_model_iter_from_selection(GncTreeViewTransaction *tv,
                              GtkTreeSelection *sel, GtkTreeIter *iter)
{
    GtkTreeModel *s_model;
    GtkTreeIter s_iter;

    if (gtk_tree_selection_get_selected(sel, &s_model, &s_iter)) {
        gtk_tree_model_sort_convert_iter_to_child_iter(
            GTK_TREE_MODEL_SORT(s_model), iter, &s_iter);
        return TRUE;
    }
    return FALSE;

}

#define get_selected_split gnc_tree_view_transaction_get_selected_split
Split *
gnc_tree_view_transaction_get_selected_split(GncTreeViewTransaction *tv)
{
    GtkTreeIter iter;
    GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(tv));
    Split *split = NULL;

    if (get_model_iter_from_selection(tv, sel, &iter)) {
        GncTreeModelTransaction *model = get_trans_model_from_view(tv);
        gnc_tree_model_transaction_get_split_and_trans(
            model, &iter, NULL, NULL, &split, NULL);
    }
    return split;
}

#define get_selected_trans gnc_tree_view_transaction_get_selected_trans
Transaction *
gnc_tree_view_transaction_get_selected_trans(GncTreeViewTransaction *tv)
{
    GtkTreeIter iter;
    GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(tv));
    Transaction *trans = NULL;

    if (get_model_iter_from_selection(tv, sel, &iter)) {
        GncTreeModelTransaction *model = get_trans_model_from_view(tv);
        gnc_tree_model_transaction_get_split_and_trans(
            model, &iter, NULL, NULL, NULL, &trans);
    }
    return trans;
}

static GtkTreePath *
get_view_path_from_model_iter(GncTreeViewTransaction *tv, GtkTreeIter *iter)
{
    GtkTreeModelSort *s_model;
    GtkTreeIter s_iter;

    s_model = GTK_TREE_MODEL_SORT(gtk_tree_view_get_model(GTK_TREE_VIEW(tv)));
    gtk_tree_model_sort_convert_child_iter_to_iter(s_model, &s_iter, iter);
    return gtk_tree_model_get_path(GTK_TREE_MODEL(s_model), &s_iter);
}

#if UNUSED
static gboolean
get_model_iter_from_view_path(GncTreeViewTransaction *tv,
                              GtkTreePath *s_path, GtkTreeIter *iter)
{
    GtkTreeModelSort *s_model;
    GtkTreeModel *model;
    GtkTreePath *path;

    s_model = GTK_TREE_MODEL_SORT(gtk_tree_view_get_model(GTK_TREE_VIEW(tv)));
    model = gtk_tree_model_sort_get_model(s_model);
    if (!model) {
        iter = NULL;
        return FALSE;
    }

    path = gtk_tree_model_sort_convert_path_to_child_path(s_model, s_path);
    if (gtk_tree_model_get_iter(model, iter, path)) {
        gtk_tree_path_free(path);
        return TRUE;
    }
    return FALSE;
}

static gboolean
gnc_tree_view_transaction_separator(GtkTreeModel *tm, GtkTreeIter *s_iter,
                                    gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModelSort *tms = GTK_TREE_MODEL_SORT(tm);
    GtkTreeModel *tmt;

    gtk_tree_model_sort_convert_iter_to_child_iter(tms, &iter, s_iter);
    tmt = gtk_tree_model_sort_get_model(tms);
    return FALSE;
    //return gnc_tree_model_transaction_separator(tmt, &iter, data);
}

#endif

/* poor name: really means: If this is the blank split, it may now
   eventually graduate to a real split. The trans must already be
   opened for editing because the split will be added to the
   transaction if hasn't been already. */
static void
mark_split_dirty(GncTreeViewTransaction *tv, Split *split, Transaction *trans)
{
    if (split != tv->priv->dirty_split && tv->priv->dirty_split) {
        g_print("commiting dirty split\n");
        gnc_tree_model_transaction_commit_split(get_trans_model_from_view(tv),
                                                tv->priv->dirty_split);
    }

    if (split && trans && xaccSplitGetParent(split) != trans)
        xaccSplitSetParent(split, trans);

    tv->priv->dirty_split = split;
}

/* means: open trans for editing, unless we're editing a Split (split
   != NULL) AND split doesn't belong to the trans (because it is the
   blank split) */
static void
begin_edit(GncTreeViewTransaction *tv, Split *split, Transaction *trans)
{
    /* explain me */
    if (split && trans != xaccSplitGetParent(split))
        return;
    if (trans != tv->priv->dirty_trans) {
        xaccTransBeginEdit(trans);
        tv->priv->dirty_trans = trans;
        if (!xaccTransGetCurrency(trans)) {
            xaccTransSetCurrency(trans, tv->priv->reg_comm);
        }
    }
}

//FIXME
static Split *
get_other_split(GncTreeViewTransaction *tv, Transaction *trans)
{
    int i;
    Split *split = NULL;
    Account *anchor = tv->priv->anchor;

    for (i = 0; (split = xaccTransGetSplit(trans, i)); i++) {
        if (anchor == xaccSplitGetAccount(split))
            return xaccSplitGetOtherSplit(split);
    }
    return NULL;
}

/*
static Split *
get_this_split(GncTreeViewTransaction *tv, Transaction *trans)
{
    int i;
    Split *split = NULL;
    Account *anchor = tv->priv->anchor;

    for (i = 0; (split = xaccTransGetSplit(trans, i)); i++) {
        if (anchor == xaccSplitGetAccount(split))
            return split;
    }
    return NULL;
}
*/

/* The returned Splits may be newly created and not yet belong to trans. */
static gboolean
get_split_pair(GncTreeViewTransaction *tv, Transaction *trans,
               Split **osplit, Split **split)
{
    gint count = xaccTransCountSplits(trans);
    Account *anchor = tv->priv->anchor;

    if (count == 0) {
        *split = xaccMallocSplit(tv->priv->book);
        xaccSplitSetAccount(*split, anchor);
        *osplit = xaccMallocSplit(tv->priv->book);
    } else if (count == 2) {
        int i;
        Split *s;
        for (i = 0; (s = xaccTransGetSplit(trans, i)); i++) {
            if (anchor == xaccSplitGetAccount(s)) {
                *split = s;
                break;
            }
        }
        //*split = get_this_split(tv, trans);
        g_assert(*split);
        *osplit = get_other_split(tv, trans);
        g_assert(*osplit);
    } else return FALSE;
    return TRUE;
}

/* Returns a value for display. */
static gnc_numeric
get_value_for(GncTreeViewTransaction *tv, Transaction *trans,
              Split *split, gboolean is_blank)
{
    gnc_commodity *currency = xaccTransGetCurrency(trans);
    gnc_numeric total;

    total = xaccSplitGetValue(split);

    if (is_blank && gnc_numeric_zero_p(total)) {
        gnc_numeric rate;
        total = gnc_numeric_neg(xaccTransGetImbalance(trans));
        if (!gnc_numeric_zero_p(total)) {
            if (!xaccTransGetRateForCommodity(
                    trans, tv->priv->reg_comm, NULL, &rate))
                return gnc_numeric_zero();

            total = gnc_numeric_mul(
                total, rate,
                gnc_commodity_get_fraction (currency),
                GNC_RND_ROUND);
        }
    } else {
        if (!gnc_numeric_zero_p(total) &&
            gnc_numeric_check(total) == GNC_ERROR_OK) {

            /* fixme: if needs conversion? */
            gnc_commodity *commodity = tv->priv->reg_comm;
            if (commodity && !gnc_commodity_equiv(commodity, currency))
                total = xaccSplitConvertAmount(split, commodity);
        }
    }
    return total;
}

static gnc_numeric
get_rate_for(GncTreeViewTransaction *tv, Transaction *trans,
              Split *split, gboolean is_blank)
{
    gnc_numeric num;

    num = get_value_for(tv, trans, split, is_blank);
    num = gnc_numeric_div(
        xaccSplitGetAmount(split), num,
        GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    return num;
}

/* Instead of setting a different cellDataFunc for each column, we just
   collect everything here and use this one func. */
static void
cdf(GtkTreeViewColumn *col, GtkCellRenderer *cell, GtkTreeModel *s_model,
    GtkTreeIter *s_iter, gpointer data)
{
    GncTreeModelTransaction *model;
    GtkTreeIter iter;
    GtkTreePath *path;
    ViewCol viewcol;
    gboolean is_trans, is_split, is_blank;
    gboolean editable, expanded;
    GncTreeViewTransaction *tv = GNC_TREE_VIEW_TRANSACTION(data);
    Account *anchor = tv->priv->anchor;
    Split *split;
    Transaction *trans;

    gnc_numeric num;
    //gnc_commodity *comm;
    const gchar *s = "";

    g_return_if_fail(GTK_TREE_VIEW_COLUMN(col));
    g_return_if_fail(GTK_CELL_RENDERER(cell));
    g_return_if_fail(GTK_TREE_MODEL_SORT(s_model));

    model = get_trans_model_from_view(tv);
    g_return_if_fail(model);

    gtk_tree_model_sort_convert_iter_to_child_iter(
        GTK_TREE_MODEL_SORT(s_model), &iter, s_iter);
    viewcol = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cell),
                                                "view_column"));
    g_return_if_fail(gnc_tree_model_transaction_get_split_and_trans(
                         GNC_TREE_MODEL_TRANSACTION(model), &iter,
                         &is_split, &is_blank, &split, &trans));
    is_trans = !is_split;

    // TODO: gconf-ize
    if (xaccTransGetDate(trans) > time(NULL)) {
        g_object_set(cell, "foreground", "blue", NULL);
        g_object_set(cell, "style", PANGO_STYLE_ITALIC, NULL);
    } else {
        g_object_set(cell, "foreground", "black", NULL);
        g_object_set(cell, "style", PANGO_STYLE_NORMAL, NULL);
    }

    switch (viewcol) {
    case COL_DATE:
        g_object_set(cell, "visible", is_trans, NULL);
        if (is_trans) {
            Timespec ts = {0,0};
            xaccTransGetDatePostedTS (trans, &ts);
        //If the time returned by xaccTransGetDatePostedTS is 0 then assume it
        //is a new transaction and set the time to current time to show current
        //date on new transactions
        if (ts.tv_sec == 0)
            ts.tv_sec = time(NULL);
            g_object_set(cell, "text", gnc_print_date(ts), NULL);
        }
        break;
    case COL_TYPE: {
        static char ss[2];
        char type = xaccTransGetTxnType(trans);
        if (type == TXN_TYPE_NONE)
            type = '?';

        ss[0] = type;
        ss[1] = '\0';
        g_object_set(cell, "text", ss, NULL);
    }
        break;
    case COL_NOTES:
        if (is_trans) {
            s = xaccTransGetNotes(trans);
        } else s = "";
        g_object_set(cell, "text", s, NULL);
        break;
    case COL_RECN:
        if (is_trans) {
            GtkTreePath *path;

            path = gtk_tree_model_get_path(s_model, s_iter);
            if (gtk_tree_view_row_expanded(GTK_TREE_VIEW(tv), path)) {
                s = "";
            } else if (anchor) {
                s = xaccTransHasReconciledSplitsByAccount(
                    trans, anchor) ? gnc_get_reconcile_str(CREC):gnc_get_reconcile_str(NREC);
            } else {
                s = "";
            }
            gtk_tree_path_free(path);
        } else {
            s = gnc_get_reconcile_str(xaccSplitGetReconcile(split));
            if (!s) s = "";
        }
        g_object_set(cell, "text", s, NULL);
        break;
    case COL_AMOUNT:
        if (is_split) {
            gnc_numeric amt = xaccSplitGetAmount(split);
            s = xaccPrintAmount(amt, gnc_account_print_info(
                                    xaccSplitGetAccount(split), TRUE));
            editable = TRUE;
        } else {
            s = "";
            editable = FALSE;
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;
    case COL_VALUE:
        if (is_split) {
            gnc_numeric amt = xaccSplitGetValue(split);
            s = xaccPrintAmount(amt, gnc_commodity_print_info(
                                    xaccTransGetCurrency(trans), TRUE));
        } else s = "";
        g_object_set(cell, "text", s, NULL);
        break;
    case COL_DEBIT:
    case COL_CREDIT:
        if (is_split) {
            num = get_value_for(tv, trans, split, is_blank);
            editable = TRUE;
        } else {
            //comm = xaccTransGetCurrency(trans);
            if (anchor) {
                gint count = xaccTransCountSplits(trans);
                path = gtk_tree_model_get_path(s_model, s_iter);
                expanded = gtk_tree_view_row_expanded(GTK_TREE_VIEW(tv), path);
                editable = !expanded && ((2 == count) || (0 == count));
                num = xaccTransGetAccountAmount(trans, anchor);
            } else {
                editable = FALSE;
                num = gnc_numeric_zero();
                /* CHECKME: Should trans cred/debit fields in unanchored reg
                   show imbalances? I don't think so. */
            }
        }

        if ((gnc_numeric_check(num) != GNC_ERROR_OK) ||
            gnc_numeric_zero_p(num) ||
            (gnc_numeric_negative_p(num) && viewcol == COL_DEBIT) ||
            (gnc_numeric_positive_p(num) && viewcol == COL_CREDIT)) {
            s = "";
        } else {
            s = xaccPrintAmount(gnc_numeric_abs(num),
                                gnc_account_print_info(anchor, TRUE));
            //FIXME: TRUE just for debugging
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;
    case COL_BALANCE:
        if (is_trans && trans && anchor) {
            num = xaccTransGetAccountBalance(trans, anchor);
            if (gnc_reverse_balance(anchor))
                num = gnc_numeric_neg(num);
            s = xaccPrintAmount(num, gnc_account_print_info(anchor, TRUE));
            if (gnc_numeric_negative_p(num)
                && gnc_gconf_get_bool(GCONF_GENERAL, KEY_NEGATIVE_IN_RED,
                                        NULL))
                g_object_set(cell, "foreground", "red", (gchar*)NULL);
        } else {
            s = "";
        }
        g_object_set(cell, "text", s, NULL);
        break;
    case COL_ACCOUNT:
        if (is_trans) {
            gint count = xaccTransCountSplits(trans);
            path = gtk_tree_model_get_path(s_model, s_iter);
            expanded = gtk_tree_view_row_expanded(GTK_TREE_VIEW(tv), path);
            if (count == 0 || expanded) {
                s = ""; /* blank-out if splits are visible */
            } else if (2 == count) {
                Account *acct;
                Split *osplit;
                osplit = get_other_split(tv, trans);
                acct = xaccSplitGetAccount(osplit);
                s = xaccAccountGetName(acct);
            } else {
                s = SPLIT_TRANS_STR;
            }
            editable = anchor && !expanded && ((2 == count) || (0 == count));
            gtk_tree_path_free(path);
        }
        if (is_split) {
            Account *acct = xaccSplitGetAccount(split);
            s = xaccAccountGetName(acct);
            editable = TRUE;
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;
    case COL_RATE:
        if (is_trans) {
            s = "";
            editable = FALSE;
        } else {
            gnc_commodity *split_com = xaccAccountGetCommodity(
                xaccSplitGetAccount(split));
            num = get_rate_for(tv, trans, split, is_blank);
            if (gnc_numeric_check(num) == GNC_ERROR_OK) {
                s = xaccPrintAmount(num, gnc_default_price_print_info());
                editable = !gnc_numeric_zero_p(num) &&
                    !gnc_commodity_equiv(split_com, tv->priv->reg_comm);
            } else {
                s = "";
                editable = FALSE;
            }
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;
    default:
        break;
    }
}

static gboolean
needs_exchange_rate(GncTreeViewTransaction *tv, Transaction *trans,
                    Split *split)
{
    gnc_commodity *split_com, *txn_curr, *reg_com;

    if (tv->priv->has_rate) return FALSE;

    txn_curr = xaccTransGetCurrency(trans);
    split_com = xaccAccountGetCommodity(xaccSplitGetAccount(split));
    if (split_com && txn_curr && !gnc_commodity_equiv(split_com, txn_curr))
        return TRUE;

    reg_com = tv->priv->reg_comm;
    if (reg_com && split_com && !gnc_commodity_equiv(reg_com, split_com))
        return TRUE;

    return FALSE;
}

/* Either sets the value and amount for split and returns TRUE, or
   does nothing and returns FALSE. */
static gboolean
handle_exchange_rate(GncTreeViewTransaction *tv, gnc_numeric amount,
                     Transaction *trans, Split *split)
{
    XferDialog *xfer;
    gboolean rate_split_ok, rate_reg_ok;
    gnc_numeric rate_split, rate_reg, value;
    gnc_commodity *xfer_comm =
        xaccAccountGetCommodity(xaccSplitGetAccount(split));
    gnc_commodity *reg_comm = tv->priv->reg_comm;
    gnc_commodity *trans_curr = xaccTransGetCurrency(trans);

    /* Rate from trans-curr to split-comm */
    rate_split_ok = xaccTransGetRateForCommodity(trans, xfer_comm,
                                                 split, &rate_split);
    /* Rate from trans-curr to reg-comm */
    rate_reg_ok = xaccTransGetRateForCommodity(trans, reg_comm,
                                               split, &rate_reg);

    if (rate_reg_ok && rate_split_ok) {
        value = gnc_numeric_div(
            amount, rate_reg, gnc_commodity_get_fraction(trans_curr),
            GNC_DENOM_REDUCE);
        amount = gnc_numeric_mul(value, rate_split, GNC_DENOM_AUTO,
                                 GNC_HOW_RND_ROUND);
    } else {
        rate_split = gnc_numeric_create(1, 1);

        //g_message("reg amt: %s", gnc_numeric_to_string(amount));
        /* create the exchange-rate dialog */
        xfer = gnc_xfer_dialog(NULL, NULL); //FIXME: get parent window
        gnc_xfer_dialog_is_exchange_dialog(xfer, &rate_split);

        /* fill in the dialog entries */
        gnc_xfer_dialog_set_description(xfer, xaccTransGetDescription(trans));
        gnc_xfer_dialog_set_memo(xfer, xaccSplitGetMemo(split));
        gnc_xfer_dialog_set_num(xfer, xaccTransGetNum(trans));
        gnc_xfer_dialog_set_date(
            xfer, timespecToTime_t(xaccTransRetDatePostedTS(trans)));

        value = amount;
        if (gnc_xfer_dialog_run_exchange_dialog(
                xfer, &rate_split, &value, reg_comm, trans, xfer_comm))
            return FALSE;
        amount = gnc_numeric_mul(value, rate_split,
                                 GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }
    xaccSplitSetAmount(split, amount);
    xaccSplitSetValue(split, value);
    g_message("split amt=%s; split val=%s", gnc_numeric_to_string(amount),
              gnc_numeric_to_string(value));
    return TRUE;
}

static void
set_value_for(GncTreeViewTransaction *tv, Transaction *trans, Split *split,
              gnc_numeric input)
{
    Account *anchor = tv->priv->anchor;
    Account *acct = xaccSplitGetAccount(split);
    gnc_commodity *currency = xaccTransGetCurrency(trans);
    gnc_numeric value, amount, rate;

    if (gnc_numeric_zero_p(input)) {
        xaccSplitSetValue(split, input);
        xaccSplitSetAmount(split, input);
        return;
    }

    if (needs_exchange_rate(tv, trans, split)) {
        if (handle_exchange_rate(tv, input, trans, split)) {
            ;
        }
        return;
    }

    /* Context determines the interpretation of the input.  If the
       treeview is anchored to an account then the input is
       interpreted as being in the Account's commodity.  Otherwise,
       it's interpreted as being in the Transaction's currency. */
    if (anchor) {
        gnc_commodity *reg_com = tv->priv->reg_comm;
        gnc_commodity *split_com = xaccAccountGetCommodity(acct);
        /* Convert from the anchor account's commodity to
           trans currency */
        //xaccSplitSetAmount(split, input);
        if (gnc_commodity_equiv(currency, reg_com))
            value = input;
        else {
            if (!xaccTransGetRateForCommodity(trans, reg_com, NULL, &rate))
                return;
            //rate = xaccTransGetAccountConvRate(trans, anchor);
            if (gnc_numeric_zero_p(rate)) {
                // FIXME: probably wrong.
                xaccTransSetCurrency(trans, reg_com);
                value = input;
            } else
                value = gnc_numeric_div(
                    input, rate,
                    GNC_DENOM_AUTO, //?
                    //gnc_commodity_get_fraction(currency),
                    GNC_HOW_RND_ROUND);
        }
        xaccSplitSetValue(split, value);
        //return;
        if (gnc_commodity_equiv(split_com, reg_com))
            amount = input;
        else {
            rate = xaccTransGetAccountConvRate(trans, acct);
            amount = gnc_numeric_mul(
                value, rate,
                xaccAccountGetCommoditySCU(acct), GNC_RND_ROUND);
        }
        xaccSplitSetAmount(split, amount);
    } else {
        //FIXME: untested; assumes entry in the trans currency
        //gnc_commodity *split_com = xaccAccountGetCommodity(acct);
        value = input;
        xaccSplitSetValue(split, value);
        //g_assert(split_com == currency);
        //FIXME: obsolete
        /* For a split belonging to another account */
        rate = xaccTransGetAccountConvRate(trans, acct);
        amount = gnc_numeric_mul(
            value, rate,
            xaccAccountGetCommoditySCU(acct), GNC_RND_ROUND);
        if (gnc_numeric_check(amount) == GNC_ERROR_OK) {
            xaccSplitSetAmount(split, amount);
        }
    }
}

static void
set_amount_for(GncTreeViewTransaction *tv, Transaction *trans, Split *split,
               gnc_numeric input)
{
    Account *acct = xaccSplitGetAccount(split);
    gnc_commodity *split_com = xaccAccountGetCommodity(acct);
    gnc_commodity *currency = xaccTransGetCurrency(trans);

    xaccSplitSetAmount(split, input);
    if (gnc_commodity_equiv(currency, split_com))
        xaccSplitSetValue(split, input);

    return;
}

static void
set_rate_for(GncTreeViewTransaction *tv, Transaction *trans, Split *split,
             gnc_numeric input, gboolean is_blank)
{
    gnc_commodity *split_comm;

    gnc_numeric old_rate = get_rate_for(tv, trans, split, is_blank);
    gnc_numeric factor = gnc_numeric_div(input, old_rate, GNC_DENOM_AUTO,
                                         GNC_HOW_RND_ROUND);
    split_comm = xaccAccountGetCommodity(xaccSplitGetAccount(split));
    xaccTransAdjustRateForCommodity(trans, split_comm, factor);

#if JUNK
    reg_comm = tv->priv->reg_comm;
    if (xaccTransGetRateForCommodity(
            trans, reg_comm, split, &reg_rate)) {
        input = gnc_numeric_div(input, reg_rate,
                                GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
        /* input is now the rate from the transaction currency to the
           split_com */
    }

    if (gnc_numeric_zero_p(val) && gnc_numeric_zero_p(amt)) {
        gnc_numeric one = gnc_numeric_create(1, 1);
        xaccSplitSetAmount(split, one);
        amt = one;
    }

    if (gnc_numeric_zero_p(val)) {
        val = gnc_numeric_div(input, amt,
                              GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    } else {
        amt = gnc_numeric_mul(input, val,
                              GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }

    amt = gnc_numeric_mul(get_value_for(tv, trans, split, FALSE /*FIXME*/),
                          input,
                          GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    set_amount_for(tv, trans, split, amt);

    //xaccSplitSetValue(split, val);
    //xaccSplitSetAmount(split, amt);

#endif
}

//Makes a copy of the data in the appropriate column of the tree
//model to use for autocompletion, so that the real tree model is
//not altered by the autocompletion.
static void
model_copy(gpointer data)
{
    GtkListStore *description_list, *memo_list;
    GtkTreeIter parent_iter, description_iter, memo_iter, child_iter;
    gchar *string;
    gboolean parents = FALSE, children = FALSE;
    //gint column = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cell), 
    //			"model_column"));
    gint column = GNC_TREE_MODEL_TRANSACTION_COL_DESCRIPTION;

    description_list = gtk_list_store_new(1, G_TYPE_STRING);
    memo_list = gtk_list_store_new(1, G_TYPE_STRING);
    
    parents = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(get_trans_model_from_view(data)), &parent_iter);
    while (parents)	
    {
        gtk_tree_model_get(GTK_TREE_MODEL(get_trans_model_from_view(data)), &parent_iter, column, &string, -1);
        gtk_list_store_append(description_list, &description_iter);
        gtk_list_store_set(description_list, &description_iter, 0, string, -1);
        children = gtk_tree_model_iter_children(GTK_TREE_MODEL(get_trans_model_from_view(data)), &child_iter, &parent_iter);
        while (children)
        {
            //Get the Memo string for the child (split) node
            gtk_tree_model_get(GTK_TREE_MODEL(get_trans_model_from_view(data)), &child_iter, column, &string, -1);
            //Store the memo field if it isn't an empty string
            if (g_ascii_strcasecmp(string, ""))    
            {
                gtk_list_store_append(memo_list, &memo_iter);
                gtk_list_store_set(memo_list, &memo_iter, 0, string, -1);
            }//if
            children = gtk_tree_model_iter_next(GTK_TREE_MODEL(get_trans_model_from_view(data)), &child_iter);
        }//while
        parents = gtk_tree_model_iter_next(GTK_TREE_MODEL(get_trans_model_from_view(data)), &parent_iter);
    }//while

    g_object_set_data(G_OBJECT(data), "model_copy", description_list);
    g_object_set_data(G_OBJECT(data), "memo_copy", memo_list);
}//model_copy

/* Connected to "edited" from cellrenderer. For reference, see
   split-register-model-save.c */
static void
gtvt_edited_cb(GtkCellRendererText *cell, const gchar *path_string,
               const gchar *new_text, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeIter copy_iter;
    GtkEntryCompletion *completion;
    Split *split;
    Transaction *trans;
    GncTreeViewTransaction *tv = GNC_TREE_VIEW_TRANSACTION(data);
    ViewCol viewcol;
    gboolean is_trans, is_split, is_blank;
    GncTreeModelTransaction *model;
    char *error_loc = NULL;
    Account *anchor = tv->priv->anchor;

    g_return_if_fail(get_model_iter_from_view_string(tv, path_string, &iter));

    viewcol = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cell),
                                                "view_column"));
    model = get_trans_model_from_view(tv);
    g_return_if_fail(model);

    gnc_tree_model_transaction_get_split_and_trans (
        model, &iter, &is_split, &is_blank, &split, &trans);
    is_trans = !is_split;

    switch (viewcol) {
    case COL_DATE:
        if (is_trans) {
            GDate date;
            g_date_set_parse(&date, new_text);
            if (g_date_valid(&date)) {
                begin_edit(tv, split, trans);
                g_print(new_text);
                xaccTransSetDate(trans, g_date_get_day(&date), 
                                 g_date_get_month(&date), 
                                 g_date_get_year(&date));
            } else {
                PERR("invalid date `%s`", new_text);
            }
        }
        break;
    case COL_NUM: // aka "ACTION"
        begin_edit(tv, split, trans);
        if (is_split) {
            xaccSplitSetAction(split, new_text);
        }
        if (is_trans) {
            xaccTransSetNum(trans, new_text);
            /* TODO: If the next number is visible in the blank trans,
               and this is not the blank split, and this trans is
               using that next number, we may want to increment number
               of the blank trans. */
        }
        break;
    case COL_DESCRIPTION: // aka "MEMO"
        begin_edit(tv, split, trans);
        if (is_split)
        {
            xaccSplitSetMemo(split, new_text);
            gtk_list_store_append(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv),
                "memo_copy")), &copy_iter);
            gtk_list_store_set(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv), 
                "memo_copy")), &copy_iter, 0, new_text, -1);
        }//if
        if (is_trans)
        {
            xaccTransSetDescription(trans, new_text);
            gtk_list_store_append(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv),
                "model_copy")), &copy_iter);
            gtk_list_store_set(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv), 
                "model_copy")), &copy_iter, 0, new_text, -1);
        }//if
        break;
    case COL_NOTES:
        if (is_trans) {
            begin_edit(tv, split, trans);
            xaccTransSetNotes(trans, new_text);
        }
        break;
    case COL_AMOUNT:
    case COL_RATE:
    case COL_DEBIT:
    case COL_CREDIT: {
        Account *acct;
        gnc_numeric input;
        Split *split2 = NULL;

        if (!gnc_exp_parser_parse (new_text, &input, &error_loc))
            break;

        if (is_trans && anchor) {
            if (!get_split_pair(tv, trans, &split2, &split)) {
                PERR("couldn't get split pair");
                break;
            }
        }
        begin_edit(tv, NULL, trans); // open trans even if split not a child
        mark_split_dirty(tv, split, trans);

        acct = xaccSplitGetAccount(split);
        if (!acct) {
            if (anchor) {
                xaccSplitSetAccount(split, anchor);
                acct = xaccSplitGetAccount(split);
            } else {
                break; //Well, what else is there to do?
            }
        }

        if (viewcol == COL_CREDIT)
            input = gnc_numeric_neg(input);

        if (viewcol == COL_AMOUNT) {
            set_amount_for(tv, trans, split, input);
            break;
        }

        if (viewcol == COL_RATE) {
            set_rate_for(tv, trans, split, input, is_blank);
            break;
        }

        set_value_for(tv, trans, split, input);
        if (split2) {
            xaccSplitSetParent(split2, trans);
            set_value_for(tv, trans, split2, gnc_numeric_neg(input));
        }
    }
        break;
    case COL_RECN:
        if (is_split) {
            begin_edit(tv, split, trans);
            xaccSplitSetReconcile(split, *new_text);
        }
        break;
    default:
        //g_assert_not_reached();
        break;
    }
}

void
gnc_tree_view_transaction_cancel_edit(GncTreeViewTransaction *tv)
{
    Transaction *t = tv->priv->dirty_trans;

    if (t && xaccTransIsOpen(t)) {
        xaccTransRollbackEdit(tv->priv->dirty_trans);
        tv->priv->dirty_trans = NULL;
    }
}
/* Returns TRUE if dialog was canceled. Does nothing is 'new_trans'
   is the dirty trans. */
static gboolean
transaction_changed_confirm(GncTreeViewTransaction *tv,
                            Transaction *new_trans)
{
    GtkWidget *dialog;
    gint response;
    const char *title = _("Save the changed transaction?");
    const char *message = _(
        "The current transaction has changed.  Would you like to "
        "record the changes, or discard the changes?");

    if (!tv->priv->dirty_trans || tv->priv->dirty_trans == new_trans)
        return FALSE;

    g_print("commiting dirty trans\n");
    dialog = gtk_message_dialog_new(NULL, /* FIXME: parent */
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_QUESTION,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                                             "%s", message);
    gtk_dialog_add_buttons(
        GTK_DIALOG(dialog),_("_Discard Changes"), GTK_RESPONSE_REJECT,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
        _("_Record Changes"), GTK_RESPONSE_ACCEPT, NULL);
    response = gnc_dialog_run(GTK_DIALOG(dialog), "transaction_changed");
    gtk_widget_destroy(dialog);

    switch (response) {
    case GTK_RESPONSE_ACCEPT:
        xaccTransCommitEdit(tv->priv->dirty_trans);
        tv->priv->dirty_trans = NULL;
        break;

    case GTK_RESPONSE_REJECT:
        gnc_tree_view_transaction_cancel_edit(tv);
        break;
    case GTK_RESPONSE_CANCEL:
    default:
        return TRUE;  //FIXME
    }

    return FALSE;
}

static void
restore_cursor_to_dirty(GncTreeViewTransaction *tv)
{
    GtkTreeIter iter;
    GncTreeModelTransaction *model = get_trans_model_from_view(tv);

    /* FIXME?: restore split cursor */
    if (gnc_tree_model_transaction_get_iter_from_trans_and_split(
            model, tv->priv->dirty_trans, NULL, &iter)) {
        GtkTreePath *path = get_view_path_from_model_iter(tv, &iter);
        GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(tv));
        gtk_tree_selection_select_path(sel, path);
        gtk_tree_path_free(path);
    }
}

static void
//motion_cb(GtkTreeView *tv, gpointer data)
motion_cb(GtkTreeSelection *sel, gpointer data)
{
    //GtkTreeView *tv = _tv;
    //gboolean is_split; //, is_expanded;
    // GtkTreePath *s_path;
    GtkTreeIter iter;
    GncTreeViewTransaction *tv = GNC_TREE_VIEW_TRANSACTION(data);
    GncTreeModelTransaction *model; // = GNC_TREE_MODEL_TRANSACTION(data);
    Transaction *trans = NULL;

    model = get_trans_model_from_view(tv);
    mark_split_dirty(tv, NULL, NULL);

    if (get_model_iter_from_selection(tv, sel, &iter)) {
        gnc_tree_model_transaction_get_split_and_trans (
            model, &iter, NULL, NULL, NULL, &trans);

        //Only ask for confirmation if data has been edited
        if (g_object_get_data(G_OBJECT(tv), "data-edited") && transaction_changed_confirm(tv, trans)) {
            /* Restore cursor position */
            restore_cursor_to_dirty(tv);
        } else {
            gnc_tree_model_transaction_set_blank_split_parent(model, trans);
            g_object_set_data(G_OBJECT(tv), "data-edited", FALSE);
        }
    }
}

void 
gnc_tree_view_transaction_select_split(GncTreeViewTransaction *tv, 
                                       Split *split)
{
    GtkTreeIter iter;
    GncTreeModelTransaction *model = get_trans_model_from_view(tv);

    if (gnc_tree_model_transaction_get_iter_from_trans_and_split(
            model, NULL, split, &iter)) {
        GtkTreePath *path = get_view_path_from_model_iter(tv, &iter);
        GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(tv));
        gtk_tree_selection_select_path(sel, path);
        gtk_tree_path_free(path);
    }
}

void gnc_tree_view_transaction_goto_blank_trans(GncTreeViewTransaction *tv)
{
    GtkTreeIter iter;
    GncTreeModelTransaction *model = get_trans_model_from_view(tv);

    if (gnc_tree_model_transaction_get_blank_trans_iter(model, &iter)) {
        GtkTreePath *path = get_view_path_from_model_iter(tv, &iter);
        GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(tv));
        gtk_tree_selection_select_path(sel, path);
        gtk_tree_path_free(path);
    }
}

/* Note: These next three functions are for handling the editable
   account combo box cell.  There are at least two ways of doing this:
   We could just catch the "edited" signal offered by
   gtk_cell_renderer_text, but then we'd get only the account name
   string for the selected account, and we'd have to lookup the actual
   account object.  Or, we can jump through these hoops below to get
   access to the GncTreeModelAccount underlying the combobox.  TODO:
   maybe the paths that are common with the gtvt_edited_cb can be
   factored. */

/* Note2: I'm not really sure these 3 functions are the way to go at
   all.  In particular, I imagine that if string completion were added
   we'd maybe want to do this differently. */
/* Connected to "editing-done" from the ComboBox. */
static void
editing_done_cb(GtkCellEditable *ce, gpointer user_data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX(ce);
    GncTreeViewTransaction *tv = GNC_TREE_VIEW_TRANSACTION(user_data);
    GtkTreeIter f_iter, a_iter;

    g_print("done edit");
    if (tv->priv->acct_edit_path &&
        gtk_combo_box_get_active_iter(cbox, &f_iter)) {
        GtkTreeModelFilter *f_model;
        GncTreeModelAccount *acc_model;
        GncTreeModelTransaction *model;
        Account *old_acct = NULL, *new_acct;
        GtkTreeIter tv_iter;
        gboolean is_split, is_blank;
        Split *split = NULL, *split2 = NULL;
        Transaction *trans;

        f_model = GTK_TREE_MODEL_FILTER(gtk_combo_box_get_model(cbox));
        gtk_tree_model_filter_convert_iter_to_child_iter(f_model, &a_iter,
                                                         &f_iter);
        acc_model = GNC_TREE_MODEL_ACCOUNT(
            gtk_tree_model_filter_get_model(f_model));
        new_acct = gnc_tree_model_account_get_account(acc_model, &a_iter);

        g_return_if_fail(get_model_iter_from_view_string(
                             tv, tv->priv->acct_edit_path, &tv_iter));

        model = get_trans_model_from_view(tv);
        gnc_tree_model_transaction_get_split_and_trans (
            model, &tv_iter, &is_split, &is_blank, &split, &trans);

        if (!is_split) {
            if (!get_split_pair(tv, trans, &split, &split2))
                PERR("couldn't get split pair");
        }
        old_acct = xaccSplitGetAccount(split);

        if (old_acct != new_acct) {
            gnc_numeric input;
            gnc_commodity *reg_comm = tv->priv->reg_comm;

            PINFO("setting %s to %s", xaccAccountGetName(old_acct),
                  xaccAccountGetName(new_acct));
            input = get_value_for(tv, trans, split, is_blank);

            /* Important: It's possible that this split contains the
               only representation of the exchange rate from the
               transaction currency into the register commodity.  If
               we allowed the loss of this info, we wouldn't know what
               to display for any split. */
            if (!old_acct || 
                xaccTransGetRateForCommodity(trans, reg_comm, split, NULL)) {
                begin_edit(tv, NULL, trans);
                mark_split_dirty(tv, split, trans);
                xaccSplitSetAccount(split, new_acct);
                set_value_for(tv, trans, split, input);
                if (split2) {
                    xaccSplitSetParent(split2, trans);
                    set_value_for(tv, trans, split2, gnc_numeric_neg(input));
                }
            } else {
                // CHECKME: false alarm when new account has same comm as old?
                g_print("Can't change anchoring split to account of "
                        "different commodity");
            }
        }
    }
}

static void
remove_edit(GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewTransaction *tv = GNC_TREE_VIEW_TRANSACTION(user_data);
    const gchar *new_string; 
    const gchar *current_string; 

    //These strings are used to determine if cell data was altered so
    //that keynav works better
    new_string = gtk_entry_get_text(GTK_ENTRY(g_object_get_data(
            G_OBJECT(tv->priv->temp_cr), "cell-editable")));	
    current_string = g_object_get_data(G_OBJECT(
            tv->priv->temp_cr), "current-string");

    //If editing wasn't canceled and strings don't match then
    //cell data was edited
    if (!g_object_get_data(G_OBJECT(tv->priv->temp_cr), "edit-canceled") 
             && g_ascii_strcasecmp(new_string, current_string))
    {
        g_object_set_data(G_OBJECT(tv), "data-edited", (gpointer)TRUE);
    }//if 
    g_print("remove edit\n");
    g_object_set_data(G_OBJECT(tv->priv->temp_cr), "cell-editable", NULL);
    tv->priv->temp_cr = NULL;
    g_free(tv->priv->acct_edit_path);
    tv->priv->acct_edit_path = NULL;

}

/* Explain: GtkEntry has a cursor that blinks upon
   g_timeout_dispatch().  It complains if it blinks after the GtkEntry
   loses focus.  So, we can't pop up any dialogs while the blinking
   cursor is around.  The solution is to force the editing to be
   finished before raising the dialog.  That finalizes the
   gtkcelleditable. */
static void
finish_edit(GtkTreeViewColumn *col)
{
    GtkCellRenderer *cr;
    GtkCellEditable *ce;

    if (!col) return;
    cr = gnc_tree_view_column_get_renderer(col);
    if ((ce = GTK_CELL_EDITABLE(g_object_get_data(G_OBJECT(cr),
                                                  "cell-editable")))) {
        gtk_cell_editable_editing_done(ce);
    }
}

//Handle the "editing-canceled" signal
static void
gtvt_editing_canceled_cb(GtkCellRenderer *cr, gpointer user_data)
{
    GncTreeViewTransaction *tv = GNC_TREE_VIEW_TRANSACTION(user_data);

    //Set edit-canceled property
    g_object_set_data(G_OBJECT(cr), "edit-canceled", (gpointer)TRUE);	

}//gtvt_editing_canceled_cb

static void
get_editable_start_editing_cb(GtkCellRenderer *cr, GtkCellEditable *editable,
                              const gchar *path_string, gpointer user_data)
{
    GncTreeViewTransaction *tv = GNC_TREE_VIEW_TRANSACTION(user_data);
    GtkListStore *description_list = g_object_get_data(G_OBJECT(tv), "model_copy");
    GtkListStore *memo_list = g_object_get_data(G_OBJECT(tv), "memo_copy");
    GtkEntryCompletion *completion = gtk_entry_completion_new();
    gint depth;
    GtkTreeViewColumn *num, *description, *transfer;

    num = gtk_tree_view_get_column(GTK_TREE_VIEW(tv), 1);
    description = gtk_tree_view_get_column(GTK_TREE_VIEW(tv), 2);
    transfer = gtk_tree_view_get_column(GTK_TREE_VIEW(tv), 3);

    //Use depth to determine if it is a split or parent transaction
    depth = gtk_tree_path_get_depth(gtk_tree_path_new_from_string(path_string));

    //First steps towards setting the column headers based
    //on whether split or parent is being edited
    if (depth >= 2)
    {
        gtk_tree_view_column_set_title(num, "Action");
        gtk_tree_view_column_set_title(description, "Memo");
        gtk_tree_view_column_set_title(transfer, "Accounts");
    }//if
    else
    {
        gtk_tree_view_column_set_title(num, "Num");
        gtk_tree_view_column_set_title(description, "Description");
        gtk_tree_view_column_set_title(transfer, "Transfer");
    }//else

    g_print("start_edit");
    g_object_set_data(G_OBJECT(cr), "cell-editable", editable);
    //Copy the string in the GtkEntry for later comparison
    g_object_set_data(G_OBJECT(cr), "current-string", 
            g_strdup(gtk_entry_get_text(GTK_ENTRY(editable))));
    tv->priv->temp_cr = cr;
    //Add edit-canceled property to cr so we can distinguish between
    //cancelled and actual changes
    g_object_set_data(G_OBJECT(cr), "edit-canceled", FALSE);
    g_signal_connect(G_OBJECT(editable), "remove-widget",
                     (GCallback) remove_edit, tv);

    if (GNC_TREE_MODEL_TRANSACTION_COL_DESCRIPTION 
        == GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cr), "model_column")))
            //&& GTK_IS_ENTRY(editable))
    {
        //Data used for completion is set based on if editing split or not
        if (depth == 1)
        {
            gtk_entry_completion_set_model(completion, GTK_TREE_MODEL(description_list));
           	g_object_set(G_OBJECT(completion), "text-column", 0, NULL);
        }//if
        else
        {
            gtk_entry_completion_set_model(completion, GTK_TREE_MODEL(memo_list));
       	    g_object_set(G_OBJECT(completion), "text-column", 0, NULL);
        }//else
        //g_object_set(G_OBJECT(completion), "text-column", 
        //	GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cr), "model_column")));
        gtk_entry_completion_set_inline_completion(completion, TRUE);
        gtk_entry_completion_set_popup_completion(completion, FALSE);
        gtk_entry_set_completion(GTK_ENTRY(editable), completion);
    }//if

}//get_editable_start_editing_cb

static void
start_edit(GtkCellRenderer *cr, GtkCellEditable *editable,
           const gchar *path_string, gpointer user_data)
{
    GncTreeViewTransaction *tv = GNC_TREE_VIEW_TRANSACTION(user_data);

    get_editable_start_editing_cb(cr, editable, path_string, user_data);
    g_signal_connect(G_OBJECT(editable), "editing-done",
                     (GCallback) editing_done_cb, tv);
    tv->priv->acct_edit_path = g_strdup(path_string); //FIXME: use rowref?
    return;
}

void
gnc_tree_view_transaction_delete_selected(GncTreeViewTransaction *tv)
{
    GncTreeModelTransaction *model;
    GtkTreeIter iter;
    GtkTreeSelection *sel;
    Transaction *trans;
    Split *split;

    model = get_trans_model_from_view(tv);
    g_return_if_fail(model);

    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(tv));

    if (get_model_iter_from_selection(tv, sel, &iter)) {
        gboolean is_split, is_blank;
        gnc_tree_model_transaction_get_split_and_trans(
            model, &iter, &is_split, &is_blank, &split, &trans);
        begin_edit(tv, split, trans);
        if (is_split) {
            gnc_commodity *reg_comm = tv->priv->reg_comm;
            gnc_numeric reg_rate;

            if (xaccTransGetRateForCommodity(
                    trans, reg_comm, split, &reg_rate)) {
                mark_split_dirty(tv, NULL, NULL);  // unnecessary?
                xaccSplitDestroy(split);
            } else {
                // FIXME: dialog
                g_print("Can't remove split that anchors to reg_comm");
            }
        } else {
            xaccTransDestroy(trans);
            g_assert(tv->priv->dirty_trans == trans);
            xaccTransCommitEdit(trans);
            tv->priv->dirty_trans = NULL;
        }
    }
}

void
gnc_tree_view_transaction_reinit_trans(GncTreeViewTransaction *tv)
{
    Transaction *trans;

    trans = get_selected_trans(tv);
    if (trans) {
        Split *s;
        int i = 0;
        
        begin_edit(tv, NULL, trans);        
        while ((s = xaccTransGetSplit(trans, i)) != NULL) {
            if (xaccTransGetRateForCommodity(
                    trans, tv->priv->reg_comm, s, NULL))
                xaccSplitDestroy(s);
            else i++;
        }
    }
}

#if 0

static gboolean
gtvt_button_press_event_handler(GtkWidget *treeview, GdkEventButton *event,
                                gpointer userdata)
{
    GtkTreeSelection *sel;
    
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
    
    if (gtk_tree_selection_count_selected_rows(sel) <= 1) {
        GtkTreePath *path;
        
        /* Get tree path for row that was clicked */
        if (gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(treeview),
                                          (gint) event->x,
                                          (gint) event->y,
                                          &path, NULL, NULL, NULL)) {
            gtk_tree_selection_unselect_all(sel);
            gtk_tree_selection_select_path(sel, path);
            gtk_tree_path_free(path);
        }
    }
    do_popup_menu(treeview, event, userdata);   
}

static gboolean
gtvt_popup_menu_handler(GtkWidget *widget, gpointer userdata)
{
    do_popup_menu(widget, NULL, userdata);
    return TRUE;
}
#endif

/* Creates a treeview with the list of fields */
static GncTreeViewTransaction *
gnc_tree_view_transaction_set_cols(GncTreeViewTransaction *tv,
                                   const ViewCol col_list[])
{
    int i = 0;

    while (col_list && col_list[i] != -1) {
        GtkCellRenderer *cr;
        GtkTreeViewColumn *col;
        ColDef def;
        int j, ncol = G_N_ELEMENTS(all_tree_view_transaction_columns);

        for (j = 0; j < ncol; j++) {
            if (col_list[i] == all_tree_view_transaction_columns[j].viewcol) {
                def = all_tree_view_transaction_columns[j];
                break;
            }
        }
        if (j == ncol) {
            PERR("Failed to find column definition.");
            i++;
            continue;
        }

        if (col_list[i] == COL_ACCOUNT) {
            //FIXME: we need to store a ref to the f_model?
            GtkTreeModel *acc_model, *f_model;
            GtkTreePath *virtual_root_path = NULL;
            Account *root;

            root = gnc_book_get_root_account(tv->priv->book);

            acc_model = gnc_tree_model_account_new(root);
            virtual_root_path = gtk_tree_path_new_first();
            f_model = gtk_tree_model_filter_new (acc_model, virtual_root_path);
            g_object_unref(G_OBJECT(acc_model));
            gtk_tree_path_free(virtual_root_path);
            col = gnc_tree_view_add_combo_column (
                GNC_TREE_VIEW(tv), def.title, def.pref_name, def.sizer,
                def.modelcol, def.visibility_model_col,
                f_model, GNC_TREE_MODEL_ACCOUNT_COL_NAME,
                def.sort_fn);
            g_object_unref(G_OBJECT(f_model));
        } else if (col_list[i] == COL_DATE) {
            col = gnc_tree_view_add_calendar_column (
                GNC_TREE_VIEW(tv), def.title, def.pref_name, NULL, def.sizer,
                def.modelcol, def.visibility_model_col, def.sort_fn);
        } else {
            col = gnc_tree_view_add_text_column (
                GNC_TREE_VIEW(tv), def.title, def.pref_name, NULL, def.sizer,
                def.modelcol, def.visibility_model_col, def.sort_fn);
        }

        g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
        cr = gnc_tree_view_column_get_renderer(col);

        if (def.editing_started_cb) {
            //Store the position of the column in the model
            g_object_set_data(G_OBJECT(cr), "model_column", 
                GINT_TO_POINTER(def.modelcol));
            g_signal_connect(G_OBJECT(cr), "editing-started",
                (GCallback) def.editing_started_cb, tv);
        }

        //Connect editing-canceled signal so that edit-cancelled can be set appropriately
        g_signal_connect(G_OBJECT(cr), "editing-canceled", G_CALLBACK(gtvt_editing_canceled_cb), tv);

        // This can die when prefs are used.
        g_object_set(G_OBJECT(col), "resizable", TRUE, NULL);

        if (def.edited_cb) {
            g_object_set(G_OBJECT(cr), "editable", TRUE, NULL);
            g_signal_connect(G_OBJECT(cr), "edited",
                             (GCallback) def.edited_cb, tv);
        }
        g_object_set_data(G_OBJECT(cr), "view_column",
                          GINT_TO_POINTER(def.viewcol));
        gtk_tree_view_column_set_cell_data_func(
            col, cr, cdf, tv, NULL);
        i++;
    }

    //Make a copy of data from the treemodel to use for autocompletion
    model_copy(tv);

    gnc_tree_view_configure_columns(GNC_TREE_VIEW(tv));

    //g_signal_connect(tv, "cursor-changed", G_CALLBACK(motion_cb), NULL);
    g_signal_connect(gtk_tree_view_get_selection(GTK_TREE_VIEW(tv)),
                     "changed", G_CALLBACK(motion_cb), tv);

    //Add a data-edited property to keep track of transaction edits
    g_object_set_data(G_OBJECT(tv), "data-edited", FALSE);

    //gtk_tree_selection_set_mode(gtk_tree_view_get_selection(tv),
    //                            GTK_SELECTION_BROWSE);
    g_signal_connect_after(G_OBJECT(tv), "key-press-event",
                     G_CALLBACK(gtvt_key_press_cb), NULL);
    return tv;
}

static ViewCol col_list[] = {
    COL_DATE, COL_NUM, COL_DESCRIPTION, COL_ACCOUNT, COL_RECN,
    COL_AMOUNT, COL_VALUE, COL_RATE, COL_DEBIT, COL_CREDIT,
    COL_BALANCE, -1};

GncTreeViewTransaction *
gnc_tree_view_transaction_new_with_model(GncTreeModelTransaction *model)
{
    GtkTreeModel *s_model;
    GncTreeViewTransaction *tv;

    tv = g_object_new(gnc_tree_view_transaction_get_type(), NULL);

    s_model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(model));
    //g_object_unref(G_OBJECT(model));
    gnc_tree_view_set_model(GNC_TREE_VIEW(tv), s_model);
    g_object_unref(G_OBJECT(s_model));

    tv->priv->book = gnc_tree_model_transaction_get_book(model);
    tv->priv->anchor = gnc_tree_model_transaction_get_anchor(model);
    tv->priv->reg_comm = xaccAccountGetCommodity(tv->priv->anchor);
    tv->priv->has_rate = TRUE;  //?

    gnc_tree_view_transaction_set_cols(tv, col_list);
    return tv;
}

/* CONTROL */

/* For handling keynav */
static gboolean
gtvt_key_press_cb(GtkWidget *treeview, GdkEventKey *event, gpointer unused)
{
    GtkTreeView *tv = GTK_TREE_VIEW(treeview);
    GtkTreeViewColumn *col;
    GtkTreePath *path = NULL;
    gboolean wrapped, tabbed = FALSE;

    if (event->type != GDK_KEY_PRESS) return TRUE;

    switch (event->keyval) {
    case GDK_Tab:
    case GDK_ISO_Left_Tab:
    case GDK_KP_Tab:
        tabbed = TRUE;
        break;
    case GDK_Return:
    case GDK_KP_Enter:
        break;
    default: 
	return TRUE;
    }

    gtk_tree_view_get_cursor(tv, &path, &col);
    if (!path) return TRUE;
    finish_edit(col);
    wrapped = gnc_tree_view_keynav(GNC_TREE_VIEW(tv), &col, path, event);

    if (wrapped && tabbed) {
    //if (g_object_get_data(G_OBJECT(tv), "data-edited") == TRUE)
    //{
        //g_print("Data reset\n");
        	mark_split_dirty(GNC_TREE_VIEW_TRANSACTION(tv), NULL, NULL);
    //}//if
        gtk_tree_view_get_cursor(tv, &path, &col);
        wrapped = gnc_tree_view_keynav(GNC_TREE_VIEW(tv), &col, path, event);
    }

    if (!path || !gnc_tree_view_path_is_valid(GNC_TREE_VIEW(tv), path)) {
        /* no need to restore cursor because we won't move. */
        //Only ask for confirmation if data was edited
        if (g_object_get_data(G_OBJECT(tv), "data-edited"))
        {
       	    transaction_changed_confirm(GNC_TREE_VIEW_TRANSACTION(tv), NULL);
            g_object_set_data(G_OBJECT(tv), "data-edited", FALSE);
        }//if
    } else
        gtk_tree_view_set_cursor(tv, path, col, TRUE);

    return TRUE;
}

void
gnc_tree_view_transaction_enter(GncTreeViewTransaction *tv)
{
    GdkEventKey event;
    event.type = GDK_KEY_PRESS;
    event.keyval = GDK_Return;
    gtvt_key_press_cb(GTK_WIDGET(tv), &event, NULL);
}

Account *
gnc_tree_view_transaction_get_anchor(GncTreeViewTransaction *tv)
{
    return tv->priv->anchor; /* cached from model */
}

void
gnc_tree_view_transaction_void(GncTreeViewTransaction *tv)
{
    const char *reason;
    GtkWidget *dialog, *entry;
    Transaction *trans;
    GladeXML *xml;
    gint result;
    
    trans = get_selected_trans(tv);
    if (!trans || xaccTransHasSplitsInState(trans, VREC))
        return;
    
    if (xaccTransHasReconciledSplits(trans) || 
        xaccTransHasSplitsInState(trans, CREC)) {
        gnc_error_dialog(NULL, _("You cannot void a transaction with "
                                 "reconciled or cleared splits."));
        return;
    }
    
    xml = gnc_glade_xml_new("register.glade", "Void Transaction");
    dialog = glade_xml_get_widget(xml, "Void Transaction");
    entry = glade_xml_get_widget(xml, "reason");
    
    result = gtk_dialog_run(GTK_DIALOG(dialog));
    if (result == GTK_RESPONSE_OK) {
        reason = gtk_entry_get_text(GTK_ENTRY(entry));
        if (reason == NULL)
            reason = "";
        begin_edit(tv, NULL, trans);
        xaccTransVoid(trans, reason);
    }
    
    /* All done. Get rid of it. */
    gtk_widget_destroy(dialog);
    g_object_unref(xml);
}

void
gnc_tree_view_transaction_unvoid(GncTreeViewTransaction *tv)
{

    Transaction *trans = get_selected_trans(tv);
    
    if (!trans || !xaccTransHasSplitsInState(trans, VREC))
        return;

    begin_edit(tv, NULL, trans);
    xaccTransUnvoid(trans);
}

static Transaction *clipboard_trans = NULL;
/* Must never dereference. */
static const Account *clipboard_acct = NULL;

void gnc_tree_view_transaction_copy_trans_to_clipboard(
    GncTreeViewTransaction *tv)
{
    Transaction *trans;

    g_return_if_fail(GNC_IS_TREE_VIEW_TRANSACTION(tv));

    trans = get_selected_trans(tv);
    if (!trans)
        return;

    if (clipboard_trans)
        xaccFreeTransaction(clipboard_trans);
        
    clipboard_trans = xaccDupeTransaction(trans);
    clipboard_acct = tv->priv->anchor;
}

void gnc_tree_view_transaction_paste_trans_from_clipboard(
    GncTreeViewTransaction *tv)
{
    Transaction *trans;

    g_return_if_fail(GNC_IS_TREE_VIEW_TRANSACTION(tv));

    trans = get_selected_trans(tv);
    if (!trans || !clipboard_trans)
        return;

    begin_edit(tv, NULL, trans);
    xaccTransCopyOntoAndChangeAccount(clipboard_trans, trans, clipboard_acct, 
                                      tv->priv->anchor);
}
