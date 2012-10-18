/********************************************************************\
 * gnc-tree-view-split-reg.c -- GtkTreeView implementation to       *
 *                     display registers   in a GtkTreeView.        *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
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

#include "gnc-tree-view.h"
#include "gnc-tree-view-split-reg.h"
#include "gnc-tree-model-split-reg.h"
#include "dialog-utils.h"
#include "gnc-gconf-utils.h"
#include "Transaction.h"
#include "Scrub.h"
#include "gnc-exp-parser.h"
#include "dialog-transfer.h"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_tree_view_split_reg_class_init (GncTreeViewSplitRegClass *klass);
static void gnc_tree_view_split_reg_init (GncTreeViewSplitReg *view);
static void gnc_tree_view_split_reg_dispose (GObject *object);
static void gnc_tree_view_split_reg_finalize (GObject *object);

static void cdf (GtkTreeViewColumn *col, GtkCellRenderer *renderer, GtkTreeModel *model,
                		GtkTreeIter  *iter, gpointer user_data);

static void gtv_split_reg_edited_cb (GtkCellRendererText *cell, const gchar *path_string,
                          	const gchar *new_text, gpointer _model);

static void start_edit (GtkCellRenderer *cr, GtkCellEditable *editable,
                       		const gchar *path, gpointer user_data);

static void begin_edit (GncTreeViewSplitReg *view, Split *split, Transaction *trans);

static void get_editable_start_editing_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
   				const gchar *path, gpointer user_data);

static void get_editable_start_editing_recn_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
   				const gchar *path, gpointer user_data);

static void gtv_split_reg_editing_canceled_cb (GtkCellRenderer *cr, gpointer user_data);

static void gtv_split_reg_match_selected_cb (GtkEntryCompletion *widget, GtkTreeModel *model,
                        GtkTreeIter *iter, gpointer user_data);

static void gtv_split_reg_changed_cb (GtkCellRendererCombo *widget, gchar *path_string,
                        GtkTreeIter *iter, gpointer user_data);

static gboolean gtv_split_reg_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data);

static void gtv_split_reg_motion_cb (GtkTreeSelection *sel, gpointer user_data);

static gboolean gtv_scroll_to_cell (GncTreeViewSplitReg *view);

static void gtv_split_reg_double_click_cb (GtkTreeView *treeview,
                                             GtkTreePath       *path,
                                             GtkTreeViewColumn *column,
                                             gpointer           user_data);

static gboolean transaction_changed_confirm (GncTreeViewSplitReg *view, Transaction *new_trans);


typedef enum {
    COL_DATE, //0
    COL_DUEDATE, //1
    COL_NUMACT, //2
    COL_DESCNOTES, //3
    COL_TRANSVOID, //4
    COL_RECN, //5
    COL_TYPE, //6
    COL_VALUE, //7
    COL_AMOUNT, //8
    COL_AMTVAL, //9
    COL_RATE, //10
    COL_PRICE, //11
    COL_DEBIT, //12
    COL_CREDIT, //13
    COL_BALANCE, //14
    COL_STATUS, //15
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


static ColDef all_tree_view_split_reg_columns[] = {
    {COL_DATE, GNC_TREE_MODEL_SPLIT_REG_COL_DATE,
     "Date", "date", "00/00/0000xxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_DUEDATE, GNC_TREE_MODEL_SPLIT_REG_COL_DUEDATE,
     "Due Date", "duedate", "00/00/0000xxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_NUMACT, GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT,
     "Num / Act", "numact", "0000xxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_DESCNOTES, GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES,
     "Description / Notes / Memo", "descnotes", "xxxxxxxxxxxxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_TRANSVOID, GNC_TREE_MODEL_SPLIT_REG_COL_TRANSVOID,
     "Transfer / Void", "transvoid", "xxxxxxxxxxxxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_RECN, -1,
     "R", "recn", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_TYPE, -1,
     "Type", "type", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_VALUE, -1,
     "Value", "value", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_AMOUNT, -1,
     "Amount", "amount", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_AMTVAL, -1,
     "Amount / Value", "amtval", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_RATE, -1,
     "Rate", "rate", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_PRICE, -1,
     "Price", "price", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_DEBIT, -1,
     "Debit", "debit", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_CREDIT, -1,
     "Credit", "credit", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, get_editable_start_editing_cb, NULL},

    {COL_BALANCE, -1,
     "Balance", "balance", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},

    {COL_STATUS, -1,
     " ", "status", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},
};


struct GncTreeViewSplitRegPrivate
{
    gboolean         disposed;
  
    Account         *anchor;
    gnc_commodity   *reg_comm;

    Split           *dirty_split;
    Transaction     *dirty_trans;

    Split           *btrans_split;

    GtkTreePath     *acct_edit_path;      // remember which row's account we're editing

    GtkTreePath     *current_path;        // remember what the current path is.

    GtkCellRenderer *temp_cr;         // Temp Cell Renderer reference

    gboolean         has_rate;          /* if set, the transfer dialog will never automatically pop-up */

    gboolean         full_refresh;

    gboolean         acct_short_names;
    gboolean         double_line;


    gboolean         default_set;
    gint             row_old;
    gint             row_now;

};


#define SPLIT_TRANS_STR _("-- Split Transaction --")

#define PINKCELL "#F8BEC6"
#define REDCELL "#F34943"
#define BLUECELL "#1D80DF"
#define BLACKCELL "#CBCBD2"

/* This could be a preference setting, The minimum length of characters in order to start completing */
#define KEY_LENGTH 2

#define GNC_TREE_VIEW_SPLIT_REG_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_VIEW_SPLIT_REG, GncTreeViewSplitRegPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_tree_view_split_reg_get_type(void)
{
    static GType gnc_tree_view_split_reg_type = 0;

    if (gnc_tree_view_split_reg_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncTreeViewSplitRegClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_tree_view_split_reg_class_init,
            NULL,
            NULL,
            sizeof (GncTreeViewSplitReg),
            0,
            (GInstanceInitFunc) gnc_tree_view_split_reg_init
        };

        gnc_tree_view_split_reg_type = g_type_register_static (GNC_TYPE_TREE_VIEW,
                                     "GncTreeViewSplitReg",
                                     &our_info, 0);
    }

    return gnc_tree_view_split_reg_type;
}


static void
gnc_tree_view_split_reg_class_init(GncTreeViewSplitRegClass *klass)
{
    GObjectClass *o_class;

    parent_class = g_type_class_peek_parent (klass);

    o_class = G_OBJECT_CLASS (klass);

    o_class->dispose =  gnc_tree_view_split_reg_dispose;
    o_class->finalize = gnc_tree_view_split_reg_finalize;

    g_type_class_add_private(klass, sizeof(GncTreeViewSplitRegPrivate));
}


static void
gnc_tree_view_split_reg_init (GncTreeViewSplitReg *view)
{
    view->priv = g_new0 (GncTreeViewSplitRegPrivate, 1);

    view->priv->current_path = gtk_tree_path_new_from_string ("99999");
    view->priv->full_refresh = TRUE;

    /* Setup the blank transaction split */
    view->priv->btrans_split = xaccMallocSplit (gnc_get_current_book());


    view->priv->acct_short_names = gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "show_leaf_account_names", NULL);
}


static void
gnc_tree_view_split_reg_dispose (GObject *object)
{
    GncTreeViewSplitReg *view;
    GncTreeViewSplitRegPrivate *priv;

    gnc_leave_return_if_fail (object != NULL);
    gnc_leave_return_if_fail (GNC_IS_TREE_VIEW_SPLIT_REG (object));

    view = GNC_TREE_VIEW_SPLIT_REG (object);
    priv = GNC_TREE_VIEW_SPLIT_REG_GET_PRIVATE (view);

    if (priv->disposed)
        return;
    priv->disposed = TRUE;

    if (G_OBJECT_CLASS (parent_class)->dispose)
        (* G_OBJECT_CLASS (parent_class)->dispose) (object);
}


static void
gnc_tree_view_split_reg_finalize (GObject *object)
{
    GncTreeViewSplitReg *view;

    gnc_leave_return_if_fail(object != NULL);
    gnc_leave_return_if_fail(GNC_IS_TREE_VIEW_SPLIT_REG (object));

    view = GNC_TREE_VIEW_SPLIT_REG (object);

    if (G_OBJECT_CLASS(parent_class)->finalize)
        (* G_OBJECT_CLASS(parent_class)->finalize) (object);
}


static GncTreeModelSplitReg *
get_split_reg_model_from_view (GncTreeViewSplitReg *view)
{
    GtkTreeModelSort *s_model = GTK_TREE_MODEL_SORT(
        gtk_tree_view_get_model (GTK_TREE_VIEW (view)));
    return GNC_TREE_MODEL_SPLIT_REG (gtk_tree_model_sort_get_model (s_model));
}


/* Set the grid lines to be solid */
static const gchar *rc_string =
{
"style \"solidTreeLines\"\n"
"{\n"
" GtkTreeView::grid-line-pattern = \"\1\"\n"
" GtkTreeView::grid-line-width = 1\n" 
"}\n"
"\n"
"class \"GtkTreeView\" style \"solidTreeLines\"\n"
};


static ViewCol *
gnc_tree_view_split_reg_get_colummn_list (GncTreeModelSplitReg *model)
{
g_print("Model-type is %d\n", model->type);
    switch (model->type)
    {
    case BANK_REGISTER2:
    case CASH_REGISTER2:
    case ASSET_REGISTER2:
    case CREDIT_REGISTER2:
    case LIABILITY_REGISTER2:
    case INCOME_REGISTER2:
    case EXPENSE_REGISTER2:
    case EQUITY_REGISTER2:
    case TRADING_REGISTER2:
    case INCOME_LEDGER2:
    case GENERAL_LEDGER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID, COL_RECN,
        COL_STATUS, COL_DEBIT, COL_CREDIT, COL_BALANCE, -1};
        return col_list;
        }
        break;

    case STOCK_REGISTER2:
    case CURRENCY_REGISTER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID, COL_RECN,
        COL_STATUS, COL_AMTVAL, COL_PRICE, COL_DEBIT, COL_CREDIT, COL_BALANCE, -1};
        return col_list;
        }
        break;

    case RECEIVABLE_REGISTER2:
    case PAYABLE_REGISTER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_TYPE, COL_DUEDATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID,
        COL_STATUS, COL_DEBIT, COL_CREDIT, COL_BALANCE, -1};
        return col_list;
        }

     case PORTFOLIO_LEDGER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID, COL_RECN,
        COL_STATUS, COL_AMTVAL, COL_PRICE, COL_DEBIT, COL_CREDIT, -1};
        return col_list;
        }

    case SEARCH_LEDGER2: //FIXME Not Setup yet

    default:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID, COL_RECN, COL_STATUS,
        COL_TYPE, COL_VALUE, COL_AMOUNT, COL_RATE, COL_PRICE, COL_DEBIT, COL_CREDIT,
        COL_BALANCE, -1};
        return col_list;
        }
    }
}


/* Creates a treeview with the list of fields */
static GncTreeViewSplitReg *
gnc_tree_view_split_reg_set_cols (GncTreeViewSplitReg *view,
                                    const ViewCol col_list[])
{
    int i = 0;

    GncTreeModelSplitReg *model;
    model = get_split_reg_model_from_view (view);

    while (col_list && col_list[i] != -1) {
        GList *renderers;
        GtkCellRenderer *cr;
        GtkTreeViewColumn *col;
        ColDef def;

        int j, ncol = G_N_ELEMENTS (all_tree_view_split_reg_columns);

        for (j = 0; j < ncol; j++) {
            if (col_list[i] == all_tree_view_split_reg_columns[j].viewcol) {
                def = all_tree_view_split_reg_columns[j];
                break;
            }
        }
        if (j == ncol) {
            PERR("Failed to find column definition.");
            i++;
            continue;
        }
        if (col_list[i] == COL_TRANSVOID) {

            col = gnc_tree_view_add_combo_column (
                GNC_TREE_VIEW (view), def.title, def.pref_name, def.sizer,
                def.modelcol, def.visibility_model_col,
                GTK_TREE_MODEL( gnc_tree_model_split_reg_get_acct_list(model)), 0, def.sort_fn);

        } else if (col_list[i] == COL_DATE) {
            col = gnc_tree_view_add_date_column (
                GNC_TREE_VIEW (view), def.title, def.pref_name, NULL, def.sizer,
                def.modelcol, def.visibility_model_col, def.sort_fn);

        } else if (col_list[i] == COL_NUMACT){ 
            col = gnc_tree_view_add_combo_column (
                GNC_TREE_VIEW (view), def.title, def.pref_name, def.sizer,
                def.modelcol, def.visibility_model_col,
                GTK_TREE_MODEL( gnc_tree_model_split_reg_get_numact_list(model)), 0, def.sort_fn);

        } else { 
            col = gnc_tree_view_add_text_column (
                GNC_TREE_VIEW (view), def.title, def.pref_name, NULL, def.sizer,
                def.modelcol, def.visibility_model_col, def.sort_fn);
        } 

        g_object_set_data (G_OBJECT (col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (col)); //FIXME I am only looking at one renderer per cell.
        g_assert (g_list_length (renderers) == 1);
        cr = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        /* Setup cell background color and default alignment */
        g_object_set (cr, "xalign", 1.0, NULL);
        gtk_tree_view_column_add_attribute (col, cr, "cell-background", GNC_TREE_MODEL_SPLIT_REG_COL_COLOR);

        if (def.editing_started_cb) {
            //Store the position of the column in the model
            g_object_set_data (G_OBJECT(cr), "model_column", GINT_TO_POINTER (def.modelcol));
            g_object_set_data (G_OBJECT(cr), "column_name", GINT_TO_POINTER (def.pref_name));
            g_signal_connect (G_OBJECT(cr), "editing-started", (GCallback) def.editing_started_cb, view);
        }

        //Connect editing-canceled signal so that edit-cancelled can be set appropriately
        g_signal_connect (G_OBJECT(cr), "editing-canceled", G_CALLBACK (gtv_split_reg_editing_canceled_cb), view);

        // Set Columns to be resizable default.
        g_object_set (G_OBJECT(col), "resizable", TRUE, NULL);

        // We do not want columns to be reorderable.
        g_object_set(G_OBJECT(col), "reorderable", FALSE, NULL);

        if (def.edited_cb) {
            g_object_set (G_OBJECT (cr), "editable", TRUE, NULL);
            g_signal_connect (G_OBJECT (cr), "edited", (GCallback) def.edited_cb, view);
        }
        g_object_set_data (G_OBJECT (cr), "view_column", GINT_TO_POINTER (def.viewcol));
        gtk_tree_view_column_set_cell_data_func ( col, cr, cdf, view, NULL);
        i++;
    }
    gtk_tree_selection_set_mode(gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), GTK_SELECTION_BROWSE);

    /* This will expand to splits on double clicking at current position */
    g_signal_connect (GTK_TREE_VIEW (view), "row-activated", G_CALLBACK (gtv_split_reg_double_click_cb), NULL);

    g_signal_connect(gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), "changed", G_CALLBACK (gtv_split_reg_motion_cb), view);

    //Add a data-edited property to keep track of transaction edits
    g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));

    g_signal_connect_after (G_OBJECT (view), "key-press-event", G_CALLBACK (gtv_split_reg_key_press_cb), NULL);
    return view;
}


static gboolean
gnc_tree_view_split_reg_set_format (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *path;
    RowDepth depth;
    gint index = 0;
    gint total_num = 0;
    gint *indices;
    gint row_now;
    gint row_old;

    model = get_split_reg_model_from_view (view);

    row_now = view->priv->row_now;
    row_old = view->priv->row_old;
    total_num = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (model), NULL);
    depth = gnc_tree_view_reg_get_selected_row_depth (view);

g_print("gnc_tree_view_split_reg_set_format row_now is %d, row_old is %d, total is %d\n", row_now, row_old, total_num);

    /* scroll the view */
    if (row_now == total_num -1)
        gtv_scroll_to_cell (view);

if ((row_now != row_old) || (view->priv->default_set == TRUE))
{
view->priv->default_set = FALSE;

    if (model->style == REG2_STYLE_JOURNAL)
    {
g_print("gnc_tree_view_split_reg_set_format journal\n");
        path = gtk_tree_path_new_first ();
        indices = gtk_tree_path_get_indices (view->priv->current_path);

        /* we need do this when we remove the blank split from the last transaction */
        if (indices[0] != total_num -1)
            total_num = total_num -1;

        while (index < total_num)
        {
            gtk_tree_path_down (path); //TROW2
            gtk_tree_view_expand_to_path (GTK_TREE_VIEW (view), path);
            gtk_tree_path_up (path);   //TROW1

            index = index + 1;
            if (index == total_num)
                break;

            gtk_tree_path_next (path); //Next Transaction
        }
        gtk_tree_path_free (path);
        return (FALSE);
    }

    if (!model->use_double_line)
    {
g_print("gnc_tree_view_split_reg_set_format single\n");
        path = gtk_tree_path_new_first ();
        while (index < gtk_tree_model_iter_n_children (GTK_TREE_MODEL (model), NULL))
        {
            gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), path);
            gtk_tree_path_next (path); //Next Transaction
            index = index + 1;
        }
        gtk_tree_path_free (path);
    }

    if (model->use_double_line)
    {
g_print("gnc_tree_view_split_reg_set_format double\n");
        path = gtk_tree_path_new_first ();
        while (index < gtk_tree_model_iter_n_children (GTK_TREE_MODEL (model), NULL))
        {
            gtk_tree_view_expand_to_path (GTK_TREE_VIEW (view), path);
            gtk_tree_path_down (path);
            gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), path);
            gtk_tree_path_up (path);
            gtk_tree_path_next (path); //Next Transaction
            index = index + 1;
        }
        gtk_tree_path_free (path);
    }

    /* This expands to split from top level auto.. */
    if ((model->style == REG2_STYLE_AUTO_LEDGER) || (model->style == REG2_STYLE_JOURNAL))
    {
        GtkTreePath *expand_path;
g_print("gnc_tree_view_split_reg_set_format auto\n");
        expand_path = gtk_tree_path_new_from_indices (row_now, -1);
        gtk_tree_view_expand_row (GTK_TREE_VIEW (view), expand_path, TRUE);
        gtk_tree_path_free (expand_path);
    }
}
    return (FALSE);
}


GncTreeViewSplitReg*
gnc_tree_view_split_reg_new_with_model (GncTreeModelSplitReg *model)
{
    GtkTreeModel *s_model;
    GncTreeViewSplitReg *view;
    GtkCellRenderer *cr;
    GtkTreeViewColumn *col;
    GtkTreeSelection    *selection;

    gtk_rc_parse_string (rc_string);

    view = g_object_new (gnc_tree_view_split_reg_get_type(), NULL);
    g_object_set (view, "name", "split_reg_tree", NULL);

    s_model = gtk_tree_model_sort_new_with_model (GTK_TREE_MODEL (model));
    // do not do this - g_object_unref (G_OBJECT (model));

    gnc_tree_view_set_model (GNC_TREE_VIEW (view), s_model);
    g_object_unref (G_OBJECT (s_model));

    view->priv->anchor = gnc_tree_model_split_reg_get_anchor (model);
    view->priv->reg_comm = xaccAccountGetCommodity (view->priv->anchor);
    view->priv->has_rate = TRUE;

    gnc_tree_view_split_reg_set_cols (view, gnc_tree_view_split_reg_get_colummn_list (model));

    /* Set default visibilities */
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (view), TRUE);

    /* TreeView Grid lines */
    if (gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "draw_horizontal_lines", NULL))
    {
        if (gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "draw_vertical_lines", NULL))
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), GTK_TREE_VIEW_GRID_LINES_BOTH);
        else
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), GTK_TREE_VIEW_GRID_LINES_HORIZONTAL);
    }
    else if (gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "draw_vertical_lines", NULL))
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), GTK_TREE_VIEW_GRID_LINES_VERTICAL);
    else
        gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), GTK_TREE_VIEW_GRID_LINES_NONE);

    /* Expanders off */
    /* gtk_tree_view_set_show_expanders (GTK_TREE_VIEW (view), FALSE); */

    /* Tree Selection */
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));

    gtk_tree_selection_unselect_all (selection);

    return view;
}


/* This allows the blocking / unblocking of selection */
void
gnc_tree_view_split_reg_block_selection (GncTreeViewSplitReg *view, gboolean block)
{
    if (block)
        g_signal_handlers_block_by_func (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), gtv_split_reg_motion_cb, view);
    else
        g_signal_handlers_unblock_by_func (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), gtv_split_reg_motion_cb, view);
}


/* Set the default selection path */
void
gnc_tree_view_split_reg_default_selection (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    GtkTreePath *path;
    gint *indices;

    model = get_split_reg_model_from_view (view);

    view->priv->default_set = TRUE;

    /* Set the default start position to end of list */
    if (gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, view->priv->current_path))
        indices = gtk_tree_path_get_indices (view->priv->current_path);
    else
        /* both values NULL will return last in list */
        indices = gtk_tree_path_get_indices (gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, NULL));

    path = gtk_tree_path_new_from_indices (indices[0], -1);

g_print(" gnc_tree_view_split_reg_default_selection current_path is %s, new path is %s\n", gtk_tree_path_to_string (view->priv->current_path),
        gtk_tree_path_to_string (path));

    gtk_tree_path_free (view->priv->current_path);
    view->priv->current_path = gtk_tree_path_copy (path);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);

    gtk_tree_path_free (path);

    /* scroll when view is idle */
    g_idle_add ((GSourceFunc)gtv_scroll_to_cell, view );
}


/* Sets read only flag */
void
gnc_tree_view_split_reg_set_read_only (GncTreeViewSplitReg *view, gboolean read_only)
{
    GncTreeModelSplitReg *model;

    model = get_split_reg_model_from_view (view);

    model->read_only = read_only;
}


/* Do we need an exchange rate */
static gboolean
needs_exchange_rate (GncTreeViewSplitReg *view, Transaction *trans, Split *split)
{
    gnc_commodity *split_com, *txn_curr, *reg_com;
g_print("needs_exchange_rate trans %p and split %p\n", trans, split);
    if (view->priv->has_rate) return FALSE;

    txn_curr = xaccTransGetCurrency (trans);
    split_com = xaccAccountGetCommodity (xaccSplitGetAccount (split));
    if (split_com && txn_curr && !gnc_commodity_equiv (split_com, txn_curr))
        return TRUE;

    reg_com = view->priv->reg_comm;
    if (reg_com && split_com && !gnc_commodity_equiv (reg_com, split_com))
        return TRUE;

    return FALSE;
}


/* Either sets the value and amount for split and returns TRUE, or
   does nothing and returns FALSE. */
static gboolean
handle_exchange_rate (GncTreeViewSplitReg *view, gnc_numeric amount, Transaction *trans, Split *split)
{
    XferDialog *xfer;
    gboolean rate_split_ok, rate_reg_ok;
    gnc_numeric rate_split, rate_reg, value;
    gnc_commodity *xfer_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));
    gnc_commodity *reg_comm = view->priv->reg_comm;
    gnc_commodity *trans_curr = xaccTransGetCurrency (trans);

g_print("handle_exchange_rate trans %p and split %p\n", trans, split);

    /* Rate from trans-curr to split-comm */
    rate_split_ok = xaccTransGetRateForCommodity (trans, xfer_comm, split, &rate_split);

    /* Rate from trans-curr to reg-comm */
    rate_reg_ok = xaccTransGetRateForCommodity(trans, reg_comm, split, &rate_reg);

    if (rate_reg_ok && rate_split_ok)
    {
        value = gnc_numeric_div (amount, rate_reg, gnc_commodity_get_fraction (trans_curr), GNC_HOW_DENOM_REDUCE);
        amount = gnc_numeric_mul (value, rate_split, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }
    else
    {
        rate_split = gnc_numeric_create (1, 1);

        //g_message("reg amt: %s", gnc_numeric_to_string(amount));
        /* create the exchange-rate dialog */
        xfer = gnc_xfer_dialog (NULL, NULL); //FIXME: get parent window
        gnc_xfer_dialog_is_exchange_dialog (xfer, &rate_split);

        /* fill in the dialog entries */
        gnc_xfer_dialog_set_description (xfer, xaccTransGetDescription (trans));
        gnc_xfer_dialog_set_memo (xfer, xaccSplitGetMemo (split));
        gnc_xfer_dialog_set_num (xfer, xaccTransGetNum (trans));
        gnc_xfer_dialog_set_date (xfer, timespecToTime_t (xaccTransRetDatePostedTS (trans)));

        value = amount;
/*FIXME        if (gnc_xfer_dialog_run_exchange_dialog(
                xfer, &rate_split, &value, reg_comm, trans, xfer_comm))
            return FALSE; */
        amount = gnc_numeric_mul (value, rate_split, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }
    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, value);
    g_message ("split amt=%s; split val=%s", gnc_numeric_to_string (amount), gnc_numeric_to_string (value));
    return TRUE;
}




static void
set_value_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input)
{
    Account *anchor = view->priv->anchor;
    Account *acct = xaccSplitGetAccount (split);
    gnc_commodity *currency = xaccTransGetCurrency (trans);
    gnc_numeric value, amount, rate;

g_print("set_value_for trans %p and split %p\n", trans, split);

//    if (xaccSplitGetAccount (split) == NULL) //FIXME this would make sure we have an account, not sure its valid.
//        xaccTransScrubOrphans (trans);

    if (gnc_numeric_zero_p (input))
    {
        xaccSplitSetValue (split, input);
        xaccSplitSetAmount (split, input);
        return;
    }

    if (needs_exchange_rate (view, trans, split))
    {
        if (handle_exchange_rate (view, input, trans, split))
        {
            ; //FIXME ??????
        }
        return;
    }

    /* Context determines the interpretation of the input.  If the
       treeview is anchored to an account then the input is
       interpreted as being in the Account's commodity.  Otherwise,
       it's interpreted as being in the Transaction's currency. */
    if (anchor)
    {
        gnc_commodity *reg_com = view->priv->reg_comm;
        gnc_commodity *split_com = xaccAccountGetCommodity (acct);
        /* Convert from the anchor account's commodity to
           trans currency */
        //xaccSplitSetAmount (split, input);
        if (gnc_commodity_equiv (currency, reg_com))
            value = input;
        else 
       {
            if (!xaccTransGetRateForCommodity (trans, reg_com, NULL, &rate))
                return;
            //rate = xaccTransGetAccountConvRate(trans, anchor);
            if (gnc_numeric_zero_p (rate))
            {
                // FIXME: probably wrong.
                xaccTransSetCurrency (trans, reg_com);
                value = input;
            }
            else
                value = gnc_numeric_div (
                    input, rate,
                    GNC_DENOM_AUTO, //?
                    //gnc_commodity_get_fraction(currency),
                    GNC_HOW_RND_ROUND);
        }
        xaccSplitSetValue (split, value);
        //return;
        if (gnc_commodity_equiv (split_com, reg_com))
            amount = input;
        else
        {
            rate = xaccTransGetAccountConvRate (trans, acct);
            amount = gnc_numeric_mul (value, rate, xaccAccountGetCommoditySCU (acct), GNC_HOW_RND_ROUND);
        }
        xaccSplitSetAmount (split, amount);
    }
    else
    {
        //FIXME: untested; assumes entry in the trans currency
        //gnc_commodity *split_com = xaccAccountGetCommodity(acct);
        value = input;
        xaccSplitSetValue (split, value);
        //g_assert (split_com == currency);
        //FIXME: obsolete
        /* For a split belonging to another account */
        rate = xaccTransGetAccountConvRate (trans, acct);
        amount = gnc_numeric_mul (value, rate, xaccAccountGetCommoditySCU (acct), GNC_HOW_RND_ROUND);
        if (gnc_numeric_check (amount) == GNC_ERROR_OK)
        {
            xaccSplitSetAmount (split, amount);
        }
    }
}


/* Returns a value for display. */
static gnc_numeric
get_value_for (GncTreeViewSplitReg *view, Transaction *trans,
              Split *split, gboolean is_blank)
{
    gnc_commodity *currency = xaccTransGetCurrency(trans);
    gnc_numeric total;

//g_print("get_value_for trans %p and split %p is_blank %d\n", trans, split, is_blank);


    total = xaccSplitGetValue (split);

    if (is_blank && gnc_numeric_zero_p (total)) {
        gnc_numeric rate;
        total = gnc_numeric_neg (xaccTransGetImbalanceValue (trans));
        if (!gnc_numeric_zero_p (total)) {

            if (!xaccTransGetRateForCommodity (trans, view->priv->reg_comm, NULL, &rate))
                return gnc_numeric_zero();

            total = gnc_numeric_mul (
                total, rate,
                gnc_commodity_get_fraction (currency),
                GNC_HOW_RND_ROUND);
        }
    } else {
        if (!gnc_numeric_zero_p (total) &&
            gnc_numeric_check (total) == GNC_ERROR_OK) {

            /* fixme: if needs conversion? */
            gnc_commodity *commodity = view->priv->reg_comm;
/*FIXME  ??          if (commodity && !gnc_commodity_equiv(commodity, currency))
                total = xaccSplitConvertAmount(split, commodity); */
        }
    }
    return total;
}


static void
set_amount_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input)
{
    Account *acct = xaccSplitGetAccount (split);
    gnc_commodity *split_com = xaccAccountGetCommodity (acct);
    gnc_commodity *currency = xaccTransGetCurrency (trans);

g_print("set_amount_for trans %p and split %p\n", trans, split);

    xaccSplitSetAmount (split, input);
    if (gnc_commodity_equiv (currency, split_com))
        xaccSplitSetValue (split, input);

    return;
}


static gnc_numeric
get_rate_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gboolean is_blank)
{
    gnc_numeric num;

g_print("get_rate_for trans %p and split %p is_blank %d\n", trans, split, is_blank);

    num = get_value_for (view, trans, split, is_blank);
    num = gnc_numeric_div ( xaccSplitGetAmount (split), num, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    return num;
}


static void
set_rate_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input, gboolean is_blank)
{
    gnc_commodity *split_comm;

    gnc_numeric old_rate = get_rate_for (view, trans, split, is_blank);
    gnc_numeric factor = gnc_numeric_div (input, old_rate, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    split_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));
/*FIXME     xaccTransAdjustRateForCommodity(trans, split_comm, factor); */

g_print("set_rate_for trans %p and split %p is_blank %d\n", trans, split, is_blank);

#if JUNK
    reg_comm = view->priv->reg_comm;
    if (xaccTransGetRateForCommodity (trans, reg_comm, split, &reg_rate))
    {
        input = gnc_numeric_div (input, reg_rate, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
        /* input is now the rate from the transaction currency to the
           split_com */
    }

    if (gnc_numeric_zero_p (val) && gnc_numeric_zero_p (amt))
    {
        gnc_numeric one = gnc_numeric_create (1, 1);
        xaccSplitSetAmount (split, one);
        amt = one;
    }

    if (gnc_numeric_zero_p (val))
    {
        val = gnc_numeric_div (input, amt, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }
    else
    {
        amt = gnc_numeric_mul (input, val, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }

    amt = gnc_numeric_mul (get_value_for(view, trans, split, FALSE /*FIXME*/),
                          input,
                          GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    set_amount_for (view, trans, split, amt);

    //xaccSplitSetValue (split, val);
    //xaccSplitSetAmount (split, amt);

#endif
}



/*FIXME This could be a list...... Returns the other Split based on the current Account */
/*FIXME Check, This might only be used with two splits */
static Split *
get_other_split (GncTreeViewSplitReg *view, Transaction *trans)
{
    int i;
    Split *split = NULL;
    Account *anchor = view->priv->anchor;

    for (i = 0; (split = xaccTransGetSplit(trans, i)); i++) {
        if (anchor == xaccSplitGetAccount(split))
            return xaccSplitGetOtherSplit(split);
    }
    return NULL;
}


/* Returns a Split that matches the current Account */
static Split *
get_this_split (GncTreeViewSplitReg *view, Transaction *trans)
{
    int i;
    Split *split = NULL;
    Account *anchor = view->priv->anchor;

    for (i = 0; (split = xaccTransGetSplit (trans, i)); i++) {
        if (anchor == xaccSplitGetAccount (split))
            return split;
    }
    return NULL;
}


/* The returned Splits may be newly created and not yet belong to trans. */
static gboolean
get_split_pair (GncTreeViewSplitReg *view, Transaction *trans, Split **osplit, Split **split)
{
    QofBook       *book; //do we have this

    gint count = xaccTransCountSplits (trans);
    Account *anchor = view->priv->anchor;

//FIXME Probably needs more work 

    book = gnc_get_current_book();

g_print("get_split_pair trans is %p, osplit is %p and split is %p\n", trans, *osplit, *split);

    if (count == 0)
    {
        *split = xaccMallocSplit (book);
        xaccSplitSetAccount (*split, anchor);
        xaccSplitSetParent (*split, trans);
        *osplit = view->priv->btrans_split;
    }
    else if (count == 1)
    {
        *split = xaccTransGetSplit (trans, 0);
        *osplit = view->priv->btrans_split;
    }
    else if (count == 2)
    {
        int i;
        Split *s;

        for (i = 0; (s = xaccTransGetSplit (trans, i)); i++)
        {
            if (anchor == xaccSplitGetAccount (s))
            {
                *split = s;
                break;
            }
        }
        //*split = get_this_split (view, trans);
        g_assert (*split);
        *osplit = get_other_split (view, trans);
        g_assert (*osplit);
    }
    else
        return FALSE;
g_print("get_split_pair return - trans is %p, count is %d, osplit is %p and split %p is set to anchor %p\n", trans, count, *osplit, *split, anchor);
    return TRUE;
}



/* poor name: really means: If this is the blank split, it may now
   eventually graduate to a real split. The trans must already be
   opened for editing because the split will be added to the
   transaction if hasn't been already. */
static void
mark_split_dirty (GncTreeViewSplitReg *view, Transaction *trans, Split *split)
{
g_print("mark_split_dirty trans %p and split %p\n", trans, split);
//    if (split != view->priv->dirty_split && view->priv->dirty_split)
  

    if (split != view->priv->dirty_split)
    {
g_print("commiting dirty split1\n");
        if (view->priv->dirty_split)
        {
g_print("commiting dirty split2\n");
        gnc_tree_model_split_reg_commit_split (get_split_reg_model_from_view (view), view->priv->dirty_split);
        }
    }

    if (split && trans && xaccSplitGetParent (split) != trans)
    {
g_print("commiting dirty split3\n");

        if (xaccTransCountSplits (trans) == 0)
        {
g_print("commiting dirty split4\n");
            xaccSplitSetAccount (split, view->priv->anchor);
        }
        xaccSplitSetParent (split, trans);
    }

    view->priv->dirty_split = split;
}



/* Does this transaction have any Imbalance splits */
static gboolean
get_imbalance (Transaction *trans)
{
    int i;
    Split *split = NULL;
    const gchar *acc_name;
    const gchar *prefix = _("Imbalance"); 

    for (i = 0; (split = xaccTransGetSplit (trans, i)); i++)
    {
        if (xaccSplitGetAccount (split) != NULL)
        {
            acc_name = xaccAccountGetName (xaccSplitGetAccount (split));

            if (g_str_has_prefix (acc_name, prefix))
                return TRUE;
        }
    }
    return FALSE;

}


static gboolean
get_model_iter_from_view_string(GncTreeViewSplitReg *view,
                                const gchar *path_string, GtkTreeIter *iter)
{
    GtkTreeModelSort *s_model;
    GtkTreeIter s_iter;

    s_model = GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view)));
    if (!gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (s_model), &s_iter, path_string))
    {
        iter = NULL;
        return FALSE;
    }

    gtk_tree_model_sort_convert_iter_to_child_iter (s_model, iter, &s_iter);
    return TRUE;
}

static gboolean
get_model_iter_from_selection (GncTreeViewSplitReg *view,
                              GtkTreeSelection *sel, GtkTreeIter *iter)
{
    GtkTreeModel *s_model;
    GtkTreeIter s_iter;

    if (gtk_tree_selection_get_selected (sel, &s_model, &s_iter))
    {
//g_print("s_iter is '%s'\n", gtk_tree_model_get_string_from_iter ( s_model, &s_iter));
        gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), iter, &s_iter);
//g_print("iter is '%s'\n", gtk_tree_model_get_string_from_iter (gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (s_model)), iter));
        return TRUE;
    }
    return FALSE;
}





/* Instead of setting a different cellDataFunc for each column, we just
   collect everything here and use this one func. */
static void
cdf (GtkTreeViewColumn *col, GtkCellRenderer *cell, GtkTreeModel *s_model,
    GtkTreeIter *s_iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    GtkTreePath *path;
    ViewCol viewcol;
    Transaction *trans;
    Split *split;
    gboolean is_split, is_blank, is_trow1, is_trow2;
    gboolean editable = FALSE, expanded = FALSE;
    gboolean read_only = FALSE;
    gint num_of_splits = 0;
    gnc_numeric num;
    const gchar *s = "";

    RowDepth depth;

    gint *indices;

    Account *anchor = view->priv->anchor;

    ENTER("");

    model = get_split_reg_model_from_view (view);

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), &iter, s_iter);

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    g_return_if_fail (gnc_tree_model_split_reg_get_split_and_trans(
                         GNC_TREE_MODEL_SPLIT_REG(model), &iter,
                          &is_trow1, &is_trow2, &is_split, &is_blank,
                          &split, &trans));

    depth = gtk_tree_path_get_depth (gtk_tree_model_get_path (GTK_TREE_MODEL (model), &iter));

// g_print("cdf split %p, trans %p, is_trow1 = %d, is_trow2 = %d, is_split = %d, is_blank = %d\n",
//                                        split, trans, is_trow1, is_trow2, is_split, is_blank);

//g_print(" cdf depth is %d\n", depth);

    /* Get the read only model setting */
    gtk_tree_model_get (GTK_TREE_MODEL (model), &iter, GNC_TREE_MODEL_SPLIT_REG_COL_RO, &read_only, -1);

    indices = gtk_tree_path_get_indices (gtk_tree_model_get_path (GTK_TREE_MODEL (model), &iter));

//g_print(" cdf path is %s\n", gtk_tree_path_to_string (gtk_tree_model_get_path (GTK_TREE_MODEL (model), &iter)));

    /* Lets see if the splits are expanded and count them */
    if (is_trow1 || is_trow2)
    {
        num_of_splits = xaccTransCountSplits (trans);
        path = gtk_tree_model_get_path (s_model, s_iter);
        if (is_trow1)
            gtk_tree_path_down (path); /* Move the path down to trow2 */
        expanded = gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), path);
        gtk_tree_path_free (path);
    }

    switch (viewcol) {
    case COL_DATE:
        /* Column is DATE */
        if (is_split)
            g_object_set(cell, "cell-background", "white", (gchar*)NULL);

        if (is_trow1) {
            Timespec ts = {0,0};
            xaccTransGetDatePostedTS (trans, &ts);
            //If the time returned by xaccTransGetDatePostedTS is 0 then assume it
            //is a new transaction and set the time to current time to show current
            //date on new transactions
            if (ts.tv_sec == 0)
            {
                ts.tv_sec = time (NULL);
                //xaccTransSetDatePostedSecs(trans, ts.tv_sec);
            }//if
            s = gnc_print_date(ts);
            editable = TRUE;
        }
        else {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        /*FIXME This will remove the calander buttons if FALSE, preference may be ? */
        g_object_set (cell, "use_buttons", TRUE, NULL ); 
        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_DUEDATE:
        /* Column is DUE DATE */
        if (is_split)
            g_object_set (cell, "cell-background", "white", (gchar*)NULL);

        if (is_trow1) {
            Timespec ts = {0,0};
            char type;

            type = xaccTransGetTxnType (trans);

            /* Only print the due date for invoice transactions */
            if (type == TXN_TYPE_INVOICE)
            {
                xaccTransGetDateDueTS (trans, &ts);
                s = gnc_print_date (ts);
                editable = FALSE;
            }
            else {
                s = "";
                editable = FALSE;
            }
        }
        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_NUMACT:
        /* Column is NUM / ACT */
        /* Override default alignment */
        g_object_set (cell, "xalign", 0.0, NULL );

        editable = TRUE;

        if (is_trow1)
            s = xaccTransGetNum (trans);
        else if (is_trow2 && !expanded)
            s = xaccSplitGetAction (get_this_split (view, trans));
        else if (is_split)
            s = xaccSplitGetAction (split);
        else
        {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_DESCNOTES:
        /* Column is DESCRIPTION / NOTES */
        /* Override default alignment */
        g_object_set( cell, "xalign", 0.0, NULL );
        if (is_trow1)
            s =  xaccTransGetDescription (trans);
        else if (is_trow2)
            s =  xaccTransGetNotes (trans);
        else if (is_split)
            s = xaccSplitGetMemo (split);
        editable = TRUE;

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_TRANSVOID:
        /* Column is TRANSFER / VOID */
        /* Not sure if this will stay here, this sets the combo column
           0 for short account names, 1 for long */
        if (view->priv->acct_short_names)
            g_object_set (G_OBJECT (cell), "text-column", 0, NULL );
        else
            g_object_set (G_OBJECT (cell), "text-column", 1, NULL );

        if (is_trow1)
        {
            if (expanded)
                s = ""; /* blank-out if splits are visible */
            else if (num_of_splits == 2)
            {
                Account *acct;
                Split *osplit;
                osplit = get_other_split (view, trans);
                acct = xaccSplitGetAccount (osplit);
                if (view->priv->acct_short_names)
                    s = xaccAccountGetName (acct);
                else
                    s = gnc_account_get_full_name (acct);
            }
            else if (num_of_splits == 0 || num_of_splits == 1)
            {
                Account *acct;
                acct = xaccSplitGetAccount (view->priv->btrans_split);
                if (acct != NULL)
                {
                    if (view->priv->acct_short_names)
                        s = xaccAccountGetName (acct);
                    else
                        s = gnc_account_get_full_name (acct);
                }
                else
                    s = "";
            }
                    
            if (num_of_splits > 2)
            {
                s = SPLIT_TRANS_STR;
            }
            editable = anchor && !expanded && (num_of_splits < 3);
        }
        if (is_trow2)
        {
            s = xaccTransGetVoidReason (trans); // This is the Void Reason
            editable = FALSE;
        }
        if (is_split)
        {
            Account *acct = xaccSplitGetAccount (split);

            if (xaccTransCountSplits (trans) == 0) // First split on blank transaction 
                acct = anchor;

            if (acct != NULL)
            {
                if (view->priv->acct_short_names)
                    s = xaccAccountGetName (acct);
                else
                    s = gnc_account_get_full_name (acct);
            }
            else
                s = "";

            if (anchor == acct)
                editable = FALSE;
            else
                editable = TRUE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_RECN:
        /* Column is RECN */
        if (is_trow1 && !expanded) {
            char rec = xaccSplitGetReconcile (get_this_split (view, trans));
            if (rec == VREC || rec == FREC || rec == YREC)
                editable = FALSE;
            else
                editable = TRUE;

            if (rec != ' ')
                s = gnc_get_reconcile_str (rec);
            else
                s = gnc_get_reconcile_str (NREC);

            g_object_set (cell, "text", s, NULL);

        } else {
            s = "";
            editable = FALSE;
            g_object_set (cell, "text", s, NULL);
        }

        if (is_split) {
            char rec = xaccSplitGetReconcile (split);
            if (rec == VREC || rec == FREC || rec == YREC)
                editable = FALSE;
            else
                editable = TRUE;

            if (rec != ' ')
                s = gnc_get_reconcile_str (rec);
            else
                s = gnc_get_reconcile_str (NREC);

            g_object_set (cell, "text", s, NULL);
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "editable", editable, NULL);
        break;

    case COL_TYPE:
        /* Column is TYPE */
        if (is_split)
            g_object_set (cell, "cell-background", "white", (gchar*)NULL);

        if (is_trow1) {
            static char ss[2];
            char type = xaccTransGetTxnType (trans);
            if (type == TXN_TYPE_NONE)
                type = '?';

            ss[0] = type;
            ss[1] = '\0';
            g_object_set (cell, "text", ss, NULL);
        }
        else
        {
            s = "";
            g_object_set (cell, "text", s, NULL);
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "editable", editable, NULL);
        break;

    case COL_VALUE:
        /* Column is VALUE */
        if (is_split) {
            gnc_numeric amt = xaccSplitGetValue (split);
            s = xaccPrintAmount (amt, gnc_commodity_print_info (xaccTransGetCurrency (trans), FALSE));
            editable = TRUE;
        } else {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_RATE:
        /* Column is RATE */
        if ((is_trow1)||(is_trow2)) {
            s = "";
            editable = FALSE;
        } else {
            gnc_commodity *split_com = xaccAccountGetCommodity (xaccSplitGetAccount (split));
            num = get_rate_for (view, trans, split, is_blank);
            if (gnc_numeric_check(num) == GNC_ERROR_OK) {
                s = xaccPrintAmount (num, gnc_split_amount_print_info (split, FALSE));
                editable = !gnc_numeric_zero_p(num) &&
                    !gnc_commodity_equiv(split_com, view->priv->reg_comm);
            } else {
                s = "";
                editable = FALSE;
            }
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_AMOUNT:
        /* Column is AMOUNT */
       if (is_split) {
            gnc_numeric amt = xaccSplitGetAmount (split);
            s = xaccPrintAmount (amt, gnc_account_print_info (xaccSplitGetAccount (split), FALSE));
            editable = TRUE;
        } else {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_AMTVAL:
        /* Column is AMOUNT / VALUE */
        if (is_trow2) {
            s = "";
            editable = FALSE;
        } else if (is_trow1) {
            if (anchor) {
                gnc_numeric amt = xaccSplitGetValue (get_this_split (view, trans));
                editable = !expanded && (num_of_splits < 3);
                if(expanded)
                    s = "";
                else
                    s = xaccPrintAmount (amt, gnc_commodity_print_info( xaccTransGetCurrency (trans), FALSE));
            } else {
                s = "";
                editable = FALSE;
            }
        }

        if (is_split) {
            if (split == get_this_split(view, trans)) {
                gnc_numeric amt = xaccSplitGetAmount (split);
                s = xaccPrintAmount (amt, gnc_account_print_info (xaccSplitGetAccount (split), FALSE));
                editable = TRUE;
            } else {
                s = "";
                editable = FALSE;
            }
            if (get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_PRICE:
        /* Column is PRICE */
        if (is_trow2) {
            s = "";
            editable = FALSE;
        } else if (is_trow1) {
            if (anchor) {
                num = xaccSplitGetSharePrice (get_this_split (view, trans));
                editable = !expanded && (num_of_splits < 3);
                if (expanded)
                    s = "";
                else {
                    if (gnc_numeric_check (num) == GNC_ERROR_OK) {
                        s = xaccPrintAmount (num, gnc_split_amount_print_info (split, FALSE));
                    } else {
                        s = "";
                        editable = FALSE;
                    }
                }
            } else {
                s = "";
                editable = FALSE;
            }
        }

        if(is_split) {
            if(split == get_this_split (view, trans)) {
                num = xaccSplitGetSharePrice (split);

                if (gnc_numeric_check (num) == GNC_ERROR_OK) {
                    s = xaccPrintAmount (num, gnc_split_amount_print_info (split, FALSE));
                    editable = TRUE;
                } else {
                    s = "";
                    editable = FALSE;
                }
            } else {
                s = "";
                editable = FALSE;
            }
            if (get_imbalance (trans))
                g_object_set(cell, "cell-background", PINKCELL, (gchar*)NULL);
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_DEBIT:
    case COL_CREDIT:
        /* Column is CREDIT and DEBIT */
        if (is_split)
        {
            num = get_value_for (view, trans, split, is_blank);
            editable = TRUE;
            if (get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }
        else if (is_trow1)
        {
            if (anchor)
            {
                editable = !expanded && (num_of_splits < 3 );
                num = xaccTransGetAccountAmount (trans, anchor);
            }
            else
            {
                editable = FALSE;
                num = gnc_numeric_zero();
            }
        }
        else if (is_trow2)
        {
            editable = FALSE;
            num = gnc_numeric_zero();
        }

        if ((gnc_numeric_check(num) != GNC_ERROR_OK) ||
             gnc_numeric_zero_p(num) ||
            (gnc_numeric_negative_p(num) && viewcol == COL_DEBIT) ||
            (gnc_numeric_positive_p(num) && viewcol == COL_CREDIT))
        {
            s = "";
        }
        else
        {
            if (expanded)
                s = "";
            else
                s = xaccPrintAmount (gnc_numeric_abs (num),
                                gnc_account_print_info (anchor, FALSE));
        }
#ifdef skip
        //FIXME may be use a function for these three if's
        /* Only allow changes to values if we have a valid split accounts */
        if (is_trow1 && !expanded && (xaccTransCountSplits (trans) == 2))
        {
            if (xaccSplitGetAccount (get_other_split (view, trans)) == NULL)
                editable = FALSE;
        }

        if (is_trow1 && !expanded && (xaccTransCountSplits (trans) == 0 || xaccTransCountSplits (trans) == 1))
        {
            if (xaccSplitGetAccount (view->priv->btrans_split) == NULL)
                editable = FALSE;
        }

        if (is_split && (xaccTransCountSplits (trans) != 0))
        {
            if (xaccSplitGetAccount (split) == NULL)
                editable = FALSE;
        }
#endif
        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_BALANCE:
        /* Column is BALANCE */
        if (is_split)
            g_object_set(cell, "cell-background", "white", (gchar*)NULL);

        if (is_trow1 && anchor) {
            num = xaccTransGetAccountBalance (trans, anchor);
            if (gnc_reverse_balance (anchor))
                num = gnc_numeric_neg (num);
            s = xaccPrintAmount (num, gnc_account_print_info(anchor, FALSE));
            if (gnc_numeric_negative_p (num)
                && gnc_gconf_get_bool (GCONF_GENERAL, KEY_NEGATIVE_IN_RED, NULL))
                g_object_set (cell, "foreground", "red", (gchar*)NULL);
            else
                g_object_set (cell, "foreground", NULL, (gchar*)NULL);
        } else {
            s = "";
        }
        g_object_set (cell, "text", s, "editable", FALSE, NULL);
        break;

    case COL_STATUS:
        /* Column is STATUS */
        if (read_only)
            g_object_set(cell, "cell-background", REDCELL, (gchar*)NULL);
        else if (xaccTransInFutureByPostedDate (trans))
            g_object_set(cell, "cell-background", BLUECELL, (gchar*)NULL);
        else
            g_object_set(cell, "cell-background", BLACKCELL, (gchar*)NULL);

    default:
        break;
    }
    LEAVE("");
}



/*####################################################################
          vvvvv    edit function call backs      vvvvvv
#####################################################################*/
static void
start_edit (GtkCellRenderer *cr, GtkCellEditable *editable,
           const gchar *path_string, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
g_print("\n\nstart_edit\n");
/*FIXME Not sure if this is required, leave for now ? */
    get_editable_start_editing_cb (cr, editable, path_string, user_data);
/*    g_signal_connect(G_OBJECT(editable), "editing-done", (GCallback) editing_done_cb, view); */
    view->priv->acct_edit_path = gtk_tree_path_new_from_string (path_string);

    return;
}


/* means: open trans for editing, unless we're editing a Split (split
   != NULL) AND split doesn't belong to the trans (because it is the
   blank split) */
static void
begin_edit (GncTreeViewSplitReg *view, Split *split, Transaction *trans)
{
g_print("\n\nbegin_edit split %p and trans %p\n", split, trans);
    /* explain me -- this may need changing */


    if (split && trans != xaccSplitGetParent (split))
    {
g_print("begin_edit - blank split, return\n");
        mark_split_dirty (view, trans, split);
        return;
    }

    if (trans != view->priv->dirty_trans)
    {
        Timespec ts = {0,0};
        xaccTransGetDatePostedTS (trans, &ts);

        xaccTransBeginEdit (trans);
        view->priv->dirty_trans = trans;

g_print("begin_edit - xaccTransBeginEdit trans %p\n", trans);

        if (!xaccTransGetCurrency (trans))
        {
            xaccTransSetCurrency (trans, view->priv->reg_comm);
        }

        if (ts.tv_sec == 0)
        {
            //If the time returned by xaccTransGetDatePostedTS is 0 then assume it
            //is a new transaction and set the time to current time to show current
            //date on new transactions

            ts.tv_sec = time (NULL);
            xaccTransSetDatePostedSecs(trans, ts.tv_sec);
        }
    }
}


static void
remove_edit_date (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncPopupEntry *popup_entry;
    const gchar *new_string; 
    const gchar *current_string;

    //These strings are used to determine if cell data was altered so
    //that keynav works better
//g_print("\n\nremove edit date\n");

    popup_entry = GNC_POPUP_ENTRY (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable"));

    new_string = g_strdup (gtk_entry_get_text (GTK_ENTRY (popup_entry->entry)));
//g_print("New String is '%s'\n", new_string);	

    current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");
//g_print("Current String is '%s'\n", current_string);

    //If editing wasn't canceled and strings don't match then
    //cell data was edited
    if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "edit-canceled")) 
             && g_ascii_strcasecmp (new_string, current_string))
    {
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
    }

    g_object_set_data (G_OBJECT (view->priv->temp_cr), "cell-editable", NULL);
    view->priv->temp_cr = NULL;
}


static void
remove_edit_combo (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkEntry *entry; 
    const gchar *new_string; 
    const gchar *current_string;

    //These strings are used to determine if cell data was altered so
    //that keynav works better
//g_print("\n\nremove edit combo\n");

    entry = GTK_ENTRY (gtk_bin_get_child (GTK_BIN (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable"))));

    new_string = gtk_entry_get_text (GTK_ENTRY (entry));
//g_print("New String is '%s'\n", new_string);	

    current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");
//g_print("Current String is '%s'\n", current_string);

    //If editing wasn't canceled and strings don't match then
    //cell data was edited
    if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "edit-canceled"))
             && g_ascii_strcasecmp (new_string, current_string))
    {
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
    }

    g_object_set_data (G_OBJECT (view->priv->temp_cr), "cell-editable", NULL);
    view->priv->temp_cr = NULL;
}


static void
remove_edit_entry (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    const gchar *new_string; 
    const gchar *current_string; 

//g_print("\n\nremove edit completion\n");

    //These strings are used to determine if cell data was altered so
    //that keynav works better
    new_string = gtk_entry_get_text (GTK_ENTRY (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable")));
//g_print("New String is '%s'\n", new_string);

    current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");
//g_print("Current String is '%s'\n", current_string);

    //If editing wasn't canceled and strings don't match then
    //cell data was edited
    if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "edit-canceled")) 
             && g_ascii_strcasecmp (new_string, current_string))
    {
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
    }

    g_object_set_data (G_OBJECT (view->priv->temp_cr), "cell-editable", NULL);
    view->priv->temp_cr = NULL;
}



/* Explain: GtkEntry has a cursor that blinks upon
   g_timeout_dispatch().  It complains if it blinks after the GtkEntry
   loses focus.  So, we can't pop up any dialogs while the blinking
   cursor is around.  The solution is to force the editing to be
   finished before raising the dialog.  That finalizes the
   gtkcelleditable. */
static void
finish_edit (GtkTreeViewColumn *col)
{
    GList *renderers;
    GtkCellRenderer *cr;
    GtkCellEditable *ce;
g_print("\n\nfinish_edit\n");
/*FIXME Not used yet, leave for now */
    if (!col)
        return;

    renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (col)); //FIXME I am only looking at one renderer per cell.
    g_assert (g_list_length (renderers) == 1);
    cr = g_list_nth_data (renderers, 0);
    g_list_free (renderers);

    if ((ce = GTK_CELL_EDITABLE (g_object_get_data (G_OBJECT (cr), "cell-editable"))))
    {
        gtk_cell_editable_editing_done (ce);
    }
}



/*####################################################################
          ^^^^^    edit function call backs      ^^^^^
          vvvvvv   gtv function call backs       vvvvv
#####################################################################*/
static void
gtv_split_reg_titles (GncTreeViewSplitReg *view, RowDepth depth)
{
    GncTreeModelSplitReg *model;
    GtkCellRenderer *cr;
    GList *renderers;
    GList *columns;
    GList  *column;
    gint i;

//g_print("title depth is %d\n", depth);

    model = get_split_reg_model_from_view (view);
    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (view));

    for ( column = columns, i = 1; column; column = g_list_next (column), i++)
    {
        GtkTreeViewColumn *tvc;
        ViewCol viewcol;

        tvc = column->data;

        /*FIXME ## Do we need to look for multiple renderers ?
          We only have one renderer per cell now */
        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (tvc)); //FIXME I am only looking at one renderer per cell.
        g_assert (g_list_length (renderers) == 1);
        cr = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT(cr), "view_column"));

        switch(viewcol)
        {
        case COL_DATE:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Date"));
                break;
            }
            break;

        case COL_DUEDATE:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Due Date"));
                break;
            }
            break;

        case COL_NUMACT:
            switch(model->type)
            {
            case RECEIVABLE_REGISTER2:
            case PAYABLE_REGISTER2:
                if(depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Reference"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Action"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Action"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Reference / Action"));
                break;


            default:
                if(depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Number"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Action"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Action"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Number / Action"));
                break;
            }
            break;

        case COL_DESCNOTES:
            switch(model->type)
            {
            case RECEIVABLE_REGISTER2:
                if(depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Customer"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Customer / Memo "));
                break;

            case PAYABLE_REGISTER2:
                if(depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Vendor"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Vendor / Memo "));
                break;


            default:
                if(depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Description"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Notes"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Description / Notes / Memo"));
                break;
            }
            break;

        case COL_TRANSVOID:
            switch(model->type)
            {
            case RECEIVABLE_REGISTER2:
            case PAYABLE_REGISTER2:
                if(depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                break;

            default:
                if(depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Void Reason"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Accounts / Void Reason"));
                break;
            }
            break;

        case COL_RECN:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("R"));
                break;
            }
            break;

        case COL_TYPE:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Type"));
                break;
            }
            break;

        case COL_VALUE:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Value"));
                break;
            }
            break;

        case COL_AMOUNT:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Amount"));
                break;
            }
            break;

        case COL_AMTVAL:
            switch(model->type)
            {
            default:
                if((depth == TRANS1) || (depth == TRANS2))
                    gtk_tree_view_column_set_title (tvc, _("Value"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Amount"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Amount / Value"));
                break;
            }
            break;

        case COL_RATE:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Rate"));
                break;
            }
            break;

        case COL_PRICE:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Price"));
                break;
            }
            break;

        case COL_CREDIT:
            if(!(model->use_accounting_labels))
            {
                switch(model->type)
                {
                case BANK_REGISTER2: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Withdrawl"));
                    break;

                case CASH_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Spend"));
                    break;

                case ASSET_REGISTER2:
                case LIABILITY_REGISTER2:
                case EQUITY_REGISTER2:
                case TRADING_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Increase"));
                    break;

                case CREDIT_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Charge"));
                    break;

                case INCOME_REGISTER2:
                case INCOME_LEDGER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Income"));
                    break;

                case EXPENSE_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Rebate"));
                    break;

                case STOCK_REGISTER2:
                case PORTFOLIO_LEDGER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Sell"));
                    break;

                case RECEIVABLE_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Payment"));
                    break;

                case PAYABLE_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Bill"));
                    break;

                case GENERAL_LEDGER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Funds Out"));
                    break;

                default:
                    if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Credit"));
                    break;
                }
            }
            else
                gtk_tree_view_column_set_title (tvc, _("Credit"));
            break;

        case COL_DEBIT:
            if(!(model->use_accounting_labels))
            {
                switch(model->type)
                {
                case BANK_REGISTER2: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Deposit"));
                    break;

                case CASH_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Recieve"));
                    break;

                case ASSET_REGISTER2:
                case LIABILITY_REGISTER2:
                case EQUITY_REGISTER2:
                case TRADING_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Decrease"));
                    break;

                case CREDIT_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Payment"));
                    break;

                case INCOME_REGISTER2:
                case INCOME_LEDGER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Charge"));
                    break;

                case EXPENSE_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Expense"));
                    break;

                case STOCK_REGISTER2:
                case PORTFOLIO_LEDGER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Buy"));
                    break;

                case RECEIVABLE_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Invoice"));
                    break;

                case PAYABLE_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Payment"));
                    break;

                case GENERAL_LEDGER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Funds In"));
                    break;

                default:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Debit"));
                    break;
                }
            }
            else
                gtk_tree_view_column_set_title (tvc, _("Debit"));
            break;

        case COL_BALANCE:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Balance"));
                break;
            }
            break;

        default:
            break;
        }
    }
g_list_free (columns);
}


/* Callback for double click */
void
gtv_split_reg_double_click_cb (GtkTreeView *treeview, GtkTreePath *path,
                               GtkTreeViewColumn *column, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (treeview);
    GncTreeModelSplitReg *model;

    model = get_split_reg_model_from_view (view);

//g_print("gtv_split_reg_double_click_cb\n\n");

    /* this does work on non editable cells like void, balance */

    if (model->style != REG2_STYLE_JOURNAL)
    {
        if (gnc_tree_view_split_reg_current_trans_expanded (view))
            gnc_tree_view_split_reg_expand_current_trans (view, FALSE);
        else
            gnc_tree_view_split_reg_expand_current_trans (view, TRUE);

        /* This updates the plugin page gui */
        if (view->moved_cb)
            (view->moved_cb)(view, view->moved_cb_data);
    }
}


/* For handling keynav */
static gboolean
gtv_split_reg_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (widget);
    GtkTreeViewColumn *col;
    ViewCol viewcol;
    GtkCellRenderer *cell;
    GList *renderers;
    GtkTreePath *path = NULL;
    gint *indices_ed, *indices_now;

    gboolean tabbed = FALSE;

    gint editable = 0;

g_print("\ngtvt_key_press_cb\n");

    if (event->type != GDK_KEY_PRESS)
        return TRUE;

    switch (event->keyval)
    {
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


    gtk_tree_view_get_cursor (GTK_TREE_VIEW (view), &path, &col);

    if (!path)
        return TRUE;

    finish_edit (col);



if (tabbed == TRUE)
{


/*FIXME This does not work, editable does not reflect status, why ????? */

//    while (editable == 0) // lets step over non editable columns
//    {

        /* Step to the next column, we may wrap */
        gnc_tree_view_keynav (GNC_TREE_VIEW (view), &col, path, event); // returns path and column

//        if (!path || !gnc_tree_view_path_is_valid (GNC_TREE_VIEW (view), path)) // We have stepped off the end
//             break;

        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (col)); //FIXME I am only looking at one renderer per cell.
        g_assert (g_list_length (renderers) == 1);
        cell = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

        g_object_get (G_OBJECT (cell), "editable", &editable, NULL);

g_print("key nav - Path is '%s' edit path is '%s' and viewcol is %d and editable is %d and cell pointer is %p\n", gtk_tree_path_to_string (path),
                  (view->priv->acct_edit_path == NULL) ? "NULL" : gtk_tree_path_to_string (view->priv->acct_edit_path), viewcol, editable, cell);



//    }
}


    if (!path || !gnc_tree_view_path_is_valid (GNC_TREE_VIEW (view), path)) // We have stepped off the end
    {
        /* no need to restore cursor because we won't move. */
        //Only ask for confirmation if data was edited
        if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT(view), "data-edited")))
        {
       	    transaction_changed_confirm (GNC_TREE_VIEW_SPLIT_REG (view), NULL);
            g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
        }
    }
    else
        /* Set cursor to new column, open for editing */
        gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), path, col, TRUE);

g_print( "end of key nav\n");

    gtk_tree_path_free (path);

    return TRUE;
}


void
gnc_tree_view_split_reg_cancel_edit (GncTreeViewSplitReg *view)
{
    Transaction *trans = view->priv->dirty_trans;

    if (trans && xaccTransIsOpen (trans))
    {
        view->priv->acct_edit_path = NULL;
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
        xaccTransRollbackEdit (view->priv->dirty_trans);
        view->priv->dirty_trans = NULL;
        xaccSplitReinit (view->priv->btrans_split);
    }
}


/* Returns TRUE if dialog was canceled. Does nothing if 'new_trans'
   is the dirty trans. */
static gboolean
transaction_changed_confirm (GncTreeViewSplitReg *view,
                            Transaction *new_trans)
{
    GtkWidget *dialog;
    gint response;
    const char *title = _("Save the changed transaction?");
    const char *message = _(
        "The current transaction has changed.  Would you like to "
        "record the changes, or discard the changes?");

    if (!view->priv->dirty_trans || view->priv->dirty_trans == new_trans)
        return FALSE;

g_print(" ** transaction_changed_confirm **\n");
    dialog = gtk_message_dialog_new(NULL,
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
    response = gnc_dialog_run (GTK_DIALOG (dialog), "transaction_changed");
    gtk_widget_destroy (dialog);

    switch (response)
    {
    case GTK_RESPONSE_ACCEPT:
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
        xaccTransCommitEdit (view->priv->dirty_trans);
        view->priv->acct_edit_path = NULL;
        view->priv->dirty_trans = NULL;
        view->priv->btrans_split = xaccMallocSplit (gnc_get_current_book());
        break;

    case GTK_RESPONSE_REJECT:
        gnc_tree_view_split_reg_cancel_edit (view);
        break;
    case GTK_RESPONSE_CANCEL:
    default:
        return TRUE;
    }

    return FALSE;
}



/* Callback for selection move */
static void
gtv_split_reg_motion_cb (GtkTreeSelection *sel, gpointer user_data)
{

    GtkTreeIter iter;
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model; 
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;
    RowDepth depth = 0;
    const gchar *path_string;
    GtkTreePath *path;
    gint *indices_old, *indices_now;
    gint row_old, row_now;

    model = get_split_reg_model_from_view (view);

g_print ("\ngtv_split_reg_motion_cb\n");

    mark_split_dirty (view, NULL, NULL);

    if (get_model_iter_from_selection (view, sel, &iter))
    {

        path = gtk_tree_model_get_path (GTK_TREE_MODEL (model), &iter);

g_print("Motion - Current Path is '%s' and edit path is '%s'\n", gtk_tree_path_to_string (view->priv->current_path),
                                                                 (view->priv->acct_edit_path == NULL) ? "NULL" :
                                                                  gtk_tree_path_to_string (view->priv->acct_edit_path));

        indices_old = gtk_tree_path_get_indices (view->priv->current_path);
        indices_now = gtk_tree_path_get_indices (path);

        row_old = indices_old[0];
        row_now = indices_now[0];

g_print ("Motion - row old is %d and now %d\n", row_old, row_now);

        view->priv->row_old = row_old;
        view->priv->row_now = row_now;

        /* save the current path */
        gtk_tree_path_free (view->priv->current_path);
        view->priv->current_path = gtk_tree_path_copy (path);

        /* Use depth to determine if it is a split or transaction */
        depth = gtk_tree_path_get_depth (path);

        /* Update the tree view titles */
        gtv_split_reg_titles (view, depth);

        gtk_tree_path_free (path);

g_print("Motion - New Current Path is '%s' and edit path is '%s'\n", gtk_tree_path_to_string (view->priv->current_path),
                                                                 (view->priv->acct_edit_path == NULL) ? "NULL" :
                                                                  gtk_tree_path_to_string (view->priv->acct_edit_path));


        /*FIXME Not sure if will need this here, leave till end */
        gnc_tree_model_split_reg_get_split_and_trans (
                GNC_TREE_MODEL_SPLIT_REG (model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

g_print("Motion - row_now is %d, split %p, trans %p, is_split %d, is_blank %d\n", row_now, split, trans, is_split, is_blank);

        //Ask for confirmation if data has been edited, transaction_changed_confirm return TRUE if canceled
        if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) && transaction_changed_confirm (view, trans))
        {
            /* Restore position - Cancel */
            gtk_tree_path_free (view->priv->current_path);
            view->priv->current_path = gtk_tree_path_copy (view->priv->acct_edit_path);

            gnc_tree_view_split_reg_default_selection (view);
        }
        else
        {
            /* Discard, commit and skip */

            /* Move the blank split */ 
            gnc_tree_model_split_reg_set_blank_split_parent (model, trans);
        }

g_print ("Motion - depth is %d row old is %d and now %d\n", depth, row_old, row_now);

        /* Set the view format */
        g_idle_add ((GSourceFunc)gnc_tree_view_split_reg_set_format, view);

        /* scroll when view is idle */
//        g_idle_add ((GSourceFunc)gtv_scroll_to_cell, view );

    }
    else
    {
g_print("Not valid selection\n");
        /* We do not have a valid iter */
        gtv_split_reg_titles (view, 0);

        /* Move the blank split to the last transaction */ 
        gnc_tree_model_split_reg_set_blank_split_parent (model, NULL);

        /* Set the default selection start position */
        gnc_tree_view_split_reg_default_selection (view);

    }

    /* This updates the plugin page gui */
    if (view->moved_cb)
        (view->moved_cb)(view, view->moved_cb_data);
}


/* Connected to "edited" from cellrenderer. For reference, see
   split-register-model-save.c */
static void
gtv_split_reg_edited_cb (GtkCellRendererText *cell, const gchar *path_string,
               const gchar *new_text, gpointer user_data)
{
    GncTreeViewSplitReg  *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeIter           iter;
    Split                *split;
    Transaction          *trans;
    gboolean              is_trow1, is_trow2, is_split, is_blank;
    ViewCol               viewcol;
    char                 *error_loc = NULL;
    Account              *anchor = view->priv->anchor;

g_print("\ngtv_split_reg_edited_cb\n");

    if (g_strcmp0 (g_object_get_data (G_OBJECT (cell), "current-string"), new_text) == 0) // No change, return
        return;

    g_return_if_fail (get_model_iter_from_view_string (view, path_string, &iter));

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    model = get_split_reg_model_from_view (view);
    g_return_if_fail (model);

    gnc_tree_model_split_reg_get_split_and_trans (
        model, &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

g_print("gtv_split_reg_edited_cb New Text is '%s'\n", new_text);

    switch (viewcol) {
    case COL_DATE:
        /* Column is DATE */
        if (is_trow1)
        {
            GDate date;
            g_date_set_parse (&date, new_text);
            if (g_date_valid (&date))
            {
                begin_edit (view, split, trans);
                xaccTransSetDate (trans, g_date_get_day (&date), g_date_get_month (&date), g_date_get_year (&date));
            }
            else
            {
                PERR("invalid date `%s`", new_text);
            }
        }
        break;

    case COL_NUMACT:
        /* Column is NUM / ACT */
        begin_edit (view, split, trans);
        if (is_trow1)
        {
            xaccTransSetNum (trans, new_text);
        }
        if (is_trow2)
        {
            xaccSplitSetAction (get_this_split (view, trans), new_text);
        }
        if (is_split)
        {
            xaccSplitSetAction (split, new_text);
        }
        break;

    case COL_DESCNOTES:
        /* Column is DESCRIPTION / NOTES */
        begin_edit (view, split, trans);
        if (is_trow1)
        {
            xaccTransSetDescription (trans, new_text);
        }
        if (is_trow2)
        {
            xaccTransSetNotes (trans, new_text);
        }
        if (is_split)
        {
            xaccSplitSetMemo (split, new_text);
        }
        break;

    case COL_TRANSVOID:
        /* Column is TRANSFER / VOID (Only updated from menu.) */
{
        Split *osplit = NULL;

        begin_edit (view, split, trans);

        if (!is_split && anchor)
        {
            if (!get_split_pair (view, trans, &osplit, &split))
            {
                PERR("couldn't get split pair");
                break;
            }
        }


        if (is_trow1)
        {
            QofBook       *book; //do we have this
            Account        *root; // do we have this
//FIXME Probably needs more work 

            Account *new_acct;
            gint num_of_splits;

            book = gnc_get_current_book();
            root = gnc_book_get_root_account(book);

            num_of_splits = xaccTransCountSplits (trans);

g_print("trans split and num_of_splits is %d\n", num_of_splits);

            if (view->priv->acct_short_names)
                new_acct = gnc_account_lookup_by_name (root, new_text);
            else
                new_acct = gnc_account_lookup_by_full_name (root, new_text);

            if (new_acct != NULL)
                xaccAccountInsertSplit (new_acct, osplit);

        }


        if (is_split)
        {
            QofBook       *book; //do we have this
            Account        *root; // do we have this
//FIXME Probably needs more work 

            Account *new_acct;
            gint     num_of_splits;

            book = gnc_get_current_book();
            root = gnc_book_get_root_account(book);

            num_of_splits = xaccTransCountSplits (trans);

g_print("split and num_of_splits is %d\n", num_of_splits);

            if (view->priv->acct_short_names)
                new_acct = gnc_account_lookup_by_name (root, new_text);
            else
                new_acct = gnc_account_lookup_by_full_name (root, new_text);

            if (new_acct != NULL)
                    xaccAccountInsertSplit (new_acct, split);
          }





}
        break;

    case COL_RECN:
        /* Column is RECONCILE */
        begin_edit (view, split, trans);

        if (new_text != NULL)
        {
            char rec = 'n';
            rec = new_text[0];

            if (is_trow1) 
                xaccSplitSetReconcile (get_this_split (view, trans), rec);
            if (is_split)
                xaccSplitSetReconcile (split, rec);
        }
        else
        {
            char rec = 'n';

            if (is_trow1) 
                xaccSplitSetReconcile (get_this_split (view, trans), rec);
            if (is_split)
                xaccSplitSetReconcile (split, rec);
        }

        break;

    case COL_TYPE:
    case COL_VALUE:
    case COL_AMOUNT:
    case COL_AMTVAL:
    case COL_RATE:
    case COL_PRICE:
        break;

    case COL_DEBIT:
    case COL_CREDIT:
        {

            Account *acct;
            gnc_numeric input;
            Split *split2 = NULL;

g_print("rest\n");

            if (!gnc_exp_parser_parse (new_text, &input, &error_loc))
                break;

            if (!is_split && anchor)
            {
g_print("rest1\n");
                if (!get_split_pair (view, trans, &split2, &split))
                {
                    PERR("couldn't get split pair");
                    break;
                }
            }

            begin_edit (view, NULL, trans); // open trans even if split not a child
            mark_split_dirty (view, trans, split);

            acct = xaccSplitGetAccount (split);
            if (!acct)
            {
g_print("rest2\n");
                if (anchor)
                {
g_print("rest3\n");
                    xaccSplitSetAccount (split, anchor);
                    acct = xaccSplitGetAccount (split);
                }
                else
                {
                    break; //Well, what else is there to do?
                }
            }

            if (viewcol == COL_CREDIT)
                input = gnc_numeric_neg (input);

//            if (viewcol == COL_AMOUNT)
//            {
//                set_amount_for (view, trans, split, input);
//                break;
//            }

//            if (viewcol == COL_RATE) // Not sure why this is here
//            {
//                set_rate_for (view, trans, split, input, is_blank);
//                break;
//            }

            set_value_for (view, trans, split, input);

            if (split2)
            {
g_print("rest4\n");
                xaccSplitSetParent (split2, trans);
                set_value_for (view, trans, split2, gnc_numeric_neg (input));
            }
        }
        break;

    default:
        //g_assert_not_reached();
        break;
    }
}


static void
gtv_split_reg_recn_cb (GtkEntry    *entry,
                          const gchar *text,
                          gint         length,
                          gint        *position,
                          gpointer     user_data)
{
    GtkEditable *editable = GTK_EDITABLE (entry);
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    const gchar *flags = gnc_get_reconcile_flag_order();
    gchar *this_flag;
    const char *result;
    static char ss[2];

    gint index = 0;

/*FIXME this works, but is there a simpler way ? */

//g_print("gtv_split_reg_recn_cb '%s'\n", text);

    result = g_ascii_strdown (text, length);

    if (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-flag") != NULL)
        index = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-flag"));

    /* So we can test for space */
    ss[0] = ' ';
    ss[1] = '\0';

    /* Find the current flag in the list of flags */
    this_flag = strstr (flags, text);

    if (this_flag == NULL || *this_flag == '\0')
    {
        if (g_strcmp0 (text, ss) == 0)  // test for space
        {
            /* In the list, choose the next item in the list
               (wrapping around as necessary). */

            flags = flags + index;
            if (*flags != '\0')
            {
                index = index + 1;
                result = flags;
            }
            else
            {
                flags = flags - index;
                index = 1;
                result = flags;
            }
        }
        else
            /* If it's not there (or the list is empty) use default_flag */
            result  = gnc_get_reconcile_str (NREC);
    }
    else
        result = text;

    /* save the index in the cellrenderer */
    g_object_set_data (G_OBJECT (view->priv->temp_cr), "current-flag", GINT_TO_POINTER (index));

    g_signal_handlers_block_by_func (editable, (gpointer) gtv_split_reg_recn_cb, user_data);

    gtk_editable_delete_text (editable, 0, -1);
    gtk_editable_insert_text (editable, result, length, position);

    g_signal_handlers_unblock_by_func (editable, (gpointer) gtv_split_reg_recn_cb, user_data);

    g_signal_stop_emission_by_name (editable, "insert_text");
}


/* The main Start Editing Call back for the TEXT columns */
static void
get_editable_start_editing_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
                              const gchar *path_string, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreePath *path;

    GtkListStore *description_list;
    GtkListStore *memo_list;
    GtkListStore *notes_list;
    GtkListStore *acct_list;

    GtkEntryCompletion *completion = gtk_entry_completion_new();
    RowDepth depth;

    ENTER(" ");
g_print("\nget_editable_start_editing_cb\n\n");

    model = get_split_reg_model_from_view (view);

    /* Description / Notes / Memo / Accounts Completion Lists */
    description_list = gnc_tree_model_split_reg_get_description_list (model);
    notes_list = gnc_tree_model_split_reg_get_notes_list (model);
    memo_list = gnc_tree_model_split_reg_get_memo_list (model);
    acct_list = gnc_tree_model_split_reg_get_acct_list (model);

//g_print("editable Path string is '%s'\n", path_string);

    //Use depth to determine if it is a split or transaction
    path = gtk_tree_path_new_from_string (path_string);
    depth = gtk_tree_path_get_depth (path);

//g_print("editable Depth is %u\n", depth);

    /* DATE COLUMN */
    if (GNC_TREE_MODEL_SPLIT_REG_COL_DATE 
        == GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr), "model_column")))
    {
        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (GTK_ENTRY (GNC_POPUP_ENTRY (editable)->entry))));

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_date, view);

//g_print("Current String date is '%s'\n", g_strdup (gtk_entry_get_text (GTK_ENTRY (GNC_POPUP_ENTRY (editable)->entry))));

    }


    /* TRANSFER / VOID COLUMN */
    else if (GNC_TREE_MODEL_SPLIT_REG_COL_TRANSVOID 
        == GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr), "model_column")))
    {
        GtkEntry *entry;

        entry = GTK_ENTRY (gtk_bin_get_child (GTK_BIN (editable)));

        gtk_entry_set_completion (GTK_ENTRY (entry), completion);
        gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (acct_list));

        /* This sets which text column to use, 0 for short names, 1 for long */
        if (view->priv->acct_short_names)
            gtk_entry_completion_set_text_column (completion, 0);
        else
            gtk_entry_completion_set_text_column (completion, 1);

        gtk_entry_completion_set_popup_completion (completion, TRUE);
        gtk_entry_completion_set_inline_selection (completion, TRUE);
        gtk_entry_completion_set_popup_set_width (completion, FALSE);
        gtk_entry_completion_set_minimum_key_length (completion, KEY_LENGTH);
/*??        g_signal_connect(G_OBJECT(completion), "match-selected", (GCallback)gtv_split_reg_match_selected_cb, view); */
        g_object_unref (completion);

        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data(G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)));

        g_signal_connect (G_OBJECT (cr), "changed", (GCallback)gtv_split_reg_changed_cb, view);
        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_combo, view);

//g_print("Current String tv is '%s'\n", g_strdup(gtk_entry_get_text (entry)));
    }


    /* NUMBER / ACTION COLUMN */
    else if (GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT 
        == GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr), "model_column")))
    {
        GtkEntry *entry;
        GtkTreeModel *listmodel;
        GtkTreeIter iter;

        if (depth == TRANS1)
            gnc_tree_model_split_reg_get_num_list (model);

        else if (depth == TRANS2)
            gnc_tree_model_split_reg_get_action_list (model);

        else if (depth == SPLIT3)
            gnc_tree_model_split_reg_get_action_list (model);

        entry = GTK_ENTRY (gtk_bin_get_child (GTK_BIN (editable)));

        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)));

/*??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback)gtv_split_reg_changed_cb, view); */
        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_combo, view);

//g_print("Current String na is '%s'\n", g_strdup (gtk_entry_get_text (entry)));
    }


    /* DESCRIPTION / NOTES / MEMO COLUMN */
    else if (GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES 
        == GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr), "model_column")))
    {
        //Data used for completion is set based on if editing split or not
        if (depth == TRANS1)
        {
            gtk_entry_set_completion (GTK_ENTRY (editable), completion);
            gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (description_list));
            gtk_entry_completion_set_text_column (completion, 0);
        }
        else if (depth == TRANS2)
        {
            gtk_entry_set_completion (GTK_ENTRY (editable), completion);
            gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (notes_list));
            gtk_entry_completion_set_text_column (completion, 0);
        }
        else if (depth == SPLIT3)
        {
            gtk_entry_set_completion (GTK_ENTRY (editable), completion);
            gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (memo_list));
            gtk_entry_completion_set_text_column (completion, 0);
        }

        //To emit "match-selected" signal we need to have a list of matches to
        //select from instead of using inline autocompletion
        gtk_entry_completion_set_popup_completion (completion, TRUE);
        gtk_entry_completion_set_inline_selection (completion, TRUE);
        gtk_entry_completion_set_minimum_key_length (completion, KEY_LENGTH);
/*??        g_signal_connect (G_OBJECT (completion), "match-selected", (GCallback)gtv_split_reg_match_selected_cb, view); */

        g_object_unref (completion);

        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (GTK_ENTRY(editable))));
        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_entry, view);

//g_print("Current String dnm is '%s'\n", g_strdup (gtk_entry_get_text (GTK_ENTRY(editable))));
    }


    /* RECN COLUMN - We do it this way as we do not want sort arrows */
    else if ( g_strcmp0 (g_object_get_data (G_OBJECT (cr), "column_name"), "recn") == 0)
    {
        GtkEntry *entry;

        entry = GTK_ENTRY (editable);

        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)));
        g_signal_connect (G_OBJECT (GTK_ENTRY (editable)), "insert_text", (GCallback)gtv_split_reg_recn_cb, view);
        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_entry, view);

/*??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback)gtv_split_reg_changed_cb, view); */
//g_print("Current String recn is '%s'\n", g_strdup (gtk_entry_get_text (entry)));
    }


    /* REST OF THE COLUMNS */
    else
    {
        GtkEntry *entry;

        entry = GTK_ENTRY (editable);

        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)));
//        g_signal_connect (G_OBJECT (GTK_ENTRY (editable)), "insert_text", (GCallback)gtv_split_reg_recn_cb, view);
        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_entry, view);

/*??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback)gtv_split_reg_changed_cb, view); */
//g_print("Current String rest is '%s'\n", g_strdup (gtk_entry_get_text (entry)));
    }


    gtk_tree_path_free (view->priv->acct_edit_path);
    view->priv->acct_edit_path = gtk_tree_path_copy (path);
//g_print("edit_path is %s\n", gtk_tree_path_to_string (view->priv->acct_edit_path));
    gtk_tree_path_free (path);
    view->priv->temp_cr = cr;
    //Add edit-canceled property to cr so we can distinguish between
    //cancelled and actual changes
    g_object_set_data (G_OBJECT (cr), "edit-canceled", GINT_TO_POINTER (FALSE));

    LEAVE(" ");
}


//Handle the "match-selected" signal
static void
gtv_split_reg_match_selected_cb (GtkEntryCompletion *widget, GtkTreeModel *model,
                        GtkTreeIter *iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

    gboolean is_trow1, is_trow2, is_split, is_blank;

g_print("gtv_split_reg_match_selected_cb\n\n");

/* Not sure what I am going to put in here yet */

}


//Handle the "changed" signal
static void
gtv_split_reg_changed_cb (GtkCellRendererCombo *widget, gchar *path_string,
                        GtkTreeIter *iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

    gboolean is_trow1, is_trow2, is_split, is_blank;

g_print("gtv_split_reg_changed_cb path string is '%s'\n\n", path_string);


/* Not sure what I am going to put in here yet */

}


//Handle the "editing-canceled" signal
static void
gtv_split_reg_editing_canceled_cb (GtkCellRenderer *cr, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
g_print("gtv_split_reg_editing_canceled_cb\n\n");

    if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) == FALSE) // None edited, reset edit path
        view->priv->acct_edit_path = NULL;

    //Set edit-canceled property
    g_object_set_data (G_OBJECT (cr), "edit-canceled", GINT_TO_POINTER (TRUE));	
}

/*####################################################################
          ^^^^   gtv function call backs    ^^^^
#####################################################################*/



/* Reinit transaction / delete the splits */
void
gnc_tree_view_split_reg_reinit_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;

g_print("gnc_tree_view_split_reg_reinit_trans\n");
    model = get_split_reg_model_from_view (view);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, view->priv->current_path);

    gnc_tree_model_split_reg_get_split_and_trans (
            GNC_TREE_MODEL_SPLIT_REG (model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    if (trans && (is_trow1 || is_trow2))
    {
        Split *s;
        int i = 0;
        
        begin_edit (view, NULL, trans);        
        while ((s = xaccTransGetSplit(trans, i)) != NULL)
        {
            if (xaccTransGetRateForCommodity (trans, view->priv->reg_comm, s, NULL))
                xaccSplitDestroy(s);
            else i++;
        }
    g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
    view->priv->dirty_trans = trans;
    }
}


/* Jump to the Blank transaction, i.e. last in list */
void
gnc_tree_view_split_reg_jump_to_blank (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *path;

//g_print("gnc_tree_view_split_reg_jump_to_blank\n");
    model = get_split_reg_model_from_view (view);

    path = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, NULL);

    gtk_tree_path_free (view->priv->current_path);
    view->priv->current_path = gtk_tree_path_copy (path);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);

    /* Scroll the window to show selection */
    if (model->use_double_line)
       gtk_tree_path_down (path); // show the second row of transaction

    /* scroll when view idle */
    g_idle_add ((GSourceFunc)gtv_scroll_to_cell, view );

    gtk_tree_path_free (path);
}


static gboolean
gtv_scroll_to_cell (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *path;

    model = get_split_reg_model_from_view (view);

    path = view->priv->current_path;
   
    /* Scroll the window to show selection */
//    if (model->use_double_line)
//       gtk_tree_path_down (path); // show the second row of transaction
//FIXME we can not move path like this, copy it or use indices ????
// can this be combined with set_view_format

    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), path, NULL, TRUE, 0.5, 0.0);

   return (FALSE);
}


/* Jump to split */
void
gnc_tree_view_split_reg_jump_to_split (GncTreeViewSplitReg *view, Split *split)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *path;

//g_print("gnc_tree_view_split_reg_jump_to_split and split is %p\n", split);

    model = get_split_reg_model_from_view (view);

    path = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, split, NULL);

//g_print("Path is '%s'\n", gtk_tree_path_to_string (path));

    gtk_tree_path_free (view->priv->current_path);

    view->priv->current_path = gtk_tree_path_copy (path);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);

    gtk_tree_path_free (path);

    /* scroll when view is idle */
    g_idle_add ((GSourceFunc)gtv_scroll_to_cell, view );
}


/* Move to the relative transaction */
void
gnc_tree_view_split_reg_goto_rel_trans_row (GncTreeViewSplitReg *view, gint relative)
{

    GncTreeModelSplitReg *model;
    GtkTreePath *path;
    gint *indices;

g_print("gnc_tree_view_split_reg_goto_rel_trans_row\n");

//FIXME Need to do some checks on relative -1,0,1
    model = get_split_reg_model_from_view (view);

    indices = gtk_tree_path_get_indices (view->priv->current_path);

    path = gtk_tree_path_new_from_indices (indices[0] + relative, -1);

    gtk_tree_path_free (view->priv->current_path);

    view->priv->current_path = gtk_tree_path_copy (path);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);

    gtk_tree_path_free (path);
}


/* Delete the current split */
void
gnc_tree_view_split_reg_delete_current_split (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;

g_print("gnc_tree_view_split_reg_delete_current_split\n");
    model = get_split_reg_model_from_view (view);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, view->priv->current_path);

    gnc_tree_model_split_reg_get_split_and_trans (
            GNC_TREE_MODEL_SPLIT_REG (model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    /* Lets get out of the way, move the selection to the transaction */
    gnc_tree_view_split_reg_goto_rel_trans_row (view, 0);

    begin_edit (view, split, trans);

//FIXME Do we need other stuff here
    if (is_split)
    {
        xaccSplitDestroy (split);
        xaccTransCommitEdit (trans);
    }
}


/* Delete the current transaction */
void
gnc_tree_view_split_reg_delete_current_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;
    gboolean was_open;

g_print("gnc_tree_view_split_reg_delete_current_trans\n");
    model = get_split_reg_model_from_view (view);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, view->priv->current_path);

    gnc_tree_model_split_reg_get_split_and_trans (
            GNC_TREE_MODEL_SPLIT_REG (model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    begin_edit (view, split, trans);

    /* Lets get out of the way, move the selection to the previous transaction */
    gnc_tree_view_split_reg_goto_rel_trans_row (view, -1);

//FIXME Do we need other stuff here

    was_open = xaccTransIsOpen (trans);

    xaccTransDestroy (trans);
    if (was_open)
    {
        DEBUG("committing");
        xaccTransCommitEdit (trans);
    }

    view->priv->dirty_trans = NULL;


}

/* Returns whether the splits are revealed at the current position */ 
gboolean
gnc_tree_view_split_reg_current_trans_expanded (GncTreeViewSplitReg *view)
{
    GtkTreePath *path;
    gint *indices;
    gboolean expanded = FALSE;

    indices = gtk_tree_path_get_indices (view->priv->current_path);

    path = gtk_tree_path_new_from_indices (indices[0], 0, -1);

    expanded = gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), path);

//g_print("Expanded indices are %d %d %d and return is %d\n", indices[0], indices[1], indices[2], expanded);
    gtk_tree_path_free(path);
    return expanded;
}


/* Returns the depth of the selected row */
RowDepth
gnc_tree_view_reg_get_selected_row_depth (GncTreeViewSplitReg *view)
{
    gint depth;

    depth = gtk_tree_path_get_depth (view->priv->current_path);

    return depth;
}


/* Returns the Blank Split */
Split * 
gnc_tree_view_split_reg_get_blank_split (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;

//g_print("gnc_tree_view_split_reg_get_blank_split\n");
    model = get_split_reg_model_from_view (view);

    return gnc_tree_model_split_get_blank_split (model);
}


/* Return the Split for the current Transaction */
Split *
gnc_tree_view_reg_get_current_trans_split (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;

//g_print("gnc_tree_view_reg_get_current_trans_split\n");
    model = get_split_reg_model_from_view (view);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, view->priv->current_path);

//g_print("gnc_tree_view_reg_get_current_trans_split %p and %s\n", model,  gtk_tree_path_to_string (view->priv->current_path));

    gnc_tree_model_split_reg_get_split_and_trans (
            GNC_TREE_MODEL_SPLIT_REG (model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    split = get_this_split (view, trans);

//g_print("gnc_tree_view_reg_get_current_trans_split %p\n", split);

    return split;
}


/* Returns the Split at the current selected position */
Split * 
gnc_tree_view_split_reg_get_current_split (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;

//g_print("gnc_tree_view_split_reg_get_current_split\n");
    model = get_split_reg_model_from_view (view);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, view->priv->current_path);

//g_print("gnc_tree_view_split_reg_get_current_split %p and %s\n", model,  gtk_tree_path_to_string (view->priv->current_path));

    gnc_tree_model_split_reg_get_split_and_trans (
            GNC_TREE_MODEL_SPLIT_REG (model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

//g_print("gnc_tree_view_split_reg_get_current_split %p\n", split);
    return split;
}


/* Returns the Transaction at the current selected position */
Transaction * 
gnc_tree_view_split_reg_get_current_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;

//g_print("gnc_tree_view_split_reg_get_current_trans\n");
    model = get_split_reg_model_from_view (view);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, view->priv->current_path);

//g_print("gnc_tree_view_split_reg_get_current_trans %p and %s\n", model,  gtk_tree_path_to_string (view->priv->current_path));

    gnc_tree_model_split_reg_get_split_and_trans (
            GNC_TREE_MODEL_SPLIT_REG (model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

//g_print("gnc_tree_view_split_reg_get_current_trans %p\n", trans);

    return trans;
}


/* Record changes */
gboolean
gnc_tree_view_split_reg_enter (GncTreeViewSplitReg *view)
{
g_print("gnc_tree_view_split_reg_enter\n");

    //Ask for confirmation if data has been edited, transaction_changed_confirm return TRUE if canceled
    if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) && transaction_changed_confirm (view, NULL))
    {
        return FALSE;
    }
    return TRUE;
}


/* Expands the current transaction to reveal splits */
void
gnc_tree_view_split_reg_expand_current_trans (GncTreeViewSplitReg *view, gboolean expand)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *path;
    gint *indices;
    RowDepth depth;

g_print("\ngnc_tree_view_split_reg_expand_current_trans and expand is %d\n", expand);

    model = get_split_reg_model_from_view (view);

    if (expand)
        gtk_tree_view_expand_row (GTK_TREE_VIEW (view), view->priv->current_path, TRUE);
    else
    {
        /* Colapse the view back to the transaction */
        indices = gtk_tree_path_get_indices (view->priv->current_path);
        depth = gtk_tree_path_get_depth (view->priv->current_path);

        gnc_tree_view_split_reg_block_selection (view, TRUE);

//g_print("indices1 %d %d %d expand is %d\n", indices[0], indices[1], indices[2], expand);

        if (model->use_double_line)
            path =  gtk_tree_path_new_from_indices (indices[0], 0, -1);
        else
            path = gtk_tree_path_new_from_indices (indices[0], -1);

        gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), path);

        gtk_tree_path_free (path);

        gnc_tree_view_split_reg_block_selection (view, FALSE);

        /* Change the selection to last available row of transaction */
        if ((model->use_double_line) && (depth != TRANS1))
            path =  gtk_tree_path_new_from_indices (indices[0], 0, -1);
        else
            path = gtk_tree_path_new_from_indices (indices[0], -1);

        gtk_tree_path_free (view->priv->current_path);
        view->priv->current_path = gtk_tree_path_copy (path);

//g_print("Expanded Path is '%s'\n", gtk_tree_path_to_string (path));

        gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);

        gtk_tree_path_free (path);
    }
    /* Scroll the window to show selection */
//    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), view->priv->current_path, NULL, TRUE, 0.5, 0.0);
}


/* This sets up the page gui update from the tree view motion callback */
void gnc_tree_view_split_reg_moved_cb (GncTreeViewSplitReg *view, GFunc cb, gpointer cb_data)
{
    view->moved_cb = cb;
    view->moved_cb_data = cb_data;
}

