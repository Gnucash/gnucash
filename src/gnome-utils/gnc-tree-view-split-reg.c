/********************************************************************\
 * gnc-tree-view-split-reg.c -- GtkTreeView implementation to       *
 *                     display registers   in a GtkTreeView.        *
 *                                                                  *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
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
#include "gnc-tree-control-split-reg.h"
#include "gnc-ui.h"
#include "dialog-utils.h"
#include "gnc-gconf-utils.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "Scrub.h"
#include "gnc-exp-parser.h"
#include "dialog-transfer.h"
#include "gnc-amount-edit.h"


/* Signal codes */
enum
{
    UPDATE_SIGNAL,
    HELP_SIGNAL,
    LAST_SIGNAL
};

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_tree_view_split_reg_class_init (GncTreeViewSplitRegClass *klass);
static void gnc_tree_view_split_reg_init (GncTreeViewSplitReg *view);
static void gnc_tree_view_split_reg_dispose (GObject *object);
static void gnc_tree_view_split_reg_finalize (GObject *object);

static guint gnc_tree_view_split_reg_signals[LAST_SIGNAL] = {0};

static void cdf (GtkTreeViewColumn *col, GtkCellRenderer *renderer, GtkTreeModel *model,
				GtkTreeIter *iter, gpointer user_data);

static void control_cdf (GtkTreeViewColumn *col, GtkCellRenderer *renderer,
                                 GtkTreeModel *model, GtkTreeIter *iter, gpointer user_data);

static void gtv_split_reg_edited_cb (GtkCellRendererText *cell, const gchar *path_string,
                          	const gchar *new_text, gpointer _model);

static void start_edit (GtkCellRenderer *cr, GtkCellEditable *editable,
				const gchar *path, gpointer user_data); //FIXME This may not be needed

static void gtv_begin_edit (GncTreeViewSplitReg *view, Split *split, Transaction *trans);

static void gtv_finish_edit (GncTreeViewSplitReg *view);

static void gtv_get_editable_start_editing_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
   				const gchar *path, gpointer user_data);

static void get_editable_start_editing_recn_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
   				const gchar *path, gpointer user_data);

static void gtv_split_reg_editing_canceled_cb (GtkCellRenderer *cr, gpointer user_data);

static void gtv_split_reg_match_selected_cb (GtkEntryCompletion *widget, GtkTreeModel *model,
                        GtkTreeIter *iter, gpointer user_data); //FIXME This may not be needed

static void gtv_split_reg_changed_cb (GtkCellRendererCombo *widget, gchar *path_string,
                        GtkTreeIter *iter, gpointer user_data); //FIXME This may not be needed

static void gtv_split_reg_trans_delete_cb (GncTreeModelSplitReg *model, gpointer item, gpointer user_data);

static gboolean gtv_split_reg_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data);

static gboolean gtv_split_reg_focus_out_cb (GtkWidget *widget, GdkEventFocus *event, gpointer user_data);

static void gtv_split_reg_motion_cb (GtkTreeSelection *sel, gpointer user_data);

static void gtv_split_reg_refresh_cb (GncTreeModelSplitReg *model, gpointer user_data);

static gboolean gtv_scroll_to_cell (GncTreeViewSplitReg *view);

static void gtv_split_reg_double_click_cb (GtkTreeView *treeview,
                                             GtkTreePath       *path,
                                             GtkTreeViewColumn *column,
                                             gpointer           user_data);



static gboolean transaction_changed_confirm (GncTreeViewSplitReg *view, Transaction *new_trans);

typedef enum {
    COL_DATE,      //0
    COL_DUEDATE,   //1
    COL_NUMACT,    //2
    COL_DESCNOTES, //3
    COL_TRANSVOID, //4
    COL_RECN,      //5
    COL_TYPE,      //6
    COL_VALUE,     //7
    COL_AMOUNT,    //8
    COL_AMTVAL,    //9
    COL_RATE,      //10
    COL_PRICE,     //11
    COL_DEBIT,     //12
    COL_CREDIT,    //13
    COL_BALANCE,   //14
    COL_STATUS,    //15
    COL_COMM,      //16
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
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb,
     gnc_tree_control_split_reg_sort_by_date},

    {COL_DUEDATE, GNC_TREE_MODEL_SPLIT_REG_COL_DUEDATE,
     "Due Date", "duedate", "00/00/0000xxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_NUMACT, GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT,
     "Num / Act / Act", "numact", "0000xxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb,
     gnc_tree_control_split_reg_sort_by_numact},

    {COL_DESCNOTES, GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES,
     "Description / Notes / Memo", "descnotes", "xxxxxxxxxxxxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb,
     gnc_tree_control_split_reg_sort_by_dnm},

    {COL_TRANSVOID, -1,
     "Transfer / Void", "transvoid", "xxxxxxxxxxxxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_RECN, GNC_TREE_MODEL_SPLIT_REG_COL_RECN,
     "R", "recn", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb,
     gnc_tree_control_split_reg_sort_by_recn},

    {COL_TYPE, -1,
     "Type", "type", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_VALUE, -1,
     "Value", "value", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_AMOUNT, -1,
     "Amount", "amount", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_AMTVAL, -1,
     "Amount / Value", "amtval", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_RATE, -1,
     "Rate", "rate", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_PRICE, -1,
     "Price", "price", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_DEBIT, -1,
     "Debit", "debit", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_CREDIT, -1,
     "Credit", "credit", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     gtv_split_reg_edited_cb, gtv_get_editable_start_editing_cb, NULL},

    {COL_BALANCE, -1,
     "Balance", "balance", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},

    {COL_STATUS, -1,
     " ", "status", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},

    {COL_COMM, -1,
     "Commodity", "commodity", "xxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},
};


struct GncTreeViewSplitRegPrivate
{
    gboolean             disposed;
  
    Account             *anchor;              // The register default Account
    gnc_commodity       *reg_comm;            // The register commodity

    Transaction         *current_trans;       // The current highlighted transaction
    Split               *current_split;       // The current highlighted split
    RowDepth             current_depth;       // The current depth 1=TROW1, 2=TROW2, 3=SPLIT3
    GtkTreeRowReference *current_ref;         // The current model path reference

    Transaction         *dirty_trans;         // Set when transaction is changed
    GtkTreeRowReference *edit_ref;            // The model edit path reference

    GtkCellRenderer     *temp_cr;             // Pointer to Temp Cell Renderer
    gulong               fo_handler_id;       // Focus out callback id

    gboolean             acct_short_names;    // Use account short names
    gboolean             double_line;         // Use double line mode
    gboolean             expanded;            // Are we expanded to splits

};


#define SPLIT_TRANS_STR _("-- Split Transaction --")

/* Define some cell colors */
#define PINKCELL "#F8BEC6"
#define REDCELL "#F34943"
#define BLUECELL "#1D80DF"
#define BLACKCELL "#CBCBD2"
#define YELLOWCELL "#FFEF98"


/* This could be a preference setting, the minimum length of characters in order to start completing */
#define KEY_LENGTH 2
/* This could be a preference setting, use calendar buttons in tree view */
#define CAL_USE_BUTTONS TRUE
/* This could be a preference setting, show currency / commodity symbols */
#define SHOW_SYMBOL FALSE
/* This could be a preference setting, move selection to blank split on expand */
#define SELECTION_TO_BLANK_ON_EXPAND FALSE
/* This could be a preference setting, display the entered date */
#define SHOW_ENTERED_DATE FALSE

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

    gnc_tree_view_split_reg_signals[UPDATE_SIGNAL] =
        g_signal_new("update_signal",
                     G_TYPE_FROM_CLASS (o_class),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET (GncTreeViewSplitRegClass, update_signal),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    gnc_tree_view_split_reg_signals[HELP_SIGNAL] =
        g_signal_new("help_signal",
                     G_TYPE_FROM_CLASS (o_class),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET (GncTreeViewSplitRegClass, help_signal),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    klass->update_signal = NULL;
    klass->help_signal = NULL;

}


/* Return the tree model from the tree view */
static GncTreeModelSplitReg *
get_split_reg_model_from_view (GncTreeViewSplitReg *view)
{
    GtkTreeModelSort *s_model = GTK_TREE_MODEL_SORT(
        gtk_tree_view_get_model (GTK_TREE_VIEW (view)));
    return GNC_TREE_MODEL_SPLIT_REG (gtk_tree_model_sort_get_model (s_model));
}


static void
gnc_tree_view_split_reg_init (GncTreeViewSplitReg *view)
{
    view->priv = g_new0 (GncTreeViewSplitRegPrivate, 1);

    view->priv->current_trans = NULL;
    view->priv->current_split = NULL;
    view->priv->current_depth = 0;
    view->reg_closing = FALSE;
    view->priv->fo_handler_id = 0;
    view->sort_depth = 1;

    view->priv->acct_short_names = gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "show_leaf_account_names", NULL);
}


static void
gnc_tree_view_split_reg_dispose (GObject *object)
{
    GncTreeViewSplitReg *view;
    GncTreeViewSplitRegPrivate *priv;

    gnc_leave_return_if_fail (object != NULL);
    gnc_leave_return_if_fail (GNC_IS_TREE_VIEW_SPLIT_REG (object));

    ENTER("split reg view %p", object);

    view = GNC_TREE_VIEW_SPLIT_REG (object);
    priv = GNC_TREE_VIEW_SPLIT_REG_GET_PRIVATE (view);

    if (priv->disposed)
        return;
    priv->disposed = TRUE;

    if(view->priv->current_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->current_ref);
        view->priv->current_ref = NULL;
    }

    if(view->priv->edit_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->edit_ref);
        view->priv->edit_ref = NULL;
    }

    if (G_OBJECT_CLASS (parent_class)->dispose)
        (* G_OBJECT_CLASS (parent_class)->dispose) (object);

    LEAVE(" ");
}


static void
gnc_tree_view_split_reg_finalize (GObject *object)
{
    GncTreeViewSplitReg *view;

    gnc_leave_return_if_fail(object != NULL);
    gnc_leave_return_if_fail(GNC_IS_TREE_VIEW_SPLIT_REG (object));

    ENTER("split reg view %p", object);

    view = GNC_TREE_VIEW_SPLIT_REG (object);

    if (G_OBJECT_CLASS(parent_class)->finalize)
        (* G_OBJECT_CLASS(parent_class)->finalize) (object);

    LEAVE(" ");
}


/* Update some settings from gconf */
void
gnc_tree_view_split_reg_refresh_from_gconf (GncTreeViewSplitReg *view) //FIXME Need to test and change.
{
    GncTreeModelSplitReg *model;

//g_print("gnc_tree_view_split_reg_refresh_from_gconf\n");

    model = get_split_reg_model_from_view (view);

    model->use_theme_colors = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER,
                              "use_theme_colors", NULL);
    model->use_accounting_labels = gnc_gconf_get_bool (GCONF_GENERAL,
                               KEY_ACCOUNTING_LABELS, NULL);

    model->alt_colors_by_txn = gnc_gconf_get_bool (GCONF_GENERAL_REGISTER,
                               "alternate_color_by_transaction", NULL);

//    sheet->use_horizontal_lines = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER,
//                                  "draw_horizontal_lines", NULL);

//    sheet->use_vertical_lines = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER,
//                                "draw_vertical_lines", NULL);
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


/* Define which columns are in which views */
static ViewCol *
gnc_tree_view_split_reg_get_colummn_list (GncTreeModelSplitReg *model)
{
    DEBUG("Model-type is %d", model->type);
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
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID, COL_RECN,
        COL_STATUS, COL_DEBIT, COL_CREDIT, COL_BALANCE, -1};
        return col_list;
        }
        break;

    case GENERAL_LEDGER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID, COL_RECN,
        COL_STATUS, COL_COMM, COL_VALUE, COL_RATE, COL_AMOUNT, COL_DEBIT, COL_CREDIT, -1};
        return col_list;
        }
        break;

    case STOCK_REGISTER2:
    case CURRENCY_REGISTER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID, COL_RECN,
        COL_STATUS, COL_COMM, COL_AMTVAL, COL_PRICE, COL_DEBIT, COL_CREDIT, COL_BALANCE, -1};
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
        COL_STATUS, COL_AMOUNT, COL_PRICE, COL_DEBIT, COL_CREDIT, -1};
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

        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (col));
        g_assert (g_list_length (renderers) == 1);
        cr = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        /* Setup cell background color and default alignment */
        g_object_set (cr, "xalign", 1.0, NULL);

        /* Add the full title for status column to the object for menu creation */
        if (col_list[i] == COL_STATUS)
            g_object_set_data_full(G_OBJECT(col), REAL_TITLE, g_strdup(_("Status Bar")), g_free);

        /* This sets the background of the treeview control columns */
        gnc_tree_view_set_control_column_background (GNC_TREE_VIEW (view), 0, control_cdf);

        if (def.editing_started_cb) {
            //Store the position of the column in the model
            g_object_set_data (G_OBJECT(cr), "model_column", GINT_TO_POINTER (def.modelcol));
            g_object_set_data (G_OBJECT(cr), "column_name", GINT_TO_POINTER (def.pref_name));
            g_signal_connect (G_OBJECT(cr), "editing-started", (GCallback) def.editing_started_cb, view);
        }

        // Connect editing-canceled signal so that edit-cancelled can be set appropriately
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
    gtk_tree_selection_set_mode (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), GTK_SELECTION_BROWSE);

    // Default the sorting to date.
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(gtk_tree_view_get_model (GTK_TREE_VIEW (view))),
                                         GNC_TREE_MODEL_SPLIT_REG_COL_DATE,
                                         GTK_SORT_ASCENDING);

    // Connect a call back to update the sort settings.
    g_signal_connect (GTK_TREE_SORTABLE (gtk_tree_view_get_model (GTK_TREE_VIEW (view))),
        "sort-column-changed", G_CALLBACK (gnc_tree_control_split_reg_sort_changed_cb), view);

    // This will expand to splits on double clicking at current position.
    g_signal_connect (GTK_TREE_VIEW (view), "row-activated", G_CALLBACK (gtv_split_reg_double_click_cb), NULL);

    g_signal_connect(gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), "changed", G_CALLBACK (gtv_split_reg_motion_cb), view);

    //Add a data-edited property to keep track of transaction edits
    g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));

    // This is used to move the selected item if the selected transaction is deleted
    g_signal_connect (G_OBJECT (model), "trans_delete", G_CALLBACK (gtv_split_reg_trans_delete_cb), view);

    // This will refresh the view
    g_signal_connect (G_OBJECT (model), "refresh_view", G_CALLBACK (gtv_split_reg_refresh_cb), view);

    // This should be for key navigation, tabbing...
    g_signal_connect_after (G_OBJECT (view), "key-press-event", G_CALLBACK (gtv_split_reg_key_press_cb), NULL);

    return view;
}


/* Set up the view */
static gboolean
gnc_tree_view_split_reg_set_format (GncTreeViewSplitReg *view)
{
    GncTreeViewSplitRegPrivate *priv;
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;
    gint total_num = 0;

    ENTER(" ");

    model = get_split_reg_model_from_view (view);

    priv = view->priv;

    total_num = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (model), NULL);

    mpath = gtk_tree_row_reference_get_path (view->priv->current_ref);
    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), mpath);

    priv->expanded = FALSE;

    {
        if (model->style == REG2_STYLE_JOURNAL)
        {
            gtk_tree_view_expand_all (GTK_TREE_VIEW (view));

            priv->expanded = TRUE;

            /* This updates the plugin page gui */
            if (view->moved_cb)
                (view->moved_cb)(view, view->moved_cb_data);

            LEAVE("journal foramt");
            return (FALSE);
        }

        if (!model->use_double_line)
        {
            gtk_tree_view_collapse_all (GTK_TREE_VIEW (view));

            priv->expanded = FALSE;

            /* This updates the plugin page gui */
            if (view->moved_cb)
                (view->moved_cb)(view, view->moved_cb_data);

            LEAVE("single line foramt");
            return (FALSE);
        }

        if (model->use_double_line)
        {
            gint index = 0;
            GtkTreePath *path;

            path = gtk_tree_path_new_first ();
            while (index < total_num)
            {
                gtk_tree_view_expand_to_path (GTK_TREE_VIEW (view), path);
                gtk_tree_path_down (path);
                gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), path);
                gtk_tree_path_up (path);
                gtk_tree_path_next (path); //Next Transaction
                index = index + 1;
            }
            gtk_tree_path_free (path);
            LEAVE("double line format");
        }

        /* This expands to split from top level auto.. */
        if ((model->style == REG2_STYLE_AUTO_LEDGER) || (model->style == REG2_STYLE_JOURNAL))
        {
            gtk_tree_view_expand_row (GTK_TREE_VIEW (view), spath, TRUE);

            priv->expanded = TRUE;
            LEAVE("auto expand line format");
        }
    }

    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);

    /* This updates the plugin page gui */
    if (view->moved_cb)
        (view->moved_cb)(view, view->moved_cb_data);

    return (FALSE);
}


/* Callback to update the view after transactions are added or deleted */
static void
gtv_split_reg_refresh_cb (GncTreeModelSplitReg *model, gpointer user_data)
{
    GncTreeViewSplitReg *view = user_data;

    if (view->reg_closing != TRUE)
        /* Set the view format */
        g_idle_add ((GSourceFunc)gnc_tree_view_split_reg_set_format, view);
}


/* Create a tree view from a given model */
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

    /* Set the user_data for the sort callback */
    gnc_tree_view_set_sort_user_data (GNC_TREE_VIEW (view), view);

    view->priv->anchor = gnc_tree_model_split_reg_get_anchor (model);
    view->priv->reg_comm = xaccAccountGetCommodity (view->priv->anchor);
    view->help_text = NULL;

    gnc_tree_view_split_reg_set_cols (view, gnc_tree_view_split_reg_get_colummn_list (model));

    /* Set default visibilities */
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (view), TRUE);

    /* TreeView Grid lines */
    if (gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "draw_horizontal_lines", NULL))
    {
        if (gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "draw_vertical_lines", NULL))
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_BOTH);
        else
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_HORIZONTAL);
    }
    else if (gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "draw_vertical_lines", NULL))
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_VERTICAL);
    else
        gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_NONE);

    /* Expanders off */
    gtk_tree_view_set_show_expanders (GTK_TREE_VIEW (view), FALSE);

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
    GtkTreePath *new_mpath, *mpath, *spath;
    gint *indices;

    ENTER(" ");

    model = get_split_reg_model_from_view (view);

    /* Set the default start position to end of list */
    if (view->priv->current_trans == NULL)
    {
        /* both values NULL will return last in list */
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, NULL);
        view->priv->current_trans = gnc_tree_control_split_reg_get_blank_trans (view);
    }
    else
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, view->priv->current_split, view->priv->current_trans);

    indices = gtk_tree_path_get_indices (mpath);

    new_mpath = gtk_tree_path_new_from_indices (indices[0], -1);

    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), new_mpath);

    DEBUG("default_selection mpath is %s, spath is %s, new path is %s", gtk_tree_path_to_string (mpath),
                                    gtk_tree_path_to_string (spath), gtk_tree_path_to_string (new_mpath));

    view->priv->current_depth = gtk_tree_path_get_depth (spath);

    if (view->priv->current_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->current_ref);
        view->priv->current_ref = NULL;
    }
    view->priv->current_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), new_mpath);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), spath);

    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);
    gtk_tree_path_free (new_mpath);

    /* Set the view format */
    g_idle_add ((GSourceFunc)gnc_tree_view_split_reg_set_format, view);

    /* scroll window to show selection when view is idle */
    g_idle_add ((GSourceFunc)gtv_scroll_to_cell, view );

    LEAVE(" ");
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

    ENTER("needs_exchange_rate - trans %p and split %p", trans, split);

    txn_curr = xaccTransGetCurrency (trans);
    split_com = xaccAccountGetCommodity (xaccSplitGetAccount (split));
    if (split_com && txn_curr && !gnc_commodity_equiv (split_com, txn_curr))
    {
        LEAVE("needs_exchange_rate split_com to txn_curr return TRUE");
        return TRUE;
    }

    reg_com = view->priv->reg_comm;
    if (split_com && reg_com && !gnc_commodity_equiv (split_com, reg_com))
    {
        LEAVE("needs_exchange_rate split_com and reg_com return TRUE");
        return TRUE;
    }
    LEAVE("No Exchange rate needed");
    return FALSE;
}


/* Get the rate from the price db */
static gnc_numeric
gtv_get_rate_from_db (gnc_commodity *from, gnc_commodity *to)
{
    GNCPrice *prc;
    gnc_numeric rate_split;
    gboolean have_rate = FALSE;
    QofBook *book = gnc_get_current_book ();

    /* Do we have a rate allready */
    prc = gnc_pricedb_lookup_latest (gnc_pricedb_get_db (book), from, to);
    if (prc)
    {
        rate_split = gnc_price_get_value (prc);
        gnc_price_unref (prc);
        have_rate = TRUE;
    }

    /* Lets try reversing the commodities */
    if (!have_rate)
    {
        prc = gnc_pricedb_lookup_latest (gnc_pricedb_get_db (book), to, from);
        if (prc)
        {
            rate_split = gnc_numeric_div (gnc_numeric_create (1, 1), gnc_price_get_value (prc),
                                 GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);

            gnc_price_unref (prc);
            have_rate = TRUE;
        }
    }

    /* No rate, set to 1/1 */
    if (!have_rate)
        rate_split = gnc_numeric_create (1, 1);

    return rate_split;
}


/* Either sets the value and amount for split and returns TRUE, or
   does nothing and returns FALSE. */
static gboolean
handle_exchange_rate (GncTreeViewSplitReg *view, gnc_numeric amount, Transaction *trans, Split *split, gboolean force)
{
    XferDialog *xfer;
    gboolean rate_split_ok, rate_reg_ok;
    gnc_numeric rate_split, rate_reg, value;
    Account *reg_acc = view->priv->anchor;
    gnc_commodity *xfer_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));
    gnc_commodity *reg_comm = view->priv->reg_comm;
    gnc_commodity *trans_curr = xaccTransGetCurrency (trans);
    gboolean expanded;
    gboolean have_rate = TRUE;


    ENTER("handle_exchange_rate amount %s, trans %p and split %p force %d", gnc_numeric_to_string (amount), trans, split, force);

    /* Rate from trans-curr to split-comm */
    rate_split_ok = xaccTransGetRateForCommodity (trans, xfer_comm, split, &rate_split);

    /* Rate from trans-curr to reg-comm */
    rate_reg_ok = xaccTransGetRateForCommodity (trans, reg_comm, split, &rate_reg);

    /* Are we expanded */
    expanded = view->priv->expanded;

    if (rate_reg_ok && rate_split_ok && !force)
    {
        value = gnc_numeric_div (amount, rate_reg, gnc_commodity_get_fraction (trans_curr), GNC_HOW_DENOM_REDUCE);
        amount = gnc_numeric_mul (value, rate_split, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }
    else
    {
        if (!rate_split_ok)
            rate_split = gtv_get_rate_from_db (reg_comm, xfer_comm);

        /* create the exchange-rate dialog */
        xfer = gnc_xfer_dialog (NULL, NULL);

        gnc_xfer_dialog_is_exchange_dialog (xfer, &rate_split);

        /* fill in the dialog entries */
        gnc_xfer_dialog_set_description (xfer, xaccTransGetDescription (trans));
        gnc_xfer_dialog_set_memo (xfer, xaccSplitGetMemo (split));

        /* Get per book option */
        gnc_xfer_dialog_set_num (xfer, gnc_get_num_action (trans, split));
        gnc_xfer_dialog_set_date (xfer, timespecToTime64 (xaccTransRetDatePostedTS (trans)));

        value = amount;
        if (gnc_xfer_dialog_run_exchange_dialog (xfer, &rate_split, value, reg_acc, trans, xfer_comm, expanded))
        {
            if (!rate_split_ok)
                rate_split = gnc_numeric_create (1, 1);
            have_rate = FALSE;
        }
        else
            have_rate = TRUE;

        amount = gnc_numeric_mul (value, rate_split, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    }
    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, value);

    LEAVE("handle_exchange_rate set split %p amt=%s; and val=%s", split, gnc_numeric_to_string (amount), gnc_numeric_to_string (value));
    return have_rate;
}


#define set_value_for gnc_tree_view_split_reg_set_value_for
void
gnc_tree_view_split_reg_set_value_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input, gboolean force)
{
    GtkWidget *window;
    Account *anchor = view->priv->anchor;
    Account *acct = xaccSplitGetAccount (split);
    gnc_commodity *currency;
    gnc_numeric value, amount, rate;

    ENTER("set_value_for trans %p and split %p input %s force %d", trans, split, gnc_numeric_to_string (input), force);

    currency = xaccTransGetCurrency (trans);

    if (gnc_numeric_zero_p (input))
    {
        xaccSplitSetValue (split, input);
        xaccSplitSetAmount (split, input);
        LEAVE("input is zero");
        return;
    }

    window = gnc_tree_view_split_reg_get_parent (view);

    if (needs_exchange_rate (view, trans, split))
    {
        if (handle_exchange_rate (view, input, trans, split, force))
        {
            ; //FIXME ??????
        }
        else
        {
            gnc_error_dialog (window, "%s",
                         _("Exchange Rate Canceled, using existing rate or default 1 to 1 rate if this is a new transaction."));
        }
        LEAVE("used exchange rate");
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

        /* Convert from the anchor account's commodity to trans currency */
        if (gnc_commodity_equiv (currency, reg_com))
            value = input;
        else 
        {
            if (!xaccTransGetRateForCommodity (trans, reg_com, NULL, &rate))
            {
                LEAVE("");
                return;
            }

            if (gnc_numeric_zero_p (rate))
            {
                xaccTransSetCurrency (trans, reg_com);
                value = input;
            }
            else
            {
                value = gnc_numeric_div (
                    input, rate,
                    GNC_DENOM_AUTO, //?
                    //gnc_commodity_get_fraction(currency),
                    GNC_HOW_RND_ROUND);
            }
        }
        xaccSplitSetValue (split, value);

        if (gnc_commodity_equiv (split_com, reg_com))
        {
            amount = input;
        }
        else
        {
            rate = xaccTransGetAccountConvRate (trans, acct);
            amount = gnc_numeric_mul (value, rate, xaccAccountGetCommoditySCU (acct), GNC_HOW_RND_ROUND);
        }
        xaccSplitSetAmount (split, amount);
    }
    else
    {
        value = input;
        xaccSplitSetValue (split, value);

        rate = xaccTransGetAccountConvRate (trans, acct);
        amount = gnc_numeric_mul (value, rate, xaccAccountGetCommoditySCU (acct), GNC_HOW_RND_ROUND);
        if (gnc_numeric_check (amount) == GNC_ERROR_OK)
        {
            xaccSplitSetAmount (split, amount);
        }
    }
    LEAVE(" ");
}


/* Returns a value for display. */
static gnc_numeric
get_value_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gboolean is_blank)
{
    gnc_commodity *currency = xaccTransGetCurrency (trans);
    gnc_numeric total;

    ENTER("get_value_for trans %p and split %p is_blank %d", trans, split, is_blank);

    total = xaccSplitGetValue (split);

    if (is_blank && gnc_numeric_zero_p (total)) //blank split and total zero
    {
        gnc_numeric rate;
        total = gnc_numeric_neg (xaccTransGetImbalanceValue (trans));
        if (!gnc_numeric_zero_p (total))
        {
            if (!xaccTransGetRateForCommodity (trans, view->priv->reg_comm, NULL, &rate))
            {
                LEAVE("zero");
                return gnc_numeric_zero();
            }

            total = gnc_numeric_mul (
                total, rate,
                gnc_commodity_get_fraction (currency),
                GNC_HOW_RND_ROUND);
        }
    }
    else
    {
        if (!gnc_numeric_zero_p (total) && gnc_numeric_check (total) == GNC_ERROR_OK)
        {
            /* if needs conversion? */
            gnc_commodity *commodity = view->priv->reg_comm;
            if (commodity && gnc_commodity_is_currency (view->priv->reg_comm)) //test for a currency register
            {
                if (!gnc_commodity_equiv (commodity, currency))
                {
                    total = xaccSplitConvertAmount (split, view->priv->anchor);
                }
            }
        }
    }
    LEAVE("return value is %s", gnc_numeric_to_string (total));
    return total;
}


/* Returns the other Split based on the current Account */
/* Only used with two split transactions */
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


/* Returns the value denom */
static int
gnc_tree_view_split_reg_get_value_denom (Split *split)
{
    gnc_commodity *currency;
    int denom;

    currency = xaccTransGetCurrency (xaccSplitGetParent (split));
    denom = gnc_commodity_get_fraction (currency);
    if (denom == 0)
    {
        gnc_commodity *commodity = gnc_default_currency ();
        denom = gnc_commodity_get_fraction (commodity);
        if (denom == 0)
            denom = 100;
    }
    return denom;
}


/* Returns the amount denom */
static int
gnc_tree_view_split_reg_get_amount_denom (Split *split)
{
    int denom;

    denom = xaccAccountGetCommoditySCU (xaccSplitGetAccount (split));
    if (denom == 0)
    {
        gnc_commodity *commodity = gnc_default_currency ();
        denom = gnc_commodity_get_fraction (commodity);
        if (denom == 0)
            denom = 100;
    }
    return denom;
}


/* Takes the input with column and sets the price / amount / value so they are consistent */
static void
set_number_for_input (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input, gint viewcol)
{
    gnc_numeric  price;
    gnc_numeric  amount;
    gnc_numeric  value;

    gboolean price_changed = FALSE;   // Price of each share
    gboolean value_changed = FALSE;   // Total value of shares
    gboolean amount_changed = FALSE;  // No of shares

    gboolean recalc_amount = FALSE;
    gboolean recalc_price = FALSE;
    gboolean recalc_value = FALSE;
    int denom;

    ENTER("set_number_for_input trans %p and split %p and input is %s and viewcol is %d", trans, split, gnc_numeric_to_string (input), viewcol);

    if (gnc_numeric_zero_p (input))
    {
        xaccSplitSetValue (split, input);
        xaccSplitSetAmount (split, input);
        LEAVE("zero");
        return;
    }

    amount = xaccSplitGetAmount (split);
    value = xaccSplitGetValue (split);

    if (viewcol == COL_AMTVAL && !view->priv->expanded)
    {
        value_changed = TRUE;
        if (gnc_numeric_zero_p (amount))
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, input);
            LEAVE("");
            return;
        }
    }
    else if (viewcol == COL_AMTVAL && view->priv->expanded)
    {
        amount_changed = TRUE;
        if (gnc_numeric_zero_p (value))
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, input);
            LEAVE("");
            return;
        }
    }

    if (viewcol == COL_PRICE)
    {
        price_changed = TRUE;
        if (gnc_numeric_zero_p (value))
        {
            amount = gnc_numeric_create (1,1);
            value = gnc_numeric_mul (input, amount, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, amount);
            LEAVE("");
            return;
        }
    }

    if (viewcol == COL_CREDIT || viewcol == COL_DEBIT)
    {
        amount_changed = TRUE;
        if (gnc_numeric_zero_p (value))
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, input);
            LEAVE("");
            return;
        }
    }

    DEBUG("value_changed %d, price_changed %d, amount_changed %d", value_changed, price_changed, amount_changed);

    {
        int choice;
        int default_value;
        GList *node;
        GList *radio_list = NULL;
        const char *title = _("Recalculate Transaction");
        const char *message = _("The values entered for this transaction "
                                "are inconsistent. Which value would you "
                                "like to have recalculated?");

        if (amount_changed)
            radio_list = g_list_append (radio_list,
                                        g_strdup_printf ("%s (%s)",
                                                _("_Shares"), _("Changed")));
        else
            radio_list = g_list_append (radio_list, g_strdup (_("_Shares")));

        if (price_changed)
            radio_list = g_list_append (radio_list,
                                        g_strdup_printf ("%s (%s)",
                                                _("_Price"), _("Changed")));
        else
            radio_list = g_list_append (radio_list, g_strdup (_("_Price")));

        if (value_changed)
            radio_list = g_list_append (radio_list,
                                        g_strdup_printf ("%s (%s)",
                                                _("_Value"), _("Changed")));
        else
            radio_list = g_list_append (radio_list, g_strdup (_("_Value")));

        if (price_changed)
            default_value = 0;  /* change the amount / shares */
        else
            default_value = 1;  /* change the value */

        choice = gnc_choose_radio_option_dialog
                 (gnc_tree_view_split_reg_get_parent (view),
                  title,
                  message,
                  _("_Recalculate"),
                  default_value,
                  radio_list);

        for (node = radio_list; node; node = node->next)
            g_free (node->data);

        g_list_free (radio_list);

        switch (choice)
        {
        case 0: /* Modify number of shares */
            recalc_amount = TRUE;
            break;
        case 1: /* Modify the share price */
            recalc_price = TRUE;
            break;
        case 2: /* Modify total value */
            recalc_value = TRUE;
            break;
        default: /* Cancel */
            return;
        }
    }

    DEBUG("recalc_value %d, recalc_price %d, recalc_amount %d", recalc_value, recalc_price, recalc_amount);

    if (recalc_amount)
    {
        denom = gnc_tree_view_split_reg_get_amount_denom (split);

        if (amount_changed)
        {
            LEAVE("");
            return;
        }

        if (price_changed)
            price = input;
        else
            price = gnc_numeric_div (value, amount, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);

        if (value_changed)
        {
            xaccSplitSetValue (split, input);
            amount = gnc_numeric_div (input, price, denom, GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetAmount (split, amount);
        }
        else
        {
            amount = gnc_numeric_div (value, price, denom, GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetAmount (split, amount);
        }
    }

    if (recalc_price)
    {
        if (price_changed)
        {
            LEAVE("");
            return;
        }

        if (amount_changed)
        {
            xaccSplitSetAmount (split, input);
            xaccSplitSetValue (split, value);
        }

        if (value_changed)
        {
            xaccSplitSetValue (split, input);
            xaccSplitSetAmount (split, amount);
        }
    }

    if (recalc_value)
    {
        denom = gnc_tree_view_split_reg_get_value_denom (split);

        if (value_changed)
        {
            LEAVE("");
            return;
        }

        if (price_changed)
            price = input;
        else
            price = gnc_numeric_div (value, amount, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);

        if (amount_changed)
        {
            xaccSplitSetAmount (split, input);
            value = gnc_numeric_mul (input, price, denom, GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetValue (split, value);
        }
        else
        {
            value = gnc_numeric_mul (amount, price, denom, GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetValue (split, value);
        }

        /* If the number of splits is two, change other split to balance */
        if ((xaccTransCountSplits (trans) == 2) && view->priv->expanded)
        {
            Split *osplit;
            gnc_commodity *split_com;

            osplit = get_other_split (view, trans);

            split_com = xaccAccountGetCommodity (xaccSplitGetAccount (osplit));

            if (gnc_commodity_is_currency (split_com))
            {
                if (gnc_numeric_negative_p (value))
                {
                    xaccSplitSetValue (osplit, gnc_numeric_neg (value));
                    xaccSplitSetAmount (osplit, gnc_numeric_neg (value));
                }
                else
                {
                    xaccSplitSetValue (osplit, value);
                    xaccSplitSetAmount (osplit, value);
                }
            }
        }
    }
    LEAVE("");
}


/* Set the value for the given input amount */
static void
set_value_for_amount (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gnc_numeric input)
{
    gnc_numeric  split_rate;
    gnc_numeric  amount;
    gnc_numeric  value, new_value;
    int denom;

    ENTER("set_value_for_amount trans %p and split %p and input is %s", trans, split, gnc_numeric_to_string (input));

    if (gnc_numeric_zero_p (input))
    {
        xaccSplitSetValue (split, input);
        xaccSplitSetAmount (split, input);
        LEAVE("zero");
        return;
    }

    amount = xaccSplitGetAmount (split);
    value = xaccSplitGetValue (split);

    denom = gnc_tree_view_split_reg_get_value_denom (split);

    split_rate = gnc_numeric_div (value, amount, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
    if (gnc_numeric_check (split_rate) != GNC_ERROR_OK)
        split_rate = gnc_numeric_create (1,1);

    new_value = gnc_numeric_mul (input, split_rate, denom, GNC_HOW_RND_ROUND_HALF_UP);

    xaccSplitSetValue (split, new_value);
    xaccSplitSetAmount (split, input);

    LEAVE("");
}


/* Get the rate */
static gnc_numeric
get_rate_for (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gboolean is_blank)
{
    gnc_numeric num;

    ENTER("get_rate_for trans %p and split %p is_blank %d", trans, split, is_blank);

    num = get_value_for (view, trans, split, is_blank);
    num = gnc_numeric_div ( xaccSplitGetAmount (split), num, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    LEAVE("get_rate_for split amount is %s and return num is %s", gnc_numeric_to_string (xaccSplitGetAmount (split)), gnc_numeric_to_string (num));
    return num;
}


/* The returned Splits may be newly created and not yet belong to trans. */
static gboolean
get_split_pair (GncTreeViewSplitReg *view, Transaction *trans, Split **osplit, Split **split)
{
    QofBook       *book;

    gint count = xaccTransCountSplits (trans);
    Account *anchor = view->priv->anchor;

    book = gnc_get_current_book();

    if (count == 0)
    {
        *split = xaccMallocSplit (book);
        xaccSplitSetAccount (*split, anchor);
        xaccSplitSetParent (*split, trans);
        *osplit = xaccMallocSplit (book);
        xaccSplitSetParent (*osplit, trans);
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
    DEBUG("get_split_pair return - trans is %p, osplit is %p and split %p is set to anchor %p", trans, *osplit, *split, anchor);
    return TRUE;
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


/* Get the model iter from the view path string */
static gboolean
get_model_iter_from_view_string (GncTreeViewSplitReg *view,
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


/* Get the model iter from the selection */
static gboolean
get_model_iter_from_selection (GncTreeViewSplitReg *view,
                              GtkTreeSelection *sel, GtkTreeIter *iter)
{
    GtkTreeModel *s_model;
    GtkTreeIter s_iter;

    if (gtk_tree_selection_get_selected (sel, &s_model, &s_iter))
    {
        gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), iter, &s_iter);
        return TRUE;
    }
    return FALSE;
}


/* Only allow changes to values if we have valid split accounts */
static gboolean
have_account (GncTreeViewSplitReg *view, RowDepth depth, gboolean expanded, Transaction *trans, Split *split)
{
    gboolean have_account = FALSE;

    DEBUG("have_account trans %p, split %p, expanded %d, depth %d", trans, split, expanded, depth);

    if ((depth == TRANS1) && !expanded && (xaccTransCountSplits (trans) == 2)) // normal trans
    {
        if (xaccSplitGetAccount (get_other_split (view, trans)) != NULL)
            have_account = TRUE;
    }

    if ((depth == SPLIT3) && (xaccTransCountSplits (trans) == 0)) // blank trans, blank split
        have_account = TRUE;

    if (depth == SPLIT3) // normal split
    {
        if (xaccSplitGetAccount (split) != NULL)
            have_account = TRUE;
    }

    return have_account;
}


/* This cellDataFunc is to set the cell-background property of the control columns. */
static void
control_cdf (GtkTreeViewColumn *col, GtkCellRenderer *cell, GtkTreeModel *s_model,
    GtkTreeIter *s_iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    GtkTreePath *mpath, *spath;
    Transaction *trans;
    Split *split;
    gboolean is_split, is_blank, is_trow1, is_trow2;
    const gchar *row_color;

    gint *indices;

    ENTER("");

    model = get_split_reg_model_from_view (view);

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), &iter, s_iter);

    g_return_if_fail (gnc_tree_model_split_reg_get_split_and_trans (
                         GNC_TREE_MODEL_SPLIT_REG (model), &iter,
                          &is_trow1, &is_trow2, &is_split, &is_blank,
                          &split, &trans));

    mpath = gtk_tree_model_get_path (GTK_TREE_MODEL (model), &iter);

    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), mpath);

    indices = gtk_tree_path_get_indices (spath);

    row_color = gnc_tree_model_split_reg_get_row_color (model, is_trow1, is_trow2, is_split, indices[0]);

    gtk_tree_path_free (spath);
    gtk_tree_path_free (mpath);

    /* Set the background color / this works for sorting and deleting transactions */
    g_object_set (cell, "cell-background", row_color, (gchar*)NULL);

    LEAVE("");
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
    GtkTreePath *path, *mpath, *spath;
    ViewCol viewcol;
    Transaction *trans;
    Split *split;
    gboolean is_split, is_blank, is_trow1, is_trow2;
    gboolean editable = FALSE, expanded = FALSE;
    gboolean read_only = FALSE;
    gboolean open_edited = FALSE;
    gint num_of_splits = 0;
    gnc_numeric num;
    const gchar *s = "";
    const gchar *row_color;

    RowDepth depth;

    gint *indices;

    Account *anchor = view->priv->anchor;

    ENTER("");

    model = get_split_reg_model_from_view (view);

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), &iter, s_iter);

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    g_return_if_fail (gnc_tree_model_split_reg_get_split_and_trans (
                         GNC_TREE_MODEL_SPLIT_REG (model), &iter,
                          &is_trow1, &is_trow2, &is_split, &is_blank,
                          &split, &trans));

    mpath = gtk_tree_model_get_path (GTK_TREE_MODEL (model), &iter);

    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), mpath);

    depth = gtk_tree_path_get_depth (spath);

    indices = gtk_tree_path_get_indices (spath);

    row_color = gnc_tree_model_split_reg_get_row_color (model, is_trow1, is_trow2, is_split, indices[0]);

    gtk_tree_path_free (spath);
    gtk_tree_path_free (mpath);

    /* Set the background color / this works for sorting and deleting of transactions */
    g_object_set (cell, "cell-background", row_color, (gchar*)NULL);

    /* Get the read only model setting */
    gtk_tree_model_get (GTK_TREE_MODEL (model), &iter, GNC_TREE_MODEL_SPLIT_REG_COL_RO, &read_only, -1);

    /* Are we being edited in other register */
    if (xaccTransIsOpen (trans) && (view->priv->dirty_trans != trans))
    {
        read_only = TRUE;
        open_edited = TRUE;
    }

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
    else
    {
        num_of_splits = xaccTransCountSplits (trans);
        expanded = TRUE;
    }

    switch (viewcol) {
    case COL_DATE:
        /* Column is DATE */
        if (is_split)
            g_object_set (cell, "cell-background", "white", (gchar*)NULL);

        if (is_trow2 && SHOW_ENTERED_DATE)
            g_object_set (cell, "cell-background", YELLOWCELL, (gchar*)NULL);

        if (is_trow1) {
            Timespec ts = {0,0};
            xaccTransGetDatePostedTS (trans, &ts);
            //If the time returned by xaccTransGetDatePostedTS is 0 then assume it
            //is a new transaction and set the time to current time to show current
            //date on new transactions
            if (ts.tv_sec == 0)
            {
                ts.tv_sec = gnc_time (NULL);
                //xaccTransSetDatePostedSecs(trans, ts.tv_sec);
            }//if
            s = gnc_print_date(ts);
            editable = TRUE;
        }
        else if (is_trow2 && SHOW_ENTERED_DATE) {
            Timespec ts = {0,0};
            xaccTransGetDateEnteredTS (trans, &ts);
            //If the time returned by xaccTransGetDateEnteredTS is 0 then assume it
            //is a new transaction and set the time to current time to show current
            //date on new transactions
            if (ts.tv_sec == 0)
            {
                ts.tv_sec = gnc_time (NULL);
                //xaccTransSetDateEnteredSecs(trans, ts.tv_sec);
            }//if
            s = gnc_print_date(ts);
            editable = FALSE;
        }
        else {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        /* This will remove the calander buttons if FALSE */
        g_object_set (cell, "use_buttons", CAL_USE_BUTTONS, NULL );
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
            /* Get per book option */
            s = gnc_get_num_action (trans, get_this_split (view, trans));
        else if (is_trow2 && (qof_book_use_split_action_for_num_field (gnc_get_current_book())))
            /* Get per book option */
            s = gnc_get_action_num (trans, get_this_split (view, trans));
        else if (is_trow2 && !expanded && (!qof_book_use_split_action_for_num_field (gnc_get_current_book())))
            /* Get per book option */
            s = gnc_get_action_num (trans, get_this_split (view, trans));
        else if (is_split)
            /* Get split-action with gnc_get_num_action which is the same as
             * xaccSplitGetAction with these arguments */
            s = gnc_get_num_action (NULL, split);
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
            else if (num_of_splits == 0)
            {
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

            if ((xaccTransCountSplits (trans) == 0) && (model->type != GENERAL_LEDGER2)) // First split on blank transaction 
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

            if (anchor == acct && model->type != GENERAL_LEDGER2)
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
            if (rec == VREC || rec == FREC)
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
            if (rec == VREC || rec == FREC)
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
        if (is_split)
        {
            gnc_numeric val = xaccSplitGetValue (split);
            s = xaccPrintAmount (val, gnc_commodity_print_info (xaccTransGetCurrency (trans), SHOW_SYMBOL));
            editable = FALSE;

            if (get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }
        else
        {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_RATE:
        /* Column is RATE */
        if ((is_trow1)||(is_trow2))
        {
            s = "";
            editable = FALSE;
        }
        else
        {
            gnc_commodity *split_com = xaccAccountGetCommodity (xaccSplitGetAccount (split));
            num = get_rate_for (view, trans, split, is_blank);
            if (gnc_numeric_check (num) == GNC_ERROR_OK)
            {
                s = xaccPrintAmount (num, gnc_split_amount_print_info (split, SHOW_SYMBOL));
                editable = !gnc_numeric_zero_p (num) && gnc_commodity_equiv (split_com, view->priv->reg_comm);
                editable = FALSE;
            }
            else
            {
                s = "";
                editable = FALSE;
            }

            if (get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_AMOUNT:
        /* Column is AMOUNT */
        if (is_split && (anchor == NULL))
        {
            gnc_numeric amt = xaccSplitGetAmount (split);
            s = xaccPrintAmount (amt, gnc_account_print_info (xaccSplitGetAccount (split), SHOW_SYMBOL));
            editable = FALSE;

            if (get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }
        else if (is_split && (anchor))
        {
            gnc_commodity *split_comm;
            split_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));

            if (!gnc_commodity_is_currency (split_comm) || (is_blank))
            {
                gnc_numeric amt = xaccSplitGetAmount (split);
                s = xaccPrintAmount (amt, gnc_account_print_info (xaccSplitGetAccount (split), SHOW_SYMBOL));
                editable = FALSE;
            }

            if (get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }
        else
        {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_AMTVAL:
        /* Column is AMOUNT / VALUE */
        if (is_trow2)
        {
            s = "";
            editable = FALSE;
        }
        else if (is_trow1)
        {
            if (anchor)
            {
                gnc_numeric val = xaccSplitGetValue (get_this_split (view, trans));
                editable = !expanded && (num_of_splits < 3);
                if (expanded)
                    s = "";
                else
                    s = xaccPrintAmount (val, gnc_commodity_print_info (xaccTransGetCurrency (trans), SHOW_SYMBOL));
            }
            else
            {
                s = "";
                editable = FALSE;
            }
        }

        if (is_split)
        {
            if (anchor == NULL)
            {
                gnc_numeric amt = xaccSplitGetAmount (split);
                s = xaccPrintAmount (amt, gnc_account_print_info (xaccSplitGetAccount (split), SHOW_SYMBOL));
                editable = TRUE;
            }
            else if (anchor)
            {
                gnc_commodity *split_comm;
                split_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));

                if (!gnc_commodity_is_currency (split_comm) || (is_blank))
                {
                    gnc_numeric amt = xaccSplitGetAmount (split);
                    s = xaccPrintAmount (amt, gnc_account_print_info (xaccSplitGetAccount (split), SHOW_SYMBOL));
                    editable = TRUE;
                }
            }
            else
            {
                s = "";
                editable = FALSE;
            }

            if (get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }

        /* Only allow changes to values if we have a valid split accounts */
        editable = have_account (view, depth, expanded, trans, split);

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_PRICE:
        /* Column is PRICE */
        if (is_trow2)
        {
            s = "";
            editable = FALSE;
        }
        else if (is_trow1)
        {
            if (anchor)
            {
                num = xaccSplitGetSharePrice (get_this_split (view, trans));
                editable = !expanded && (num_of_splits < 3);
                if (expanded)
                    s = "";
                else 
                {
                    if (gnc_numeric_check (num) == GNC_ERROR_OK)
                    {
                        s = xaccPrintAmount (num, gnc_split_amount_print_info (split, SHOW_SYMBOL));
                    }
                    else
                    {
                        s = "";
                        editable = FALSE;
                    }
                }
            }
            else
            {
                s = "";
                editable = FALSE;
            }
        }

        if (is_split)
        {
            gnc_commodity *split_comm;
            split_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));

            if (!gnc_commodity_is_currency (split_comm) || (is_blank))
            {
                num = xaccSplitGetSharePrice (split);

                if (gnc_numeric_check (num) == GNC_ERROR_OK)
                {
                    s = xaccPrintAmount (num, gnc_split_amount_print_info (split, SHOW_SYMBOL));
                    editable = TRUE;
                }
                else
                {
                    s = "";
                    editable = FALSE;
                }
            }
            else
            {
                s = "";
                editable = FALSE;
            }

            if (get_imbalance (trans))
                g_object_set(cell, "cell-background", PINKCELL, (gchar*)NULL);
        }

        /* Only allow changes to values if we have a valid split accounts */
        editable = have_account (view, depth, expanded, trans, split);

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
            if ((is_trow1 || is_trow2) && expanded)
                s = "";
            else
                s = xaccPrintAmount (gnc_numeric_abs (num),
                                gnc_account_print_info (anchor, SHOW_SYMBOL));
        }

        /* Only allow changes to values if we have a valid split accounts */
        editable = have_account (view, depth, expanded, trans, split);

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
        if (read_only && !open_edited)
            g_object_set(cell, "cell-background", REDCELL, (gchar*)NULL);
        else if (read_only && open_edited)
            g_object_set(cell, "cell-background", YELLOWCELL, (gchar*)NULL);
        else if (xaccTransInFutureByPostedDate (trans))
            g_object_set(cell, "cell-background", BLUECELL, (gchar*)NULL);
        else
            g_object_set(cell, "cell-background", BLACKCELL, (gchar*)NULL);
        break;

    case COL_COMM:
        /* Column COMMODITY */
        if (is_split)
        {
            gnc_commodity *split_com, *txn_com;

            split_com = xaccAccountGetCommodity (xaccSplitGetAccount(split));
            txn_com = xaccTransGetCurrency (trans);
            if ( split_com == txn_com)
               s = g_strconcat (gnc_commodity_get_printname (split_com), "*", NULL);
            else
               s = gnc_commodity_get_printname (split_com);
        }
        else
            s = "";

        g_object_set (cell, "text", s, "editable", FALSE, NULL);
        break;

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
    GncTreeModelSplitReg *model;
    GtkTreePath         *path;
//g_print("\n\nstart_edit\n");
/*FIXME Not sure if this is required, leave for now ? */

    model = get_split_reg_model_from_view (view);

    gtv_get_editable_start_editing_cb (cr, editable, path_string, user_data);
/*    g_signal_connect(G_OBJECT(editable), "editing-done", (GCallback) editing_done_cb, view); */

//FIXME this could be the sort path instead of model path / check !!
    path = gtk_tree_path_new_from_string (path_string);

    if(view->priv->edit_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->edit_ref);
        view->priv->edit_ref = NULL;
    }
    view->priv->edit_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), path);

    gtk_tree_path_free (path);

    return;
}



//FIXME I am not sure if we need the split here at all ???????
/* Open Transaction for editing and set the default currency */
static void
gtv_begin_edit (GncTreeViewSplitReg *view, Split *split, Transaction *trans)
{
    ENTER("gtv_begin_edit split %p and trans %p", split, trans);

    if (split && trans != xaccSplitGetParent (split))
    {
        LEAVE("gtv_begin_edit - blank split, return");
        return;
    }

    if (trans != view->priv->dirty_trans)
    {
        Timespec ts = {0,0};
        xaccTransGetDatePostedTS (trans, &ts);

        xaccTransBeginEdit (trans);
        view->priv->dirty_trans = trans;

        if (!xaccTransGetCurrency (trans))
        {
            if (gnc_commodity_is_currency (view->priv->reg_comm))
            {
                xaccTransSetCurrency (trans, view->priv->reg_comm);
            }
            else
            {
                xaccTransSetCurrency (trans, gnc_default_currency());
            }
        }

        if (ts.tv_sec == 0)
        {
            //If the time returned by xaccTransGetDatePostedTS is 0 then assume it
            //is a new transaction and set the time to current time to show current
            //date on new transactions

            ts.tv_sec = gnc_time (NULL);
            xaccTransSetDatePostedSecs (trans, ts.tv_sec);
        }
    }
    LEAVE(" ");
}


/* Call back to remove date widget */
static void
remove_edit_date (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncPopupEntry *popup_entry;
    const gchar *new_string; 
    const gchar *current_string;
    GDate date;
    char string[1024];
    time64 tt;

    ENTER("remove edit date and temp cell rend %p", view->priv->temp_cr);

    if (view->priv->temp_cr != NULL)
    {
        //These strings are used to determine if cell data was altered so that keynav works better
        popup_entry = GNC_POPUP_ENTRY (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable"));

        new_string = g_strdup (gtk_entry_get_text (GTK_ENTRY (popup_entry->entry)));

        current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");

        DEBUG("New string is %s and Current_string is %s", new_string, current_string);

        //If editing wasn't canceled and strings don't match then cell data was edited
        if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "edit-canceled"))
             && g_ascii_strcasecmp (new_string, current_string))
        {
            g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
        }

        /* Lets update the help text */
        g_date_set_parse (&date, new_string);
        if (g_date_valid (&date))
        {
            struct tm tm;
            memset (&tm, 0, sizeof (tm));
            g_date_to_struct_tm (&date, &tm);
            qof_strftime (string, sizeof (string), "%A %d %B %Y", &tm);
        }
        view->help_text = g_strdup (string);
        g_signal_emit_by_name (view, "help_signal", NULL);

        g_object_set_data (G_OBJECT (view->priv->temp_cr), "cell-editable", NULL);
        view->priv->temp_cr = NULL;
        view->editing_now = FALSE;
    }
    LEAVE(" ");
}


/* Call back to remove combo widget */
static void
remove_edit_combo (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkEntry *entry; 
    const gchar *new_string; 
    const gchar *current_string;

    ENTER("remove edit combo and temp cell rend %p", view->priv->temp_cr);

    if (view->priv->temp_cr != NULL)
    {
        //These strings are used to determine if cell data was altered so that keynav works better
        entry = GTK_ENTRY (gtk_bin_get_child (GTK_BIN (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable"))));

        new_string = gtk_entry_get_text (GTK_ENTRY (entry));

        current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");

        DEBUG("New string is %s and Current_string is %s", new_string, current_string);

        //If editing wasn't canceled and strings don't match then cell data was edited
        if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "edit-canceled"))
             && g_ascii_strcasecmp (new_string, current_string))
        {
            g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
        }

        g_object_set_data (G_OBJECT (view->priv->temp_cr), "cell-editable", NULL);
        view->priv->temp_cr = NULL;
        view->editing_now = FALSE;
    }
    LEAVE(" ");
}


/* Call back to remove entry widget */
static void
remove_edit_entry (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    const gchar *new_string; 
    const gchar *current_string; 

    ENTER("remove edit entry and temp cell rend %p", view->priv->temp_cr);

    if (view->priv->temp_cr != NULL)
    {
        //These strings are used to determine if cell data was altered so that keynav works better
        new_string = gtk_entry_get_text (GTK_ENTRY (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable")));

        current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");

        DEBUG("New string is %s and Current_string is %s", new_string, current_string);

        //If editing wasn't canceled and strings don't match then cell data was edited
        if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "edit-canceled"))
             && g_ascii_strcasecmp (new_string, current_string))
        {
            g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
        }

        g_object_set_data (G_OBJECT (view->priv->temp_cr), "cell-editable", NULL);
        view->priv->temp_cr = NULL;
        view->editing_now = FALSE;
    }
    LEAVE(" ");
}


/* Explain: GtkEntry has a cursor that blinks upon
   g_timeout_dispatch().  It complains if it blinks after the GtkEntry
   loses focus.  So, we can't pop up any dialogs while the blinking
   cursor is around.  The solution is to force the editing to be
   finished before raising the dialog.  That finalizes the
   gtkcelleditable. */
static void
gtv_finish_edit (GncTreeViewSplitReg *view)
{
    GtkCellEditable *ce;

    if (view->priv->temp_cr == NULL)
        return;

    DEBUG("gtv_finish_edit temp_cr is %p", view->priv->temp_cr);

    if ((ce = GTK_CELL_EDITABLE (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable"))))
    {
        DEBUG("gtv_finish_edit - editing_done");
        gtk_cell_editable_editing_done (ce);
        gtk_cell_editable_remove_widget (ce);
    }
}


/* This is used in g_idle_add to finish an edit */
static gboolean
gtv_idle_finish_edit (GncTreeViewSplitReg *view)
{
   gtv_finish_edit (view);
   return FALSE;
}


/* Returns TRUE if dialog was canceled or discarded.
   Does nothing if 'new_trans' is the dirty trans. */
static gboolean
transaction_changed_confirm (GncTreeViewSplitReg *view,
                            Transaction *new_trans)
{
    GtkWidget *dialog, *window;
    gint response;
    const char *title = _("Save the changed transaction?");
    const char *message = _(
        "The current transaction has changed.  Would you like to "
        "record the changes, or discard the changes?");

    if (!view->priv->dirty_trans || view->priv->dirty_trans == new_trans)
        return FALSE;

    window = gnc_tree_view_split_reg_get_parent (view);
    dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_QUESTION,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
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
        if(view->priv->edit_ref != NULL)
        {
            gtk_tree_row_reference_free (view->priv->edit_ref);
            view->priv->edit_ref = NULL;
        }
        view->priv->dirty_trans = NULL;
        view->change_allowed = FALSE;
        return FALSE;
        break;

    case GTK_RESPONSE_REJECT:
        if (view->priv->dirty_trans && xaccTransIsOpen (view->priv->dirty_trans))
        {
            GncTreeModelSplitReg *model;
            Split                *split;

            model = get_split_reg_model_from_view (view);

            // Remove the split before rollback.
            gnc_tree_model_split_reg_set_blank_split_parent (model, view->priv->dirty_trans, TRUE);

            g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
            xaccTransRollbackEdit (view->priv->dirty_trans);

            // Add the split after rollback so it is last in list.
            gnc_tree_model_split_reg_set_blank_split_parent (model, view->priv->dirty_trans, FALSE);
            view->priv->dirty_trans = NULL;

            split = gnc_tree_model_split_get_blank_split (model);
            xaccSplitReinit (split); // Clear the blank split
            view->change_allowed = FALSE;
        }
        return TRUE;
        break;

    case GTK_RESPONSE_CANCEL:
        return TRUE;
        break;

    default:
        return FALSE;
    }

    return FALSE;
}


/*####################################################################
          ^^^^^    edit function call backs      ^^^^^
          vvvvvv   gtv function call backs       vvvvv
#####################################################################*/
/* Set the column titles based on register type and depth */
static void
gtv_split_reg_titles (GncTreeViewSplitReg *view, RowDepth depth)
{
    GncTreeModelSplitReg *model;
    GtkCellRenderer *cr;
    GList *renderers;
    GList *columns;
    GList  *column;
    gint i;

    ENTER("title depth is %d and sort_depth %d, sort_col is %d", depth, view->sort_depth, view->sort_col);

    model = get_split_reg_model_from_view (view);
    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (view));

    for ( column = columns, i = 1; column; column = g_list_next (column), i++)
    {
        GtkTreeViewColumn *tvc;
        ViewCol viewcol;

        tvc = column->data;

        /*FIXME ## Do we need to look for multiple renderers ?
          We only have one renderer per cell now */
        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (tvc));
        g_assert (g_list_length (renderers) == 1);
        cr = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT(cr), "view_column"));

        DEBUG("viewcol is %d", viewcol);

        switch(viewcol)
        {
        case COL_DATE:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                /* Display arrows if we are sorting on this row */
                if (view->sort_depth == depth && view->sort_col == viewcol)
                    gtk_tree_view_column_set_sort_indicator (tvc, TRUE);
                else
                    gtk_tree_view_column_set_sort_indicator (tvc, FALSE);

                if(depth == TRANS1 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Date Posted"));
                else if(depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Date Entered"));
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
                if (depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Reference"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Action"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Action"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Reference / Action"));
                break;


            default:
                /* Display arrows if we are sorting on this row */
                if (view->sort_depth == depth && view->sort_col == viewcol)
                    gtk_tree_view_column_set_sort_indicator (tvc, TRUE);
                else
                    gtk_tree_view_column_set_sort_indicator (tvc, FALSE);

                if (depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Number"));
                else if (depth == TRANS2 && (qof_book_use_split_action_for_num_field (gnc_get_current_book())))
                    gtk_tree_view_column_set_title (tvc, _("T-Number"));
                else if (depth == TRANS2 && (!qof_book_use_split_action_for_num_field (gnc_get_current_book())))
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
                    gtk_tree_view_column_set_title (tvc, _("Customer / Memo"));
                break;

            case PAYABLE_REGISTER2:
                if(depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Vendor"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Vendor / Memo"));
                break;


            default:
                /* Display arrows if we are sorting on this row */
                if (view->sort_depth == depth && view->sort_col == viewcol)
                    gtk_tree_view_column_set_sort_indicator (tvc, TRUE);
                else
                    gtk_tree_view_column_set_sort_indicator (tvc, FALSE);

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

        case COL_COMM:
            switch(model->type)
            {
            default: //FIXME These if statements may not be required
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Commodity"));
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
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Decrease"));
                    break;

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
                case CURRENCY_REGISTER2:
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
                        gtk_tree_view_column_set_title (tvc, _("Receive"));
                    break;

                case ASSET_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Increase"));
                    break;

                case LIABILITY_REGISTER2:
                case EQUITY_REGISTER2:
                case TRADING_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Decrease"));
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
                case CURRENCY_REGISTER2:
                case PORTFOLIO_LEDGER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Buy"));
                    break;

                case RECEIVABLE_REGISTER2:
                if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Invoice"));
                    break;

                case CREDIT_REGISTER2:
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
    LEAVE(" ");
    g_list_free (columns);
}


/* Update the help text */
static void
gtv_split_reg_help (GncTreeViewSplitReg *view, GtkCellRenderer *cr, ViewCol viewcol, RowDepth depth)
{
    GncTreeModelSplitReg *model;
    const char *help = " ";
    const gchar *current_string;

    ENTER("Help Viewcol is %d and depth is %d", viewcol, depth);

    model = get_split_reg_model_from_view (view);

    switch(viewcol)
    {
    case COL_DATE:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1)
            {
                GDate date;
                char string[1024];

                current_string = g_object_get_data (G_OBJECT (cr), "current-string");
                g_date_set_parse (&date, current_string);
                if (g_date_valid (&date))
                {
                    struct tm tm;
                    memset (&tm, 0, sizeof (tm));
                    g_date_to_struct_tm (&date, &tm);
                    qof_strftime (string, sizeof (string), "%A %d %B %Y", &tm);
                }
                help = g_strdup (string);
            }
            else
                help = " ";
            break;
        }
        break;

    case COL_DUEDATE:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter Due Date");
            break;
        }
        break;

    case COL_NUMACT:
        switch(model->type)
        {
        case RECEIVABLE_REGISTER2:
        case PAYABLE_REGISTER2:
            if(depth == TRANS1)
                help = _("Enter the transaction reference, such as the invoice or check number");
            else if (depth == TRANS2 || depth == SPLIT3)
                help = _("Enter the type of transaction, or choose one from the list");
            break;

        default:
            if(depth == TRANS1)
                help = _("Enter the transaction number, such as the check number");
            else if (depth == TRANS2 || depth == SPLIT3)
                help = _("Enter the type of transaction, or choose one from the list");
            break;
        }
        break;

    case COL_DESCNOTES:
        switch(model->type)
        {
        case RECEIVABLE_REGISTER2:
            if(depth == TRANS1)
                help = _("Enter the name of the Customer");
            else if (depth == TRANS2)
                help = _("Enter notes for the transaction");
            else if (depth == SPLIT3)
                help = _("Enter a description of the split");
            break;

        case PAYABLE_REGISTER2:
            if(depth == TRANS1)
                help = _("Enter the name of the Vendor");
            else if (depth == TRANS2)
                help = _("Enter notes for the transaction");
            else if (depth == SPLIT3)
                help = _("Enter a description of the split");
            break;

        default:
            if(depth == TRANS1)
                help = _("Enter a description of the transaction");
            else if (depth == TRANS2)
                help = _("Enter notes for the transaction");
            else if (depth == SPLIT3)
                help = _("Enter a description of the split");
            break;
        }
        break;

    case COL_TRANSVOID:
        switch(model->type)
        {
        default:
            if(depth == TRANS1)
                help = _("Enter the account to transfer from, or choose one from the list");
            else if (depth == TRANS2)
                help = _("Reason the transaction was voided");
            else if (depth == SPLIT3)
                help = " ";
            break;
        }
        break;

    case COL_RECN:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter the reconcile type");
            break;
        }
        break;

    case COL_TYPE:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter the type of transaction");
            break;
        }
        break;

    case COL_VALUE:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter the value of shares bought or sold");
            break;
        }
        break;

    case COL_AMOUNT:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter the number of shares bought or sold");
            break;
        }
        break;

    case COL_AMTVAL:
        switch(model->type)
        {
        default:
            if((depth == TRANS1) || (depth == TRANS2))
                help = _("Enter the value of shares bought or sold");
            else if (depth == SPLIT3)
                help = _("Enter the number of shares bought or sold");
            break;
        }
        break;

    case COL_COMM:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("* Indicates the transaction Commodity.");
            break;
        }
        break;

    case COL_RATE:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter the rate");
            break;
        }
        break;

    case COL_PRICE:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter the effective share price");
            break;
        }
        break;

    case COL_CREDIT:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter credit formula for real transaction");
            break;
        }
        break;

    case COL_DEBIT:
        switch(model->type)
        {
        default: //FIXME These if statements may not be required
            if(depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = _("Enter debit formula for real transaction");
            break;
        }
        break;

    default:
            help = " ";
        break;
    }

    LEAVE("Help text is - %s", help);
    view->help_text = g_strdup (help);
    g_signal_emit_by_name (view, "help_signal", NULL);
}


/* Move the selection to the blank split when expanded */
static gboolean
gtv_selection_to_blank (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *bpath, *spath;
    Split *bsplit;

    /* give gtk+ a chance to handle pending events */
    while (gtk_events_pending ())
        gtk_main_iteration ();

    /* Make sure we have expanded splits */
    if (view->priv->expanded == FALSE)
        return FALSE;

    model = get_split_reg_model_from_view (view);

    bsplit = gnc_tree_model_split_get_blank_split (model);
    bpath =  gnc_tree_model_split_reg_get_path_to_split_and_trans (model, bsplit, NULL);

    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), bpath);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), spath);

    gtk_tree_path_free (bpath);
    gtk_tree_path_free (spath);

    return FALSE;
}


/* Callback for double click */
void
gtv_split_reg_double_click_cb (GtkTreeView *treeview, GtkTreePath *path,
                               GtkTreeViewColumn *column, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (treeview);
    GncTreeModelSplitReg *model;

    model = get_split_reg_model_from_view (view);

    /* this works on non editable cells like void, balance */

    if (model->style != REG2_STYLE_JOURNAL)
    {
        if (view->priv->expanded)
            gnc_tree_view_split_reg_collapse_trans (view, NULL);
        else
            gnc_tree_view_split_reg_expand_trans (view, NULL);

        /* This updates the plugin page gui */
        if (view->moved_cb)
            (view->moved_cb)(view, view->moved_cb_data);
    }
}


/* Call back for when a Transaction is deleted so we can move selection out of way */
static void
gtv_split_reg_trans_delete_cb (GncTreeModelSplitReg *model, gpointer item, gpointer user_data)
{
    GncTreeViewSplitReg *view = user_data;
    Transaction *trans = item;

    DEBUG("gtv_split_reg_trans_delete_cb view %p model %p trans %p ", view, model, trans);

    DEBUG("gtv_split_reg_trans_delete_cb current_trans %p trans %p", view->priv->current_trans, trans);

    /* if same, lets get out of the way so move */
    if (trans == view->priv->current_trans)
        gnc_tree_control_split_reg_goto_rel_trans_row (view, 1);
}


/* Call back for focus out evnt so we can finish edit */
static gboolean
gtv_split_reg_focus_out_cb (GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

    gnc_tree_view_split_reg_finish_edit (view);

    return FALSE;
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

//FIXME ????
//g_print("\ngtvt_key_press_cb\n");

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

    gnc_tree_view_split_reg_finish_edit (view);



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

//g_print("key nav - Path is '%s' and viewcol is %d and editable is %d and cell pointer is %p\n", gtk_tree_path_to_string (path),
//                   viewcol, editable, cell);



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

//g_print( "end of key nav\n");

    gtk_tree_path_free (path);

    return TRUE;
}


/* Callback for selection move */
static void
gtv_split_reg_motion_cb (GtkTreeSelection *sel, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    Split *split = NULL;
    Transaction *trans = NULL;
    Transaction *old_trans;
    gboolean is_trow1, is_trow2, is_split, is_blank;
    RowDepth depth = 0;
    GtkTreeIter iter;
    GtkTreePath *mpath;
    GtkTreePath *spath;

//g_print ("\n** gtv_split_reg_motion_cb start\n");

    model = get_split_reg_model_from_view (view);

//g_print("Motion - ** model is %p and view is %p dirty_trans is %p **\n", model, view, view->priv->dirty_trans);
//g_print("Motion - ** o_trans is %p o_split is %p o_depth %d **\n", view->priv->current_trans, view->priv->current_split, view->priv->current_depth);

    /* Reset help text */
    view->help_text = " ";
    g_signal_emit_by_name (view, "help_signal", NULL);

    if (get_model_iter_from_selection (view, sel, &iter))
    {
        mpath = gtk_tree_model_get_path (GTK_TREE_MODEL (model), &iter);

        spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), mpath);

//g_print ("Motion - mpath is %s spath is %s\n\n", gtk_tree_path_to_string (mpath), gtk_tree_path_to_string (spath));

//g_print("Motion - current_ref is valid %d edit_ref is %d\n", gtk_tree_row_reference_valid (view->priv->current_ref),
//                                                             gtk_tree_row_reference_valid (view->priv->edit_ref));

//if(gtk_tree_row_reference_valid (view->priv->current_ref))
    //g_print("Motion - Old Current Path is '%s'\n", gtk_tree_path_to_string (gtk_tree_row_reference_get_path (view->priv->current_ref)));
//if(gtk_tree_row_reference_valid (view->priv->edit_ref))
    //g_print("Motion - Old Edit path is '%s'\n", gtk_tree_path_to_string (gtk_tree_row_reference_get_path (view->priv->edit_ref)));

        /* save the current path */
        if(view->priv->current_ref != NULL)
        {
            gtk_tree_row_reference_free (view->priv->current_ref);
            view->priv->current_ref = NULL;
        }
        view->priv->current_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), mpath);

        /* Use depth to determine if it is a split or transaction */
        depth = gtk_tree_path_get_depth (mpath);

        /* Update the tree view titles */
        gtv_split_reg_titles (view, depth);

        gtk_tree_path_free (mpath);

//if(gtk_tree_row_reference_valid (view->priv->current_ref))
    //g_print("Motion - Current Path is '%s'\n", gtk_tree_path_to_string (gtk_tree_row_reference_get_path (view->priv->current_ref)));
//if(gtk_tree_row_reference_valid (view->priv->edit_ref))
    //g_print("Motion - Edit path is '%s'\n", gtk_tree_path_to_string (gtk_tree_row_reference_get_path (view->priv->edit_ref)));

        gnc_tree_model_split_reg_get_split_and_trans (
                GNC_TREE_MODEL_SPLIT_REG (model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

//g_print("Motion - get model split %p, trans %p, is_split %d, is_blank %d\n", split, trans, is_split, is_blank);


        //Ask for confirmation if data has been edited, transaction_changed_confirm return TRUE if canceled
        if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) && transaction_changed_confirm (view, trans))
        {
//g_print("Restore position - Cancel / Discard\n");
            /* Restore position - Cancel / Discard */
            if(view->priv->current_ref != NULL)
            {
                gtk_tree_row_reference_free (view->priv->current_ref);
                view->priv->current_ref = NULL;
            }
            view->priv->current_ref = gtk_tree_row_reference_copy (view->priv->edit_ref);

            gnc_tree_view_split_reg_default_selection (view);
            return;
        }
        else
        {
//g_print("Commit and skip\n");
            /* Commit and skip */

            /* Move the blank split */ 
            gnc_tree_model_split_reg_set_blank_split_parent (model, trans, FALSE);
        }

        old_trans = view->priv->current_trans;
        view->priv->current_trans = trans;
        view->priv->current_split = split;
        view->priv->current_depth = depth;

//g_print("Motion - ** view->priv-> c_trans is %p c_split is %p depth %d **\n\n", view->priv->current_trans, view->priv->current_split, view->priv->current_depth);

        /* Auto expand transaction and collapse previous transaction */
        if (old_trans != trans)
        {
            /* Reset allow changes for reconciled transctions */
            view->change_allowed = FALSE;

            if (model->style != REG2_STYLE_JOURNAL)
            {
                if (gnc_tree_view_split_reg_trans_expanded (view, old_trans))
                    gnc_tree_view_split_reg_collapse_trans (view, old_trans);
            }
            else
                gnc_tree_view_split_reg_expand_trans (view, NULL);

            if (model->style == REG2_STYLE_AUTO_LEDGER)
            {
                gtk_tree_view_expand_row (GTK_TREE_VIEW (view), spath, TRUE);

                view->priv->expanded = TRUE;

                if (SELECTION_TO_BLANK_ON_EXPAND)
                    gtv_selection_to_blank (view);
            }
        }
        gtk_tree_path_free (spath);
    }
    else
    {
//g_print("Not valid selection\n");
        /* We do not have a valid iter */
        gtv_split_reg_titles (view, 0);

        /* Move the blank split to the last transaction */ 
        gnc_tree_model_split_reg_set_blank_split_parent (model, NULL, FALSE);

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
    GtkCellEditable      *editable;
    GtkTreeIter           iter;
    Split                *split;
    Transaction          *trans;
    gboolean              is_trow1, is_trow2, is_split, is_blank;
    ViewCol               viewcol;
    char                 *error_loc = NULL;
    Account              *anchor = view->priv->anchor;

    editable = g_object_get_data (G_OBJECT (cell), "cell-editable");

    DEBUG("cell is %p editable pointer is %p and id %lu", cell, editable, view->priv->fo_handler_id);

    /* Remove the focus out cb if one exists */
    if (view->priv->fo_handler_id != 0)
    {
        if (g_signal_handler_is_connected (G_OBJECT (editable), view->priv->fo_handler_id))
            g_signal_handler_disconnect (G_OBJECT (editable), view->priv->fo_handler_id);
    }
    view->priv->fo_handler_id = 0;

    if (g_strcmp0 (g_object_get_data (G_OBJECT (cell), "current-string"), new_text) == 0) // No change, return
        return;

    g_return_if_fail (get_model_iter_from_view_string (view, path_string, &iter));

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    model = get_split_reg_model_from_view (view);
    g_return_if_fail (model);

    gnc_tree_model_split_reg_get_split_and_trans (
        model, &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    switch (viewcol) {
    case COL_DATE:
        /* Column is DATE */
        if (is_trow1)
        {
            GDate date;
            g_date_set_parse (&date, new_text);
            if (g_date_valid (&date))
            {
                gtv_begin_edit (view, NULL, trans);
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
        gtv_begin_edit (view, NULL, trans);
        if (is_trow1)
        {
            /* set per book option */
            gnc_set_num_action (trans, get_this_split (view, trans),
                                                                new_text, NULL);
        }
        if (is_trow2)
        {
            /* set per book option */
            gnc_set_num_action (trans, get_this_split (view, trans),
                                                                NULL, new_text);
        }
        if (is_split)
        {
            /* Set split-action with gnc_set_num_action which is the same as
             * xaccSplitSetAction with these arguments */
            gnc_set_num_action (NULL, split, NULL, new_text);
        }
        break;

    case COL_DESCNOTES:
        /* Column is DESCRIPTION / NOTES / MEMO */
        gtv_begin_edit (view, NULL, trans);
        if (is_trow1)
            xaccTransSetDescription (trans, new_text);

        if (is_trow2)
            xaccTransSetNotes (trans, new_text);

        if (is_split)
            xaccSplitSetMemo (split, new_text);

        break;

    case COL_RECN:
        /* Column is RECONCILE */
        gtv_begin_edit (view, NULL, trans);

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

        break;

    case COL_TRANSVOID:
    case COL_AMTVAL:
    case COL_PRICE:
    case COL_DEBIT:
    case COL_CREDIT:
        {

            Account       *acct, *old_acct;
            gnc_numeric    input;
            Split         *osplit = NULL;
            QofBook       *book; //do we have this
            Account       *root; // do we have this
            gboolean       valid_input = FALSE;
            gboolean       force = FALSE;
            gboolean       input_used = FALSE;

            book = gnc_get_current_book();
            root = gnc_book_get_root_account (book);

            gtv_begin_edit (view, NULL, trans);

            /* Get the split pair if anchored to a register */
            if (!is_split && anchor)
            {
                if (!get_split_pair (view, trans, &osplit, &split))
                {
                    PERR("couldn't get split pair");
                    break;
                }
            }

            /* Setup the account field */
            if (viewcol == COL_TRANSVOID)
            {
                if (view->priv->acct_short_names)
                    acct = gnc_account_lookup_by_name (root, new_text);
                else
                    acct = gnc_account_lookup_by_full_name (root, new_text);

                if (acct != NULL && is_split)
                {
                    old_acct = xaccSplitGetAccount (split);
                    xaccSplitSetAccount (split, acct);
                    if (!gnc_commodity_equiv (xaccAccountGetCommodity (old_acct), xaccAccountGetCommodity (acct)))
                        force = TRUE;
                }
                else
                {
                    old_acct = xaccSplitGetAccount (osplit);
                    xaccSplitSetAccount (osplit, acct);
                    if (!gnc_commodity_equiv (xaccAccountGetCommodity (old_acct), xaccAccountGetCommodity (acct)))
                        force = TRUE;
                }
            }
            else
            {
                if (!gnc_exp_parser_parse (new_text, &input, &error_loc))
                    break;
                else
                    valid_input = TRUE;
            }

            /* Get the account for this split */
            acct = xaccSplitGetAccount (split);
            if (!acct)
            {
                if (anchor)
                {
                    xaccSplitSetAccount (split, anchor);
                    acct = xaccSplitGetAccount (split);
                }
                else
                {
                    break; //Well, what else is there to do?
                }
            }

            /* Set the transaction currency if not set or if this is a non currency register,
                 this should be same as first currency split */
            if (!xaccTransGetCurrency (trans) || !gnc_commodity_is_currency (view->priv->reg_comm))
            {
                gnc_commodity *split_commodity;
                split_commodity = xaccAccountGetCommodity (xaccSplitGetAccount (split));

                if (gnc_commodity_is_currency (split_commodity))
                    xaccTransSetCurrency (trans, xaccAccountGetCommodity (xaccSplitGetAccount (split)));
            }

            /* This computes the value if we just commit the split after entering account */
            if (!valid_input)
                input = get_value_for (view, trans, split, is_blank);

            // Negate the input if COL_CREDIT
            if (viewcol == COL_CREDIT)
                input = gnc_numeric_neg (input);

            // Set the split parent trans
            xaccSplitSetParent (split, trans);

            // If we are at trasaction level, column is value, split level is amount
            if (viewcol == COL_AMTVAL)
            {
                set_number_for_input (view, trans, split, input, COL_AMTVAL);
                input_used = TRUE;
            }

            // The price of stock / shares
            if (viewcol == COL_PRICE)
            {
                set_number_for_input (view, trans, split, input, COL_PRICE);
                input_used = TRUE;
            }

            // Check if this is a stock / share amount
            if (viewcol == COL_CREDIT || viewcol == COL_DEBIT)
            {
                if (!gnc_commodity_is_currency (xaccAccountGetCommodity (acct)))
                {
                    set_number_for_input (view, trans, split, input, viewcol);
                    input_used = TRUE;
                }
            }

            // This is used in transaction mode, two splits
            if (input_used == FALSE)
            {
                if (gnc_commodity_is_currency (xaccAccountGetCommodity (acct)))
                    set_value_for (view, trans, split, input, force);
                else
                    set_value_for_amount (view, trans, split, input);
            }

            // If this is the blank split, promote it.
            if (is_blank)
            {
                /*FIXME May be this should be a signal - Promote the blank split to a real split */
                g_idle_add ((GSourceFunc)gnc_tree_model_split_reg_commit_blank_split, get_split_reg_model_from_view (view));
            }

            // In transaction mode, two splits only, set up the other split.
            if (osplit)
            {
                xaccSplitSetParent (osplit, trans);

                if (gnc_commodity_is_currency (xaccAccountGetCommodity (acct)))
                    set_value_for (view, trans, osplit, gnc_numeric_neg (input), force);
                else
                    set_value_for_amount (view, trans, osplit, gnc_numeric_neg (xaccSplitGetValue (split)));
            }
        }
        break;

    default:
        //g_assert_not_reached();
        break;
    }
}


/* Callback for changing reconcile setting with space bar */
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
gtv_get_editable_start_editing_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
                              const gchar *path_string, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeModel     *smodel;
    GtkTreePath *path, *mpath;
    ViewCol viewcol;

    GtkListStore *description_list;
    GtkListStore *memo_list;
    GtkListStore *notes_list;
    GtkListStore *acct_list;

    GtkEntryCompletion *completion = gtk_entry_completion_new();
    RowDepth depth;

    ENTER("ngtv_get_editable_start_editing_cb Path string is '%s'\n", path_string);

    model = get_split_reg_model_from_view (view);

    smodel = gtk_tree_view_get_model (GTK_TREE_VIEW (view));

    /* Description / Notes / Memo / Accounts Completion Lists */
    description_list = gnc_tree_model_split_reg_get_description_list (model);
    notes_list = gnc_tree_model_split_reg_get_notes_list (model);
    memo_list = gnc_tree_model_split_reg_get_memo_list (model);
    acct_list = gnc_tree_model_split_reg_get_acct_list (model);

    // Use depth to determine if it is a split or transaction
    path = gtk_tree_path_new_from_string (path_string);
    depth = gtk_tree_path_get_depth (path);

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT(cr), "view_column"));

    DEBUG("editable Depth is %u and ViewCol is %d", depth, viewcol);

    /* DATE COLUMN */
    if (viewcol == COL_DATE)
    {
        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (GTK_ENTRY (GNC_POPUP_ENTRY (editable)->entry))));

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_date, view);

        DEBUG("Current String date is '%s'", g_strdup (gtk_entry_get_text (GTK_ENTRY (GNC_POPUP_ENTRY (editable)->entry))));

    }


    /* TRANSFER / VOID COLUMN */
    else if (viewcol == COL_TRANSVOID)
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
//??        g_signal_connect(G_OBJECT(completion), "match-selected", (GCallback) gtv_split_reg_match_selected_cb, view);
        g_object_unref (completion);

        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data(G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)));

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback) gtv_split_reg_changed_cb, view);
        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_combo, view);

        DEBUG("Current String tv is '%s'", g_strdup(gtk_entry_get_text (entry)));
    }


    /* NUMBER / ACTION COLUMN */
    else if (viewcol == COL_NUMACT)
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

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback) gtv_split_reg_changed_cb, view);
        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_combo, view);

        DEBUG("Current String na is '%s'", g_strdup (gtk_entry_get_text (entry)));
    }


    /* DESCRIPTION / NOTES / MEMO COLUMN */
    else if (viewcol == COL_DESCNOTES)
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
//??        g_signal_connect (G_OBJECT (completion), "match-selected", (GCallback) gtv_split_reg_match_selected_cb, view);

        g_object_unref (completion);

        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (GTK_ENTRY(editable))));

        view->priv->fo_handler_id = g_signal_connect (G_OBJECT (editable), "focus-out-event", (GCallback) gtv_split_reg_focus_out_cb, view);

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_entry, view);

        DEBUG("Current String dnm is '%s'", g_strdup (gtk_entry_get_text (GTK_ENTRY(editable))));
    }


    /* RECN COLUMN */
    else if (viewcol == COL_RECN)
    {
        GtkEntry *entry;

        entry = GTK_ENTRY (editable);

         g_object_set_data (G_OBJECT (cr), "cell-editable", editable);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)));

        g_signal_connect (G_OBJECT (GTK_ENTRY (editable)), "insert_text", (GCallback)gtv_split_reg_recn_cb, view);

        view->priv->fo_handler_id = g_signal_connect (G_OBJECT (editable), "focus-out-event", (GCallback) gtv_split_reg_focus_out_cb, view);

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_entry, view);

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback) gtv_split_reg_changed_cb, view);
        DEBUG("Current String recn is '%s'", g_strdup (gtk_entry_get_text (entry)));
    }


    /* THE REST OF THE COLUMNS */
    else
    {
        GtkEntry *entry;

        entry = GTK_ENTRY (editable);

        g_object_set_data (G_OBJECT (cr), "cell-editable", editable);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)));

        view->priv->fo_handler_id = g_signal_connect (G_OBJECT (editable), "focus-out-event", (GCallback) gtv_split_reg_focus_out_cb, view);

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) remove_edit_entry, view);

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback)gtv_split_reg_changed_cb, view);
        DEBUG("Current String rest is '%s'", g_strdup (gtk_entry_get_text (entry)));
    }


    gtv_split_reg_help (view, cr, viewcol, depth);

    mpath = gtk_tree_model_sort_convert_path_to_child_path (GTK_TREE_MODEL_SORT (smodel), path);

    if(view->priv->edit_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->edit_ref);
        view->priv->edit_ref = NULL;
    }
    view->priv->edit_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), mpath);

    DEBUG("edit_path is %s", gtk_tree_path_to_string (gtk_tree_row_reference_get_path (view->priv->edit_ref)));
    gtk_tree_path_free (path);
    gtk_tree_path_free (mpath);

    view->priv->temp_cr = cr;
    view->editing_now = TRUE;

    DEBUG("Temp Cell Rend %p", view->priv->temp_cr);

    //Add edit-canceled property to cr so we can distinguish between
    //cancelled and actual changes
    g_object_set_data (G_OBJECT (cr), "edit-canceled", GINT_TO_POINTER (FALSE));


    /*******************************************************************/
    /*            Test function before edit started                    */
    /*******************************************************************/

    /* Test for change of RECN COLUMN setting from reconciled */
    if (viewcol == COL_RECN)
    {
       /* Are we trying to change the reconcile setting */
        if (gnc_tree_control_split_reg_recn_change (view))
        {
            /* Make sure we have stopped editing */
            g_idle_add( (GSourceFunc)gtv_idle_finish_edit, view ); //FIXME Not sure if this is ok.
        }
        else
        {
            /* Make sure we have stopped editing */
            g_idle_add( (GSourceFunc)gtv_idle_finish_edit, view ); //FIXME Not sure if this is ok.
        }
    }

    /* Ask, are we allowed to change reconciled values other than 'description / notes / memo'
       which we can change always */
    if (viewcol != COL_DESCNOTES && viewcol != COL_RECN)
    {
        if (gnc_tree_control_split_reg_recn_test (view))
        {
            ;
        }
        else
        {
            /* Make sure we have stopped editing */
            g_idle_add( (GSourceFunc)gtv_idle_finish_edit, view ); //FIXME Not sure if this is ok.
        }
    }

    LEAVE(" ");
}


// Handle the "match-selected" signal
static void
gtv_split_reg_match_selected_cb (GtkEntryCompletion *widget, GtkTreeModel *model,
                        GtkTreeIter *iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

//FIXME g_print("gtv_split_reg_match_selected_cb\n\n");

/* Not sure what I am going to put in here yet */

}


// Handle the "changed" signal
static void
gtv_split_reg_changed_cb (GtkCellRendererCombo *widget, gchar *path_string,
                        GtkTreeIter *iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

//FIXME g_print("gtv_split_reg_changed_cb path string is '%s'\n\n", path_string);

/* Not sure what I am going to put in here yet */

}


// Handle the "editing-canceled" signal
static void
gtv_split_reg_editing_canceled_cb (GtkCellRenderer *cr, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

    if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) == FALSE) // Not edited, reset edit path
    {
        if(view->priv->edit_ref != NULL)
        {
            gtk_tree_row_reference_free (view->priv->edit_ref);
            view->priv->edit_ref = NULL;
        }
    }

    /* Reset Help text */
    view->help_text = " ";
    g_signal_emit_by_name (view, "help_signal", NULL);

    //Set edit-canceled property
    g_object_set_data (G_OBJECT (cr), "edit-canceled", GINT_TO_POINTER (TRUE));	
}


/* Scroll the view to show selected row based on sort direction */
static gboolean
gtv_scroll_to_cell (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;

    model = get_split_reg_model_from_view (view);

    mpath = gnc_tree_view_split_reg_get_current_path (view);
    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), mpath);


    if (view->sort_direction == 1)
        gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), spath, NULL, TRUE, 0.0, 0.0);
    else
    {
        if (model->use_double_line)
        {
            gtk_tree_path_down (spath); // move to the second row of transaction
            gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), spath, NULL, TRUE, 1.0, 0.0);
            gtk_tree_path_up (spath); // back to first row of transaction
        }
        else
            gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), spath, NULL, TRUE, 1.0, 0.0);
    }
    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);
    return (FALSE);
}


/*####################################################################
          ^^^^   gtv function call backs    ^^^^
#####################################################################*/

/* Returns the Transaction at the current selected position */
Transaction *
gnc_tree_view_split_reg_get_current_trans (GncTreeViewSplitReg *view)
{
    return view->priv->current_trans;
}


/* Returns the Split at the current selected position or NULL */
Split *
gnc_tree_view_split_reg_get_current_split (GncTreeViewSplitReg *view)
{
    return view->priv->current_split;
}


/* Returns the depth of the selected row */
RowDepth
gnc_tree_view_reg_get_selected_row_depth (GncTreeViewSplitReg *view)
{
    return view->priv->current_depth;
}


/* Returns the dirty_trans or NULL */
Transaction *
gnc_tree_view_split_reg_get_dirty_trans (GncTreeViewSplitReg *view)
{
    return view->priv->dirty_trans;
}


/* Sets dirty_trans to trans or NULL to clear */
void
gnc_tree_view_split_reg_set_dirty_trans (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;

    model = get_split_reg_model_from_view (view);

    if (trans == NULL)
    {
        if(view->priv->edit_ref != NULL)
        {
            gtk_tree_row_reference_free (view->priv->edit_ref);
            view->priv->edit_ref = NULL;
        }
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
        view->priv->dirty_trans = NULL;
    }
    else
    {
        if(view->priv->edit_ref != NULL)
        {
            gtk_tree_row_reference_free (view->priv->edit_ref);
            view->priv->edit_ref = NULL;
        }
        view->priv->edit_ref = gtk_tree_row_reference_copy (view->priv->current_ref);
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
        view->priv->dirty_trans = trans;
    }
}


/* Returns the current path */
GtkTreePath *
gnc_tree_view_split_reg_get_current_path (GncTreeViewSplitReg *view)
{
    return gtk_tree_row_reference_get_path (view->priv->current_ref);
}


/* Set the Current path to path */
void
gnc_tree_view_split_reg_set_current_path (GncTreeViewSplitReg *view, GtkTreePath *path)
{
    GncTreeModelSplitReg *model;

    model = get_split_reg_model_from_view (view);

    if(view->priv->current_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->current_ref);
        view->priv->current_ref = NULL;
    }
    view->priv->current_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), path);

    view->priv->current_trans = gnc_tree_view_split_reg_get_current_trans (view);
    view->priv->current_split = gnc_tree_view_split_reg_get_current_split (view);
    view->priv->current_depth = gnc_tree_view_reg_get_selected_row_depth (view);
}


/* Reinit transaction / delete the splits */
void
gnc_tree_view_split_reg_reinit_trans (GncTreeViewSplitReg *view)
{
    Transaction           *trans;
    RowDepth               depth;

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    trans = view->priv->current_trans;

    /* Lets get out of the way, move the selection to the transaction */
    gnc_tree_control_split_reg_goto_rel_trans_row (view, 0);

    depth = view->priv->current_depth;

    if (trans && (depth != SPLIT3))
    {
        Split *s;
        int i = 0;

        gtv_begin_edit (view, NULL, trans);
        gnc_tree_view_split_reg_set_dirty_trans (view, trans);

        while ((s = xaccTransGetSplit (trans, i)) != NULL)
        {
            if (xaccTransGetRateForCommodity (trans, view->priv->reg_comm, s, NULL))
                xaccSplitDestroy (s);
            else i++;
        }
    }
}


/* Delete the current split */
void
gnc_tree_view_split_reg_delete_current_split (GncTreeViewSplitReg *view)
{
    Transaction           *trans;
    Split                 *split;
    gboolean               was_open;

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    trans = view->priv->current_trans;
    split = view->priv->current_split;

    gtv_begin_edit (view, NULL, trans);

    gnc_tree_view_split_reg_set_dirty_trans (view, trans);

    /* Lets get out of the way, move the selection to the transaction */
    gnc_tree_control_split_reg_goto_rel_trans_row (view, 0);

    was_open = xaccTransIsOpen (trans);
    if (was_open)
        xaccSplitDestroy (split);
}


/* Delete the current transaction */
void
gnc_tree_view_split_reg_delete_current_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg  *model;
    Transaction           *trans;
    gboolean               was_open;

    /* We do not use the normal confirmation with this one as we have
       all ready asked the user to confirm delete */

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    trans = view->priv->current_trans;

    /* We need to go back one to select the next transaction */
    gnc_tree_control_split_reg_goto_rel_trans_row (view, 1);

    gtv_begin_edit (view, NULL, trans);

    was_open = xaccTransIsOpen (trans);

    xaccTransDestroy (trans);
    if (was_open)
    {
        DEBUG("committing");
        xaccTransCommitEdit (trans);
    }
    view->priv->dirty_trans = NULL;
}


/* Record changes */
gboolean
gnc_tree_view_split_reg_enter (GncTreeViewSplitReg *view)
{
    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    /* Ask for confirmation if data has been edited, transaction_changed_confirm return TRUE if canceled */
    if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) && transaction_changed_confirm (view, NULL))
    {
        return FALSE;
    }
    return TRUE;
}


/* Cancel the edit and rollback changes */
void
gnc_tree_view_split_reg_cancel_edit (GncTreeViewSplitReg *view, gboolean reg_closing)
{
    GncTreeModelSplitReg *model;
    Transaction          *trans = view->priv->dirty_trans;
    Split                *split;

    ENTER("gnc_tree_view_split_reg_cancel_edit view is %p and reg_closing is %d", view, reg_closing);

    model = get_split_reg_model_from_view (view);

    if (trans && xaccTransIsOpen (trans))
    {
        gnc_tree_control_split_reg_goto_rel_trans_row (view, 0);

        // Remove the split before rollback.
        gnc_tree_model_split_reg_set_blank_split_parent (model, trans, TRUE);

        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
        xaccTransRollbackEdit (view->priv->dirty_trans);

        // Add the split after rollback so it is last in list.
        gnc_tree_model_split_reg_set_blank_split_parent (model, trans, FALSE);
        view->priv->dirty_trans = NULL;

        split = gnc_tree_model_split_get_blank_split (model);
        xaccSplitReinit (split); // Clear the blank split

        if (view->priv->edit_ref != NULL)
        {
            gtk_tree_row_reference_free (view->priv->edit_ref);
            view->priv->edit_ref = NULL;
        }
    }
    /* Reset allow changes for reconciled transctions */
    view->change_allowed = FALSE;
    LEAVE(" ");
}


/* Make sure we have stopped editing */
void
gnc_tree_view_split_reg_finish_edit (GncTreeViewSplitReg *view)
{
    gtv_finish_edit (view);

    /* give gtk+ a chance to handle pending events */
    while (gtk_events_pending ())
       gtk_main_iteration ();
}


/* Returns whether the splits are revealed for the transaction or current position
   if transaction is NULL */
gboolean
gnc_tree_view_split_reg_trans_expanded (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;
    GtkTreePath  *mpath, *spath;
    gboolean expanded;

    /* if trans is NULL use priv->expanded */
    if (trans == NULL)
        expanded = view->priv->expanded;
    else
    {
        model = get_split_reg_model_from_view (view);

        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, trans);

        spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), mpath);

        gtk_tree_path_down (spath); /* Move the path down to trow2 */
        expanded = gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), spath);

        gtk_tree_path_free (mpath);
        gtk_tree_path_free (spath);
    }
    return expanded;
}


/* Collapse the transaction, if trans is NULL,  use current_ref */
void
gnc_tree_view_split_reg_collapse_trans (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *temp_spath, *mpath, *spath;
    gint *indices;
    RowDepth depth;

    ENTER("gnc_tree_view_split_reg_collapse_trans and trans is %p", trans);

    model = get_split_reg_model_from_view (view);

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    /* if trans is NULL use current_ref */
    if (trans == NULL)
        mpath = gtk_tree_row_reference_get_path (view->priv->current_ref);
    else
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, trans);

    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), mpath);

    /* Collapse the view back to the transaction */
    indices = gtk_tree_path_get_indices (spath);
    depth = gtk_tree_path_get_depth (spath);

    if (model->use_double_line)
        temp_spath = gtk_tree_path_new_from_indices (indices[0], 0, -1);
    else
        temp_spath = gtk_tree_path_new_from_indices (indices[0], -1);

    /* if trans is NULL, collapse and update current_ref */
    if (trans == NULL)
    {
        GtkTreePath *temp_mpath;

        gnc_tree_view_split_reg_block_selection (view, TRUE);

        /* Change the selection to last available row of transaction - double */
        if ((model->use_double_line) && (depth == SPLIT3))
            gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), temp_spath);

        /* Change the selection to last available row of transaction - single */
        if ((!model->use_double_line) && (depth != TRANS1))
            gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), temp_spath);

        gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), temp_spath);

        if (view->priv->current_ref != NULL)
        {
            gtk_tree_row_reference_free (view->priv->current_ref);
            view->priv->current_ref = NULL;
        }

        temp_mpath = gtk_tree_model_sort_convert_path_to_child_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), temp_spath);

        view->priv->current_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), temp_mpath);

        gnc_tree_view_split_reg_block_selection (view, FALSE);

        gtk_tree_path_free (temp_mpath);
    }
    else
        gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), temp_spath);

    gtk_tree_path_free (temp_spath);
    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);

    view->priv->expanded = FALSE;
    LEAVE(" ");
}


/* Expands the transaction or the current transaction if NULL */
void
gnc_tree_view_split_reg_expand_trans (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;

    ENTER("gnc_tree_view_split_reg_expand_trans and trans is %p", trans);

    model = get_split_reg_model_from_view (view);

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    if (trans == NULL)
        mpath = gtk_tree_row_reference_get_path (view->priv->current_ref);
    else
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, trans);

    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view))), mpath);

    gtk_tree_view_expand_row (GTK_TREE_VIEW (view), spath, TRUE);

    view->priv->expanded = TRUE;

    if (SELECTION_TO_BLANK_ON_EXPAND && (model->style != REG2_STYLE_JOURNAL))
        gtv_selection_to_blank (view);

    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);

    LEAVE(" ");
}


/* Returns the parent Window */
GtkWidget *
gnc_tree_view_split_reg_get_parent (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    model = get_split_reg_model_from_view (view);
    return gnc_tree_model_split_reg_get_parent (model);
}


/* This sets up the page gui update from the tree view motion callback */
void
gnc_tree_view_split_reg_moved_cb (GncTreeViewSplitReg *view, GFunc cb, gpointer cb_data)
{
    view->moved_cb = cb;
    view->moved_cb_data = cb_data;
}

