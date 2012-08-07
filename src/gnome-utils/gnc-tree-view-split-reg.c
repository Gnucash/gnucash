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

#include "gnc-tree-view.h"
#include "gnc-tree-view-split-reg.h"
#include "gnc-tree-model-split-reg.h"
#include "gnc-gconf-utils.h"
#include "Transaction.h"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_tree_view_split_reg_class_init(GncTreeViewSplitRegClass *klass);
static void gnc_tree_view_split_reg_init(GncTreeViewSplitReg *view);
static void gnc_tree_view_split_reg_dispose(GObject *object);
static void gnc_tree_view_split_reg_finalize(GObject *object);

static void cdf (GtkTreeViewColumn *col, GtkCellRenderer *renderer, GtkTreeModel *model,
                		GtkTreeIter  *iter, gpointer view);

static void gtv_split_reg_edited_cb (GtkCellRendererText *cell, const gchar *path_string,
                          	const gchar *new_text, gpointer _model);

static void start_edit (GtkCellRenderer *cr, GtkCellEditable *editable,
                       		const gchar *path, gpointer user_data);

static void begin_edit(GncTreeViewSplitReg *tv, Split *split, Transaction *trans);

static void get_editable_start_editing_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
   				const gchar *path, gpointer user_data);

static void gtv_split_reg_editing_canceled_cb (GtkCellRenderer *cr, gpointer user_data);

static void gtv_split_reg_match_selected_cb(GtkEntryCompletion *widget, GtkTreeModel *model,
                        GtkTreeIter *iter, gpointer user_data);

static void gtv_split_reg_changed_cb(GtkCellRendererCombo *widget, gchar *path_string,
                        GtkTreeIter *iter, gpointer user_data);

static gboolean gtv_split_reg_key_press_cb (GtkWidget *treeview, GdkEventKey *event, gpointer userdata);
static void gtv_split_reg_motion_cb(GtkTreeSelection *sel, gpointer data);


typedef enum {
    COL_DATE,
    COL_NUMACT,
    COL_DESCNOTES,
    COL_TRANSVOID,
    COL_RECN,
    COL_TYPE,
    COL_VALUE,
    COL_AMOUNT,
    COL_RATE,
    COL_PRICE,
    COL_DEBIT,
    COL_CREDIT,
    COL_BALANCE,
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
     gtv_split_reg_edited_cb, NULL, NULL},

    {COL_TYPE, -1,
     "Type", "type", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},

    {COL_VALUE, -1,
     "Val", "value", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
     NULL, NULL, NULL},

    {COL_AMOUNT, -1,
     "Amt", "amount", "xxxxxxxx",
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
};


struct GncTreeViewSplitRegPrivate
{
    GtkTreeModel *tree_model;
    gboolean disposed;

    Account *anchor;
    gnc_commodity *reg_comm;

    Split *dirty_split;
    Transaction *dirty_trans;

    gchar *acct_edit_path;    // remember which row's account we're editing
    GtkCellRenderer *temp_cr; // Temp Cell Renderer reference

    gboolean has_rate;  /* if set, the transfer dialog will never automatically pop-up */

    gboolean acct_short_names;
    gboolean double_line;
};


#define SPLIT_TRANS_STR _("-- Split Transaction --")

/* This could be a preference setting, The minimum length of the key in order to start completing */
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
    view->priv->double_line = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "double_line_mode", NULL);
    view->priv->acct_short_names = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "show_leaf_account_names", NULL);
}



static void
gnc_tree_view_split_reg_dispose(GObject *object)
{
    GncTreeViewSplitReg *view;
    GncTreeViewSplitRegPrivate *priv;

    gnc_leave_return_if_fail (object != NULL);
    gnc_leave_return_if_fail (GNC_IS_TREE_VIEW_SPLIT_REG (object));

    view = GNC_TREE_VIEW_SPLIT_REG (object);
    priv = GNC_TREE_VIEW_SPLIT_REG_GET_PRIVATE(view);

    if (priv->disposed)
        return;
    priv->disposed = TRUE;

    g_object_unref(G_OBJECT(priv->tree_model));
    priv->tree_model = NULL;

    if (G_OBJECT_CLASS (parent_class)->dispose)
        (* G_OBJECT_CLASS (parent_class)->dispose) (object);
}



static void
gnc_tree_view_split_reg_finalize(GObject *object)
{
    GncTreeViewSplitReg *view;

    gnc_leave_return_if_fail(object != NULL);
    gnc_leave_return_if_fail(GNC_IS_TREE_VIEW_SPLIT_REG (object));

    view = GNC_TREE_VIEW_SPLIT_REG(object);

    if (G_OBJECT_CLASS(parent_class)->finalize)
        (* G_OBJECT_CLASS(parent_class)->finalize) (object);
}



static GncTreeModelSplitReg *
get_split_reg_model_from_view(GncTreeViewSplitReg *tv)
{
    GtkTreeModelSort *s_model = GTK_TREE_MODEL_SORT(
        gtk_tree_view_get_model(GTK_TREE_VIEW(tv)));
    return GNC_TREE_MODEL_SPLIT_REG(gtk_tree_model_sort_get_model(s_model));
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

static ViewCol col_list[] = {
    COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSVOID, COL_RECN,
    COL_TYPE, COL_VALUE, COL_AMOUNT, COL_RATE, COL_PRICE, COL_DEBIT, COL_CREDIT,
    COL_BALANCE, -1};

/* Creates a treeview with the list of fields */
static GncTreeViewSplitReg *
gnc_tree_view_split_reg_set_cols(GncTreeViewSplitReg *tv,
                                   const ViewCol col_list[])
{
    int i = 0;

    GncTreeModelSplitReg *model;
    model = get_split_reg_model_from_view(tv);

    while (col_list && col_list[i] != -1) {
        GtkCellRenderer *cr;
        GtkTreeViewColumn *col;
        ColDef def;
        int j, ncol = G_N_ELEMENTS(all_tree_view_split_reg_columns);

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
                GNC_TREE_VIEW(tv), def.title, def.pref_name, def.sizer,
                def.modelcol, def.visibility_model_col,
                GTK_TREE_MODEL( gnc_tree_model_split_reg_get_acct_list(model)), 0, def.sort_fn);

        } else if (col_list[i] == COL_DATE) {
            col = gnc_tree_view_add_date_column (
                GNC_TREE_VIEW(tv), def.title, def.pref_name, NULL, def.sizer,
                def.modelcol, def.visibility_model_col, def.sort_fn);

        } else if (col_list[i] == COL_NUMACT){ 
            col = gnc_tree_view_add_combo_column (
                GNC_TREE_VIEW(tv), def.title, def.pref_name, def.sizer,
                def.modelcol, def.visibility_model_col,
                GTK_TREE_MODEL( gnc_tree_model_split_reg_get_numact_list(model)), 0, def.sort_fn);

        } else { 
            col = gnc_tree_view_add_text_column (
                GNC_TREE_VIEW(tv), def.title, def.pref_name, NULL, def.sizer,
                def.modelcol, def.visibility_model_col, def.sort_fn);
        } 

        g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
        cr = gnc_tree_view_column_get_renderer(col);


        /* Setup cell background color and default alignment */
        g_object_set( cr, "xalign", 1.0, NULL );
        gtk_tree_view_column_add_attribute(col, cr, "cell-background", GNC_TREE_MODEL_SPLIT_REG_COL_COLOR);

        if (def.editing_started_cb) {
            //Store the position of the column in the model
            g_object_set_data(G_OBJECT(cr), "model_column", GINT_TO_POINTER(def.modelcol));
            g_signal_connect(G_OBJECT(cr), "editing-started", (GCallback) def.editing_started_cb, tv);
        }

        //Connect editing-canceled signal so that edit-cancelled can be set appropriately
        g_signal_connect(G_OBJECT(cr), "editing-canceled", G_CALLBACK(gtv_split_reg_editing_canceled_cb), tv);

        // This can die when prefs are used.
        g_object_set(G_OBJECT(col), "resizable", TRUE, NULL);

        if (def.edited_cb) {
            g_object_set(G_OBJECT(cr), "editable", TRUE, NULL);
            g_signal_connect(G_OBJECT(cr), "edited", (GCallback) def.edited_cb, tv);
        }
        g_object_set_data(G_OBJECT(cr), "view_column", GINT_TO_POINTER(def.viewcol));
        gtk_tree_view_column_set_cell_data_func( col, cr, cdf, tv, NULL);
        i++;
    }

    gnc_tree_view_configure_columns(GNC_TREE_VIEW(tv));

    g_signal_connect(gtk_tree_view_get_selection(GTK_TREE_VIEW(tv)), "changed", G_CALLBACK(gtv_split_reg_motion_cb), tv);

    //Add a data-edited property to keep track of transaction edits
    g_object_set_data(G_OBJECT(tv), "data-edited", FALSE);

    //???? gtk_tree_selection_set_mode(gtk_tree_view_get_selection(tv), GTK_SELECTION_BROWSE);

/*    g_signal_connect_after(G_OBJECT(tv), "key-press-event", G_CALLBACK(gtv_split_reg_key_press_cb), NULL); */
    return tv;
}



GncTreeViewSplitReg*
gnc_tree_view_split_reg_new_with_model(GncTreeModelSplitReg *model)
{
    GtkTreeModel *s_model;
    GncTreeViewSplitReg *tv;
    GtkCellRenderer *cr;
    GtkTreeViewColumn *col;
    GtkTreeSelection    *selection;

    gtk_rc_parse_string ( rc_string );

    tv = g_object_new(gnc_tree_view_split_reg_get_type(), NULL);
    g_object_set(tv, "name", "split_reg_tree", NULL);

    s_model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(model));
/*FIXME  should we  g_object_unref(G_OBJECT(model)); */
    gnc_tree_view_set_model(GNC_TREE_VIEW(tv), s_model);
    g_object_unref(G_OBJECT(s_model));

    tv->priv->anchor = gnc_tree_model_split_reg_get_anchor(model);
    tv->priv->reg_comm = xaccAccountGetCommodity(tv->priv->anchor);
    tv->priv->has_rate = TRUE;  /*FIXME Not sure what this ????? */

    gnc_tree_view_split_reg_set_cols(tv, col_list);

    /* Set default visibilities */
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(tv), TRUE);

    /* TreeView Grid lines */
    if(gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "draw_horizontal_lines", NULL))
    {
        if(gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "draw_vertical_lines", NULL))
            gtk_tree_view_set_grid_lines( GTK_TREE_VIEW(tv), GTK_TREE_VIEW_GRID_LINES_BOTH);
        else
            gtk_tree_view_set_grid_lines( GTK_TREE_VIEW(tv), GTK_TREE_VIEW_GRID_LINES_HORIZONTAL);
    }
    else if(gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "draw_vertical_lines", NULL))
            gtk_tree_view_set_grid_lines( GTK_TREE_VIEW(tv), GTK_TREE_VIEW_GRID_LINES_VERTICAL);
    else
        gtk_tree_view_set_grid_lines( GTK_TREE_VIEW(tv), GTK_TREE_VIEW_GRID_LINES_NONE);


    /* Expanders off */
    /* gtk_tree_view_set_show_expanders (GTK_TREE_VIEW(view), FALSE); */

    /* Tree Selection */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tv));

    gtk_tree_selection_unselect_all( selection );

/*

    gnc_tree_view_configure_columns(view);

    gtk_widget_show(GTK_WIDGET(view)); */
    return tv;
}



/* Returns a value for display. */
static gnc_numeric
get_value_for(GncTreeViewSplitReg *tv, Transaction *trans,
              Split *split, gboolean is_blank)
{
    gnc_commodity *currency = xaccTransGetCurrency(trans);
    gnc_numeric total;

    total = xaccSplitGetValue(split);

    if (is_blank && gnc_numeric_zero_p(total)) {
        gnc_numeric rate;
        total = gnc_numeric_neg(xaccTransGetImbalanceValue(trans));
        if (!gnc_numeric_zero_p(total)) {

            if (!xaccTransGetRateForCommodity(
                    trans, tv->priv->reg_comm, NULL, &rate))
                return gnc_numeric_zero();

            total = gnc_numeric_mul(
                total, rate,
                gnc_commodity_get_fraction (currency),
                GNC_HOW_RND_ROUND);
        }
    } else {
        if (!gnc_numeric_zero_p(total) &&
            gnc_numeric_check(total) == GNC_ERROR_OK) {

            /* fixme: if needs conversion? */
            gnc_commodity *commodity = tv->priv->reg_comm;
/*FIXME  ??          if (commodity && !gnc_commodity_equiv(commodity, currency))
                total = xaccSplitConvertAmount(split, commodity); */
        }
    }
    return total;
}



static gnc_numeric
get_rate_for(GncTreeViewSplitReg *tv, Transaction *trans,
              Split *split, gboolean is_blank)
{
    gnc_numeric num;

    num = get_value_for(tv, trans, split, is_blank);
    num = gnc_numeric_div(
        xaccSplitGetAmount(split), num,
        GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    return num;
}



/* Returns the other Split based on the current Account */
static Split *
get_other_split(GncTreeViewSplitReg *tv, Transaction *trans)
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



/* Returns a Split that matches the current Account */
static Split *
get_this_split(GncTreeViewSplitReg *tv, Transaction *trans)
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



static gboolean
get_model_iter_from_selection(GncTreeViewSplitReg *tv,
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



/* Instead of setting a different cellDataFunc for each column, we just
   collect everything here and use this one func. */
static void
cdf(GtkTreeViewColumn *col, GtkCellRenderer *cell, GtkTreeModel *s_model,
    GtkTreeIter *s_iter, gpointer data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(data);
    GncTreeModelSplitReg *model;
    GtkTreeIter iter;
    GtkTreePath *path;
    ViewCol viewcol;
    gboolean is_split, is_blank, is_trow1, is_trow2;
    gboolean editable, expanded;

    gchar *cell_color;
    gint depth, *indices;

    Account *anchor = tv->priv->anchor;
    Split *split;
    Transaction *trans;

    gnc_numeric num;
    const gchar *s = "";

    ENTER("");

    model = get_split_reg_model_from_view(tv);

    gtk_tree_model_sort_convert_iter_to_child_iter(GTK_TREE_MODEL_SORT(s_model), &iter, s_iter);

    viewcol = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cell), "view_column"));

    g_return_if_fail(gnc_tree_model_split_reg_get_split_and_trans(
                         GNC_TREE_MODEL_SPLIT_REG(model), &iter,
                          &is_trow1, &is_trow2, &is_split, &is_blank,
                          &split, &trans));

    depth = gtk_tree_path_get_depth (gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter));

/* g_print(" cdf depth is %d\n", depth); */

    /* This expands to split from top level and you cannot collapse split, may be needed
    if (depth == 2)
        gtk_tree_view_expand_to_path (GTK_TREE_VIEW(tv), gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter));
    */

    /* This gives me double lines, you can not collapse them */
    if ((tv->priv->double_line) && (depth == 1))
        gtk_tree_view_expand_to_path (GTK_TREE_VIEW(tv), gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter));

    indices = gtk_tree_path_get_indices (gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter));

/* g_print(" cdf path is %s\n", gtk_tree_path_to_string (gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter))); */

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
                ts.tv_sec = time(NULL);
                //xaccTransSetDatePostedSecs(trans, ts.tv_sec);
            }//if
            s = gnc_print_date(ts);
            editable = TRUE;
        }
        else {
            s = "";
            editable = FALSE;
        }
        /* This will remove the calander buttons if FALSE, preference may be ? */
        g_object_set(cell, "use_buttons", TRUE, NULL ); 
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;

    case COL_NUMACT:
        /* Column is NUM/ACT */
        /* Override default alignment */
        g_object_set( cell, "xalign", 0.0, NULL );

        if (is_trow1)
           s = xaccTransGetNum(trans);
        else if (is_trow2)
           s = xaccSplitGetAction(get_this_split(tv, trans));
        else if (is_split)
           s = xaccSplitGetAction(split);

        g_object_set(cell, "text", s, "editable", TRUE, NULL);
        break;

    case COL_DESCNOTES:
        /* Column is DESCRIPTION / NOTES */
        /* Override default alignment */
        g_object_set( cell, "xalign", 0.0, NULL );
        if (is_trow1)
            s =  xaccTransGetDescription (trans);
        else if (is_trow2)
            s =  xaccTransGetNotes(trans);
        else if (is_split)
            s = xaccSplitGetMemo(split);

        g_object_set(cell, "text", s, NULL);
        break;

    case COL_TRANSVOID:
        /* Column is TRANSFER / VOID */

        /* Not sure if this will stay here, this sets the combo column
           0 for short account names, 1 for long */
        if (tv->priv->acct_short_names)
            g_object_set(G_OBJECT(cell), "text-column", 0, NULL );
        else
            g_object_set(G_OBJECT(cell), "text-column", 1, NULL );

        if (is_trow1) {
            gint count = xaccTransCountSplits(trans);
            path = gtk_tree_model_get_path(s_model, s_iter);
            gtk_tree_path_down (path); /* Move the path down to trow2 */
            expanded = gtk_tree_view_row_expanded(GTK_TREE_VIEW(tv), path);
            if (count == 0 || expanded) {
                s = ""; /* blank-out if splits are visible */
            } else if (2 == count) {
                Account *acct;
                Split *osplit;
                osplit = get_other_split(tv, trans);
                acct = xaccSplitGetAccount(osplit);
                if(tv->priv->acct_short_names)
                    s = xaccAccountGetName(acct);
                else
                    s = gnc_account_get_full_name(acct);
            } else {
                s = SPLIT_TRANS_STR;
            }
            editable = anchor && !expanded && ((2 == count) || (0 == count));
            gtk_tree_path_free(path);
        }
        if (is_trow2) {
            s = xaccTransGetVoidReason(trans); /* This is the Void Reason */
            editable = FALSE;
        }
        if (is_split) {
            Account *acct = xaccSplitGetAccount(split);
            if(anchor == acct)
                editable = FALSE;
            else
                editable = TRUE;
            if(tv->priv->acct_short_names)
                s = xaccAccountGetName(acct);
            else
                s = gnc_account_get_full_name(acct);
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;

    case COL_RECN:
        /* Column is RECN */
        if (is_trow1) {
            static char ss[2];
            char rec = xaccSplitGetReconcile(get_this_split(tv, trans));
            if (rec == VREC || rec == FREC || rec == YREC)
                editable = FALSE;
            else
                editable = TRUE;

            ss[0] = rec;
            ss[1] = '\0';
            g_object_set(cell, "text", ss, "editable", editable, NULL);

        } else {
            s = "";
            editable = FALSE;
            g_object_set(cell, "text", s, "editable", editable, NULL);
        }

        if (is_split) {
            static char ss[2];
            char rec = xaccSplitGetReconcile(split);
            if (rec == VREC || rec == FREC || rec == YREC)
                editable = FALSE;
            else
                editable = TRUE;

            ss[0] = rec;
            ss[1] = '\0';
            g_object_set(cell, "text", ss, "editable", editable, NULL);
        }

        break;

    case COL_TYPE:
        /* Column is TYPE */
        if (is_trow1) {
            static char ss[2];
            char type = xaccTransGetTxnType(trans);
            if (type == TXN_TYPE_NONE)
                type = '?';

            ss[0] = type;
            ss[1] = '\0';
            g_object_set(cell, "text", ss, NULL);
        }
        else
        {
            s = "";
            g_object_set(cell, "text", s, NULL);
        }
        break;

    case COL_AMOUNT:
        /* Column is AMOUNT */
       if (is_split) {
            gnc_numeric amt = xaccSplitGetAmount(split);
            s = xaccPrintAmount(amt, gnc_account_print_info( xaccSplitGetAccount(split), TRUE));
            editable = TRUE;
        } else {
            s = "";
            editable = FALSE;
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;

    case COL_VALUE:
        /* Column is VALUE */
        if (is_split) {
            gnc_numeric amt = xaccSplitGetValue(split);
            s = xaccPrintAmount(amt, gnc_commodity_print_info( xaccTransGetCurrency(trans), TRUE));
        } else {
            s = "";
            editable = FALSE;
        }
        g_object_set(cell, "text", s, NULL);
        break;

    case COL_RATE:
        /* Column is RATE */
        if ((is_trow1)||(is_trow2)) {
            s = "";
            editable = FALSE;
        } else {
            gnc_commodity *split_com = xaccAccountGetCommodity( xaccSplitGetAccount(split));
            num = get_rate_for(tv, trans, split, is_blank);
            if (gnc_numeric_check(num) == GNC_ERROR_OK) {
                s = xaccPrintAmount(num, gnc_split_amount_print_info(split, FALSE));
                editable = !gnc_numeric_zero_p(num) &&
                    !gnc_commodity_equiv(split_com, tv->priv->reg_comm);
            } else {
                s = "";
                editable = FALSE;
            }
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;

    case COL_PRICE:
        /* Column is PRICE */
        if ((is_trow1)||(is_trow2)) {
            s = "";
            editable = FALSE;
        } else {
            num = xaccSplitGetSharePrice(split);

            if (gnc_numeric_check(num) == GNC_ERROR_OK) {
                s = xaccPrintAmount(num, gnc_split_amount_print_info(split, FALSE));
                editable = FALSE;
            } else {
                s = "";
                editable = FALSE;
            }
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;

    case COL_DEBIT:
    case COL_CREDIT:
        /* Column is CREDIT and DEBIT */
        if (is_split) {
            num = get_value_for(tv, trans, split, is_blank);
            editable = TRUE;
        } else if (is_trow1) {
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
        } else if (is_trow2) {
            editable = FALSE;
            num = gnc_numeric_zero();
        }

        if ((gnc_numeric_check(num) != GNC_ERROR_OK) || gnc_numeric_zero_p(num) ||
            (gnc_numeric_negative_p(num) && viewcol == COL_CREDIT) ||
            (gnc_numeric_positive_p(num) && viewcol == COL_DEBIT)) {
            s = "";
        } else {
            s = xaccPrintAmount(gnc_numeric_abs(num),
                                gnc_account_print_info(anchor, TRUE)); //FIXME: TRUE just for debugging maybe
        }
        g_object_set(cell, "text", s, "editable", editable, NULL);
        break;

    case COL_BALANCE:
        /* Column is BALANCE */
        if (is_trow1 && anchor) {
            num = xaccTransGetAccountBalance(trans, anchor);
            if (gnc_reverse_balance(anchor))
                num = gnc_numeric_neg(num);
            s = xaccPrintAmount(num, gnc_account_print_info(anchor, TRUE));
            if (gnc_numeric_negative_p(num)
                && gnc_gconf_get_bool(GCONF_GENERAL, KEY_NEGATIVE_IN_RED, NULL))
                g_object_set(cell, "foreground", "red", (gchar*)NULL);
            else
                g_object_set(cell, "foreground", NULL, (gchar*)NULL);
        } else {
            s = "";
        }
        g_object_set(cell, "text", s, NULL);
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
start_edit(GtkCellRenderer *cr, GtkCellEditable *editable,
           const gchar *path_string, gpointer user_data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(user_data);
g_print("start_edit\n");
/*FIXME Not sure if this is required, leave for now ? */
    get_editable_start_editing_cb(cr, editable, path_string, user_data);
/*    g_signal_connect(G_OBJECT(editable), "editing-done", (GCallback) editing_done_cb, tv); */
    tv->priv->acct_edit_path = g_strdup(path_string);

    return;
}


/* means: open trans for editing, unless we're editing a Split (split
   != NULL) AND split doesn't belong to the trans (because it is the
   blank split) */
static void
begin_edit(GncTreeViewSplitReg *tv, Split *split, Transaction *trans)
{
g_print("begin edit\n");
    /* explain me -- this may need changing */
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



static void
remove_edit_date(GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(user_data);
    GncPopupEntry *popup_entry;
    const gchar *new_string; 
    const gchar *current_string;

    //These strings are used to determine if cell data was altered so
    //that keynav works better
g_print("remove edit date\n");

    popup_entry = GNC_POPUP_ENTRY(g_object_get_data( G_OBJECT(tv->priv->temp_cr), "cell-editable"));

    new_string = g_strdup(gtk_entry_get_text(GTK_ENTRY(popup_entry->entry)));
g_print("New String is '%s'\n", new_string);	

    current_string = g_object_get_data(G_OBJECT( tv->priv->temp_cr), "current-string");
g_print("Current String is '%s'\n\n", current_string);

    //If editing wasn't canceled and strings don't match then
    //cell data was edited
    if (!g_object_get_data(G_OBJECT(tv->priv->temp_cr), "edit-canceled") 
             && g_ascii_strcasecmp(new_string, current_string))
    {
        g_object_set_data(G_OBJECT(tv), "data-edited", (gpointer)TRUE);
    }

    g_object_set_data(G_OBJECT(tv->priv->temp_cr), "cell-editable", NULL);
    tv->priv->temp_cr = NULL;
    g_free(tv->priv->acct_edit_path);
    tv->priv->acct_edit_path = NULL;
}



static void
remove_edit_combo(GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(user_data);
    GtkEntry *entry; 
    const gchar *new_string; 
    const gchar *current_string;

    //These strings are used to determine if cell data was altered so
    //that keynav works better
g_print("remove edit combo\n");

    entry = GTK_ENTRY (gtk_bin_get_child(GTK_BIN ( g_object_get_data( G_OBJECT(tv->priv->temp_cr), "cell-editable"))));

    new_string = gtk_entry_get_text(GTK_ENTRY(entry));
g_print("New String is '%s'\n", new_string);	

    current_string = g_object_get_data(G_OBJECT( tv->priv->temp_cr), "current-string");
g_print("Current String is '%s'\n\n", current_string);

    //If editing wasn't canceled and strings don't match then
    //cell data was edited
    if (!g_object_get_data(G_OBJECT(tv->priv->temp_cr), "edit-canceled") 
             && g_ascii_strcasecmp(new_string, current_string))
    {
        g_object_set_data(G_OBJECT(tv), "data-edited", (gpointer)TRUE);
    }

    g_object_set_data(G_OBJECT(tv->priv->temp_cr), "cell-editable", NULL);
    tv->priv->temp_cr = NULL;
    g_free(tv->priv->acct_edit_path);
    tv->priv->acct_edit_path = NULL;
}



static void
remove_edit_completion(GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(user_data);
    const gchar *new_string; 
    const gchar *current_string; 

g_print("remove edit completion\n");

    //These strings are used to determine if cell data was altered so
    //that keynav works better
    new_string = gtk_entry_get_text(GTK_ENTRY(g_object_get_data( G_OBJECT(tv->priv->temp_cr), "cell-editable")));
g_print("New String is '%s'\n", new_string);

    current_string = g_object_get_data(G_OBJECT( tv->priv->temp_cr), "current-string");
g_print("Current String is '%s'\n\n", current_string);

    //If editing wasn't canceled and strings don't match then
    //cell data was edited
    if (!g_object_get_data(G_OBJECT(tv->priv->temp_cr), "edit-canceled") 
             && g_ascii_strcasecmp(new_string, current_string))
    {
        g_object_set_data(G_OBJECT(tv), "data-edited", (gpointer)TRUE);
    }

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
g_print("finish_edit\n");
/*FIXME Not used yet, leave for now */
    if (!col) return;
    cr = gnc_tree_view_column_get_renderer(col);
    if ((ce = GTK_CELL_EDITABLE(g_object_get_data(G_OBJECT(cr), "cell-editable")))) {
        gtk_cell_editable_editing_done(ce);
    }
}



/*####################################################################
          ^^^^^    edit function call backs      ^^^^^
          vvvvvv   gtv function call backs       vvvvv
#####################################################################*/
static void
gtv_split_reg_motion_cb(GtkTreeSelection *sel, gpointer data)
{

    GtkTreeIter iter;
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(data);
    GncTreeModelSplitReg *model; 
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;
    GtkTreeViewColumn *numact, *descnotes, *transvoid;
    gint depth;
    const gchar *path_string;

    model = get_split_reg_model_from_view(tv);

g_print("gtv_split_reg_motion_cb\n\n");

    numact = gtk_tree_view_get_column(GTK_TREE_VIEW(tv), 1);
    descnotes = gtk_tree_view_get_column(GTK_TREE_VIEW(tv), 2);
    transvoid = gtk_tree_view_get_column(GTK_TREE_VIEW(tv), 3);

    if (get_model_iter_from_selection(tv, sel, &iter))
    {
        gnc_tree_model_split_reg_get_split_and_trans (
            GNC_TREE_MODEL_SPLIT_REG(model), &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

        path_string = gtk_tree_path_to_string (gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter));

g_print("Path is '%s'\n", path_string);

       /* Use depth to determine if it is a split or parent transaction */
       depth = gtk_tree_path_get_depth(gtk_tree_path_new_from_string(path_string));

       if (depth == 1)
       {
           gtk_tree_view_column_set_title(numact, _("Number"));
           gtk_tree_view_column_set_title(descnotes, _("Description"));
           gtk_tree_view_column_set_title(transvoid, _("Accounts"));
       }
       else if (depth == 2)
       {
           gtk_tree_view_column_set_title(numact, _("Action"));
           gtk_tree_view_column_set_title(descnotes, _("Notes"));
           gtk_tree_view_column_set_title(transvoid, _("Void"));
       }     
       else if (depth == 3)
       {
           gtk_tree_view_column_set_title(numact, _("Action"));
           gtk_tree_view_column_set_title(descnotes, _("Memo"));
           gtk_tree_view_column_set_title(transvoid, _("Accounts"));
       }
    }
    else
    {
        /* We end up here if tree collapse and no row selected */
        gtk_tree_view_column_set_title(numact, _("Num / Act"));
        gtk_tree_view_column_set_title(descnotes, _("Description / Notes / Memo"));
        gtk_tree_view_column_set_title(transvoid, _("Transfer / Void"));
    }
}



/* Connected to "edited" from cellrenderer. For reference, see
   split-register-model-save.c */
static void
gtv_split_reg_edited_cb(GtkCellRendererText *cell, const gchar *path_string,
               const gchar *new_text, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeIter copy_iter;
    GtkEntryCompletion *completion;
    Split *split;
    Transaction *trans;
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(data);
    ViewCol viewcol;
    gboolean is_trow1, is_trow2, is_split, is_blank;
    GncTreeModelSplitReg *model;
    char *error_loc = NULL;
    Account *anchor = tv->priv->anchor;

g_print("gtv_split_reg_edited_cb\n\n");

/*    g_return_if_fail(get_model_iter_from_view_string(tv, path_string, &iter)); */

    viewcol = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cell), "view_column"));

    model = get_split_reg_model_from_view(tv);
    g_return_if_fail(model);

    gnc_tree_model_split_reg_get_split_and_trans (
        model, &iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

g_print("New text is '%s'\n", new_text);

    switch (viewcol) {
    case COL_DATE:
        /* Column is DATE */
/*FIXME        begin_edit(tv, split, trans); */
        if (is_trow1)
        {
/* New_text would need to be converted to time_t */
/*            xaccTransSetDatePostedSecs (trans, time); */

        }
        break;

    case COL_NUMACT:
        /* Column is NUM / ACT */
/*FIXME        begin_edit(tv, split, trans); */

        if (is_trow1)
        {
/*            xaccTransSetNum(trans, new_text); */

        }
        if (is_trow2)
        {
           /*FIXME Not sure if this will need to be changed */
/*            xaccSplitSetAction(split, new_text); */

        }

        if (is_split)
        {
/*            xaccSplitSetAction(split, new_text); */

        }
        break;

    case COL_DESCNOTES:
        /* Column is DESCRIPTION / NOTES */
/*FIXME        begin_edit(tv, split, trans); */
        if (is_trow1)
        {
/*            xaccTransSetDescription(trans, new_text); */
/*FIXME May need to add new entries to list **
            gtk_list_store_append(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv), "desc_copy")), &copy_iter);
            gtk_list_store_set(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv), "desc_copy")), &copy_iter, 0, new_text, -1);
*/
        }
        if (is_trow2)
        {
/*            xaccTransSetNotes(trans, new_text); */
/*FIXME May need to add new entries to list **
            gtk_list_store_append(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv), "notes_copy")), &copy_iter);
            gtk_list_store_set(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv), "notes_copy")), &copy_iter, 0, new_text, -1); 
*/
        }
        if (is_split)
        {
/*            xaccSplitSetMemo(split, new_text); */
/*FIXME May need to add new entries to list **
            gtk_list_store_append(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv), "memo_copy")), &copy_iter);
            gtk_list_store_set(GTK_LIST_STORE(g_object_get_data(G_OBJECT(tv), "memo_copy")), &copy_iter, 0, new_text, -1); 
*/
        }
        break;

    case COL_TRANSVOID:
    case COL_RECN:
    case COL_TYPE:
    case COL_VALUE:
    case COL_AMOUNT:
    case COL_RATE:
    case COL_PRICE:
    case COL_DEBIT:
    case COL_CREDIT:
        break;

    default:
        //g_assert_not_reached();
        break;
    }
}



/* The main Start Editing Call back for the TEXT columns */
static void
get_editable_start_editing_cb(GtkCellRenderer *cr, GtkCellEditable *editable,
                              const gchar *path_string, gpointer user_data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(user_data);
    GncTreeModelSplitReg *model;

    GtkListStore *description_list;
    GtkListStore *memo_list;
    GtkListStore *notes_list;
    GtkListStore *acct_list;

    GtkEntryCompletion *completion = gtk_entry_completion_new();
    gint depth;

    ENTER(" ");
g_print("get_editable_start_editing_cb\n\n");

    model = get_split_reg_model_from_view(tv);

    description_list = gnc_tree_model_split_reg_get_description_list(model);
    notes_list = gnc_tree_model_split_reg_get_notes_list(model);
    memo_list = gnc_tree_model_split_reg_get_memo_list(model);
    acct_list = gnc_tree_model_split_reg_get_acct_list(model);

g_print("editable Path string is '%s'\n", path_string);

    //Use depth to determine if it is a split or transaction
    depth = gtk_tree_path_get_depth(gtk_tree_path_new_from_string(path_string));

g_print("editable Depth is %u\n", depth);

    /* DATE COLUMN */
    if (GNC_TREE_MODEL_SPLIT_REG_COL_DATE 
        == GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cr), "model_column")))
    {
        g_object_set_data(G_OBJECT(cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data(G_OBJECT(cr), "current-string", g_strdup(gtk_entry_get_text(GTK_ENTRY(GNC_POPUP_ENTRY(editable)->entry))));

        g_signal_connect(G_OBJECT(editable), "remove-widget", (GCallback) remove_edit_date, tv);

g_print("Current String is '%s'\n", g_strdup(gtk_entry_get_text(GTK_ENTRY(GNC_POPUP_ENTRY(editable)->entry))));

    }

    /* TRANSFER / VOID COLUMN */
    if (GNC_TREE_MODEL_SPLIT_REG_COL_TRANSVOID 
        == GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cr), "model_column")))
    {
        GtkEntry *entry;

        entry = GTK_ENTRY (gtk_bin_get_child(GTK_BIN (editable)));

        gtk_entry_set_completion (GTK_ENTRY (entry), completion);
        gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (acct_list));

        /* This sets which text column to use, 0 for short names, 1 for long */
        if (tv->priv->acct_short_names)
            gtk_entry_completion_set_text_column (completion, 0);
        else
            gtk_entry_completion_set_text_column (completion, 1);

        gtk_entry_completion_set_popup_completion (completion, TRUE);
        gtk_entry_completion_set_inline_selection (completion, TRUE);
        gtk_entry_completion_set_popup_set_width (completion, FALSE);
        gtk_entry_completion_set_minimum_key_length (completion, KEY_LENGTH);
/*??        g_signal_connect(G_OBJECT(completion), "match-selected", (GCallback)gtv_split_reg_match_selected_cb, tv); */
        g_object_unref (completion);

        g_object_set_data(G_OBJECT(cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data(G_OBJECT(cr), "current-string", g_strdup(gtk_entry_get_text (entry)));

        g_signal_connect(G_OBJECT(cr), "changed", (GCallback)gtv_split_reg_changed_cb, tv);
        g_signal_connect(G_OBJECT(editable), "remove-widget", (GCallback) remove_edit_combo, tv);
g_print("Current String is '%s'\n", g_strdup(gtk_entry_get_text(entry)));

    }

    /* NUMBER / ACTION COLUMN */
    if (GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT 
        == GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cr), "model_column")))
    {
        GtkEntry *entry;

        if (depth == 1)
            gnc_tree_model_split_reg_get_num_list(model);

        else if (depth == 2)
            gnc_tree_model_split_reg_get_action_list(model);

        else if (depth == 3)
            gnc_tree_model_split_reg_get_action_list(model);

        entry = GTK_ENTRY (gtk_bin_get_child(GTK_BIN (editable)));

        g_object_set_data(G_OBJECT(cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data(G_OBJECT(cr), "current-string", g_strdup(gtk_entry_get_text (entry)));

/*??        g_signal_connect(G_OBJECT(cr), "changed", (GCallback)gtv_split_reg_changed_cb, tv); */
        g_signal_connect(G_OBJECT(editable), "remove-widget", (GCallback) remove_edit_combo, tv);
g_print("Current String is '%s'\n", g_strdup(gtk_entry_get_text(entry)));

    }

    /* DESCRIPTION / NOTES / MEMO COLUMN */
    if (GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES 
        == GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cr), "model_column")))
    {
        //Data used for completion is set based on if editing split or not
        if (depth == 1)
        {
            gtk_entry_set_completion (GTK_ENTRY (editable), completion);
            gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (description_list));
            gtk_entry_completion_set_text_column (completion, 0);
        }
        else if (depth == 2)
        {
            gtk_entry_set_completion (GTK_ENTRY (editable), completion);
            gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (notes_list));
            gtk_entry_completion_set_text_column (completion, 0);
        }
        else if (depth == 3)
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
/*??        g_signal_connect(G_OBJECT(completion), "match-selected", (GCallback)gtv_split_reg_match_selected_cb, tv); */

        g_object_unref (completion);

        g_object_set_data(G_OBJECT(cr), "cell-editable", editable);
        //Copy the string in the GtkEntry for later comparison
        g_object_set_data(G_OBJECT(cr), "current-string", g_strdup(gtk_entry_get_text(GTK_ENTRY(editable))));
        g_signal_connect(G_OBJECT(editable), "remove-widget", (GCallback) remove_edit_completion, tv);
g_print("Current String is '%s'\n", g_strdup(gtk_entry_get_text(GTK_ENTRY(editable))));

    }

    tv->priv->temp_cr = cr;
    //Add edit-canceled property to cr so we can distinguish between
    //cancelled and actual changes
    g_object_set_data(G_OBJECT(cr), "edit-canceled", FALSE);

    LEAVE(" ");
}



//Handle the "match-selected" signal
static void
gtv_split_reg_match_selected_cb(GtkEntryCompletion *widget, GtkTreeModel *model,
                        GtkTreeIter *iter, gpointer user_data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(user_data);

    gboolean is_trow1, is_trow2, is_split, is_blank;

g_print("gtv_split_reg_match_selected_cb\n\n");

/* Not sure what I am going to put in here yet */

}


//Handle the "changed" signal
static void
gtv_split_reg_changed_cb(GtkCellRendererCombo *widget, gchar *path_string,
                        GtkTreeIter *iter, gpointer user_data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(user_data);

    gboolean is_trow1, is_trow2, is_split, is_blank;

g_print("gtv_split_reg_changed_cb\n\n");
g_print("Path String is '%s'\n", path_string);

/* Not sure what I am going to put in here yet */

}


//Handle the "editing-canceled" signal
static void
gtv_split_reg_editing_canceled_cb(GtkCellRenderer *cr, gpointer user_data)
{
    GncTreeViewSplitReg *tv = GNC_TREE_VIEW_SPLIT_REG(user_data);
g_print("gtv_split_reg_editing_canceled_cb\n\n");

    //Set edit-canceled property
    g_object_set_data(G_OBJECT(cr), "edit-canceled", (gpointer)TRUE);	
}

/*####################################################################
          ^^^^   gtv function call backs    ^^^^
#####################################################################*/

