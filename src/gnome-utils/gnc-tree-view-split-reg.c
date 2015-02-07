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
#include <stdlib.h>
#include <string.h>
#include <gdk/gdkkeysyms.h>

#include "gnc-tree-view.h"
#include "gnc-tree-view-split-reg.h"
#include "gnc-tree-model-split-reg.h"
#include "gnc-tree-control-split-reg.h"
#include "gnc-tree-util-split-reg.h"
#include "gnc-ui.h"
#include "gnome-utils/gnc-warnings.h"
#include "dialog-utils.h"
#include "gnc-prefs.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "Scrub.h"
#include "gnc-exp-parser.h"
#include "SchedXaction.h"

#include "gnc-amount-edit.h"


/* Signal codes */
enum
{
    UPDATE_SIGNAL,
    HELP_SIGNAL,
    LAST_SIGNAL
};

typedef enum {
    RESET,  //0
    ACCEPT, //1
    DISCARD,//2
    CANCEL  //3
}TransConfirm;


/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_LEDGER;

static void gnc_tree_view_split_reg_class_init (GncTreeViewSplitRegClass *klass);
static void gnc_tree_view_split_reg_init (GncTreeViewSplitReg *view);
static void gnc_tree_view_split_reg_dispose (GObject *object);
static void gnc_tree_view_split_reg_finalize (GObject *object);

static guint gnc_tree_view_split_reg_signals[LAST_SIGNAL] = {0};

static void gnc_tree_view_split_reg_pref_changed (gpointer prefs, gchar *pref, gpointer user_data);

static void gtv_sr_cdf0 (GtkTreeViewColumn *col, GtkCellRenderer *renderer, GtkTreeModel *s_model,
				GtkTreeIter *s_iter, gpointer user_data);

static void gtv_sr_cdf1 (GtkTreeViewColumn *col, GtkCellRenderer *renderer, GtkTreeModel *s_model,
				GtkTreeIter *s_iter, gpointer user_data);

static void gtv_sr_control_cdf0 (GtkTreeViewColumn *col, GtkCellRenderer *renderer,
                                 GtkTreeModel *s_model, GtkTreeIter *s_iter, gpointer user_data);

static void gtv_sr_titles (GncTreeViewSplitReg *view, RowDepth depth);

static void gtv_sr_edited_cb (GtkCellRendererText *cell, const gchar *path_string,
				const gchar *new_text, gpointer user_data);

static void gtv_sr_edited_normal_cb (GtkCellRendererText *cell, const gchar *path_string,
                                const gchar *new_text, gpointer user_data);

static void gtv_sr_edited_template_cb (GtkCellRendererText *cell, const gchar *path_string,
                                const gchar *new_text, gpointer user_data);

static void start_edit (GtkCellRenderer *cr, GtkCellEditable *editable,
				const gchar *path, gpointer user_data); //FIXME This may not be needed

static void gtv_sr_begin_edit (GncTreeViewSplitReg *view, Transaction *trans);

static void gtv_sr_finish_edit (GncTreeViewSplitReg *view);

static void gtv_sr_editable_start_editing_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
				const gchar *path, gpointer user_data);

static void gtv_sr_editing_canceled_cb (GtkCellRenderer *cr, gpointer user_data);

static void gtv_sr_match_selected_cb (GtkEntryCompletion *widget, GtkTreeModel *model,
                        GtkTreeIter *iter, gpointer user_data); //FIXME This may not be needed

static void gtv_sr_changed_cb (GtkCellRendererCombo *widget, gchar *path_string,
                        GtkTreeIter *iter, gpointer user_data); //FIXME This may not be needed

static void gtv_sr_selection_move_delete_cb (GncTreeModelSplitReg *model, gpointer item, gpointer user_data);

static gboolean gtv_sr_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data);

static gboolean gtv_sr_ed_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data);

static gboolean gtv_sr_button_cb (GtkWidget *widget, GdkEventButton *event, gpointer user_data);

static gboolean gtv_sr_focus_out_cb (GtkWidget *widget, GdkEventFocus *event, gpointer user_data);

static void gtv_sr_motion_cb (GtkTreeSelection *sel, gpointer user_data);

static void gtv_sr_refresh_view_cb (GncTreeModelSplitReg *model, gpointer user_data);

static gboolean gtv_sr_transaction_changed_confirm (GncTreeViewSplitReg *view, Transaction *new_trans);


typedef struct {
    ViewCol viewcol;
    gint modelcol;
    gchar *title;
    gchar *pref_name;
    gchar *sizer;
    int visibility_model_col;
    int always_visible_col;
    void (*edited_cb)(GtkCellRendererText *, const gchar *,
                      const gchar *, gpointer);
    void (*editing_started_cb)(GtkCellRenderer *, GtkCellEditable *,
                               const gchar *, gpointer);
    GtkTreeIterCompareFunc sort_fn;
} ColDef;


static ColDef all_tree_view_split_reg_columns[] = {
    {COL_DATE, GNC_TREE_MODEL_SPLIT_REG_COL_DATE,
     "Date", "date", "00/00/0000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb,
     gnc_tree_model_split_reg_sort_iter_compare_func},

    {COL_DUEDATE, -1,
     "Due Date", "duedate", "00/00/0000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb, NULL},

    {COL_NUMACT, GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT,
     "Num / Act / Act", "numact", "0000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 0,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb,
     gnc_tree_model_split_reg_sort_iter_compare_func},

    {COL_DESCNOTES, GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES,
     "Description / Notes / Memo", "descnotes", "xxxxxxxxxxxxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb,
     gnc_tree_model_split_reg_sort_iter_compare_func},

    {COL_TRANSFERVOID, -1,
     "Transfer / Void", "transfervoid", "xxxxxxxxxxxxxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb, NULL},

    {COL_RECN, GNC_TREE_MODEL_SPLIT_REG_COL_RECN,
     "R", "recn", "xx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 0,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb,
     gnc_tree_model_split_reg_sort_iter_compare_func},

    {COL_TYPE, -1,
     "Type", "type", "xx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 0,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb, NULL},

    {COL_VALUE, -1,
     "Value", "value", "00000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 0,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb, NULL},

    {COL_AMOUNT, -1,
     "Amount", "amount", "00000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 0,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb, NULL},

    {COL_AMTVAL, -1,
     "Amount / Value", "amtval", "00000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb, NULL},

    {COL_RATE, -1,
     "Rate", "rate", "00000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 0,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb, NULL},

    {COL_PRICE, -1,
     "Price", "price", "00000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb, NULL},

    {COL_DEBIT, GNC_TREE_MODEL_SPLIT_REG_COL_DEBIT,
     "Debit", "debit", "00000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb,
     gnc_tree_model_split_reg_sort_iter_compare_func},

    {COL_CREDIT, GNC_TREE_MODEL_SPLIT_REG_COL_CREDIT,
     "Credit", "credit", "00000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     gtv_sr_edited_cb, gtv_sr_editable_start_editing_cb,
     gnc_tree_model_split_reg_sort_iter_compare_func},

    {COL_BALANCE, -1,
     "Balance", "balance", "00000",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 0,
     NULL, NULL, NULL},

    {COL_STATUS, -1,
     " ", "status", "x",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 1,
     NULL, NULL, NULL},

    {COL_COMM, -1,
     "Commodity", "commodity", "xxxxxxxx",
     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, 0,
     NULL, NULL, NULL},
};


struct GncTreeViewSplitRegPrivate
{
    gboolean             disposed;
  
    Account             *anchor;              // The register default Account
    gnc_commodity       *reg_comm;            // The register commodity (which may be a non-currency)
    gnc_commodity       *reg_currency;        // The currency for txns in this register (guaranteed to be a currency)

    Transaction         *current_trans;       // The current highlighted transaction
    Split               *current_split;       // The current highlighted split
    RowDepth             current_depth;       // The current depth 1=TROW1, 2=TROW2, 3=SPLIT3
    GtkTreeRowReference *current_ref;         // The current model path reference

    Transaction         *dirty_trans;         // Set when transaction is changed
    TransConfirm         trans_confirm;       // This is the return value for gtv_sr_transaction_changed_confirm

    GtkCellRenderer     *temp_cr;             // Pointer to Temp Cell Renderer
    gulong               fo_handler_id;       // Focus out callback id

    gboolean             acct_short_names;    // Use account short names
    gboolean             double_line;         // Use double line mode
    gboolean             expanded;            // Are we expanded to splits
    gboolean             auto_complete;       // Whether auto complete has run
    gboolean             negative_in_red;     // Display negative numbers in red
    gboolean             use_horizontal_lines;// Draw horizontal lines
    gboolean             use_vertical_lines;  // Draw vertical lines

    gboolean             show_calendar_buttons;        // Show the calendar buttons
    gboolean             show_extra_dates_on_selection;// Show the above on the selected transaction
    gboolean             selection_to_blank_on_expand; // Move the selection to the blank split on expand

    gint                 key_length;                   // The number of characters before auto complete starts.
    gint                 single_button_press;          // Capture single button press.

    gchar               *transfer_string;              // The transfer account string.
    gboolean             stop_cell_move;               // Stops the cursor moving to a different cell.

};

/* Define some cell colors */
#define PINKCELL "#F8BEC6"
#define REDCELL "#F34943"
#define BLUECELL "#1D80DF"
#define BLACKCELL "#CBCBD2"
#define YELLOWCELL "#FFEF98"
#define ORANGECELL "#F39536"


#define GNC_PREF_SHOW_EXTRA_DATES        "show-extra-dates"
#define GNC_PREF_SHOW_EXTRA_DATES_ON_SEL "show-extra-dates-on-selection"
#define GNC_PREF_SHOW_CAL_BUTTONS        "show-calendar-buttons"
#define GNC_PREF_SEL_TO_BLANK_ON_EXPAND  "selection-to-blank-on-expand"
#define GNC_PREF_KEY_LENGTH              "key-length"

/* This could be a preference setting, show currency / commodity symbols */
#define SHOW_SYMBOL FALSE

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
gnc_tree_view_split_reg_class_init (GncTreeViewSplitRegClass *klass)
{
    GObjectClass *o_class;

    parent_class = g_type_class_peek_parent (klass);

    o_class = G_OBJECT_CLASS (klass);

    o_class->dispose =  gnc_tree_view_split_reg_dispose;
    o_class->finalize = gnc_tree_view_split_reg_finalize;

    g_type_class_add_private (klass, sizeof(GncTreeViewSplitRegPrivate));

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

/*****************************************************************************/

/* Return the tree model from the tree view */
GncTreeModelSplitReg *
gnc_tree_view_split_reg_get_model_from_view (GncTreeViewSplitReg *view)
{
    GtkTreeModelSort *s_model = GTK_TREE_MODEL_SORT (gtk_tree_view_get_model (GTK_TREE_VIEW (view)));
    return GNC_TREE_MODEL_SPLIT_REG (gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (s_model)));
}

/* Get the model iter from the view path string */
static gboolean
gtv_sr_get_model_iter_from_view_string (GncTreeViewSplitReg *view,
                                const gchar *path_string, GtkTreeIter *m_iter)
{
    GtkTreeModel *s_model;
    GtkTreeIter s_iter;

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));

    if (!gtk_tree_model_get_iter_from_string (s_model, &s_iter, path_string))
    {
        m_iter = NULL;
        return FALSE;
    }
    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), m_iter, &s_iter);
    return TRUE;
}

/* Get the model iter from the selection */
static gboolean
gtv_sr_get_model_iter_from_selection (GncTreeViewSplitReg *view,
                              GtkTreeSelection *sel, GtkTreeIter *m_iter)
{
    GtkTreeModel *s_model;
    GtkTreeIter s_iter;

    if (gtk_tree_selection_get_selected (sel, &s_model, &s_iter))
    {
        gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), m_iter, &s_iter);
        return TRUE;
    }
    return FALSE;
}

/* Get sort model path from the model path
 *
 * Return A newly allocated GtkTreePath, or NULL */
GtkTreePath *
gnc_tree_view_split_reg_get_sort_path_from_model_path (GncTreeViewSplitReg *view, GtkTreePath *mpath)
{
    GtkTreeModel *s_model;
    GtkTreePath *spath;

    g_return_val_if_fail (mpath, NULL);
    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    spath = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model), mpath);
    if (!spath)
    {
        /* No parent path available */
        return NULL;
    }
    return spath;
}

/* Get model path from the sort model path
 *
 * Return A newly allocated GtkTreePath, or NULL. */
GtkTreePath *
gnc_tree_view_split_reg_get_model_path_from_sort_path (GncTreeViewSplitReg *view, GtkTreePath *spath)
{
    GtkTreeModel *s_model;
    GtkTreePath *mpath;

    g_return_val_if_fail (spath, NULL);
    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    mpath = gtk_tree_model_sort_convert_path_to_child_path (GTK_TREE_MODEL_SORT (s_model), spath);
    if (!mpath)
    {
        /* No child path available */
        return NULL;
    }
    return mpath;
}

/*****************************************************************************/

static void
gnc_tree_view_split_reg_init (GncTreeViewSplitReg *view)
{
    view->priv = g_new0 (GncTreeViewSplitRegPrivate, 1);

    view->priv->current_trans = NULL;
    view->priv->current_split = NULL;
    view->priv->current_depth = 0;
    view->reg_closing = FALSE;
    view->priv->fo_handler_id = 0;
    view->priv->auto_complete = FALSE;
    view->priv->trans_confirm = RESET;
    view->priv->single_button_press = 0;

    view->priv->transfer_string = g_strdup ("Dummy");
    view->priv->stop_cell_move = FALSE;

    view->priv->show_calendar_buttons = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_SHOW_CAL_BUTTONS);
    view->show_extra_dates = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_SHOW_EXTRA_DATES);
    view->priv->show_extra_dates_on_selection = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_SHOW_EXTRA_DATES_ON_SEL);
    view->priv->selection_to_blank_on_expand = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_SEL_TO_BLANK_ON_EXPAND);
    view->priv->key_length = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_KEY_LENGTH);

    view->priv->acct_short_names = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_SHOW_LEAF_ACCT_NAMES);
    view->priv->negative_in_red = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);
    view->priv->use_horizontal_lines = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                           GNC_PREF_DRAW_HOR_LINES);

    view->priv->use_vertical_lines = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                         GNC_PREF_DRAW_VERT_LINES);

    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REGISTER,
                           GNC_PREF_DRAW_HOR_LINES,
                           gnc_tree_view_split_reg_pref_changed,
                           view);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REGISTER,
                           GNC_PREF_DRAW_VERT_LINES,
                           gnc_tree_view_split_reg_pref_changed,
                           view);
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

    ENTER("split reg view %p", object);

    priv->disposed = TRUE;

    if(view->priv->current_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->current_ref);
        view->priv->current_ref = NULL;
    }

    if (view->help_text)
        g_free (view->help_text);

    if (view->priv->transfer_string)
        g_free (view->priv->transfer_string);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                 GNC_PREF_DRAW_HOR_LINES,
                                 gnc_tree_view_split_reg_pref_changed,
                                 view);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                 GNC_PREF_DRAW_VERT_LINES,
                                 gnc_tree_view_split_reg_pref_changed,
                                 view);

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


/* Update internal settings based on preferences */
void
gnc_tree_view_split_reg_refresh_from_prefs (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    model->use_theme_colors = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                 GNC_PREF_USE_THEME_COLORS);
    model->use_accounting_labels = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                                       GNC_PREF_ACCOUNTING_LABELS);

    model->alt_colors_by_txn = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                   GNC_PREF_ALT_COLOR_BY_TRANS);

    view->priv->negative_in_red = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                                      GNC_PREF_NEGATIVE_IN_RED);
}


static void
gnc_tree_view_split_reg_pref_changed (gpointer prefs, gchar *pref, gpointer user_data)
{
    GncTreeViewSplitReg *view = user_data;

    g_return_if_fail (pref);

    if (view == NULL)
        return;

    if (g_str_has_suffix (pref, GNC_PREF_DRAW_HOR_LINES) || g_str_has_suffix (pref, GNC_PREF_DRAW_VERT_LINES))
    {
        view->priv->use_horizontal_lines = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                               GNC_PREF_DRAW_HOR_LINES);

        view->priv->use_vertical_lines = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                             GNC_PREF_DRAW_VERT_LINES);

        if (view->priv->use_horizontal_lines)
        {
            if (view->priv->use_vertical_lines)
                gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_BOTH);
            else
                gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_HORIZONTAL);
        }
        else if (view->priv->use_vertical_lines)
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_VERTICAL);
        else
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_NONE);
    }
    else
    {
        g_warning("gnc_tree_view_split_reg_pref_changed: Unknown preference %s", pref);
    }
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
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSFERVOID, COL_RECN,
        COL_STATUS, COL_DEBIT, COL_CREDIT, COL_BALANCE, -1};
        return col_list;
        }
        break;

    case GENERAL_JOURNAL2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSFERVOID, COL_RECN,
        COL_STATUS, COL_COMM, COL_VALUE, COL_RATE, COL_AMOUNT, COL_DEBIT, COL_CREDIT, -1};
        return col_list;
        }
        break;

    case STOCK_REGISTER2:
    case CURRENCY_REGISTER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSFERVOID, COL_RECN,
        COL_STATUS, COL_AMTVAL, COL_PRICE, COL_DEBIT, COL_CREDIT, COL_BALANCE, -1};
        return col_list;
        }
        break;

    case RECEIVABLE_REGISTER2:
    case PAYABLE_REGISTER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_TYPE, COL_DUEDATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSFERVOID,
        COL_STATUS, COL_DEBIT, COL_CREDIT, COL_BALANCE, -1};
        return col_list;
        }

     case PORTFOLIO_LEDGER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSFERVOID, COL_RECN,
        COL_STATUS, COL_AMOUNT, COL_PRICE, COL_DEBIT, COL_CREDIT, -1};
        return col_list;
        }

    case SEARCH_LEDGER2:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSFERVOID, COL_RECN,
        COL_STATUS, COL_DEBIT, COL_CREDIT, -1};
        return col_list;
        }
        break;

    default:
        {
        static ViewCol col_list[] = {
        COL_DATE, COL_NUMACT, COL_DESCNOTES, COL_TRANSFERVOID, COL_RECN,
	COL_STATUS,
        COL_VALUE, COL_AMOUNT, COL_RATE, COL_PRICE, COL_DEBIT, COL_CREDIT,
        COL_BALANCE, -1};
        return col_list;
        }
    }
}


/* Creates a treeview with the list of fields */
static GncTreeViewSplitReg *
gnc_tree_view_split_reg_set_cols (GncTreeViewSplitReg *view,
				  GncTreeModelSplitReg *model,
				  ViewCol col_list[])
{
    int i = 0;

    while (col_list && col_list[i] != -1) {
        GList *renderers;
        GtkCellRenderer *cr0;
        GtkCellRenderer *cr1;
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
        if (col_list[i] == COL_TRANSFERVOID) {

            col = gnc_tree_view_add_combo_column (
                GNC_TREE_VIEW (view), def.title, def.pref_name, def.sizer,
                def.modelcol, def.visibility_model_col,
                GTK_TREE_MODEL (gnc_tree_model_split_reg_get_acct_list (model)), 0, def.sort_fn);

        } else if (col_list[i] == COL_DATE) {
            col = gnc_tree_view_add_date_column (
                GNC_TREE_VIEW (view), def.title, def.pref_name, NULL, def.sizer,
                def.modelcol, def.visibility_model_col, def.sort_fn);

        } else if (col_list[i] == COL_NUMACT) { 
            col = gnc_tree_view_add_combo_column (
                GNC_TREE_VIEW (view), def.title, def.pref_name, def.sizer,
                def.modelcol, def.visibility_model_col,
                GTK_TREE_MODEL (gnc_tree_model_split_reg_get_action_list (model)), 0, def.sort_fn);

            // Here we are adding a second renderer, we will use the model to switch between the
            // two by hiding one so we endup with rows of text or combo renderers.
            cr1 = gtk_cell_renderer_text_new ();
            gtk_tree_view_column_pack_start (col, cr1, TRUE);
            gtk_tree_view_column_add_attribute (col, cr1, "visible", GNC_TREE_MODEL_SPLIT_REG_COL_NUM_VIS);

            // Set all the same properties as the first renderer.
            g_object_set (cr1, "xalign", 1.0, NULL);
            g_object_set_data (G_OBJECT(cr1), "model_column", GINT_TO_POINTER (def.modelcol));
            g_object_set_data (G_OBJECT(cr1), "column_name", GINT_TO_POINTER (def.pref_name));
            g_signal_connect (G_OBJECT(cr1), "editing-started", (GCallback) def.editing_started_cb, view);
            g_signal_connect (G_OBJECT(cr1), "editing-canceled", G_CALLBACK (gtv_sr_editing_canceled_cb), view);
            g_object_set (G_OBJECT (cr1), "editable", TRUE, NULL);
            g_signal_connect (G_OBJECT (cr1), "edited", (GCallback) def.edited_cb, view);
            g_object_set_data (G_OBJECT (cr1), "view_column", GINT_TO_POINTER (def.viewcol));
            gtk_tree_view_column_set_cell_data_func (col, cr1, gtv_sr_cdf1, view, NULL);

        } else { 
            col = gnc_tree_view_add_text_column (
                GNC_TREE_VIEW (view), def.title, def.pref_name, NULL, def.sizer,
                def.modelcol, def.visibility_model_col, def.sort_fn);
        } 

        g_object_set_data (G_OBJECT (col), DEFAULT_VISIBLE, GINT_TO_POINTER (1));
        g_object_set_data (G_OBJECT (col), ALWAYS_VISIBLE, GINT_TO_POINTER (def.always_visible_col));

        // Set the properties for the first renderer.
        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (col));
        cr0 = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        /* Setup cell background color and default alignment */
        g_object_set (cr0, "xalign", 1.0, NULL);

        if (col_list[i] == COL_NUMACT)
            gtk_tree_view_column_add_attribute (col, cr0, "visible", GNC_TREE_MODEL_SPLIT_REG_COL_ACT_VIS);

        /* Add the full title for status column to the object for menu creation */
        if (col_list[i] == COL_STATUS)
            g_object_set_data_full (G_OBJECT(col), REAL_TITLE, g_strdup (_("Status Bar")), g_free);

        /* This sets the background of the treeview control columns */
        gnc_tree_view_set_control_column_background (GNC_TREE_VIEW (view), 0, gtv_sr_control_cdf0);

        if (def.editing_started_cb)
        {
            //Store the position of the column in the model
            g_object_set_data (G_OBJECT (cr0), "model_column", GINT_TO_POINTER (def.modelcol));
            g_object_set_data (G_OBJECT (cr0), "column_name", GINT_TO_POINTER (def.pref_name));
            g_signal_connect (G_OBJECT (cr0), "editing-started", (GCallback) def.editing_started_cb, view);
        }

        // Connect editing-canceled signal so that edit-cancelled can be set appropriately
        g_signal_connect (G_OBJECT (cr0), "editing-canceled", G_CALLBACK (gtv_sr_editing_canceled_cb), view);

        gtk_tree_view_column_set_sizing (col, GTK_TREE_VIEW_COLUMN_FIXED);

//        gtk_tree_view_column_set_min_width (col, -1);

        // Set Columns to be resizable default.
        g_object_set (G_OBJECT (col), "resizable", TRUE, NULL);

        // Allow the columns to be reorderable.
        g_object_set (G_OBJECT (col), "reorderable", TRUE, NULL);

        if (def.edited_cb)
        {
            g_object_set (G_OBJECT (cr0), "editable", TRUE, NULL);
            g_signal_connect (G_OBJECT (cr0), "edited", (GCallback) def.edited_cb, view);
        }

        g_object_set_data (G_OBJECT (cr0), "view_column", GINT_TO_POINTER (def.viewcol));
        gtk_tree_view_column_set_cell_data_func (col, cr0, gtv_sr_cdf0, view, NULL);

        i++;
    }
    gtk_tree_selection_set_mode (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), GTK_SELECTION_BROWSE);

    g_signal_connect (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), "changed", G_CALLBACK (gtv_sr_motion_cb), view);

    //Add a data-edited property to keep track of transaction edits.
    g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));

    // This is used to move the selected item if the selected transaction is deleted.
    g_signal_connect (G_OBJECT (model), "selection_move_delete", G_CALLBACK (gtv_sr_selection_move_delete_cb), view);

    // This will refresh the view.
    g_signal_connect (G_OBJECT (model), "refresh_view", G_CALLBACK (gtv_sr_refresh_view_cb), view);

    // This is for key navigation, tabbing...
    g_signal_connect (G_OBJECT (view), "key-press-event", G_CALLBACK (gtv_sr_key_press_cb), NULL);

    // This is for mouse buttons...
    g_signal_connect (G_OBJECT (view), "button_press_event", G_CALLBACK (gtv_sr_button_cb), NULL);

    return view;
}


/* Set up the view */
gboolean
gnc_tree_view_split_reg_set_format (GncTreeViewSplitReg *view)
{
    GncTreeViewSplitRegPrivate *priv;
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;
    gint total_num = 0;

    ENTER(" #### Set View Format #### ");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    priv = view->priv;

    total_num = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (model), NULL);

    mpath = gtk_tree_row_reference_get_path (view->priv->current_ref);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    priv->expanded = FALSE;

    {
        if (model->style == REG2_STYLE_JOURNAL)
        {
            gtk_tree_view_expand_all (GTK_TREE_VIEW (view));

            priv->expanded = TRUE;

            gtk_tree_path_free (mpath);
            gtk_tree_path_free (spath);

            /* This updates the plugin page gui */
            gnc_tree_view_split_reg_call_uiupdate_cb (view);

            LEAVE("#### Journal format ####");
            return (FALSE);
        }

        if (!model->use_double_line)
        {
            gtk_tree_view_collapse_all (GTK_TREE_VIEW (view));

            priv->expanded = FALSE;

            LEAVE("#### Single line foramt ####");
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
            LEAVE("#### Double line format ####");
        }

        /* This expands to split from top level auto.. */
        if ((model->style == REG2_STYLE_AUTO_LEDGER) || (model->style == REG2_STYLE_JOURNAL))
        {
            gtk_tree_view_expand_row (GTK_TREE_VIEW (view), spath, TRUE);

            priv->expanded = TRUE;
            LEAVE("#### Auto expand line format ####");
        }
    }

    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);

    /* This updates the plugin page gui */
    gnc_tree_view_split_reg_call_uiupdate_cb (view);

    return (FALSE);
}


/* Set up the view for this transaction, used in transaction discard and cancel */
static gboolean
gnc_tree_view_split_reg_format_trans (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeViewSplitRegPrivate *priv;
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;

    ENTER(" ");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    priv = view->priv;

    mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, trans);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    if ((!model->use_double_line) && (model->style != REG2_STYLE_JOURNAL))
    {
        gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), spath);
        priv->expanded = FALSE;
        LEAVE("#### Single line transaction foramt ####");
    }

    if ((model->use_double_line) && (model->style != REG2_STYLE_JOURNAL))
    {
        gtk_tree_view_expand_to_path (GTK_TREE_VIEW (view), spath);
        gtk_tree_path_down (spath);
        gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), spath);
        gtk_tree_path_up (spath);
        priv->expanded = FALSE;
        LEAVE("#### Double line transaction format ####");
    }

    /* This expands to split from top level auto.. */
    if ((model->style == REG2_STYLE_AUTO_LEDGER) || (model->style == REG2_STYLE_JOURNAL))
    {
        gtk_tree_view_expand_row (GTK_TREE_VIEW (view), spath, TRUE);
        priv->expanded = TRUE;
        LEAVE("#### Auto expand line transaction format ####");
    }

    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);

    /* This updates the plugin page gui */
    gnc_tree_view_split_reg_call_uiupdate_cb (view);

    return (FALSE);
}


/* Callback to update the view after transactions are added or deleted */
static void
gtv_sr_refresh_view_cb (GncTreeModelSplitReg *model, gpointer user_data)
{
    GncTreeViewSplitReg *view = user_data;

    gnc_tree_view_split_reg_set_format (view);
}


/* Create a tree view from a given model */
GncTreeViewSplitReg*
gnc_tree_view_split_reg_new_with_model (GncTreeModelSplitReg *model)
{
    GtkTreeModel        *s_model;
    GncTreeViewSplitReg *view;
    GtkTreeSelection    *selection;

    view = g_object_new (gnc_tree_view_split_reg_get_type(), NULL);
    g_object_set (view, "name", "split_reg_tree", NULL);

    view->priv->anchor = gnc_tree_model_split_reg_get_anchor (model);
    view->priv->reg_comm = xaccAccountGetCommodity (view->priv->anchor);
    view->priv->reg_currency = gnc_account_or_default_currency (view->priv->anchor, NULL);
    g_assert (view->priv->reg_currency);
    g_assert (gnc_commodity_is_currency (view->priv->reg_currency));
    view->help_text = g_strdup ("Help Text");

    // This sets up solid lines for the grid line.
    gtk_rc_parse_string (rc_string);

    /* TreeView Grid lines */
    if (view->priv->use_horizontal_lines)
    {
        if (view->priv->use_vertical_lines)
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_BOTH);
        else
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_HORIZONTAL);
    }
    else if (view->priv->use_vertical_lines)
            gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_VERTICAL);
    else
        gtk_tree_view_set_grid_lines (GTK_TREE_VIEW (view), GTK_TREE_VIEW_GRID_LINES_NONE);

    // Set the view to fixed height mode...
//    gtk_tree_view_set_fixed_height_mode (GTK_TREE_VIEW (view), TRUE);

    /* Expanders off */
    gtk_tree_view_set_show_expanders (GTK_TREE_VIEW (view), FALSE);

    /* Tree Selection */
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));

    gtk_tree_selection_unselect_all (selection);

    // Setup the sort model
    s_model = gtk_tree_model_sort_new_with_model (GTK_TREE_MODEL (model));

    PINFO("#### After Models are Setup ####");

    /* Set the user_data for the sort callback */
    gnc_tree_view_set_sort_user_data (GNC_TREE_VIEW (view), s_model);

    /* Set up the columns */
    gnc_tree_view_split_reg_set_cols (view, model, gnc_tree_view_split_reg_get_colummn_list (model));

    PINFO("#### Before View connected to Model ####");

    // Connect model to tree view
    gtk_tree_view_set_model (GTK_TREE_VIEW (view), s_model);
    g_object_unref (G_OBJECT (s_model));

    PINFO("#### After View connected to Model ####");

    // Default the sorting to date.
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (s_model),
                                          GNC_TREE_MODEL_SPLIT_REG_COL_DATE,
                                          GTK_SORT_ASCENDING);

    PINFO("#### After Set Default Sort Column ####");

    return view;
}


/* This allows the blocking / unblocking of selection */
void
gnc_tree_view_split_reg_block_selection (GncTreeViewSplitReg *view, gboolean block)
{
    if (block)
        g_signal_handlers_block_by_func (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), gtv_sr_motion_cb, view);
    else
        g_signal_handlers_unblock_by_func (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), gtv_sr_motion_cb, view);
}


/* Set the default selection path */
void
gnc_tree_view_split_reg_default_selection (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *new_mpath, *mpath, *spath;
    gint *indices;

    ENTER("#### Default Selection ####");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    /* Do we have a current transaction set on the model, use it */
    if (model->current_trans != NULL)
        view->priv->current_trans = model->current_trans;

    /* Set the default start position to end of list */
    if (view->priv->current_trans == NULL)
    {
        Transaction *btrans;

        btrans = gnc_tree_control_split_reg_get_blank_trans (view);
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, btrans);
        view->priv->current_trans = btrans;
    }
    else
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, view->priv->current_split, view->priv->current_trans);

    indices = gtk_tree_path_get_indices (mpath);

    if (view->priv->current_depth == 2)
        new_mpath = gtk_tree_path_new_from_indices (indices[0], indices[1], -1);
    else
        new_mpath = gtk_tree_path_new_from_indices (indices[0], -1);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, new_mpath);

    {
        gchar *mstring, *sstring, *tstring;
        mstring = gtk_tree_path_to_string (mpath);
        sstring = gtk_tree_path_to_string (spath);
        tstring = gtk_tree_path_to_string (new_mpath);
        DEBUG("default_selection mpath is %s, spath is %s, new path is %s", mstring, sstring, tstring);
        g_free (mstring);
        g_free (sstring);
        g_free (tstring);
    }

    if (view->priv->current_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->current_ref);
        view->priv->current_ref = NULL;
    }
    view->priv->current_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), new_mpath);

    /* Update the titles */
    gtv_sr_titles (view, view->priv->current_depth);

    /* Make sure blank split is on current transaction */
    gnc_tree_model_split_reg_set_blank_split_parent (model, view->priv->current_trans, FALSE);

    PINFO("#### Default Selection - After Titles ####");

    /* Set the view format */
    gnc_tree_view_split_reg_set_format (view);

    PINFO("#### Default Selection - After View Format ####");

    /* scroll window to show selection */
    gnc_tree_view_split_reg_scroll_to_cell (view);

    /* Set cursor to new spath */
    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, NULL, FALSE);

    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);
    gtk_tree_path_free (new_mpath);

    LEAVE("#### Leave Default Selection ####");
}

/*###########################################################################*/

/* Sets read only flag */
void
gnc_tree_view_split_reg_set_read_only (GncTreeViewSplitReg *view, gboolean read_only)
{
    GncTreeModelSplitReg *model;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    model->read_only = read_only;
}


/* Return the register commodity */
gnc_commodity *
gnc_tree_view_split_reg_get_reg_commodity (GncTreeViewSplitReg *view)
{
    return view->priv->reg_comm;
}


/* Returns a Split that matches the current Account */
static Split *
gtv_sr_get_this_split (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;
    int i;
    Split *split = NULL;
    Account *anchor;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    if (xaccTransCountSplits (trans) == 0) // this may be a blank or a reinit trans.
    {
        if (gnc_tree_model_split_reg_is_blank_split_parent (model, trans))
            return gnc_tree_model_split_get_blank_split (model);
    }

    for (i = 0; (split = xaccTransGetSplit (trans, i)); i++) {
        if (anchor == xaccSplitGetAccount (split))
            return split;
    }
    return NULL;
}


/* The returned Splits may be newly created and not yet belong to trans. */
static gboolean
gtv_sr_get_split_pair (GncTreeViewSplitReg *view, Transaction *trans, Split **osplit, Split **split)
{
    GncTreeModelSplitReg *model;
    QofBook       *book;

    gint count = xaccTransCountSplits (trans);
    Account *anchor = view->priv->anchor;

    book = gnc_get_current_book();

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    if (count == 0) // blank trans
    {
        *split = gnc_tree_model_split_get_blank_split (model);
        xaccSplitSetAccount (*split, anchor);
        xaccSplitSetParent (*split, trans);
        *osplit = xaccMallocSplit (book);
        xaccSplitSetParent (*osplit, trans);
    }
    else
    {
        int i;
        Split *s, *first_split;

        first_split = xaccTransGetSplit (trans, 0);

        if (gnc_tree_util_split_reg_is_multi (first_split)) // multi trans
            return FALSE;
        else // two split trans
        {
            for (i = 0; (s = xaccTransGetSplit (trans, i)); i++)
            {
                if (anchor == xaccSplitGetAccount (s))
                {
                    *split = s;
                    break;
                }
            }
            g_assert (*split);
            *osplit = xaccSplitGetOtherSplit(*split);
            g_assert (*osplit);
        }
    }
    DEBUG("gtv_sr_get_split_pair return - trans is %p, osplit is %p and split %p is set to anchor %p", trans, *osplit, *split, anchor);
    return TRUE;
}


/* Does this transaction have any Imbalance splits */
static gboolean
gtv_sr_get_imbalance (Transaction *trans)
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


/* Only allow changes to values if we have valid split accounts */
static gboolean
gtv_sr_have_account (GncTreeViewSplitReg *view, RowDepth depth, gboolean expanded, gboolean is_template, Transaction *trans, Split *split)
{
    gboolean gtv_sr_have_account = FALSE;

    DEBUG("gtv_sr_have_account trans %p, split %p, expanded %d, depth %d", trans, split, expanded, depth);

    if ((depth == TRANS1) && !expanded && !gnc_tree_util_split_reg_is_multi (split)) // normal trans
    {
        if (xaccSplitGetAccount (xaccSplitGetOtherSplit (split)) != NULL)
            gtv_sr_have_account = TRUE;
    }

    if ((depth == SPLIT3) && (xaccTransCountSplits (trans) == 0)) // blank trans, blank split
        gtv_sr_have_account = TRUE;

    if (depth == SPLIT3)
    {
        if (!is_template) // Are we using a template
        {
            Account *acc = xaccSplitGetAccount (split);
            if (acc != NULL)
            {
                if (xaccAccountGetType (acc) != ACCT_TYPE_TRADING)
                    gtv_sr_have_account = TRUE; // normal split
                else
                    gtv_sr_have_account = FALSE; // trading split
            }
         }
         else
         {
             if (gnc_tree_util_split_reg_template_get_transfer_entry (split) != NULL)
                 gtv_sr_have_account = TRUE;
         }
    }
    return gtv_sr_have_account;
}

/*###########################################################################*/

/* This cellDataFunc is to set the cell-background property of the control columns. */
static void
gtv_sr_control_cdf0 (GtkTreeViewColumn *col, GtkCellRenderer *cell, GtkTreeModel *s_model,
    GtkTreeIter *s_iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeIter m_iter;
    GtkTreePath *mpath;
    Transaction *trans;
    Split *split;
    gboolean is_split, is_blank, is_trow1, is_trow2;
    const gchar *row_color;

    gint *indices;

    ENTER("");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), &m_iter, s_iter);

    g_return_if_fail (gnc_tree_model_split_reg_get_split_and_trans (
                         GNC_TREE_MODEL_SPLIT_REG (model), &m_iter,
                          &is_trow1, &is_trow2, &is_split, &is_blank,
                          &split, &trans));

    mpath = gtk_tree_model_get_path (GTK_TREE_MODEL (model), &m_iter);

    indices = gtk_tree_path_get_indices (mpath);

    row_color = gnc_tree_model_split_reg_get_row_color (model, is_trow1, is_trow2, is_split, indices[0]);

    gtk_tree_path_free (mpath);

    /* Set the background color / this works for sorting and deleting transactions */
    g_object_set (cell, "cell-background", row_color, (gchar*)NULL);

    LEAVE("");
}


/* Instead of setting a different cellDataFunc for each column, we just
   collect everything here for the first cell renderer. */
static void
gtv_sr_cdf0 (GtkTreeViewColumn *col, GtkCellRenderer *cell, GtkTreeModel *s_model,
    GtkTreeIter *s_iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeIter m_iter;
    GtkTreePath *spath;
    ViewCol viewcol;
    Transaction *trans;
    Split *split;
    gboolean is_split, is_blank, is_trow1, is_trow2;
    gboolean editable = FALSE, expanded = FALSE;
    gboolean read_only = FALSE;
    gboolean open_edited = FALSE;
    gboolean is_template = FALSE;
    gboolean negative_in_red = FALSE;
    gboolean show_extra_dates = FALSE;
    gnc_numeric num;
    const gchar *s = "";
    const gchar *row_color;
    RowDepth depth;
    gint *indices;
    Account *anchor = view->priv->anchor;
    char type;

    ENTER("");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), &m_iter, s_iter);

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    g_return_if_fail (gnc_tree_model_split_reg_get_split_and_trans (
                         GNC_TREE_MODEL_SPLIT_REG (model), &m_iter,
                          &is_trow1, &is_trow2, &is_split, &is_blank,
                          &split, &trans));

    spath = gtk_tree_model_get_path (GTK_TREE_MODEL (s_model), s_iter);

    depth = gtk_tree_path_get_depth (spath);

    indices = gtk_tree_path_get_indices (spath);

    row_color = gnc_tree_model_split_reg_get_row_color (model, is_trow1, is_trow2, is_split, indices[0]);

    /* Lets see if the splits are expanded */
    if (is_trow1 || is_trow2) // transaction
    {
        if (is_trow1)
            gtk_tree_path_down (spath); /* Move the path down to trow2 */
        expanded = gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), spath);
    }
    else
        expanded = TRUE; // splits are always expanded

    gtk_tree_path_free (spath);

    /* Set the background color / this works for sorting and deleting of transactions */
    g_object_set (cell, "cell-background", row_color, (gchar*)NULL);

    /* Get the read only model setting */
    gtk_tree_model_get (GTK_TREE_MODEL (model), &m_iter, GNC_TREE_MODEL_SPLIT_REG_COL_RO, &read_only, -1);

    /* Are we being edited in other register */
    if (xaccTransIsOpen (trans) && (view->priv->dirty_trans != trans))
    {
        read_only = TRUE;
        open_edited = TRUE;
    }

    /* Test for a transaction type of invoice, always read only */
    type = xaccTransGetTxnType (trans);
    if (model->type == RECEIVABLE_REGISTER2 || model->type == PAYABLE_REGISTER2)
    {
        if (((type == TXN_TYPE_INVOICE) || (type == TXN_TYPE_NONE)) && (view->priv->dirty_trans != trans) && !is_blank)
            read_only = TRUE;
    }

    /* Is this a template */
    is_template = gnc_tree_model_split_reg_get_template (model);

    /* Show negative numbers in red */
    negative_in_red = view->priv->negative_in_red;

    switch (viewcol) {
    case COL_DATE:
        /* Column is DATE */
        if (is_split)
            g_object_set (cell, "cell-background", "white", (gchar*)NULL);

        // Show the extra dates for selected transaction
        if ((view->priv->current_trans == trans) && view->priv->show_extra_dates_on_selection)
            show_extra_dates = TRUE;

        // Show the extra dates allways
        if (view->show_extra_dates == TRUE)
            show_extra_dates = TRUE;

        if (is_trow1) {
            Timespec ts = {0,0};
            xaccTransGetDatePostedTS (trans, &ts);
            //If the time returned by xaccTransGetDatePostedTS is 0 then assume it
            //is a new transaction and set the time to current time to show current
            //date on new transactions
            if (ts.tv_sec == 0)
            {
                ts.tv_sec = gnc_time (NULL);
                //xaccTransSetDatePostedSecs (trans, ts.tv_sec);
            }//if
            s = gnc_print_date (ts);
            editable = TRUE;
        }
        else if (is_trow2 && show_extra_dates) {
            Timespec ts = {0,0};

            g_object_set (cell, "cell-background", YELLOWCELL, (gchar*)NULL);

            xaccTransGetDateEnteredTS (trans, &ts);
            //If the time returned by xaccTransGetDateEnteredTS is 0 then assume it
            //is a new transaction and set the time to current time to show current
            //date on new transactions
            if (ts.tv_sec == 0)
            {
                ts.tv_sec = gnc_time (NULL);
                //xaccTransSetDateEnteredSecs (trans, ts.tv_sec);
            }//if
            s = gnc_print_date (ts);
            editable = FALSE;
        }
        else if (is_split && show_extra_dates) {
            Timespec ts = {0,0};

            if (xaccSplitGetReconcile (split) == YREC)
            {
                xaccSplitGetDateReconciledTS (split, &ts);
                //If the time returned by xaccTransGetDateEnteredTS is 0 then assume it
                //is a new transaction and set the time to current time to show current
                //date on new transactions
                if (ts.tv_sec == 0)
                {
                    ts.tv_sec = gnc_time (NULL);
                    //xaccSplitSetDateReconciledTS (split, ts.tv_sec);
                }//if
                s = gnc_print_date (ts);
            }
            else
                s = "";
            editable = FALSE;
        }
        else {
            s = "";
            editable = FALSE;
        }

        /* Is this a template */
        if (is_template && is_trow1)
        {
            s =  _(" Scheduled ");
            editable = FALSE;
        }
        else if (is_template && is_trow2 && show_extra_dates)
        {
            s = "";
            editable = FALSE;
        }
        else if (is_template && is_split && show_extra_dates)
        {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        /* This will remove the calander buttons if FALSE */
        g_object_set (cell, "use_buttons", view->priv->show_calendar_buttons, NULL );
        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_DUEDATE:
        /* Column is DUE DATE */
        if (is_split)
            g_object_set (cell, "cell-background", "white", (gchar*)NULL);

        if (is_trow1) {
            Timespec ts = {0,0};

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
        /* Column is NUM / ACT but relates to ACT */
        /* Override default alignment */
        g_object_set (cell, "xalign", 0.0, NULL );

        editable = TRUE;

        if (is_trow1)
            /* Get per book option */
            s = gnc_get_num_action (trans, gtv_sr_get_this_split (view, trans));

        else if (is_trow2 && expanded)
        {
            /* Get per book option */
            if (qof_book_use_split_action_for_num_field (gnc_get_current_book()))
                s = gnc_get_action_num (trans, gtv_sr_get_this_split (view, trans));
            else
                s = "";
            editable = FALSE;
        }
        else if (is_trow2 && !expanded)
        {
            /* Get per book option */
            if (gtv_sr_get_this_split (view, trans) != NULL) // Blank split of blank trans is not child of trans yet.
               s = gnc_get_action_num (trans, gtv_sr_get_this_split (view, trans));
            else
               s = "";
        }
        else if (is_split)
            /* Get split-action with gnc_get_num_action which is the same as
             * xaccSplitGetAction with these arguments */
            s = gnc_get_num_action (NULL, split);

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

    case COL_TRANSFERVOID:
        /* Column is TRANSFER / VOID */
        /* Not sure if this will stay here, this sets the combo column
           0 for short account names, 1 for long */
        if (view->priv->acct_short_names)
            g_object_set (G_OBJECT (cell), "text-column", 0, NULL );
        else
            g_object_set (G_OBJECT (cell), "text-column", 1, NULL );

        {
            gchar *string = NULL;

            if (is_trow1)
            {
                if (expanded)
                {
                    string = g_strdup (" "); /* blank-out if splits are visible */
                    editable = FALSE;
                }
                else
                {
                    gboolean is_multi;
                    string = g_strdup (gnc_tree_util_split_reg_get_transfer_entry (gtv_sr_get_this_split (view, trans), &is_multi));

                    editable = anchor && !expanded && !is_multi;
                }
            }
            if (is_trow2)
            {
                string = g_strdup (xaccTransGetVoidReason (trans)); // This is the Void Reason
                editable = FALSE;
            }
            if (is_split)
            {
                if (!is_template) // Are we using a template
                {
                    Account *acct = xaccSplitGetAccount (split);

                    // This will be all but the General Journal which has anchor == NULL
                    if ((xaccTransCountSplits (trans) == 0) && (anchor != NULL)) // First split on blank transaction
                        acct = anchor;

                    if (acct != NULL)
                    {
                        if (view->priv->acct_short_names)
                            string = g_strdup (xaccAccountGetName (acct));
                        else
                            string = gnc_account_get_full_name (acct);

                    }
                    else
                        string = g_strdup (" ");

                    if (anchor == acct && model->type != GENERAL_JOURNAL2 && model->type != SEARCH_LEDGER2)
                        editable = FALSE;
                    else
                        editable = TRUE;
                }
                else
                {
                    string = g_strdup (gnc_tree_util_split_reg_template_get_transfer_entry (split));
                    editable = TRUE;
                }
            }
            editable = (read_only == TRUE) ? FALSE : editable;

            g_object_set (cell, "text", string, "editable", editable, NULL);
            g_free (string);
        }
        break;

    case COL_RECN:
        /* Column is RECN */
        /* Override default alignment */
        g_object_set( cell, "xalign", 0.5, NULL );
        editable = FALSE;
        s = "";
        if (is_trow1 && !expanded)
        {
            Split *this_split;
            char rec;

            this_split = gtv_sr_get_this_split (view, trans);

            if (this_split != NULL) // this could be a blank trans
            {
                rec = xaccSplitGetReconcile (this_split);
                if (rec == VREC || rec == FREC)
                    editable = FALSE;
                else
                    editable = TRUE;

                if (rec != ' ')
                    s = gnc_get_reconcile_str (rec);
                else
                    s = gnc_get_reconcile_str (NREC);
            }
        }

        if (is_split)
        {
            char rec = xaccSplitGetReconcile (split);
            if (rec == VREC || rec == FREC)
                editable = FALSE;
            else
                editable = TRUE;

            if (rec != ' ')
                s = gnc_get_reconcile_str (rec);
            else
                s = gnc_get_reconcile_str (NREC);
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_TYPE:
        /* Column is TYPE */
        /* Override default alignment */
        g_object_set( cell, "xalign", 0.5, NULL );
        if (is_split)
            g_object_set (cell, "cell-background", "white", (gchar*)NULL);

        if (is_trow1) {
            static char ss[2];
            if (type == TXN_TYPE_NONE)
                type = '?';

            ss[0] = type;
            ss[1] = '\0';
            editable = TRUE;
            g_object_set (cell, "text", ss, NULL);
        }
        else
        {
            s = "";
            editable = FALSE;
            g_object_set (cell, "text", s, NULL);
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "editable", editable, NULL);
        break;

    case COL_VALUE:
        /* Column is VALUE */
        if (is_split)
        {
            num = xaccSplitGetValue (split);
            s = xaccPrintAmount (num, gnc_commodity_print_info (xaccTransGetCurrency (trans), SHOW_SYMBOL));
            editable = FALSE;

            if (gtv_sr_get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }
        else
        {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        // Display negative numbers in red if requested in preferences
        if (gnc_numeric_negative_p (num) && negative_in_red)
            g_object_set (cell, "foreground", "red", (gchar*)NULL);
        else
            g_object_set (cell, "foreground", NULL, (gchar*)NULL);

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
            GNCPrintAmountInfo print_info;

            gnc_commodity *split_com = xaccAccountGetCommodity (xaccSplitGetAccount (split));

            print_info = gnc_default_price_print_info();
            print_info.min_decimal_places = 2;

            num = gnc_numeric_convert (gnc_tree_util_get_rate_for (view, trans, split, is_blank), 1000000, GNC_HOW_RND_ROUND_HALF_UP);

            if (gnc_numeric_check (num) == GNC_ERROR_OK)
                s = xaccPrintAmount (num, print_info);
            else
                s = "";

            editable = FALSE;

            if (gtv_sr_get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_AMOUNT:
        /* Column is AMOUNT */
        if (is_split && (anchor == NULL))
        {
            num = xaccSplitGetAmount (split);
            s = xaccPrintAmount (num, gnc_account_print_info (xaccSplitGetAccount (split), SHOW_SYMBOL));
            editable = FALSE;

            if (gtv_sr_get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }
        else if (is_split && (anchor))
        {
            gnc_commodity *split_comm;
            split_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));

            if (!gnc_commodity_is_currency (split_comm) || (is_blank))
            {
                num = xaccSplitGetAmount (split);
                s = xaccPrintAmount (num, gnc_account_print_info (xaccSplitGetAccount (split), SHOW_SYMBOL));
                editable = TRUE;
            }

            if (gtv_sr_get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }
        else
        {
            s = "";
            editable = FALSE;
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        // Display negative numbers in red if requested in preferences
        if (gnc_numeric_negative_p (num) && negative_in_red)
            g_object_set (cell, "foreground", "red", (gchar*)NULL);
        else
            g_object_set (cell, "foreground", NULL, (gchar*)NULL);

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_AMTVAL:
        /* Column is AMOUNT / VALUE */
        if (is_trow2)
        {
            s = "";
            editable = FALSE;
        }
        else if (is_trow1) // Value
        {
            if (anchor)
            {
                Split *this_split;

                this_split = gtv_sr_get_this_split (view, trans);

                num = xaccTransGetAccountValue (trans, anchor);

                editable = !expanded && !gnc_tree_util_split_reg_is_multi (this_split);

                if (expanded)
                    s = "";
                else
                    s = xaccPrintAmount (num, gnc_commodity_print_info (xaccTransGetCurrency (trans), SHOW_SYMBOL));
            }
            else
            {
                s = "";
                editable = FALSE;
            }
        }

        if (is_split) // Amount
        {
            if (anchor == NULL)
            {
                num = xaccSplitGetAmount (split);
                s = xaccPrintAmount (num, gnc_account_print_info (xaccSplitGetAccount (split), SHOW_SYMBOL));
                editable = TRUE;
            }
            else if (anchor)
            {
                gnc_commodity *split_comm;
                split_comm = xaccAccountGetCommodity (xaccSplitGetAccount (split));

                if (!gnc_commodity_is_currency (split_comm) || (is_blank))
                {
                    num = xaccSplitGetAmount (split);
                    s = xaccPrintAmount (num, gnc_account_print_info (xaccSplitGetAccount (split), SHOW_SYMBOL));
                    editable = TRUE;
                }
            }
            else
            {
                s = "";
                editable = FALSE;
            }

            if (gtv_sr_get_imbalance (trans))
                g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
        }

        /* Only allow changes to entries if we have a valid split accounts */
        editable = gtv_sr_have_account (view, depth, expanded, is_template, trans, split);

        editable = (read_only == TRUE) ? FALSE : editable;

        // Display negative numbers in red if requested in preferences
        if (gnc_numeric_negative_p (num) && negative_in_red)
            g_object_set (cell, "foreground", "red", (gchar*)NULL);
        else
            g_object_set (cell, "foreground", NULL, (gchar*)NULL);

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
            if (expanded)
            {
                s = "";
                editable = FALSE;
            }
            else
            {
                if (anchor)
                {
                    Split *this_split;

                    this_split = gtv_sr_get_this_split (view, trans);
                    if (this_split != NULL) // this could be a blank split
                    {
                        if (gnc_tree_util_split_reg_is_multi (this_split))
                            num = gnc_numeric_zero();
                        else
                            num = xaccSplitGetSharePrice (this_split);

                        editable = !expanded && !gnc_tree_util_split_reg_is_multi (this_split);

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
                    else
                    {
                        s = "";
                        editable = FALSE;
                    }
                }
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

            if (gtv_sr_get_imbalance (trans))
                g_object_set(cell, "cell-background", PINKCELL, (gchar*)NULL);
        }

        /* Only allow changes to entries if we have a valid split accounts */
        editable = gtv_sr_have_account (view, depth, expanded, is_template, trans, split);

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_DEBIT:
    case COL_CREDIT:
        /* Column is CREDIT and DEBIT */
        {
            if (!is_template) // Is this a template
            {
                GNCPrintAmountInfo print_info;
                print_info = gnc_account_print_info (anchor, SHOW_SYMBOL);

                if (is_split)
                {
                    if (!gnc_tree_util_split_reg_get_debcred_entry (view, trans, split, is_blank, &num, &print_info))
                        num = gnc_numeric_zero();

                    editable = TRUE;
                    if (gtv_sr_get_imbalance (trans))
                        g_object_set (cell, "cell-background", PINKCELL, (gchar*)NULL);
                }
                else if (is_trow1)
                {
                    if (anchor)
                    {
                         editable = !expanded && !gnc_tree_util_split_reg_is_multi (gtv_sr_get_this_split (view, trans));
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
                        s = xaccPrintAmount (gnc_numeric_abs (num), print_info);
                }
            }
            else
            {
                editable = TRUE;

                if (is_trow1 || is_trow2)
                {
                    s = "";
                    editable = FALSE;
                }
                else if (is_split && viewcol == COL_DEBIT)
                    s = gnc_tree_util_split_reg_template_get_fdebt_entry (split);
                else
                    s = gnc_tree_util_split_reg_template_get_fcred_entry (split);
            }

            /* Only allow changes to entries if we have a valid split accounts */
            editable = gtv_sr_have_account (view, depth, expanded, is_template, trans, split);
        }

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

            // Display negative numbers in red if requested in preferences
            if (gnc_numeric_negative_p (num) && negative_in_red)
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
            g_object_set(cell, "cell-background", ORANGECELL, (gchar*)NULL);
        else if (xaccTransInFutureByPostedDate (trans))
            g_object_set(cell, "cell-background", BLUECELL, (gchar*)NULL);
        else
            g_object_set(cell, "cell-background", BLACKCELL, (gchar*)NULL);
        break;

    case COL_COMM:
        /* Column COMMODITY */
        {
            gchar *string = NULL;
            if (is_split)
            {
                gnc_commodity *split_com, *txn_com;

                split_com = xaccAccountGetCommodity (xaccSplitGetAccount(split));
                txn_com = xaccTransGetCurrency (trans);
                if (split_com == txn_com)
                   string = g_strconcat (gnc_commodity_get_printname (split_com), "*", NULL);
                else
                   string = g_strdup (gnc_commodity_get_printname (split_com));
            }
            else
                string = g_strdup ("");

            g_object_set (cell, "text", string, "editable", FALSE, NULL);
            g_free (string);
        }
        break;

    default:
        break;
    }
    LEAVE("");
}


/* Instead of setting a different cellDataFunc for each column, we just
   collect everything here for the second cell renderer. */
static void
gtv_sr_cdf1 (GtkTreeViewColumn *col, GtkCellRenderer *cell, GtkTreeModel *s_model,
    GtkTreeIter *s_iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeIter m_iter;
    GtkTreePath *spath;
    ViewCol viewcol;
    Transaction *trans;
    Split *split;
    gboolean is_split, is_blank, is_trow1, is_trow2;
    gboolean editable = FALSE, expanded = FALSE;
    gboolean read_only = FALSE;
    gboolean open_edited = FALSE;
    gnc_numeric num;
    const gchar *s = "";
    const gchar *row_color;
    RowDepth depth;
    gint *indices;
    Account *anchor = view->priv->anchor;
    char type;

    ENTER("");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model), &m_iter, s_iter);

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    g_return_if_fail (gnc_tree_model_split_reg_get_split_and_trans (
                         GNC_TREE_MODEL_SPLIT_REG (model), &m_iter,
                          &is_trow1, &is_trow2, &is_split, &is_blank,
                          &split, &trans));

    spath = gtk_tree_model_get_path (GTK_TREE_MODEL (s_model), s_iter);

    depth = gtk_tree_path_get_depth (spath);

    indices = gtk_tree_path_get_indices (spath);

    row_color = gnc_tree_model_split_reg_get_row_color (model, is_trow1, is_trow2, is_split, indices[0]);

    /* Lets see if the splits are expanded */
    if (is_trow1 || is_trow2) // transaction
    {
        if (is_trow1)
            gtk_tree_path_down (spath); /* Move the path down to trow2 */
        expanded = gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), spath);
    }
    else
        expanded = TRUE; // splits are always expanded

    gtk_tree_path_free (spath);

    /* Set the background color / this works for sorting and deleting of transactions */
    g_object_set (cell, "cell-background", row_color, (gchar*)NULL);

    /* Get the read only model setting */
    gtk_tree_model_get (GTK_TREE_MODEL (model), &m_iter, GNC_TREE_MODEL_SPLIT_REG_COL_RO, &read_only, -1);

    /* Are we being edited in other register */
    if (xaccTransIsOpen (trans) && (view->priv->dirty_trans != trans))
    {
        read_only = TRUE;
        open_edited = TRUE;
    }

    /* Test for a transaction type of invoice, always read only */
    type = xaccTransGetTxnType (trans);
    if (model->type == RECEIVABLE_REGISTER2 || model->type == PAYABLE_REGISTER2)
    {
        if (((type == TXN_TYPE_INVOICE) || (type == TXN_TYPE_NONE)) && (view->priv->dirty_trans != trans) && !is_blank)
            read_only = TRUE;
    }

    switch (viewcol) {
    case COL_DATE:
        /* Column is DATE */
        break;

    case COL_DUEDATE:
        /* Column is DUE DATE */
        break;

    case COL_NUMACT:
        /* Column is NUM / ACT  but relates to NUM */
        /* Override default alignment */
        g_object_set (cell, "xalign", 0.0, NULL );

        editable = TRUE;

        if (is_trow1)
        {
            /* Get per book option */
            s = gnc_get_num_action (trans, gtv_sr_get_this_split (view, trans));
        }
        else if (is_trow2 && expanded)
        {
            /* Get per book option */
            if (qof_book_use_split_action_for_num_field (gnc_get_current_book()))
                s = gnc_get_action_num (trans, gtv_sr_get_this_split (view, trans));
            else
                s = "";
            editable = FALSE;
        }
        else if (is_trow2 && !expanded)
        {
            /* Get per book option */
            if (qof_book_use_split_action_for_num_field (gnc_get_current_book()))
            {
               if (gtv_sr_get_this_split (view, trans) != NULL) // Blank split of blank trans is not child of trans yet.
                   s = gnc_get_action_num (trans, gtv_sr_get_this_split (view, trans));
               else
                   s = "";
            }
            else
            {
                s = "XY";
            }
        }
        else if (is_split)
        {
            s = "XZ";
        }

        editable = (read_only == TRUE) ? FALSE : editable;

        g_object_set (cell, "text", s, "editable", editable, NULL);
        break;

    case COL_DESCNOTES:
        /* Column is DESCRIPTION / NOTES */
        break;

    case COL_TRANSFERVOID:
        /* Column is TRANSFER / VOID */
        break;

    case COL_RECN:
        /* Column is RECN */
        break;

    case COL_TYPE:
        /* Column is TYPE */
        break;

    case COL_VALUE:
        /* Column is VALUE */
        break;

    case COL_RATE:
        /* Column is RATE */
        break;

    case COL_AMOUNT:
        /* Column is AMOUNT */
        break;

    case COL_AMTVAL:
        /* Column is AMOUNT / VALUE */
        break;

    case COL_PRICE:
        /* Column is PRICE */
        break;

    case COL_DEBIT:
    case COL_CREDIT:
        /* Column is CREDIT and DEBIT */
        break;

    case COL_BALANCE:
        /* Column is BALANCE */
        break;

    case COL_STATUS:
        /* Column is STATUS */
        break;

    case COL_COMM:
        /* Column COMMODITY */
        break;

    default:
        break;
    }
    LEAVE("");
}


/*###########################################################################*/

/* Returns TRUE if dialog was canceled or discarded.
   Does nothing if 'new_trans' is the dirty trans. */
static gboolean
gtv_sr_transaction_changed_confirm (GncTreeViewSplitReg *view,
                            Transaction *new_trans)
{
    GtkWidget            *dialog, *window;
    GncTreeModelSplitReg *model;
    Split                *split;
    gint response;
    const char *title = _("Save the changed transaction?");
    const char *message = _(
        "The current transaction has changed. Would you like to "
        "record the changes, or discard the changes?");

    // Look for dirty_trans not being new_trans.
    if (!view->priv->dirty_trans || view->priv->dirty_trans == new_trans)
        return FALSE;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    // If using trading accounts, lets scrub them to make them work.
    if (xaccTransUseTradingAccounts (view->priv->dirty_trans))
    {
        Account *default_account = gnc_tree_model_split_reg_get_anchor (model);
        if (default_account != NULL)
            xaccTransScrubImbalance (view->priv->dirty_trans, gnc_account_get_root(default_account), NULL);
        else
        {
            Account *root = gnc_book_get_root_account (gnc_get_current_book());
            xaccTransScrubImbalance (view->priv->dirty_trans, root, NULL);
        }
    }

    // Test if the transaction is balanced.
    if (gnc_tree_control_split_reg_balance_trans (view, view->priv->dirty_trans))
    {
        view->priv->trans_confirm = CANCEL;
        return TRUE;
    }

    window = gnc_tree_view_split_reg_get_parent (view);
    dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_QUESTION,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                             "%s", message);

    gtk_dialog_add_buttons (GTK_DIALOG(dialog),_("_Discard Changes"), GTK_RESPONSE_REJECT,
                            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                            _("_Record Changes"), GTK_RESPONSE_ACCEPT, NULL);

    response = gnc_dialog_run (GTK_DIALOG (dialog), GNC_PREF_WARN_REG_TRANS_MOD);
    gtk_widget_destroy (dialog);

    switch (response)
    {
    case GTK_RESPONSE_ACCEPT:
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
        xaccTransCommitEdit (view->priv->dirty_trans);
        split = gnc_tree_model_split_get_blank_split (model);
        xaccSplitReinit (split); // Clear the blank split
        view->priv->dirty_trans = NULL;
        view->change_allowed = FALSE;
        view->priv->auto_complete = FALSE;
        view->priv->trans_confirm = ACCEPT;
        return FALSE;
        break;

    case GTK_RESPONSE_REJECT:
        if (view->priv->dirty_trans && xaccTransIsOpen (view->priv->dirty_trans))
        {
            // Move selection to trans - selection is blocked
            gnc_tree_control_split_reg_goto_rel_trans_row (view, 0);

            g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
            xaccTransRollbackEdit (view->priv->dirty_trans);
            split = gnc_tree_model_split_get_blank_split (model);
            xaccSplitReinit (split); // Clear the blank split
            view->change_allowed = FALSE;
            view->priv->auto_complete = FALSE;
            view->priv->trans_confirm = DISCARD;
        }
        return TRUE;
        break;

    case GTK_RESPONSE_CANCEL:
        view->priv->trans_confirm = CANCEL;
        return TRUE;
        break;

    default:
        return FALSE;
    }
    return FALSE;
}


/*###########################################################################
             vvvvv    edit function call backs      vvvvvv
#############################################################################*/
static void
start_edit (GtkCellRenderer *cr, GtkCellEditable *editable,
           const gchar *path_string, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreePath         *path;
//g_print("\n\nstart_edit\n");
/*FIXME Not sure if this is required, leave for now ? */

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    gtv_sr_editable_start_editing_cb (cr, editable, path_string, user_data);
/*    g_signal_connect(G_OBJECT(editable), "editing-done", (GCallback) editing_done_cb, view); */

//FIXME this could be the sort path instead of model path / check !!
    path = gtk_tree_path_new_from_string (path_string);

//FIXME stuff here...

    gtk_tree_path_free (path);

    return;
}


/* Open Transaction for editing */
static void
gtv_sr_begin_edit (GncTreeViewSplitReg *view, Transaction *trans)
{
    ENTER("gtv_sr_begin_edit trans %p", trans);

    if (trans != view->priv->dirty_trans)
    {
        Timespec ts = {0,0};
        xaccTransGetDatePostedTS (trans, &ts);

        if (!xaccTransIsOpen (trans))
            xaccTransBeginEdit (trans);
        view->priv->dirty_trans = trans;

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
gtv_sr_remove_edit_date (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncPopupEntry *popup_entry;
    const gchar *new_string; 
    const gchar *current_string;
    GDate date;
    gchar *date_string;

    ENTER("remove edit date and temp cell rend %p", view->priv->temp_cr);

    if (view->priv->temp_cr != NULL)
    {
        // These strings are used to determine if cell data was altered so that keynav works better
        popup_entry = GNC_POPUP_ENTRY (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable"));

        new_string = gtk_entry_get_text (GTK_ENTRY (popup_entry->entry));

        current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");

        DEBUG("New string is %s and Current_string is %s", new_string, current_string);

        // If editing wasn't canceled and strings don't match then cell data was edited
        if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "edit-canceled"))
             && g_ascii_strcasecmp (new_string, current_string))
        {
            g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
        }

        /* Lets update the help text */
        gnc_tree_util_split_reg_parse_date (&date, new_string);
        date_string = gnc_tree_util_split_reg_get_date_help (&date);

        if (view->help_text)
            g_free (view->help_text);
        view->help_text = g_strdup (date_string);

        g_signal_emit_by_name (view, "help_signal", NULL);
        g_free (date_string);

        g_object_set_data (G_OBJECT (view->priv->temp_cr), "cell-editable", NULL);
        view->priv->temp_cr = NULL;
        view->editing_now = FALSE;
    }
    LEAVE(" ");
}


/* Call back to remove combo widget */
static void
gtv_sr_remove_edit_combo (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkEntry *entry; 
    const gchar *new_string; 
    const gchar *current_string;

    ENTER("remove edit combo and temp cell rend %p", view->priv->temp_cr);

    if (view->priv->temp_cr != NULL)
    {
        // These strings are used to determine if cell data was altered so that keynav works better
        entry = GTK_ENTRY (gtk_bin_get_child (GTK_BIN (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable"))));

        new_string = gtk_entry_get_text (GTK_ENTRY (entry));

        current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");

        DEBUG("New string is %s and Current_string is %s", new_string, current_string);

        // If editing wasn't canceled and strings don't match then cell data was edited
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
gtv_sr_remove_edit_entry (GtkCellEditable *ce, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    const gchar *new_string; 
    const gchar *current_string; 

    ENTER("remove edit entry and temp cell rend %p", view->priv->temp_cr);

    if (view->priv->temp_cr != NULL)
    {
        // These strings are used to determine if cell data was altered so that keynav works better
        new_string = gtk_entry_get_text (GTK_ENTRY (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable")));

        current_string = g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string");

        DEBUG("New string is %s and Current_string is %s", new_string, current_string);

        // If editing wasn't canceled and strings don't match then cell data was edited
        if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "edit-canceled"))
             && g_ascii_strcasecmp (new_string, current_string))
        {
            g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
        }
        if (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-flag") != NULL) // flag
            g_object_set_data (G_OBJECT (view->priv->temp_cr), "current-flag", NULL);

        g_object_set_data (G_OBJECT (view->priv->temp_cr), "cell-editable", NULL);
        view->priv->temp_cr = NULL;
        view->editing_now = FALSE;
    }
    LEAVE(" ");
}


/* Explain: GtkEntry has a cursor that blinks upon
   g_timeout_dispatch(). It complains if it blinks after the GtkEntry
   loses focus. So, we can't pop up any dialogs while the blinking
   cursor is around. The solution is to force the editing to be
   finished before raising the dialog. That finalizes the
   gtkcelleditable. */
static void
gtv_sr_finish_edit (GncTreeViewSplitReg *view)
{
    GtkCellEditable *ce;

    if (view->priv->temp_cr == NULL)
        return;

    DEBUG("gtv_sr_finish_edit temp_cr is %p", view->priv->temp_cr);

    if ((ce = GTK_CELL_EDITABLE (g_object_get_data (G_OBJECT (view->priv->temp_cr), "cell-editable"))))
    {
        DEBUG("gtv_sr_finish_edit - editing_done");
        gtk_cell_editable_editing_done (ce);
        gtk_cell_editable_remove_widget (ce);
    }
}


/* This is used in g_idle_add to finish an edit */
static gboolean
gtv_sr_idle_finish_edit (GncTreeViewSplitReg *view)
{
   gtv_sr_finish_edit (view);
   return FALSE;
}


/* This is used in g_idle_add to cancel an edit */
static gboolean
gtv_sr_idle_cancel_edit (GtkCellRenderer *cr)
{
    GtkCellEditable *ce;

    gtk_cell_renderer_stop_editing (cr, TRUE);

    ce = GTK_CELL_EDITABLE (g_object_get_data (G_OBJECT (cr), "cell-editable"));
    gtk_cell_editable_editing_done (ce);
    gtk_cell_editable_remove_widget (ce);

   return FALSE;
}

/* This is used in g_idle_add to repopulate the transfer cell */
static gboolean
gtv_sr_idle_transfer (GncTreeViewSplitReg *view)
{
    GtkTreePath *spath;
    GList *columns;
    GList  *column;
    gint i;

    spath = gnc_tree_view_split_reg_get_current_path (view);
    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (view));

    for (column = columns, i = 1; column; column = g_list_next (column), i++)
    {
        GList *renderers;
        GtkCellRenderer *cr0;
        GtkTreeViewColumn *tvc;
        ViewCol viewcol;

        tvc = column->data;

        // Get the first renderer, it has the view-column value.
        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (tvc));
        cr0 = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr0), "view_column"));

        if (viewcol == COL_TRANSFERVOID)
            gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, tvc, TRUE);
    }
    g_list_free (columns);
    gtk_tree_path_free (spath);
    return FALSE;
}

/*###########################################################################*/

/* Set the column titles based on register type and depth */
static void
gtv_sr_titles (GncTreeViewSplitReg *view, RowDepth depth)
{
    GncTreeModelSplitReg *model;
    GtkCellRenderer *cr0;
    GList *renderers;
    GList *columns;
    GList  *column;
    gint i;
    RowDepth temp_depth;
    gboolean is_template;

    model = gnc_tree_view_split_reg_get_model_from_view (view);
    ENTER("title depth is %d and sort_depth %d, sort_col is %d", depth, model->sort_depth, model->sort_col);

    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (view));

    is_template = gnc_tree_model_split_reg_get_template (model);

    for (column = columns, i = 1; column; column = g_list_next (column), i++)
    {
        GtkTreeViewColumn *tvc;
        ViewCol viewcol;

        tvc = column->data;

        // Get the first renderer, it has the view-column value.
        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (tvc));
        cr0 = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr0), "view_column"));

        DEBUG("viewcol is %d", viewcol);

        switch (viewcol)
        {
        case COL_DATE:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                /* Display arrows if we are sorting on this row */
                if (model->sort_depth == depth && model->sort_col == viewcol)
                    gtk_tree_view_column_set_sort_indicator (tvc, TRUE);
                else
                    gtk_tree_view_column_set_sort_indicator (tvc, FALSE);

                if (depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Date Posted"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Date Entered"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Date Reconciled"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Date Posted / Entered / Reconciled"));
                break;
            }
            break;

        case COL_DUEDATE:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Due Date"));
                break;
            }
            break;

        case COL_NUMACT:
            switch (model->type)
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
                if (model->sort_depth == depth && model->sort_col == viewcol)
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
            switch (model->type)
            {
            case RECEIVABLE_REGISTER2:
                if (depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Customer"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Memo"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Customer / Memo"));
                break;

            case PAYABLE_REGISTER2:
                if (depth == TRANS1)
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
                if (model->sort_depth == depth && model->sort_col == viewcol)
                    gtk_tree_view_column_set_sort_indicator (tvc, TRUE);
                else
                    gtk_tree_view_column_set_sort_indicator (tvc, FALSE);

                if (depth == TRANS1)
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

        case COL_TRANSFERVOID:
            switch (model->type)
            {
            case RECEIVABLE_REGISTER2:
            case PAYABLE_REGISTER2:
                if (depth == TRANS1)
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                else if (depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Accounts"));
                break;

            default:
                /* Display arrows if we are sorting on this row */
                if (model->sort_depth == depth && model->sort_col == viewcol)
                    gtk_tree_view_column_set_sort_indicator (tvc, TRUE);
                else
                    gtk_tree_view_column_set_sort_indicator (tvc, FALSE);

                if (depth == TRANS1)
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
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("R"));
                break;
            }
            break;

        case COL_TYPE:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Type"));
                break;
            }
            break;

        case COL_VALUE:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Value"));
                break;
            }
            break;

        case COL_AMOUNT:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Amount"));
                break;
            }
            break;

        case COL_AMTVAL:
            switch (model->type)
            {
            default:
                if (depth == TRANS1 || depth == TRANS2)
                    gtk_tree_view_column_set_title (tvc, _("Value"));
                else if (depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Amount"));
                else
                    gtk_tree_view_column_set_title (tvc, _("Amount / Value"));
                break;
            }
            break;

        case COL_COMM:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Commodity"));
                break;
            }
            break;

        case COL_RATE:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Rate"));
                break;
            }
            break;

        case COL_PRICE:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                    gtk_tree_view_column_set_title (tvc, _("Price"));
                break;
            }
            break;

        case COL_CREDIT:
            if(!(model->use_accounting_labels))
            {
                switch (model->type)
                {
                case BANK_REGISTER2: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Withdrawal"));
                    break;

                case CASH_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Spend"));
                    break;

                case ASSET_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Decrease"));
                    break;

                case LIABILITY_REGISTER2:
                case EQUITY_REGISTER2:
                case TRADING_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Increase"));
                    break;

                case CREDIT_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Charge"));
                    break;

                case INCOME_REGISTER2:
                case INCOME_LEDGER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Income"));
                    break;

                case EXPENSE_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Rebate"));
                    break;

                case STOCK_REGISTER2:
                case CURRENCY_REGISTER2:
                case PORTFOLIO_LEDGER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Sell"));
                    break;

                case RECEIVABLE_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Payment"));
                    break;

                case PAYABLE_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Bill"));
                    break;

                case GENERAL_JOURNAL2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Funds Out"));
                    break;

                case SEARCH_LEDGER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                {
                    if (!is_template)
                        gtk_tree_view_column_set_title (tvc, _("Funds Out"));
                    else
                        gtk_tree_view_column_set_title (tvc, _("Credit Formula"));
                }
                    break;

                default:
                    if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
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
                switch (model->type)
                {
                case BANK_REGISTER2: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Deposit"));
                    break;

                case CASH_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Receive"));
                    break;

                case ASSET_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Increase"));
                    break;

                case LIABILITY_REGISTER2:
                case EQUITY_REGISTER2:
                case TRADING_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Decrease"));
                    break;

                case INCOME_REGISTER2:
                case INCOME_LEDGER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Charge"));
                    break;

                case EXPENSE_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Expense"));
                    break;

                case STOCK_REGISTER2:
                case CURRENCY_REGISTER2:
                case PORTFOLIO_LEDGER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Buy"));
                    break;

                case RECEIVABLE_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Invoice"));
                    break;

                case CREDIT_REGISTER2:
                case PAYABLE_REGISTER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Payment"));
                    break;

                case GENERAL_JOURNAL2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Funds In"));
                    break;

                case SEARCH_LEDGER2:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                {
                    if (!is_template)
                        gtk_tree_view_column_set_title (tvc, _("Funds In"));
                    else
                        gtk_tree_view_column_set_title (tvc, _("Debit Formula"));
                }
                    break;

                default:
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                        gtk_tree_view_column_set_title (tvc, _("Debit"));
                    break;
                }
            }
            else
                gtk_tree_view_column_set_title (tvc, _("Debit"));
            break;

        case COL_BALANCE:
            switch (model->type)
            {
            default: //FIXME These if statements may not be required
                if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
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
gtv_sr_help (GncTreeViewSplitReg *view, GtkCellRenderer *cr, ViewCol viewcol, RowDepth depth)
{
    GncTreeModelSplitReg *model;
    gchar *help = NULL;
    const gchar *current_string;

    ENTER("Help Viewcol is %d and depth is %d", viewcol, depth);

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    switch (viewcol)
    {
    case COL_DATE:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1)
            {
                GDate date;

                current_string = g_object_get_data (G_OBJECT (cr), "current-string");
                g_date_set_parse (&date, current_string);
                help = gnc_tree_util_split_reg_get_date_help (&date);
            }
            else
                help = g_strdup (" ");
            break;
        }
        break;

    case COL_DUEDATE:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter Due Date"));
            break;
        }
        break;

    case COL_NUMACT:
        switch (model->type)
        {
        case RECEIVABLE_REGISTER2:
        case PAYABLE_REGISTER2:
            if (depth == TRANS1)
                help = g_strdup (_("Enter the transaction reference, such as the invoice or check number"));
            else if (depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter the type of transaction, or choose one from the list"));
            break;

        default:
            if (depth == TRANS1)
                help = g_strdup (_("Enter the transaction number, such as the check number"));
            else if (depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter the type of transaction, or choose one from the list"));
            break;
        }
        break;

    case COL_DESCNOTES:
        switch (model->type)
        {
        case RECEIVABLE_REGISTER2:
            if (depth == TRANS1)
                help = g_strdup (_("Enter the name of the Customer"));
            else if (depth == TRANS2)
                help = g_strdup (_("Enter notes for the transaction"));
            else if (depth == SPLIT3)
                help = g_strdup (_("Enter a description of the split"));
            break;

        case PAYABLE_REGISTER2:
            if (depth == TRANS1)
                help = g_strdup (_("Enter the name of the Vendor"));
            else if (depth == TRANS2)
                help = g_strdup (_("Enter notes for the transaction"));
            else if (depth == SPLIT3)
                help = g_strdup (_("Enter a description of the split"));
            break;

        default:
            if (depth == TRANS1)
                help = g_strdup (_("Enter a description of the transaction"));
            else if (depth == TRANS2)
                help = g_strdup (_("Enter notes for the transaction"));
            else if (depth == SPLIT3)
                help = g_strdup (_("Enter a description of the split"));
            break;
        }
        break;

    case COL_TRANSFERVOID:
        switch (model->type)
        {
        default:
            if (depth == TRANS1)
                help = g_strdup (_("Enter the account to transfer from, or choose one from the list"));
            else if (depth == TRANS2)
                help = g_strdup (_("Reason the transaction was voided"));
            else if (depth == SPLIT3)
                help = g_strdup (_("Enter the account to transfer from, or choose one from the list"));
            break;
        }
        break;

    case COL_RECN:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter the reconcile type"));
            break;
        }
        break;

    case COL_TYPE:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter the type of transaction"));
            break;
        }
        break;

    case COL_VALUE:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter the value of shares bought or sold"));
            break;
        }
        break;

    case COL_AMOUNT:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter the number of shares bought or sold"));
            break;
        }
        break;

    case COL_AMTVAL:
        switch (model->type)
        {
        default:
            if ((depth == TRANS1) || (depth == TRANS2))
                help = g_strdup (_("Enter the value of shares bought or sold"));
            else if (depth == SPLIT3)
                help = g_strdup (_("Enter the number of shares bought or sold"));
            break;
        }
        break;

    case COL_COMM:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("* Indicates the transaction Commodity."));
            break;
        }
        break;

    case COL_RATE:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter the rate"));
            break;
        }
        break;

    case COL_PRICE:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter the effective share price"));
            break;
        }
        break;

    case COL_CREDIT:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter credit formula for real transaction"));
            break;
        }
        break;

    case COL_DEBIT:
        switch (model->type)
        {
        default: //FIXME These if statements may not be required
            if (depth == TRANS1 || depth == TRANS2 || depth == SPLIT3)
                help = g_strdup (_("Enter debit formula for real transaction"));
            break;
        }
        break;

    default:
            help = g_strdup (" ");
        break;
    }

    LEAVE("Help text is - %s", help);
    if (view->help_text)
        g_free (view->help_text);
    view->help_text = g_strdup (help);
    g_free (help);
    g_signal_emit_by_name (view, "help_signal", NULL);
}

/*###########################################################################*/

/* Move the selection to the blank split when expanded */
static gboolean
gtv_sr_selection_to_blank (GncTreeViewSplitReg *view)
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

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    bsplit = gnc_tree_model_split_get_blank_split (model);
    bpath =  gnc_tree_model_split_reg_get_path_to_split_and_trans (model, bsplit, NULL);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, bpath);

    /* Set cursor to new spath */
    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, NULL, FALSE);

    gtk_tree_path_free (bpath);
    gtk_tree_path_free (spath);

    return FALSE;
}


/* Call back for when a change to a Transaction requires the selection to get out of the way */
static void
gtv_sr_selection_move_delete_cb (GncTreeModelSplitReg *model, gpointer item, gpointer user_data)
{
    GncTreeViewSplitReg *view = user_data;
    Transaction *trans = item;

    DEBUG("gtv_sr_selection_move_delete_cb view %p model %p trans %p", view, model, trans);

    DEBUG("gtv_sr_selection_move_delete_cb current_trans %p trans %p", view->priv->current_trans, trans);

    /* if same, lets get out of the way, so move */
    if (trans == view->priv->current_trans)
        gnc_tree_control_split_reg_goto_rel_trans_row (view, 1);

}


/* Call back for focus out event so we can finish edit */
static gboolean
gtv_sr_focus_out_cb (GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

    gnc_tree_view_split_reg_finish_edit (view);

    return FALSE;
}


/* Reconcile column tests */
static gboolean
gtv_sr_recn_tests (GncTreeViewSplitReg *view, GtkTreeViewColumn *column, GtkTreePath *spath)
{
    GtkCellRenderer *cr0;
    GList *renderers;
    ViewCol viewcol;

    ENTER(" ");

    // Get the first renderer, it has the view-column value.
    renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (column));
    cr0 = g_list_nth_data (renderers, 0);
    g_list_free (renderers);

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr0), "view_column"));

    /* Test for change of RECN COLUMN setting from reconciled */
    if (viewcol == COL_RECN)
    {
        /* Are we trying to change the reconcile setting */
        if (!gnc_tree_control_split_reg_recn_change (view, spath))
        {
            LEAVE("Not allowed to change reconciled transaction");
            return TRUE;
        }
    }

    /* Ask, are we allowed to change reconciled values other than 'description / notes / memo'
       which we can change always */
    if (viewcol != COL_DESCNOTES && viewcol != COL_RECN)
    {
        if (!gnc_tree_control_split_reg_recn_test (view, spath))
        {
            LEAVE("Not allowed to edit reconciled transaction");
            return TRUE;
        }
    }
    LEAVE(" ");
    return FALSE;
}


/* Test to see if we need to do a move */
static void
gtv_split_reg_test_for_move (GncTreeModelSplitReg *model, GtkTreePath *spath)
{
    gint num_of_trans, trans_pos;
    gint *indices;

    indices = gtk_tree_path_get_indices (spath);
    num_of_trans = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (model), NULL);

    trans_pos = indices[0];

    if (trans_pos < num_of_trans*1/3)
        gnc_tree_model_split_reg_move (model, VIEW_UP);

    if (trans_pos > num_of_trans*2/3)
        gnc_tree_model_split_reg_move (model, VIEW_DOWN);
}

/*###########################################################################*/

/* This is the callback for the mouse click */
static gboolean
gtv_sr_button_cb (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    GncTreeViewSplitReg  *view = GNC_TREE_VIEW_SPLIT_REG (widget);
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;
    GtkTreeViewColumn    *col;
    ViewCol viewcol;
    GtkTreeIter m_iter;
    Split *split = NULL;
    Split *rotate_split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    /* This is for a single click */
    if (event->button == 1 && event->type == GDK_BUTTON_PRESS)
    {
        GdkWindow *window = gtk_tree_view_get_bin_window (GTK_TREE_VIEW (view));

        if (event->window != window)
            return FALSE;

        // Make sure we have stoped editing.
        gnc_tree_view_split_reg_finish_edit (view);

        // This prevents the cell changing.
        if (view->priv->stop_cell_move == TRUE)
            return TRUE;

        /* Get tree path for row that was clicked, true if row exists */
        if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (view), (gint) event->x, (gint) event->y,
                                             &spath, &col, NULL, NULL))
        {
            DEBUG("event->x is %d and event->y is %d", (gint)event->x, (gint)event->y);

            mpath = gnc_tree_view_split_reg_get_model_path_from_sort_path (view, spath);

            /* This is to block the single click on a double click */
            if (view->priv->single_button_press > 0)
            {
                view->priv->single_button_press = view->priv->single_button_press -1;
                return TRUE;
            }

            if (gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &m_iter, mpath))
            {
                gchar *mstring, *sstring;
                mstring = gtk_tree_path_to_string (mpath);
                sstring = gtk_tree_path_to_string (spath);
                DEBUG("Mouse Button Press - mpath is %s, spath is %s", mstring, sstring);
                g_free (mstring);
                g_free (sstring);

                // Reset the transaction confirm flag.
                view->priv->trans_confirm = RESET;

                gnc_tree_model_split_reg_get_split_and_trans (
                       GNC_TREE_MODEL_SPLIT_REG (model), &m_iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

                // Ask for confirmation if data has been edited, gtv_sr_transaction_changed_confirm return TRUE if canceled
                if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) && gtv_sr_transaction_changed_confirm (view, trans))
                {
                    DEBUG("MB - Restore position - Cancel / Discard");

                    /* Restore position - Cancel / Discard */
                    if (view->priv->trans_confirm == CANCEL)
                    {
                        DEBUG("MB - Cancel");

                        // Expand trans on split-trans (We only expand on cancel and more than two splits)
                        if ((xaccTransCountSplits (view->priv->dirty_trans) > 2) && view->priv->dirty_trans != NULL)
                        {
                            // Jump to the first split of dirty_trans.
                            gnc_tree_control_split_reg_jump_to (view, NULL, xaccTransGetSplit (view->priv->dirty_trans, 0), FALSE);
                        }
                        else
                            // Jump to the dirty_trans.
                            gnc_tree_control_split_reg_jump_to (view, view->priv->dirty_trans, NULL, FALSE);

                        gtk_tree_path_free (spath);
                        gtk_tree_path_free (mpath);
                        return TRUE;
                    }

                    if (view->priv->trans_confirm == DISCARD)
                    {
                        DEBUG("MB - Discard");
                        view->priv->dirty_trans = NULL;
                    }
                }
                /* Skip */

                /* Test for change of transaction */
                if (view->priv->current_trans != trans)
                    /* Reset allow changes for reconciled transactions */
                    view->change_allowed = FALSE;

                // Reconcile tests
                if (gtv_sr_recn_tests (view, col, spath))
                {
                    gtk_tree_path_free (spath);
                    gtk_tree_path_free (mpath);
                    return TRUE;
                }

                // Get the right split for rotate test
                if (is_split)
                    rotate_split = split;
                else
                    rotate_split = gtv_sr_get_this_split (view, trans);

                /* Set cursor to column */
                if (!gnc_tree_util_split_reg_rotate (view, col, trans, rotate_split))
                    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, col, TRUE);
                else
                    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, col, FALSE);

                /* Test to see if we need to do a move */
                gtv_split_reg_test_for_move (model, spath);

                gtk_tree_path_free (spath);
                gtk_tree_path_free (mpath);
                return TRUE;
            }
            gtk_tree_path_free (spath);
            gtk_tree_path_free (mpath);
        }
    }

    /* This is for a double click */
    if (event->button == 1 && event->type == GDK_2BUTTON_PRESS)
    {
        GdkWindow *window = gtk_tree_view_get_bin_window (GTK_TREE_VIEW (view));

        if (event->window != window)
            return FALSE;

        /* this works on non editable cells like void, balance */
        if (model->style != REG2_STYLE_JOURNAL)
        {
            /* This is to block the single click on a double click */
            view->priv->single_button_press = 1;

            if (view->priv->expanded)
                gnc_tree_view_split_reg_collapse_trans (view, NULL);
            else
                gnc_tree_view_split_reg_expand_trans (view, NULL);

            /* This updates the plugin page gui */
            gnc_tree_view_split_reg_call_uiupdate_cb(view);
        }
        return TRUE;
    }
    return FALSE;
}


static gboolean
gtv_sr_transaction_changed (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreeViewColumn *col;
    GtkTreePath *spath;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    // spath is where we are...
    gtk_tree_view_get_cursor (GTK_TREE_VIEW (view), &spath, &col);

    if (!spath)
        return FALSE;

    if (gtv_sr_recn_tests (view, col, spath))
    {
        gtk_tree_path_free (spath);
        return FALSE;
    }
    gtk_tree_path_free (spath);

    // Reset the transaction confirm flag.
    view->priv->trans_confirm = RESET;

    //Ask for confirmation if data has been edited, gtv_sr_transaction_changed_confirm return TRUE if canceled
    if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) && gtv_sr_transaction_changed_confirm (view, NULL))
    {
        /* Restore position - Cancel / Discard */
        DEBUG("KB - Restore position - Cancel / Discard");

        if (view->priv->trans_confirm == CANCEL)
        {
            DEBUG("KB - Cancel");

            // Expand trans on split-trans (We only expand on cancel)
            if ((xaccTransCountSplits (view->priv->dirty_trans) > 2) && view->priv->dirty_trans != NULL)
            {
                // Jump to the first split of dirty_trans.
                gnc_tree_control_split_reg_jump_to (view, NULL, xaccTransGetSplit (view->priv->dirty_trans, 0), FALSE);
            }
            else
                // Jump to the dirty_trans.
                gnc_tree_control_split_reg_jump_to (view, view->priv->dirty_trans, NULL, FALSE);

            return TRUE;
        }

        if (view->priv->trans_confirm == DISCARD)
        {
            DEBUG("KB - Discard");

            gnc_tree_view_split_reg_block_selection (view, TRUE);

            // Check to see if dirty_trans expanded, collapse it.
            if (gnc_tree_view_split_reg_trans_expanded (view, view->priv->dirty_trans))
                gnc_tree_view_split_reg_collapse_trans (view, view->priv->dirty_trans);

            gnc_tree_view_split_reg_block_selection (view, FALSE);

            /* Remove the blank split and re-add - done so we keep it last in list */
            gnc_tree_model_split_reg_set_blank_split_parent (model, view->priv->dirty_trans, TRUE);
            gnc_tree_model_split_reg_set_blank_split_parent (model, view->priv->dirty_trans, FALSE);

            // Set the transaction to show correct view
            gnc_tree_view_split_reg_format_trans (view, view->priv->dirty_trans);
            view->priv->dirty_trans = NULL;
        }
    }
    return FALSE;
}


/* Return whether the cell is in editing mode */
static gboolean
gtv_sr_get_editing (GtkTreeViewColumn *col)
{
    GtkCellRenderer *cr0 = NULL, *cr1 = NULL;
    GList *renderers;
    gboolean cell_editing0 = FALSE;
    gboolean cell_editing1 = FALSE;
    gboolean editing = FALSE;

    renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (col));
    cr0 = g_list_nth_data (renderers, 0); // We always have one renderer
    if (g_list_length (renderers) == 2)
        cr1 = g_list_nth_data (renderers, 1); // There is only one column with two renderers
    g_list_free (renderers);

    if (gtk_cell_renderer_get_visible (cr0))
        g_object_get (G_OBJECT (cr0), "editing", &cell_editing0, NULL);

    if (cr1 && gtk_cell_renderer_get_visible (cr1))
        g_object_get (G_OBJECT (cr1), "editing", &cell_editing1, NULL);

    if (cell_editing0 || cell_editing1)
        editing = TRUE;

    DEBUG("editing is %d for column title %s", editing, gtk_tree_view_column_get_title (col));

    return editing;
}


/* For handling keynav */
static gboolean
gtv_sr_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (widget);
    GncTreeModelSplitReg *model;
    GtkTreeViewColumn *col;
    GtkTreePath *spath, *start_spath;
    GtkTreePath *start_path, *end_path;
    gboolean editing = FALSE;
    gboolean step_off = FALSE;
    gboolean trans_changed = FALSE;
    gint *start_indices;
    gint *next_indices;
    gboolean keyup = FALSE;
    Transaction *btrans, *ctrans, *hetrans;
    gboolean goto_blank = FALSE;
    gboolean next_trans = TRUE;
    gint depth;

    // spath is where we are, before key press...
    gtk_tree_view_get_cursor (GTK_TREE_VIEW (view), &spath, &col);

    if (event->type != GDK_KEY_PRESS)
    {
        if (spath)
            gtk_tree_path_free (spath);
        return FALSE;
    }

    switch (event->keyval)
    {
    case GDK_KEY_plus:
    case GDK_KEY_minus:
    case GDK_KEY_KP_Add:
    case GDK_KEY_KP_Subtract:

        if (!spath)
            return TRUE;

        gtk_tree_path_free (spath);
        return TRUE; //FIXME I may use these to expand / collapse to splits later...
        break;

    case GDK_KEY_Up:
    case GDK_KEY_Down:

        model = gnc_tree_view_split_reg_get_model_from_view (view);

        if (event->keyval == GDK_KEY_Up)
        {
            gnc_tree_model_split_reg_move (model, VIEW_UP);
        }
        else
            gnc_tree_model_split_reg_move (model, VIEW_DOWN);

        return FALSE;
        break;

    case GDK_KEY_Page_Up:
    case GDK_KEY_Page_Down:

        model = gnc_tree_view_split_reg_get_model_from_view (view);

        if (gtk_tree_view_get_visible_range (GTK_TREE_VIEW (view), &start_path, &end_path))
        {
            if (event->keyval == GDK_KEY_Page_Up)
            {
                GtkTreePath *new_start_path;
                gint *start_indices, *end_indices;
                gint new_start;
                gint num_of_trans;

                start_indices = gtk_tree_path_get_indices (start_path);
                end_indices = gtk_tree_path_get_indices (end_path);
                num_of_trans = end_indices[0] - start_indices[0];

                new_start = start_indices[0] - num_of_trans + 2;

                if (new_start < 0)
                    new_start = 0;

                new_start_path = gtk_tree_path_new_from_indices (new_start, -1);

                /* Scroll to cell, top of view */
                gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), new_start_path, NULL, TRUE, 0.0, 0.0);

                /* Set cursor to new top row */
                gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), new_start_path, col, FALSE);

                gtk_tree_path_free (new_start_path);

                gnc_tree_model_split_reg_move (model, VIEW_UP);
            }
            else
            {
                GtkTreePath *new_end_path;
                gint *start_indices, *end_indices;
                gint new_end;
                gint num_of_trans, total_num;

                start_indices = gtk_tree_path_get_indices (start_path);
                end_indices = gtk_tree_path_get_indices (end_path);
                num_of_trans = end_indices[0] - start_indices[0];

                total_num = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (model), NULL);

                new_end = end_indices[0] + num_of_trans - 1;

                if (new_end > (total_num - 1))
                    new_end = total_num -1;

                new_end_path = gtk_tree_path_new_from_indices (new_end, -1);

                /* Scroll to cell, bottom of view */
                if (model->use_double_line == TRUE)
                {
                    gtk_tree_path_down (new_end_path);
                    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), new_end_path, NULL, TRUE, 1.0, 0.0);
                    gtk_tree_path_up (new_end_path);
                }
                else
                    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), new_end_path, NULL, TRUE, 1.0, 0.0);

                /* Set cursor to new bottom row */
                gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), new_end_path, col, FALSE);

                gtk_tree_path_free (new_end_path);

                gnc_tree_model_split_reg_move (model, VIEW_DOWN);
            }
            gtk_tree_path_free (start_path);
            gtk_tree_path_free (end_path);
        }
        return TRUE;
        break;

    case GDK_KEY_Home:
    case GDK_KEY_End:

        model = gnc_tree_view_split_reg_get_model_from_view (view);

        if (event->keyval == GDK_KEY_Home)
            hetrans = gnc_tree_model_split_reg_get_first_trans (model);
        else
            hetrans = gnc_tree_model_split_get_blank_trans (model);

        model->current_trans = hetrans;

        if (!gnc_tree_model_split_reg_trans_is_in_view (model, hetrans))
            g_signal_emit_by_name (model, "refresh_trans");
        else
            gnc_tree_control_split_reg_jump_to (view, hetrans, NULL, FALSE);

        return TRUE;
        break;

    case GDK_KEY_Return:
    case GDK_KEY_space:

        if (!spath)
            return TRUE;

        // Do the reconcile tests.
        if (!gtv_sr_recn_tests (view, col, spath))
        {
            /* Set cursor to new column, open for editing */
            gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, col, TRUE);
        }

        gtk_tree_path_free (spath);
        return TRUE;
        break;

    case GDK_KEY_KP_Enter:

        if (!spath)
            return TRUE;

        goto_blank = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                         GNC_PREF_ENTER_MOVES_TO_END);

        model = gnc_tree_view_split_reg_get_model_from_view (view);
        btrans = gnc_tree_model_split_get_blank_trans (model);
        ctrans = gnc_tree_view_split_reg_get_current_trans (view);

        /* Are we on the blank transaction */
        if (btrans == ctrans)
            next_trans = FALSE;

        /* First record the transaction */
        if (gnc_tree_view_split_reg_enter (view))
        {
            /* Now move. */
            if (goto_blank)
                g_idle_add ((GSourceFunc)gnc_tree_control_split_reg_jump_to_blank, view);
            else if (next_trans)
                gnc_tree_control_split_reg_goto_rel_trans_row (view, 1);
        }
        return TRUE;
        break;

    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
    case GDK_KEY_KP_Tab:

        if (!spath)
            return TRUE;

        // Bypass Auto-complete
        if (event->state & GDK_CONTROL_MASK)
            view->priv->auto_complete = TRUE;

        // Make sure we have stopped editing.
        gnc_tree_view_split_reg_finish_edit (view);

        // This prevents the cell changing.
        if (view->priv->stop_cell_move == TRUE)
        {
            gtk_tree_path_free (spath);
            return TRUE;
        }

        while (!editing && !step_off) // lets step over non editable columns
        {
            // Create a copy of the path we started with.
            start_spath = gtk_tree_path_copy (spath);
            start_indices = gtk_tree_path_get_indices (start_spath);

            {
                gchar *string = gtk_tree_path_to_string (start_spath);
                DEBUG("Column title is %s and start path is %s", gtk_tree_view_column_get_title (col), string);
                g_free (string);
            }

            model = gnc_tree_view_split_reg_get_model_from_view (view);

            /* Step to the next column, we may wrap */
            gnc_tree_view_keynav (GNC_TREE_VIEW (view), &col, spath, event); // returns path and column

            {
                gchar *string = gtk_tree_path_to_string (spath);
                DEBUG("Column title is %s and spath is %s", gtk_tree_view_column_get_title (col), string);
                g_free (string);
            }

            // Have we changed transactions
            next_indices = gtk_tree_path_get_indices (spath);
            if (start_indices[0] != next_indices[0])
            {
                 if (view->priv->dirty_trans != NULL) // from a dirty trans
                    trans_changed = TRUE;

                 /* Reset allow changes for reconciled transctions */
                 view->change_allowed = FALSE;
            }

            // Do the reconcile tests.
            if (gnc_tree_view_path_is_valid (GNC_TREE_VIEW (view), spath))
            {
                if (gtv_sr_recn_tests (view, col, spath))
                {
                    gtk_tree_path_free (start_spath);
                    gtk_tree_path_free (spath);
                    return TRUE;
                }
            }

            // Have we stepped off the end
            if (!spath || !gnc_tree_view_path_is_valid (GNC_TREE_VIEW (view), spath) || trans_changed) // We have stepped off the end / or changed trans
            {
                // Test for transaction changed.
                if (gtv_sr_transaction_changed (view))
                {
                    gtk_tree_path_free (spath);
                    return TRUE;
                }
                step_off = TRUE;
            }
            // This stops the cell activation on discard
            if (view->priv->trans_confirm != DISCARD)
            {
                // Set cursor to new column, open for editing
                gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, col, TRUE);
            }
            // Is this an editable cell ?
            editing = gtv_sr_get_editing (col);
        }
        gtk_tree_path_free (start_spath);
        gtk_tree_path_free (spath);
        return TRUE;
        break;

    default:
        gtk_tree_path_free (spath);
	return FALSE;
    }
}


/*###########################################################################*/

/* Callback for selection move */
static void
gtv_sr_motion_cb (GtkTreeSelection *sel, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeModel *s_model;
    GtkTreePath *mpath, *spath;
    Split *split = NULL;
    Transaction *trans = NULL;
    Transaction *old_trans;
    gboolean is_trow1, is_trow2, is_split, is_blank;
    RowDepth depth = 0;
    GtkTreeIter m_iter;
    gint *indices;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    ENTER("View is %p and Model is %p", view, model);

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));

    DEBUG("Current trans %p, Split %p, Depth %d and Dirty Trans %p", view->priv->current_trans, view->priv->current_split,
                                                                     view->priv->current_depth, view->priv->dirty_trans);

    /* Reset help text */
    if (view->help_text)
        g_free (view->help_text);
    view->help_text = g_strdup (" ");
    g_signal_emit_by_name (view, "help_signal", NULL);

    if (gtv_sr_get_model_iter_from_selection (view, sel, &m_iter))
    {
        gchar *mstring, *sstring;

        mpath = gtk_tree_model_get_path (GTK_TREE_MODEL (model), &m_iter);
        spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

        mstring = gtk_tree_path_to_string (mpath);
        sstring = gtk_tree_path_to_string (spath);
        DEBUG("Valid Selection - mpath is %s, spath is %s", mstring, sstring);
        g_free (mstring);
        g_free (sstring);

        /* save the current path */
        gnc_tree_view_split_reg_set_current_path (view, mpath);

        /* Use depth to determine if it is a split or transaction */
        depth = gtk_tree_path_get_depth (mpath);

        gtk_tree_path_free (mpath);

        gnc_tree_model_split_reg_get_split_and_trans (
                GNC_TREE_MODEL_SPLIT_REG (model), &m_iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

        DEBUG("Get model trans %p, split %p, is_split %d, is_blank %d\n", trans, split, is_split, is_blank);

        /* Update the titles if depth changes, we change rows */
        if (depth != view->priv->current_depth)
            gtv_sr_titles (view, depth);

        /* Move the blank split */ 
        gnc_tree_model_split_reg_set_blank_split_parent (model, trans, FALSE);

        /* Save trans / split / depth to the current values */
        old_trans = view->priv->current_trans;
        view->priv->current_trans = trans;
        view->priv->current_split = split;
        view->priv->current_depth = depth;

        DEBUG("Current trans %p, split %p, depth %d and old_trans %p", view->priv->current_trans, view->priv->current_split,
                                                                     view->priv->current_depth, old_trans);

        /* Save trans and current row to model */
        model->current_trans = trans;
        indices = gtk_tree_path_get_indices (spath);
        model->current_row = indices[0];
        gnc_tree_model_split_reg_sync_scrollbar (model);

        /* Test for change of transaction and old transaction equals a dirty transaction */
        if ((trans != old_trans) && (old_trans == view->priv->dirty_trans))
        {
            if (gtv_sr_transaction_changed (view))
            {
                gtk_tree_path_free (spath);
                LEAVE("Leave Transaction Changed");
                return;
            }
        }
        if (view->priv->trans_confirm == CANCEL)
        {
            gtk_tree_path_free (spath);
            LEAVE("Leave Transaction Changed - Cancel");
            return;
        }

        /* Auto expand transaction and collapse previous transaction */
        if (old_trans != trans)
        {
            if (model->style != REG2_STYLE_JOURNAL)
            {
                gnc_tree_view_split_reg_block_selection (view, TRUE);

                if (gnc_tree_view_split_reg_trans_expanded (view, old_trans))
                    gnc_tree_view_split_reg_collapse_trans (view, old_trans);

                gnc_tree_view_split_reg_block_selection (view, FALSE);
            }
            else
                gnc_tree_view_split_reg_expand_trans (view, NULL);

            if (model->style == REG2_STYLE_AUTO_LEDGER)
            {
                gtk_tree_view_expand_row (GTK_TREE_VIEW (view), spath, TRUE);

                view->priv->expanded = TRUE;

                if (view->priv->selection_to_blank_on_expand)
                    gtv_sr_selection_to_blank (view);
            }
        }
        gtk_tree_path_free (spath);

        // Check to see if current trans is expanded and remember
        if (gnc_tree_view_split_reg_trans_expanded (view, trans))
            view->priv->expanded = TRUE;
        else
            view->priv->expanded = FALSE;
    }
    else
    {
        DEBUG("Not Valid Selection");
        /* We do not have a valid iter */
        gtv_sr_titles (view, 0);

        /* Move the blank split to the last transaction */ 
        gnc_tree_model_split_reg_set_blank_split_parent (model, NULL, FALSE);

        /* Set the default selection start position */
        gnc_tree_view_split_reg_default_selection (view);
    }

    /* This updates the plugin page gui */
    gnc_tree_view_split_reg_call_uiupdate_cb (view);

    LEAVE(" ");
}

/*###########################################################################*/

/* Connected to "edited" from cellrenderer. For reference, see
   split-register-model-save.c */
static void
gtv_sr_edited_cb (GtkCellRendererText *cell, const gchar *path_string,
               const gchar *new_text, gpointer user_data)
{
    GncTreeViewSplitReg  *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkCellEditable      *editable;

    editable = g_object_get_data (G_OBJECT (cell), "cell-editable");

    DEBUG("cell is %p editable pointer is %p and id %lu", cell, editable, view->priv->fo_handler_id);

    /* Remove the focus out cb if one exists */
    if (view->priv->fo_handler_id != 0)
    {
        if (g_signal_handler_is_connected (G_OBJECT (editable), view->priv->fo_handler_id))
            g_signal_handler_disconnect (G_OBJECT (editable), view->priv->fo_handler_id);
    }
    view->priv->fo_handler_id = 0;

    /* Make sure we set focus to the tree view after cell editing */
    gtk_widget_grab_focus (GTK_WIDGET (view));

    if (g_strcmp0 (g_object_get_data (G_OBJECT (cell), "current-string"), new_text) == 0) // No change, return
    {
        if (view->priv->stop_cell_move == FALSE)
            return;
    }

    model = gnc_tree_view_split_reg_get_model_from_view (view);
    g_return_if_fail (model);

    /* Are we using a template or not */
    if (!gnc_tree_model_split_reg_get_template (model))
        gtv_sr_edited_normal_cb (cell, path_string, new_text, view);
    else
        gtv_sr_edited_template_cb (cell, path_string, new_text, view);
}


/* This is used for the normal registers */
static void
gtv_sr_edited_normal_cb (GtkCellRendererText *cell, const gchar *path_string,
               const gchar *new_text, gpointer user_data)
{
    GncTreeViewSplitReg  *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkCellEditable      *editable;
    GtkTreeIter           m_iter;
    Split                *split;
    Transaction          *trans;
    gboolean              is_trow1, is_trow2, is_split, is_blank;
    ViewCol               viewcol;
    char                 *error_loc = NULL;
    Account              *anchor = view->priv->anchor;

    editable = g_object_get_data (G_OBJECT (cell), "cell-editable");

    DEBUG("cell is %p editable pointer is %p", cell, editable);

    g_return_if_fail (gtv_sr_get_model_iter_from_view_string (view, path_string, &m_iter));

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    model = gnc_tree_view_split_reg_get_model_from_view (view);
    g_return_if_fail (model);

    gnc_tree_model_split_reg_get_split_and_trans (
        model, &m_iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    switch (viewcol) {
    case COL_DATE:
        /* Column is DATE */
        if (is_trow1)
        {
            GDate parsed_date;
            gnc_tree_util_split_reg_parse_date (&parsed_date, new_text);
            if (g_date_valid (&parsed_date))
            {
                gtv_sr_begin_edit (view, trans);
                xaccTransSetDate (trans, g_date_get_day (&parsed_date), g_date_get_month (&parsed_date), g_date_get_year (&parsed_date));
            }
            else
            {
                // We should never get here
                PERR("invalid date '%s'", new_text);
            }
        }
        break;

    case COL_NUMACT:
        /* Column is NUM / ACT */
        gtv_sr_begin_edit (view, trans);
        if (is_trow1)
        {
            /* set per book option */
            gnc_set_num_action (trans, gtv_sr_get_this_split (view, trans),
                                                                new_text, NULL);

            if (!qof_book_use_split_action_for_num_field (gnc_get_current_book()))
            {
                // Set the last number value for this account.
                if (gnc_strisnum (new_text) && anchor != NULL)
                    xaccAccountSetLastNum (anchor, new_text);
            }
        }
        if (is_trow2)
        {
            /* set per book option */
            gnc_set_num_action (trans, gtv_sr_get_this_split (view, trans),
                                                                NULL, new_text);

            if (qof_book_use_split_action_for_num_field (gnc_get_current_book()))
            {
                // Set the last number value for this account.
                if (gnc_strisnum (new_text) && anchor != NULL)
                    xaccAccountSetLastNum (anchor, new_text);
            }
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
        gtv_sr_begin_edit (view, trans);
        if (is_trow1)
        {
            xaccTransSetDescription (trans, new_text);
            // This will potentially fill in the rest of the transaction.
            if (view->priv->auto_complete == FALSE)
            {
                gnc_tree_control_auto_complete (view, trans, new_text);
                view->priv->auto_complete = TRUE;
            }
        }
        if (is_trow2)
            xaccTransSetNotes (trans, new_text);

        if (is_split)
            xaccSplitSetMemo (split, new_text);

        break;

    case COL_RECN:
        /* Column is RECONCILE */
        gtv_sr_begin_edit (view, trans);
        {
            char rec = 'n';

            if (new_text != NULL)
            {
                const gchar *cflag = gnc_get_reconcile_str (CREC);
                const gchar *nflag = gnc_get_reconcile_str (NREC);
                const char recn_flags[] = {NREC, CREC, 0}; // List of reconciled flags
                const gchar *flags;
                gchar *this_flag;
                gint index = 0;

                flags = g_strconcat (nflag, cflag, NULL); // List of translated strings.

                /* Find the current flag in the list of flags */
                this_flag = strstr (flags, new_text);

                if (this_flag != NULL)
                {
                    index = this_flag - flags;
                    rec = recn_flags[index];
                }
            }
            if (is_trow1) 
                xaccSplitSetReconcile (gtv_sr_get_this_split (view, trans), rec);
            if (is_split)
                xaccSplitSetReconcile (split, rec);
        }
        break;

    case COL_TYPE:
        /* Column is TYPE */
        gtv_sr_begin_edit (view, trans);
        {
            char type = TXN_TYPE_NONE;
            if (new_text != NULL)
                type = new_text[0];

            if (is_trow1)
                xaccTransSetTxnType (trans, type);
        }
        break;

    case COL_TRANSFERVOID:
    case COL_AMTVAL:
    case COL_AMOUNT:
    case COL_PRICE:
    case COL_DEBIT:
    case COL_CREDIT:
        {
            Account       *acct, *old_acct;
            gnc_numeric    input;
            Split         *osplit = NULL;
            gboolean       valid_input = FALSE;
            gboolean       force = FALSE;
            gboolean       input_used = FALSE;

            gtv_sr_begin_edit (view, trans);

            /* Get the split pair if anchored to a register */
            if (!is_split && anchor)
            {
                if (!gtv_sr_get_split_pair (view, trans, &osplit, &split))
                {
                    DEBUG("couldn't get split pair");
                    break;
                }
            }

            /* Setup the account field */
            if (viewcol == COL_TRANSFERVOID)
            {
                view->priv->stop_cell_move = FALSE;
                acct = gnc_tree_control_split_reg_get_account_by_name (view, new_text);
                if (acct == NULL)
                {
                    DEBUG("Account is NULL");
                    xaccSplitReinit(split);
                    if (osplit)
                        xaccSplitDestroy (osplit);

                    g_free (view->priv->transfer_string);
                    view->priv->transfer_string = g_strdup (new_text);
                    view->priv->stop_cell_move = TRUE;

                    /* this will populate cell with original value */
                    g_idle_add ((GSourceFunc) gtv_sr_idle_transfer, view);
                    break;
                }

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

            /* Set the transaction currency if not set */
            if (!xaccTransGetCurrency (trans))
            {
                // set transaction currency to that of register (which is guaranteed to be a currency)
                xaccTransSetCurrency (trans, view->priv->reg_currency);

                // We are on General ledger
                if (!anchor)
                {
                    xaccTransSetCurrency (trans, gnc_account_or_default_currency (xaccSplitGetAccount (split), NULL));
                }
            }

            // No need to check for a non-currency register because that's what
            // was already checked when reg_currency was stored.

            /* This computes the value if we just commit the split after entering account */
            if (!valid_input)
                input = gnc_tree_util_split_reg_get_value_for (view, trans, split, is_blank);

            // Negate the input if COL_CREDIT
            if (viewcol == COL_CREDIT)
                input = gnc_numeric_neg (input);

            // Set the split parent trans
            xaccSplitSetParent (split, trans);

            // If we are at trasaction level, column is value, split level is amount
            if (viewcol == COL_AMTVAL)
            {
                gnc_tree_util_set_number_for_input (view, trans, split, input, COL_AMTVAL);
                input_used = TRUE;
            }

            // The price of stock / shares, editable only when expanded and sub_account
            if (viewcol == COL_AMOUNT)
            {
                gnc_tree_util_set_number_for_input (view, trans, split, input, COL_AMTVAL);
                input_used = TRUE;
            }

            // The price of stock / shares
            if (viewcol == COL_PRICE)
            {
                gnc_tree_util_set_number_for_input (view, trans, split, input, COL_PRICE);
                input_used = TRUE;
            }

            // Check if this is a stock / share amount
            if (viewcol == COL_CREDIT || viewcol == COL_DEBIT)
            {
                if (!gnc_commodity_is_currency (xaccAccountGetCommodity (acct)))
                {
                    gnc_tree_util_set_number_for_input (view, trans, split, input, viewcol);
                    input_used = TRUE;
                }
            }

            // This is used in transaction mode, two splits
            if (input_used == FALSE)
            {
                if (gnc_commodity_is_currency (xaccAccountGetCommodity (acct)))
                    gnc_tree_util_split_reg_set_value_for (view, trans, split, input, force);
                else
                    gnc_tree_util_set_value_for_amount (view, trans, split, input);
            }

            // If this is the blank split, promote it.
            if (is_blank)
            {
                /*FIXME May be this should be a signal - Promote the blank split to a real split */
                g_idle_add ((GSourceFunc) gnc_tree_model_split_reg_commit_blank_split, gnc_tree_view_split_reg_get_model_from_view (view));

                /* scroll when view idle */
                g_idle_add ((GSourceFunc) gnc_tree_view_split_reg_scroll_to_cell, view);
            }

            // In transaction mode, two splits only, set up the other split.
            if (osplit)
            {
                xaccSplitSetParent (osplit, trans);

                if (gnc_commodity_is_currency (xaccAccountGetCommodity (acct)))
                    gnc_tree_util_split_reg_set_value_for (view, trans, osplit, gnc_numeric_neg (input), force);
                else
                    gnc_tree_util_set_value_for_amount (view, trans, osplit, gnc_numeric_neg (xaccSplitGetValue (split)));
            }
        }
        break;

    default:
        //g_assert_not_reached();
        break;
    }
}


/* This is used for the template registers */
static void
gtv_sr_edited_template_cb (GtkCellRendererText *cell, const gchar *path_string,
               const gchar *new_text, gpointer user_data)
{
    GncTreeViewSplitReg  *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkCellEditable      *editable;
    GtkTreeIter           m_iter;
    Split                *split;
    Transaction          *trans;
    gboolean              is_trow1, is_trow2, is_split, is_blank;
    ViewCol               viewcol;
    char                 *error_loc = NULL;
    Account              *anchor = view->priv->anchor;

    editable = g_object_get_data (G_OBJECT (cell), "cell-editable");

    DEBUG("cell is %p editable pointer is %p", cell, editable);

    g_return_if_fail (gtv_sr_get_model_iter_from_view_string (view, path_string, &m_iter));

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    model = gnc_tree_view_split_reg_get_model_from_view (view);
    g_return_if_fail (model);

    gnc_tree_model_split_reg_get_split_and_trans (
        model, &m_iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    switch (viewcol) {
    case COL_NUMACT:
        /* Column is NUM / ACT */
        gtv_sr_begin_edit (view, trans);
        if (is_trow1)
        {
            /* set per book option */
            gnc_set_num_action (trans, gtv_sr_get_this_split (view, trans),
                                                                new_text, NULL);

            if (!qof_book_use_split_action_for_num_field (gnc_get_current_book()))
            {
                // Set the last number value for this account.
                if (gnc_strisnum (new_text) && anchor != NULL)
                    xaccAccountSetLastNum (anchor, new_text);
            }
        }
        if (is_trow2)
        {
            /* set per book option */
            gnc_set_num_action (trans, gtv_sr_get_this_split (view, trans),
                                                                NULL, new_text);

            if (qof_book_use_split_action_for_num_field (gnc_get_current_book()))
            {
                // Set the last number value for this account.
                if (gnc_strisnum (new_text) && anchor != NULL)
                    xaccAccountSetLastNum (anchor, new_text);
            }
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
        gtv_sr_begin_edit (view, trans);
        if (is_trow1)
        {
            xaccTransSetDescription (trans, new_text);
            // This will potentially fill in the rest of the transaction.
            if (view->priv->auto_complete == FALSE)
            {
                gnc_tree_control_auto_complete (view, trans, new_text);
                view->priv->auto_complete = TRUE;
            }
        }
        if (is_trow2)
            xaccTransSetNotes (trans, new_text);

        if (is_split)
            xaccSplitSetMemo (split, new_text);

        break;

    case COL_RECN:
        /* Column is RECONCILE */
        gtv_sr_begin_edit (view, trans);
        {
            char rec = 'n';

            if (new_text != NULL)
            {
                const gchar *cflag = gnc_get_reconcile_str (CREC);
                const gchar *nflag = gnc_get_reconcile_str (NREC);
                const char recn_flags[] = {NREC, CREC, 0}; // List of reconciled flags
                const gchar *flags;
                gchar *this_flag;
                gint index = 0;

                flags = g_strconcat (nflag, cflag, NULL); // List of translated strings.

                /* Find the current flag in the list of flags */
                this_flag = strstr (flags, new_text);

                if (this_flag != NULL)
                {
                    index = this_flag - flags;
                    rec = recn_flags[index];
                }
            }
            if (is_trow1)
                xaccSplitSetReconcile (gtv_sr_get_this_split (view, trans), rec);
            if (is_split)
                xaccSplitSetReconcile (split, rec);
        }
        break;

    case COL_TRANSFERVOID:
    case COL_DEBIT:
    case COL_CREDIT:
        {
            gtv_sr_begin_edit (view, trans);

            /* Setup the account field */
            if (viewcol == COL_TRANSFERVOID)
            {
                Account *template_acc;
		Account *acct;
                const GncGUID *acctGUID;

                /* save the account GncGUID into the kvp_data. */
                view->priv->stop_cell_move = FALSE;
                acct = gnc_tree_control_split_reg_get_account_by_name (view, new_text);
                if (acct == NULL)
                {
                    DEBUG("Template Account is NULL");

                    g_free (view->priv->transfer_string);
                    view->priv->transfer_string = g_strdup (new_text);
                    view->priv->stop_cell_move = TRUE;

                    /* this will populate cell with original value */
                    g_idle_add ((GSourceFunc) gtv_sr_idle_transfer, view);
                    break;
                }

                acctGUID = xaccAccountGetGUID (acct);
		qof_instance_set (QOF_INSTANCE (split),
				  "sx-account", acctGUID,
				  NULL);

                template_acc = gnc_tree_model_split_reg_get_template_account (model);

                /* set the actual account to the fake account for these templates */
                xaccAccountInsertSplit (template_acc, split);
            }

            /* Set the transaction currency if not set */
            if (!xaccTransGetCurrency (trans))
            {
                xaccTransSetCurrency (trans, gnc_account_or_default_currency (xaccSplitGetAccount (split), NULL));
            }

            // No need to check for a non-currency register because that's what
            // was already checked when reg_currency was stored.

            /* Setup the debit and credit fields */
            if (viewcol == COL_DEBIT)
            {
                char *error_loc;
                gnc_numeric new_value;
                gboolean parse_result;

                /* Setup the debit formula */

                /* If the value can be parsed into a numeric result, store that
                 * numeric value additionally. See above comment.*/
                parse_result = gnc_exp_parser_parse_separate_vars (new_text, &new_value, &error_loc, NULL);
                if (!parse_result)
                {
                    new_value = gnc_numeric_zero();
                }
		qof_instance_set (QOF_INSTANCE (split),
				  "sx-debit-formula", new_text,
				  "sx-debit-numeric", &new_value,
				  "sx-credit-formula", NULL,
				  "sx-credit-numeric", NULL,
				  NULL);
            }

            /* Setup the debit and credit fields */
            if (viewcol == COL_CREDIT)
            {
                char *error_loc;
                gnc_numeric new_value;
                gboolean parse_result;

               /* If the value can be parsed into a numeric result (without any
                 * further variable definitions), store that numeric value
                 * additionally in the kvp. Otherwise store a zero numeric
                 * there.*/
                parse_result = gnc_exp_parser_parse_separate_vars (new_text, &new_value, &error_loc, NULL);
                if (!parse_result)
                {
                    new_value = gnc_numeric_zero();
                }
		qof_instance_set (QOF_INSTANCE (split),
				  "sx-credit-formula", new_text,
				  "sx-credit-numeric", &new_value,
				  "sx-debit-formula", NULL,
				  "sx-debit-numeric", NULL,
				  NULL);
            }
            /* set the amount to an innocuous value */
            xaccSplitSetValue (split, gnc_numeric_create (0, 1));

            // Set the split parent trans
            xaccSplitSetParent (split, trans);

            // If this is the blank split, promote it.
            if (is_blank)
            {
                /*FIXME May be this should be a signal - Promote the blank split to a real split */
                g_idle_add ((GSourceFunc) gnc_tree_model_split_reg_commit_blank_split, gnc_tree_view_split_reg_get_model_from_view (view));

                /* scroll when view idle */
                g_idle_add ((GSourceFunc) gnc_tree_view_split_reg_scroll_to_cell, view);
            }
        }
        break;

    default:
        //g_assert_not_reached();
        break;
    }
}

/*###########################################################################*/

/* Parses the string value and returns true if it is a
 * number. In that case, *num is set to the value parsed. */
static gboolean
gtv_sr_parse_num (const char *string, long int *num)
{
    long int number;

    if (string == NULL)
        return FALSE;

    if (!gnc_strisnum (string))
        return FALSE;

    number = strtol (string, NULL, 10);

    if ((number == LONG_MIN) || (number == LONG_MAX))
        return FALSE;

    if (num != NULL)
        *num = number;

    return TRUE;
}

/* Callback for Number Accelerator key */
static void
gtv_sr_num_cb (GtkEntry    *entry,
                          const gchar *text,
                          gint         length,
                          gint        *position,
                          gpointer     user_data)
{
    GtkEditable *editable = GTK_EDITABLE (entry);
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    RowDepth depth;
    Account *account;
    gchar *entered_string;
    gchar *leave_string = NULL;

    gboolean accel = FALSE;
    gboolean is_num;
    long int number = 0;
    gunichar uc;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    account = gnc_tree_model_split_reg_get_anchor (model);

    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    // This only works on the number field.
    if ((depth == TRANS2 || depth == SPLIT3))
        return;

    // Get entered string
    entered_string = gtk_editable_get_chars (editable, 0, -1);

    // Test for number and return it.
    is_num = gtv_sr_parse_num (entered_string, &number);

    if (is_num && (number < 0))
        is_num = FALSE;

    // Test for accelerator keys.
    uc = g_utf8_get_char (text);
    switch (uc)
    {
    case '+':
    case '=':
        number++;
        accel = TRUE;
        break;

    case '_':
    case '-':
        number--;
        accel = TRUE;
        break;

    case '}':
    case ']':
        number += 10;
        accel = TRUE;
        break;

    case '{':
    case '[':
        number -= 10;
        accel = TRUE;
        break;
    }

    if (number < 0)
        number = 0;

    /* If there is already a non-number there, don't accelerate. */
    if (accel && !is_num && (g_strcmp0 (entered_string, "") != 0))
        accel = FALSE;

    // See if entered string is empty, try and get the last number.
    if (accel && (g_strcmp0 (entered_string, "") == 0))
    {
        if (account != NULL)
        {
            if (gtv_sr_parse_num (xaccAccountGetLastNum (account), &number))
                number = number + 1;
            else
                number = 1;
        }
        else
            number = 1;

        is_num = TRUE;
    }

    if (!accel)
    {
        leave_string = g_strconcat (entered_string, text, NULL);
    }

    if (accel && is_num)
    {
        char buff[128];

        strcpy (buff, "");
        snprintf (buff, sizeof(buff), "%ld", number);

        if (g_strcmp0 (buff, "") == 0)
            leave_string = "";
        else
            leave_string = g_strdup (buff);
    }

    g_signal_handlers_block_by_func (editable, (gpointer) gtv_sr_num_cb, user_data);

    gtk_editable_delete_text (editable, 0, -1);
    gtk_editable_set_position (editable, 0);

    if (leave_string != NULL)
        gtk_editable_insert_text (editable, leave_string, -1, position);

    g_signal_handlers_unblock_by_func (editable, (gpointer) gtv_sr_num_cb, user_data);

    g_signal_stop_emission_by_name (editable, "insert_text");

    if (leave_string)
        g_free (leave_string);

    g_free (entered_string);
}


/* Callback for Account seperator key */
static void
gtv_sr_acct_cb (GtkEntry    *entry,
                          const gchar *text,
                          gint         length,
                          gint        *position,
                          gpointer     user_data)
{
    GtkEditable *editable = GTK_EDITABLE (entry);
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkEntryCompletion *completion;
    GtkTreeModel *model;
    GtkTreeIter  iter;

    const gchar *sep_char;
    gchar       *entered_string;
    gchar       *acct_string = NULL;

    gint         num_of_items = 0;
    gboolean     valid;
    gboolean     all_the_same = TRUE;

    sep_char = gnc_get_account_separator_string ();

    if (g_strcmp0 (text, sep_char) == 0)
        entered_string = g_strconcat (gtk_editable_get_chars (editable, 0, -1), NULL);
    else
        entered_string = g_strconcat (gtk_editable_get_chars (editable, 0, -1), text, NULL);

    // Get the completion and model
    completion = gtk_entry_get_completion (entry);
    model = gtk_entry_completion_get_model (completion);

    // Get the first item in the list
    valid = gtk_tree_model_get_iter_first (model, &iter);
    while (valid)
    {
        gchar *item, *item_string, *l_item, *l_entered_string, *l_acct_string;

        // Walk through the list, reading each row
        if (view->priv->acct_short_names)
            gtk_tree_model_get (model, &iter, 0, &item, -1);
        else
            gtk_tree_model_get (model, &iter, 1, &item, -1);

        item_string = g_strconcat (item, sep_char, NULL);

        l_item = g_utf8_strdown (item_string, -1);
        l_entered_string = g_utf8_strdown (entered_string, -1);

        if (g_str_has_prefix (l_item, l_entered_string))
        {
            if (num_of_items == 0)
                acct_string = g_strdup (item);
            else
            {
                l_acct_string = g_utf8_strdown (acct_string, -1);
                if (!g_str_has_prefix (g_utf8_strdown (l_item, -1), l_acct_string))
                    all_the_same = FALSE;
                g_free (l_acct_string);
            }
            num_of_items = num_of_items + 1;
        }
        g_free (item);
        g_free (item_string);
        g_free (l_item);
        g_free (l_entered_string);
        valid = gtk_tree_model_iter_next (model, &iter);
    }

    g_signal_handlers_block_by_func (editable, (gpointer) gtv_sr_acct_cb, user_data);

    gtk_editable_delete_text (editable, 0, -1);
    gtk_editable_set_position (editable, 0);

    if (num_of_items == 0)
        gtk_editable_insert_text (editable, entered_string, -1, position);
    else
    {
        if (num_of_items == 1)
            gtk_editable_insert_text (editable, acct_string, -1, position);
        else
        {
            if (all_the_same)
            {
                if (g_strcmp0 (text, sep_char) == 0)
                    gtk_editable_insert_text (editable, g_strconcat (acct_string, sep_char, NULL), -1, position);
                else
                    gtk_editable_insert_text (editable, entered_string, -1, position);
            }
            else
               gtk_editable_insert_text (editable, entered_string, -1, position);
        }
    }
    g_signal_handlers_unblock_by_func (editable, (gpointer) gtv_sr_acct_cb, user_data);

    g_signal_stop_emission_by_name (editable, "insert_text");
    g_free (acct_string);
    g_free (entered_string);
}


/* Callback for changing reconcile setting with space bar */
static void
gtv_sr_recn_cb (GtkEntry    *entry,
                          const gchar *text,
                          gint         length,
                          gint        *position,
                          gpointer     user_data)
{
    GtkEditable *editable = GTK_EDITABLE (entry);
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

    const gchar *cflag = gnc_get_reconcile_str (CREC);
    const gchar *nflag = gnc_get_reconcile_str (NREC);

    const gchar *flags;
    gchar *this_flag;
    gchar *result;
    static char ss[2];
    gint index = 0;

    result = g_ascii_strdown (text, length);

    if (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-flag") != NULL)
        index = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-flag"));
    else
    {
        if (g_strcmp0 (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string"), nflag) == 0)
            index = 0;
    }

    flags = g_strconcat (nflag, cflag, NULL);

    /* So we can test for space */
    ss[0] = ' ';
    ss[1] = '\0';

    /* Find the entered text in the list of flags */
    this_flag = strstr (flags, text);

    if (this_flag == NULL || *this_flag == '\0')
    {
        if (g_strcmp0 (text, ss) == 0)  // test for space
        {
            /* In the list, choose the next item in the list
               (wrapping around as necessary). */

            if (flags[index + 1] != '\0')
                index = index + 1;
            else
                index = 0;

            g_free (result);
            result = g_strdup_printf("%c", flags[index]);
        }
        else
        {
            /* If it's not there (or the list is empty) use default_flag */
            g_free (result);
            result = g_strdup (gnc_get_reconcile_str (NREC));
        }
    }
    else
    {
        g_free (result);
        result = g_strdup (text);
    }

    /* save the index in the cellrenderer */
    g_object_set_data (G_OBJECT (view->priv->temp_cr), "current-flag", GINT_TO_POINTER (index));

    g_signal_handlers_block_by_func (editable, (gpointer) gtv_sr_recn_cb, user_data);

    gtk_editable_delete_text (editable, 0, -1);
    gtk_editable_insert_text (editable, result, length, position);

    g_signal_handlers_unblock_by_func (editable, (gpointer) gtv_sr_recn_cb, user_data);

    g_signal_stop_emission_by_name (editable, "insert_text");

    g_free (result);
}


/* Callback for changing type setting with space bar */
static void
gtv_sr_type_cb (GtkEntry    *entry,
                          const gchar *text,
                          gint         length,
                          gint        *position,
                          gpointer     user_data)
{
    GtkEditable *editable = GTK_EDITABLE (entry);
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    const gchar *flags;
    const char type_flags[] = {TXN_TYPE_INVOICE, TXN_TYPE_PAYMENT, 0};
    gchar *this_flag;
    gchar *result;
    static char ss[2];
    gint index = 0;

    flags = type_flags;

    result = g_ascii_strup (text, length);

    if (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-flag") != NULL)
        index = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-flag"));
    else
    {
        if (g_strcmp0 (g_object_get_data (G_OBJECT (view->priv->temp_cr), "current-string"), "I") == 0)
            index = 0;
    }

    /* So we can test for space */
    ss[0] = ' ';
    ss[1] = '\0';

    /* Find the entered text in the list of flags */
    this_flag = strstr (flags, text);

    if (this_flag == NULL || *this_flag == '\0')
    {
        if (g_strcmp0 (text, ss) == 0)  // test for space
        {
            /* In the list, choose the next item in the list
               (wrapping around as necessary). */

            if (flags[index + 1] != '\0')
                index = index + 1;
            else
                index = 0;

            g_free (result);
            result = g_strdup_printf("%c", flags[index]);
        }
        else
        {
            /* If it's not there (or the list is empty) use default_flag */
            g_free (result);
            result  = NULL;
        }
    }
    else
    {
        g_free (result);
        result = g_strdup (text);
    }

    /* save the index in the cellrenderer */
    g_object_set_data (G_OBJECT (view->priv->temp_cr), "current-flag", GINT_TO_POINTER (index));

    g_signal_handlers_block_by_func (editable, (gpointer) gtv_sr_type_cb, user_data);

    gtk_editable_delete_text (editable, 0, -1);
    gtk_editable_insert_text (editable, result, length, position);

    g_signal_handlers_unblock_by_func (editable, (gpointer) gtv_sr_type_cb, user_data);

    g_signal_stop_emission_by_name (editable, "insert_text");

    g_free (result);
}


/* For handling keynav */
static gboolean
gtv_sr_ed_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeViewColumn *col;
    GtkTreePath *spath, *start_spath;
    gboolean goto_blank = FALSE;
    gboolean next_trans = TRUE;
    Transaction *btrans, *ctrans;
    gint depth;
    gboolean auto_popped = FALSE;

    // spath is where we are, before key press...
    gtk_tree_view_get_cursor (GTK_TREE_VIEW (view), &spath, &col);

    if (event->type != GDK_KEY_PRESS)
    {
        if (spath)
            gtk_tree_path_free (spath);
        return FALSE;
    }

    switch (event->keyval)
    {

    case GDK_KEY_Up:
    case GDK_KEY_Down:

        if (!spath)
            return TRUE;

        // This is to test for the auto completion popup window
        {
            GtkWidget *toplevel;
            GtkWindowGroup *window_group;
            GList *win_list;

            toplevel = gtk_widget_get_toplevel (widget);
            if (GTK_IS_WINDOW (toplevel))
            {
                window_group = gtk_window_get_group (GTK_WINDOW (toplevel));
                win_list = gtk_window_group_list_windows (window_group);
                if (g_list_length (win_list) == 1 && gtk_widget_get_visible (GTK_WIDGET (win_list->data)))
                    auto_popped = TRUE;

            g_list_free (win_list);
            }
        }

        // Auto complete window popped
        if (auto_popped == TRUE)
        {
            gtk_tree_path_free (spath);
            return FALSE;
        }

        model = gnc_tree_view_split_reg_get_model_from_view (view);

        // Make sure we have stopped editing.
        gnc_tree_view_split_reg_finish_edit (view);

        // This stops the cell changing.
        if (view->priv->stop_cell_move == TRUE)
        {
            gtk_tree_path_free (spath);
            return TRUE;
        }

        depth = gtk_tree_path_get_depth (spath);
        if (event->keyval == GDK_KEY_Up)
        {
            if (depth == 1)
            {
                if (gtk_tree_path_prev (spath))
                {
                    if (gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), spath))
                    {
                        gtk_tree_path_down (spath);

                        if (gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), spath) && model->type == GENERAL_JOURNAL2)
                        {
                            gtk_tree_path_down (spath);

                            while (gnc_tree_view_path_is_valid (GNC_TREE_VIEW (view), spath))
                            {
                                gtk_tree_path_next (spath);
                            }
                            gtk_tree_path_prev (spath);
                        }
                    }
                }
            }
            else if (!gtk_tree_path_prev (spath) && depth > 1)
            {
                gtk_tree_path_up (spath);
            }
        }
        else if (gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), spath))
        {
            gtk_tree_path_down (spath);
        }
        else
        {
            gtk_tree_path_next (spath);
            if (!gnc_tree_view_path_is_valid (GNC_TREE_VIEW (view), spath) && depth > 2)
            {
                gtk_tree_path_prev (spath);
                gtk_tree_path_up (spath);
                gtk_tree_path_next (spath);
            }
            if (!gnc_tree_view_path_is_valid (GNC_TREE_VIEW (view), spath) && depth > 1)
            {
                gtk_tree_path_prev (spath);
                gtk_tree_path_up (spath);
                gtk_tree_path_next (spath);
            }
        }

        /* Set cursor to new column, open for editing */
        gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, col, TRUE);

        if (event->keyval == GDK_KEY_Up)
        {
            gnc_tree_model_split_reg_move (model, VIEW_UP);
        }
        else
            gnc_tree_model_split_reg_move (model, VIEW_DOWN);

        return TRUE;
        break;

    case GDK_KEY_Return:

        if (!spath)
            return TRUE;

        // This stops the cell changing.
        if (view->priv->stop_cell_move == TRUE)
        {
            gtk_tree_path_free (spath);
            return TRUE;
        }

        // Do sums if we have ctrl key
        if (event->state & GDK_CONTROL_MASK)
        {
            // Make sure we have stopped editing.
            gnc_tree_view_split_reg_finish_edit (view);

            /* Set cursor to the column, open for editing */
            gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, col, TRUE);
            gtk_tree_path_free (spath);
            return TRUE;
        }
        return FALSE;
        break;

    case GDK_KEY_KP_Enter:

        if (!spath)
            return TRUE;

        // This stops the cell changing.
        if (view->priv->stop_cell_move == TRUE)
        {
            gtk_tree_path_free (spath);
            return TRUE;
        }

        goto_blank = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                         GNC_PREF_ENTER_MOVES_TO_END);

        model = gnc_tree_view_split_reg_get_model_from_view (view);
        btrans = gnc_tree_model_split_get_blank_trans (model);
        ctrans = gnc_tree_view_split_reg_get_current_trans (view);

        /* Are we on the blank transaction */
        if (btrans == ctrans)
            next_trans = FALSE;

        /* First record the transaction */
        if (gnc_tree_view_split_reg_enter (view))
        {
            /* Now move. */
            if (goto_blank)
                g_idle_add ((GSourceFunc)gnc_tree_control_split_reg_jump_to_blank, view);
            else if (next_trans)
                gnc_tree_control_split_reg_goto_rel_trans_row (view, 1);
        }
        return TRUE;
        break;

    default:
        gtk_tree_path_free (spath);
	return FALSE;
    }
}

/*###########################################################################*/

/* The main Start Editing Call back for the TEXT columns */
static void
gtv_sr_editable_start_editing_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
                              const gchar *path_string, gpointer user_data)
{
    GncTreeViewSplitReg  *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GncTreeModelSplitReg *model;
    GtkTreeModel         *s_model;
    GtkTreePath          *spath, *mpath, *fpath;
    GtkEntry             *entry = NULL;
    ViewCol               viewcol;
    RowDepth              depth;
    gint                 *indices;

    GtkListStore *description_list;
    GtkListStore *memo_list;
    GtkListStore *notes_list;
    GtkListStore *account_list;

    GtkEntryCompletion *completion = gtk_entry_completion_new();

    ENTER("gtv_sr_editable_start_editing_cb Path string is '%s'", path_string);

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));

    /* Description / Notes / Memo / Accounts Completion Lists */
    description_list = gnc_tree_model_split_reg_get_description_list (model);
    notes_list = gnc_tree_model_split_reg_get_notes_list (model);
    memo_list = gnc_tree_model_split_reg_get_memo_list (model);
    account_list = gnc_tree_model_split_reg_get_acct_list (model);

    // Use depth to determine if it is a split or transaction
    spath = gtk_tree_path_new_from_string (path_string);
    depth = gtk_tree_path_get_depth (spath);
    indices = gtk_tree_path_get_indices (spath);

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT(cr), "view_column"));

    DEBUG("editable Depth is %u and ViewCol is %d", depth, viewcol);

    g_object_set_data (G_OBJECT (cr), "cell-editable", editable);

    // This is for key navigation...
    g_signal_connect (G_OBJECT (editable), "key-press-event", G_CALLBACK (gtv_sr_ed_key_press_cb), view);

    /* DATE COLUMN */
    if (viewcol == COL_DATE)
    {
        entry = GTK_ENTRY (GNC_POPUP_ENTRY (editable)->entry);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data_full (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)), g_free);

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) gtv_sr_remove_edit_date, view);

        DEBUG("Current String date is '%s'", gtk_entry_get_text (entry));
    }

    /* TRANSFER / VOID COLUMN */
    else if (viewcol == COL_TRANSFERVOID)
    {
        entry = GTK_ENTRY (gtk_bin_get_child (GTK_BIN (editable)));

        // This is for key navigation...
        g_signal_connect (G_OBJECT (entry), "key-press-event", G_CALLBACK (gtv_sr_ed_key_press_cb), view);

        {
            GtkEditable *editable = GTK_EDITABLE (entry);

            if (view->priv->stop_cell_move == TRUE)
            {
                gint textPosition = 0;
                gtk_editable_insert_text (GTK_EDITABLE (editable), view->priv->transfer_string, -1, &textPosition);
                gtk_editable_set_position (GTK_EDITABLE (editable), -1);
            }
        }

        // Update the Account list combo.
        gnc_tree_model_split_reg_update_account_list (model);

        gtk_entry_set_completion (entry, completion);
        gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (account_list));

        /* This sets which text column to use, 0 for short names, 1 for long */
        if (view->priv->acct_short_names)
            gtk_entry_completion_set_text_column (completion, 0);
        else
            gtk_entry_completion_set_text_column (completion, 1);

        gtk_entry_completion_set_popup_completion (completion, TRUE);
        gtk_entry_completion_set_inline_selection (completion, TRUE);
        gtk_entry_completion_set_popup_set_width (completion, FALSE);
        gtk_entry_completion_set_minimum_key_length (completion, 1);
//??        g_signal_connect(G_OBJECT(completion), "match-selected", (GCallback) gtv_sr_match_selected_cb, view);
        g_object_unref (completion);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data_full (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)), g_free);

        g_signal_connect (G_OBJECT (entry), "insert_text", (GCallback) gtv_sr_acct_cb, view);

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback) gtv_sr_changed_cb, view);
        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) gtv_sr_remove_edit_combo, view);

        DEBUG("Current String tv is '%s'", gtk_entry_get_text (entry));
    }

    /* NUMBER / ACTION COLUMN */
    else if (viewcol == COL_NUMACT)
    {
        if ((depth == TRANS1) || ((depth == TRANS2) && (qof_book_use_split_action_for_num_field (gnc_get_current_book()))))
        {
            entry = GTK_ENTRY (editable);

            //Copy the string in the GtkEntry for later comparison
            g_object_set_data_full (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)), g_free);

            g_signal_connect (G_OBJECT (GTK_ENTRY (entry)), "insert_text", (GCallback) gtv_sr_num_cb, view);

            view->priv->fo_handler_id = g_signal_connect (G_OBJECT (editable), "focus-out-event", (GCallback) gtv_sr_focus_out_cb, view);

            g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) gtv_sr_remove_edit_entry, view);

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback)gtv_sr_changed_cb, view);
            DEBUG("Current String num is '%s'", gtk_entry_get_text (entry));
        }

        if ((depth == SPLIT3) || ((depth == TRANS2) && (!qof_book_use_split_action_for_num_field (gnc_get_current_book()))))
        {
            gnc_tree_model_split_reg_update_action_list (model);

            entry = GTK_ENTRY (gtk_bin_get_child (GTK_BIN (editable)));

            //Copy the string in the GtkEntry for later comparison
            g_object_set_data_full (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)), g_free);

//??          g_signal_connect (G_OBJECT (cr), "changed", (GCallback) gtv_sr_changed_cb, view);
            g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) gtv_sr_remove_edit_combo, view);

            DEBUG("Current String action is '%s'", gtk_entry_get_text (entry));
        }
    }

    /* DESCRIPTION / NOTES / MEMO COLUMN */
    else if (viewcol == COL_DESCNOTES)
    {
        entry = GTK_ENTRY (editable);

        // Update the auto completion lists.
        gnc_tree_model_split_reg_update_completion (model);

        //Data used for completion is set based on if editing split or not
        if (depth == TRANS1)
        {
            gtk_entry_set_completion (entry, completion);
            gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (description_list));
            gtk_entry_completion_set_text_column (completion, 0);
        }
        else if (depth == TRANS2)
        {
            gtk_entry_set_completion (entry, completion);
            gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (notes_list));
            gtk_entry_completion_set_text_column (completion, 0);
        }
        else if (depth == SPLIT3)
        {
            gtk_entry_set_completion (entry, completion);
            gtk_entry_completion_set_model (completion, GTK_TREE_MODEL (memo_list));
            gtk_entry_completion_set_text_column (completion, 0);
        }

        //To emit "match-selected" signal we need to have a list of matches to
        //select from instead of using inline autocompletion
        gtk_entry_completion_set_popup_completion (completion, TRUE);
        gtk_entry_completion_set_inline_selection (completion, TRUE);
        gtk_entry_completion_set_minimum_key_length (completion, view->priv->key_length);
//??        g_signal_connect (G_OBJECT (completion), "match-selected", (GCallback) gtv_sr_match_selected_cb, view);

        g_object_unref (completion);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data_full (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)), g_free);

        view->priv->fo_handler_id = g_signal_connect (G_OBJECT (editable), "focus-out-event", (GCallback) gtv_sr_focus_out_cb, view);

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) gtv_sr_remove_edit_entry, view);

        DEBUG("Current String dnm is '%s'", gtk_entry_get_text (entry));
    }

    /* RECN COLUMN */
    else if (viewcol == COL_RECN)
    {
        entry = GTK_ENTRY (editable);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data_full (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)), g_free);

        g_signal_connect (G_OBJECT (GTK_ENTRY (editable)), "insert_text", (GCallback)gtv_sr_recn_cb, view);

        view->priv->fo_handler_id = g_signal_connect (G_OBJECT (editable), "focus-out-event", (GCallback) gtv_sr_focus_out_cb, view);

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) gtv_sr_remove_edit_entry, view);

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback) gtv_sr_changed_cb, view);
        DEBUG("Current String recn is '%s'", gtk_entry_get_text (entry));
    }

    /* TYPE COLUMN */
    else if (viewcol == COL_TYPE)
    {
        entry = GTK_ENTRY (editable);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data_full (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)), g_free);

        g_signal_connect (G_OBJECT (GTK_ENTRY (editable)), "insert_text", (GCallback)gtv_sr_type_cb, view);

        view->priv->fo_handler_id = g_signal_connect (G_OBJECT (editable), "focus-out-event", (GCallback) gtv_sr_focus_out_cb, view);

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) gtv_sr_remove_edit_entry, view);

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback) gtv_sr_changed_cb, view);
        DEBUG("Current String type is '%s'", gtk_entry_get_text (entry));
    }

    /* THE REST OF THE COLUMNS */
    else
    {
        entry = GTK_ENTRY (editable);

        //Copy the string in the GtkEntry for later comparison
        g_object_set_data_full (G_OBJECT (cr), "current-string", g_strdup (gtk_entry_get_text (entry)), g_free);

        view->priv->fo_handler_id = g_signal_connect (G_OBJECT (editable), "focus-out-event", (GCallback) gtv_sr_focus_out_cb, view);

        g_signal_connect (G_OBJECT (editable), "remove-widget", (GCallback) gtv_sr_remove_edit_entry, view);

//??        g_signal_connect (G_OBJECT (cr), "changed", (GCallback)gtv_sr_changed_cb, view);
        DEBUG("Current String rest is '%s'", gtk_entry_get_text (entry));
    }

    /* Lets change the background of the entry widgets */
    {
        GdkColor     color;
        const gchar *row_color;
        gboolean     is_trow1 = FALSE;
        gboolean     is_trow2 = FALSE;
        gboolean     is_split = FALSE;

        if (depth == TRANS1)
            is_trow1 = TRUE;
        if (depth == TRANS2)
            is_trow2 = TRUE;
        if (depth == SPLIT3)
            is_split = TRUE;

        row_color = gnc_tree_model_split_reg_get_row_color (model, is_trow1, is_trow2, is_split, indices[0]);

        if (gdk_color_parse (row_color, &color))
        {
            if (entry != NULL)
                gtk_widget_modify_base (GTK_WIDGET (entry), GTK_STATE_NORMAL, &color);
        }
    }

    gtv_sr_help (view, cr, viewcol, depth);
    gtk_tree_path_free (spath);

    view->priv->temp_cr = cr;
    view->editing_now = TRUE;

    DEBUG("Temp Cell Rend %p", view->priv->temp_cr);

    //Add edit-canceled property to cr so we can distinguish between
    //cancelled and actual changes
    g_object_set_data (G_OBJECT (cr), "edit-canceled", GINT_TO_POINTER (FALSE));
    LEAVE(" ");
}


// Handle the "match-selected" signal
static void
gtv_sr_match_selected_cb (GtkEntryCompletion *widget, GtkTreeModel *model,
                        GtkTreeIter *iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

//FIXME g_print("gtv_sr_match_selected_cb\n\n");

/* Not sure what I am going to put in here yet if anything */

}


// Handle the "changed" signal
static void
gtv_sr_changed_cb (GtkCellRendererCombo *widget, gchar *path_string,
                        GtkTreeIter *iter, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

//FIXME g_print("gtv_sr_changed_cb path string is '%s'\n\n", path_string);

/* Not sure what I am going to put in here yet if anything */

}


// Handle the "editing-canceled" signal
static void
gtv_sr_editing_canceled_cb (GtkCellRenderer *cr, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);

    if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (view), "data-edited")) == FALSE) // Not edited, reset edit path
    {
        view->priv->dirty_trans = NULL;
    }

    /* Reset stop_cell_move */
    if (view->priv->stop_cell_move == TRUE)
    {
        view->priv->stop_cell_move = FALSE;

        /* this will populate cell with original value */
        g_idle_add ((GSourceFunc) gtv_sr_idle_transfer, view);
    }

    /* Reset Help text */
    if (view->help_text)
        g_free (view->help_text);
    view->help_text = g_strdup (" ");
    g_signal_emit_by_name (view, "help_signal", NULL);

    //Set edit-canceled property
    g_object_set_data (G_OBJECT (cr), "edit-canceled", GINT_TO_POINTER (TRUE));	
}

/*####################################################################
          ^^^^   gtv function call backs    ^^^^
#####################################################################*/

/* Scroll the view to show selected row based on sort direction */
gboolean
gnc_tree_view_split_reg_scroll_to_cell (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;

    PINFO("#### Start Scroll to Cell ####");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    mpath = gnc_tree_view_split_reg_get_current_path (view);
    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    if (model->sort_direction == GTK_SORT_DESCENDING)
    {
        gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), spath, NULL, TRUE, 0.5, 0.0); //0.0
    }
    else
    {
        if (model->use_double_line)
        {
            gtk_tree_path_down (spath); // move to the second row of transaction
            gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), spath, NULL, TRUE, 1.0, 0.0); //1.0
        }
        else
        {
            gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), spath, NULL, TRUE, 1.0, 0.0); //1.0
        }
    }

    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);

    PINFO("#### End Scroll to Cell ####");

    return (FALSE);
}


/* Scroll the view to show the blank split with least movement */
gboolean
gnc_tree_view_split_reg_scroll_to_bsplit (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *bsplit_mpath, *bsplit_spath;
    Split *bsplit;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    /* Get the blank split spath */
    bsplit = gnc_tree_model_split_get_blank_split (model);
    bsplit_mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, bsplit, NULL);
    bsplit_spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, bsplit_mpath);

    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), bsplit_spath, NULL, FALSE, 1.0, 0.0);

    gtk_tree_path_free (bsplit_mpath);
    gtk_tree_path_free (bsplit_spath);
    return (FALSE);
}


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

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    if (trans == NULL)
    {
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
        view->priv->dirty_trans = NULL;
    }
    else
    {
        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (TRUE));
        view->priv->dirty_trans = trans;
    }
}


/* Returns the current path, or NULL if the current path is the blank split. */
GtkTreePath *
gnc_tree_view_split_reg_get_current_path (GncTreeViewSplitReg *view)
{
    if (!view->priv->current_ref)
        return NULL;
    return gtk_tree_row_reference_get_path (view->priv->current_ref);
}


/* Sets the current path reference to path */
void
gnc_tree_view_split_reg_set_current_path (GncTreeViewSplitReg *view, GtkTreePath *mpath)
{
    GncTreeModelSplitReg *model;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    if (view->priv->current_ref != NULL)
    {
        gtk_tree_row_reference_free (view->priv->current_ref);
        view->priv->current_ref = NULL;
    }
    view->priv->current_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (model), mpath);
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

    // Lets get out of the way, move selection to trans - selection is blocked
    gnc_tree_control_split_reg_goto_rel_trans_row (view, 0);

    depth = view->priv->current_depth;

    if (trans && (depth != SPLIT3))
    {
        Split *s;
        int i = 0;

        if (!xaccTransIsOpen (trans))
            xaccTransBeginEdit (trans);

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

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    trans = view->priv->current_trans;
    split = view->priv->current_split;

    if (!xaccTransIsOpen (trans))
        xaccTransBeginEdit (trans);

    gnc_tree_view_split_reg_set_dirty_trans (view, trans);

    // Lets get out of the way, move selection to trans - selection is blocked
    gnc_tree_control_split_reg_goto_rel_trans_row (view, 0);

    xaccSplitDestroy (split);
}


/* Delete the current transaction */
void
gnc_tree_view_split_reg_delete_current_trans (GncTreeViewSplitReg *view)
{
    Transaction           *trans;

    /* We do not use the normal confirmation with this one as we have
       all ready asked the user to confirm delete */

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    trans = view->priv->current_trans;

    /* We need to go back one to select the next transaction */
    gnc_tree_control_split_reg_goto_rel_trans_row (view, 1);

    if (!xaccTransIsOpen (trans))
        xaccTransBeginEdit (trans);
    gnc_tree_view_split_reg_set_dirty_trans (view, trans);

    xaccTransDestroy (trans);
    xaccTransCommitEdit (trans);

    gnc_tree_view_split_reg_set_dirty_trans (view, NULL);
}


/* Record changes */
gboolean
gnc_tree_view_split_reg_enter (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg  *model;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    // Test for transaction changed
    if (gtv_sr_transaction_changed (view))
        return FALSE;

    // Return FALSE on discard
    if (view->priv->trans_confirm == DISCARD)
        return FALSE;

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

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    if (trans && xaccTransIsOpen (trans))
    {
        // Move selection to trans - selection is blocked
        gnc_tree_control_split_reg_goto_rel_trans_row (view, 0);

        // Remove the split before rollback.
        gnc_tree_model_split_reg_set_blank_split_parent (model, trans, TRUE);

        g_object_set_data (G_OBJECT (view), "data-edited", GINT_TO_POINTER (FALSE));
        xaccTransRollbackEdit (view->priv->dirty_trans);

        // Add the split after rollback so it is last in list.
        gnc_tree_model_split_reg_set_blank_split_parent (model, trans, FALSE);

        // Set the transaction to show correct view
        gnc_tree_view_split_reg_format_trans (view, view->priv->dirty_trans);

        gnc_tree_view_split_reg_set_dirty_trans (view, NULL);

        split = gnc_tree_model_split_get_blank_split (model);
        xaccSplitReinit (split); // Clear the blank split
    }
    /* Reset allow changes for reconciled transctions */
    view->change_allowed = FALSE;

    view->priv->auto_complete = FALSE; // reset auto_complete has run flag

    /* This updates the plugin page gui */
    gnc_tree_view_split_reg_call_uiupdate_cb(view);

    LEAVE(" ");
}


/* Make sure we have stopped editing */
void
gnc_tree_view_split_reg_finish_edit (GncTreeViewSplitReg *view)
{
    gtv_sr_finish_edit (view);

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
    GtkTreePath *mpath, *spath;
    gboolean expanded;

    /* if trans is NULL use priv->expanded */
    if (trans == NULL)
        expanded = view->priv->expanded;
    else
    {
        model = gnc_tree_view_split_reg_get_model_from_view (view);

        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, trans);

        spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

        gtk_tree_path_down (spath); /* Move the path down to trow2 */

        expanded = gtk_tree_view_row_expanded (GTK_TREE_VIEW (view), spath);

        gtk_tree_path_free (mpath);
        gtk_tree_path_free (spath);
    }
    return expanded;
}


/* Collapse the transaction, if trans is NULL, use current_ref */
void
gnc_tree_view_split_reg_collapse_trans (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *temp_spath, *mpath, *spath;
    GtkTreeIter m_iter;
    gint *indices;
    RowDepth depth;

    ENTER("gnc_tree_view_split_reg_collapse_trans and trans is %p", trans);

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    /* if trans is NULL use current_ref */
    if (trans == NULL)
        mpath = gtk_tree_row_reference_get_path (view->priv->current_ref);
    else
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, trans);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

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

        /* Get the selection */
        if (gtv_sr_get_model_iter_from_selection (view, gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), &m_iter))
        {
            temp_mpath = gtk_tree_model_get_path (GTK_TREE_MODEL (model), &m_iter);

            /* Update the tree view titles */
            gtv_sr_titles (view, gtk_tree_path_get_depth (temp_mpath));

            /* Save the new model path to path ref */
            gnc_tree_view_split_reg_set_current_path (view, temp_mpath);

            gtk_tree_path_free (temp_mpath);
        }
        gnc_tree_view_split_reg_block_selection (view, FALSE);
    }
    else
        gtk_tree_view_collapse_row (GTK_TREE_VIEW (view), temp_spath);

    gtk_tree_path_free (temp_spath);
    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);

    view->priv->expanded = FALSE;

    /* This updates the plugin page gui */
    gnc_tree_view_split_reg_call_uiupdate_cb(view);

    LEAVE(" ");
}


/* Expands the transaction or the current transaction if NULL */
void
gnc_tree_view_split_reg_expand_trans (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;
    GtkTreePath *start_path, *end_path;
    gint *indices_spath;
    gint num_splits;

    ENTER("gnc_tree_view_split_reg_expand_trans and trans is %p", trans);

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    if (trans == NULL)
        mpath = gtk_tree_row_reference_get_path (view->priv->current_ref);
    else
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, trans);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    gtk_tree_view_expand_row (GTK_TREE_VIEW (view), spath, TRUE);

    view->priv->expanded = TRUE;

    if (view->priv->selection_to_blank_on_expand && (model->style != REG2_STYLE_JOURNAL))
        gtv_sr_selection_to_blank (view);

    /* Get spath indices and the number of splits */
    indices_spath = gtk_tree_path_get_indices (spath);
    num_splits = xaccTransCountSplits (view->priv->current_trans);

    if (gtk_tree_view_get_visible_range (GTK_TREE_VIEW (view), &start_path, &end_path))
    {
        gint *indices_start, *indices_end;
        gint lines = 0;

        /* The first and last visible path */
        indices_start = gtk_tree_path_get_indices (start_path);
        indices_end = gtk_tree_path_get_indices (end_path);

        if (model->use_double_line)
            lines = (indices_end[0] - indices_spath[0])*2;
        else
            lines = indices_end[0] - indices_spath[0];

        if ((num_splits + 1) > lines)
        {
            /* scroll window to show selection when view is idle */
            g_idle_add ((GSourceFunc) gnc_tree_view_split_reg_scroll_to_bsplit, view );
        }
        gtk_tree_path_free (start_path);
        gtk_tree_path_free (end_path);
    }
    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);

    /* This updates the plugin page gui */
    gnc_tree_view_split_reg_call_uiupdate_cb(view);

    LEAVE(" ");
}


/* Return the credit and debit titles of those columns */
const char *
gnc_tree_view_split_reg_get_credit_debit_string (GncTreeViewSplitReg *view, gboolean credit)
{
    GtkCellRenderer *cr0;
    GList *renderers;
    GList *columns;
    GList  *column;
    gint i;
    const char *title = NULL;

    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (view));

    for ( column = columns, i = 1; column; column = g_list_next (column), i++)
    {
        GtkTreeViewColumn *tvc;
        ViewCol viewcol;

        tvc = column->data;

        // Get the first renderer, it has the view-column value.
        renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (tvc));
        cr0 = g_list_nth_data (renderers, 0);
        g_list_free (renderers);

        viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT(cr0), "view_column"));

        DEBUG("viewcol is %d", viewcol);

        if (viewcol == COL_CREDIT && credit)
            title = gtk_tree_view_column_get_title (tvc);

        if (viewcol == COL_DEBIT && !credit)
            title = gtk_tree_view_column_get_title (tvc);
    }
    g_list_free (columns);
    return title;
}


/* Returns the parent Window */
GtkWidget *
gnc_tree_view_split_reg_get_parent (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    model = gnc_tree_view_split_reg_get_model_from_view (view);
    return gnc_tree_model_split_reg_get_parent (model);
}


/* This sets up the page gui update from the tree view motion callback */
void
gnc_tree_view_split_reg_set_uiupdate_cb (GncTreeViewSplitReg *view, GFunc cb, gpointer cb_data)
{
    view->uiupdate_cb = cb;
    view->uiupdate_cb_data = cb_data;
}

/** Call the moved_cb callback that is used to update the page ui, if it is
set. If it is not set, this function does nothing.

\return FALSE so that this function can be used in g_idle_add() */
gboolean gnc_tree_view_split_reg_call_uiupdate_cb(GncTreeViewSplitReg *view)
{
    g_assert(view);
    if (view->uiupdate_cb)
        (view->uiupdate_cb)(view, view->uiupdate_cb_data);
    return FALSE;
}

