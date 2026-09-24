/*******************************************************************\
 * dialog-lot-viewer.c -- a GTK4 lot viewer for GnuCash             *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#include <config.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "Account.h"
#include "cap-gains.h"
#include "gnc-commodity.h"
#include "qof.h"
#include "gnc-lot.h"
#include "Scrub.h"
#include "Scrub3.h"
#include "ScrubBusiness.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "gncInvoice.h"
#include "dialog-utils.h"
#include "dialog-lot-viewer.h"
#include "gnc-component-manager.h"
#include "gnc-event.h"
#include "gnc-session.h"
#include "gnc-prefs.h"
#include "gnc-scrub-job-runner.h"
#include "gnc-ui-util.h"
#include "gnc-window.h"
#include "misc-gnome-utils.h"

#define LOT_VIEWER_CM_CLASS "dialog-lot-viewer"
#define GNC_PREFS_GROUP "dialogs.lot-viewer"
#define GNC_PREF_HPOS "hpane-position"
#define GNC_PREF_VPOS "vpane-position"
typedef enum
{
    LOT_COLUMN_TYPE,
    LOT_COLUMN_OPEN,
    LOT_COLUMN_CLOSE,
    LOT_COLUMN_TITLE,
    LOT_COLUMN_BALANCE,
    LOT_COLUMN_GAINS
} LotColumn;
typedef enum
{
    SPLIT_COLUMN_DATE,
    SPLIT_COLUMN_NUM,
    SPLIT_COLUMN_DESCRIPTION,
    SPLIT_COLUMN_AMOUNT,
    SPLIT_COLUMN_VALUE,
    SPLIT_COLUMN_GAIN_LOSS,
    SPLIT_COLUMN_BALANCE
} SplitColumn;

typedef struct _LotRow LotRow;
typedef struct _LotRowClass LotRowClass;
struct _LotRow
{
    GObject parent_instance;
    gchar *type, *title, *balance, *gains;
    time64 open_date, close_date;
    gdouble balance_sort, gains_sort;
    GNCLot *lot;
};
struct _LotRowClass
{
    GObjectClass parent_class;
};
GType lot_row_get_type (void);

G_DEFINE_FINAL_TYPE (LotRow, lot_row, G_TYPE_OBJECT)
enum
{
    LOT_ROW_PROP_0,
    LOT_ROW_PROP_TITLE,
    LOT_ROW_N_PROPERTIES
};
static GParamSpec *lot_row_properties[LOT_ROW_N_PROPERTIES];
static void
lot_row_finalize (GObject *object)
{
    LotRow *row = (LotRow *)object;
    g_clear_pointer (&row->type, g_free);
    g_clear_pointer (&row->title, g_free);
    g_clear_pointer (&row->balance, g_free);
    g_clear_pointer (&row->gains, g_free);
    G_OBJECT_CLASS (lot_row_parent_class)->finalize (object);
}
static void
lot_row_get_property (GObject *object, guint property_id, GValue *value, GParamSpec *pspec)
{
    LotRow *row = (LotRow *)object;
    if (property_id == LOT_ROW_PROP_TITLE)
        g_value_set_string (value, row->title);
    else
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
}
static void
lot_row_set_property (GObject *object, guint property_id, const GValue *value, GParamSpec *pspec)
{
    LotRow *row = (LotRow *)object;
    if (property_id == LOT_ROW_PROP_TITLE)
    {
        g_free (row->title);
        row->title = g_value_dup_string (value);
    }
    else
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
}
static void
lot_row_class_init (LotRowClass *klass)
{
    GObjectClass *oc = G_OBJECT_CLASS (klass);
    oc->finalize = lot_row_finalize;
    oc->get_property = lot_row_get_property;
    oc->set_property = lot_row_set_property;
    lot_row_properties[LOT_ROW_PROP_TITLE] = g_param_spec_string (
        "title", "Title", "The lot title", NULL, G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY);
    g_object_class_install_properties (oc, LOT_ROW_N_PROPERTIES, lot_row_properties);
}
static void
lot_row_init (G_GNUC_UNUSED LotRow *row)
{
}
static LotRow *
lot_row_new (const gchar *type, time64 open, time64 close, const gchar *title, const gchar *balance,
             gdouble bsort, const gchar *gains, gdouble gsort, GNCLot *lot)
{
    LotRow *row = g_object_new (lot_row_get_type (), NULL);
    row->type = g_strdup (type);
    row->open_date = open;
    row->close_date = close;
    g_object_set (row, "title", title, NULL);
    row->balance = g_strdup (balance);
    row->balance_sort = bsort;
    row->gains = g_strdup (gains);
    row->gains_sort = gsort;
    row->lot = lot;
    return row;
}

typedef struct _SplitRow SplitRow;
typedef struct _SplitRowClass SplitRowClass;
struct _SplitRow
{
    GObject parent_instance;
    time64 date;
    gchar *number, *description, *amount, *value, *gain_loss, *balance;
    gdouble amount_sort, value_sort, gain_loss_sort, balance_sort;
    Split *split;
};
struct _SplitRowClass
{
    GObjectClass parent_class;
};
GType split_row_get_type (void);

G_DEFINE_FINAL_TYPE (SplitRow, split_row, G_TYPE_OBJECT)
static void
split_row_finalize (GObject *object)
{
    SplitRow *row = (SplitRow *)object;
    g_clear_pointer (&row->number, g_free);
    g_clear_pointer (&row->description, g_free);
    g_clear_pointer (&row->amount, g_free);
    g_clear_pointer (&row->value, g_free);
    g_clear_pointer (&row->gain_loss, g_free);
    g_clear_pointer (&row->balance, g_free);
    G_OBJECT_CLASS (split_row_parent_class)->finalize (object);
}
static void
split_row_class_init (SplitRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = split_row_finalize;
}
static void
split_row_init (G_GNUC_UNUSED SplitRow *row)
{
}
static SplitRow *
split_row_new (time64 date, const gchar *number, const gchar *description, const gchar *amount,
               gdouble asort, const gchar *value, gdouble vsort, const gchar *gain_loss,
               gdouble gsort, const gchar *balance, gdouble bsort, Split *split)
{
    SplitRow *row = g_object_new (split_row_get_type (), NULL);
    row->date = date;
    row->number = g_strdup (number);
    row->description = g_strdup (description);
    row->amount = g_strdup (amount);
    row->amount_sort = asort;
    row->value = g_strdup (value);
    row->value_sort = vsort;
    row->gain_loss = g_strdup (gain_loss);
    row->gain_loss_sort = gsort;
    row->balance = g_strdup (balance);
    row->balance_sort = bsort;
    row->split = split;
    return row;
}

struct _GNCLotViewer
{
    GtkWidget *window;
    GtkButton *delete_button, *scrub_lot_button, *new_lot_button, *scrub_account_button;
    GtkColumnView *lot_view, *split_in_lot_view, *split_free_view;
    GListStore *lot_store, *split_in_lot_store, *split_free_store;
    GtkSortListModel *lot_sorted, *split_in_lot_sorted, *split_free_sorted;
    GtkSingleSelection *lot_selection;
    GtkMultiSelection *split_in_lot_selection, *split_free_selection;
    GtkTextView *lot_notes;
    GtkEntry *title_entry;
    GtkWidget *split_hpaned;
    GtkButton *add_split_to_lot_button, *remove_split_from_lot_button;
    GtkCheckButton *only_show_open_lots_checkbutton, *show_adjusted_amounts_checkbutton;
    Account *account;
    GNCLot *selected_lot;
    gint component_id;
    GncGUID account_id;
    GncScrubJobRunner *scrub_runner;
    gboolean closing;
};

gboolean
gnc_lot_viewer_account_uses_async_scrub (const Account *account)
{
    return account && !xaccAccountIsAPARType (xaccAccountGetType (account));
}

void
gnc_lot_viewer_set_scrub_controls_sensitive (GtkWidget *lot_button, GtkWidget *account_button,
                                             gboolean sensitive)
{
    gtk_widget_set_sensitive (lot_button, sensitive);
    gtk_widget_set_sensitive (account_button, sensitive);
}

static void gnc_lot_viewer_fill (GNCLotViewer *lv);
static void gnc_split_viewer_fill (GNCLotViewer *lv, GListStore *store, SplitList *split_list);
static void lv_refresh (GNCLotViewer *lv);
static void lv_update_split_buttons (GNCLotViewer *lv);

static gnc_commodity *
find_first_currency (GNCLot *lot)
{
    for (SplitList *node = gnc_lot_get_split_list (lot); node; node = node->next)
        if (gnc_numeric_zero_p (xaccSplitGetAmount (node->data)))
            return xaccTransGetCurrency (xaccSplitGetParent (node->data));
    return NULL;
}
static gnc_numeric
get_realized_gains (GNCLot *lot, gnc_commodity *currency)
{
    gnc_numeric gains = gnc_numeric_zero ();
    if (!currency)
        return gains;
    for (SplitList *node = gnc_lot_get_split_list (lot); node; node = node->next)
    {
        Split *split = node->data;
        if (gnc_numeric_zero_p (xaccSplitGetAmount (split))
            && gnc_commodity_equal (xaccTransGetCurrency (xaccSplitGetParent (split)), currency))
            gains = gnc_numeric_add (gains, xaccSplitGetValue (split), GNC_DENOM_AUTO,
                                     GNC_HOW_DENOM_FIXED);
    }
    return gains;
}
static gchar *
lot_open_date_text (GNCLot *lot)
{
    return !gnc_lot_get_split_list (lot)
               ? g_strdup (_ ("Empty"))
               : qof_print_date (
                     xaccTransGetDate (xaccSplitGetParent (gnc_lot_get_earliest_split (lot))));
}
static gchar *
lot_close_date_text (GNCLot *lot)
{
    if (!gnc_lot_get_split_list (lot))
        return NULL;
    return !gnc_lot_is_closed (lot) ? g_strdup (C_ ("Adjective", "Open"))
                                    : qof_print_date (xaccTransGetDate (
                                          xaccSplitGetParent (gnc_lot_get_latest_split (lot))));
}

static GtkOrdering
ordering_int64 (gint64 a, gint64 b)
{
    return a < b ? GTK_ORDERING_SMALLER : a > b ? GTK_ORDERING_LARGER : GTK_ORDERING_EQUAL;
}
static GtkOrdering
ordering_double (gdouble a, gdouble b)
{
    return a < b ? GTK_ORDERING_SMALLER : a > b ? GTK_ORDERING_LARGER : GTK_ORDERING_EQUAL;
}
static GtkOrdering
ordering_string (const gchar *a, const gchar *b)
{
    gint result = g_strcmp0 (a, b);
    return result < 0   ? GTK_ORDERING_SMALLER
           : result > 0 ? GTK_ORDERING_LARGER
                        : GTK_ORDERING_EQUAL;
}
static GtkOrdering
lot_sort_cb (gconstpointer first, gconstpointer second, gpointer data)
{
    const LotRow *a = first, *b = second;
    switch (GPOINTER_TO_UINT (data))
    {
    case LOT_COLUMN_TYPE:
        return ordering_string (a->type, b->type);
    case LOT_COLUMN_OPEN:
        return ordering_int64 (a->open_date, b->open_date);
    case LOT_COLUMN_CLOSE:
        return ordering_int64 (a->close_date, b->close_date);
    case LOT_COLUMN_TITLE:
        return ordering_string (a->title, b->title);
    case LOT_COLUMN_BALANCE:
        return ordering_double (a->balance_sort, b->balance_sort);
    case LOT_COLUMN_GAINS:
        return ordering_double (a->gains_sort, b->gains_sort);
    default:
        return GTK_ORDERING_EQUAL;
    }
}
static GtkOrdering
split_sort_cb (gconstpointer first, gconstpointer second, gpointer data)
{
    const SplitRow *a = first, *b = second;
    switch (GPOINTER_TO_UINT (data))
    {
    case SPLIT_COLUMN_DATE:
        return ordering_int64 (a->date, b->date);
    case SPLIT_COLUMN_NUM:
        return ordering_string (a->number, b->number);
    case SPLIT_COLUMN_DESCRIPTION:
        return ordering_string (a->description, b->description);
    case SPLIT_COLUMN_AMOUNT:
        return ordering_double (a->amount_sort, b->amount_sort);
    case SPLIT_COLUMN_VALUE:
        return ordering_double (a->value_sort, b->value_sort);
    case SPLIT_COLUMN_GAIN_LOSS:
        return ordering_double (a->gain_loss_sort, b->gain_loss_sort);
    case SPLIT_COLUMN_BALANCE:
        return ordering_double (a->balance_sort, b->balance_sort);
    default:
        return GTK_ORDERING_EQUAL;
    }
}

static void
lot_setup_cb (G_GNUC_UNUSED GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    guint column = GPOINTER_TO_UINT (data);
    GtkWidget *label = gtk_label_new (NULL);
    gtk_label_set_xalign (GTK_LABEL (label),
                          column == LOT_COLUMN_BALANCE || column == LOT_COLUMN_GAINS ? 1.0f : 0.0f);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
}
static void
lot_title_binding_destroy (gpointer data)
{
    GBinding *binding = data;
    g_binding_unbind (binding);
    g_object_unref (binding);
}
static void
lot_bind_cb (G_GNUC_UNUSED GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    LotRow *row = (LotRow *)gtk_list_item_get_item (item);
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (item));
    guint column = GPOINTER_TO_UINT (data);
    gchar *text = NULL;
    if (!row)
    {
        gtk_label_set_text (label, "");
        return;
    }
    switch (column)
    {
    case LOT_COLUMN_TYPE:
        gtk_label_set_text (label, row->type);
        break;
    case LOT_COLUMN_OPEN:
        text = lot_open_date_text (row->lot);
        gtk_label_set_text (label, text ? text : "");
        g_free (text);
        break;
    case LOT_COLUMN_CLOSE:
        text = lot_close_date_text (row->lot);
        gtk_label_set_text (label, text ? text : "");
        g_free (text);
        break;
    case LOT_COLUMN_TITLE:
        g_object_set_data_full (
            G_OBJECT (item), "lot-title-binding",
            g_object_bind_property (row, "title", label, "label", G_BINDING_SYNC_CREATE),
            lot_title_binding_destroy);
        break;
    case LOT_COLUMN_BALANCE:
        gtk_label_set_text (label, row->balance);
        break;
    case LOT_COLUMN_GAINS:
        gtk_label_set_text (label, row->gains);
        break;
    default:
        break;
    }
}
static void
lot_unbind_cb (G_GNUC_UNUSED GtkListItemFactory *factory, GtkListItem *item,
               G_GNUC_UNUSED gpointer data)
{
    g_object_set_data (G_OBJECT (item), "lot-title-binding", NULL);
}
static void
split_setup_cb (G_GNUC_UNUSED GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    guint column = GPOINTER_TO_UINT (data);
    GtkWidget *label = gtk_label_new (NULL);
    gtk_label_set_xalign (GTK_LABEL (label), column >= SPLIT_COLUMN_AMOUNT ? 1.0f : 0.0f);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    if (column == SPLIT_COLUMN_DESCRIPTION)
        gtk_widget_set_size_request (label, 200, -1);
    gtk_list_item_set_child (item, label);
}
static void
split_bind_cb (G_GNUC_UNUSED GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    SplitRow *row = (SplitRow *)gtk_list_item_get_item (item);
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (item));
    guint column = GPOINTER_TO_UINT (data);
    gchar *date = NULL;
    if (!row)
    {
        gtk_label_set_text (label, "");
        return;
    }
    switch (column)
    {
    case SPLIT_COLUMN_DATE:
        date = qof_print_date (row->date);
        gtk_label_set_text (label, date ? date : "");
        g_free (date);
        break;
    case SPLIT_COLUMN_NUM:
        gtk_label_set_text (label, row->number);
        break;
    case SPLIT_COLUMN_DESCRIPTION:
        gtk_label_set_text (label, row->description);
        break;
    case SPLIT_COLUMN_AMOUNT:
        gtk_label_set_text (label, row->amount);
        break;
    case SPLIT_COLUMN_VALUE:
        gtk_label_set_text (label, row->value);
        break;
    case SPLIT_COLUMN_GAIN_LOSS:
        gtk_label_set_text (label, row->gain_loss);
        break;
    case SPLIT_COLUMN_BALANCE:
        gtk_label_set_text (label, row->balance);
        break;
    default:
        break;
    }
}
static GtkColumnViewColumn *
lot_column_new (const gchar *title, LotColumn column)
{
    GtkListItemFactory *factory = GTK_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    GtkColumnViewColumn *view_column;
    GtkCustomSorter *sorter;
    g_signal_connect (factory, "setup", G_CALLBACK (lot_setup_cb), GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (lot_bind_cb), GUINT_TO_POINTER (column));
    if (column == LOT_COLUMN_TITLE)
        g_signal_connect (factory, "unbind", G_CALLBACK (lot_unbind_cb), NULL);
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, column == LOT_COLUMN_TITLE);
    if (column == LOT_COLUMN_OPEN || column == LOT_COLUMN_CLOSE)
        gtk_column_view_column_set_fixed_width (view_column, 110);
    if (column == LOT_COLUMN_TYPE)
        gtk_column_view_column_set_fixed_width (view_column, 55);
    if (column == LOT_COLUMN_BALANCE || column == LOT_COLUMN_GAINS)
        gtk_column_view_column_set_fixed_width (view_column, 115);
    sorter = gtk_custom_sorter_new (lot_sort_cb, GUINT_TO_POINTER (column), NULL);
    gtk_column_view_column_set_sorter (view_column, GTK_SORTER (sorter));
    g_object_unref (sorter);
    return view_column;
}
static GtkColumnViewColumn *
split_column_new (const gchar *title, SplitColumn column)
{
    GtkListItemFactory *factory = GTK_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    GtkColumnViewColumn *view_column;
    GtkCustomSorter *sorter;
    g_signal_connect (factory, "setup", G_CALLBACK (split_setup_cb), GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (split_bind_cb), GUINT_TO_POINTER (column));
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, column == SPLIT_COLUMN_DESCRIPTION);
    if (column == SPLIT_COLUMN_DATE)
        gtk_column_view_column_set_fixed_width (view_column, 110);
    if (column == SPLIT_COLUMN_NUM)
        gtk_column_view_column_set_fixed_width (view_column, 70);
    if (column >= SPLIT_COLUMN_AMOUNT)
        gtk_column_view_column_set_fixed_width (view_column, 100);
    sorter = gtk_custom_sorter_new (split_sort_cb, GUINT_TO_POINTER (column), NULL);
    gtk_column_view_column_set_sorter (view_column, GTK_SORTER (sorter));
    g_object_unref (sorter);
    return view_column;
}
static void
append_lot_columns (GtkColumnView *view)
{
    const gchar *titles[]
        = { N_ ("Type"), N_ ("Opened"), N_ ("Closed"), N_ ("Title"), N_ ("Balance"), N_ ("Gains") };
    for (guint column = LOT_COLUMN_TYPE; column <= LOT_COLUMN_GAINS; column++)
    {
        GtkColumnViewColumn *vc = lot_column_new (_ (titles[column]), column);
        gtk_column_view_append_column (view, vc);
        g_object_unref (vc);
    }
}
static void
append_split_columns (GtkColumnView *view)
{
    const gchar *titles[] = { N_ ("Date"),  N_ ("Num"),       N_ ("Description"), N_ ("Amount"),
                              N_ ("Value"), N_ ("Gain/Loss"), N_ ("Balance") };
    for (guint column = SPLIT_COLUMN_DATE; column <= SPLIT_COLUMN_BALANCE; column++)
    {
        GtkColumnViewColumn *vc = split_column_new (_ (titles[column]), column);
        gtk_column_view_append_column (view, vc);
        g_object_unref (vc);
    }
}
static GNCLot *
selected_lot (GNCLotViewer *lv)
{
    guint position = gtk_single_selection_get_selected (lv->lot_selection);
    LotRow *row;
    GNCLot *lot = NULL;
    if (position == GTK_INVALID_LIST_POSITION)
        return NULL;
    row = (LotRow *)g_list_model_get_item (G_LIST_MODEL (lv->lot_selection), position);
    if (row)
    {
        lot = row->lot;
        g_object_unref (row);
    }
    return lot;
}
/* The returned list owns only its nodes; the account owns the splits. */
static GList *
selected_splits (GtkMultiSelection *selection)
{
    GList *splits = NULL;
    guint count = g_list_model_get_n_items (G_LIST_MODEL (selection));
    for (guint position = 0; position < count; position++)
    {
        SplitRow *row;
        if (!gtk_selection_model_is_selected (GTK_SELECTION_MODEL (selection), position))
            continue;
        row = (SplitRow *)g_list_model_get_item (G_LIST_MODEL (selection), position);
        if (!row)
            continue;
        if (row->split)
            splits = g_list_prepend (splits, row->split);
        g_object_unref (row);
    }
    return g_list_reverse (splits);
}
static gboolean
select_lot (GNCLotViewer *lv, GNCLot *lot)
{
    guint count = g_list_model_get_n_items (G_LIST_MODEL (lv->lot_selection));
    gtk_single_selection_set_selected (lv->lot_selection, GTK_INVALID_LIST_POSITION);
    for (guint position = 0; position < count; position++)
    {
        LotRow *row = (LotRow *)g_list_model_get_item (G_LIST_MODEL (lv->lot_selection), position);
        gboolean matches = row && row->lot == lot;
        if (row)
            g_object_unref (row);
        if (matches)
        {
            gtk_single_selection_set_selected (lv->lot_selection, position);
            return TRUE;
        }
    }
    return FALSE;
}
static void
save_current_lot (GNCLotViewer *lv)
{
    gchar *notes;
    if (!lv->selected_lot)
        return;
    gnc_lot_begin_edit (lv->selected_lot);
    gnc_lot_set_title (lv->selected_lot, gnc_entry_get_text (lv->title_entry));
    notes = xxxgtk_textview_get_text (lv->lot_notes);
    gnc_lot_set_notes (lv->selected_lot, notes);
    g_free (notes);
    gnc_lot_commit_edit (lv->selected_lot);
}
static void
unset_lot (GNCLotViewer *lv)
{
    lv->selected_lot = NULL;
    gnc_entry_set_text (lv->title_entry, "");
    gtk_editable_set_editable (GTK_EDITABLE (lv->title_entry), FALSE);
    xxxgtk_textview_set_text (lv->lot_notes, "");
    gtk_text_view_set_editable (lv->lot_notes, FALSE);
    g_list_store_remove_all (lv->split_in_lot_store);
    gtk_widget_set_sensitive (GTK_WIDGET (lv->delete_button), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (lv->scrub_lot_button), FALSE);
}
static void
show_splits_in_lot (GNCLotViewer *lv)
{
    if (!lv->selected_lot)
    {
        g_list_store_remove_all (lv->split_in_lot_store);
        return;
    }
    gnc_split_viewer_fill (lv, lv->split_in_lot_store, gnc_lot_get_split_list (lv->selected_lot));
}
static void
select_row (GNCLotViewer *lv, GNCLot *lot)
{
    const gchar *text;
    save_current_lot (lv);
    text = gnc_lot_get_title (lot);
    gnc_entry_set_text (lv->title_entry, text ? text : "");
    gtk_editable_set_editable (GTK_EDITABLE (lv->title_entry), TRUE);
    text = gnc_lot_get_notes (lot);
    xxxgtk_textview_set_text (lv->lot_notes, text ? text : "");
    gtk_text_view_set_editable (lv->lot_notes, TRUE);
    lv->selected_lot = lot;
    show_splits_in_lot (lv);
    gtk_widget_set_sensitive (GTK_WIDGET (lv->delete_button), TRUE);
    gtk_widget_set_sensitive (GTK_WIDGET (lv->scrub_lot_button), TRUE);
}
static void
show_splits_free (GNCLotViewer *lv)
{
    SplitList *all = xaccAccountGetSplitList (lv->account), *free_splits = NULL;
    for (SplitList *node = all; node; node = node->next)
        if (!xaccSplitGetLot (node->data))
            free_splits = g_list_prepend (free_splits, node->data);
    free_splits = g_list_reverse (free_splits);
    gnc_split_viewer_fill (lv, lv->split_free_store, free_splits);
    g_list_free (free_splits);
    g_list_free (all);
}
static void
gnc_lot_viewer_fill (GNCLotViewer *lv)
{
    GNCLot *previous = lv->selected_lot;
    LotList *lots = xaccAccountGetLotList (lv->account);
    gboolean is_business_lot = xaccAccountIsAPARType (xaccAccountGetType (lv->account));
    g_list_store_remove_all (lv->lot_store);
    for (LotList *node = lots; node; node = node->next)
    {
        GNCLot *lot = node->data;
        Split *earliest = gnc_lot_get_earliest_split (lot);
        gnc_numeric balance = gnc_lot_get_balance (lot);
        gnc_commodity *currency = find_first_currency (lot);
        gnc_numeric gains = get_realized_gains (lot, currency);
        gchar type[200] = "", balance_text[200], gains_text[200];
        time64 open_date = earliest ? xaccTransGetDate (xaccSplitGetParent (earliest)) : G_MININT64;
        time64 close_date
            = gnc_lot_is_closed (lot)
                  ? xaccTransGetDate (xaccSplitGetParent (gnc_lot_get_latest_split (lot)))
                  : G_MAXINT64;
        LotRow *row;
        if (gtk_check_button_get_active (lv->only_show_open_lots_checkbutton)
            && gnc_lot_is_closed (lot))
            continue;
        if (gncInvoiceGetInvoiceFromLot (lot))
            g_strlcpy (type, "I", sizeof type);
        xaccSPrintAmount (balance_text, balance,
                          gnc_account_print_info (lv->account, is_business_lot));
        xaccSPrintAmount (gains_text, gains, gnc_commodity_print_info (currency, TRUE));
        row = lot_row_new (type, open_date, close_date, gnc_lot_get_title (lot), balance_text,
                           gnc_numeric_to_double (balance), gains_text,
                           gnc_numeric_to_double (gains), lot);
        g_list_store_append (lv->lot_store, row);
        g_object_unref (row);
    }
    g_list_free (lots);
    if (!previous || !select_lot (lv, previous))
        gtk_single_selection_set_selected (lv->lot_selection, GTK_INVALID_LIST_POSITION);
}
static gboolean
can_remove_split (Split *split, GNCLot *lot)
{
    GncInvoice *lot_invoice = gncInvoiceGetInvoiceFromLot (lot);
    return !lot_invoice || lot_invoice != gncInvoiceGetInvoiceFromTxn (xaccSplitGetParent (split));
}
static void
gnc_split_viewer_fill (GNCLotViewer *lv, GListStore *store, SplitList *splits)
{
    gboolean business_lot = xaccAccountIsAPARType (xaccAccountGetType (lv->account));
    gboolean show_adjusted = gtk_check_button_get_active (lv->show_adjusted_amounts_checkbutton);
    gnc_numeric running_balance = gnc_numeric_zero ();
    g_list_store_remove_all (store);
    for (SplitList *node = splits; node; node = node->next)
    {
        Split *split = node->data;
        Transaction *transaction = xaccSplitGetParent (split);
        gnc_commodity *currency = xaccTransGetCurrency (transaction);
        gnc_numeric amount, value, gains;
        gchar amount_text[200], value_text[200], gains_text[200], balance_text[200];
        SplitRow *row;
        amount = xaccSplitGetAdjustedAmount (split);
        if (!business_lot && gnc_numeric_zero_p (amount))
            continue;
        if (xaccSplitIsStockSplit (split))
            continue;
        value = xaccSplitGetValue (split);
        if (lv->selected_lot && !business_lot && node != splits)
            value = gnc_numeric_neg (value);
        gains = store == lv->split_in_lot_store ? xaccSplitGetCapGains (split)
                                                : xaccLotFreeSplitCapGain (split, lv->selected_lot);
        running_balance = gnc_numeric_add_fixed (running_balance, amount);
        xaccSPrintAmount (amount_text, show_adjusted ? amount : xaccSplitGetAmount (split),
                          gnc_account_print_info (lv->account, business_lot));
        xaccSPrintAmount (value_text, value, gnc_commodity_print_info (currency, TRUE));
        if (gnc_numeric_zero_p (gains))
            gains_text[0] = '\0';
        else
            xaccSPrintAmount (gains_text, gains, gnc_commodity_print_info (currency, TRUE));
        if (gnc_numeric_zero_p (running_balance))
            balance_text[0] = '\0';
        else
            xaccSPrintAmount (balance_text, running_balance,
                              gnc_account_print_info (lv->account, business_lot));
        row = split_row_new (
            xaccTransGetDate (transaction), gnc_get_num_action (transaction, split),
            xaccTransGetDescription (transaction), amount_text, gnc_numeric_to_double (amount),
            value_text, gnc_numeric_to_double (value), gains_text, gnc_numeric_to_double (gains),
            balance_text, gnc_numeric_to_double (running_balance), split);
        g_list_store_append (store, row);
        g_object_unref (row);
    }
}
static void
lv_update_split_buttons (GNCLotViewer *lv)
{
    GList *splits;
    gboolean can_remove_all = TRUE;
    gtk_widget_set_sensitive (GTK_WIDGET (lv->add_split_to_lot_button), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (lv->remove_split_from_lot_button), FALSE);
    if (!lv->selected_lot)
        return;
    splits = selected_splits (lv->split_free_selection);
    if (splits)
        gtk_widget_set_sensitive (GTK_WIDGET (lv->add_split_to_lot_button), TRUE);
    g_list_free (splits);
    splits = selected_splits (lv->split_in_lot_selection);
    for (GList *node = splits; node; node = node->next)
        if (!can_remove_split (node->data, lv->selected_lot))
        {
            can_remove_all = FALSE;
            break;
        }
    if (splits && can_remove_all)
        gtk_widget_set_sensitive (GTK_WIDGET (lv->remove_split_from_lot_button), TRUE);
    g_list_free (splits);
}
static void
set_window_title (GNCLotViewer *lv)
{
    gchar *title = g_strdup_printf (_ ("Lots in Account %s"), xaccAccountGetName (lv->account));
    gtk_window_set_title (GTK_WINDOW (lv->window), title);
    g_free (title);
}
static void
set_adjusted_amounts_checkbutton_visibility (GNCLotViewer *lv)
{
    gtk_widget_set_visible (GTK_WIDGET (lv->show_adjusted_amounts_checkbutton),
                            xaccAccountHasStockSplit (lv->account));
}
static void
lv_refresh (GNCLotViewer *lv)
{
    gnc_lot_viewer_fill (lv);
    show_splits_free (lv);
    show_splits_in_lot (lv);
    set_window_title (lv);
    set_adjusted_amounts_checkbutton_visibility (lv);
    lv_update_split_buttons (lv);
}
static void
lv_refresh_handler (GHashTable *changes, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    if (changes)
    {
        const EventInfo *info = gnc_gui_get_entity_events (changes, &lv->account_id);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (lv->component_id);
            return;
        }
    }
    lv_refresh (lv);
}
static void
lv_close_handler (gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    save_current_lot (lv);
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW (lv->window));
    gtk_window_destroy (GTK_WINDOW (lv->window));
}

static void
title_changed_cb (G_GNUC_UNUSED GtkEditable *editable, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    guint position = gtk_single_selection_get_selected (lv->lot_selection);
    LotRow *row;
    if (position == GTK_INVALID_LIST_POSITION)
        return;
    row = (LotRow *)g_list_model_get_item (G_LIST_MODEL (lv->lot_selection), position);
    if (!row)
        return;
    g_object_set (row, "title", gnc_entry_get_text (lv->title_entry), NULL);
    g_object_notify_by_pspec (G_OBJECT (row), lot_row_properties[LOT_ROW_PROP_TITLE]);
    g_object_unref (row);
}
static void
lot_selection_changed_cb (G_GNUC_UNUSED GObject *object, G_GNUC_UNUSED GParamSpec *pspec,
                          gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    GNCLot *lot = selected_lot (lv);
    if (lot)
        select_row (lv, lot);
    else
    {
        save_current_lot (lv);
        unset_lot (lv);
    }
    lv_update_split_buttons (lv);
}
static void
split_selection_changed_cb (G_GNUC_UNUSED GtkSelectionModel *selection,
                            G_GNUC_UNUSED guint position, G_GNUC_UNUSED guint n_items,
                            gpointer user_data)
{
    lv_update_split_buttons (user_data);
}
static void
add_split_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    GList *splits = lv->selected_lot ? selected_splits (lv->split_free_selection) : NULL;
    if (!splits)
        return;
    gnc_suspend_gui_refresh ();
    xaccAccountBeginEdit (lv->account);
    for (GList *node = splits; node; node = node->next)
        gnc_lot_add_split (lv->selected_lot, node->data);
    xaccAccountCommitEdit (lv->account);
    gnc_resume_gui_refresh ();
    g_list_free (splits);
    lv_refresh (lv);
}
static void
remove_split_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    GList *splits = lv->selected_lot ? selected_splits (lv->split_in_lot_selection) : NULL;
    if (!splits)
        return;
    for (GList *node = splits; node; node = node->next)
        if (!can_remove_split (node->data, lv->selected_lot))
        {
            g_list_free (splits);
            return;
        }
    gnc_suspend_gui_refresh ();
    xaccAccountBeginEdit (lv->account);
    for (GList *node = splits; node; node = node->next)
        gnc_lot_remove_split (lv->selected_lot, node->data);
    xaccAccountCommitEdit (lv->account);
    gnc_resume_gui_refresh ();
    g_list_free (splits);
    lv_refresh (lv);
}
static void
open_lots_toggled_cb (G_GNUC_UNUSED GtkCheckButton *button, gpointer user_data)
{
    lv_refresh (user_data);
}
static void
new_lot_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    GNCLot *lot;
    save_current_lot (lv);
    lot = gnc_lot_make_default (lv->account);
    xaccAccountInsertLot (lv->account, lot);
    lv_refresh (lv);
    select_lot (lv, lot);
}
static void
delete_lot_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    GNCLot *lot = lv->selected_lot;
    if (!lot || gncInvoiceGetInvoiceFromLot (lot))
        return;
    xaccAccountRemoveLot (gnc_lot_get_account (lot), lot);
    gnc_lot_destroy (lot);
    unset_lot (lv);
    lv_refresh (lv);
}
static void
adjusted_amounts_toggled_cb (G_GNUC_UNUSED GtkCheckButton *button, gpointer user_data)
{
    lv_refresh (user_data);
}

static void
scrub_job_done (GncScrubJobRunner *runner, GncScrubJobState state, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    if (!lv->closing)
    {
        gnc_lot_viewer_set_scrub_controls_sensitive (GTK_WIDGET (lv->scrub_lot_button),
                                                     GTK_WIDGET (lv->scrub_account_button), TRUE);
        if (state == GNC_SCRUB_JOB_DONE)
            lv_refresh (lv);
    }
    if (lv->scrub_runner == runner)
    {
        lv->scrub_runner = NULL;
        gnc_scrub_job_runner_unref (runner);
    }
}

static void
start_scrub_job (GNCLotViewer *lv, GncScrubJob *job)
{
    if (!job || lv->scrub_runner)
    {
        if (job)
            gnc_scrub_job_free (job);
        return;
    }
    gnc_lot_viewer_set_scrub_controls_sensitive (GTK_WIDGET (lv->scrub_lot_button),
                                                 GTK_WIDGET (lv->scrub_account_button), FALSE);
    lv->scrub_runner = gnc_scrub_job_runner_start (job, G_OBJECT (lv->window), NULL, 1, NULL,
                                                   scrub_job_done, lv, NULL);
    if (!lv->scrub_runner)
    {
        gnc_scrub_job_free (job);
        gnc_lot_viewer_set_scrub_controls_sensitive (GTK_WIDGET (lv->scrub_lot_button),
                                                     GTK_WIDGET (lv->scrub_account_button), TRUE);
    }
}

static void
scrub_lot_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    if (!lv->selected_lot || lv->scrub_runner)
        return;
    if (!gnc_lot_viewer_account_uses_async_scrub (lv->account))
    {
        GncScrubContext *context
            = gnc_scrub_context_begin (qof_instance_get_book (QOF_INSTANCE (lv->account)));
        if (!context)
            return;
        gncScrubBusinessLotWithContext (lv->selected_lot, context);
        gnc_scrub_context_end (context);
        gnc_scrub_context_unref (context);
        lv_refresh (lv);
    }
    else
        start_scrub_job (lv, gnc_scrub_lot_job_begin (lv->selected_lot));
}

static void
scrub_account_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    if (lv->scrub_runner)
        return;
    if (!gnc_lot_viewer_account_uses_async_scrub (lv->account))
    {
        GncScrubContext *context
            = gnc_scrub_context_begin (qof_instance_get_book (QOF_INSTANCE (lv->account)));
        if (!context)
            return;
        gnc_suspend_gui_refresh ();
        gncScrubBusinessAccountLotsWithContext (lv->account, gnc_window_show_progress, context);
        gnc_scrub_context_end (context);
        gnc_scrub_context_unref (context);
        gnc_resume_gui_refresh ();
        lv_refresh (lv);
    }
    else
        start_scrub_job (lv, gnc_scrub_lots_job_begin (lv->account, FALSE));
}

static void
close_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    gnc_close_gui_component_by_data (LOT_VIEWER_CM_CLASS, user_data);
}
static gboolean
close_request_cb (G_GNUC_UNUSED GtkWindow *window, gpointer user_data)
{
    gnc_close_gui_component_by_data (LOT_VIEWER_CM_CLASS, user_data);
    return TRUE;
}
static gboolean
escape_cb (G_GNUC_UNUSED GtkWidget *widget, G_GNUC_UNUSED GVariant *args, gpointer user_data)
{
    gnc_close_gui_component_by_data (LOT_VIEWER_CM_CLASS, user_data);
    return TRUE;
}
static void
window_destroy_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    lv->closing = TRUE;
    if (lv->scrub_runner)
        gnc_scrub_job_runner_cancel (lv->scrub_runner);
    gnc_unregister_gui_component (lv->component_id);
    g_clear_object (&lv->lot_selection);
    g_clear_object (&lv->lot_sorted);
    g_clear_object (&lv->lot_store);
    g_clear_object (&lv->split_in_lot_selection);
    g_clear_object (&lv->split_in_lot_sorted);
    g_clear_object (&lv->split_in_lot_store);
    g_clear_object (&lv->split_free_selection);
    g_clear_object (&lv->split_free_sorted);
    g_clear_object (&lv->split_free_store);
    g_free (lv);
}
static void
split_paned_realize_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    gint width;
    gtk_window_get_default_size (GTK_WINDOW (lv->window), &width, NULL);
    gtk_paned_set_position (GTK_PANED (lv->split_hpaned), width / 2);
}
static void
add_shortcuts (GNCLotViewer *lv)
{
    GtkShortcutController *controller = GTK_SHORTCUT_CONTROLLER (gtk_shortcut_controller_new ());
    gtk_shortcut_controller_set_scope (controller, GTK_SHORTCUT_SCOPE_MANAGED);
    gtk_shortcut_controller_add_shortcut (
        controller, gtk_shortcut_new (gtk_keyval_trigger_new (GDK_KEY_Escape, 0),
                                      gtk_callback_action_new (escape_cb, lv, NULL)));
    gtk_widget_add_controller (lv->window, GTK_EVENT_CONTROLLER (controller));
}

static void
init_lot_view (GNCLotViewer *lv)
{
    GtkSorter *sorter;
    lv->lot_store = g_list_store_new (lot_row_get_type ());
    append_lot_columns (lv->lot_view);
    sorter = g_object_ref (gtk_column_view_get_sorter (lv->lot_view));
    lv->lot_sorted = gtk_sort_list_model_new (G_LIST_MODEL (g_object_ref (lv->lot_store)), sorter);
    lv->lot_selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (lv->lot_sorted)));
    gtk_single_selection_set_autoselect (lv->lot_selection, FALSE);
    gtk_single_selection_set_can_unselect (lv->lot_selection, TRUE);
    gtk_column_view_set_model (lv->lot_view, GTK_SELECTION_MODEL (lv->lot_selection));
    gtk_column_view_set_show_row_separators (
        lv->lot_view, gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL));
    gtk_column_view_set_show_column_separators (
        lv->lot_view, gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL));
    g_signal_connect (lv->lot_selection, "notify::selected", G_CALLBACK (lot_selection_changed_cb),
                      lv);
    g_signal_connect (lv->only_show_open_lots_checkbutton, "toggled",
                      G_CALLBACK (open_lots_toggled_cb), lv);
}
static void
init_split_view (GNCLotViewer *lv, GtkColumnView *view, GListStore **store,
                 GtkSortListModel **sorted, GtkMultiSelection **selection)
{
    GtkSorter *sorter;
    *store = g_list_store_new (split_row_get_type ());
    append_split_columns (view);
    sorter = g_object_ref (gtk_column_view_get_sorter (view));
    *sorted = gtk_sort_list_model_new (G_LIST_MODEL (g_object_ref (*store)), sorter);
    *selection = gtk_multi_selection_new (G_LIST_MODEL (g_object_ref (*sorted)));
    gtk_column_view_set_model (view, GTK_SELECTION_MODEL (*selection));
    gtk_column_view_set_show_row_separators (
        view, gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL));
    gtk_column_view_set_show_column_separators (
        view, gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL));
    g_signal_connect (*selection, "selection-changed", G_CALLBACK (split_selection_changed_cb), lv);
}
static void
lv_create (GNCLotViewer *lv, GtkWindow *parent)
{
    GtkBuilder *builder = gtk_builder_new ();
    GObject *object;
    gnc_builder_add_from_file (builder, "dialog-lot-viewer.glade", "lot_viewer_dialog");
    lv->window = GTK_WIDGET (gtk_builder_get_object (builder, "lot_viewer_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW (lv->window), parent);
    gtk_widget_set_name (lv->window, "gnc-id-lot-viewer");
    lv->delete_button = GTK_BUTTON (gtk_builder_get_object (builder, "delete_button"));
    lv->scrub_lot_button = GTK_BUTTON (gtk_builder_get_object (builder, "scrub_lot_button"));
    lv->new_lot_button = GTK_BUTTON (gtk_builder_get_object (builder, "new_lot_button"));
    lv->scrub_account_button
        = GTK_BUTTON (gtk_builder_get_object (builder, "scrub_account_button"));
    lv->lot_view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "lot_view"));
    lv->only_show_open_lots_checkbutton
        = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "only_show_open_lots_checkbutton"));
    lv->show_adjusted_amounts_checkbutton
        = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "show_adjusted_amounts_checkbutton"));
    lv->lot_notes = GTK_TEXT_VIEW (gtk_builder_get_object (builder, "lot_notes_text"));
    lv->title_entry = GTK_ENTRY (gtk_builder_get_object (builder, "lot_title_entry"));
    lv->split_in_lot_view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "split_in_lot_view"));
    lv->split_free_view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "split_free_view"));
    lv->split_hpaned = GTK_WIDGET (gtk_builder_get_object (builder, "split_hpaned"));
    lv->add_split_to_lot_button
        = GTK_BUTTON (gtk_builder_get_object (builder, "add_split_to_lot_button"));
    lv->remove_split_from_lot_button
        = GTK_BUTTON (gtk_builder_get_object (builder, "remove_split_from_lot_button"));
    init_lot_view (lv);
    init_split_view (lv, lv->split_free_view, &lv->split_free_store, &lv->split_free_sorted,
                     &lv->split_free_selection);
    init_split_view (lv, lv->split_in_lot_view, &lv->split_in_lot_store, &lv->split_in_lot_sorted,
                     &lv->split_in_lot_selection);
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
    {
        object = gtk_builder_get_object (builder, "lot_vpaned");
        gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_VPOS, NULL, object, "position");
        object = gtk_builder_get_object (builder, "lot_hpaned");
        gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_HPOS, NULL, object, "position");
    }
    g_signal_connect (lv->title_entry, "changed", G_CALLBACK (title_changed_cb), lv);
    g_signal_connect (lv->add_split_to_lot_button, "clicked", G_CALLBACK (add_split_cb), lv);
    g_signal_connect (lv->remove_split_from_lot_button, "clicked", G_CALLBACK (remove_split_cb),
                      lv);
    g_signal_connect (lv->new_lot_button, "clicked", G_CALLBACK (new_lot_cb), lv);
    g_signal_connect (lv->delete_button, "clicked", G_CALLBACK (delete_lot_cb), lv);
    g_signal_connect (lv->scrub_lot_button, "clicked", G_CALLBACK (scrub_lot_cb), lv);
    g_signal_connect (lv->scrub_account_button, "clicked", G_CALLBACK (scrub_account_cb), lv);
    g_signal_connect (lv->show_adjusted_amounts_checkbutton, "toggled",
                      G_CALLBACK (adjusted_amounts_toggled_cb), lv);
    g_signal_connect (gtk_builder_get_object (builder, "close_button"), "clicked",
                      G_CALLBACK (close_cb), lv);
    g_signal_connect (lv->window, "close-request", G_CALLBACK (close_request_cb), lv);
    g_signal_connect (lv->window, "destroy", G_CALLBACK (window_destroy_cb), lv);
    g_signal_connect (lv->window, "realize", G_CALLBACK (split_paned_realize_cb), lv);
    add_shortcuts (lv);
    g_object_unref (builder);
    lv_update_split_buttons (lv);
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW (lv->window), parent);
}
GNCLotViewer *
gnc_lot_viewer_dialog (GtkWindow *parent, Account *account)
{
    GNCLotViewer *lv;
    if (!account)
        return NULL;
    lv = g_new0 (GNCLotViewer, 1);
    lv->account = account;
    lv_create (lv, parent);
    lv_refresh (lv);
    lv->component_id = gnc_register_gui_component (LOT_VIEWER_CM_CLASS, lv_refresh_handler,
                                                   lv_close_handler, lv);
    lv->account_id = *xaccAccountGetGUID (account);
    gnc_gui_component_set_session (lv->component_id, gnc_get_current_session ());
    gnc_gui_component_watch_entity_type (lv->component_id, GNC_ID_LOT,
                                         QOF_EVENT_CREATE | QOF_EVENT_ADD | QOF_EVENT_REMOVE
                                             | QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
    gnc_gui_component_watch_entity (lv->component_id, &lv->account_id,
                                    QOF_EVENT_MODIFY | QOF_EVENT_DESTROY | GNC_EVENT_ITEM_CHANGED);
    gnc_window_adjust_for_screen (GTK_WINDOW (lv->window));
    gtk_widget_set_visible (lv->window, TRUE);
    return lv;
}

/* ============================ END OF FILE =============================== */
