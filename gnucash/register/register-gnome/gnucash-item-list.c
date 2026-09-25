/********************************************************************\
 * gnucash-item-list.c -- A scrollable GTK4 list box                     *
 *                                                                  *
 * Initial copyright not recorded.                                  *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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

/*
 *  A scrollable list box.
 */

#include <config.h>

#include <glib/gi18n.h>
#include <gdk/gdk.h>

#include "gnc-engine.h"
#include "gnucash-item-list.h"

#define ITEM_TEXT_KEY "gnc-item-list-text"
#define ITEM_MARKUP_KEY "gnc-item-list-markup"
#define ITEM_WEIGHT_KEY "gnc-item-list-weight"
#define ITEM_FOUND_LOCATION_KEY "gnc-item-list-found-location"

/* Item list signals */
enum
{
    SELECT_ITEM,
    CHANGE_ITEM,
    ACTIVATE_ITEM,
    KEY_PRESSED,
    LAST_SIGNAL
};

static guint gnc_item_list_signals[LAST_SIGNAL];

G_DEFINE_TYPE (GncItemList, gnc_item_list, GTK_TYPE_BOX);

static void row_click_released_cb (GtkGestureClick *gesture, gint n_press,
                                   gdouble x, gdouble y, gpointer user_data);

static const char *
item_text (GObject *item)
{
    return item ? g_object_get_data (item, ITEM_TEXT_KEY) : NULL;
}

static const char *
item_markup (GObject *item)
{
    return item ? g_object_get_data (item, ITEM_MARKUP_KEY) : NULL;
}

static gint
item_found_location (GObject *item)
{
    return item ? GPOINTER_TO_INT (g_object_get_data (item,
                                                       ITEM_FOUND_LOCATION_KEY)) : -1;
}

GListStore *
gnc_item_list_store_new (void)
{
    return g_list_store_new (G_TYPE_OBJECT);
}

void
gnc_item_list_store_clear (GListStore *store)
{
    g_return_if_fail (G_IS_LIST_STORE (store));

    g_list_store_remove_all (store);
}

void
gnc_item_list_store_append (GListStore *store, const char *text,
                            const char *markup, gint weight,
                            gint found_location)
{
    GObject *item;

    g_return_if_fail (G_IS_LIST_STORE (store));
    g_return_if_fail (text != NULL);

    item = g_object_new (G_TYPE_OBJECT, NULL);
    g_object_set_data_full (item, ITEM_TEXT_KEY, g_strdup (text), g_free);
    if (markup)
        g_object_set_data_full (item, ITEM_MARKUP_KEY, g_strdup (markup), g_free);
    g_object_set_data (item, ITEM_WEIGHT_KEY, GINT_TO_POINTER (weight));
    g_object_set_data (item, ITEM_FOUND_LOCATION_KEY,
                       GINT_TO_POINTER (found_location));
    g_list_store_append (store, item);
    g_object_unref (item);
}

static gint
item_compare (gconstpointer item1, gconstpointer item2,
              G_GNUC_UNUSED gpointer user_data)
{
    const char *text1 = item_text (G_OBJECT (item1));
    const char *text2 = item_text (G_OBJECT (item2));
    gint comparison = g_utf8_collate (text1 ? text1 : "", text2 ? text2 : "");

    if (comparison < 0)
        return -1;
    if (comparison > 0)
        return 1;
    return 0;
}

static GListStore *
active_store (GncItemList *item_list)
{
    return item_list->temp_store ? item_list->temp_store : item_list->list_store;
}

static GObject *
active_item_at (GncItemList *item_list, guint position)
{
    if (!item_list->sorted_model)
        return NULL;

    return g_list_model_get_item (G_LIST_MODEL (item_list->sorted_model), position);
}

static void
gnc_item_list_emit_change (GncItemList *item_list)
{
    char *string = gnc_item_list_get_selection (item_list);

    if (!string)
        return;

    g_signal_emit (item_list, gnc_item_list_signals[CHANGE_ITEM], 0, string);
    g_free (string);
}

static void
selection_changed_cb (G_GNUC_UNUSED GtkSelectionModel *model,
                      G_GNUC_UNUSED guint position,
                      G_GNUC_UNUSED guint n_items,
                      gpointer user_data)
{
    gnc_item_list_emit_change (GNC_ITEM_LIST (user_data));
}

static void
gnc_item_list_set_active_store (GncItemList *item_list, GListStore *store)
{
    GtkSortListModel *sorted_model;
    GtkSingleSelection *selection;

    gtk_list_view_set_model (item_list->list_view, NULL);
    g_clear_object (&item_list->selection);
    g_clear_object (&item_list->sorted_model);

    if (!store)
        return;

    sorted_model = gtk_sort_list_model_new
        (G_LIST_MODEL (g_object_ref (store)),
         GTK_SORTER (g_object_ref (item_list->sorter)));
    selection = gtk_single_selection_new
        (G_LIST_MODEL (g_object_ref (sorted_model)));
    gtk_single_selection_set_autoselect (selection, FALSE);
    gtk_single_selection_set_can_unselect (selection, TRUE);
    g_signal_connect (selection, "selection-changed",
                      G_CALLBACK (selection_changed_cb), item_list);

    gtk_list_view_set_model (item_list->list_view,
                             GTK_SELECTION_MODEL (selection));
    g_set_object (&item_list->sorted_model, sorted_model);
    g_set_object (&item_list->selection, selection);
    g_object_unref (selection);
    g_object_unref (sorted_model);
}

static void
list_item_setup_cb (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
                    GtkListItem *list_item,
                    gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);
    GtkGesture *click = gtk_gesture_click_new ();

    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (click),
                                   GDK_BUTTON_PRIMARY);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0f);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_NONE);
    gtk_widget_set_hexpand (label, TRUE);
    gtk_widget_set_margin_start (label, 4);
    gtk_widget_set_margin_end (label, 4);
    g_object_set_data (G_OBJECT (label), "gnc-item-list-list-item", list_item);
    g_signal_connect (click, "released", G_CALLBACK (row_click_released_cb),
                      user_data);
    gtk_widget_add_controller (label, GTK_EVENT_CONTROLLER (click));
    gtk_list_item_set_child (list_item, label);
}

static void
list_item_bind_cb (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
                   GtkListItem *list_item,
                   G_GNUC_UNUSED gpointer user_data)
{
    GObject *item = gtk_list_item_get_item (list_item);
    GtkWidget *child = gtk_list_item_get_child (list_item);
    const char *markup = item_markup (item);

    g_return_if_fail (GTK_IS_LABEL (child));

    if (markup && *markup)
        gtk_label_set_markup (GTK_LABEL (child), markup);
    else
        gtk_label_set_text (GTK_LABEL (child), item_text (item));
}

static void
gnc_item_list_activate_position (GncItemList *item_list, guint position)
{
    GObject *item;
    const char *string;

    if (!item_list->selection || position == GTK_INVALID_LIST_POSITION)
        return;

    gtk_single_selection_set_selected (item_list->selection, position);
    item = active_item_at (item_list, position);
    if (!item)
        return;

    string = item_text (item);
    if (string)
        g_signal_emit (item_list, gnc_item_list_signals[ACTIVATE_ITEM], 0,
                       (gchar *)string);
    g_object_unref (item);
}

static void
row_click_released_cb (GtkGestureClick *gesture,
                       G_GNUC_UNUSED gint n_press,
                       G_GNUC_UNUSED gdouble x,
                       G_GNUC_UNUSED gdouble y,
                       gpointer user_data)
{
    GncItemList *item_list = GNC_ITEM_LIST (user_data);
    GtkWidget *label = gtk_event_controller_get_widget
        (GTK_EVENT_CONTROLLER (gesture));
    GtkListItem *list_item = label ? g_object_get_data
        (G_OBJECT (label), "gnc-item-list-list-item") : NULL;

    if (!list_item)
        return;

    gnc_item_list_activate_position (item_list,
                                     gtk_list_item_get_position (list_item));
}

static void
list_view_activate_cb (G_GNUC_UNUSED GtkListView *list_view, guint position, gpointer user_data)
{
    gnc_item_list_activate_position (GNC_ITEM_LIST (user_data), position);
}

static gboolean
key_pressed_cb (G_GNUC_UNUSED GtkEventControllerKey *controller,
                guint keyval, guint keycode, GdkModifierType state,
                gpointer user_data)
{
    GncItemList *item_list = GNC_ITEM_LIST (user_data);
    gboolean handled = FALSE;
    char *string;

    switch (keyval)
    {
    case GDK_KEY_Return:
    case GDK_KEY_KP_Enter:
        string = gnc_item_list_get_selection (item_list);
        if (!string)
            return FALSE;
        g_signal_emit (item_list, gnc_item_list_signals[ACTIVATE_ITEM], 0,
                       string);
        g_signal_emit (item_list, gnc_item_list_signals[CHANGE_ITEM], 0,
                       string);
        g_free (string);
        return TRUE;

    case GDK_KEY_Page_Up:
    case GDK_KEY_Page_Down:
    case GDK_KEY_Up:
    case GDK_KEY_Down:
    case GDK_KEY_KP_Up:
    case GDK_KEY_KP_Down:
    case GDK_KEY_KP_Page_Up:
    case GDK_KEY_KP_Page_Down:
        return FALSE;
    }

    g_signal_emit (item_list, gnc_item_list_signals[KEY_PRESSED], 0,
                   keyval, keycode, state, &handled);
    return handled;
}

static void
gnc_item_list_dispose (GObject *object)
{
    GncItemList *item_list = GNC_ITEM_LIST (object);

    if (item_list->list_view)
        gtk_list_view_set_model (item_list->list_view, NULL);
    g_clear_object (&item_list->selection);
    g_clear_object (&item_list->sorted_model);
    g_clear_object (&item_list->temp_store);
    g_clear_object (&item_list->list_store);
    g_clear_object (&item_list->sorter);

    G_OBJECT_CLASS (gnc_item_list_parent_class)->dispose (object);
}

static void
gnc_item_list_class_init (GncItemListClass *item_list_class)
{
    GObjectClass *object_class = G_OBJECT_CLASS (item_list_class);

    object_class->dispose = gnc_item_list_dispose;
    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS (item_list_class),
                                   "gnc-id-sheet-list");

    gnc_item_list_signals[SELECT_ITEM] =
        g_signal_new ("select_item", G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (GncItemListClass, select_item),
                      NULL, NULL, g_cclosure_marshal_VOID__POINTER,
                      G_TYPE_NONE, 1, G_TYPE_POINTER);
    gnc_item_list_signals[CHANGE_ITEM] =
        g_signal_new ("change_item", G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (GncItemListClass, change_item),
                      NULL, NULL, g_cclosure_marshal_VOID__POINTER,
                      G_TYPE_NONE, 1, G_TYPE_POINTER);
    gnc_item_list_signals[ACTIVATE_ITEM] =
        g_signal_new ("activate_item", G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (GncItemListClass, activate_item),
                      NULL, NULL, g_cclosure_marshal_VOID__POINTER,
                      G_TYPE_NONE, 1, G_TYPE_POINTER);
    gnc_item_list_signals[KEY_PRESSED] =
        g_signal_new ("key-pressed", G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (GncItemListClass, key_pressed),
                      NULL, NULL, g_cclosure_marshal_generic,
                      G_TYPE_BOOLEAN, 3, G_TYPE_UINT, G_TYPE_UINT,
                      GDK_TYPE_MODIFIER_TYPE);
}

static void
gnc_item_list_init (GncItemList *item_list)
{
    GtkListItemFactory *factory;
    GtkEventController *key_controller;

    gtk_orientable_set_orientation (GTK_ORIENTABLE (item_list),
                                    GTK_ORIENTATION_VERTICAL);
    item_list->sorter = GTK_CUSTOM_SORTER
        (gtk_custom_sorter_new (item_compare, NULL, NULL));
    item_list->scrollwin = GTK_SCROLLED_WINDOW (gtk_scrolled_window_new ());
    gtk_scrolled_window_set_policy (item_list->scrollwin,
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);

    factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (list_item_setup_cb),
                      item_list);
    g_signal_connect (factory, "bind", G_CALLBACK (list_item_bind_cb),
                      item_list);
    item_list->list_view = GTK_LIST_VIEW (gtk_list_view_new (NULL, factory));
    g_signal_connect (item_list->list_view, "activate",
                      G_CALLBACK (list_view_activate_cb), item_list);

    key_controller = gtk_event_controller_key_new ();
    g_signal_connect (key_controller, "key-pressed", G_CALLBACK (key_pressed_cb),
                      item_list);
    gtk_widget_add_controller (GTK_WIDGET (item_list->list_view), key_controller);

    gtk_scrolled_window_set_child (item_list->scrollwin,
                                   GTK_WIDGET (item_list->list_view));
    gtk_box_append (GTK_BOX (item_list), GTK_WIDGET (item_list->scrollwin));
}

GtkWidget *
gnc_item_list_new (GListStore *list_store)
{
    GncItemList *item_list = GNC_ITEM_LIST
        (g_object_new (GNC_TYPE_ITEM_LIST, NULL));

    if (!list_store)
        list_store = gnc_item_list_store_new ();
    else
        g_object_ref (list_store);

    g_set_object (&item_list->list_store, list_store);
    gnc_item_list_set_active_store (item_list, item_list->list_store);
    g_object_unref (list_store);

    return GTK_WIDGET (item_list);
}

gint
gnc_item_list_num_entries (GncItemList *item_list)
{
    GListStore *store;

    g_return_val_if_fail (IS_GNC_ITEM_LIST (item_list), 0);

    store = active_store (item_list);
    return store ? g_list_model_get_n_items (G_LIST_MODEL (store)) : 0;
}

void
gnc_item_list_clear (GncItemList *item_list)
{
    g_return_if_fail (IS_GNC_ITEM_LIST (item_list));
    g_return_if_fail (item_list->list_store != NULL);

    g_signal_handlers_block_matched (item_list->selection, G_SIGNAL_MATCH_DATA,
                                     0, 0, NULL, NULL, item_list);
    gnc_item_list_store_clear (item_list->list_store);
    g_signal_handlers_unblock_matched (item_list->selection, G_SIGNAL_MATCH_DATA,
                                       0, 0, NULL, NULL, item_list);
}

void
gnc_item_list_append (GncItemList *item_list, const char *string)
{
    g_return_if_fail (IS_GNC_ITEM_LIST (item_list));
    g_return_if_fail (item_list->list_store != NULL);

    gnc_item_list_store_append (item_list->list_store, string, NULL, 0, -1);
}

void
gnc_item_list_set_sort_column (GncItemList *item_list,
                               G_GNUC_UNUSED gint column_id)
{
    g_return_if_fail (IS_GNC_ITEM_LIST (item_list));

    gtk_sorter_changed (GTK_SORTER (item_list->sorter),
                        GTK_SORTER_CHANGE_DIFFERENT);
}

gboolean
gnc_item_in_list (GncItemList *item_list, const char *string)
{
    guint n_items;

    g_return_val_if_fail (IS_GNC_ITEM_LIST (item_list), FALSE);

    n_items = g_list_model_get_n_items (G_LIST_MODEL (item_list->list_store));
    for (guint position = 0; position < n_items; position++)
    {
        GObject *item = g_list_model_get_item
            (G_LIST_MODEL (item_list->list_store), position);
        gboolean found = g_strcmp0 (string, item_text (item)) == 0;

        g_object_unref (item);
        if (found)
            return TRUE;
    }
    return FALSE;
}

void
gnc_item_list_select_at (GncItemList *item_list, guint position)
{
    guint n_items;

    g_return_if_fail (IS_GNC_ITEM_LIST (item_list));
    g_return_if_fail (item_list->selection != NULL);

    n_items = item_list->sorted_model ?
        g_list_model_get_n_items (G_LIST_MODEL (item_list->sorted_model)) : 0;
    if (position >= n_items)
        gtk_single_selection_set_selected (item_list->selection,
                                           GTK_INVALID_LIST_POSITION);
    else
        gtk_single_selection_set_selected (item_list->selection, position);
}

void
gnc_item_list_select (GncItemList *item_list, const char *string)
{
    guint n_items;

    g_return_if_fail (IS_GNC_ITEM_LIST (item_list));

    if (!string)
    {
        gnc_item_list_select_at (item_list, GTK_INVALID_LIST_POSITION);
        return;
    }

    n_items = item_list->sorted_model ?
        g_list_model_get_n_items (G_LIST_MODEL (item_list->sorted_model)) : 0;
    for (guint position = 0; position < n_items; position++)
    {
        GObject *item = active_item_at (item_list, position);
        gboolean found = g_strcmp0 (string, item_text (item)) == 0;

        g_object_unref (item);
        if (!found)
            continue;

        gnc_item_list_select_at (item_list, position);
        gnc_item_list_show_selected (item_list);
        return;
    }
}

char *
gnc_item_list_get_selection (GncItemList *item_list)
{
    GObject *item;
    const char *string;

    g_return_val_if_fail (IS_GNC_ITEM_LIST (item_list), NULL);
    if (!item_list->selection)
        return NULL;

    item = gtk_single_selection_get_selected_item (item_list->selection);
    if (!item)
        return NULL;

    string = item_text (item);
    return string ? g_strdup (string) : NULL;
}

gint
gnc_item_list_get_selected_found_location (GncItemList *item_list)
{
    GObject *item;
    gint found_location;

    g_return_val_if_fail (IS_GNC_ITEM_LIST (item_list), -1);
    if (!item_list->selection)
        return -1;

    item = gtk_single_selection_get_selected_item (item_list->selection);
    if (!item)
        return -1;

    found_location = item_found_location (item);
    return found_location;
}

void
gnc_item_list_show_selected (GncItemList *item_list)
{
    guint position;

    g_return_if_fail (IS_GNC_ITEM_LIST (item_list));
    if (!item_list->selection)
        return;

    position = gtk_single_selection_get_selected (item_list->selection);
    if (position != GTK_INVALID_LIST_POSITION)
        gtk_list_view_scroll_to (item_list->list_view, position,
                                 GTK_LIST_SCROLL_FOCUS, NULL);
}

GtkWidget *
gnc_item_list_get_view (GncItemList *item_list)
{
    g_return_val_if_fail (IS_GNC_ITEM_LIST (item_list), NULL);

    return GTK_WIDGET (item_list->list_view);
}

int
gnc_item_list_autosize (GncItemList *item_list)
{
    g_return_val_if_fail (IS_GNC_ITEM_LIST (item_list), 0);

    return 150;
}

void
gnc_item_list_set_temp_store (GncItemList *item_list, GListStore *store)
{
    g_return_if_fail (IS_GNC_ITEM_LIST (item_list));

    if (store)
        g_return_if_fail (G_IS_LIST_STORE (store));

    g_set_object (&item_list->temp_store, store);
    gnc_item_list_set_active_store (item_list, active_store (item_list));
}

gboolean
gnc_item_list_using_temp (GncItemList *item_list)
{
    return item_list && item_list->temp_store;
}

static gint
gnc_item_list_get_cell_height (GncItemList *item_list)
{
    GtkWidget *label;
    gint minimum = 0;

    if (item_list->cell_height)
        return item_list->cell_height;

    label = gtk_label_new (_("List"));
    gtk_widget_measure (label, GTK_ORIENTATION_VERTICAL, -1,
                        &minimum, NULL, NULL, NULL);
    g_object_unref (label);
    item_list->cell_height = MAX (minimum, 1);
    return item_list->cell_height;
}

gint
gnc_item_list_get_popup_height (GncItemList *item_list)
{
    GtkWidget *hscrollbar;
    gint minimum = 0;
    gint count;
    gint height;

    g_return_val_if_fail (IS_GNC_ITEM_LIST (item_list), 0);

    count = gnc_item_list_num_entries (item_list);
    height = count * (gnc_item_list_get_cell_height (item_list) + 2);
    hscrollbar = gtk_scrolled_window_get_hscrollbar (item_list->scrollwin);
    if (hscrollbar && gtk_widget_get_visible (hscrollbar))
    {
        gtk_widget_measure (hscrollbar, GTK_ORIENTATION_VERTICAL, -1,
                            &minimum, NULL, NULL, NULL);
        height += minimum;
    }
    return height;
}
