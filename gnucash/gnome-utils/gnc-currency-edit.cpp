/*
 * gnc-currency-edit.c --  Currency editor widget
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 */

/** @addtogroup GUI
    @{ */
/** @addtogroup GncCurrencyEdit
 * @{ */
/** @file gnc-currency-edit.c
 *  @brief Currency selection widget.
 *  @author Dave Peticolas <dave@krondo.com>
 *  @author David Hampton <hampton@employees.org>
 *
 *  This widget is a GTK4 composite selector that is wrapped with support
 *  functions for building/selecting from a list of ISO4217 currency
 *  names.  All data is maintained within the widget itself, which
 *  makes the name/item lookup functions somewhat complicated.  The
 *  alternative coding would be to keep an auxiliary list of strings
 *  attached to the widget for lookup purposes, but that would be 100%
 *  redundant information.
 *
 *  This function currently builds a new string model for each widget
 *  created.  It could be optimized to build a single string model and
 *  share across all extant version of the widget, or even build the
 *  list store once and maintain for the life of the application.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-currency-edit.h"
#include "gnc-commodity.h"
#include "gnc-ui-util.h"
#include "gnc-engine.h"
#include "gnc-gtk-utils.h"

/** The debugging module used by this file. */

static void gnc_currency_edit_finalize     (GObject *object);
static void gnc_currency_edit_entry_changed (GtkEditable *editable,
        gpointer     user_data);
static void gnc_currency_edit_item_setup (GtkSignalListItemFactory *factory,
        GtkListItem *list_item, gpointer user_data);
static void gnc_currency_edit_item_bind (GtkSignalListItemFactory *factory,
        GtkListItem *list_item, gpointer user_data);
static void gnc_currency_edit_list_activate (GtkListView *list_view,
        guint position, gpointer user_data);
static void gnc_currency_edit_popover_visible (GObject *object,
        GParamSpec *pspec, gpointer user_data);
static gboolean gnc_currency_edit_entry_key_pressed (
        GtkEventControllerKey *controller, guint keyval, guint keycode,
        GdkModifierType state, gpointer user_data);

struct _GNCCurrencyEdit
{
    GtkBox parent_instance;

    gchar *mnemonic;
    GtkEntry *entry;
    GListStore *model;
    GtkMenuButton *menu_button;
    GtkPopover *popover;
    GtkListView *popup_view;
    GtkSingleSelection *popup_selection;
    gnc_commodity *currency;
    gboolean updating;
};

G_DEFINE_TYPE(GNCCurrencyEdit, gnc_currency_edit, GTK_TYPE_BOX)

/** @name Basic Object Implementation */
/** @{ */

enum
{
    PROP_0,

    PROP_GCE_MNEMONIC,

    N_PROPERTIES
};

static GParamSpec *obj_properties[N_PROPERTIES] = { nullptr, };

enum
{
    SIGNAL_CHANGED,

    N_SIGNALS
};

static guint currency_edit_signals[N_SIGNALS] = { 0, };

constexpr const char *CURRENCY_DATA = "gnc-currency-edit-currency";

static void fill_currencies (GNCCurrencyEdit *gce);

static guint
currency_position (GNCCurrencyEdit *gce, const gnc_commodity *currency)
{
    auto count = g_list_model_get_n_items (G_LIST_MODEL (gce->model));

    for (guint position = 0; position < count; position++)
    {
        auto item = g_list_model_get_item (G_LIST_MODEL (gce->model), position);
        auto item_currency = static_cast<gnc_commodity *> (
            g_object_get_data (G_OBJECT (item), CURRENCY_DATA));
        g_object_unref (item);
        if (item_currency == currency)
            return position;
    }
    return GTK_INVALID_LIST_POSITION;
}

static gnc_commodity *
currency_from_text (GNCCurrencyEdit *gce, const char *text)
{
    if (!text || !*text)
        return nullptr;

    auto currency = gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                                  GNC_COMMODITY_NS_CURRENCY, text);
    if (currency)
        return currency;

    auto folded = g_utf8_casefold (text, -1);
    auto count = g_list_model_get_n_items (G_LIST_MODEL (gce->model));
    for (guint position = 0; position < count; position++)
    {
        auto item = g_list_model_get_item (G_LIST_MODEL (gce->model), position);
        auto name = gtk_string_object_get_string (GTK_STRING_OBJECT (item));
        auto name_folded = g_utf8_casefold (name, -1);
        auto matches = g_strcmp0 (folded, name_folded) == 0;
        g_free (name_folded);

        if (matches)
        {
            currency = static_cast<gnc_commodity *> (
                g_object_get_data (G_OBJECT (item), CURRENCY_DATA));
            g_object_unref (item);
            break;
        }
        g_object_unref (item);
    }
    g_free (folded);
    return currency;
}

static void
set_mnemonic (GNCCurrencyEdit *gce, const char *mnemonic)
{
    if (g_strcmp0 (gce->mnemonic, mnemonic) == 0)
        return;

    g_free (gce->mnemonic);
    gce->mnemonic = g_strdup (mnemonic);
    g_object_notify_by_pspec (G_OBJECT (gce), obj_properties[PROP_GCE_MNEMONIC]);
}

static void
gnc_currency_edit_set_property (GObject      *object,
                                guint         property_id,
                                const GValue *value,
                                GParamSpec   *pspec)
{
    GNCCurrencyEdit *self = GNC_CURRENCY_EDIT (object);

    switch (property_id)
    {
    case PROP_GCE_MNEMONIC:
        if (self->updating)
            set_mnemonic (self, g_value_get_string (value));
        else
        {
            auto currency = currency_from_text (self, g_value_get_string (value));
            if (!currency)
                currency = gnc_locale_default_currency ();
            gnc_currency_edit_set_currency (self, currency);
        }
        break;

    default:
        /* We don't have any other property... */
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        break;
    }
}

static void
gnc_currency_edit_get_property (GObject    *object,
                                guint       property_id,
                                GValue     *value,
                                GParamSpec *pspec)
{
    GNCCurrencyEdit *self = GNC_CURRENCY_EDIT (object);

    switch (property_id)
    {
    case PROP_GCE_MNEMONIC:
        g_value_set_string (value, self->mnemonic);
        break;

    default:
        /* We don't have any other property... */
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        break;
    }
}

/** Initialize the GncCurrencyEdit class object.
 *
 *  @internal
 *
 *  @param klass A pointer to the newly created class object.
 */
static void
gnc_currency_edit_class_init (GNCCurrencyEditClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

    gobject_class->set_property = gnc_currency_edit_set_property;
    gobject_class->get_property = gnc_currency_edit_get_property;
    gobject_class->finalize     = gnc_currency_edit_finalize;

    obj_properties[PROP_GCE_MNEMONIC] =
        g_param_spec_string ("mnemonic",
                             "Active currency's mnemonic",
                             "Active currency's mnemonic",
                             "USD" /* default value */,
                             G_PARAM_READWRITE);

    g_object_class_install_properties (gobject_class,
                                       N_PROPERTIES,
                                       obj_properties);

    currency_edit_signals[SIGNAL_CHANGED] =
        g_signal_new ("changed", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,
                      0, nullptr, nullptr, nullptr, G_TYPE_NONE, 0);
}


/** Initialize a GncCurrencyEdit object.
 *
 *  @internal
 *
 *  @param gce A pointer to the newly created object.
 */
static void
gnc_currency_edit_init (GNCCurrencyEdit *gce)
{
    GtkListItemFactory *factory;
    GtkWidget *list_view;
    GtkWidget *scroller;
    GtkEventController *key_controller;

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gce), "gnc-id-currency-edit");
    gtk_orientable_set_orientation (GTK_ORIENTABLE (gce), GTK_ORIENTATION_HORIZONTAL);
    gtk_box_set_spacing (GTK_BOX (gce), 6);

    gce->model = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    fill_currencies (gce);
    gce->entry = GTK_ENTRY (gtk_entry_new ());
    gce->popup_selection = gtk_single_selection_new (
        G_LIST_MODEL (g_object_ref (gce->model)));
    gtk_single_selection_set_autoselect (gce->popup_selection, FALSE);
    gtk_single_selection_set_can_unselect (gce->popup_selection, TRUE);
    gtk_single_selection_set_selected (gce->popup_selection,
                                       GTK_INVALID_LIST_POSITION);

    factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup",
                      G_CALLBACK (gnc_currency_edit_item_setup), nullptr);
    g_signal_connect (factory, "bind",
                      G_CALLBACK (gnc_currency_edit_item_bind), nullptr);
    list_view = gtk_list_view_new (
        GTK_SELECTION_MODEL (g_object_ref (gce->popup_selection)), factory);
    gce->popup_view = GTK_LIST_VIEW (list_view);
    gtk_list_view_set_single_click_activate (GTK_LIST_VIEW (list_view), TRUE);

    scroller = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroller),
                                    GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scroller), list_view);
    gtk_scrolled_window_set_propagate_natural_width (
        GTK_SCROLLED_WINDOW (scroller), TRUE);
    gtk_scrolled_window_set_propagate_natural_height (
        GTK_SCROLLED_WINDOW (scroller), TRUE);
    gtk_scrolled_window_set_max_content_height (GTK_SCROLLED_WINDOW (scroller),
                                                240);
    gce->popover = GTK_POPOVER (gtk_popover_new ());
    gtk_popover_set_child (gce->popover, scroller);
    gce->menu_button = GTK_MENU_BUTTON (gtk_menu_button_new ());
    gtk_menu_button_set_icon_name (gce->menu_button, "pan-down-symbolic");
    gtk_widget_set_tooltip_text (GTK_WIDGET (gce->menu_button),
                                 _("Show available currencies"));
    gtk_accessible_update_property (
        GTK_ACCESSIBLE (gce->menu_button), GTK_ACCESSIBLE_PROPERTY_LABEL,
        _("Show available currencies"), -1);
    gtk_menu_button_set_popover (gce->menu_button,
                                 GTK_WIDGET (gce->popover));

    gtk_widget_set_hexpand (GTK_WIDGET (gce->entry), TRUE);
    gtk_box_append (GTK_BOX (gce), GTK_WIDGET (gce->entry));
    gtk_box_append (GTK_BOX (gce), GTK_WIDGET (gce->menu_button));

    g_signal_connect (gce->entry, "changed",
                      G_CALLBACK (gnc_currency_edit_entry_changed), gce);
    g_signal_connect (list_view, "activate",
                      G_CALLBACK (gnc_currency_edit_list_activate), gce);
    g_signal_connect (gce->popover, "notify::visible",
                      G_CALLBACK (gnc_currency_edit_popover_visible), gce);
    key_controller = gtk_event_controller_key_new ();
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (gnc_currency_edit_entry_key_pressed), gce);
    gtk_widget_add_controller (GTK_WIDGET (gce->entry), key_controller);
}


/** Finalize the GncCurrencyEdit object.  This function is called from
 *  the G_Object level to complete the destruction of the object.  It
 *  should release any memory not previously released by the destroy
 *  function (i.e. the private data structure), then chain up to the
 *  parent's destroy function.
 *
 *  @param object The object being destroyed.
 *
 *  @internal
 */
static void
gnc_currency_edit_finalize (GObject *object)
{
    g_return_if_fail (object != nullptr);
    g_return_if_fail (GNC_IS_CURRENCY_EDIT (object));

    GNCCurrencyEdit *self = GNC_CURRENCY_EDIT(object);

    g_free (self->mnemonic);
    g_clear_object (&self->popup_selection);
    g_clear_object (&self->model);

    G_OBJECT_CLASS(gnc_currency_edit_parent_class)->finalize (object);
}


static void
gnc_currency_edit_entry_changed (GtkEditable *editable, gpointer user_data)
{
    auto self = GNC_CURRENCY_EDIT (user_data);

    if (self->updating)
        return;

    auto currency = currency_from_text (self, gtk_editable_get_text (editable));
    if (currency)
        gnc_currency_edit_set_currency (self, currency);
    else
    {
        self->currency = nullptr;
        self->updating = TRUE;
        gtk_single_selection_set_selected (self->popup_selection,
                                           GTK_INVALID_LIST_POSITION);
        self->updating = FALSE;
    }
}

static void
gnc_currency_edit_item_setup (GtkSignalListItemFactory *, GtkListItem *list_item,
                              gpointer)
{
    auto label = gtk_label_new (nullptr);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_list_item_set_child (list_item, label);
}

static void
gnc_currency_edit_item_bind (GtkSignalListItemFactory *, GtkListItem *list_item,
                             gpointer)
{
    auto item = GTK_STRING_OBJECT (gtk_list_item_get_item (list_item));
    auto label = GTK_LABEL (gtk_list_item_get_child (list_item));

    gtk_label_set_text (label, gtk_string_object_get_string (item));
}

static void
gnc_currency_edit_list_activate (GtkListView *, guint position,
                                 gpointer user_data)
{
    auto self = GNC_CURRENCY_EDIT (user_data);
    auto item = g_list_model_get_item (G_LIST_MODEL (self->model), position);
    auto currency = static_cast<gnc_commodity *> (g_object_get_data (G_OBJECT (item), CURRENCY_DATA));
    g_object_unref (item);

    gtk_menu_button_popdown (self->menu_button);
    gnc_currency_edit_set_currency (self, currency);
}

static void
gnc_currency_edit_popover_visible (GObject *object, GParamSpec *,
                                   gpointer user_data)
{
    auto self = GNC_CURRENCY_EDIT (user_data);

    if (gtk_widget_get_visible (GTK_WIDGET (object)))
    {
        gtk_single_selection_set_selected (
            self->popup_selection,
            self->currency ? currency_position (self, self->currency)
                           : GTK_INVALID_LIST_POSITION);
        gtk_widget_grab_focus (GTK_WIDGET (self->popup_view));
    }
    else
        gtk_widget_grab_focus (GTK_WIDGET (self->entry));
}

static gboolean
gnc_currency_edit_entry_key_pressed (GtkEventControllerKey *, guint keyval,
                                     guint, GdkModifierType state,
                                     gpointer user_data)
{
    auto self = GNC_CURRENCY_EDIT (user_data);

    if (keyval != GDK_KEY_Down || !(state & GDK_ALT_MASK))
        return FALSE;
    gtk_menu_button_popup (self->menu_button);
    return TRUE;
}

/** This auxiliary function adds a single currency name to the GTK4
 *  string model. It is called as an iterator function when running a list of
 *  currencies.
 *
 *  @internal
 *
 *  @param commodity The currency to add to the selection widget.
 *
 *  @param gce A pointer to the selection widget.
 */
static void
add_item(gnc_commodity *commodity, GNCCurrencyEdit *gce)
{
    const char *string;

    string = gnc_commodity_get_printname(commodity);
    auto item = gtk_string_object_new (string);
    g_object_set_data (G_OBJECT (item), CURRENCY_DATA, commodity);
    g_list_store_append (gce->model, item);
    g_object_unref (item);
}


static gint
currency_compare (gconstpointer a, gconstpointer b)
{
    return g_utf8_collate (gnc_commodity_get_printname (GNC_COMMODITY (a)),
                           gnc_commodity_get_printname (GNC_COMMODITY (b)));
}

/** This auxiliary function adds all the currency names to a string
 *  model.
 *
 *  @internal
 *
 *  @param gce A pointer to the widget that should be filled with
 *  currency names.
 */
static void
fill_currencies(GNCCurrencyEdit *gce)
{
    GList *currencies;

    currencies = gnc_commodity_table_get_commodities
                 (gnc_get_current_commodities (), GNC_COMMODITY_NS_CURRENCY);
    currencies = g_list_sort (currencies, currency_compare);
    g_list_foreach(currencies, (GFunc)add_item, gce);
    g_list_free(currencies);
}

/*  Create a new GNCCurrencyEdit widget which can be used to provide
 *  an easy way to enter ISO currency codes.
 *
 *  @return A GNCCurrencyEdit widget.
 */
GtkWidget *
gnc_currency_edit_new (void)
{
    auto gce = GNC_CURRENCY_EDIT (g_object_new (GNC_TYPE_CURRENCY_EDIT, nullptr));

    return GTK_WIDGET (gce);
}

/** @} */

/** @name Get/Set Functions */
/** @{ */

/*  Set the widget to display a certain currency name.
 *
 *  @param gce The currency editor widget to set.
 *
 *  @param currency The currency to set as the displayed/selected
 *  value of the widget.
 */
void
gnc_currency_edit_set_currency (GNCCurrencyEdit *gce,
                                const gnc_commodity *currency)
{
    g_return_if_fail(gce != nullptr);
    g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));
    g_return_if_fail(currency != nullptr);

    auto changed = gce->currency != currency;
    auto position = currency_position (gce, currency);

    gce->updating = TRUE;
    gce->currency = const_cast<gnc_commodity *> (currency);
    gtk_single_selection_set_selected (gce->popup_selection, position);
    gtk_editable_set_text (GTK_EDITABLE (gce->entry),
                           gnc_commodity_get_printname (currency));
    set_mnemonic (gce, gnc_commodity_get_mnemonic (currency));
    gce->updating = FALSE;

    if (changed)
        g_signal_emit (gce, currency_edit_signals[SIGNAL_CHANGED], 0);
}


/*  Retrieve the displayed currency of the widget.
 *
 *  @param gce The currency editor widget whose values should be retrieved.
 *
 *  @return A pointer to the selected currency (a gnc_commodity
 *  structure).
 */
gnc_commodity *
gnc_currency_edit_get_currency (GNCCurrencyEdit *gce)
{
    g_return_val_if_fail(gce != nullptr, nullptr);
    g_return_val_if_fail(GNC_IS_CURRENCY_EDIT(gce), nullptr);

    return gce->currency ? gce->currency : gnc_locale_default_currency ();
}

/** Clear the displayed currency of the widget.
 *
 *  This will clear the currency being displayed just like when first created
 *  but it still returns the default currency as usual
 *
 *  @param gce The currency editor widget whose values should be retrieved.
 */
void
gnc_currency_edit_clear_display (GNCCurrencyEdit *gce)
{
    g_return_if_fail(gce != nullptr);
    g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));

    gce->updating = TRUE;
    gce->currency = nullptr;
    gtk_single_selection_set_selected (gce->popup_selection,
                                       GTK_INVALID_LIST_POSITION);
    gtk_editable_set_text (GTK_EDITABLE (gce->entry), "");
    gce->updating = FALSE;
}

/** @} */
/** @} */
/** @} */
