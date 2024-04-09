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
 *  This widget is a GtkComboBox that is wrapped with support
 *  functions for building/selecting from a list of ISO4217 currency
 *  names.  All data is maintained within the widget itself, which
 *  makes the name/item lookup functions somewhat complicated.  The
 *  alternative coding would be to keep an auxiliary list of strings
 *  attached to the widget for lookup purposes, but that would be 100%
 *  redundant information.
 *
 *  This function currently builds a new GtkListStore for each widget
 *  created.  It could be optimized to build a single list store and
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
#include "gnc-gtk-utils.h"
#include "gnc-ui-util.h"
#include "gnc-engine.h"
#include "dialog-utils.h"

/** The debugging module used by this file. */
static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_currency_edit_finalize     (GObject *object);
static void gnc_currency_edit_mnemonic_changed (GObject    *gobject,
        GParamSpec *pspec,
        gpointer    user_data);
static void gnc_currency_edit_active_changed (GtkComboBox *gobject,
        gpointer     user_data);

struct _GNCCurrencyEdit
{
    GtkComboBox combobox;

    gchar *mnemonic;
};

G_DEFINE_TYPE(GNCCurrencyEdit, gnc_currency_edit, GTK_TYPE_COMBO_BOX)

/** @name Basic Object Implementation */
/** @{ */

enum
{
    PROP_0,

    PROP_GCE_MNEMONIC,

    N_PROPERTIES
};

static GParamSpec *obj_properties[N_PROPERTIES] = { nullptr, };

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
        g_free (self->mnemonic);
        self->mnemonic = g_value_dup_string (value);
        DEBUG ("mnemonic: %s\n", self->mnemonic);
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
    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gce), "gnc-id-currency-edit");

    g_signal_connect (gce, "notify::mnemonic",
                      G_CALLBACK (gnc_currency_edit_mnemonic_changed), gce);
    g_signal_connect (gce, "changed",
                      G_CALLBACK (gnc_currency_edit_active_changed), gce);
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

    G_OBJECT_CLASS(gnc_currency_edit_parent_class)->finalize (object);
}


static void
gnc_currency_edit_mnemonic_changed (GObject    *gobject,
                                    GParamSpec *pspec,
                                    gpointer    user_data)
{

    GNCCurrencyEdit *self = GNC_CURRENCY_EDIT (gobject);

    gnc_commodity *currency = gnc_commodity_table_lookup (gnc_get_current_commodities (),
                              GNC_COMMODITY_NS_CURRENCY,
                              self->mnemonic);

    /* If there isn't any such commodity, get the default */
    if (!currency)
    {
        currency = gnc_locale_default_currency();
        DEBUG("gce %p, default currency mnemonic %s",
              self, gnc_commodity_get_mnemonic(currency));
    }

    g_signal_handlers_block_by_func(G_OBJECT(self),
                                    (gpointer)gnc_currency_edit_mnemonic_changed,
                                    user_data);
    gnc_currency_edit_set_currency(self, currency);
    g_signal_handlers_unblock_by_func(G_OBJECT(self),
                                      (gpointer)gnc_currency_edit_mnemonic_changed,
                                      user_data);
}


static void gnc_currency_edit_active_changed (GtkComboBox *gobject,
        gpointer     user_data)
{
    GNCCurrencyEdit *self = GNC_CURRENCY_EDIT (gobject);

    /* Check that there is a proper selection before proceeding.  Doing so allows
     * GTK entry completion to proceed. */
    if (gtk_combo_box_get_active(GTK_COMBO_BOX(self)) != -1)
    {
        gnc_commodity *currency = gnc_currency_edit_get_currency (self);
        const gchar *mnemonic = gnc_commodity_get_mnemonic (currency);

        g_signal_handlers_block_by_func(G_OBJECT(self),
                                        (gpointer)gnc_currency_edit_active_changed, user_data);
        g_object_set (G_OBJECT (self), "mnemonic", mnemonic, nullptr);
        g_signal_handlers_unblock_by_func(G_OBJECT(self),
                                          (gpointer)gnc_currency_edit_active_changed, user_data);
    }
}

enum
{
    CURRENCY_COL_NAME,
    CURRENCY_COL_NORMALIZED_FOLDED,
    NUM_CURRENCY_COLS
};

/** This auxiliary function adds a single currency name to the combo
 *  box.  It is called as an iterator function when running a list of
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
    GtkTreeModel *model;
    GtkTreeIter iter;
    const char *string;
    gchar *normalized, *normalized_folded;

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(gce));

    string = gnc_commodity_get_printname(commodity);
    normalized = g_utf8_normalize (string, -1, G_NORMALIZE_NFC);
    normalized_folded = normalized ? g_utf8_casefold (normalized, -1) : nullptr;

    gtk_list_store_append(GTK_LIST_STORE(model), &iter);
    gtk_list_store_set (GTK_LIST_STORE(model), &iter,
                        CURRENCY_COL_NAME, string,
                        CURRENCY_COL_NORMALIZED_FOLDED, normalized_folded,
                        -1);
    g_free (normalized_folded);
    g_free (normalized);
}


/** This auxiliary function adds all the currency names to a combo
 *  box.
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
    g_list_foreach(currencies, (GFunc)add_item, gce);
    g_list_free(currencies);
}


static gboolean
match_func (GtkEntryCompletion *completion, const char *entry_str,
            GtkTreeIter *iter, GtkTreeModel *model)
{
    gchar *currency_name;
    gboolean ret = FALSE;
    gtk_tree_model_get (model, iter,
                        CURRENCY_COL_NORMALIZED_FOLDED, &currency_name,
                        -1);
    if (currency_name && *currency_name && strstr (currency_name, entry_str))
        ret = TRUE;
    g_free (currency_name);
    return ret;
}

/*  Create a new GNCCurrencyEdit widget which can be used to provide
 *  an easy way to enter ISO currency codes.
 *
 *  @return A GNCCurrencyEdit widget.
 */
GtkWidget *
gnc_currency_edit_new (void)
{
    GNCCurrencyEdit *gce;
    GtkListStore *store;
    GtkEntryCompletion* completion;

    store = gtk_list_store_new (NUM_CURRENCY_COLS, G_TYPE_STRING, G_TYPE_STRING);
    gce = GNC_CURRENCY_EDIT(g_object_new (GNC_TYPE_CURRENCY_EDIT,
                                          "model", store,
                                          "has-entry", true,
                                          nullptr));
    g_object_unref (store);

    /* Set the column for the text */
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(gce), CURRENCY_COL_NAME);

    /* Now the signals to make sure the user can't leave the
       widget without a valid currency. */
    gnc_cbwe_require_list_item(GTK_COMBO_BOX(gce));

    /* Fill in all the data. */
    fill_currencies (gce);
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(store),
                                          CURRENCY_COL_NAME,
                                          GTK_SORT_ASCENDING);

    completion = gtk_entry_completion_new ();
    gtk_entry_completion_set_model (completion, GTK_TREE_MODEL(store));
    gtk_entry_completion_set_text_column (completion, CURRENCY_COL_NAME);
    gtk_entry_completion_set_match_func (completion,
                                         (GtkEntryCompletionMatchFunc)match_func,
                                         GTK_TREE_MODEL(store), nullptr);
    gtk_entry_set_completion (GTK_ENTRY (gtk_bin_get_child (GTK_BIN (gce))),
                              completion);

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
    const gchar *printname;

    g_return_if_fail(gce != nullptr);
    g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));
    g_return_if_fail(currency != nullptr);

    printname = gnc_commodity_get_printname(currency);
    gnc_cbwe_set_by_string(GTK_COMBO_BOX(gce), printname);
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
    gnc_commodity *commodity;
    char *mnemonic, *name;
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_return_val_if_fail(gce != nullptr, nullptr);
    g_return_val_if_fail(GNC_IS_CURRENCY_EDIT(gce), nullptr);

    if (gtk_combo_box_get_active_iter(GTK_COMBO_BOX(gce), &iter))
    {
        model = gtk_combo_box_get_model(GTK_COMBO_BOX(gce));
        gtk_tree_model_get (model, &iter, 0, &mnemonic, -1);

        name = strchr(mnemonic, ' ');
        if (name != nullptr)
            *name = '\0';
        commodity = gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                                GNC_COMMODITY_NS_CURRENCY,
                                                mnemonic);
        g_free(mnemonic);
    }
    else
    {
        g_warning("Combo box returned 'inactive'. Using locale default currency.");
        commodity = gnc_locale_default_currency();
    }


    return commodity;
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
    GtkTreeModel *model;
    GtkWidget *entry;

    g_return_if_fail(gce != nullptr);
    g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));

    model = gtk_combo_box_get_model (GTK_COMBO_BOX(gce));

    entry = gtk_bin_get_child (GTK_BIN(gce));

    g_object_ref (model);

    g_signal_handlers_block_by_func (G_OBJECT(gce),
                                     (gpointer)gnc_currency_edit_active_changed, gce);

    gtk_combo_box_set_model (GTK_COMBO_BOX(gce), nullptr);
    gtk_entry_set_text (GTK_ENTRY(entry),"");
    gtk_combo_box_set_active (GTK_COMBO_BOX(gce), -1);
    gtk_combo_box_set_model (GTK_COMBO_BOX(gce), model);

    g_signal_handlers_block_by_func (G_OBJECT(gce),
                                     (gpointer)gnc_currency_edit_active_changed, gce);

    g_object_unref (model);
}

/** @} */
/** @} */
/** @} */

