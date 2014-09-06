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
 *
 *  When the GtkComboCellEntry widget supports completion, this Gnucash
 *  widget should be modified so that it is based upon that widget.
 *  That would give users the capability to select a currency by typing
 *  its ISO 4217 code (e.g. USD, GBP, etc).  Moving to that widget
 *  today, however, would cause more problems that its worth.  There is
 *  currently no way to get access to the embedded GtkEntry widget, and
 *  therefore no way to implement completion in gnucash or prevent the
 *  user from typing in random data.
 */

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-currency-edit.h"
#include "gnc-commodity.h"
#include "gnc-gtk-utils.h"
#include "gnc-ui-util.h"
#include "gnc-engine.h"

/** The debugging module used by this file. */
static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_currency_edit_init         (GNCCurrencyEdit      *gce);
static void gnc_currency_edit_class_init   (GNCCurrencyEditClass *klass);
static void gnc_currency_edit_finalize     (GObject *object);
static void gnc_currency_edit_mnemonic_changed (GObject    *gobject,
        GParamSpec *pspec,
        gpointer    user_data);
static void gnc_currency_edit_active_changed (GtkComboBox *gobject,
        gpointer     user_data);

static GtkComboBoxClass *parent_class;

/** The instance private data for a content plugin. */
typedef struct _GNCCurrencyEditPrivate
{
    gchar *mnemonic;
} GNCCurrencyEditPrivate;

#define GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_CURRENCY_EDIT, GNCCurrencyEditPrivate))

/** @name Basic Object Implementation */
/** @{ */

/*  Return the GType for the GNCCurrencyEdit currency selection widget.
 */
GType
gnc_currency_edit_get_type (void)
{
    static GType currency_edit_type = 0;

    if (currency_edit_type == 0)
    {
        static const GTypeInfo currency_edit_info =
        {
            sizeof (GNCCurrencyEditClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_currency_edit_class_init,
            NULL,
            NULL,
            sizeof (GNCCurrencyEdit),
            0, /* n_preallocs */
            (GInstanceInitFunc) gnc_currency_edit_init,
            NULL
        };

        currency_edit_type = g_type_register_static (GTK_TYPE_COMBO_BOX,
                             "GNCCurrencyEdit",
                             &currency_edit_info, 0);
    }

    return currency_edit_type;
}

enum
{
    PROP_0,

    PROP_GCE_MNEMONIC,

    N_PROPERTIES
};

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL, };

static void
gnc_currency_edit_set_property (GObject      *object,
                                guint         property_id,
                                const GValue *value,
                                GParamSpec   *pspec)
{
    GNCCurrencyEdit *self = GNC_CURRENCY_EDIT (object);
    GNCCurrencyEditPrivate *priv = GET_PRIVATE (self);

    switch (property_id)
    {
    case PROP_GCE_MNEMONIC:
        g_free (priv->mnemonic);
        priv->mnemonic = g_value_dup_string (value);
        DEBUG ("mnemonic: %s\n", priv->mnemonic);
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
    GNCCurrencyEditPrivate *priv = GET_PRIVATE (self);

    switch (property_id)
    {
    case PROP_GCE_MNEMONIC:
        g_value_set_string (value, priv->mnemonic);
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
    parent_class = g_type_class_peek_parent (klass);

    g_type_class_add_private(klass, sizeof(GNCCurrencyEditPrivate));

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
    GNCCurrencyEditPrivate *priv;
    GNCCurrencyEdit *period;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_CURRENCY_EDIT (object));

    period = GNC_CURRENCY_EDIT(object);
    priv = GET_PRIVATE(period);

    g_free (priv->mnemonic);

    /* Do not free the private data structure itself. It is part of
     * a larger memory block allocated by the type system. */

    if (G_OBJECT_CLASS(parent_class)->finalize)
        (* G_OBJECT_CLASS(parent_class)->finalize) (object);
}


static void
gnc_currency_edit_mnemonic_changed (GObject    *gobject,
                                    GParamSpec *pspec,
                                    gpointer    user_data)
{

    GNCCurrencyEdit *self = GNC_CURRENCY_EDIT (gobject);
    GNCCurrencyEditPrivate *priv = GET_PRIVATE (self);

    gnc_commodity *currency = gnc_commodity_table_lookup (gnc_get_current_commodities (),
                              GNC_COMMODITY_NS_CURRENCY,
                              priv->mnemonic);

    /* If there isn't any such commodity, get the default */
    if (!currency)
    {
        currency = gnc_locale_default_currency();
        DEBUG("gce %p, default currency mnemonic %s",
              self, gnc_commodity_get_mnemonic(currency));
    }

    g_signal_handlers_block_by_func(G_OBJECT(self),
                                    G_CALLBACK(gnc_currency_edit_mnemonic_changed), user_data);
    gnc_currency_edit_set_currency(self, currency);
    g_signal_handlers_unblock_by_func(G_OBJECT(self),
                                      G_CALLBACK(gnc_currency_edit_mnemonic_changed), user_data);
}


static void gnc_currency_edit_active_changed (GtkComboBox *gobject,
        gpointer     user_data)
{
    GNCCurrencyEdit *self = GNC_CURRENCY_EDIT (gobject);
    GNCCurrencyEditPrivate *priv = GET_PRIVATE (self);

    gnc_commodity *currency = gnc_currency_edit_get_currency (self);
    const gchar *mnemonic = gnc_commodity_get_mnemonic (currency);

    g_signal_handlers_block_by_func(G_OBJECT(self),
                                    G_CALLBACK(gnc_currency_edit_active_changed), user_data);
    g_object_set (G_OBJECT (self), "mnemonic", mnemonic, NULL);
    g_signal_handlers_unblock_by_func(G_OBJECT(self),
                                      G_CALLBACK(gnc_currency_edit_active_changed), user_data);
}

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

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(gce));

    string = gnc_commodity_get_printname(commodity);

    gtk_list_store_append(GTK_LIST_STORE(model), &iter);
    gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0, string, -1);

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

    store = gtk_list_store_new (1, G_TYPE_STRING);
    gce = g_object_new (GNC_TYPE_CURRENCY_EDIT,
                        "model", store,
                        "has-entry", TRUE,
                        NULL);
    g_object_unref (store);

    /* Set the column for the text */
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(gce), 0);

    /* Now the signals to make sure the user can't leave the
       widget without a valid currency. */
    gnc_cbwe_require_list_item(GTK_COMBO_BOX(gce));

    /* Fill in all the data. */
    fill_currencies (gce);
    gtk_tree_sortable_set_sort_column_id
    (GTK_TREE_SORTABLE(store), 0, GTK_SORT_ASCENDING);

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

    g_return_if_fail(gce != NULL);
    g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));
    g_return_if_fail(currency != NULL);

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
    const char *fullname;
    char *mnemonic, *name;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GValue value = { 0 };

    g_return_val_if_fail(gce != NULL, NULL);
    g_return_val_if_fail(GNC_IS_CURRENCY_EDIT(gce), NULL);

    if (gtk_combo_box_get_active_iter(GTK_COMBO_BOX(gce), &iter))
    {
        model = gtk_combo_box_get_model(GTK_COMBO_BOX(gce));
        gtk_tree_model_get_value(model, &iter, 0, &value);
        fullname = g_value_get_string(&value);
        mnemonic = g_strdup(fullname);
        g_value_unset(&value);

        name = strchr(mnemonic, ' ');
        if (name != NULL)
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

/** @} */
/** @} */
/** @} */

