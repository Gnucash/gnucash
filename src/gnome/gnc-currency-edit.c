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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

/*
 * Currency editor widget
 *
 * Authors: Dave Peticolas <dave@krondo.com>
 */

#include <config.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-currency-edit.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "messages.h"


static void gnc_currency_edit_init         (GNCCurrencyEdit      *gce);
static void gnc_currency_edit_class_init   (GNCCurrencyEditClass *class);
static void gnc_currency_edit_destroy      (GtkObject            *object);

static GtkComboClass *parent_class;

/**
 * gnc_currency_edit_get_type:
 *
 * Returns the GtkType for the GNCCurrencyEdit widget
 */
guint
gnc_currency_edit_get_type (void)
{
	static guint currency_edit_type = 0;

	if (!currency_edit_type){
		GtkTypeInfo currency_edit_info = {
			"GNCCurrencyEdit",
			sizeof (GNCCurrencyEdit),
			sizeof (GNCCurrencyEditClass),
			(GtkClassInitFunc) gnc_currency_edit_class_init,
			(GtkObjectInitFunc) gnc_currency_edit_init,
			NULL,
			NULL,
		};

		currency_edit_type = gtk_type_unique (gtk_combo_get_type (),
                                                      &currency_edit_info);
	}

	return currency_edit_type;
}

static void
gnc_currency_edit_class_init (GNCCurrencyEditClass *class)
{
	GtkObjectClass *object_class = (GtkObjectClass *) class;

	object_class = (GtkObjectClass*) class;

	parent_class = gtk_type_class (gtk_combo_get_type ());

	object_class->destroy = gnc_currency_edit_destroy;
}

static void
gnc_currency_edit_init (GNCCurrencyEdit *gce)
{
        gtk_combo_set_use_arrows_always(GTK_COMBO(gce), TRUE);
        gtk_combo_set_value_in_list(GTK_COMBO(gce), FALSE, TRUE);
        gtk_combo_disable_activate(GTK_COMBO(gce));
}

static void
gnc_currency_edit_destroy (GtkObject *object)
{
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_CURRENCY_EDIT (object));

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		(* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
add_item(GNCCurrencyEdit *gce, gnc_commodity *commodity)
{
        GtkWidget *item;
        GtkWidget *label;
        const char *string;

        item = gtk_list_item_new();

        string = gnc_commodity_get_printname (commodity);

        label = gtk_label_new(string);
        gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);

        gtk_container_add(GTK_CONTAINER(item), label);

        gtk_widget_show_all(item);

        gtk_combo_set_item_string(GTK_COMBO(gce), GTK_ITEM (item),
                                  gnc_commodity_get_mnemonic (commodity));

        gtk_container_add(GTK_CONTAINER(GTK_COMBO(gce)->list), item);
}

static int
currency_compare(gconstpointer a, gconstpointer b)
{
        return strcmp (gnc_commodity_get_fullname (a),
                       gnc_commodity_get_fullname (b));
}

static void
fill_currencies(GNCCurrencyEdit *gce)
{
        GList *currencies;
        GList *node;

        currencies = gnc_commodity_table_get_commodities
                (gnc_engine_commodities (), GNC_COMMODITY_NS_ISO);

        currencies = g_list_sort(currencies, currency_compare);

        for (node = currencies; node != NULL; node = node->next)
                add_item(gce, node->data);

        g_list_free(currencies);
}

/**
 * gnc_currency_edit_new:
 *
 * Creates a new GNCCurrencyEdit widget which can be used to provide
 * an easy way to enter ISO currency codes.
 * 
 * Returns a GNCCurrencyEdit widget.
 */
GtkWidget *
gnc_currency_edit_new (void)
{
	GNCCurrencyEdit *gce;

	gce = gtk_type_new (gnc_currency_edit_get_type ());

	fill_currencies (gce);

	return GTK_WIDGET (gce);
}

/**
 * gnc_currency_edit_set_curreny:
 * @gce: the currency editor widget
 * @currency: the currency to select
 *
 * Sets the currency value of the widget to a particular currency.
 *
 * Returns nothing.
 */
void
gnc_currency_edit_set_currency (GNCCurrencyEdit *gce,
                                const gnc_commodity *currency)
{
        g_return_if_fail(gce != NULL);
        g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));
        g_return_if_fail(currency != NULL);

        gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(gce)->entry),
                           gnc_commodity_get_mnemonic (currency));
}

/**
 * gnc_currency_edit_get_curreny:
 * @gce: the currency editor widget
 *
 * Returns the selected currency.
 */
gnc_commodity *
gnc_currency_edit_get_currency (GNCCurrencyEdit *gce)
{
        const char *mnemonic;

        g_return_val_if_fail(gce != NULL, NULL);
        g_return_val_if_fail(GNC_IS_CURRENCY_EDIT(gce), NULL);

        mnemonic = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(gce)->entry));

        return gnc_commodity_table_lookup (gnc_engine_commodities (),
                                           GNC_COMMODITY_NS_ISO,
                                           mnemonic);
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
