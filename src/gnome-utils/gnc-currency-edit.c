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
 *  attacked to the widget for lookup purposes, but that would be 100%
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
#include "gnc-ui-util.h"

static void gnc_currency_edit_init         (GNCCurrencyEdit      *gce);
static void gnc_currency_edit_class_init   (GNCCurrencyEditClass *class);

static GtkComboBoxClass *parent_class;

/** The instance private data for a content plugin. */
typedef struct _GNCCurrencyEditPrivate
{
	gint last_index;	/**< Last valid GtkListStore index  */
	gulong changed_id;	/**< Signal handler id */
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

	if (currency_edit_type == 0) {
		static const GTypeInfo currency_edit_info = {
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

		currency_edit_type = g_type_register_static (GTK_TYPE_COMBO_BOX_ENTRY, 
							     "GNCCurrencyEdit",
							     &currency_edit_info, 0);
	}

	return currency_edit_type;
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
	parent_class = g_type_class_peek_parent (klass);

	g_type_class_add_private(klass, sizeof(GNCCurrencyEditPrivate));
}


/** Initialize a GncCurrencyEdit object.  This function is currently a
 *  noop.
 *
 *  @internal
 *
 *  @param gce A pointer to the newly created object.
 */
static void
gnc_currency_edit_init (GNCCurrencyEdit *gce)
{
	GNCCurrencyEditPrivate *priv;

	priv = GET_PRIVATE(gce);
	priv->last_index = -1;
	priv->changed_id = 0;
}


/** Find an entry in the GtkComboBoxEntry by its text value, and set
 *  the widget to that value.  This function also records the index of
 *  that text value for use when the user leaves the widget.
 *
 *  @param gce A pointer to a currency entry widget.
 *
 *  @param text The entry text to find in the model of the combo box
 *  entry. */
static void
gce_set_by_string(GNCCurrencyEdit *gce,
		  const gchar *text)
{
	GNCCurrencyEditPrivate *priv;
	GtkTreeModel *model;
	GtkTreeIter iter;
	GValue value = { 0 };
	const gchar *tree_string;
	gint result = 1;

	priv = GET_PRIVATE(gce);
	model = gtk_combo_box_get_model(GTK_COMBO_BOX(gce));
	if (!gtk_tree_model_get_iter_first(model, &iter)) {
		/* empty tree */
		return;
	}

	do {
		gtk_tree_model_get_value(model, &iter, 0, &value);
		tree_string = g_value_get_string(&value);
		result = strcmp(text, tree_string);
		g_value_unset(&value);
		if (result != 0)
			continue;

		/* Found a matching string */
		g_signal_handler_block(gce, priv->changed_id);
		gtk_combo_box_set_active_iter(GTK_COMBO_BOX(gce), &iter);
		g_signal_handler_unblock(gce, priv->changed_id);
		priv->last_index = gtk_combo_box_get_active(GTK_COMBO_BOX(gce));
		return;
	} while (gtk_tree_model_iter_next(model, &iter));
}


/**  The currency edit widget has changed its value.  If the widget
 *   now points to another valid currency name then record the index
 *   of that currency name for use when the user leaves the widget.
 *
 *   @param widget Unused.
 *
 *   @param gce A pointer to a currency entry widget. */
static void
gnc_changed_cb (GtkComboBox *widget,
		GNCCurrencyEdit *gce)
{
	GNCCurrencyEditPrivate *priv;
	gint current;

	priv = GET_PRIVATE(gce);
	current = gtk_combo_box_get_active(widget);
	if (current == -1)
		return;
	priv->last_index = current;
}


/**  The completion attached to currency edit widget has selected a
 *   match.  This function extracts the completed string from the
 *   completion code's temporary model, and uses that to set the index
 *   of that currency name for use when the user leaves the widget.
 *   This should always point to a valid currency name since the user
 *   made the selection from a list of currency names.
 *
 *   @param completion Unused.
 *
 *   @param comp_model A temporary model used by completion code that
 *   contains only the current matches.
 *
 *   @param comp_iter The iter in the completion's temporary model
 *   that represents the user selected match.
 *
 *   @param gce A pointer to a currency entry widget. */
static gboolean
gnc_match_selected_cb (GtkEntryCompletion *completion,
		       GtkTreeModel       *comp_model,
		       GtkTreeIter        *comp_iter,
		       GNCCurrencyEdit    *gce)
{
	gchar *text;

	gtk_tree_model_get(comp_model, comp_iter, 0, &text, -1);
	gce_set_by_string(gce, text);
	return FALSE;
}


/**  The focus left the currency edit widget, so reset the widget to
 *   its last known good value.  If the widget value contained a valid
 *   currency then this is a noop.  Otherwise the widget will be reset
 *   to the last user selected currency.  This latter state will occur
 *   if the user has typed characters directly into the widget but not
 *   selected a completion.
 *
 *   @param entry Unused.
 *
 *   @param event Unused.
 *
 *   @param gce A pointer to a currency entry widget. */
static gboolean
gce_focus_out_cb (GtkEntry *entry,
		  GdkEventFocus *event,
		  GNCCurrencyEdit *gce)
{
	GNCCurrencyEditPrivate *priv;

	priv = GET_PRIVATE(gce);
	gtk_combo_box_set_active(GTK_COMBO_BOX(gce), priv->last_index);
	return FALSE;
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
        const char *string;

        string = gnc_commodity_get_printname(commodity);
	gtk_combo_box_append_text(GTK_COMBO_BOX(gce), string);
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
	GNCCurrencyEditPrivate *priv;
	GNCCurrencyEdit *gce;
	GtkListStore *store;
	GtkEntry *entry;
	GtkEntryCompletion* completion;

	store = gtk_list_store_new (1, G_TYPE_STRING);
	gce = g_object_new (GNC_TYPE_CURRENCY_EDIT,
			    "model", store,
			    "text-column", 0,
			    NULL);
	g_object_unref (store);

	/* Set up completion on the entry */
	entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN(gce)));
	completion = gtk_entry_completion_new();
	gtk_entry_completion_set_model(completion,
				       GTK_TREE_MODEL(store));
	gtk_entry_completion_set_text_column(completion, 0);
	gtk_entry_set_completion(entry, completion);

	/* Now the signals to make sure the user can't leave the
	   widget without a valid currency. */
	priv = GET_PRIVATE(gce);
	priv->changed_id =
		g_signal_connect(gce, "changed",
				 G_CALLBACK(gnc_changed_cb), gce);
	g_signal_connect(completion, "match_selected",
			 G_CALLBACK(gnc_match_selected_cb), gce);
	g_signal_connect(entry, "focus-out-event",
			 G_CALLBACK(gce_focus_out_cb), gce);

	/* Fill in all the data. */
	fill_currencies (gce);
	gtk_tree_sortable_set_sort_column_id
		(GTK_TREE_SORTABLE(store), 0, GTK_SORT_ASCENDING);

	return GTK_WIDGET (gce);
}

/** @} */

/** @name Get/Set Functions */

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
	gce_set_by_string(gce, printname);
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

	if (gtk_combo_box_get_active_iter(GTK_COMBO_BOX(gce), &iter)) {
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
	} else {
		g_warning("Combo box returned 'inactive'. Using locale default currency.");
		commodity = gnc_locale_default_currency();
	}


	return commodity;
}

/** @} */

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
