/*
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
#include "guile-util.h"
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
insert_text_cb(GtkEditable *editable, gchar *new_text, gint new_text_length,
               gint *position, gpointer user_data)
{
        gint i;

        for (i = 0; i < new_text_length; i++)
        {
                if (!isalpha(new_text[i]))
                {
                        gtk_signal_emit_stop_by_name(GTK_OBJECT(editable),
                                                     "insert_text");
                        return;
                }

                new_text[i] = toupper(new_text[i]);
        }
}

static void
gnc_currency_edit_init (GNCCurrencyEdit *gce)
{
        GtkTooltips *tooltips;

        gtk_combo_set_use_arrows_always(GTK_COMBO(gce), TRUE);
        gtk_combo_set_value_in_list(GTK_COMBO(gce), TRUE, TRUE);
        gtk_combo_disable_activate(GTK_COMBO(gce));

        gtk_entry_set_max_length(GTK_ENTRY(GTK_COMBO(gce)->entry), 3);

        gtk_signal_connect(GTK_OBJECT(GTK_COMBO(gce)->entry),
                           "insert_text", insert_text_cb, NULL);

        tooltips = gtk_tooltips_new();
        gtk_tooltips_set_tip(GTK_TOOLTIPS(tooltips),
                             GTK_COMBO(gce)->entry,
                             TOOLTIP_CURRENCY, NULL);
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
add_item(GNCCurrencyEdit *gce, const char *code, const char *desc)
{
        GtkWidget *item;
        GtkWidget *label;
        char *string;

        item = gtk_list_item_new();

        string = g_strconcat(desc, " (", code, ")", NULL);

        label = gtk_label_new(string);
        gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
        g_free(string);

        gtk_container_add(GTK_CONTAINER(item), label);

        gtk_widget_show_all(item);

        gtk_combo_set_item_string(GTK_COMBO(gce), GTK_ITEM (item), code);

        gtk_container_add(GTK_CONTAINER(GTK_COMBO(gce)->list), item);
}

static int
currency_compare(gconstpointer a, gconstpointer b)
{
        return strcoll(a, b);
}

static void
destroy_hash_node(gpointer key, gpointer value, gpointer data)
{
        free(key);
        free(value);
}

static void
fill_currencies(GNCCurrencyEdit *gce)
{
        GHashTable *table;
        GList *list = NULL;
        GList *node;
        SCM currencies;
        SCM item;

        table = g_hash_table_new(g_str_hash, g_str_equal);

        gnc_depend("currencies.scm");

        currencies = gh_eval_str("gnc:*currencies*");

        while (gh_list_p(currencies) && !gh_null_p(currencies))
        {
                char *code;
                char *desc;
                SCM value;

                item = gh_car(currencies);
                currencies = gh_cdr(currencies);

                if (!gh_pair_p(item))
                        continue;

                value = gh_car(item);
                if (!gh_string_p(value))
                        continue;

                code = gh_scm2newstr(value, NULL);
                if (code == NULL)
                        continue;

                value = gh_cdr(item);
                if (!gh_string_p(value))
                {
                        free(code);
                        continue;
                }

                desc = gh_scm2newstr(value, NULL);
                if (desc == NULL)
                {
                        free(code);
                        continue;
                }

                g_hash_table_insert(table, desc, code);
                list = g_list_prepend(list, desc);
        }

        list = g_list_reverse(list);
        list = g_list_sort(list, currency_compare);

        for (node = list; node != NULL; node = node->next)
                add_item(gce, g_hash_table_lookup(table, node->data),
                         node->data);

        g_hash_table_foreach(table, destroy_hash_node, NULL);
        g_hash_table_destroy(table);

        g_list_free(list);
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
gnc_currency_edit_new ()
{
	GNCCurrencyEdit *gce;

	gce = gtk_type_new (gnc_currency_edit_get_type ());

	fill_currencies (gce);

	return GTK_WIDGET (gce);
}

/**
 * gnc_currency_edit_new:
 * @gce: the currency editor widget
 * @currency: the currency code to select
 *
 * Sets the currency value of the widget to a particular currency.
 * 
 * Returns nothing.
 */
void
gnc_currency_edit_set_currency (GNCCurrencyEdit *gce, const gchar *currency)
{
        g_return_if_fail(gce != NULL);
        g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));

        gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(gce)->entry), currency);
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
