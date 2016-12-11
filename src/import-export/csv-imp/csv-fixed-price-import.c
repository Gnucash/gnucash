/*******************************************************************\
 * csv-fixed-price-import.c -- Price importing from file            *
 *                                                                  *
 * Copyright (C) 2016 Robert Fewell                                 *
 *                                                                  *
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
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include "gnc-ui-util.h"
#include <regex.h>
#include "gnc-commodity.h"
#include "gnc-pricedb.h"
#include "gnc-component-manager.h"
#include "csv-fixed-price-import.h"
#include "gnc-gdate-utils.h"
#include "gnc-csv-model.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/* This helper function takes a regexp match and fills the model */
static void
fill_model_with_match(GMatchInfo *match_info,
                      const gchar *match_name,
                      GtkListStore *store,
                      GtkTreeIter *iterptr,
                      gint column)
{
    gchar *temp;

    if (!match_info || !match_name)
        return;

    temp = g_match_info_fetch_named (match_info, match_name);
    if (temp)
    {
        g_strstrip (temp);
        if (g_str_has_prefix (temp, "\""))
        {
            if (strlen (temp) >= 2)
            {
                gchar *toptail = g_strndup (temp + 1, strlen (temp)-2);
                gchar **parts = g_strsplit (toptail, "\"\"", -1);
                temp = g_strjoinv ("\"", parts);
                g_strfreev (parts);
                g_free (toptail);
            }
        }
        gtk_list_store_set (store, iterptr, column, temp, -1);
        g_free (temp);
     }
}


/*******************************************************
 * csv_fixed_price_import_read_file
 *
 * Parse the file for a correctly formatted file
 *******************************************************/
csv_fixed_price_import_result
csv_fixed_price_import_read_file (const gchar *filename, const gchar *parser_regexp,
                                  GtkListStore *store, guint max_rows)
{
    gchar      *locale_cont, *contents;
    GMatchInfo *match_info = NULL;
    GRegex     *regexpat = NULL;
    GError     *err;
    gint       row = 0;
    gboolean   match_found = FALSE;

    // model
    GtkTreeIter iter;

    if (!g_file_get_contents (filename, &locale_cont, NULL, NULL))
        return RESULT_OPEN_FAILED;

    contents = g_locale_to_utf8 (locale_cont, -1, NULL, NULL, NULL);
    g_free (locale_cont);

    // compile the regular expression and check for errors
    err = NULL;
    regexpat =
        g_regex_new (parser_regexp, G_REGEX_OPTIMIZE, 0, &err);
    if (err != NULL)
    {
        GtkWidget *dialog;
        gchar *errmsg;

        errmsg = g_strdup_printf (_("Error in regular expression '%s':\n%s"),
                                  parser_regexp, err->message);
        g_error_free (err);

        dialog = gtk_message_dialog_new (NULL,
                                         GTK_DIALOG_MODAL,
                                         GTK_MESSAGE_ERROR,
                                         GTK_BUTTONS_OK, "%s", errmsg);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        g_free (errmsg);
        g_free (contents);

        return RESULT_ERROR_IN_REGEXP;
    }

    g_regex_match (regexpat, contents, 0, &match_info);
    while (g_match_info_matches (match_info))
    {
        match_found = TRUE;
        // fill in the values
        gtk_list_store_append (store, &iter);
        fill_model_with_match (match_info, "p_comm1", store, &iter, PRICE_COMM1);
        fill_model_with_match (match_info, "p_date", store, &iter, PRICE_IMPORT_DATE);
        fill_model_with_match (match_info, "p_amount", store, &iter, PRICE_IMPORT_AMOUNT);
        fill_model_with_match (match_info, "p_comm2", store, &iter, PRICE_COMM2);
        gtk_list_store_set (store, &iter, PRICE_ROW_COLOR, NULL, -1);

        row++;
        if (row == max_rows)
            break;
        g_match_info_next (match_info, &err);
    }

    g_match_info_free (match_info);
    g_regex_unref (regexpat);
    g_free (contents);

    if (err != NULL)
    {
        g_printerr ("Error while matching: %s\n", err->message);
        g_error_free (err);
    }

    if (match_found == TRUE)
        return MATCH_FOUND;
    else
        return RESULT_OK;
}


/*******************************************************
 * save_error_text
 *
 * Add error text to existing errors
 *******************************************************/
static void
save_error_text (CsvPriceImportInfo *info, gint row, gchar *etext)
{
    gchar *current_error_text;
    gchar *text;

    current_error_text = g_strdup (info->error);

    if (g_strcmp0 (info->error, "") != 0)
        g_free (info->error);

    text = g_strdup_printf (gettext("Row %u, %s\n"), row + 1, etext);
    info->error = g_strconcat (current_error_text, text, NULL);
    g_free (text);
    g_free (current_error_text);
}


/*******************************************************
 * parse_number_string
 *
 * Parse the number string and return a gnc_number
 *******************************************************/
static gboolean
parse_number_string (CsvPriceImportInfo *info, const gchar *num_string, gnc_numeric *ret_num)
{
    char       *endptr, *str_num;
    gboolean    valid = FALSE;
    gchar      *result = NULL;
    gnc_numeric num;

    str_num = g_strdup (num_string);

    /* Currency format */
    switch (info->currency_format)
    {
        case 0:
            /* Currency locale */
            valid = xaccParseAmount (str_num, TRUE, &num, &endptr);
            break;
        case 1:
            /* Currency decimal period */
            valid = xaccParseAmountExtended (str_num, TRUE, '-', '.', ',', "\003\003", "$+", &num, &endptr);
            break;
        case 2:
            /* Currency decimal comma */
            valid = xaccParseAmountExtended (str_num, TRUE, '-', ',', '.', "\003\003", "$+", &num, &endptr);
            break;
    }
    *ret_num = num;
    g_free (str_num);

    return valid;
}


/*******************************************************
 * csv_fixed_price_test_one_line
 *
 * Test a line for valid date and valid number
 *******************************************************/
gboolean
csv_fixed_price_test_one_line (CsvPriceImportInfo *info)
{
    gboolean     valid;
    GtkTreeIter  iter;
    gchar       *date, *amount_num;
    gnc_numeric  amount;
    gint         row;
    gboolean     date_ok = FALSE;
    gboolean     num_ok = FALSE;

    row = info->header_rows;
    valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(info->store), &iter, NULL, row);

    gtk_tree_model_get (GTK_TREE_MODEL (info->store), &iter,
                        PRICE_IMPORT_DATE, &date,
                        PRICE_IMPORT_AMOUNT, &amount_num, -1);

    if (parse_date (date, info->date_format) != -1) // invalid date
        date_ok = TRUE;

    if (g_strcmp0 (amount_num, "") != 0) // test for valid number
    {
        if (parse_number_string (info, amount_num, &amount))
            num_ok = TRUE;
    }
    /* free resources */
    g_free (date);
    g_free (amount_num);

    if ((date_ok == TRUE) && (num_ok == TRUE))
       return TRUE;
    else
       return FALSE;
}

/*******************************************************
 * get_price_commodity
 *
 * Find a commodity by Symbol/mnemonic
 *******************************************************/
static gnc_commodity *
get_price_commodity (const char *symbol)
{
    const gnc_commodity_table *commodity_table = gnc_get_current_commodities ();
    GList         *namespaces;
    gnc_commodity *retval = NULL;
    gnc_commodity *tmp_commodity = NULL;
    char  *tmp_namespace = NULL;
    GList *commodity_list = NULL;
    GList *namespace_list = gnc_commodity_table_get_namespaces (commodity_table);

    namespace_list = g_list_first (namespace_list);
    while (namespace_list != NULL && retval == NULL)
    {
        tmp_namespace = namespace_list->data;
        DEBUG("Looking at namespace %s", tmp_namespace);
        commodity_list = gnc_commodity_table_get_commodities (commodity_table, tmp_namespace);
        commodity_list  = g_list_first (commodity_list);
        while (commodity_list != NULL && retval == NULL)
        {
            const char* tmp_mnemonic = NULL;
            tmp_commodity = commodity_list->data;
            DEBUG("Looking at commodity %s", gnc_commodity_get_fullname (tmp_commodity));
            tmp_mnemonic = gnc_commodity_get_mnemonic (tmp_commodity);
            if (g_strcmp0 (tmp_mnemonic, symbol) == 0)
            {
                retval = tmp_commodity;
                DEBUG("Commodity %s%s", gnc_commodity_get_fullname (retval), " matches.");
            }
            commodity_list = g_list_next (commodity_list);
        }
        namespace_list = g_list_next (namespace_list);
    }
    g_list_free (commodity_list);
    g_list_free (namespace_list);

    return retval;
}


/*******************************************************
 * csv_fixed_price_import
 *
 * Parse the liststore for price updates
 *******************************************************/
void
csv_fixed_price_import (CsvPriceImportInfo *info)
{
    QofBook       *book = gnc_get_current_book();
    gboolean       valid;
    GtkTreeIter    iter;
    gchar         *p_comm1, *p_date, *p_amount, *p_comm2;
    gchar         *last_p_comm1 = NULL;
    gchar         *last_p_comm2 = NULL;
    int            row;
    gnc_numeric    amount;
    gnc_commodity *commodity1 = NULL;
    gnc_commodity *commodity2 = NULL;
    gboolean commodity1_is_currency = FALSE;

    ENTER("");

    info->num_new = 0;
    info->num_duplicates = 0;

    /* Move to the first valid entry in store */
    row = info->header_rows;
    valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(info->store), &iter, NULL, row );
    while (valid)
    {
        gboolean price_error = FALSE;

        /* Walk through the list, reading each row */
        gtk_tree_model_get (GTK_TREE_MODEL (info->store), &iter,
                            PRICE_COMM1, &p_comm1,
                            PRICE_IMPORT_DATE, &p_date,
                            PRICE_IMPORT_AMOUNT, &p_amount,
                            PRICE_COMM2, &p_comm2, -1);

        DEBUG("Row is %u and Symbol from is %s, Symbol to is %s", row, p_comm1, p_comm2);
g_print("Row is %u and Symbol from is %s, Symbol to is %s\n", row, p_comm1, p_comm2);

        if (g_strcmp0 (p_comm1, last_p_comm1) != 0)
        {
            g_free (last_p_comm1);
            commodity1 = get_price_commodity (p_comm1); // find commodity for symbol from
            if (commodity1 == NULL)
            {
                save_error_text (info, row, _("Can not find First Commodity."));
                price_error = TRUE;
            }
            else
                commodity1_is_currency = gnc_commodity_is_currency (commodity1);

            last_p_comm1 = g_strdup (p_comm1);
        }

        if (g_strcmp0 (p_comm2, last_p_comm2) != 0)
        {
            g_free (last_p_comm2);
            commodity2 = get_price_commodity (p_comm2); // find commodity for symbol to
            if (commodity2 == NULL)
            {
                save_error_text (info, row, _("Can not find Second Commodity."));
                price_error = TRUE;
            }
            else
            {
                if ((commodity1_is_currency == FALSE) && (gnc_commodity_is_currency (commodity2) == FALSE))
                {
                    save_error_text (info, row, _("Second Commodity is not a Currency when first is not."));
                    price_error = TRUE;
                }
            }
            last_p_comm2 = g_strdup (p_comm2);
        }

        if (parse_date (p_date, info->date_format) == -1) // invalid date
        {
            save_error_text (info, row, _("Date is invalid."));
            price_error = TRUE;
        }

        // Lets get some numbers
        if ((parse_number_string (info, p_amount, &amount) == FALSE) &&
            (gnc_numeric_positive_p (amount) == FALSE)) // invalid amount
        {
            save_error_text (info, row, _("Numeric Amount is invalid."));
            price_error = TRUE;
        }

        // Start to Create a new Price 
        if (price_error == FALSE)
        {
            GNCPriceDB *pdb = gnc_pricedb_get_db (book);
            GNCPrice *price, *old_price = NULL;
            Timespec date;
            gboolean rev = FALSE;

            DEBUG("Create new price");

            timespecFromTime64 (&date, parse_date (p_date, info->date_format));
            date.tv_nsec = 0;

            if (commodity1_is_currency == TRUE)
            {
                // Check for currency in reverse direction.
                GNCPrice *rev_price = gnc_pricedb_lookup_day (pdb, commodity2, commodity1, date);
                if (rev_price != NULL)
                    rev = TRUE;
                gnc_price_unref (rev_price);

                // Check for price less than 1, reverse if so.
                if (gnc_numeric_compare (amount, gnc_numeric_create (1, 1)) != 1)
                    rev = TRUE;
                DEBUG("Commodity one is a Currency, amount is %s, rev is %d", gnc_num_dbg_to_string (amount), rev);
            }

            // Should the commodities be reversed
            if (rev)
                old_price = gnc_pricedb_lookup_day (pdb, commodity2, commodity1, date);
            else
                old_price = gnc_pricedb_lookup_day (pdb, commodity1, commodity2, date);

            // Should old price be over writen
            if ((old_price != NULL) && (info->over_write == TRUE))
            {
                DEBUG("Over write");
                gnc_pricedb_remove_price (pdb, old_price);
                old_price = NULL;
            }

            // Create the new price
            if (old_price == NULL)
            {
                price = gnc_price_create (book);
                gnc_price_begin_edit (price);
                if(rev)
                {
                    gnc_price_set_commodity (price, commodity2);
                    gnc_price_set_currency (price, commodity1);
                    amount = gnc_numeric_convert (gnc_numeric_invert (amount),
                                                  CURRENCY_DENOM, GNC_HOW_RND_ROUND_HALF_UP);
                    gnc_price_set_value (price, amount);
                }
                else
                {
                    gnc_price_set_commodity (price, commodity1);
                    gnc_price_set_currency (price, commodity2);
                    gnc_price_set_value (price, amount);
                }
                gnc_price_set_time (price, date);
                gnc_price_set_source (price, PRICE_SOURCE_USER_PRICE);
                gnc_price_set_typestr (price, PRICE_TYPE_LAST);
                gnc_price_commit_edit (price);

                if (!gnc_pricedb_add_price (pdb, price))
                    save_error_text (info, row, _("Error adding Price to database."));

                gnc_price_unref (price);
                info->num_new++;
            }
            else
                info->num_duplicates++;

            gnc_price_unref (old_price);
        }
        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (info->store), &iter);
        row++;

        /* free resources */
        g_free (p_comm1);
        g_free (p_date);
        g_free (p_amount);
        g_free (p_comm2);
    }
    g_free (last_p_comm1);
    g_free (last_p_comm2);

    LEAVE("");
}
