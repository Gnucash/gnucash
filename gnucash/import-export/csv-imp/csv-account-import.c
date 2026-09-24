/*******************************************************************\
 * csv-account-import.c -- Account importing from file              *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
 *                                                                  *
 * Based on code from bi_import written by Sebastian Held  and      *
 * Mike Evans.                                                      *
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

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include "gnc-string-utils.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"
#include "gnc-ui.h"
#include <regex.h>
#include "Account.h"
#include "gnc-component-manager.h"
#include "csv-account-import.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

#define CSV_IMPORT_ROW_VALUES "csv-import-row-values"

GObject *
csv_import_row_new (void)
{
    GObject *row = g_object_new (G_TYPE_OBJECT, NULL);
    gchar **values = g_new0 (gchar *, N_COLUMNS + 1);

    g_object_set_data_full (row, CSV_IMPORT_ROW_VALUES, values,
                            (GDestroyNotify)g_strfreev);
    return row;
}

const gchar *
csv_import_row_get (GObject *row, guint column)
{
    gchar **values;

    g_return_val_if_fail (G_IS_OBJECT (row), "");
    g_return_val_if_fail (column < N_COLUMNS, "");
    values = g_object_get_data (row, CSV_IMPORT_ROW_VALUES);
    return values[column] ? values[column] : "";
}

void
csv_import_row_set (GObject *row, guint column, const gchar *value)
{
    gchar **values;

    g_return_if_fail (G_IS_OBJECT (row));
    g_return_if_fail (column < N_COLUMNS);
    values = g_object_get_data (row, CSV_IMPORT_ROW_VALUES);
    g_free (values[column]);
    values[column] = g_strdup (value ? value : "");
}

/* This helper function takes a regexp match and fills an import row. */
static void
fill_model_with_match(GMatchInfo *match_info,
               const gchar *match_name,
               GObject *row,
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
        csv_import_row_set (row, column, temp);
        g_free (temp);
     }
}

/*******************************************************
 * csv_import_read_file
 *
 * Parse the file for a correctly formatted file
 *******************************************************/
csv_import_result
csv_import_read_file (GtkWindow *window, const gchar *filename,
                      const gchar *parser_regexp,
                      GListStore *store, guint max_rows)
{
    gchar      *locale_cont, *contents;
    GMatchInfo *match_info = NULL;
    GRegex     *regexpat = NULL;
    GError     *err;
    gint       row = 0;
    gboolean   match_found = FALSE;

    if (!g_file_get_contents (filename, &locale_cont, NULL, NULL))
    {
        //gnc_error_dialog (NULL, _("File %s cannot be opened."), filename );
        return RESULT_OPEN_FAILED;
    }

    // if the contents don't conform to UTF-8, try a default charcter set
    // conversion based on locale
    if (g_utf8_validate(locale_cont, -1, NULL))
    {
        contents = locale_cont;
    }
    else
    {
        contents = g_locale_to_utf8 (locale_cont, -1, NULL, NULL, NULL);
        g_free (locale_cont);
    }

    // Remove the potential XML-prohibited codepoints from the UTF-8 compliant content
    gnc_utf8_strip_invalid(contents);


    // compile the regular expression and check for errors
    err = NULL;
    regexpat =
        g_regex_new (parser_regexp, G_REGEX_OPTIMIZE, 0, &err);
    if (err != NULL)
    {
        gchar *errmsg;

        errmsg = g_strdup_printf (_("Error in regular expression '%s':\n%s"),
                                  parser_regexp, err->message);
        g_error_free (err);

        gnc_error_dialog (window, "%s", errmsg);
        g_free (errmsg);
        g_free (contents);

        return RESULT_ERROR_IN_REGEXP;
    }

    g_regex_match (regexpat, contents, 0, &match_info);
    while (g_match_info_matches (match_info))
    {
        // fill in the values, pattern match names must match those defined in
        // regular expression
        GObject *row_object = csv_import_row_new ();
        fill_model_with_match (match_info, "type", row_object, TYPE);
        fill_model_with_match (match_info, "full_name", row_object, FULL_NAME);
        fill_model_with_match (match_info, "name", row_object, NAME);
        fill_model_with_match (match_info, "code", row_object, CODE);
        fill_model_with_match (match_info, "description", row_object, DESCRIPTION);
        fill_model_with_match (match_info, "color", row_object, COLOR);
        fill_model_with_match (match_info, "notes", row_object, NOTES);
        fill_model_with_match (match_info, "symbol", row_object, SYMBOL);
        fill_model_with_match (match_info, "namespace", row_object, NAMESPACE);
        fill_model_with_match (match_info, "hidden", row_object, HIDDEN);
        fill_model_with_match (match_info, "tax", row_object, TAX);
        fill_model_with_match (match_info, "placeholder", row_object, PLACE_HOLDER);
        csv_import_row_set (row_object, ROW_COLOR, "");

        if (row == 0)
        {
            const gchar *str_type = csv_import_row_get (row_object, TYPE);

            if (g_strcmp0 (_("Type"), str_type) == 0)
                match_found = TRUE;
        }

        g_list_store_append (store, row_object);
        g_object_unref (row_object);

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
 * csv_account_import
 *
 * Parse the liststore for account updates
 *******************************************************/
void
csv_account_import (CsvImportInfo *info)
{
    QofBook       *book;
    Account       *acc, *parent, *root;
    GdkRGBA       testcolor;
    gchar         *type, *full_name, *name, *code, *description, *color;
    gchar         *notes, *symbol, *namespace, *hidden, *tax, *place_holder;
    int            row;

    ENTER("");
    book = gnc_get_current_book();
    root = gnc_book_get_root_account (book);

    info->num_new = 0;
    info->num_updates = 0;

    /* Move to the first valid entry in store */
    row = info->header_rows;
    while (row < g_list_model_get_n_items (G_LIST_MODEL (info->store)))
    {
        /* Walk through the list, reading each row */
        GObject *row_object = g_list_model_get_item (G_LIST_MODEL (info->store), row);
        type = g_strdup (csv_import_row_get (row_object, TYPE));
        full_name = g_strdup (csv_import_row_get (row_object, FULL_NAME));
        name = g_strdup (csv_import_row_get (row_object, NAME));
        code = g_strdup (csv_import_row_get (row_object, CODE));
        description = g_strdup (csv_import_row_get (row_object, DESCRIPTION));
        color = g_strdup (csv_import_row_get (row_object, COLOR));
        notes = g_strdup (csv_import_row_get (row_object, NOTES));
        symbol = g_strdup (csv_import_row_get (row_object, SYMBOL));
        namespace = g_strdup (csv_import_row_get (row_object, NAMESPACE));
        hidden = g_strdup (csv_import_row_get (row_object, HIDDEN));
        tax = g_strdup (csv_import_row_get (row_object, TAX));
        place_holder = g_strdup (csv_import_row_get (row_object, PLACE_HOLDER));

        /* See if we can find the account by full name */
        acc = gnc_account_lookup_by_full_name (root, full_name);

        DEBUG("Row is %u and full name is %s", row, full_name);
        if (acc == NULL)
        {
            /* Account does not exist, Lets try and add it */
            if (g_strrstr (full_name, name) != NULL)
            {
                gint string_position;
                gnc_commodity *commodity;
                gnc_commodity_table *table;
                gchar *full_parent;

                /* Get full name of parent account, allow for separator */
                string_position = strlen (full_name) - strlen (name) - 1;

                if (string_position == -1)
                    full_parent = g_strdup (full_name);
                else
                    full_parent = g_strndup (full_name, string_position);

                parent = gnc_account_lookup_by_full_name (root, full_parent);
                g_free (full_parent);

                if (parent == NULL && string_position != -1)
                {
                    gchar *text = g_strdup_printf (gettext("Row %u, path to account %s not found, added as top level\n"), row + 1, name);
                    info->error = g_strconcat (info->error, text, NULL);
                    g_free (text);
                    PINFO("Unable to import Row %u for account %s, path not found!", row, name);
                }

                if (parent == NULL)
                    parent = root;

                /* Do we have a valid commodity */
                table = gnc_commodity_table_get_table (book);
                commodity = gnc_commodity_table_lookup (table, namespace, symbol);

                if (commodity)
                {
                    DEBUG("We have a valid commodity and will add account %s", full_name);
                    info->num_new =  info->num_new + 1;
                    gnc_suspend_gui_refresh ();
                    acc = xaccMallocAccount (book);
                    xaccAccountBeginEdit (acc);
                    xaccAccountSetName (acc, name);
                    xaccAccountSetType (acc, xaccAccountStringToEnum (type));

                    if (g_strcmp0 (notes, "") != 0)
                        xaccAccountSetNotes (acc, notes);
                    if (g_strcmp0 (description, "") != 0)
                        xaccAccountSetDescription (acc, description);
                    if (g_strcmp0 (code, "") != 0)
                        xaccAccountSetCode (acc, code);

                    if (g_strcmp0 (color, "") != 0)
                    {
                        if (gdk_rgba_parse (&testcolor, color))
                            xaccAccountSetColor (acc, color);
                        else
                            xaccAccountSetColor (acc, "");
                    }

                    if (g_strcmp0 (hidden, "T") == 0)
                        xaccAccountSetHidden (acc, TRUE);
                    if (g_strcmp0 (place_holder, "T") == 0)
                        xaccAccountSetPlaceholder (acc, TRUE);

                    xaccAccountSetCommodity (acc, commodity);
                    xaccAccountBeginEdit (parent);
                    gnc_account_append_child (parent, acc);
                    xaccAccountCommitEdit (parent);
                    xaccAccountCommitEdit (acc);
                    gnc_resume_gui_refresh ();
                }
                else
                {
                    gchar *err_string = g_strdup_printf (gettext("Row %u, commodity %s / %s not found\n"), row + 1,
                                                         symbol, namespace);
                    info->error = g_strconcat (info->error, err_string, NULL);
                    g_free (err_string);
                    PINFO("Unable to import Row %u for account %s, commodity!", row, full_name);
                }
            }
            else
            {
                gchar *err_string = g_strdup_printf (gettext("Row %u, account %s not in %s\n"), row + 1, name, full_name);
                info->error = g_strconcat (info->error, err_string, NULL);
                g_free (err_string);
                PINFO("Unable to import Row %u for account %s, name!", row, full_name);
            }
        }
        else
        {
            /* Lets try and update the color, notes, description, code entries */
            DEBUG("Existing account, will try and update account %s", full_name);
            info->num_updates = info->num_updates + 1;
            if (g_strcmp0 (color, "") != 0)
            {
                if (gdk_rgba_parse (&testcolor, color))
                    xaccAccountSetColor (acc, color);
                else
                    xaccAccountSetColor (acc, "");
            }

            if (g_strcmp0 (notes, "") != 0)
                xaccAccountSetNotes (acc, notes);

            if (g_strcmp0 (description, "") != 0)
                xaccAccountSetDescription (acc, description);

            if (g_strcmp0 (code, "") != 0)
                xaccAccountSetCode (acc, code);
        }
        row++;

        /* free resources */
        g_free (type);
        g_free (full_name);
        g_free (name);
        g_free (code);
        g_free (description);
        g_free (color);
        g_free (notes);
        g_free (symbol);
        g_free (namespace);
        g_free (hidden);
        g_free (tax);
        g_free (place_holder);
        g_object_unref (row_object);
    }
    LEAVE("");
}
