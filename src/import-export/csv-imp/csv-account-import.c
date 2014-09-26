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

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include "gnc-ui-util.h"
#include <regex.h>
#include "Account.h"
#include "gnc-component-manager.h"
#include "csv-account-import.h"

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
 * csv_import_read_file
 *
 * Parse the file for a correctly formatted file
 *******************************************************/
csv_import_result
csv_import_read_file (const gchar *filename, const gchar *parser_regexp,
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
    {
        //gnc_error_dialog( 0, _("File %s cannot be opened."), filename );
        return RESULT_OPEN_FAILED;
    }

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
        fill_model_with_match (match_info, "type", store, &iter, TYPE);
        fill_model_with_match (match_info, "full_name", store, &iter, FULL_NAME);
        fill_model_with_match (match_info, "name", store, &iter, NAME);
        fill_model_with_match (match_info, "code", store, &iter, CODE);
        fill_model_with_match (match_info, "description", store, &iter, DESCRIPTION);
        fill_model_with_match (match_info, "color", store, &iter, COLOR);
        fill_model_with_match (match_info, "notes", store, &iter, NOTES);
        fill_model_with_match (match_info, "commoditym", store, &iter, COMMODITYM);
        fill_model_with_match (match_info, "commodityn", store, &iter, COMMODITYN);
        fill_model_with_match (match_info, "hidden", store, &iter, HIDDEN);
        fill_model_with_match (match_info, "tax", store, &iter, TAX);
        fill_model_with_match (match_info, "place_holder", store, &iter, PLACE_HOLDER);
        gtk_list_store_set (store, &iter, ROW_COLOR, NULL, -1);

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
    gboolean       valid;
    GdkColor       testcolor;
    GtkTreeIter    iter;
    gchar         *type, *full_name, *name, *code, *description, *color;
    gchar         *notes, *commoditym, *commodityn, *hidden, *tax, *place_holder;
    int            row;

    ENTER("");
    book = gnc_get_current_book();
    root = gnc_book_get_root_account (book);

    info->num_new = 0;
    info->num_updates = 0;

    /* Move to the first valid entry in store */
    row = info->header_rows;
    valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(info->store), &iter, NULL, row );
    while (valid)
    {
        /* Walk through the list, reading each row */
        gtk_tree_model_get (GTK_TREE_MODEL (info->store), &iter,
                            TYPE, &type,
                            FULL_NAME, &full_name,
                            NAME, &name,
                            CODE, &code,
                            DESCRIPTION, &description,
                            COLOR, &color,
                            NOTES, &notes,
                            COMMODITYM, &commoditym,
                            COMMODITYN, &commodityn,
                            HIDDEN, &hidden,
                            TAX, &tax,
                            PLACE_HOLDER, &place_holder, -1);

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
                commodity = gnc_commodity_table_lookup (table, commodityn, commoditym);

                if (commodity)
                {
                    DEBUG("We have a valid commodity and will add account %s", full_name);
                    info->num_new =  info->num_new + 1;
                    gnc_suspend_gui_refresh ();
                    acc = xaccMallocAccount (book);
                    xaccAccountBeginEdit (acc);
                    xaccAccountSetName (acc, name);
                    xaccAccountSetType (acc, xaccAccountStringToEnum (type));

                    if (!g_strcmp0 (notes, "") == 0)
                        xaccAccountSetNotes (acc, notes);
                    if (!g_strcmp0 (description, "") == 0)
                        xaccAccountSetDescription (acc, description);
                    if (!g_strcmp0 (code, "") == 0)
                        xaccAccountSetCode (acc, code);

                    if (!g_strcmp0 (color, "") == 0)
                    {
                        if (gdk_color_parse (color, &testcolor))
                            xaccAccountSetColor (acc, color);
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
                                                         commoditym, commodityn);
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
            if (!g_strcmp0 (color, "") == 0)
            {
                if (gdk_color_parse (color, &testcolor))
                    xaccAccountSetColor (acc, color);
            }

            if (!g_strcmp0 (notes, "") == 0)
                xaccAccountSetNotes (acc, notes);

            if (!g_strcmp0 (description, "") == 0)
                xaccAccountSetDescription (acc, description);

            if (!g_strcmp0 (code, "") == 0)
                xaccAccountSetCode (acc, code);
        }
        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (info->store), &iter);
        row++;

        /* free resources */
        g_free (type);
        g_free (full_name);
        g_free (name);
        g_free (code);
        g_free (description);
        g_free (color);
        g_free (notes);
        g_free (commoditym);
        g_free (commodityn);
        g_free (hidden);
        g_free (tax);
        g_free (place_holder);
    }
    LEAVE("");
}
