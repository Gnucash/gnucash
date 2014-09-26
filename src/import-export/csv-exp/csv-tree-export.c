/*******************************************************************\
 * csv-tree-export.c -- Export Account Tree to a file               *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
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
/** @file csv-tree-export.c
    @brief CSV Export Account Tree
    @author Copyright (c) 2012 Robert Fewell
*/
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include "gnc-commodity.h"
#include "gnc-ui-util.h"

#include "csv-tree-export.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/* CSV spec requires CRLF line endings. Tweak the end-of-line string so this
 * true for each platform */
#ifdef G_OS_WIN32
# define EOLSTR "\n"
#else
# define EOLSTR "\r\n"
#endif

/******************************************************************/

/*******************************************************
 * write_line_to_file
 *
 * write a text string to a file pointer, return TRUE if
 * successfull.
 *******************************************************/
static
gboolean write_line_to_file (FILE *fh, char * line)
{
    int len, written;
    DEBUG("Account String: %s", line);

    /* Write account line */
    len = strlen (line);
    written = fwrite (line, 1, len, fh);

    if (written != len)
        return FALSE;
    else
        return TRUE;
}

/*******************************************************
 * csv_test_field_string
 *
 * Test the field string for ," and new lines
 *******************************************************/
static
gchar *csv_test_field_string (CsvExportInfo *info, const gchar *string_in)
{
    gboolean need_quote = FALSE;
    gchar **parts;
    gchar *string_parts;
    gchar *string_out;

    /* Check for " and then "" them */
    parts = g_strsplit (string_in, "\"", -1);
    string_parts = g_strjoinv ("\"\"", parts);
    g_strfreev (parts);

    /* Check for separator string and \n and " in field,
       if so quote field if not allready quoted */
    if (g_strrstr (string_parts, info->separator_str) != NULL)
        need_quote = TRUE;
    if (g_strrstr (string_parts, "\n") != NULL)
        need_quote = TRUE;
    if (g_strrstr (string_parts, "\"") != NULL)
        need_quote = TRUE;

    if (!info->use_quotes && need_quote)
        string_out = g_strconcat ("\"", string_parts, "\"", NULL);
    else
        string_out = g_strdup (string_parts);

    g_free (string_parts);
    return string_out;
}

/*******************************************************
 * csv_tree_export
 *
 * write a list of accounts settings to a text file
 *******************************************************/
void csv_tree_export (CsvExportInfo *info)
{
    FILE    *fh;
    Account *root;
    Account *acc;
    GList   *accts, *ptr;

    ENTER("");
    DEBUG("File name is : %s", info->file_name);

    /* Get list of Accounts */
    root = gnc_book_get_root_account (gnc_get_current_book());
    accts = gnc_account_get_descendants_sorted (root);
    info->failed = FALSE;

    /* Open File for writing */
    fh = g_fopen (info->file_name, "w");
    if (fh != NULL)
    {
        gchar *header;
        gchar *part1;
        gchar *part2;
        const gchar *currentSel;
        gchar *end_sep;
        gchar *mid_sep;
        int i;


        /* Set up separators */
        if (info->use_quotes)
        {
            end_sep = "\"";
            mid_sep = g_strconcat ("\"", info->separator_str, "\"", NULL);
        }
        else
        {
            end_sep = "";
            mid_sep = g_strconcat (info->separator_str, NULL);
        }

        /* Header string, 'eol = end of line marker' */
        header = g_strconcat (end_sep, _("type"), mid_sep, _("full_name"), mid_sep, _("name"), mid_sep,
                                _("code"), mid_sep, _("description"), mid_sep, _("color"), mid_sep,
                                _("notes"), mid_sep, _("commoditym"), mid_sep, _("commodityn"), mid_sep,
                                _("hidden"), mid_sep, _("tax"), mid_sep, _("place_holder"), end_sep, EOLSTR, NULL);
        DEBUG("Header String: %s", header);

        /* Write header line */
        if (!write_line_to_file (fh, header))
        {
            info->failed = TRUE;
            g_free (mid_sep);
            g_free (header);
            return;
        }
        g_free (header);

        /* Go through list of accounts */
        for (ptr = accts, i = 0; ptr; ptr = g_list_next (ptr), i++)
        {
            gchar *fullname = NULL;
            gchar *str_temp = NULL;
            acc = ptr->data;
            DEBUG("Account being processed is : %s", xaccAccountGetName (acc));
            /* Type */
            currentSel = xaccAccountTypeEnumAsString (xaccAccountGetType (acc));
            part1 = g_strconcat (end_sep, currentSel, mid_sep, NULL);
            /* Full Name */
            fullname = gnc_account_get_full_name (acc);
            str_temp = csv_test_field_string (info, fullname);
            part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (fullname);
            g_free (part1);
            /* Name */
            currentSel = xaccAccountGetName (acc);
            str_temp = csv_test_field_string (info, currentSel);
            part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part2);
            /* Code */
            currentSel = xaccAccountGetCode (acc) ? xaccAccountGetCode (acc) : "";
            str_temp = csv_test_field_string (info, currentSel);
            part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part1);
            /* Description */
            currentSel = xaccAccountGetDescription (acc) ? xaccAccountGetDescription (acc) : "";
            str_temp = csv_test_field_string (info, currentSel);
            part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part2);
            /* Color */
            currentSel = xaccAccountGetColor (acc) ? xaccAccountGetColor (acc) : "" ;
            part2 = g_strconcat (part1, currentSel, mid_sep, NULL);
            g_free (part1);
            /* Notes */
            currentSel = xaccAccountGetNotes (acc) ? xaccAccountGetNotes (acc) : "" ;
            str_temp = csv_test_field_string (info, currentSel);
            part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part2);
            /* Commodity Mnemonic */
            currentSel = gnc_commodity_get_mnemonic (xaccAccountGetCommodity (acc));
            str_temp = csv_test_field_string (info, currentSel);
            part2 = g_strconcat (part1, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part1);
            /* Commodity Namespace */
            currentSel = gnc_commodity_get_namespace (xaccAccountGetCommodity (acc));
            str_temp = csv_test_field_string (info, currentSel);
            part1 = g_strconcat (part2, str_temp, mid_sep, NULL);
            g_free (str_temp);
            g_free (part2);
            /* Hidden */
            currentSel = xaccAccountGetHidden (acc) ? "T" : "F" ;
            part2 = g_strconcat (part1, currentSel, mid_sep, NULL);
            g_free (part1);
            /* Tax */
            currentSel = xaccAccountGetTaxRelated (acc) ? "T" : "F" ;
            part1 = g_strconcat (part2, currentSel, mid_sep, NULL);
            g_free (part2);
            /* Place Holder / end of line marker */
            currentSel = xaccAccountGetPlaceholder (acc) ? "T" : "F" ;
            part2 = g_strconcat (part1, currentSel, end_sep, EOLSTR, NULL);
            g_free (part1);

            DEBUG("Account String: %s", part2);

            /* Write to file */
            if (!write_line_to_file (fh, part2))
            {
                info->failed = TRUE;
                break;
            }
            g_free (part2);
        }
        g_free (mid_sep);
    }
    else
        info->failed = TRUE;
    if (fh)
        fclose (fh);

    g_list_free (accts);
    LEAVE("");
}



