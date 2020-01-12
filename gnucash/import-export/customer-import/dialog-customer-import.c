/*
 * customer_import.c --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @internal
 * @brief core import functions for customer import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib/gi18n.h>
#include <regex.h>
#include <glib.h>
#include <glib/gstdio.h>

#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
#include "gncAddress.h"
#include "gncCustomerP.h"
#include "gncVendorP.h"
// query
#include "Query.h"
#include "qof.h"
#include "gncIDSearch.h"

#include "dialog-customer-import.h"

// private prototypes
//static GncCustomer *gnc_customer_import_searchCustomer (const gchar *id, QofBook *book);



// perl regular expressions are available

// this helper macro takes a regexp match and fills the model
#define FILL_IN_HELPER(match_name,column) \
            temp = g_match_info_fetch_named (match_info, match_name); \
            if (temp) \
            { \
				g_strstrip( temp ); \
                gtk_list_store_set (store, &iter, column, temp, -1); \
                g_free (temp); \
            }
customer_import_result
gnc_customer_import_read_file (const gchar *filename, const gchar *parser_regexp, GtkListStore *store, guint max_rows, customer_import_stats *stats)
{
    // some statistics
    customer_import_stats stats_fallback;
    FILE *f;

    // regexp
    char *line;
    gchar *line_utf8, *temp;
    GMatchInfo *match_info;
    GError *err;
    GRegex *regexpat;

    // model
    GtkTreeIter iter;

    f = g_fopen( filename, "rt" );
    if (!f)
    {
        //gnc_error_dialog (NULL, _("File %s cannot be opened."), filename );
        return CI_RESULT_OPEN_FAILED;
    }

    // set up statistics
    if (!stats)
        stats = &stats_fallback;

    // compile the regular expression and check for errors
    err = NULL;
    regexpat = g_regex_new (parser_regexp, G_REGEX_EXTENDED | G_REGEX_OPTIMIZE | G_REGEX_DUPNAMES, 0, &err);
    if (err != NULL)
    {
        GtkWidget *dialog;
        gchar *errmsg;

        errmsg = g_strdup_printf (_("Error in regular expression '%s':\n%s"),
                                  parser_regexp, err->message);
        g_error_free (err);
        err = NULL;

        dialog = gtk_message_dialog_new (NULL,
                                         GTK_DIALOG_MODAL,
                                         GTK_MESSAGE_ERROR,
                                         GTK_BUTTONS_OK,
                                         "%s", errmsg);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy(dialog);
        g_free (errmsg);
        errmsg = 0;

        fclose (f);
        return CI_RESULT_ERROR_IN_REGEXP;
    }

    // start the import
    stats->n_imported = 0;
    stats->n_ignored = 0;
    stats->ignored_lines = g_string_new (NULL);
#define buffer_size 1000
    line = g_malloc0 (buffer_size);
    while (!feof (f) && ((max_rows == 0) || (stats->n_imported + stats->n_ignored < max_rows)))
    {
        int l;
        // read one line
        if (!fgets (line, buffer_size, f))
            break; // eof
        // now strip the '\n' from the end of the line
        l = strlen (line);
        if ((l > 0) && (line[l - 1] == '\n'))
            line[l - 1] = 0;

        // convert line from locale into utf8
        line_utf8 = g_locale_to_utf8 (line, -1, NULL, NULL, NULL);

        // parse the line
        match_info = NULL; // it seems, that in contrast to documentation, match_info is not always set -> g_match_info_free will segfault
        if (g_regex_match (regexpat, line_utf8, 0, &match_info))
        {
            // match found
            stats->n_imported++;

            // fill in the values
            gtk_list_store_append (store, &iter);
            FILL_IN_HELPER ("id", CI_ID);
            FILL_IN_HELPER ("company", CI_COMPANY);
            FILL_IN_HELPER ("name", CI_NAME);
            FILL_IN_HELPER ("addr1", CI_ADDR1);
            FILL_IN_HELPER ("addr2", CI_ADDR2);
            FILL_IN_HELPER ("addr3", CI_ADDR3);
            FILL_IN_HELPER ("addr4", CI_ADDR4);
            FILL_IN_HELPER ("phone", CI_PHONE);
            FILL_IN_HELPER ("fax", CI_FAX);
            FILL_IN_HELPER ("email", CI_EMAIL);
            FILL_IN_HELPER ("notes", CI_NOTES);
            FILL_IN_HELPER ("shipname", CI_SHIPNAME);
            FILL_IN_HELPER ("shipaddr1", CI_SHIPADDR1);
            FILL_IN_HELPER ("shipaddr2", CI_SHIPADDR2);
            FILL_IN_HELPER ("shipaddr3", CI_SHIPADDR3);
            FILL_IN_HELPER ("shipaddr4", CI_SHIPADDR4);
            FILL_IN_HELPER ("shipphone", CI_SHIPPHONE);
            FILL_IN_HELPER ("shipfax", CI_SHIPFAX);
            FILL_IN_HELPER ("shipemail", CI_SHIPEMAIL);
        }
        else
        {
            // ignore line
            stats->n_ignored++;
            g_string_append (stats->ignored_lines, line_utf8);
            g_string_append_c (stats->ignored_lines, '\n');
        }

        g_match_info_free (match_info);
        match_info = 0;
        g_free (line_utf8);
        line_utf8 = 0;
    }
    g_free (line);
    line = 0;

    g_regex_unref (regexpat);
    regexpat = 0;
    fclose (f);

    if (stats == &stats_fallback)
        // stats are not requested -> free the string
        g_string_free (stats->ignored_lines, TRUE);

    return CI_RESULT_OK;
}



void
gnc_customer_import_fix_customers (GtkListStore *store, guint *fixed, guint *deleted, gchar * type)
{
    GtkTreeIter iter;
    gboolean valid;
    gchar *company, *name, *addr1, *addr2, *addr3, *addr4;
    guint dummy;

    // allow the call to this function with only GtkListeStore* specified
    if (!fixed)
        fixed = &dummy;
    if (!deleted)
        deleted = &dummy;

    *fixed = 0;
    *deleted = 0;

    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL(store), &iter);
    while (valid)
    {
        // Walk through the list, reading each row
        gtk_tree_model_get (GTK_TREE_MODEL(store), &iter,
                            CI_COMPANY, &company,
                            CI_NAME, &name,
                            CI_ADDR1, &addr1,
                            CI_ADDR2, &addr2,
                            CI_ADDR3, &addr3,
                            CI_ADDR4, &addr4,
                            -1);

        // Company name is mandatory.
        // If not provided, default the company name to the value of the field name.
        if (strlen(company) == 0)
        {
            //But if the field name is also blank, then delete the row.
            if (strlen(name) == 0)
            {
                // no fix possible -> delete row
                valid = gtk_list_store_remove (store, &iter);
                (*deleted)++;
                continue;
            }
            else
            {
                // fix possible -> copy name to company
                gtk_list_store_set (store, &iter, CI_COMPANY, name, -1);
                (*fixed)++;
            }
        }
        
        // At least one of the address fields must have a value.
        // If not, then delete the row.
        if (strlen(addr1) == 0 && strlen(addr2) == 0 && strlen(addr3) == 0 && strlen(addr4) == 0)
        {
            valid = gtk_list_store_remove (store, &iter);
            (*deleted)++;
            continue;
        }
        
        g_free (company);
        g_free (name);
        g_free (addr1);
        g_free (addr2);
        g_free (addr3);
        g_free (addr4);

        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL(store), &iter);
    }
}

void
gnc_customer_import_create_customers (GtkListStore *store, QofBook *book, guint *n_customers_created, guint *n_customers_updated, gchar * type)
{
    gboolean valid;
    GtkTreeIter iter;
    gchar *id, *company, *name, *addr1, *addr2, *addr3, *addr4, *phone, *fax, *email;
    gchar *notes, *shipname, *shipaddr1, *shipaddr2, *shipaddr3, *shipaddr4, *shipphone, *shipfax, *shipemail;
    GncAddress *addr, *shipaddr;
    guint dummy;
    GncCustomer *customer;
    GncVendor *vendor;
    customer = NULL;
    vendor = NULL;
    addr = NULL;
    shipaddr = NULL;
    // these arguments are needed
    g_return_if_fail (store && book);
    printf("\nTYPE = %s\n", type);

    // allow to call this function without statistics
    if (!n_customers_created)
        n_customers_created = &dummy;
    if (!n_customers_updated)
        n_customers_updated = &dummy;
    *n_customers_created = 0;
    *n_customers_updated = 0;

    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL(store), &iter);
    while (valid)
    {
        // Walk through the list, reading each row
        gtk_tree_model_get (GTK_TREE_MODEL(store), &iter,
                            CI_ID, &id,
                            CI_COMPANY, &company,
                            CI_NAME, &name,
                            CI_ADDR1, &addr1,
                            CI_ADDR2, &addr2,
                            CI_ADDR3, &addr3,
                            CI_ADDR4, &addr4,
                            CI_PHONE, &phone,
                            CI_FAX, &fax,
                            CI_EMAIL, &email,
                            CI_NOTES, &notes,
                            CI_SHIPNAME, &shipname,
                            CI_SHIPADDR1, &shipaddr1,
                            CI_SHIPADDR2, &shipaddr2,
                            CI_SHIPADDR3, &shipaddr3,
                            CI_SHIPADDR4, &shipaddr4,
                            CI_SHIPPHONE, &shipphone,
                            CI_SHIPFAX, &shipfax,
                            CI_SHIPEMAIL, &shipemail,
                            -1);

        // Set the customer id if one has not been chosen
        if (strlen (id) == 0)
        {
            if (g_ascii_strcasecmp (type, "CUSTOMER") == 0) id = gncCustomerNextID (book);
            else if (g_ascii_strcasecmp (type, "VENDOR") == 0)id = gncVendorNextID (book);
            //printf("ASSIGNED ID = %s\n",id);
        }

        // Now save it off after checking if a vend/cust number doesn't already exist
        {
            if (g_ascii_strcasecmp (type, "CUSTOMER") == 0)
            {
                customer = gnc_search_customer_on_id (book, id);
                if (!customer)
                {
                    customer = gncCustomerCreate( book );
                    gncCustomerSetCurrency( customer, gnc_default_currency() );
                    (*n_customers_created)++;
                }
                else (*n_customers_updated)++;
            }
            else if (g_ascii_strcasecmp (type, "VENDOR") == 0)
            {
                vendor = gnc_search_vendor_on_id (book, id);
                if ( !vendor)
                {
                    vendor = gncVendorCreate( book );
                    gncVendorSetCurrency( vendor, gnc_default_currency() );
                    (*n_customers_created)++;
                }
                else (*n_customers_updated)++;
            }

            if (g_ascii_strcasecmp (type, "CUSTOMER") == 0)
            {
                gncCustomerBeginEdit (customer);
                gncCustomerSetID (customer, id);
                gncCustomerSetName (customer, company);
                gncCustomerSetNotes (customer, notes);
                addr = gncCustomerGetAddr (customer);
                shipaddr = gncCustomerGetShipAddr (customer);
            }
            else if (g_ascii_strcasecmp (type, "VENDOR") == 0)
            {
                gncVendorBeginEdit (vendor);
                gncVendorSetID (vendor, id);
                gncVendorSetName (vendor, company);
                gncVendorSetNotes (vendor, notes);
                addr = gncVendorGetAddr (vendor);
            }
            gncAddressSetName (addr, name);
            gncAddressSetAddr1 (addr, addr1);
            gncAddressSetAddr2 (addr, addr2);
            gncAddressSetAddr3 (addr, addr3);
            gncAddressSetAddr4 (addr, addr4);
            gncAddressSetPhone (addr, phone);
            gncAddressSetFax (addr, fax);
            gncAddressSetEmail (addr, email);
            if (g_ascii_strcasecmp (type, "CUSTOMER") == 0)
            {
                gncAddressSetName (shipaddr, shipname);
                gncAddressSetAddr1 (shipaddr, shipaddr1);
                gncAddressSetAddr2 (shipaddr, shipaddr2);
                gncAddressSetAddr3 (shipaddr, shipaddr3);
                gncAddressSetAddr4 (shipaddr, shipaddr4);
                gncAddressSetPhone (shipaddr, shipphone);
                gncAddressSetFax (shipaddr, shipfax);
                gncAddressSetEmail (shipaddr, shipemail);
                gncCustomerSetActive (customer, TRUE);
                gncCustomerCommitEdit (customer);
            }
            else if (g_ascii_strcasecmp (type, "VENDOR") == 0)
            {
                gncVendorSetActive (vendor, TRUE);
                gncVendorCommitEdit (vendor);
            }
            //printf("TYPE %s created with ID = %s.\n", type, id); // DEBUG
        }

        g_free (id);
        g_free (company);
        g_free (name);
        g_free (addr1);
        g_free (addr2);
        g_free (addr3);
        g_free (addr4);
        g_free (phone);
        g_free (fax);
        g_free (email);
        g_free (notes);
        g_free (shipname);
        g_free (shipaddr1);
        g_free (shipaddr2);
        g_free (shipaddr3);
        g_free (shipaddr4);
        g_free (shipphone);
        g_free (shipfax);
        g_free (shipemail);
        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL(store), &iter);
    }
}

