/*
 * dialog-customer_import.h --
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
 * @addtogroup Tools
 * @{
 * @file
 * @brief core import functions for customer import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 */

#ifndef GNC_PLUGIN_customer_import_customer_import_H
#define GNC_PLUGIN_customer_import_customer_import_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

// model
enum customer_import_model_columns
{
    CI_ID, CI_COMPANY,
    CI_NAME, CI_ADDR1, CI_ADDR2, CI_ADDR3, CI_ADDR4, CI_PHONE, CI_FAX, CI_EMAIL,
    CI_NOTES,
    CI_SHIPNAME, CI_SHIPADDR1, CI_SHIPADDR2, CI_SHIPADDR3, CI_SHIPADDR4, CI_SHIPPHONE, 
    CI_SHIPFAX, CI_SHIPEMAIL,  CI_N_COLUMNS
};

enum _customer_import_result
{
    CI_RESULT_OK,
    CI_RESULT_OPEN_FAILED,
    CI_RESULT_ERROR_IN_REGEXP,
};
typedef enum _customer_import_result customer_import_result;

struct _customer_import_stats
{
    int n_imported, n_ignored;
    GString *ignored_lines;
};
typedef struct _customer_import_stats customer_import_stats;


customer_import_result
gnc_customer_import_read_file (const gchar *filename, const gchar *parser_regexp, GtkListStore *store, guint max_rows, customer_import_stats *stats);

void
gnc_customer_import_fix_customers (GtkListStore *store, guint *fixed, guint *deleted, gchar * type);

void
gnc_customer_import_create_customers (GtkListStore *store, QofBook *book, guint *n_customers_created, guint *n_customers_updated, gchar * type);


G_END_DECLS

#endif /* GNC_PLUGIN_customer_import_customer_import_H */

/** @} */
