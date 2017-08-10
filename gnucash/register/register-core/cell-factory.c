/********************************************************************\
 * cell-factory.c -- register cell creation object                  *
 * Copyright 2001 Free Software Foundation                          *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>

#include "cell-factory.h"
#include "gnc-engine.h"


typedef struct cell_record
{
    char *cell_type_name;

    CellCreateFunc creator;
} CellRecord;

struct cell_factory
{
    GHashTable *cell_table;
};


CellFactory *
gnc_cell_factory_new (void)
{
    CellFactory *cf;

    cf = g_new0 (CellFactory, 1);

    cf->cell_table = g_hash_table_new (g_str_hash, g_str_equal);

    return cf;
}

static void
cell_table_destroy_helper (gpointer key, gpointer value, gpointer user_data)
{
    CellRecord *cr = value;

    g_free (cr->cell_type_name);
    g_free (cr);
}

void
gnc_cell_factory_destroy (CellFactory *cf)
{
    if (!cf) return;

    g_hash_table_foreach (cf->cell_table, cell_table_destroy_helper, NULL);

    g_free (cf);
}

void
gnc_cell_factory_add_cell_type (CellFactory *cf,
                                const char *cell_type_name,
                                CellCreateFunc cell_creator)
{
    CellRecord *cr;

    g_return_if_fail (cell_type_name != NULL);
    g_return_if_fail (cell_creator != NULL);

    cr = g_hash_table_lookup (cf->cell_table, cell_type_name);

    if (cr)
    {
        g_hash_table_remove (cf->cell_table, cell_type_name);
        g_free (cr->cell_type_name);
    }
    else
        cr = g_new0 (CellRecord, 1);

    cr->cell_type_name = g_strdup (cell_type_name);
    cr->creator = cell_creator;

    g_hash_table_insert (cf->cell_table, cr->cell_type_name, cr);
}

BasicCell *
gnc_cell_factory_make_cell (CellFactory *cf, const char *cell_type_name)
{
    CellRecord *cr;

    g_return_val_if_fail (cf != NULL, NULL);
    g_return_val_if_fail (cell_type_name != NULL, NULL);

    cr = g_hash_table_lookup (cf->cell_table, cell_type_name);
    g_return_val_if_fail (cr != NULL, NULL);

    return cr->creator ();
}
