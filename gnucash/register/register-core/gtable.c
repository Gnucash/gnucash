/********************************************************************\
 * gtable.c -- glib -- basic datatype for 2D array of values        *
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

#include "gtable.h"


struct GTable
{
    GArray *array;

    guint entry_size;

    int rows;
    int cols;

    g_table_entry_constructor constructor;
    g_table_entry_destroyer destroyer;

    gpointer user_data;
};

GTable *
g_table_new (guint entry_size,
             g_table_entry_constructor constructor,
             g_table_entry_destroyer destroyer,
             gpointer user_data)
{
    GTable *gtable;

    gtable = g_new(GTable, 1);

    gtable->array = g_array_new(FALSE, FALSE, entry_size);

    gtable->entry_size = entry_size;

    gtable->rows = 0;
    gtable->cols = 0;

    gtable->constructor = constructor;
    gtable->destroyer = destroyer;

    gtable->user_data = user_data;

    return gtable;
}

void
g_table_destroy (GTable *gtable)
{
    if (gtable == NULL)
        return;

    g_table_resize (gtable, 0, 0);

    g_array_free (gtable->array, TRUE);

    gtable->array = NULL;

    g_free(gtable);
}

gpointer
g_table_index (GTable *gtable, int row, int col)
{
    guint index;

    if (gtable == NULL)
        return NULL;
    if ((row < 0) || (col < 0))
        return NULL;
    if (row >= gtable->rows)
        return NULL;
    if (col >= gtable->cols)
        return NULL;

    index = ((row * gtable->cols) + col) * gtable->entry_size;

    return &gtable->array->data[index];
}

void
g_table_resize (GTable *gtable, int rows, int cols)
{
    guint old_len;
    guint new_len;

    if (gtable == NULL)
        return;
    if ((rows < 0) || (cols < 0))
        return;

    old_len = gtable->array->len;
    new_len = rows * cols;

    if (new_len == old_len)
        return;

    /* If shrinking, destroy extra cells */
    if ((new_len < old_len) && gtable->destroyer)
    {
        gchar *entry;
        guint i;

        entry = &gtable->array->data[new_len * gtable->entry_size];
        for (i = new_len; i < old_len; i++)
        {
            gtable->destroyer(entry, gtable->user_data);
            entry += gtable->entry_size;
        }
    }

    /* Change the size */
    g_array_set_size(gtable->array, new_len);

    /* If expanding, construct the new cells */
    if ((new_len > old_len) && gtable->constructor)
    {
        gchar *entry;
        guint i;

        entry = &gtable->array->data[old_len * gtable->entry_size];
        for (i = old_len; i < new_len; i++)
        {
            gtable->constructor(entry, gtable->user_data);
            entry += gtable->entry_size;
        }
    }

    gtable->rows = rows;
    gtable->cols = cols;
}

int
g_table_rows (GTable *gtable)
{
    if (gtable == NULL)
        return 0;

    return gtable->rows;
}

int
g_table_cols (GTable *gtable)
{
    if (gtable == NULL)
        return 0;

    return gtable->cols;
}
