/********************************************************************\
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include "gtable.h"


struct GTable
{
  GPtrArray *array;

  int rows;
  int cols;

  g_table_allocator allocator;
  g_table_deallocator deallocator;
};

GTable *
g_table_new (g_table_allocator allocator,
             g_table_deallocator deallocator)
{
  GTable *gtable;

  g_assert (allocator);
  g_assert (deallocator);

  gtable = g_new(GTable, 1);

  gtable->array = g_ptr_array_new();

  gtable->rows = 0;
  gtable->cols = 0;

  gtable->allocator = allocator;
  gtable->deallocator = deallocator;

  return gtable;
}

void
g_table_destroy (GTable *gtable)
{
  if (gtable == NULL)
    return;

  g_table_resize (gtable, 0, 0);

  g_ptr_array_free (gtable->array, FALSE);

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

  index = (row * gtable->cols) + col;

  return gtable->array->pdata[index];
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

  /* If shrinking, free extra cells */
  if (new_len < old_len)
  {
    gpointer *tcp;
    guint i;

    tcp = &gtable->array->pdata[new_len];
    for (i = new_len; i < old_len; i++, tcp++)
      gtable->deallocator(*tcp);
  }

  /* Change the size */
  g_ptr_array_set_size(gtable->array, new_len);

  /* If expanding, create the new cells */
  if (new_len > old_len)
  {
    gpointer *tcp;
    guint i;

    tcp = &gtable->array->pdata[old_len];
    for (i = old_len; i < new_len; i++, tcp++)
      *tcp = gtable->allocator();
  }

  gtable->rows = rows;
  gtable->cols = cols;
}
