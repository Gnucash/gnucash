/********************************************************************\
 * basiccell.c -- base class for editable cell in a table           *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * FILE:
 * basiccell.c
 *
 * FUNCTION: 
 * Implements the base class for the cell handler object.
 * See the header file for additional documentation.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas <dave@krondo.com>
 */

#include <stdlib.h>
#include <string.h>

#include "basiccell.h"
#include "gnc-engine-util.h"


/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_REGISTER;

BasicCell *
gnc_basic_cell_new (void)
{
  BasicCell * cell;

  cell = g_new0 (BasicCell, 1);

  xaccInitBasicCell (cell);

  return cell;
}

static char *
BasicCellHelpValue(BasicCell *cell)
{
  if ((cell->value != NULL) && (cell->value[0] != 0))
    return g_strdup(cell->value);

  if (cell->blank_help != NULL)
    return g_strdup(cell->blank_help);

  return NULL;
}

static void
gnc_basic_cell_clear (BasicCell *cell)
{
  cell->cell_type = -1;

  cell->changed = FALSE;
  cell->conditionally_changed = FALSE;

  cell->value = NULL;
  cell->value_w = NULL;
  cell->value_len = 0;

  cell->blank_help = NULL;
  cell->set_value = NULL;
  cell->enter_cell = NULL;
  cell->modify_verify = NULL;
  cell->direct_update = NULL;
  cell->leave_cell = NULL;
  cell->gui_realize = NULL;
  cell->gui_move = NULL;
  cell->gui_destroy = NULL;
  cell->get_help_value = NULL;

  cell->is_popup = FALSE;

  cell->gui_private = NULL;
}

void
xaccInitBasicCell (BasicCell *cell)
{
  gnc_basic_cell_clear (cell);

  cell->value = g_strdup ("");

  cell->value_len = gnc_mbstowcs (&cell->value_w, cell->value);

  cell->get_help_value = BasicCellHelpValue;
}

void
gnc_basic_cell_destroy (BasicCell *cell)
{
  if (cell->destroy)
    cell->destroy (cell);

  /* give any gui elements a chance to clean up */
  if (cell->gui_destroy)
    (*(cell->gui_destroy)) (cell);

  /* free up data strings */
  g_free (cell->value);
  cell->value = NULL;

  g_free (cell->value_w);
  cell->value_w = NULL;

  g_free (cell->blank_help);
  cell->blank_help = NULL;

  /* help prevent access to freed memory */
  gnc_basic_cell_clear (cell);

  /* free the object itself */
  g_free (cell);
}

void
gnc_basic_cell_set_name (BasicCell *cell, int cell_type)
{
  if (!cell) return;
  cell->cell_type = cell_type;
}

const char *
gnc_basic_cell_get_value (BasicCell *cell)
{
  g_return_val_if_fail (cell != NULL, NULL);

  return cell->value;
}

void
xaccSetBasicCellValue (BasicCell *cell, const char *val)
{
  CellSetValueFunc cb;

  cb = cell->set_value;
  if (cb)
  {
    /* avoid recursion by disabling the  
     * callback while it's being called. */
    cell->set_value = NULL;
    cb (cell, val);
    cell->set_value = cb;
  }
  else
    xaccSetBasicCellValueInternal (cell, val);
}

gboolean
gnc_basic_cell_get_changed (BasicCell *cell)
{
  if (!cell) return FALSE;

  return cell->changed;
}

gboolean
gnc_basic_cell_get_conditionally_changed (BasicCell *cell)
{
  if (!cell) return FALSE;

  return cell->conditionally_changed;
}

void
gnc_basic_cell_set_changed (BasicCell *cell, gboolean changed)
{
  if (!cell) return;

  cell->changed = changed;
}

void
gnc_basic_cell_set_conditionally_changed (BasicCell *cell, gboolean changed)
{
  if (!cell) return;

  cell->conditionally_changed = changed;
}

void
xaccSetBasicCellBlankHelp (BasicCell *cell, const char *blank_help)
{
  if (cell == NULL)
    return;

  g_free (cell->blank_help);

  if (blank_help == NULL)
    cell->blank_help = NULL;
  else
    cell->blank_help = g_strdup (blank_help);
}

/* ===================================================== */

char *
xaccBasicCellGetHelp (BasicCell *cell)
{
  if (cell == NULL)
    return NULL;

  if (cell->get_help_value == NULL)
    return NULL;

  return cell->get_help_value(cell);
}

/* ===================================================== */

void
xaccSetBasicCellValueInternal (BasicCell *cell, const char *value)
{
  if (value == NULL)
    value = "";

  g_free (cell->value);
  cell->value = g_strdup (value);

  g_free (cell->value_w);
  cell->value_len = gnc_mbstowcs (&cell->value_w, cell->value);
}

void
xaccSetBasicCellWCValueInternal (BasicCell *cell, const GdkWChar *value)
{
  if (!value)
  {
    xaccSetBasicCellValueInternal (cell, "");
    return;
  }

  g_free (cell->value);
  cell->value = gnc_wcstombs (value);

  g_free (cell->value_w);
  cell->value_len = gnc_mbstowcs (&cell->value_w, cell->value);
}

/* ===================================================== */

gint
gnc_mbstowcs (GdkWChar **dest_p, const char *src)
{
  GdkWChar *dest;
  gint src_len;
  gint retval;

  if (!src)
    return -1;

  src_len = strlen (src);

  dest = g_new0 (GdkWChar, src_len + 1);

  retval = gdk_mbstowcs (dest, src, src_len);

  if (retval < 0)
  {
    PERR ("bad multi-byte conversion");
  }

  if (dest_p)
    *dest_p = dest;
  else
    g_free (dest);

  return retval;
}

char *
gnc_wcstombs (const GdkWChar *src)
{
  char *retval;

  if (!src)
    return NULL;

  retval = gdk_wcstombs (src);
  if (!retval)
  {
    PERR ("bad multi-byte conversion");
  }

  return retval;
}

gint
gnc_wcslen (const GdkWChar *src)
{
  int len = 0;

  if (!src)
    return 0;

  while (src[len])
    len++;

  return len;
}

GdkWChar *
gnc_wcsdup (const GdkWChar *src)
{
  GdkWChar *dest;
  int len;
  int i;

  if (!src)
    return NULL;

  len = gnc_wcslen (src);

  dest = g_new (GdkWChar, len + 1);

  for (i = 0; i < len; i++)
    dest[i] = src[i];

  dest[len] = 0;

  return dest;
}

/* ================== end of file ====================== */
