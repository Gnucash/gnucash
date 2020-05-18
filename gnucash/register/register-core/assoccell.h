/********************************************************************\
 * assoccell.h -- association checkbox cell                         *
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

/** @addtogroup Cell Cell
 * @{
 * @file assoccell.h
 * @struct AssocCell
 * @brief The AssocCell object implements a cell handler
 * that will cycle through a series of single-character
 * values when clicked upon by the mouse and will return a glyph
 * if font can show it.
 */
/* HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2001 Derek Atkins
 * Copyright (c) 2020 Robert Fewell
 */

#ifndef ASSOC_CELL_H
#define ASSOC_CELL_H

#include <glib.h>

#include "basiccell.h"

#define GLYPH_PAPERCLIP "\360\237\223\216" // Codepoint U+1F4CE
#define GLYPH_LINK      "\360\237\224\227" // Codepoint U+1F517

typedef const char * (*AssocCellStringGetter) (char flag);
typedef gboolean (*AssocCellConfirm) (char old_flag, gpointer data);

typedef struct
{
    BasicCell cell;

    char flag; /** The actual flag value */

    char * valid_flags;     /** The list of valid flags */
    char * flag_order;      /** Automatic flag selection order */
    char   default_flag;    /** Default flag for unknown user input */

    AssocCellStringGetter get_string;
    AssocCellConfirm confirm_cb;
    gpointer confirm_data;
    gboolean read_only;
    gboolean use_glyphs;
} AssocCell;

BasicCell * gnc_assoc_cell_new (void);

void gnc_assoc_cell_set_flag (AssocCell *cell, char flag);
char gnc_assoc_cell_get_flag (AssocCell *cell);

void gnc_assoc_cell_set_confirm_cb (AssocCell *cell,
                                    AssocCellConfirm confirm_cb,
                                    gpointer data);

void gnc_assoc_cell_set_string_getter (AssocCell *cell,
                                       AssocCellStringGetter getter);

/** note that @param flags is copied into the RecnCell directly, but 
 * remains the "property" of the caller.  The caller must maintain the
 * chars pointer, and the caller must setup a mechanism to 'free' the
 * chars pointer.  The rationale is that you may have many AssocCell
 * objects that use the same set of flags.
 */
void gnc_assoc_cell_set_valid_flags (AssocCell *cell, const char *flags,
                                     char default_flag);
void gnc_assoc_cell_set_flag_order (AssocCell *cell, const char *flags);

void gnc_assoc_cell_set_read_only (AssocCell *cell, gboolean read_only);

void gnc_assoc_cell_set_use_glyphs (AssocCell *cell);

gboolean gnc_assoc_get_use_glyphs (AssocCell *cell);

const char * gnc_assoc_get_glyph_from_flag (char association_flag);

/** @} */
#endif
