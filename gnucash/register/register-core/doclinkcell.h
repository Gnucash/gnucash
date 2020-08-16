/********************************************************************\
 * doclinkcell.h -- Document link checkbox cell                     *
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
 * @file doclinkcell.h
 * @struct Doclinkcell
 * @brief The Doclinkcell object implements a cell handler
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

#ifndef DOC_LINK_CELL_H
#define DOC_LINK_CELL_H

#include <glib.h>

#include "basiccell.h"

#define GLYPH_PAPERCLIP "\360\237\223\216" // Codepoint U+1F4CE
#define GLYPH_LINK      "\360\237\224\227" // Codepoint U+1F517

typedef const char * (*DoclinkcellStringGetter) (char flag);
typedef gboolean (*DoclinkcellConfirm) (char old_flag, gpointer data);

typedef struct
{
    BasicCell cell;

    char flag; /** The actual flag value */

    char * valid_flags;     /** The list of valid flags */
    char * flag_order;      /** Automatic flag selection order */
    char   default_flag;    /** Default flag for unknown user input */

    DoclinkcellStringGetter get_string;
    DoclinkcellConfirm confirm_cb;
    gpointer confirm_data;
    gboolean read_only;
    gboolean use_glyphs;
} Doclinkcell;

BasicCell * gnc_doclink_cell_new (void);

void gnc_doclink_cell_set_flag (Doclinkcell *cell, char flag);
char gnc_doclink_cell_get_flag (Doclinkcell *cell);

void gnc_doclink_cell_set_confirm_cb (Doclinkcell *cell,
                                    DoclinkcellConfirm confirm_cb,
                                    gpointer data);

void gnc_doclink_cell_set_string_getter (Doclinkcell *cell,
                                       DoclinkcellStringGetter getter);

/** note that @param flags is copied into the RecnCell directly, but 
 * remains the "property" of the caller.  The caller must maintain the
 * chars pointer, and the caller must setup a mechanism to 'free' the
 * chars pointer.  The rationale is that you may have many Doclinkcell
 * objects that use the same set of flags.
 */
void gnc_doclink_cell_set_valid_flags (Doclinkcell *cell, const char *flags,
                                     char default_flag);
void gnc_doclink_cell_set_flag_order (Doclinkcell *cell, const char *flags);

void gnc_doclink_cell_set_read_only (Doclinkcell *cell, gboolean read_only);

void gnc_doclink_cell_set_use_glyphs (Doclinkcell *cell);

gboolean gnc_doclink_get_use_glyphs (Doclinkcell *cell);

const char * gnc_doclink_get_glyph_from_flag (char link_flag);

/** @} */
#endif
