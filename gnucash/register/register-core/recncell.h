/********************************************************************\
 * recncell.h -- reconcile checkbox cell                            *
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
 * @file recncell.h
 * @struct RecnCell
 * @brief The RecnCell object implements a cell handler
 * that will cycle through a series of single-character
 * values when clicked upon by the mouse.
 */
/* HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2001 Derek Atkins
 */

#ifndef RECN_CELL_H
#define RECN_CELL_H

#include <glib.h>

#include "basiccell.h"

typedef struct _RecnCell RecnCell;

typedef const char * (*RecnCellStringGetter) (char flag);

typedef enum
{
    GNC_RECN_CELL_CONFIRM_ACCEPT,
    GNC_RECN_CELL_CONFIRM_DEFERRED,
    GNC_RECN_CELL_CONFIRM_REJECT
} GncRecnCellConfirmResult;

typedef GncRecnCellConfirmResult (*RecnCellConfirm) (RecnCell *cell,
                                                      char old_flag,
                                                      char next_flag,
                                                      gpointer data);

struct _RecnCell
{
    BasicCell cell;

    char flag; /** The actual flag value */

    char * valid_flags;		/** The list of valid flags */
    char * flag_order;		/** Automatic flag selection order */
    char default_flag;		/** Default flag for unknown user input */

    RecnCellStringGetter get_string;
    RecnCellConfirm confirm_cb;
    gpointer confirm_data;
    char pending_flag;
    gboolean confirmation_pending;
    gboolean read_only;
};

BasicCell * gnc_recn_cell_new (void);

void gnc_recn_cell_set_flag (RecnCell *cell, char flag);
char gnc_recn_cell_get_flag (RecnCell *cell);

void gnc_recn_cell_set_confirm_cb (RecnCell *cell,
                                   RecnCellConfirm confirm_cb,
                                   gpointer data);

/* Completes a deferred confirmation. Returns TRUE only when the proposed
 * flag was applied and marked as a table change. */
gboolean gnc_recn_cell_complete_confirm (RecnCell *cell, gboolean accepted);

void gnc_recn_cell_set_string_getter (RecnCell *cell,
                                      RecnCellStringGetter getter);

/** note that chars is copied into the RecnCell directly, but remains
 * the "property" of the caller.  The caller must maintain the chars
 * pointer, and the caller must setup a mechanism to 'free' the chars
 * pointer.  The rationale is that you may have many RecnCell objects
 * that use the same set of flags -- this saves you an alloc/free for
 * each cell.  - warlord  2001-11-28
 */
void gnc_recn_cell_set_valid_flags (RecnCell *cell, const char *flags,
                                    char default_flag);
void gnc_recn_cell_set_flag_order (RecnCell *cell, const char *flags);

void gnc_recn_cell_set_read_only (RecnCell *cell, gboolean read_only);
/** @} */
#endif
