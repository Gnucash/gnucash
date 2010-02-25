/********************************************************************\
 * print-session.h -- data structures for printing via gtkprint     *
 *                       (GnuCash)                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#ifndef PRINT_SESSION_H
#define PRINT_SESSION_H

/** @addtogroup Printing
    @{ */
/** @file print-session.h
    @brief Functions for printing
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
*/

/** @addtogroup Basic Session Functions
    @{ */

#include <gtk/gtkprintoperation.h>

/**
 * Retrieve the print settings from the GtkPrintOperation @a op and save them in
 * a static variable.
 *
 * @param op non-NULL print operation
 */
void gnc_print_operation_save_print_settings(GtkPrintOperation *op);

/**
 * If print settings have been saved by
 * gnc_print_operation_save_print_settings(), then set them on the given
 * GtkPrintOperation @a op.  Set the default page setup as well.
 *
 * @param op non-NULL print operation
 */
void gnc_print_operation_init(GtkPrintOperation *op);

/**
 * Run a page setup dialog and save the resulting GtkPageSetup in a static
 * variable.
 *
 * @param parent Transient parent, or NULL
 */
void gnc_ui_page_setup(GtkWindow *parent);


/** @} */
/** @} */

#endif
