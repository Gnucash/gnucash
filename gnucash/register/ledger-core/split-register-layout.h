/********************************************************************\
 * split-register-layout.h -- split register layout object          *
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

#ifndef SPLIT_REGISTER_LAYOUT_H
#define SPLIT_REGISTER_LAYOUT_H

#include "table-layout.h"
#include "split-register.h"
/** @addtogroup SplitRegister
 *  @{
 */
/** @file split-register-layout.h
 *  @brief Create the actual register visual layout
 *  @author Copyright (C) 1998, 2004 Linas Vepstas <linas@linas.org>
 *  Pick specific cell types to sit in specific columns, and add
 *  connections so that user can tab from one field to the next.
 *
 *  The actual layout depends on the register type, but, typically,
 *  all of the registers have the date cell on the left, description
 *  in the middle, and monetary totals on the right.
 *
 *  This implementation hard-codes the layout in C, although the
 *  original intent was that the layout would be fetched from a
 *  config file that could be tweaked for a specific, non-GnuCash
 *  application.
 */

/** Generate the split register layout. */
TableLayout * gnc_split_register_layout_new (SplitRegister *reg);

/** @} */
#endif
