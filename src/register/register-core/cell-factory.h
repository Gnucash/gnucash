/********************************************************************\
 * cell-factory.h -- register cell creation object                  *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef CELL_FACTORY_H
#define CELL_FACTORY_H

#include "basiccell.h"

typedef struct cell_factory CellFactory;

CellFactory * gnc_cell_factory_new (void);
void gnc_cell_factory_destroy (CellFactory *cf);

void gnc_cell_factory_add_cell_type (CellFactory *cf,
                                     const char *cell_type_name,
                                     CellCreateFunc cell_creator);

BasicCell * gnc_cell_factory_make_cell (CellFactory *cf,
                                        const char *cell_type_name);

#endif
