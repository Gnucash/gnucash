/*******************************************************************\
 * dialog-lot-viewer.h -- a basic lot viewer for GnuCash            *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>           *
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

#ifndef LOT_VIEWER_H
#define LOT_VIEWER_H

#include "Account.h"

typedef struct _GNCLotViewer GNCLotViewer;

/** Create and realize and show a lot-viewing dialog. */
GNCLotViewer * gnc_lot_viewer_dialog (Account *);

#endif /* LOT_VIEWER_H */
