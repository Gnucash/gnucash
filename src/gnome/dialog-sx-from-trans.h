/********************************************************************
 * dialog-sx-from-trans.h -- a simple dialog for creating a         *
 *                           scheduled transaction for a "real      *
 *                           one                                    *
 *                       (GnuCash)                                  *
 * Copyright (C) 2000 Robert Merkel <rgmerk@mira.net>               *
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
 ********************************************************************/

#ifndef GNC_DIALOG_SX_FROM_TRANS_H
#define GNC_DIALOG_SX_FROM_TRANS_H

#include "Transaction.h"

void gnc_sx_create_from_trans(Transaction *trans);

#endif
