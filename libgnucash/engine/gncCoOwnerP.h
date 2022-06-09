/********************************************************************\
 * gncCoOwnerP.h -- the Core CoOwner Interface
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

/*
 * Copyright (C) 2022 Ralf Zerres
 * Author: Ralf Zerres <ralf.zerres@mail.de>
 */

#ifndef GNC_COOWNERP_H_
#define GNC_COOWNERP_H_

#include "gncCoOwner.h"

gboolean gncCoOwnerRegister (void);
gchar *gncCoOwnerNextID (QofBook *book);
const gnc_numeric *gncCoOwnerGetCachedBalance (GncCoOwner *coowner);
void gncCoOwnerSetCachedBalance (GncCoOwner *coowner, const gnc_numeric *new_bal);

#define gncCoOwnerSetGUID(C,G) qof_instance_set_guid(QOF_INSTANCE(C),(G))

#endif /* GNC_COOWNERP_H_ */
