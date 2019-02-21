/********************************************************************\
 * gnc-version.h -- functions to query the build-time version info  *
 *                                                                  *
 * Copyright (C) 2019 Geert Janssens <geert@kobaltwit.be>           *
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
 ********************************************************************/

/** @addtogroup Utils
    @{ */
/** @addtogroup GncVersion Version Information

    The functions in this file allow you to query various version related
    strings that were set at build time..

    @{ */
/** @file gnc-version.h
 *  @brief functions to query various version related strings that were set at build time.
 *  @author Copyright (C) 2018 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_VERSION_H
#define GNC_VERSION_H
#include "gnc-vcs-info.h"

/** Parse <prefix>/etc/gnucash/environment and set environment variables
 *  based on the contents of that file. Read the comments in
 *  <prefix>/etc/gnucash/environment for more details.
 */

const char *gnc_version(void);
const char *gnc_build_id(void);
const char *gnc_vcs_rev(void);
const char *gnc_vcs_rev_date(void);
const int gnc_gnucash_major_version(void);

#endif /* GNC_VERSION_H */

/** @} */
/** @} */
