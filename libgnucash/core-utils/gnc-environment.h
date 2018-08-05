/********************************************************************\
 * gnc-environment.h -- code to set up the environment for proper   *
 *                      gnucash functioning                         *
 *                                                                  *
 * Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>           *
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

/** @addtogroup Engine
    @{ */
/** @addtogroup Environment Environment

    The API in this file is used to read the environment configuration
    file and set up a number of environment variables based on the values
    found in it. These parameters can be used to configure certain aspects
    of gnucash or components it depends on.

    For example if not installed in the standard prefix "/usr", environment
    variable XDG_DATA_DIRS should be set such that glib can find the
    gsettings schemas installed by GnuCash and yelp can find the help file
    and guide (if these are installed).

    @{ */
/** @file gnc-environment.h
 *  @brief code to set up the environment for proper gnucash functioning.
 *  @author Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_ENVIRONMENT_H
#define GNC_ENVIRONMENT_H

/** Parse <prefix>/etc/gnucash/environment and set environment variables
 *  based on the contents of that file. Read the comments in
 *  <prefix>/etc/gnucash/environment for more details.
 */
void gnc_environment_setup (void);

#endif /* GNC_ENVIRONMENT_H */

/** @} */
/** @} */
