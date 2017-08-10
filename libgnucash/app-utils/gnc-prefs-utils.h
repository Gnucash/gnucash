/********************************************************************\
 * gnc-prefs-utils.h -- utility functions for preferences management*
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
\********************************************************************/

/** @addtogroup GLib
    @{ */
/** @addtogroup Preferences Generic Preference Utilities

    The only function in this file is meant to initialize the
    preferences system early in the load process.

    This is done in a way to hide the actual preferences backend from the
    rest of the engine.

    @{ */
/** @file gnc-prefs-utils.h
 *  @brief Preferences initialization function.
 *  @author Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_PREFS_UTILS_H_
#define GNC_PREFS_UTILS_H_


/** This function is called early in the load process
 *  to preload a number of preferences from the settings backend
 */
void gnc_prefs_init (void);

#endif /* GNC_PREFS_UTILS_H_ */
/** @} */
/** @} */

