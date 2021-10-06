/********************************************************************\
 * gnc-gsettings.h -- utility functions for storing/retrieving      *
 *              data in the GSettings database for GnuCash          *
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
/** @addtogroup GSettings GSettings Utilities

    The API in this file is designed to make it easy to use the GSettings
    system from within Gnucash.  GSettings is a shared key/value storage
    system.

    The main benefits of these routines are that they
    -# maintain a list of GSettings objects (one per schema),
    -# convert gnucash internal schema names into full gsettings schema id's, and
    -# optionally take care of error checking on return values.

    Note that this api should not be called directly. Instead use
    the gnc_gsettings_load_backend function to configure gsettings
    as backend for the gnucash preferences api and then use
    the gnc_prefs_* functions instead to work with preferences.

    @{ */
/** @file gnc-gsettings.h
 *  @brief GSettings helper routines.
 *  @author Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
 */


#ifndef GNC_GSETTINGS_H
#define GNC_GSETTINGS_H


/** Configure gsettings as the backend for the gnucash preferences api.
 */
void gnc_gsettings_load_backend (void);

#endif /* GNC_GSETTINGS_H */
/** @} */
/** @} */
