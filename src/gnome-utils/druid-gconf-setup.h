/*
 * druid-gconf-setup.h  -- install gconf keys where they can be found.
 *
 * Copyright (c) 2005 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup Druids
    @{ */
/** @addtogroup GConfDruid Setup Druid for GConf
    @{ */
/** @file druid-gconf-setup.h
    @brief Check for gconf.  Help user set up if needed.
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>
*/

#ifndef GNC_DRUID_GCONF_SETUP_H
#define GNC_DRUID_GCONF_SETUP_H

/** This routine checks to see if GnuCash's gconf schemas are visible
 *  to the user.  The schemas typically should be visible, as rpm and
 *  deb installs will put the schemas in the default system location.
 *  For things like network installs or developers, this function will
 *  present a warning dialog that asks the user whether to setup
 *  gconf, continue without the schemas, or quit.  If the user chooses
 *  to set up the schemas, this function will invoke a druid to walk
 *  the user through making the schemas visible.
 */
void druid_gconf_install_check_schemas(void);

#endif

/** @} */
/** @} */
