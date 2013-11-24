/********************************************************************\
 * gnc-state.h -- functions to manage gui state                     *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1998 Rob Browning                                  *
 * Copyright (C) 2004 Derek Atkins <derek@ihtfp.com>                *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

/** @addtogroup Utils Utility functions
    @{ */
/** @addtogroup GncState State
 * @{ */
/** @file gnc-state.h
 *  @brief  Functions to load, save and get gui state
 *  @author Copyright (C) 1997 Robin D. Clark
 *  @author Copyright (C) 1998 Linas Vepstas
 *  @author Copyright (C) 1998 Rob Browning
 *  @author Copyright (C) 2004 Derek Atkins <derek@ihtfp.com>
 *  @author Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
 *
 *  The state of the gui (number of open windows, number of open pages in each window,
 *  window positions and sizes, page content of open pages,...) is loaded/saved
 *  from to a file when opening/closing a book. These functions help
 *  in loading/saving from/to the appropriate state file for a given session.
 *
 *  Note that each gui component is responsible itself to actually
 *  add/read its state to/from the state file. The functions in
 *  gnc-state only help to link the in-memory state file to an
 *  actual file on disk.
 */

#ifndef GNC_STATE_H
#define GNC_STATE_H

#include "qof.h"

/* Definitions shared by file-utils.c and gnc-main-window.c */
#define STATE_FILE_TOP           "Top"
#define STATE_FILE_BOOK_GUID     "BookGuid"
#define STATE_FILE_EXT           ".gcm"

/** Load the state from a state file on disk for the given session.
 *
 * @param session The session to load the state for.
 *
 * @return A pointer to a GKeyFile that holds the state information
 *         for the given session. If no state file was found an
 *         empty state is returned (not NULL!). This pointer should
 *         never be freed, except when gnucash is exiting !
 */
GKeyFile *gnc_state_load (const QofSession *session);

/** Save the state to a state file on disk for the given session.
 *
 * @param session The session to save the state for.
 */
void      gnc_state_save (const QofSession *session);

/** Returns a pointer to the most recently loaded state.
 *
 * @return A pointer to a GKeyFile that holds the current state
 *         information. If there is no current state, an
 *         empty state is returned (not NULL!). This pointer should
 *         never be freed, except when gnucash is exiting !
 */
GKeyFile *gnc_state_get_current (void);

/** Drop all sections from the state file whose name contains
 *  partial_name.
 *
 *  This function is meant to be called when an object is deleted
 *  for which state is kept. For example, when an account is
 *  deleted from GnuCash, all state sections that refer to it
 *  should get removed. In that case you can call this function
 *  with the account's guid as parameter.
 *
 *  @param partial_name a string to match in the section names
 *                      for most objects in GnuCash that maintain
 *                      state, this will be the object's guid
 *
 * @return The number of successfully dropped sections.
 */
gint gnc_state_drop_sections_for (const gchar *partial_name);

#endif /* GNC_STATE_H */
/** @} */
/** @} */

