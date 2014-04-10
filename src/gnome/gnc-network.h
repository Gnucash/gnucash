/********************************************************************
 * gnc-network.h -- handlers for forms and objects relevant to      *
 * GnuCash Network functions                                        *
 * Copyright (C) 2001 Bill Gribble <grib@billgribble.com>           *
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

#ifndef GNC_NETWORK_H
#define GNC_NETWORK_H

#include "config.h"

void     gnc_network_init(void);

char   * gnc_network_get_passphrase(void);
char   * gnc_network_ask_passphrase(const char * prompt);
char   * gnc_network_build_url(const char * gnc_action);
char   * gnc_network_get_session_id(void);
void     gnc_network_set_session_id(char * sid);
char   * gnc_network_get_uid(void);
#endif
