/********************************************************************\
 * io-gncxml-p.h - private header for xml read code                 *
 *                                                                  *
 * Copyright (C) 1999-2001 Rob Browning                             *
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
 *                                                                  *
 ********************************************************************/

#ifndef IO_GNCXML_P_H
#define IO_GNCXML_P_H

#include "sixtp.h"
#include "io-gncxml.h"

typedef enum {
  GNC_PARSE_ERR_NONE,
  GNC_PARSE_ERR_BAD_VERSION,
} GNCParseErr;

typedef struct {
  /* have we gotten the file version yet? */
  gboolean seen_version;
  gint64 version;

  /* top level <gnc-data> parser - we need this so we can set it up
     after we see the file version. */
  sixtp *gnc_parser;

  /* The account group */
  AccountGroup *account_group;

  /* The pricedb */
  GNCPriceDB *pricedb;

  /* The query */
  Query *query;
  
  GNCParseErr error;
} GNCParseStatus;

#endif
