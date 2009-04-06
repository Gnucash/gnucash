/********************************************************************\
 * sixtp-parsers.h -- api for gnucash sixtp parsers                 *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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

#ifndef SIXTP_PARSERS_H
#define SIXTP_PARSERS_H

#include "sixtp.h"

/* Create a parser that will turn the entire sub-tree into a DOM tree
   an pass it in as (don't put anything into parent_data) 
   you must deal with the xml tree in *result.
*/
sixtp* sixtp_dom_parser_new(sixtp_end_handler ender,
                            sixtp_result_handler cleanup_result_by_default_func,
                            sixtp_result_handler cleanup_result_on_fail_func);

#endif /* _SIXTP_PARSERS_H_ */
