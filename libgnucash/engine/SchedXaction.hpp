/********************************************************************\
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

#ifndef XACC_SCHEDXACTION_HPP
#define XACC_SCHEDXACTION_HPP

#include "qof.h"
#include "gnc-engine.h"

#include <vector>
#include "SX-ttinfo.hpp"

/** \brief Set the schedxaction's template transaction.  tt_vec is a
vector of TTInfo's as defined in SX-ttinfo.hpp The edit dialog doesn't
use this mechanism; maybe it should.

@param sx SchedXaction* the SchedXaction to modify

@param tt_vec TTInfoVec vector of transaction templates to assign to the SX

@param book QofBook* the book that the SX belongs to
*/
void xaccSchedXactionSetTemplateTrans (SchedXaction*, const TTInfoVec&, QofBook*);

#endif /* XACC_SCHEDXACTION_HPP */
