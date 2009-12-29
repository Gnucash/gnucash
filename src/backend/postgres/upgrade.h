/********************************************************************\
 * upgrade.h -- handle back-ward compatible database table upgrades *
 * Copyright (c) 2001 Linas Vepstas <linas@linas.org>               *
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
\********************************************************************/

#ifndef PGEND_UPGRADE_H
#define PGEND_UPGRADE_H

#include "PostgresBackend.h"

/* The pgendVersionIsCurrent() routine ... Are we up to date ?
 *   Return 0 if we are at db version. Return +1 if we are newer.
 *   Return -1 if we are older and so we can't run.
 */

int pgendDBVersionIsCurrent (PGBackend *be);

/* The pgendUpgradeDB() routine installs upgrade features
 *    into the sql database
 */

void pgendUpgradeDB (PGBackend *be);

#endif /* PGEND_UPGRADE_H */
