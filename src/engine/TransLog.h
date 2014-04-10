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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @addtogroup Engine
    @{ */
/** @file TransLog.h
    @brief API for the transaction logger
*
 * The logfiles are useful for tracing, journalling, error recovery.
 * Note that the current support for journalling is at best
 * embryonic, at worst, is dangerous by setting the wrong expectations.
 *
    @author Copyright (C) 1998 Linas Vepstas
*/

#ifndef XACC_TRANS_LOG_H
#define XACC_TRANS_LOG_H

#include "config.h"

#include "Account.h"
#include "Transaction.h"

void    xaccOpenLog (void);
void    xaccCloseLog (void);
/**
 \param char The engine currently uses the log mechanism with flag char set as
 * follows:
 * 'B' for 'begin edit' (followed by the transaction as it looks
 *     before any changes, i.e. the 'old value')
 * 'D' for delete (i.e. delete the previous B; echoes the data in the
 *     'old B')
 * 'C' for commit (i.e. accept a previous B; data that follows is the
 *     'new value')
 * 'R' for rollback (i.e. revert to previous B; data that follows should
 *     be identical to old B)
 */
void    xaccTransWriteLog (Transaction *, char);

/** document me */
void    xaccLogEnable (void);

/** document me */
void    xaccLogDisable (void);

/** The xaccLogSetBaseName() method sets the base filepath and the
 *    root part of the journal file name.  If the journal file is
 *    already open, it will close it and reopen it with the new
 *    base name.
 */
void    xaccLogSetBaseName (const char *);

#endif /* XACC_TRANS_LOG_H */
/** @} */

