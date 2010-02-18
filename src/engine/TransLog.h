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

/** @addtogroup Engine
    @{ */
/** @addtogroup TransLog  Transaction Logging
    The transaction logging mechanism provides a very simple,
    low-level logging of user input to a file.   The goal of
    the transaction logger is to provide mechanism of last resort
    for recovering lost user data in the event of a crash.

    Ideally, the storage backends should provide a robust journaling,
    logging and crash-recovery mechanism.  But just in case they
    don't, or it didn't work, this mechanism provides a "Plan B"
    by providing a low-tech, fool-proof, simple logging system
    that can be used to recover user input.
    There are some simple command-line tools that will read a log
    and replay it.

    @{ */
/** @file TransLog.h
    @brief API for the transaction logger
    @author Copyright (C) 1998 Linas Vepstas
*/

#ifndef XACC_TRANS_LOG_H
#define XACC_TRANS_LOG_H

#include "Account.h"
#include "Transaction.h"

void    xaccOpenLog (void);
void    xaccCloseLog (void);
void    xaccReopenLog (void);

/**
 * @param trans The transaction to write out to the log
 * @param flag The engine currently uses the log mechanism with flag char set as
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
void    xaccTransWriteLog (Transaction *trans, char flag);

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

/** Test a filename to see if it is the name of the current logfile */
gboolean xaccFileIsCurrentLog (const gchar *name);

#endif /* XACC_TRANS_LOG_H */
/** @} */
/** @} */

