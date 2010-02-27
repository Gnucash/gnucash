/********************************************************************\
 * druid-merge.h -- account hierarchy merge functionality           *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2004 Neil Williams <linux@codehelp.co.uk>          *
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

#ifndef DRUID_MERGE_H
#define DRUID_MERGE_H
#include "qof.h"

/** @addtogroup GUI
	@{ */
/** @addtogroup NewHierarchy Merging a new account tree into an existing file

<b>Collision handling principles.</b>\n
\n
This druid builds a second ::QofBook in memory using ::QofSession and
populates the book with accounts created using the usual New Account Tree
code. The druid then uses ::qof_book_merge_init to begin the merge
of the new book (created with QofSession) with the existing QofBook
(loaded by the user), with user intervention and collision handling.

	-# Always check for a ::GUID first and compare. qof_book_merge only accepts valid ::QofBook
	data  and therefore ALL objects in the import book will	include valid GUID's.
	-# If the original import data did not contain a GUID (e.g. an external non-GnuCash source)
	the GUID values will have been created by QofSession and will not match any existing
	GUID's in the target book so objects that do not have a GUID match cannot be assumed to
	be ::MERGE_NEW - parameter values must be checked.

- If a GUID match exists, set QofBookMergeRule::mergeAbsolute to \a TRUE.
	-# If ALL parameters in the import object match the target object with the same \a GUID,
	set ::QofBookMergeResult to \a MERGE_ABSOLUTE.
	-# If any parameters differ, set ::MERGE_UPDATE.
- If the import object \a GUID does not match an existing object,
mergeAbsolute is unchanged from the default \a FALSE
The parameter values of the object are compared to other objects of the same
type in the target book.
	-# If the same data exists in the target book with a different GUID, the object
	is tagged as DUPLICATE.
	-# If the data has changed, the object is tagged as REPORT.
	-# If the data does not match, the object is tagged as NEW

More information is at http://code.neil.williamsleesmill.me.uk/

Each foreach function uses g_return_if_fail checks to protect the target book. If
any essential data is missing, the loop returns without changing the target book.
Note that this will not set or return an error value. However, g_return is only
used for critical errors that arise from programming errors, not for invalid import data
which should be cleaned up before creating the import QofBook.

Only ::qof_book_merge_init, ::qof_book_merge_update_result and ::qof_book_merge_commit return
any error values to the calling process.

	@{ */
/** @file  druid-merge.h
	@brief API for merging two \c QofBook* structures with collision handling
	@author Copyright (c) 2004 Neil Williams <linux@codehelp.co.uk>
*/

void gnc_ui_qsf_import_merge_druid(QofSession *original, QofSession *import);

#define GNC_QSF_IMPORT "gnc-qsf-import-druid"

/** \brief 	gncCommodity is not QOF enabled, need to set a default commodity before the merge */
void currency_transfer_cb ( QofInstance* ent, gpointer user_data);

/** \brief workaround for AccountGroup not being fully QOF enabled. Eh? */
void reference_parent_cb ( QofInstance* ent, gpointer user_data);

/** @} */
/** @} */

#endif
