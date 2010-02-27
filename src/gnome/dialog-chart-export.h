/***************************************************************************
 *            dialog-chart-export.h
 *
 *  Sun Feb 27 14:19:21 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

#ifndef _DIALOG_CHART_EXPORT_H
#define _DIALOG_CHART_EXPORT_H

/** @addtogroup Import_Export
	@{
*/
/** @addtogroup ChartExport Export a chart of accounts.

Remember to use qof_instance_copy routines
like ::qof_instance_copy_to_session so that the QofBook is marked
as \a partial.

\par guidelines Guidelines for partial book export
-# When exporting GnuCash accounts into QSF, remember that there is no
   AccountGroup in the partial book, not even a root. Some account functions
   that you have used in full books will \b not work within a partial book.
-# Take special care with the book pointer. It is \b very easy to use a
   QofBook* pointer to a book in a different QofSession. Certain API functions
   also make assumptions about the current book - \b check carefully.
-# Remember that just because the function does not use books or AccountGroup
   itself, it does \b not follow that other functions called by the routine
   are also suitable. You may have to reimplement the body of certain functions.
-# Commodities are \b not supported. Most Account functions will use the
   commodity of the account, so be sure to set at least the ::gnc_default_currency().

	@{
*/
/** @file dialog-chart-export.h
    @brief  Chart Export - Routines to export Chart of Accounts to file
    @author Copyright (C) 2005 Neil Williams <linux@codehelp.co.uk>
*/

/** \brief Export the Chart of Accounts to QSF

Write out the Chart of Accounts \b with balances as of a
specific date, as QSF.

The function iterates over each account in the current book and
gets the balance as of the specified date. The account is copied to
the export session, setting the export session book as partial.
The function then looks up the new entity in the export session
book and sets the opening balance of the copied account.
*/
void
gnc_main_window_chart_export(void);

/** @} */
/** @} */

#endif /* _DIALOG_CHART_EXPORT_H */
