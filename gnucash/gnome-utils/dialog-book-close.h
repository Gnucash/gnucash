/********************************************************************\
 * dialog-book-close.h -- dialog for helping the user close the     *
 *                        book at the end of the year by adding     *
 *                        zero-izing splits to all Income and       *
 *                        Expense accounts                          *
 *                                                                  *
 * Copyright (C) 2007-8 Derek Atkins <derek@ihtfp.com>              *
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

#ifndef DIALOG_BOOK_CLOSE_H
#define DIALOG_BOOK_CLOSE_H

/** @addtogroup GUI
    @{ */
/** @file dialog-book-close.h
 *
 *  This file contains the functions to present a GUI to select
 *  a book closing date and accounts into which to close the
 *  Income and Expense accounts.
 */

/** Create and run the dialog to close the book.
 *
 *  @param book This parameter specifies the book whose data
 *  will be closed.
 */
void gnc_ui_close_book (QofBook* book);


/** @} */

#endif /* DIALOG_BOOK_CLOSE_H */
