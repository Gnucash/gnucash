/********************************************************************\
 * gnc-optiondb.h -- Collection of GncOption objects C interface    *
 * Copyright (C) 2019 John Ralls <jralls@ceridwen.us>               *
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
/** @addtogroup Engine
    @{ */
/** @addtogroup Options
    GnuCash Options System for Book, Report, and Stylesheet Options.

    The GnuCash Options System supports two somewhat different purposes:
    - File properties, such as business information and whether to use automatic
      trading accounts.

    - Report options for configuring and customizing reports. Most report
      options are user-configurable through a report options dialog but some are
      used as a way of passing enumeration values between the scheme modules
      that generate the report.

    The options system is centered on an options database or optiondb. A
    separate optionsdb is created and instantiated for every use, so the book
    gets one at the beginning of the session with its values loaded from KVP,
    and every report gets one when the report is run, as do report stylesheets
    when they are edited. Customized report and stylesheet options are saved as
    Scheme code fragments in files in the user's GnuCash Config directory.

    @note
    Persistence via text scheme code is a security vulnerability as it
    enables an attacker to make GnuCash execute arbitrary code. The Guile
    interpreter affords full system access with at least the user's privileges.

    @{ */
/** @file gnc-optiondb.h
    @brief C public interface for the Options Database.
    @author Copyright 2019-2021 John Ralls <jralls@ceridwen.us>
*/
#ifndef GNC_OPTIONDB_H_
#define GNC_OPTIONDB_H_

#ifdef __cplusplus
class GncOption;
class GncOptionDB;
#else
// It's a class in C++ but the C compiler can't tell.
typedef struct GncOption GncOption;
typedef struct GncOptionDB GncOptionDB;
#endif

#include <config.h>
#include "Account.h"
#include "gnc-budget.h"
#include "gnc-commodity.h"
#include "gncInvoice.h"
#include "gncTaxTable.h"

#ifdef __cplusplus
extern "C"
{
#endif

/**
 * Create an empty option database.
 *
 * @return A newly allocated GncOptionDB. Use delete to destroy it.
 */
GncOptionDB* gnc_option_db_new(void);

/**
 * Destruct and release a GncOptionDB.
 * @param odb The GncOptionDB.
 */
void gnc_option_db_destroy(GncOptionDB* odb);

/**
 * Write all changed ui_item values to their options.
 * @param odb The GncOptionDB.
 * @return A GList* conatining the names of options that raised exceptions when
 * attempting to set their values. The names are const, free only the list.
 */
GList* gnc_option_db_commit(GncOptionDB* odb);

/**
 * Reset all ui_items to the option value.
 * @param odb The GncOptionDB.
 */
void gnc_option_db_clean(GncOptionDB* odb);

/**
 * Load a book's options into the GncOptionDB.
 * @param odb The GncOptionDB
 * @param book The book in which the options are saved.
 */
void gnc_option_db_load(GncOptionDB* odb, QofBook* book);

/**
 * Save the GncOptionDB contents into a book's options store.
 * @param odb The GncOptionDB
 * @param book The book in which the options are saved.
 * @param clear_options TRUE if the books existing options should be removed first.
 */
void gnc_option_db_save(GncOptionDB* odb, QofBook* book,
                        gboolean clear_options);

/**
 * Register the standard option set for a QofBook.
 *
 * @param odb The GncOptionDB
 */
void gnc_option_db_book_options(GncOptionDB*);

/**
 * Retrieve the QofInstance value of an option in the GncOptionDB
 *
 * @param odb the GncOptionDB
 * @param section the section in which the option is stored
 * @param name the option name
 * @return the const QofInstance* of the value or nullptr if the option isn't
 * found or if its value isn't a QofInstance*.
 */

const QofInstance* gnc_option_db_lookup_qofinstance_value(GncOptionDB*,
                                                          const char*,
                                                          const char*);

#ifdef __cplusplus
}
#endif
#endif //GNC_OPTIONDB_H_

/** @}
    @} */
