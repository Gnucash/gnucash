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

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#include <config.h>
#include <Account.h>
#include <gnc-budget.h>
#include <gnc-commodity.h>
#include <gncInvoice.h>
#include <gncTaxTable.h>

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
 * Obtain a GncOptionDB* from Scheme
 *
 * When report or stylesheet options are generated in Scheme the GncObjectDB is
 * wrapped in a std::unique_ptr and then in a Guile SMOB by SWIG. The GUI code
 * needs a reference to the GncObjectDB and we don't want to introduce swig
 * library dependencies.
 *
 * @param dispatch The scheme dispatch function returned by gnc:new-options
 * @return GncOptiondDB* Do not free this pointer!
 */
GncOptionDB*
gnc_get_optiondb_from_dispatcher(SCM dispatcher);

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
 * Retrieve the string value of an option in the GncOptionDB
 *
 * @param odb the GncOptionDB
 * @param section the section in which the option is stored
 * @param name the option name
 * @return the static char* of the value or nullptr if the option isn't found
 * or if its value isn't a string.
 */
const char* gnc_option_db_lookup_string_value(GncOptionDB*, const char*,
                                              const char*);

/**
 * Set the string value of an option in the GncOptionDB.
 *
 * The value will not be saved if the option is not in the GncOptionDB or if the
 * type of the option isn't string or text.
 *
 * @param odb the GncOptionDB
 * @param section the section in which the option is stored
 * @param name the option name
 * @param value the value to be stored in the option.
 */
void gnc_option_db_set_string_value(GncOptionDB*, const char*,
                                    const char*, const char*);

/**
 * Retrieve the string value of an option in the GncOptionDB
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

/**
 * Retrieve the GList* value of an option in the GncOptionDB
 *
 * @param odb the GncOptionDB
 * @param section the section in which the option is stored
 * @param name the option name
 * @return the GList* of the value or nullptr if the option isn't found
 * or if its value isn't a string.
 */
SCM gnc_option_db_lookup_scm_value(GncOptionDB*, const char*, const char*);

/**
 * Set the GList* value of an option in the GncOptionDB.
 *
 * The value will not be saved if the option is not in the GncOptionDB or if the
 * type of the option isn't string or text.
 *
 * @param odb the GncOptionDB
 * @param section the section in which the option is stored
 * @param name the option name
 * @param value the value to be stored in the option.
 */
void gnc_option_db_set_scm_value(GncOptionDB*, const char*,
                                    const char*, SCM);

#ifdef __cplusplus
}
#endif
#endif //GNC_OPTIONDB_H_
