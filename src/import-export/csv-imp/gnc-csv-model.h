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
\********************************************************************/

/** @file
     @brief CSV import GUI
     *
     gnc-csv-model.h
     @author Copyright (c) 2007 Benny Sperisen <lasindi@gmail.com>
 */

#ifndef GNC_CSV_MODEL_H
#define GNC_CSV_MODEL_H

#include "config.h"

#include "Account.h"
#include "Transaction.h"

#include "stf/stf-parse.h"

/** Enumeration for column types. These are the different types of
 * columns that can exist in a CSV/Fixed-Width file. There should be
 * no two columns with the same type except for the GNC_CSV_NONE
 * type. */
enum GncCsvColumnType {GNC_CSV_NONE,
                       GNC_CSV_DATE,
                       GNC_CSV_NUM,
                       GNC_CSV_DESCRIPTION,
                       GNC_CSV_NOTES,
                       GNC_CSV_ACCOUNT,
                       GNC_CSV_DEPOSIT,
                       GNC_CSV_WITHDRAWAL,
                       GNC_CSV_BALANCE,
                       GNC_CSV_NUM_COL_TYPES
                      };

/** Enumeration for error types. These are the different types of
 * errors that various functions used for the CSV/Fixed-Width importer
 * can have. */
enum GncCsvErrorType {GNC_CSV_FILE_OPEN_ERR,
                      GNC_CSV_ENCODING_ERR
                     };

/** Struct for containing a string. This struct simply contains
 * pointers to the beginning and end of a string. We need this because
 * the STF code that gnc_csv_parse calls requires these pointers. */
typedef struct
{
    char* begin;
    char* end;
} GncCsvStr;

/* TODO We now sort transactions by date, not line number, so we
 * should probably get rid of this struct and uses of it. */

/** Struct pairing a transaction with a line number. This struct is
 * used to keep the transactions in order. When rows are separated
 * into "valid" and "error" lists (in case some of the rows have cells
 * that are unparseable), we want the user to still be able to
 * "correct" the error list. If we keep the line numbers of valid
 * transactions, we can then put transactions created from the newly
 * corrected rows into the right places. */
typedef struct
{
    int line_no;
    Transaction* trans;
    gnc_numeric balance; /**< The (supposed) balance after this transaction takes place */
    gboolean balance_set; /**< TRUE if balance has been set from user data, FALSE otherwise */
    gchar *num; /**< Saves the 'num'for use if balance has been set from user data */
} GncCsvTransLine;

/* A set of currency formats that the user sees. */
extern const int num_currency_formats;
extern const gchar* currency_format_user[];

/* A set of date formats that the user sees. */
extern const int num_date_formats;
extern const gchar* date_format_user[];

/* This array contains all of the different strings for different column types. */
extern gchar* gnc_csv_column_type_strs[];

/** Struct containing data for parsing a CSV/Fixed-Width file. */
typedef struct
{
    gchar* encoding;
    GMappedFile* raw_mapping;   /**< The mapping containing raw_str */
    GncCsvStr raw_str;          /**< Untouched data from the file as a string */
    GncCsvStr file_str;         /**< raw_str translated into UTF-8 */
    GPtrArray* orig_lines;      /**< file_str parsed into a two-dimensional array of strings */
    GArray* orig_row_lengths;   /**< The lengths of rows in orig_lines
                                      before error messages are appended */
    int orig_max_row;           /**< Holds the maximum value in orig_row_lengths */
    GStringChunk* chunk;        /**< A chunk of memory in which the contents of orig_lines is stored */
    StfParseOptions_t* options; /**< Options controlling how file_str should be parsed */
    GArray* column_types;       /**< Array of values from the GncCsvColumnType enumeration */
    GList* error_lines;         /**< List of row numbers in orig_lines that have errors */
    GList* transactions;        /**< List of GncCsvTransLine*s created using orig_lines and column_types */
    int date_format;            /**< The format of the text in the date columns from date_format_internal. */
    int start_row;              /**< The start row to generate transactions from. */
    int end_row;                /**< The end row to generate transactions from. */
    gboolean skip_rows;         /**< Skip Alternate Rows from start row. */
    int currency_format;        /**< The currency format, 0 for locale, 1 for comma dec and 2 for period */
} GncCsvParseData;

GncCsvParseData* gnc_csv_new_parse_data (void);

void gnc_csv_parse_data_free (GncCsvParseData* parse_data);

int gnc_csv_load_file (GncCsvParseData* parse_data, const char* filename,
                      GError** error);

int gnc_csv_convert_encoding (GncCsvParseData* parse_data, const char* encoding, GError** error);

int gnc_csv_parse (GncCsvParseData* parse_data, gboolean guessColTypes, GError** error);

int gnc_csv_parse_to_trans (GncCsvParseData* parse_data, Account* account, gboolean redo_errors);

time64 parse_date (const char* date_str, int format);

#endif
