/********************************************************************\
 * gnc-csv-imp-trans.cpp - import transactions from csv files       *
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

extern "C" {
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib/gi18n.h>
#include <goffice/go-glib-extras.h>

#include "gnc-csv-account-map.h"
#include "gnc-ui-util.h"
#include "engine-helpers.h"

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <regex.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <math.h>
}

#include "gnc-csv-imp-trans.hpp"

GQuark
gnc_csv_imp_error_quark (void)
{
  return g_quark_from_static_string ("g-csv-imp-error-quark");
}

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

//const int num_date_formats = 5;
//const gchar* date_format_user[] = {N_("y-m-d"),
//                                   N_("d-m-y"),
//                                   N_("m-d-y"),
//                                   N_("d-m"),
//                                   N_("m-d")
//                                  };
//
//const int num_currency_formats = 3;
//const gchar* currency_format_user[] = {N_("Locale"),
//                                       N_("Period: 123,456.78"),
//                                       N_("Comma: 123.456,78")
//                                      };
//
///* This array contains all of the different strings for different column types. */
//const gchar* gnc_csv_column_type_strs[GNC_CSV_NUM_COL_TYPES] = {
//        N_("None"),
//        N_("Date"),
//        N_("Num"),
//        N_("Description"),
//        N_("Notes"),
//        N_("Account"),
//        N_("Deposit"),
//        N_("Withdrawal"),
//        N_("Balance"),
//        N_("Memo"),
//        N_("Other Account"),
//        N_("Other Memo")
//};

/** A set of sensible defaults for parsing CSV files.
 * @return StfParseOptions_t* for parsing a file with comma separators
 */
static StfParseOptions_t* default_parse_options (void)
{
    StfParseOptions_t* options = stf_parse_options_new();
    stf_parse_options_set_type (options, PARSE_TYPE_CSV);
    stf_parse_options_csv_set_separators (options, ",", NULL);
    return options;
}

/** Parses a string into a date, given a format. The format must
 * include the year. This function should only be called by
 * parse_date.
 * @param date_str The string containing a date being parsed
 * @param format An index specifying a format in date_format_user
 * @return The parsed value of date_str on success or -1 on failure
 */
static time64 parse_date_with_year (const char* date_str, int format)
{
    time64 rawtime; /* The integer time */
    struct tm retvalue, test_retvalue; /* The time in a broken-down structure */

    int i, j, mem_length, orig_year = -1, orig_month = -1, orig_day = -1;

    /* Buffer for containing individual parts (e.g. year, month, day) of a date */
    char date_segment[5];

    /* The compiled regular expression */
    regex_t preg = {0};

    /* An array containing indices specifying the matched substrings in date_str */
    regmatch_t pmatch[4] = { {0}, {0}, {0}, {0} };

    /* The regular expression for parsing dates */
    const char* regex = "^ *([0-9]+) *[-/.'] *([0-9]+) *[-/.'] *([0-9]+).*$|^ *([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]).*$";

    /* We get our matches using the regular expression. */
    regcomp (&preg, regex, REG_EXTENDED);
    regexec (&preg, date_str, 4, pmatch, 0);
    regfree (&preg);

    /* If there wasn't a match, there was an error. */
    if (pmatch[0].rm_eo == 0)
        return -1;

    /* If this is a string without separators ... */
    if (pmatch[1].rm_so == -1)
    {
        /* ... we will fill in the indices based on the user's selection. */
        int k = 0; /* k traverses date_str by keeping track of where separators "should" be. */
        j = 1; /* j traverses pmatch. */
        for (i = 0; date_format_user[format][i]; i++)
        {
            char segment_type = date_format_user[format][i];
            /* Only do something if this is a meaningful character */
            if (segment_type == 'y' || segment_type == 'm' || segment_type == 'd')
            {
                pmatch[j].rm_so = k;
                switch (segment_type)
                {
                case 'm':
                case 'd':
                    k += 2;
                    break;

                case 'y':
                    k += 4;
                    break;
                }

                pmatch[j].rm_eo = k;
                j++;
            }
        }
    }

    /* Put some sane values in retvalue by using a fixed time for
     * the non-year-month-day parts of the date. */
    gnc_time (&rawtime);
    gnc_localtime_r (&rawtime, &retvalue);
    retvalue.tm_hour = 11;
    retvalue.tm_min = 0;
    retvalue.tm_sec = 0;
    retvalue.tm_isdst = -1;

    /* j traverses pmatch (index 0 contains the entire string, so we
     * start at index 1 for the first meaningful match). */
    j = 1;
    /* Go through the date format and interpret the matches in order of
     * the sections in the date format. */
    for (i = 0; date_format_user[format][i]; i++)
    {
        char segment_type = date_format_user[format][i];
        /* Only do something if this is a meaningful character */
        if (segment_type == 'y' || segment_type == 'm' || segment_type == 'd')
        {
            /* Copy the matching substring into date_segment so that we can
             * convert it into an integer. */
            mem_length = pmatch[j].rm_eo - pmatch[j].rm_so;
            memcpy (date_segment, date_str + pmatch[j].rm_so, mem_length);
            date_segment[mem_length] = '\0';

            /* Set the appropriate member of retvalue. Save the original
             * values so that we can check if they change when we use gnc_mktime
             * below. */
            switch (segment_type)
            {
            case 'y':
                retvalue.tm_year = atoi (date_segment);

                /* Handle two-digit years. */
                if (retvalue.tm_year < 100)
                {
                    /* We allow two-digit years in the range 1969 - 2068. */
                    if (retvalue.tm_year < 69)
                        retvalue.tm_year += 100;
                }
                else
                    retvalue.tm_year -= 1900;
                orig_year = retvalue.tm_year;
                break;

            case 'm':
                orig_month = retvalue.tm_mon = atoi (date_segment) - 1;
                break;

            case 'd':
                orig_day = retvalue.tm_mday = atoi (date_segment);
                break;
            }
            j++;
        }
    }
    /* Convert back to an integer. If gnc_mktime leaves retvalue unchanged,
     * everything is okay; otherwise, an error has occurred. */
    /* We have to use a "test" date value to account for changes in
     * daylight savings time, which can cause a date change with gnc_mktime
     * near midnight, causing the code to incorrectly think a date is
     * incorrect. */
    test_retvalue = retvalue;
    gnc_mktime (&test_retvalue);
    retvalue.tm_isdst = test_retvalue.tm_isdst;
    rawtime = gnc_mktime (&retvalue);
    if (retvalue.tm_mday == orig_day &&
            retvalue.tm_mon == orig_month &&
            retvalue.tm_year == orig_year)
    {
        return rawtime;
    }
    else
    {
        return -1;
    }
}

/** Parses a string into a date, given a format. The format cannot
 * include the year. This function should only be called by
 * parse_date.
 * @param date_str The string containing a date being parsed
 * @param format An index specifying a format in date_format_user
 * @return The parsed value of date_str on success or -1 on failure
 */
static time64 parse_date_without_year (const char* date_str, int format)
{
    time64 rawtime; /* The integer time */
    struct tm retvalue, test_retvalue; /* The time in a broken-down structure */

    int i, j, mem_length, orig_year = -1, orig_month = -1, orig_day = -1;

    /* Buffer for containing individual parts (e.g. year, month, day) of a date */
    gchar* date_segment;

    /* The compiled regular expression */
    regex_t preg = {0};

    /* An array containing indices specifying the matched substrings in date_str */
    regmatch_t pmatch[3] = { {0}, {0}, {0} };

    /* The regular expression for parsing dates */
    const char* regex = "^ *([0-9]+) *[-/.'] *([0-9]+).*$";

    /* We get our matches using the regular expression. */
    regcomp (&preg, regex, REG_EXTENDED);
    regexec (&preg, date_str, 3, pmatch, 0);
    regfree (&preg);

    /* If there wasn't a match, there was an error. */
    if (pmatch[0].rm_eo == 0)
        return -1;

    /* Put some sane values in retvalue by using a fixed time for
     * the non-year-month-day parts of the date. */
    gnc_time (&rawtime);
    gnc_localtime_r (&rawtime, &retvalue);
    retvalue.tm_hour = 11;
    retvalue.tm_min = 0;
    retvalue.tm_sec = 0;
    retvalue.tm_isdst = -1;
    orig_year = retvalue.tm_year;

    /* j traverses pmatch (index 0 contains the entire string, so we
     * start at index 1 for the first meaningful match). */
    j = 1;
    /* Go through the date format and interpret the matches in order of
     * the sections in the date format. */
    for (i = 0; date_format_user[format][i]; i++)
    {
        char segment_type = date_format_user[format][i];
        /* Only do something if this is a meaningful character */
        if (segment_type == 'm' || segment_type == 'd')
        {
            /* Copy the matching substring into date_segment so that we can
             * convert it into an integer. */
            mem_length = pmatch[j].rm_eo - pmatch[j].rm_so;
            date_segment = g_new (gchar, mem_length);
            memcpy(date_segment, date_str + pmatch[j].rm_so, mem_length);
            date_segment[mem_length] = '\0';

            /* Set the appropriate member of retvalue. Save the original
             * values so that we can check if they change when we use gnc_mktime
             * below. */
            switch (segment_type)
            {
            case 'm':
                orig_month = retvalue.tm_mon = atoi (date_segment) - 1;
                break;

            case 'd':
                orig_day = retvalue.tm_mday = atoi (date_segment);
                break;
            }
            g_free (date_segment);
            j++;
        }
    }
    /* Convert back to an integer. If gnc_mktime leaves retvalue unchanged,
     * everything is okay; otherwise, an error has occurred. */
    /* We have to use a "test" date value to account for changes in
     * daylight savings time, which can cause a date change with gnc_mktime
     * near midnight, causing the code to incorrectly think a date is
     * incorrect. */
    test_retvalue = retvalue;
    gnc_mktime (&test_retvalue);
    retvalue.tm_isdst = test_retvalue.tm_isdst;
    rawtime = gnc_mktime (&retvalue);
    if (retvalue.tm_mday == orig_day &&
            retvalue.tm_mon == orig_month &&
            retvalue.tm_year == orig_year)
    {
        return rawtime;
    }
    else
    {
        return -1;
    }
}

/** Parses a string into a date, given a format. This function
 * requires only knowing the order in which the year, month and day
 * appear. For example, 01-02-2003 will be parsed the same way as
 * 01/02/2003.
 * @param date_str The string containing a date being parsed
 * @param format An index specifying a format in date_format_user
 * @return The parsed value of date_str on success or -1 on failure
 */
time64 parse_date (const char* date_str, int format)
{
    if (strchr (date_format_user[format], 'y'))
        return parse_date_with_year (date_str, format);
    else
        return parse_date_without_year (date_str, format);
}

/** Constructor for GncCsvParseData.
 * @return Pointer to a new GncCSvParseData
 */
GncCsvParseData::GncCsvParseData()
{
    encoding = "UTF-8";
    /* All of the data pointers are initially NULL. This is so that, if
     * gnc_csv_parse_data_free is called before all of the data is
     * initialized, only the data that needs to be freed is freed. */
    raw_mapping = NULL;
    raw_str.begin = raw_str.end = file_str.begin = file_str.end = NULL;
    orig_lines = NULL;
    orig_row_lengths = NULL;
    column_types = NULL;
    error_lines = transactions = NULL;
    options = default_parse_options();
    date_format = -1;
    currency_format = 0;
    chunk = g_string_chunk_new(100 * 1024);
    start_row = 0;
    end_row = 1000;
    skip_rows = FALSE;
}

/** Destructor for GncCsvParseData.
 */
GncCsvParseData::~GncCsvParseData()
{
    /* All non-NULL pointers have been initialized and must be freed. */

    if (raw_mapping != NULL)
    {
        g_mapped_file_unref (raw_mapping);
    }

    if (file_str.begin != NULL)
        g_free (file_str.begin);

    if (orig_lines != NULL)
        stf_parse_general_free (orig_lines);

    if (orig_row_lengths != NULL)
        g_array_free (orig_row_lengths, FALSE);

    if (options != NULL)
        stf_parse_options_free (options);

    if (column_types != NULL)
        g_array_free (column_types, TRUE);

    if (error_lines != NULL)
        g_list_free (error_lines);

    if (transactions != NULL)
        g_list_free_full (transactions, g_free);

    g_string_chunk_free (chunk);
}

/** Converts raw file data using a new encoding. This function must be
 * called after gnc_csv_load_file only if gnc_csv_load_file guessed
 * the wrong encoding.
 * @param parse_data Data that is being parsed
 * @param encoding Encoding that data should be translated using
 * @param error Will point to an error on failure
 * @return 0 on success, 1 on failure
 */
int GncCsvParseData::convert_encoding (const char* encoding,
                                       GError** error)
{
    gsize bytes_read, bytes_written;

    /* If file_str has already been initialized it must be
     * freed first. (This should always be the case, since
     * gnc_csv_load_file should always be called before this
     * function.) */
    if (file_str.begin != NULL)
        g_free(file_str.begin);

    /* Do the actual translation to UTF-8. */
    file_str.begin = g_convert (raw_str.begin,
                                           raw_str.end - raw_str.begin,
                                           "UTF-8", encoding, &bytes_read, &bytes_written,
                                           error);
    /* Handle errors that occur. */
    if (file_str.begin == NULL)
        return 1;

    /* On success, save the ending pointer of the translated data and
     * the encoding type and return 0. */
    file_str.end = file_str.begin + bytes_written;
    encoding = (gchar*)encoding;
    return 0;
}

/** Loads a file into a GncCsvParseData. This is the first function
 * that must be called after creating a new GncCsvParseData. If this
 * fails because the file couldn't be opened, no more functions can be
 * called on the parse data until this succeeds (or until it fails
 * because of an encoding guess error). If it fails because the
 * encoding could not be guessed, gnc_csv_convert_encoding must be
 * called until it succeeds.
 * @param parse_data Data that is being parsed
 * @param filename Name of the file that should be opened
 * @param error Will contain an error if there is a failure
 * @return 0 on success, 1 on failure
 */
int GncCsvParseData::load_file (const char* filename,
                                GError** error)
{
    const char* guess_enc = NULL;

    /* Get the raw data first and handle an error if one occurs. */
    raw_mapping = g_mapped_file_new (filename, FALSE, error);
    if (raw_mapping == NULL)
    {
        /* TODO Handle file opening errors more specifically,
         * e.g. inexistent file versus no read permission. */
        raw_str.begin = NULL;
        g_set_error (error, GNC_CSV_IMP_ERROR, GNC_CSV_IMP_ERROR_OPEN, "%s", _("File opening failed."));
        return 1;
    }

    /* Copy the mapping's contents into parse-data->raw_str. */
    raw_str.begin = g_mapped_file_get_contents (raw_mapping);
    raw_str.end = raw_str.begin + g_mapped_file_get_length (raw_mapping);

    /* Make a guess at the encoding of the data. */
    if (!g_mapped_file_get_length (raw_mapping) == 0)
        guess_enc = go_guess_encoding ((const char*)(raw_str.begin),
                                      (size_t)(raw_str.end - raw_str.begin),
                                      "UTF-8", NULL);
    if (guess_enc == NULL)
    {
        g_set_error (error, GNC_CSV_IMP_ERROR, GNC_CSV_IMP_ERROR_ENCODING, "%s", _("Unknown encoding."));
        return 1;
    }
    /* Convert using the guessed encoding into file_str and
     * handle any errors that occur. */
    convert_encoding (guess_enc, error);
    if (file_str.begin == NULL)
    {
        g_set_error (error, GNC_CSV_IMP_ERROR, GNC_CSV_IMP_ERROR_ENCODING, "%s", _("Unknown encoding."));
        return 1;
    }
    else
        return 0;
}

/** Parses a file into cells. This requires having an encoding that
 * works (see gnc_csv_convert_encoding). options should be
 * set according to how the user wants before calling this
 * function. (Note: this function must be called with guessColTypes as
 * TRUE before it is ever called with it as FALSE.) (Note: if
 * guessColTypes is TRUE, all the column types will be GNC_CSV_NONE
 * right now.)
 * @param parse_data Data that is being parsed
 * @param guessColTypes TRUE to guess what the types of columns are based on the cell contents
 * @param error Will contain an error if there is a failure
 * @return 0 on success, 1 on failure
 */
int GncCsvParseData::parse (gboolean guessColTypes, GError** error)
{
    /* max_cols is the number of columns in the row with the most columns. */
    guint i, max_cols = 0;

    if (orig_lines != NULL)
    {
        stf_parse_general_free (orig_lines);
    }

    /* If everything is fine ... */
    if (file_str.begin != NULL)
    {
        /* Do the actual parsing. */
        orig_lines = stf_parse_general (options, chunk,
                                 file_str.begin,
                                 file_str.end);
    }
    /* If we couldn't get the encoding right, we just want an empty array. */
    else
    {
        orig_lines = g_ptr_array_new();
    }

    /* Record the original row lengths of orig_lines. */
    if (orig_row_lengths != NULL)
        g_array_free (orig_row_lengths, FALSE);

    orig_row_lengths =
        g_array_sized_new (FALSE, FALSE, sizeof(int), orig_lines->len);

    g_array_set_size (orig_row_lengths, orig_lines->len);
    orig_max_row = 0;
    for (i = 0; i < orig_lines->len; i++)
    {
        int length = ((GPtrArray*)orig_lines->pdata[i])->len;
        orig_row_lengths->data[i] = length;
        if (length > orig_max_row)
            orig_max_row = length;
    }

    /* If it failed, generate an error. */
    if (orig_lines == NULL)
    {
        g_set_error (error, GNC_CSV_IMP_ERROR, GNC_CSV_IMP_ERROR_PARSE, "Parsing failed.");
        return 1;
    }

    /* Now that we have data, let's set max_cols. */
    for (i = 0; i < orig_lines->len; i++)
    {
        if (max_cols < ((GPtrArray*)(orig_lines->pdata[i]))->len)
            max_cols = ((GPtrArray*)(orig_lines->pdata[i]))->len;
    }

    if (guessColTypes)
    {
        /* Free column_types if it's already been created. */
        if (column_types != NULL)
            g_array_free (column_types, TRUE);

        /* Create column_types and fill it with guesses based
         * on the contents of each column. */
        column_types = g_array_sized_new (FALSE, FALSE, sizeof(int),
                                   max_cols);
        g_array_set_size (column_types, max_cols);
        /* TODO Make it actually guess. */
        for (i = 0; i < column_types->len; i++)
        {
            column_types->data[i] = GNC_CSV_NONE;
        }
    }
    else
    {
        /* If we don't need to guess column types, we will simply set any
         * new columns that are created that didn't exist before to "None"
         * since we don't want gibberish to appear. Note:
         * column_types should have already been
         * initialized, so we don't check for it being NULL. */
        i = column_types->len;
        g_array_set_size (column_types, max_cols);
        for (; i < column_types->len; i++)
        {
            column_types->data[i] = GNC_CSV_NONE;
        }
    }
    return 0;
}

/** A struct containing TransProperties that all describe a single transaction. */
typedef struct
{
    int date_format; /**< The format for parsing dates */
    int currency_format; /**< The format for currency */
    Account* account; /**< The account the transaction belongs to */
    GList* properties; /**< List of TransProperties */
} TransPropertyList;

/** A struct encapsulating a property of a transaction. */
typedef struct
{
    int type;                /**< A value from the GncCsvColumnType enum except
                               * GNC_CSV_NONE and GNC_CSV_NUM_COL_TYPES */
    void* value;             /**< Pointer to the data that will be used to configure a transaction */
    TransPropertyList* list; /**< The list the property belongs to */
} TransProperty;

/** Constructor for TransProperty.
 * @param type The type of the new property (see TransProperty.type for possible values)
 */
static TransProperty* trans_property_new (int type, TransPropertyList* list)
{
    TransProperty* prop = g_new (TransProperty, 1);
    prop->type = type;
    prop->list = list;
    prop->value = NULL;
    return prop;
}

/** Destructor for TransProperty.
 * @param prop The property to be freed
 */
static void trans_property_free (TransProperty* prop)
{
    switch (prop->type)
    {
        /* The types for "Date" and "Balance" (time64 and gnc_numeric,
         * respectively) are typically not pointed to, we have to free
         * them, unlike types like char* ("Description"). */
    case GNC_CSV_DATE:
    case GNC_CSV_BALANCE:
    case GNC_CSV_DEPOSIT:
    case GNC_CSV_WITHDRAWAL:
        if (prop->value != NULL)
            g_free(prop->value);
        break;
    default:
       break;
    }
    g_free (prop);
}

/** Sets the value of the property by parsing str. Note: this should
 * only be called once on an instance of TransProperty, as calling it
 * more than once can cause memory leaks.
 * @param prop The property being set
 * @param str The string to be parsed
 * @return TRUE on success, FALSE on failure
 */
static gboolean trans_property_set (TransProperty* prop, char* str)
{
    char *endptr, *possible_currency_symbol, *str_dupe;
    gnc_numeric val;
    int reti;
    regex_t regex;
    switch (prop->type)
    {
    case GNC_CSV_DATE:
        prop->value = g_new(time64, 1);
        *((time64*)(prop->value)) = parse_date(str, prop->list->date_format);
        return *((time64*)(prop->value)) != -1;

    case GNC_CSV_DESCRIPTION:
    case GNC_CSV_NOTES:
    case GNC_CSV_MEMO:
    case GNC_CSV_OMEMO:
    case GNC_CSV_NUM:
        prop->value = g_strdup (str);
        return TRUE;

    case GNC_CSV_OACCOUNT:
        prop->value = gnc_csv_account_map_search (str);
        return TRUE;

    case GNC_CSV_BALANCE:
    case GNC_CSV_DEPOSIT:
    case GNC_CSV_WITHDRAWAL:
        str_dupe = g_strdup (str); /* First, we make a copy so we can't mess up real data. */
        /* If a cell is empty or just spaces make its value = "0" */
        reti = regcomp(&regex, "[0-9]", 0);
        reti = regexec(&regex, str_dupe, 0, NULL, 0);
        if (reti == REG_NOMATCH)
        {
            g_free (str_dupe);
            str_dupe = g_strdup ("0");
        }
        /* Go through str_dupe looking for currency symbols. */
        for (possible_currency_symbol = str_dupe; *possible_currency_symbol;
                possible_currency_symbol = g_utf8_next_char (possible_currency_symbol))
        {
            if (g_unichar_type (g_utf8_get_char (possible_currency_symbol)) == G_UNICODE_CURRENCY_SYMBOL)
            {
                /* If we find a currency symbol, save the position just ahead
                 * of the currency symbol (next_symbol), and find the null
                 * terminator of the string (last_symbol). */
                char *next_symbol = g_utf8_next_char (possible_currency_symbol), *last_symbol = next_symbol;
                while (*last_symbol)
                    last_symbol = g_utf8_next_char (last_symbol);

                /* Move all of the string (including the null byte, which is
                 * why we have +1 in the size parameter) following the
                 * currency symbol back one character, thereby overwriting the
                 * currency symbol. */
                memmove (possible_currency_symbol, next_symbol, last_symbol - next_symbol + 1);
                break;
            }
        }

        /* Currency format */
        switch (prop->list->currency_format)
        {
        case 0:
            /* Currency locale */
            if (!(xaccParseAmount (str_dupe, TRUE, &val, &endptr)))
            {
                g_free (str_dupe);
                return FALSE;
            }
            break;
        case 1:
            /* Currency decimal period */
            if (!(xaccParseAmountExtended (str_dupe, TRUE, '-', '.', ',', "\003\003", "$+", &val, &endptr)))
            {
                g_free (str_dupe);
                return FALSE;
            }
            break;
        case 2:
            /* Currency decimal comma */
            if (!(xaccParseAmountExtended (str_dupe, TRUE, '-', ',', '.', "\003\003", "$+", &val, &endptr)))
            {
                g_free (str_dupe);
                return FALSE;
            }
            break;
        }

        prop->value = g_new (gnc_numeric, 1);
        *((gnc_numeric*)(prop->value)) = val;
        g_free (str_dupe);
        return TRUE;

    default:
        break;
    }
    return FALSE; /* We should never actually get here. */
}

/** Constructor for TransPropertyList.
 * @param account The account with which transactions should be built
 * @param date_format An index from date_format_user for how date properties should be parsed
 * @return A pointer to a new TransPropertyList
 */
static TransPropertyList* trans_property_list_new (Account* account, int date_format, int currency_format)
{
    TransPropertyList* list = g_new (TransPropertyList, 1);
    list->account = account;
    list->date_format = date_format;
    list->currency_format = currency_format;
    list->properties = NULL;
    return list;
}

/** Destructor for TransPropertyList.
 * @param list The list to be freed
 */
static void trans_property_list_free (TransPropertyList* list)
{
    /* Free all of the properties in this list before freeing the list itself. */
    GList* properties_begin = list->properties;
    while (list->properties != NULL)
    {
        trans_property_free ((TransProperty*)(list->properties->data));
        list->properties = g_list_next (list->properties);
    }
    g_list_free (properties_begin);
    g_free (list);
}

/** Adds a property to the list it's linked with.
 * (The TransPropertyList is not passed as a parameter because the property is
 * associated with a list when it's constructed.)
 * @param property The property to be added to its list
 */
static void trans_property_list_add (TransProperty* property)
{
    property->list->properties = g_list_append (property->list->properties, property);
}

/** Adds a split to a transaction.
 * @param trans The transaction to add a split to
 * @param account The account used for the split
 * @param book The book where the split should be stored
 * @param amount The amount of the split
 */
static void trans_add_split (Transaction* trans, Account* account, QofBook* book,
                            gnc_numeric amount, const char *num, const char *memo)
{
    Split* split = xaccMallocSplit (book);
    xaccSplitSetAccount (split, account);
    xaccSplitSetParent (split, trans);
    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, amount);
    xaccSplitSetMemo (split, memo);
    /* set tran-num and/or split-action per book option */
    gnc_set_num_action (trans, split, num, NULL);
}

/** Adds a other split to a transaction.
 * @param trans The transaction to add a split to
 * @param account The account used for the other split
 * @param book The book where the split should be stored
 * @param amount The amount of the split
 */
static void trans_add_osplit (Transaction* trans, Account* account, QofBook* book,
                            gnc_numeric amount, const char *num, const char *memo)
{
    Split *osplit = xaccMallocSplit (book);
    xaccSplitSetAccount (osplit, account);
    xaccSplitSetParent (osplit, trans);
    xaccSplitSetAmount (osplit, amount);
    xaccSplitSetValue (osplit, gnc_numeric_neg (amount));
    xaccSplitSetMemo (osplit, memo);
}

/** Tests a TransPropertyList for having enough essential properties.
 * Essential properties are "Date" and one of the following: "Balance", "Deposit", or
 * "Withdrawal".
 * @param list The list we are checking
 * @param error Contains an error message on failure
 * @return TRUE if there are enough essentials; FALSE otherwise
 */
static gboolean trans_property_list_verify_essentials (TransPropertyList* list, gchar** error)
{
    int i;
    /* possible_errors lists the ways in which a list can fail this test. */
    enum PossibleErrorTypes {NO_DATE, NO_AMOUNT, NUM_OF_POSSIBLE_ERRORS};
    const gchar* possible_errors[NUM_OF_POSSIBLE_ERRORS] =
    {
        N_("No date column."),
        N_("No balance, deposit, or withdrawal column.")
    };
    int possible_error_lengths[NUM_OF_POSSIBLE_ERRORS] = {0};
    GList *properties_begin = list->properties, *errors_list = NULL;

    /* Go through each of the properties and erase possible errors. */
    while (list->properties)
    {
        switch (((TransProperty*)(list->properties->data))->type)
        {
        case GNC_CSV_DATE:
            possible_errors[NO_DATE] = NULL;
            break;

        case GNC_CSV_BALANCE:
        case GNC_CSV_DEPOSIT:
        case GNC_CSV_WITHDRAWAL:
            possible_errors[NO_AMOUNT] = NULL;
            break;
        default:
            break;
        }
        list->properties = g_list_next (list->properties);
    }
    list->properties = properties_begin;

    /* Accumulate a list of the actual errors. */
    for (i = 0; i < NUM_OF_POSSIBLE_ERRORS; i++)
    {
        if (possible_errors[i] != NULL)
        {
            errors_list = g_list_append (errors_list, GINT_TO_POINTER(i));
            /* Since we added an error, we want to also store its length for
             * when we construct the full error string. */
            possible_error_lengths[i] = strlen (_(possible_errors[i]));
        }
    }

    /* If there are no errors, we can quit now. */
    if (errors_list == NULL)
        return TRUE;
    else
    {
        /* full_error_size is the full length of the error message. */
        int full_error_size = 0, string_length = 0;
        GList* errors_list_begin = errors_list;
        gchar *error_message, *error_message_begin;

        /* Find the value for full_error_size. */
        while (errors_list)
        {
            /* We add an extra 1 to account for spaces in between messages. */
            full_error_size += possible_error_lengths[GPOINTER_TO_INT(errors_list->data)] + 1;
            errors_list = g_list_next (errors_list);
        }
        errors_list = errors_list_begin;

        /* Append the error messages one after another. */
        error_message = error_message_begin = g_new (gchar, full_error_size);
        while (errors_list)
        {
            i = GPOINTER_TO_INT(errors_list->data);
            string_length = possible_error_lengths[i];

            /* Copy the error message and put a space after it. */
            strncpy(error_message, _(possible_errors[i]), string_length);
            error_message += string_length;
            *error_message = ' ';
            error_message++;

            errors_list = g_list_next (errors_list);
        }
        *error_message = '\0'; /* Replace the last space with the null byte. */
        g_list_free (errors_list_begin);

        *error = error_message_begin;
        return FALSE;
    }
}

/** Create a Transaction from a TransPropertyList.
 * @param list The list of properties
 * @param error Contains an error on failure
 * @return On success, a GncCsvTransLine; on failure, the trans pointer is NULL
 */
static GncCsvTransLine* trans_property_list_to_trans (TransPropertyList* list, gchar** error)
{
    GncCsvTransLine* trans_line = g_new (GncCsvTransLine, 1);
    GList* properties_begin = list->properties;
    QofBook* book = gnc_account_get_book (list->account);
    gnc_commodity* currency = xaccAccountGetCommodity (list->account);
    gnc_numeric amount = double_to_gnc_numeric (0.0, xaccAccountGetCommoditySCU (list->account),
                         GNC_HOW_RND_ROUND_HALF_UP);
    gchar *num = NULL;
    gchar *memo = NULL;
    gchar *omemo = NULL;
    Account *oaccount = NULL;

    /* This flag is set to TRUE if we can use the "Deposit" or "Withdrawal" column. */
    gboolean amount_set = FALSE;

    /* The balance is 0 by default. */
    trans_line->balance_set = FALSE;
    trans_line->balance = amount;
    trans_line->num = NULL;

    /* We make the line_no -1 just to mark that it hasn't been set. We
     * may get rid of line_no soon anyway, so it's not particularly
     * important. */
    trans_line->line_no = -1;

    /* Make sure this is a transaction with all the columns we need. */
    if (!trans_property_list_verify_essentials (list, error))
    {
        g_free(trans_line);
        return NULL;
    }

    trans_line->trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (trans_line->trans);
    xaccTransSetCurrency (trans_line->trans, currency);

    /* Go through each of the properties and edit the transaction accordingly. */
    list->properties = properties_begin;
    while (list->properties != NULL)
    {
        TransProperty* prop = (TransProperty*)(list->properties->data);
        switch (prop->type)
        {
        case GNC_CSV_DATE:
            xaccTransSetDatePostedSecsNormalized (trans_line->trans, *((time64*)(prop->value)));
            break;

        case GNC_CSV_DESCRIPTION:
            xaccTransSetDescription (trans_line->trans, (char*)(prop->value));
            break;

        case GNC_CSV_NOTES:
            xaccTransSetNotes (trans_line->trans, (char*)(prop->value));
            break;

        case GNC_CSV_OACCOUNT:
            oaccount = ((Account*)(prop->value));
            break;

        case GNC_CSV_MEMO:
            memo = g_strdup ((char*)(prop->value));
            break;

        case GNC_CSV_OMEMO:
            omemo = g_strdup ((char*)(prop->value));
            break;

        case GNC_CSV_NUM:
            /* the 'num' is saved and passed to 'trans_add_split' below where
             * 'gnc_set_num_action' is used to set tran-num and/or split-action
             * per book option */
            num = g_strdup ((char*)(prop->value));
            /* the 'num' is also saved and used in 'gnc_csv_parse_to_trans' when
             * it calls 'trans_add_split' after deleting the splits added below
             * when a balance is used by the user */
            trans_line->num = g_strdup ((char*)(prop->value));
            break;

        case GNC_CSV_DEPOSIT: /* Add deposits to the existing amount. */
            if (prop->value != NULL)
            {
                amount = gnc_numeric_add (*((gnc_numeric*)(prop->value)),
                                         amount,
                                         xaccAccountGetCommoditySCU (list->account),
                                         GNC_HOW_RND_ROUND_HALF_UP);
                amount_set = TRUE;
                /* We will use the "Deposit" and "Withdrawal" columns in preference to "Balance". */
                trans_line->balance_set = FALSE;
            }
            break;

        case GNC_CSV_WITHDRAWAL: /* Withdrawals are just negative deposits. */
            if (prop->value != NULL)
            {
                amount = gnc_numeric_add (gnc_numeric_neg(*((gnc_numeric*)(prop->value))),
                                         amount,
                                         xaccAccountGetCommoditySCU (list->account),
                                         GNC_HOW_RND_ROUND_HALF_UP);
                amount_set = TRUE;
                /* We will use the "Deposit" and "Withdrawal" columns in preference to "Balance". */
                trans_line->balance_set = FALSE;
            }
            break;

        case GNC_CSV_BALANCE: /* The balance gets stored in a separate field in trans_line. */
            /* We will use the "Deposit" and "Withdrawal" columns in preference to "Balance". */
            if (!amount_set && prop->value != NULL)
            {
                /* This gets put into the actual transaction at the end of gnc_csv_parse_to_trans. */
                trans_line->balance = *((gnc_numeric*)(prop->value));
                trans_line->balance_set = TRUE;
            }
            break;
        default:
            break;
        }
        list->properties = g_list_next (list->properties);
    }

    /* Add a split with the cumulative amount value. */
    trans_add_split (trans_line->trans, list->account, book, amount, num, memo);

    if (oaccount)
        trans_add_osplit (trans_line->trans, oaccount, book, amount, num, omemo);

    if (num)
        g_free (num);
    if (memo)
        g_free (memo);
    if (omemo)
        g_free (omemo);

    return trans_line;
}

/** Creates a list of transactions from parsed data. Transactions that
 * could be created from rows are placed in transactions;
 * rows that fail are placed in error_lines. (Note: there
 * is no way for this function to "fail," i.e. it only returns 0, so
 * it may be changed to a void function in the future.)
 * @param parse_data Data that is being parsed
 * @param account Account with which transactions are created
 * @param redo_errors TRUE to convert only error data, FALSE for all data
 * @return 0 on success, 1 on failure
 */
int GncCsvParseData::parse_to_trans (Account* account,
                                     gboolean redo_errors)
{
    gboolean hasBalanceColumn;
    guint i, j, max_cols = 0;
    GList *error_lines_iter = NULL, *begin_error_lines = NULL;
    Account *home_account = NULL;

    /* last_transaction points to the last element in
     * transactions, or NULL if it's empty. */
    GList* last_transaction = NULL;

    /* Free error_lines and transactions if they
     * already exist. */
    if (redo_errors) /* If we're redoing errors, we save freeing until the end. */
        begin_error_lines = error_lines_iter = error_lines;
    else
    {
        if (error_lines != NULL)
            g_list_free(error_lines);

        if (transactions != NULL)
            g_list_free (transactions);
    }
    error_lines = NULL;

    if (redo_errors) /* If we're looking only at error data ... */
    {
        if (transactions == NULL)
            last_transaction = NULL;
        else
        {
            /* Move last_transaction to the end. */
            last_transaction = transactions;
            while (g_list_next (last_transaction) != NULL)
            {
                last_transaction = g_list_next (last_transaction);
            }
        }
        /* ... we use only the lines in error_lines_iter. */
        if (error_lines_iter == NULL)
            i = orig_lines->len; /* Don't go into the for loop. */
        else
            i = GPOINTER_TO_INT(error_lines_iter->data);
    }
    else /* Otherwise, we look at all the data. */
    {
        /* The following while-loop effectively behaves like the following for-loop:
         * for(i = 0; i < orig_lines->len; i++). */
        i = start_row;
        last_transaction = NULL;
    }

    /* set end_row to number of lines */
    if (end_row > orig_lines->len)
        end_row = orig_lines->len;

    while (i < end_row)
    {
        GPtrArray* line = (GPtrArray*) orig_lines->pdata[i];
        /* This flag is TRUE if there are any errors in this row. */
        gboolean errors = FALSE;
        gchar* error_message = NULL;
        TransPropertyList* list;
        GncCsvTransLine* trans_line = NULL;

        home_account = account;

        // If account = NULL, we should have an Account column
        if (home_account == NULL)
        {
            for (j = 0; j < line->len; j++)
            {
                /* Look for "Account" columns. */
                if (column_types->data[j] == GNC_CSV_ACCOUNT)
                {
                    home_account = gnc_csv_account_map_search ((gchar*) line->pdata[j]);
                }
            }
        }

        if (home_account == NULL)
        {
            error_message = g_strdup_printf (_("Account column could not be understood."));
            errors = TRUE;
        }
        else
        {
            list = trans_property_list_new (home_account, date_format, currency_format);

            for (j = 0; j < line->len; j++)
            {
                /* We do nothing in "None" or "Account" columns. */
                if ((column_types->data[j] != GNC_CSV_NONE) && (column_types->data[j] != GNC_CSV_ACCOUNT))
                {
                    /* Affect the transaction appropriately. */
                    TransProperty* property = trans_property_new (column_types->data[j], list);
                    gboolean succeeded = trans_property_set (property, (gchar *) line->pdata[j]);

                    /* TODO Maybe move error handling to within TransPropertyList functions? */
                    if (succeeded)
                        trans_property_list_add (property);
                    else
                    {
                        errors = TRUE;
                        error_message = g_strdup_printf (_("%s column could not be understood."),
                                                        _(gnc_csv_column_type_strs[property->type]));
                        trans_property_free (property);
                        break;
                    }
                }
            }

            /* If we had success, add the transaction to transaction. */
            if (!errors)
            {
                trans_line = trans_property_list_to_trans (list, &error_message);
                errors = trans_line == NULL;
            }
            trans_property_list_free (list);
        }

        /* If there were errors, add this line to error_lines. */
        if (errors)
        {
            error_lines = g_list_append (error_lines,
                                         GINT_TO_POINTER(i));
            /* If there's already an error message, we need to replace it. */
            if (line->len > (guint)(orig_row_lengths->data[i]))
            {
                g_free(line->pdata[line->len - 1]);
                line->pdata[line->len - 1] = error_message;
            }
            else
            {
                /* Put the error message at the end of the line. */
                g_ptr_array_add (line, error_message);
            }
        }
        else
        {
            /* If all went well, add this transaction to the list. */
            trans_line->line_no = i;

            /* We keep the transactions sorted by date. We start at the end
             * of the list and go backward, simply because the file itself
             * is probably also sorted by date (but we need to handle the
             * exception anyway). */

            /* If we can just put it at the end, do so and increment last_transaction. */
            if (last_transaction == NULL ||
                    xaccTransGetDate (((GncCsvTransLine*)(last_transaction->data))->trans) <= xaccTransGetDate (trans_line->trans))
            {
                transactions = g_list_append (transactions, trans_line);
                /* If this is the first transaction, we need to get last_transaction on track. */
                if (last_transaction == NULL)
                    last_transaction = transactions;
                else /* Otherwise, we can just continue. */
                    last_transaction = g_list_next (last_transaction);
            }
            /* Otherwise, search backward for the correct spot. */
            else
            {
                GList* insertion_spot = last_transaction;
                while (insertion_spot != NULL &&
                        xaccTransGetDate (((GncCsvTransLine*)(insertion_spot->data))->trans) > xaccTransGetDate (trans_line->trans))
                {
                    insertion_spot = g_list_previous (insertion_spot);
                }
                /* Move insertion_spot one location forward since we have to
                 * use the g_list_insert_before function. */
                if (insertion_spot == NULL) /* We need to handle the case of inserting at the beginning of the list. */
                    insertion_spot = transactions;
                else
                    insertion_spot = g_list_next (insertion_spot);

                transactions = g_list_insert_before (transactions, insertion_spot, trans_line);
            }
        }

        /* Increment to the next row. */
        if (redo_errors)
        {
            /* Move to the next error line in the list. */
            error_lines_iter = g_list_next (error_lines_iter);
            if (error_lines_iter == NULL)
                i = orig_lines->len; /* Don't continue the for loop. */
            else
                i = GPOINTER_TO_INT(error_lines_iter->data);
        }
        else
        {
            if (skip_rows == FALSE)
                i++;
            else
                i = i + 2;
        }
    }

    /* If we have a balance column, set the appropriate amounts on the transactions. */
    hasBalanceColumn = FALSE;
    for (i = 0; i < column_types->len; i++)
    {
        if (column_types->data[i] == GNC_CSV_BALANCE)
        {
            hasBalanceColumn = TRUE;
            break;
        }
    }

    if (hasBalanceColumn) // This is only used if we have one home account
    {
        Split      *split, *osplit;
        gnc_numeric balance_offset;
        GList* tx_iter = transactions;

        if (account != NULL)
            home_account = account;

        /* balance_offset is how much the balance currently in the account
         * differs from what it will be after the transactions are
         * imported. This will be sum of all the previous transactions for
         * any given transaction. */
        balance_offset = double_to_gnc_numeric (0.0, xaccAccountGetCommoditySCU (home_account),
                                     GNC_HOW_RND_ROUND_HALF_UP);
        while (tx_iter != NULL)
        {
            GncCsvTransLine* trans_line = (GncCsvTransLine*)tx_iter->data;
            if (trans_line->balance_set)
            {
                time64 date = xaccTransGetDate (trans_line->trans);
                /* Find what the balance should be by adding the offset to the actual balance. */
                gnc_numeric existing_balance = gnc_numeric_add (balance_offset,
                                               xaccAccountGetBalanceAsOfDate (home_account, date),
                                               xaccAccountGetCommoditySCU (home_account),
                                               GNC_HOW_RND_ROUND_HALF_UP);

                /* The amount of the transaction is the difference between the new and existing balance. */
                gnc_numeric amount = gnc_numeric_sub (trans_line->balance,
                                                     existing_balance,
                                                     xaccAccountGetCommoditySCU (home_account),
                                                     GNC_HOW_RND_ROUND_HALF_UP);

                // Find home account split
                split  = xaccTransFindSplitByAccount (trans_line->trans, home_account);
                xaccSplitSetAmount (split, amount);
                xaccSplitSetValue (split, amount);

                // If we have two splits, change other side
                if (xaccTransCountSplits (trans_line->trans) == 2)
                {
                    osplit = xaccSplitGetOtherSplit (split);
                    xaccSplitSetAmount (split, amount);
                    xaccSplitSetValue (split, gnc_numeric_neg (amount));
                }

                if (trans_line->num)
                    g_free (trans_line->num);

                /* This new transaction needs to be added to the balance offset. */
                balance_offset = gnc_numeric_add (balance_offset,
                                                 amount,
                                                 xaccAccountGetCommoditySCU (home_account),
                                                 GNC_HOW_RND_ROUND_HALF_UP);
            }
            tx_iter = g_list_next (tx_iter);
        }
    }

    if (redo_errors) /* Now that we're at the end, we do the freeing. */
        g_list_free (begin_error_lines);

    /* We need to resize column_types since errors may have added columns. */
    for (i = 0; i < orig_lines->len; i++)
    {
        if (max_cols < ((GPtrArray*)(orig_lines->pdata[i]))->len)
            max_cols = ((GPtrArray*)(orig_lines->pdata[i]))->len;
    }
    i = column_types->len;
    column_types = g_array_set_size (column_types, max_cols);
    for (; i < max_cols; i++)
    {
        column_types->data[i] = GNC_CSV_NONE;
    }
    return 0;
}


bool
GncCsvParseData::check_for_column_type (int type)
{
    gboolean ret = FALSE;
    int j, ncols = column_types->len; /* ncols is the number of columns in the data. */

    for (j = 0; j < ncols; j++)
    {
        if (column_types->data[j] == type)
            ret = TRUE;
    }
    return ret;
}
