#include "gnc-csv-model.h"


#include <glib/gi18n.h>

#include <goffice/goffice-features.h>
#if (GO_VERSION_EPOCH == 0) && (GO_VERSION_MAJOR == 7) && (GO_VERSION_MINOR == 8)
/* For libgoffice-0.7.8, disable its internal inclusion of <regutf8.h>
   so to avoid clashing symbol definitions with <regex.h> */
# define GO_REGUTF8_H
#endif
#include <goffice/utils/go-glib-extras.h>

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

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

const int num_date_formats = 5;
const gchar* date_format_user[] = {N_("y-m-d"),
                                   N_("d-m-y"),
                                   N_("m-d-y"),
                                   N_("d-m"),
                                   N_("m-d")
                                  };

const int num_currency_formats = 3;
const gchar* currency_format_user[] = {N_("Locale"),
                                       N_("Period: 123,456.78"),
                                       N_("Comma: 123.456,78")
                                      };

/* This array contains all of the different strings for different column types. */
gchar* gnc_csv_column_type_strs[GNC_CSV_NUM_COL_TYPES] = {N_("None"),
                                                          N_("Date"),
                                                          N_("Num"),
                                                          N_("Description"),
                                                          N_("Notes"),
                                                          N_("Account"),
                                                          N_("Deposit"),
                                                          N_("Withdrawal"),
                                                          N_("Balance")
                                                         };

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

    /* Put some sane values in retvalue by using the current time for
     * the non-year-month-day parts of the date. */
    gnc_time (&rawtime);
    gnc_localtime_r (&rawtime, &retvalue);

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
             * values so that we can check if the change when we use gnc_mktime
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

    /* Put some sane values in retvalue by using the current time for
     * the non-year-month-day parts of the date. */
    gnc_time (&rawtime);
    gnc_localtime_r (&rawtime, &retvalue);
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
             * values so that we can check if the change when we use gnc_mktime
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
GncCsvParseData* gnc_csv_new_parse_data (void)
{
    GncCsvParseData* parse_data = g_new(GncCsvParseData, 1);
    parse_data->encoding = "UTF-8";
    /* All of the data pointers are initially NULL. This is so that, if
     * gnc_csv_parse_data_free is called before all of the data is
     * initialized, only the data that needs to be freed is freed. */
    parse_data->raw_str.begin = parse_data->raw_str.end
                                = parse_data->file_str.begin = parse_data->file_str.end = NULL;
    parse_data->orig_lines = NULL;
    parse_data->orig_row_lengths = NULL;
    parse_data->column_types = NULL;
    parse_data->error_lines = parse_data->transactions = NULL;
    parse_data->options = default_parse_options();
    parse_data->date_format = -1;
    parse_data->currency_format = 0;
    parse_data->chunk = g_string_chunk_new(100 * 1024);
    parse_data->start_row = 0;
    parse_data->end_row = 1000;
    parse_data->skip_rows = FALSE;
    return parse_data;
}

/** Destructor for GncCsvParseData.
 * @param parse_data Parse data whose memory will be freed
 */
void gnc_csv_parse_data_free (GncCsvParseData* parse_data)
{
    /* All non-NULL pointers have been initialized and must be freed. */

    if (parse_data->raw_mapping != NULL)
    {
        g_mapped_file_unref (parse_data->raw_mapping);
    }

    if (parse_data->file_str.begin != NULL)
        g_free (parse_data->file_str.begin);

    if (parse_data->orig_lines != NULL)
        stf_parse_general_free (parse_data->orig_lines);

    if (parse_data->orig_row_lengths != NULL)
        g_array_free (parse_data->orig_row_lengths, FALSE);

    if (parse_data->options != NULL)
        stf_parse_options_free (parse_data->options);

    if (parse_data->column_types != NULL)
        g_array_free (parse_data->column_types, TRUE);

    if (parse_data->error_lines != NULL)
        g_list_free (parse_data->error_lines);

    if (parse_data->transactions != NULL)
    {
        GList* transactions = parse_data->transactions;
        /* We have to free the GncCsvTransLine's that are at each node in
         * the list before freeing the entire list. */
        do
        {
            g_free (transactions->data);
            transactions = g_list_next (transactions);
        }
        while (transactions != NULL);
        g_list_free (parse_data->transactions);
    }

    g_free (parse_data->chunk);
    g_free (parse_data);
}

/** Converts raw file data using a new encoding. This function must be
 * called after gnc_csv_load_file only if gnc_csv_load_file guessed
 * the wrong encoding.
 * @param parse_data Data that is being parsed
 * @param encoding Encoding that data should be translated using
 * @param error Will point to an error on failure
 * @return 0 on success, 1 on failure
 */
int gnc_csv_convert_encoding (GncCsvParseData* parse_data, const char* encoding,
                             GError** error)
{
    gsize bytes_read, bytes_written;

    /* If parse_data->file_str has already been initialized it must be
     * freed first. (This should always be the case, since
     * gnc_csv_load_file should always be called before this
     * function.) */
    if (parse_data->file_str.begin != NULL)
        g_free(parse_data->file_str.begin);

    /* Do the actual translation to UTF-8. */
    parse_data->file_str.begin = g_convert (parse_data->raw_str.begin,
                                           parse_data->raw_str.end - parse_data->raw_str.begin,
                                           "UTF-8", encoding, &bytes_read, &bytes_written,
                                           error);
    /* Handle errors that occur. */
    if (parse_data->file_str.begin == NULL)
        return 1;

    /* On success, save the ending pointer of the translated data and
     * the encoding type and return 0. */
    parse_data->file_str.end = parse_data->file_str.begin + bytes_written;
    parse_data->encoding = (gchar*)encoding;
    return 0;
}

/** Loads a file into a GncCsvParseData. This is the first function
 * that must be called after createing a new GncCsvParseData. If this
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
int gnc_csv_load_file (GncCsvParseData* parse_data, const char* filename,
                      GError** error)
{
    const char* guess_enc = NULL;

    /* Get the raw data first and handle an error if one occurs. */
    parse_data->raw_mapping = g_mapped_file_new (filename, FALSE, error);
    if (parse_data->raw_mapping == NULL)
    {
        /* TODO Handle file opening errors more specifically,
         * e.g. inexistent file versus no read permission. */
        parse_data->raw_str.begin = NULL;
        g_clear_error (error);
        g_set_error (error, 0, GNC_CSV_FILE_OPEN_ERR, "%s", _("File opening failed."));
        return 1;
    }

    /* Copy the mapping's contents into parse-data->raw_str. */
    parse_data->raw_str.begin = g_mapped_file_get_contents (parse_data->raw_mapping);
    parse_data->raw_str.end = parse_data->raw_str.begin + g_mapped_file_get_length (parse_data->raw_mapping);

    /* Make a guess at the encoding of the data. */
    if (!g_mapped_file_get_length (parse_data->raw_mapping) == 0)
        guess_enc = go_guess_encoding ((const char*)(parse_data->raw_str.begin),
                                      (size_t)(parse_data->raw_str.end - parse_data->raw_str.begin),
                                      "UTF-8", NULL);
    if (guess_enc == NULL)
    {
        g_set_error (error, 0, GNC_CSV_ENCODING_ERR, "%s", _("Unknown encoding."));
        return 1;
    }
    /* Convert using the guessed encoding into parse_data->file_str and
     * handle any errors that occur. */
    gnc_csv_convert_encoding (parse_data, guess_enc, error);
    if (parse_data->file_str.begin == NULL)
    {
        g_set_error (error, 0, GNC_CSV_ENCODING_ERR, "%s", _("Unknown encoding."));
        return 1;
    }
    else
        return 0;
}

/** Parses a file into cells. This requires having an encoding that
 * works (see gnc_csv_convert_encoding). parse_data->options should be
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
int gnc_csv_parse (GncCsvParseData* parse_data, gboolean guessColTypes, GError** error)
{
    /* max_cols is the number of columns in the row with the most columns. */
    int i, max_cols = 0;

    if (parse_data->orig_lines != NULL)
    {
        stf_parse_general_free (parse_data->orig_lines);
    }

    /* If everything is fine ... */
    if (parse_data->file_str.begin != NULL)
    {
        /* Do the actual parsing. */
        parse_data->orig_lines = stf_parse_general (parse_data->options, parse_data->chunk,
                                 parse_data->file_str.begin,
                                 parse_data->file_str.end);
    }
    /* If we couldn't get the encoding right, we just want an empty array. */
    else
    {
        parse_data->orig_lines = g_ptr_array_new();
    }

    /* Record the original row lengths of parse_data->orig_lines. */
    if (parse_data->orig_row_lengths != NULL)
        g_array_free (parse_data->orig_row_lengths, FALSE);

    parse_data->orig_row_lengths =
        g_array_sized_new (FALSE, FALSE, sizeof(int), parse_data->orig_lines->len);

    g_array_set_size (parse_data->orig_row_lengths, parse_data->orig_lines->len);
    parse_data->orig_max_row = 0;
    for (i = 0; i < parse_data->orig_lines->len; i++)
    {
        int length = ((GPtrArray*)parse_data->orig_lines->pdata[i])->len;
        parse_data->orig_row_lengths->data[i] = length;
        if (length > parse_data->orig_max_row)
            parse_data->orig_max_row = length;
    }

    /* If it failed, generate an error. */
    if (parse_data->orig_lines == NULL)
    {
        g_set_error (error, 0, 0, "Parsing failed.");
        return 1;
    }

    /* Now that we have data, let's set max_cols. */
    for (i = 0; i < parse_data->orig_lines->len; i++)
    {
        if (max_cols < ((GPtrArray*)(parse_data->orig_lines->pdata[i]))->len)
            max_cols = ((GPtrArray*)(parse_data->orig_lines->pdata[i]))->len;
    }

    if (guessColTypes)
    {
        /* Free parse_data->column_types if it's already been created. */
        if (parse_data->column_types != NULL)
            g_array_free (parse_data->column_types, TRUE);

        /* Create parse_data->column_types and fill it with guesses based
         * on the contents of each column. */
        parse_data->column_types = g_array_sized_new (FALSE, FALSE, sizeof(int),
                                   max_cols);
        g_array_set_size (parse_data->column_types, max_cols);
        /* TODO Make it actually guess. */
        for (i = 0; i < parse_data->column_types->len; i++)
        {
            parse_data->column_types->data[i] = GNC_CSV_NONE;
        }
    }
    else
    {
        /* If we don't need to guess column types, we will simply set any
         * new columns that are created that didn't exist before to "None"
         * since we don't want gibberish to appear. Note:
         * parse_data->column_types should have already been
         * initialized, so we don't check for it being NULL. */
        int i = parse_data->column_types->len;
        g_array_set_size (parse_data->column_types, max_cols);
        for (; i < parse_data->column_types->len; i++)
        {
            parse_data->column_types->data[i] = GNC_CSV_NONE;
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
    int type; /**< A value from the GncCsvColumnType enum except
             * GNC_CSV_NONE and GNC_CSV_NUM_COL_TYPES */
    void* value; /**< Pointer to the data that will be used to configure a transaction */
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
    case GNC_CSV_NUM:
        prop->value = g_strdup (str);
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
            /* Currancy locale */
            if (!(xaccParseAmount (str_dupe, TRUE, &val, &endptr)))
            {
                g_free (str_dupe);
                return FALSE;
            }
            break;
        case 1:
            /* Currancy decimal period */
            if (!(xaccParseAmountExtended (str_dupe, TRUE, '-', '.', ',', "\003\003", "$+", &val, &endptr)))
            {
                g_free (str_dupe);
                return FALSE;
            }
            break;
        case 2:
            /* Currancy decimal comma */
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
    /* Free all of the properties in this list before freeeing the list itself. */
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
                            gnc_numeric amount, const char *num)
{
    Split* split = xaccMallocSplit (book);
    xaccSplitSetAccount (split, account);
    xaccSplitSetParent (split, trans);
    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, amount);
    /* set tran-num and/or split-action per book option */
    gnc_set_num_action (trans, split, num, NULL);
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
    gchar* possible_errors[NUM_OF_POSSIBLE_ERRORS] =
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
        }
        list->properties = g_list_next (list->properties);
    }

    /* Add a split with the cumulative amount value. */
    trans_add_split (trans_line->trans, list->account, book, amount, num);
    if (num)
        g_free (num);

    return trans_line;
}

/** Creates a list of transactions from parsed data. Transactions that
 * could be created from rows are placed in parse_data->transactions;
 * rows that fail are placed in parse_data->error_lines. (Note: there
 * is no way for this function to "fail," i.e. it only returns 0, so
 * it may be changed to a void function in the future.)
 * @param parse_data Data that is being parsed
 * @param account Account with which transactions are created
 * @param redo_errors TRUE to convert only error data, FALSE for all data
 * @return 0 on success, 1 on failure
 */
int gnc_csv_parse_to_trans (GncCsvParseData* parse_data, Account* account,
                           gboolean redo_errors)
{
    gboolean hasBalanceColumn;
    int i, j, max_cols = 0;
    GArray* column_types = parse_data->column_types;
    GList *error_lines = NULL, *begin_error_lines = NULL;

    /* last_transaction points to the last element in
     * parse_data->transactions, or NULL if it's empty. */
    GList* last_transaction = NULL;

    /* Free parse_data->error_lines and parse_data->transactions if they
     * already exist. */
    if (redo_errors) /* If we're redoing errors, we save freeing until the end. */
    {
        begin_error_lines = error_lines = parse_data->error_lines;
    }
    else
    {
        if (parse_data->error_lines != NULL)
        {
            g_list_free(parse_data->error_lines);
        }
        if (parse_data->transactions != NULL)
        {
            g_list_free (parse_data->transactions);
        }
    }
    parse_data->error_lines = NULL;

    if (redo_errors) /* If we're looking only at error data ... */
    {
        if (parse_data->transactions == NULL)
        {
            last_transaction = NULL;
        }
        else
        {
            /* Move last_transaction to the end. */
            last_transaction = parse_data->transactions;
            while (g_list_next (last_transaction) != NULL)
            {
                last_transaction = g_list_next (last_transaction);
            }
        }
        /* ... we use only the lines in error_lines. */
        if (error_lines == NULL)
            i = parse_data->orig_lines->len; /* Don't go into the for loop. */
        else
            i = GPOINTER_TO_INT(error_lines->data);
    }
    else /* Otherwise, we look at all the data. */
    {
        /* The following while-loop effectively behaves like the following for-loop:
         * for(i = 0; i < parse_data->orig_lines->len; i++). */
        i = parse_data->start_row;
        last_transaction = NULL;
    }

    /* set parse_data->end_row to number of lines */
    if (parse_data->end_row > parse_data->orig_lines->len)
        parse_data->end_row = parse_data->orig_lines->len;

    while (i < parse_data->end_row)
    {
        GPtrArray* line = parse_data->orig_lines->pdata[i];
        /* This flag is TRUE if there are any errors in this row. */
        gboolean errors = FALSE;
        gchar* error_message = NULL;
        TransPropertyList* list = trans_property_list_new (account, parse_data->date_format, parse_data->currency_format);
        GncCsvTransLine* trans_line = NULL;

        for (j = 0; j < line->len; j++)
        {
            /* We do nothing in "None" or "Account" columns. */
            if ((column_types->data[j] != GNC_CSV_NONE) && (column_types->data[j] != GNC_CSV_ACCOUNT))
            {
                /* Affect the transaction appropriately. */
                TransProperty* property = trans_property_new (column_types->data[j], list);
                gboolean succeeded = trans_property_set (property, line->pdata[j]);

                /* TODO Maybe move error handling to within TransPropertyList functions? */
                if (succeeded)
                {
                    trans_property_list_add (property);
                }
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

        /* If we had success, add the transaction to parse_data->transaction. */
        if (!errors)
        {
            trans_line = trans_property_list_to_trans (list, &error_message);
            errors = trans_line == NULL;
        }

        trans_property_list_free (list);

        /* If there were errors, add this line to parse_data->error_lines. */
        if (errors)
        {
            parse_data->error_lines = g_list_append (parse_data->error_lines,
                                                    GINT_TO_POINTER(i));
            /* If there's already an error message, we need to replace it. */
            if (line->len > (int)(parse_data->orig_row_lengths->data[i]))
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
                parse_data->transactions = g_list_append (parse_data->transactions, trans_line);
                /* If this is the first transaction, we need to get last_transaction on track. */
                if (last_transaction == NULL)
                    last_transaction = parse_data->transactions;
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
                    insertion_spot = parse_data->transactions;
                else
                    insertion_spot = g_list_next (insertion_spot);

                parse_data->transactions = g_list_insert_before (parse_data->transactions, insertion_spot, trans_line);
            }
        }

        /* Increment to the next row. */
        if (redo_errors)
        {
            /* Move to the next error line in the list. */
            error_lines = g_list_next (error_lines);
            if (error_lines == NULL)
                i = parse_data->orig_lines->len; /* Don't continue the for loop. */
            else
                i = GPOINTER_TO_INT(error_lines->data);
        }
        else
        {
            if (parse_data->skip_rows == FALSE)
                i++;
            else
                i = i + 2;
        }
    }

    /* If we have a balance column, set the appropriate amounts on the transactions. */
    hasBalanceColumn = FALSE;
    for (i = 0; i < parse_data->column_types->len; i++)
    {
        if (parse_data->column_types->data[i] == GNC_CSV_BALANCE)
        {
            hasBalanceColumn = TRUE;
            break;
        }
    }

    if (hasBalanceColumn)
    {
        GList* transactions = parse_data->transactions;

        /* balance_offset is how much the balance currently in the account
         * differs from what it will be after the transactions are
         * imported. This will be sum of all the previous transactions for
         * any given transaction. */
        gnc_numeric balance_offset = double_to_gnc_numeric (0.0,
                                     xaccAccountGetCommoditySCU (account),
                                     GNC_HOW_RND_ROUND_HALF_UP);
        while (transactions != NULL)
        {
            GncCsvTransLine* trans_line = (GncCsvTransLine*)transactions->data;
            if (trans_line->balance_set)
            {
                time64 date = xaccTransGetDate (trans_line->trans);
                /* Find what the balance should be by adding the offset to the actual balance. */
                gnc_numeric existing_balance = gnc_numeric_add (balance_offset,
                                               xaccAccountGetBalanceAsOfDate (account, date),
                                               xaccAccountGetCommoditySCU (account),
                                               GNC_HOW_RND_ROUND_HALF_UP);

                /* The amount of the transaction is the difference between the new and existing balance. */
                gnc_numeric amount = gnc_numeric_sub (trans_line->balance,
                                                     existing_balance,
                                                     xaccAccountGetCommoditySCU (account),
                                                     GNC_HOW_RND_ROUND_HALF_UP);

                SplitList* splits = xaccTransGetSplitList (trans_line->trans);
                while (splits)
                {
                    SplitList* next_splits = g_list_next (splits);
                    xaccSplitDestroy ((Split*)splits->data);
                    splits = next_splits;
                }

                trans_add_split (trans_line->trans, account,
                                gnc_account_get_book (account), amount, trans_line->num);
                if (trans_line->num)
                    g_free (trans_line->num);

                /* This new transaction needs to be added to the balance offset. */
                balance_offset = gnc_numeric_add (balance_offset,
                                                 amount,
                                                 xaccAccountGetCommoditySCU (account),
                                                 GNC_HOW_RND_ROUND_HALF_UP);
            }
            transactions = g_list_next (transactions);
        }
    }

    if (redo_errors) /* Now that we're at the end, we do the freeing. */
    {
        g_list_free (begin_error_lines);
    }

    /* We need to resize parse_data->column_types since errors may have added columns. */
    for (i = 0; i < parse_data->orig_lines->len; i++)
    {
        if (max_cols < ((GPtrArray*)(parse_data->orig_lines->pdata[i]))->len)
            max_cols = ((GPtrArray*)(parse_data->orig_lines->pdata[i]))->len;
    }
    i = parse_data->column_types->len;
    parse_data->column_types = g_array_set_size (parse_data->column_types, max_cols);
    for (; i < max_cols; i++)
    {
        parse_data->column_types->data[i] = GNC_CSV_NONE;
    }

    return 0;
}
