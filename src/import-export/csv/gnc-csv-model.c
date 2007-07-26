#include "gnc-csv-model.h"

#include "gnc-book.h"

#include <glib/gi18n.h>
#include <goffice/utils/go-glib-extras.h>

#include <string.h>
#include <sys/time.h>

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <regex.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>

static QofLogModule log_module = GNC_MOD_IMPORT;

const int num_date_formats = 5;

const gchar* date_format_user[] = {N_("y-m-d"),
                                   N_("d-m-y"),
                                   N_("m-d-y"),
                                   N_("d-m"),
                                   N_("m-d")};

/** A set of sensible defaults for parsing CSV files. 
 * @return StfParseOptions_t* for parsing a file with comma separators
 */
static StfParseOptions_t* default_parse_options(void)
{
  StfParseOptions_t* options = stf_parse_options_new();
  stf_parse_options_set_type(options, PARSE_TYPE_CSV);
  stf_parse_options_csv_set_separators(options, ",", NULL);
  return options;
}

/* TODO Comment */
static time_t parse_date_with_year(const char* date_str, int format)
{
  time_t rawtime; /* The integer time */
  struct tm retvalue, test_retvalue; /* The time in a broken-down structure */
  
  int i, j, mem_length, orig_year, orig_month, orig_day;

  /* Buffer for containing individual parts (e.g. year, month, day) of a date */
  char date_segment[5];

  /* The compiled regular expression */
  regex_t preg = {0};

  /* An array containing indices specifying the matched substrings in date_str */
  regmatch_t pmatch[4] = { {0}, {0}, {0}, {0} };

  /* The regular expression for parsing dates */
  const char* regex = "^ *([0-9]+) *[-/.'] *([0-9]+) *[-/.'] *([0-9]+).*$|^ *([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]).*$";

  /* We get our matches using the regular expression. */
  regcomp(&preg, regex, REG_EXTENDED);
  regexec(&preg, date_str, 4, pmatch, 0);
  regfree(&preg);

  /* If there wasn't a match, there was an error. */
  if(pmatch[0].rm_eo == 0)
    return -1;

  /* If this is a string without separators ... */
  if(pmatch[1].rm_so == -1)
  {
    /* ... we will fill in the indices based on the user's selection. */
    int k = 0; /* k traverses date_str by keeping track of where separators "should" be. */
    j = 1; /* j traverses pmatch. */
    for(i = 0; date_format_user[format][i]; i++)
    {
      char segment_type = date_format_user[format][i];
      /* Only do something if this is a meaningful character */
      if(segment_type == 'y' || segment_type == 'm' || segment_type == 'd')
      {
        pmatch[j].rm_so = k;
        switch(segment_type)
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
  time(&rawtime);
  localtime_r(&rawtime, &retvalue);

  /* j traverses pmatch (index 0 contains the entire string, so we
   * start at index 1 for the first meaningful match). */
  j = 1;
  /* Go through the date format and interpret the matches in order of
   * the sections in the date format. */
  for(i = 0; date_format_user[format][i]; i++)
  {
    char segment_type = date_format_user[format][i];
    /* Only do something if this is a meaningful character */
    if(segment_type == 'y' || segment_type == 'm' || segment_type == 'd')
    {
      /* Copy the matching substring into date_segment so that we can
       * convert it into an integer. */
      mem_length = pmatch[j].rm_eo - pmatch[j].rm_so;
      memcpy(date_segment, date_str + pmatch[j].rm_so, mem_length);
      date_segment[mem_length] = '\0';

      /* Set the appropriate member of retvalue. Save the original
       * values so that we can check if the change when we use mktime
       * below. */
      switch(segment_type)
      {
      case 'y':
        retvalue.tm_year = atoi(date_segment);

        /* Handle two-digit years. */
        if(retvalue.tm_year < 100)
        {
          /* We allow two-digit years in the range 1969 - 2068. */
          if(retvalue.tm_year < 69)
            retvalue.tm_year += 100;
        }
        else
          retvalue.tm_year -= 1900;
        orig_year = retvalue.tm_year;
        break;
        
      case 'm':
        orig_month = retvalue.tm_mon = atoi(date_segment) - 1;
        break;
        
      case 'd':
        orig_day = retvalue.tm_mday = atoi(date_segment);
        break;
      }
      j++;
    }
  }
  /* Convert back to an integer. If mktime leaves retvalue unchanged,
   * everything is okay; otherwise, an error has occurred. */
  /* We have to use a "test" date value to account for changes in
   * daylight savings time, which can cause a date change with mktime
   * near midnight, causing the code to incorrectly think a date is
   * incorrect. */
  test_retvalue = retvalue;
  mktime(&test_retvalue);
  retvalue.tm_isdst = test_retvalue.tm_isdst;
  rawtime = mktime(&retvalue);
  if(retvalue.tm_mday == orig_day &&
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

/* TODO Comment */
static time_t parse_date_without_year(const char* date_str, int format)
{
  time_t rawtime; /* The integer time */
  struct tm retvalue, test_retvalue; /* The time in a broken-down structure */
  
  int i, j, mem_length, orig_year, orig_month, orig_day;

  /* Buffer for containing individual parts (e.g. year, month, day) of a date */
  gchar* date_segment;

  /* The compiled regular expression */
  regex_t preg = {0};

  /* An array containing indices specifying the matched substrings in date_str */
  regmatch_t pmatch[3] = { {0}, {0}, {0} };

  /* The regular expression for parsing dates */
  const char* regex = "^ *([0-9]+) *[-/.'] *([0-9]+).*$";

  /* We get our matches using the regular expression. */
  regcomp(&preg, regex, REG_EXTENDED);
  regexec(&preg, date_str, 3, pmatch, 0);
  regfree(&preg);

  /* If there wasn't a match, there was an error. */
  if(pmatch[0].rm_eo == 0)
    return -1;

  /* Put some sane values in retvalue by using the current time for
   * the non-year-month-day parts of the date. */
  time(&rawtime);
  localtime_r(&rawtime, &retvalue);
  orig_year = retvalue.tm_year;

  /* j traverses pmatch (index 0 contains the entire string, so we
   * start at index 1 for the first meaningful match). */
  j = 1;
  /* Go through the date format and interpret the matches in order of
   * the sections in the date format. */
  for(i = 0; date_format_user[format][i]; i++)
  {
    char segment_type = date_format_user[format][i];
    /* Only do something if this is a meaningful character */
    if(segment_type == 'm' || segment_type == 'd')
    {
      /* Copy the matching substring into date_segment so that we can
       * convert it into an integer. */
      mem_length = pmatch[j].rm_eo - pmatch[j].rm_so;
      date_segment = g_new(gchar, mem_length);
      memcpy(date_segment, date_str + pmatch[j].rm_so, mem_length);
      date_segment[mem_length] = '\0';

      /* Set the appropriate member of retvalue. Save the original
       * values so that we can check if the change when we use mktime
       * below. */
      switch(segment_type)
      {
      case 'm':
        orig_month = retvalue.tm_mon = atoi(date_segment) - 1;
        break;
        
      case 'd':
        orig_day = retvalue.tm_mday = atoi(date_segment);
        break;
      }
      g_free(date_segment);
      j++;
    }
  }
  /* Convert back to an integer. If mktime leaves retvalue unchanged,
   * everything is okay; otherwise, an error has occurred. */
  /* We have to use a "test" date value to account for changes in
   * daylight savings time, which can cause a date change with mktime
   * near midnight, causing the code to incorrectly think a date is
   * incorrect. */
  test_retvalue = retvalue;
  mktime(&test_retvalue);
  retvalue.tm_isdst = test_retvalue.tm_isdst;
  rawtime = mktime(&retvalue);
  if(retvalue.tm_mday == orig_day &&
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
static time_t parse_date(const char* date_str, int format)
{
  if(strchr(date_format_user[format], 'y'))
    return parse_date_with_year(date_str, format);
  else
    return parse_date_without_year(date_str, format);
}

/** Constructor for GncCsvParseData.
 * @return Pointer to a new GncCSvParseData
 */
GncCsvParseData* gnc_csv_new_parse_data(void)
{
  GncCsvParseData* parse_data = g_new(GncCsvParseData, 1);
  parse_data->encoding = "UTF-8";
  /* All of the data pointers are initially NULL. This is so that, if
   * gnc_csv_parse_data_free is called before all of the data is
   * initialized, only the data that needs to be freed is freed. */
  parse_data->raw_str.begin = parse_data->raw_str.end
    = parse_data->file_str.begin = parse_data->file_str.end = NULL;
  parse_data->orig_lines = NULL;
  parse_data->column_types = NULL;
  parse_data->error_lines = parse_data->transactions = NULL;
  parse_data->options = default_parse_options();
  parse_data->date_format = -1;
  parse_data->chunk = g_string_chunk_new(100 * 1024);
  return parse_data;
}

/** Destructor for GncCsvParseData.
 * @param parse_data Parse data whose memory will be freed
 */
void gnc_csv_parse_data_free(GncCsvParseData* parse_data)
{
  /* All non-NULL pointers have been initialized and must be freed. */

  if(parse_data->raw_mapping != NULL)
    g_mapped_file_free(parse_data->raw_mapping);

  if(parse_data->file_str.begin != NULL)
    g_free(parse_data->file_str.begin);

  if(parse_data->orig_lines != NULL)
    g_ptr_array_free(parse_data->orig_lines, TRUE);

  if(parse_data->options != NULL)
    stf_parse_options_free(parse_data->options);

  if(parse_data->column_types != NULL)
    g_array_free(parse_data->column_types, TRUE);

  if(parse_data->error_lines != NULL)
    g_list_free(parse_data->error_lines);

  if(parse_data->transactions != NULL)
  {
    GList* transactions = parse_data->transactions;
    /* We have to free the GncCsvTransLine's that are at each node in
     * the list before freeing the entire list. */
    do
    {
      g_free(transactions->data);
      transactions = g_list_next(transactions);
    } while(transactions != NULL);
    g_list_free(parse_data->transactions);
  }

  g_free(parse_data->chunk);
  g_free(parse_data);
}

/** Converts raw file data using a new encoding. This function must be
 * called after gnc_csv_load_file only if gnc_csv_load_file guessed
 * the wrong encoding.
 * @param parse_data Data that is being parsed
 * @param encoding Encoding that data should be translated using
 * @param error Will point to an error on failure
 * @return 0 on success, 1 on failure
 */
int gnc_csv_convert_encoding(GncCsvParseData* parse_data, const char* encoding,
                             GError** error)
{
  gsize bytes_read, bytes_written;

  /* If parse_data->file_str has already been initialized it must be
   * freed first. (This should always be the case, since
   * gnc_csv_load_file should always be called before this
   * function.) */
  if(parse_data->file_str.begin != NULL)
    g_free(parse_data->file_str.begin);

  /* Do the actual translation to UTF-8. */
  parse_data->file_str.begin = g_convert(parse_data->raw_str.begin,
                                         parse_data->raw_str.end - parse_data->raw_str.begin,
                                         "UTF-8", encoding, &bytes_read, &bytes_written,
                                         error);
  /* Handle errors that occur. */
  if(parse_data->file_str.begin == NULL)
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
int gnc_csv_load_file(GncCsvParseData* parse_data, const char* filename,
                      GError** error)
{
  const char* guess_enc;

  /* Get the raw data first and handle an error if one occurs. */
  parse_data->raw_mapping = g_mapped_file_new(filename, FALSE, error);
  if(parse_data->raw_mapping == NULL)
  {
    /* TODO Handle file opening errors more specifically,
     * e.g. inexistent file versus no read permission. */
    parse_data->raw_str.begin = NULL;
    g_set_error(error, 0, GNC_CSV_FILE_OPEN_ERR, "File opening failed.");
    return 1;
  }

  /* Copy the mapping's contents into parse-data->raw_str. */
  parse_data->raw_str.begin = g_mapped_file_get_contents(parse_data->raw_mapping);
  parse_data->raw_str.end = parse_data->raw_str.begin + g_mapped_file_get_length(parse_data->raw_mapping);

  /* Make a guess at the encoding of the data. */
  guess_enc = go_guess_encoding((const char*)(parse_data->raw_str.begin),
                                (size_t)(parse_data->raw_str.end - parse_data->raw_str.begin),
                                "UTF-8", NULL);
  if(guess_enc == NULL)
  {
    g_set_error(error, 0, GNC_CSV_ENCODING_ERR, "Unknown encoding.");
    return 1;
  }

  /* Convert using the guessed encoding into parse_data->file_str and
   * handle any errors that occur. */
  gnc_csv_convert_encoding(parse_data, guess_enc, error);
  if(parse_data->file_str.begin == NULL)
  {
    g_set_error(error, 0, GNC_CSV_ENCODING_ERR, "Unknown encoding.");
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
 * @error error Will contain an error if there is a failure
 * @return 0 on success, 1 on failure
 */
/* TODO Should we use 0 for domain and code in errors? */
int gnc_csv_parse(GncCsvParseData* parse_data, gboolean guessColTypes, GError** error)
{
  /* max_cols is the number of columns in the row with the most columns. */
  int i, max_cols = 0;

  /* If everything is fine ... */
  /* TODO Check about freeing parse_data->orig_lines ... */
  if(parse_data->file_str.begin != NULL)
  {
    /* Do the actual parsing. */
    parse_data->orig_lines = stf_parse_general(parse_data->options, parse_data->chunk,
                                               parse_data->file_str.begin,
                                               parse_data->file_str.end);
  }
  /* If we couldn't get the encoding right, we just want an empty array. */
  else
  {
    parse_data->orig_lines = g_ptr_array_new();
  }

  /* If it failed, generate an error. */
  if(parse_data->orig_lines == NULL)
  {
    g_set_error(error, 0, 0, "Parsing failed.");
    return 1;
  }

  /* Now that we have data, let's set max_cols. */
  for(i = 0; i < parse_data->orig_lines->len; i++)
  {
    if(max_cols < ((GPtrArray*)(parse_data->orig_lines->pdata[i]))->len)
      max_cols = ((GPtrArray*)(parse_data->orig_lines->pdata[i]))->len;
  }

  if(guessColTypes)
  {
    /* Free parse_data->column_types if it's already been created. */
    if(parse_data->column_types != NULL)
      g_array_free(parse_data->column_types, TRUE);

    /* Create parse_data->column_types and fill it with guesses based
     * on the contents of each column. */
    parse_data->column_types = g_array_sized_new(FALSE, FALSE, sizeof(int),
                                                 max_cols);
    g_array_set_size(parse_data->column_types, max_cols);
    /* TODO Make it actually guess. */
    for(i = 0; i < parse_data->column_types->len; i++)
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
    g_array_set_size(parse_data->column_types, max_cols);
    for(; i < parse_data->column_types->len; i++)
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
  Account* account; /**< The account the transaction belongs to */
  GList* properties; /**< List of TransProperties */
} TransPropertyList;

/** A struct encapsulating a property of a transaction. */
typedef struct
{
  gboolean essential; /**< TRUE if every transaction needs this property */
  int type; /**< A value from the GncCsvColumnType enum except
             * GNC_CSV_NONE and GNC_CSV_NUM_COL_TYPES */
  void* value; /**< Pointer to the data that will be used to configure a transaction */
  TransPropertyList* set; /**< The set the property belongs to */
} TransProperty;

/** Constructor TransProperty.
 * @param type The type of the new property (see TransProperty.type for possible values)
 */
static TransProperty* trans_property_new(int type, TransPropertyList* set)
{
  TransProperty* prop = g_new(TransProperty, 1);
  prop->type = type;
  prop->set = set;
  prop->value = NULL;
  
  switch(type)
  {
    /* Only the "Date" and "Amount" properties are essential. */
  case GNC_CSV_DATE:
  case GNC_CSV_AMOUNT:
    prop->essential = TRUE;
    break;

  default:
    prop->essential = FALSE;
  }
  return prop;
}

static void trans_property_free(TransProperty* prop)
{
  switch(prop->type)
  {
    /* The types for "Date" and "Amount" (time_t and gnc_numeric,
     * respectively) are typically not pointed to, we have to free
     * them, unlike types like char* ("Description"). */
  case GNC_CSV_DATE:
  case GNC_CSV_AMOUNT:
  case GNC_CSV_DEPOSIT:
  case GNC_CSV_WITHDRAWAL:
    if(prop->value != NULL)
      g_free(prop->value);
    break;
  }
  g_free(prop);
}

/** Sets the value of the property by parsing str. Note: this should
 * only be called once on an instance of TransProperty, as calling it
 * more than once can cause memory leaks.
 * @param prop The property being set
 * @param str The string to be parsed
 * @return TRUE on success, FALSE on failure
 */
static gboolean trans_property_set(TransProperty* prop, char* str)
{
  char *endptr, *possible_currency_symbol, *str_dupe;
  double value;
  switch(prop->type)
  {
  case GNC_CSV_DATE:
    prop->value = g_new(time_t, 1);
    *((time_t*)(prop->value)) = parse_date(str, prop->set->date_format);
    return *((time_t*)(prop->value)) != -1;

  case GNC_CSV_DESCRIPTION:
    prop->value = g_strdup(str);
    return TRUE;

  case GNC_CSV_AMOUNT:
  case GNC_CSV_DEPOSIT:
  case GNC_CSV_WITHDRAWAL:
    str_dupe = g_strdup(str); /* First, we make a copy so we can't mess up real data. */

    /* Go through str_dupe looking for currency symbols. */
    for(possible_currency_symbol = str_dupe; *possible_currency_symbol;
        possible_currency_symbol = g_utf8_next_char(possible_currency_symbol))
    {
      if(g_unichar_type(g_utf8_get_char(possible_currency_symbol)) == G_UNICODE_CURRENCY_SYMBOL)
      {
        /* If we find a currency symbol, save the position just ahead
         * of the currency symbol (next_symbol), and find the null
         * terminator of the string (last_symbol). */
        char *next_symbol = g_utf8_next_char(possible_currency_symbol), *last_symbol = next_symbol;
        while(*last_symbol)
          last_symbol = g_utf8_next_char(last_symbol);

        /* Move all of the string (including the null byte, which is
         * why we have +1 in the size parameter) following the
         * currency symbol back one character, thereby overwriting the
         * currency symbol. */
        memmove(possible_currency_symbol, next_symbol, last_symbol - next_symbol + 1);
        break;
      }
    }

    /* Translate the string (now clean of currency symbols) into a number. */
    value = strtod(str_dupe, &endptr);

    /* If this isn't a valid numeric string, this is an error. */
    if(endptr != str_dupe + strlen(str_dupe))
    {
      g_free(str_dupe);
      return FALSE;
    }

    g_free(str_dupe);

    if(abs(value) > 0.00001)
    {
      prop->value = g_new(gnc_numeric, 1);
      *((gnc_numeric*)(prop->value)) = double_to_gnc_numeric(value, xaccAccountGetCommoditySCU(prop->set->account),
                                                             GNC_RND_ROUND);
    }
    return TRUE;
  }
  return FALSE; /* We should never actually get here. */
}

/* TODO Comment */
static TransPropertyList* trans_property_list_new(Account* account, int date_format)
{
  TransPropertyList* set = g_new(TransPropertyList, 1);
  set->account = account;
  set->date_format = date_format;
  set->properties = NULL;
  return set;
}

/* TODO Comment */
static void trans_property_list_free(TransPropertyList* set)
{
  GList* properties_begin = set->properties;
  while(set->properties != NULL)
  {
    trans_property_free((TransProperty*)(set->properties->data));
    set->properties = g_list_next(set->properties);
  }
  g_list_free(properties_begin);
  g_free(set);
}

/* TODO Comment */
static void trans_property_list_add(TransProperty* property)
{
  property->set->properties = g_list_append(property->set->properties, property);
}

/* TODO Comment */
static void trans_add_split(Transaction* trans, Account* account, GNCBook* book,
                            gnc_numeric amount)
{
  Split* split = xaccMallocSplit(book);
  xaccSplitSetAccount(split, account);
  xaccSplitSetParent(split, trans);
  xaccSplitSetAmount(split, amount);
  xaccSplitSetValue(split, amount);
  xaccSplitSetAction(split, "Deposit");
}

/* TODO Comment */
static Transaction* trans_property_list_to_trans(TransPropertyList* set)
{
  Transaction* trans;
  GList* properties_begin = set->properties;
  GNCBook* book = gnc_account_get_book(set->account);
  gnc_commodity* currency = xaccAccountGetCommodity(set->account);
  gnc_numeric amount;
  gboolean alreadyHaveDepositOrWithdrawal = FALSE, noAmountYet = TRUE;
  
  unsigned int essential_properties_left = 2;
  while(set->properties != NULL)
  {
    if(((TransProperty*)(set->properties->data))->essential)
      essential_properties_left--;
    else if(((TransProperty*)(set->properties->data))->type == GNC_CSV_DEPOSIT ||
            ((TransProperty*)(set->properties->data))->type == GNC_CSV_WITHDRAWAL)
    {
      if(alreadyHaveDepositOrWithdrawal)
        essential_properties_left--;
      else
        alreadyHaveDepositOrWithdrawal = TRUE;
    }
    set->properties = g_list_next(set->properties);
  }
  if(essential_properties_left)
    return NULL;

  trans = xaccMallocTransaction(book);
  
  xaccTransBeginEdit(trans);
  xaccTransSetCurrency(trans, currency);

  set->properties = properties_begin;
  while(set->properties != NULL)
  {
    TransProperty* prop = (TransProperty*)(set->properties->data);
    switch(prop->type)
    {
    case GNC_CSV_DATE:
      xaccTransSetDatePostedSecs(trans, *((time_t*)(prop->value)));
      break;

    case GNC_CSV_DESCRIPTION:
      xaccTransSetDescription(trans, (char*)(prop->value));
      break;

    case GNC_CSV_DEPOSIT:
    case GNC_CSV_WITHDRAWAL:
    case GNC_CSV_AMOUNT:
      if(noAmountYet && prop->value != NULL)
      {
        /* Withdrawals are just negative deposits. */
        if(prop->type == GNC_CSV_WITHDRAWAL)
          amount = gnc_numeric_neg(*((gnc_numeric*)(prop->value)));
        else
          amount = *((gnc_numeric*)(prop->value));

        trans_add_split(trans, set->account, book, amount);
        
        noAmountYet = FALSE;
      }
    }
    set->properties = g_list_next(set->properties);
  }

  if(noAmountYet)
  {
    amount = double_to_gnc_numeric(0.0, xaccAccountGetCommoditySCU(set->account), GNC_RND_ROUND);
    trans_add_split(trans, set->account, book, amount);
  }

  return trans;
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
int gnc_parse_to_trans(GncCsvParseData* parse_data, Account* account,
                       gboolean redo_errors)
{
  int i, j;
  GArray* column_types = parse_data->column_types;
  GList *error_lines = NULL, *begin_error_lines = NULL;
  GList* last_transaction = NULL;

  /* Free parse_data->error_lines and parse_data->transactions if they
   * already exist. */
  if(redo_errors) /* If we're redoing errors, we save freeing until the end. */
  {
    begin_error_lines = error_lines = parse_data->error_lines;
  }
  else if(parse_data->error_lines != NULL)
  {
    g_list_free(parse_data->error_lines);
  }
  parse_data->error_lines = NULL;

  if(redo_errors) /* If we're looking only at error data ... */
  {
    last_transaction = parse_data->transactions;
    /* ... we use only the lines in error_lines. */
    if(error_lines == NULL)
      i = parse_data->orig_lines->len; /* Don't go into the for loop. */
    else
      i = GPOINTER_TO_INT(error_lines->data);
  }
  else /* Otherwise, we look at all the data. */
  {
    /* The following while-loop effectively behaves like the following for-loop:
     * for(i = 0; i < parse_data->orig_lines->len; i++). */
    i = 0;
  }
  while(i < parse_data->orig_lines->len)
  {
    GPtrArray* line = parse_data->orig_lines->pdata[i];
    /* This flag is TRUE if there are any errors in this row. */
    gboolean errors = FALSE;
    TransPropertyList* set = trans_property_list_new(account, parse_data->date_format);
    Transaction* trans = NULL;

    /* TODO There should eventually be a specification of what errors
     * actually occurred, not just that one happened. */

    for(j = 0; j < line->len; j++)
    {
      /* We do nothing in "None" columns. */
      if(column_types->data[j] != GNC_CSV_NONE)
      {
        /* Affect the transaction appropriately. */
        TransProperty* property = trans_property_new(column_types->data[j], set);
        gboolean succeeded = trans_property_set(property, line->pdata[j]);
        /* TODO Maybe move error handling to within TransPropertyList functions? */
        if(succeeded)
        {
          trans_property_list_add(property);
        }
        else
        {
          errors = TRUE;
          trans_property_free(property);
          break;
        }
      }
    }

    /* If we had success, add the transaction to parse_data->transaction. */
    if(!errors)
    {
      trans = trans_property_list_to_trans(set);
      errors = trans == NULL;
    }

    trans_property_list_free(set);
      

    /* If there was no success, add this line to parse_data->error_lines. */
    if(errors)
    {
      parse_data->error_lines = g_list_append(parse_data->error_lines,
                                              GINT_TO_POINTER(i));
    }
    else
    {
      GncCsvTransLine* trans_line = g_new(GncCsvTransLine, 1);

      trans_line->trans = trans;
      trans_line->line_no = i;

      if(redo_errors)
      {
        while(last_transaction != NULL &&
              ((GncCsvTransLine*)(last_transaction->data))->line_no < i)
        {
          last_transaction = g_list_next(last_transaction);
        }
        parse_data->transactions =
          g_list_insert_before(parse_data->transactions, last_transaction, trans_line);
      }
      else
      {
        parse_data->transactions = g_list_append(parse_data->transactions, trans_line);
      }
    }

    /* Increment to the next row. */
    if(redo_errors)
    {
      /* Move to the next error line in the list. */
      error_lines = g_list_next(error_lines);
      if(error_lines == NULL)
        i = parse_data->orig_lines->len; /* Don't continue the for loop. */
      else
        i = GPOINTER_TO_INT(error_lines->data);
    }
    else
    {
      i++;
    }
  }
  if(redo_errors)
  {
    g_list_free(begin_error_lines);
  }
  return 0;
}
