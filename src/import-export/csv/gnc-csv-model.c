#include "gnc-csv-model.h"

#include "gnc-book.h"

#include <goffice/utils/go-glib-extras.h>

#include <string.h>
#include <sys/time.h>

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <time.h>

const int num_date_formats = 8;
/* A set of date formats that the user sees. */
const gchar* date_format_user[] = {"yyyy/mm/dd",
                                   "yy/mm/dd",
                                   "dd/mm/yyyy",
                                   "dd/mm/yy",
                                   "dd/mm/yyyy",
                                   "dd/mm/yy",
                                   "mm/dd/yyyy",
                                   "mm/dd/yy"};

/* Matching formats for date_format_user to be used with strptime. */
const gchar* date_format_internal[] = {"%Y/%m/%d",
                                       "%y/%m/%d",
                                       "%d/%m/%Y",
                                       "%d/%m/%y",
                                       "%d/%m/%Y",
                                       "%d/%m/%y",
                                       "%m/%d/%Y",
                                       "%m/%d/%y"};

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

/* TODO This will be replaced by something more sophisticated. */
static time_t parse_date(const char* date_str, int format)
{
  struct tm retvalue;
  char mstr[3], dstr[3], ystr[3];
  strptime(date_str, date_format_internal[format], &retvalue);
  /* TODO Handle error */
  /* We have to set the hour, minute, second and daylight savings time
   * flags to valid values. */
  retvalue.tm_hour = 0;
  retvalue.tm_min = 0;
  retvalue.tm_sec = 1;
  retvalue.tm_isdst = -1;
  return mktime(&retvalue);

  /* TODO Get rid of the old clumsy method */
  strncpy(mstr, date_str, 2);
  strncpy(dstr, date_str + 3, 2);
  strncpy(ystr, date_str + 6, 2);
  mstr[2] = dstr[2] = ystr[2] = '\0';
  retvalue.tm_mon = atoi(mstr) - 1;
  retvalue.tm_mday = atoi(dstr);
  retvalue.tm_year = atoi(ystr);
  if(retvalue.tm_year < 10)
    retvalue.tm_year += 100;
  retvalue.tm_hour = 0;
  retvalue.tm_min = 0;
  retvalue.tm_sec = 1;
  retvalue.tm_isdst = -1;
  return mktime(&retvalue);
}

/** Loads a file into a string.
 * @param filename Name of the file to open
 * @param error Passes back the error that occurred, if one occurred.
 * @return Contents of the file at filename if successful; NULL if an error occurred
 */
GncCsvStr file_to_string(const char* filename, GError** error)
{
  /* The file descriptor for opening the file, a flag indicating
   * whether the file actually exists, the length of the file */
  int fd, exists, length, max_cols = 0, i;

  struct stat buf; /* Used to find file size */

  /* What we want to return. */
  GncCsvStr file_str;

  /* Make sure filename is meaningful. */
  if(filename == NULL)
  {
    g_set_error(error, 0, 0, "Received NULL filename.");
    file_str.begin = file_str.end = NULL;
    return file_str;
  }

  exists = stat(filename, &buf);
  /* Make sure the file exists. */
  if(exists == -1)
  {
    g_set_error(error, 0, 0, "File %s does not exist.", filename);
    file_str.begin = file_str.end = NULL;
    return file_str;
  }

  /* Now we can get the length of the file out of buf. */
  length = (int)(buf.st_size);

  /* Put the file's contents into a string starting at data_begin. */
  fd = open(filename, O_RDONLY);
  file_str.begin = mmap(0, length, PROT_READ, MAP_PRIVATE, fd, 0);
  file_str.end = file_str.begin + length - 1; /* Point data_end to the end. */
  close(fd);

  return file_str;
}

/** Constructor for GncCsvParseData.
 * @return Pointer to a new GncCSvParseData
 */
GncCsvParseData* gnc_csv_new_parse_data(void)
{
  GncCsvParseData* parse_data = g_malloc(sizeof(GncCsvParseData));
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
  return parse_data;
}

/** Destructor for GncCsvParseData.
 * @param parse_data Parse data whose memory will be freed
 */
void gnc_csv_parse_data_free(GncCsvParseData* parse_data)
{
  /* All non-NULL pointers have been initialized and must be freed. */
  
  /* parse_data->raw_str is created using mmap (see file_to_string),
   * so we free it using munmap. */
  if(parse_data->raw_str.begin != NULL)
    munmap(parse_data->raw_str.begin,
           parse_data->raw_str.end - parse_data->raw_str.begin);

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
  parse_data->raw_str = file_to_string(filename, error);
  if(parse_data->raw_str.begin == NULL)
  {
    /* TODO Handle file opening errors more specifically,
     * e.g. inexistent file versus no read permission. */
    g_set_error(error, 0, GNC_CSV_FILE_OPEN_ERR, "File opening failed.");
    return 1;
  }

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
  GStringChunk* chunk; /* TODO Find out exactly what this is. */
  /* max_cols is the number of columns in the row with the most columns. */
  int i, max_cols = 0;

  /* Do the actual parsing. */
  /* TODO: This size might have to change ... because I'm not exactly
   * sure what it's for. ... */
  chunk = g_string_chunk_new(100);
  parse_data->orig_lines = stf_parse_general(parse_data->options, chunk,
                                             parse_data->file_str.begin,
                                             parse_data->file_str.end);
  g_string_chunk_free(chunk);
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
  g_debug("max_cols %d\n", max_cols);

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
  GNCBook* book = gnc_account_get_book(account);
  GList *error_lines, *begin_error_lines;
  gnc_commodity* currency = xaccAccountGetCommodity(account);

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
    parse_data->transactions = NULL;
  }

  if(redo_errors) /* If we're looking only at error data ... */
  {
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
    Transaction* trans = xaccMallocTransaction(book);
    GPtrArray* line = parse_data->orig_lines->pdata[i];
    Split* split;
    /* This flag is FALSE if there are any errors in this row. */
    /* TODO This flag isn't used yet. */
    gboolean noErrors = TRUE;

    /* By the time we traverse the column, essential_properties_left
     * should be 0; if it isn't the row is considered to have an
     * error. Each time an "essential" property is set, we subtract 1
     * from it. At the moment, the only essential properties are
     * "Date" and "Amount". */
    unsigned int essential_properties_left = 2;
    
    /* The data types that are used to create transactions */
    time_t date;
    const char* description;
    gnc_numeric amount;

    xaccTransBeginEdit(trans);
    xaccTransSetCurrency(trans, currency);

    /* TODO There should eventually be a specification of what errors
     * actually occurred, not just that one happened. */

    for(j = 0; j < line->len; j++)
    {
      /* We do nothing in "None" columns. */
      if(column_types->data[j] != GNC_CSV_NONE)
      {
        /* Affect the transaction appropriately. */
        if(column_types->data[j] == GNC_CSV_DATE)
        {
          date = parse_date(line->pdata[j], parse_data->date_format);
          xaccTransSetDatePostedSecs(trans, date);
          essential_properties_left--;
        }
        else if(column_types->data[j] == GNC_CSV_DESCRIPTION)
        {
          description = line->pdata[j];
          xaccTransSetDescription(trans, description);
        }
        else if(column_types->data[j] == GNC_CSV_AMOUNT)
        {
          amount = double_to_gnc_numeric(atof(line->pdata[j]), 1,
                                         GNC_RND_ROUND);
          split = xaccMallocSplit(book);
          xaccSplitSetAccount(split, account);
          xaccSplitSetParent(split, trans);
          xaccSplitSetAmount(split, amount);
          xaccSplitSetValue(split, amount);
          xaccSplitSetAction(split, "Deposit");
          essential_properties_left--;
        }
      }
    }

    /* If we had success, add the transaction to parse_data->transaction. */
    if(noErrors && essential_properties_left == 0)
    {
      GncCsvTransLine* trans_line = g_malloc(sizeof(GncCsvTransLine));
      trans_line->line_no = i;
      trans_line->trans = trans;
      parse_data->transactions = g_list_append(parse_data->transactions, trans_line);
    }
    /* If there was no success, add this line to
     * parse_data->error_lines and free the transaction. */
    else
    {
      parse_data->error_lines = g_list_append(parse_data->error_lines,
                                              GINT_TO_POINTER(i));
      xaccTransDestroy(trans);
    }

    if(redo_errors)
    {
      /* Move to the next error line. */
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
