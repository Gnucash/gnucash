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
static time_t parse_date(const char* date_str)
{
  struct tm retvalue;
  char mstr[3], dstr[3], ystr[3];
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
/* TODO Comment */
GncCsvParseData* gnc_csv_new_parse_data(void)
{
  GncCsvParseData* parse_data = g_malloc(sizeof(GncCsvParseData));
  parse_data->encoding = "UTF-8";
  parse_data->raw_str.begin = parse_data->raw_str.end
    = parse_data->file_str.begin = parse_data->file_str.end = NULL;
  parse_data->orig_lines = NULL;
  parse_data->column_types = NULL;
  parse_data->error_lines = parse_data->transactions = NULL;
  parse_data->options = default_parse_options();
  return parse_data;
}

/** Destructor for GncCsvParseData.
 * @param parse_data Parse data whose memory will be freed
 */
/* TODO Comment */
void gnc_csv_parse_data_free(GncCsvParseData* parse_data)
{
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
  /* TODO Find out if there's a potential memory leak here. */
  if(parse_data->transactions != NULL)
  {
    GList* transactions = parse_data->transactions;
    do
    {
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
 * @return 0 on success, 1 on failure
 */
/* TODO Comment */
int gnc_csv_convert_encoding(GncCsvParseData* parse_data, const char* encoding)
{
  GError* error;
  gsize bytes_read, bytes_written;
  if(parse_data->file_str.begin != NULL)
  {
    g_free(parse_data->file_str.begin);
  }
  parse_data->file_str.begin = g_convert(parse_data->raw_str.begin,
                                         parse_data->raw_str.end - parse_data->raw_str.begin,
                                         "UTF-8", encoding, &bytes_read,
                                         &bytes_written, &error);
  if(parse_data->file_str.begin == NULL)
  {
    return 1;
  }
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
/* TODO Comment */
int gnc_csv_load_file(GncCsvParseData* parse_data, const char* filename,
                      GError** error)
{
  const char* guess_enc;
  parse_data->raw_str = file_to_string(filename, error);
  if(parse_data->raw_str.begin == NULL)
  {
    g_set_error(error, 0, GNC_CSV_FILE_OPEN_ERR, "File opening failed.");
    return 1;
  }
  guess_enc = go_guess_encoding((const char*)(parse_data->raw_str.begin),
                                (size_t)(parse_data->raw_str.end - parse_data->raw_str.begin),
                                "UTF-8", NULL);
  g_debug("Guessed %s\n", guess_enc);
  /* TODO Handle error */
  gnc_csv_convert_encoding(parse_data, guess_enc);
  if(parse_data->file_str.begin == NULL)
  {
    g_set_error(error, 0, GNC_CSV_ENCODING_ERR, "Encoding conversion failed.");
    return 1;
  }
  else
    return 0;
}

/** Parses a file into cells. This requires having an encoding that
 * works (see gnc_csv_convert_encoding). parse_data->options should be
 * set according to how the user wants before calling this
 * function. (Note: if guessColTypes is TRUE, all the column types
 * will be GNC_CSV_NONE right now.)
 * @param parse_data Data that is being parsed
 * @param guessColTypes TRUE to guess what the types of columns are based on the cell contents
 * @error error Will contain an error if there is a failure
 * @return 0 on success, 1 on failure
 */
/* TODO Comment. */
/* TODO Should we use 0 for domain and code in errors? */
int gnc_csv_parse(GncCsvParseData* parse_data, gboolean guessColTypes, GError** error)
{
  GStringChunk* chunk; /* TODO Find out exactly what this is. */
  int i, max_cols = 0;

  /* Do the actual parsing. */
  /* TODO: This size might have to change ... because I'm not exactly
   * sure what it's for. ... */
  chunk = g_string_chunk_new(100);
  parse_data->orig_lines = stf_parse_general(parse_data->options, chunk,
                                             parse_data->file_str.begin,
                                             parse_data->file_str.end);
  g_string_chunk_free(chunk);
  if(parse_data->orig_lines == NULL) /* If it failed, generate an error message. */
  {
    g_set_error(error, 0, 0, "Parsing failed.");
    return 1;
  }

  /* Now that we have data, let's update max_cols. */
  for(i = 0; i < parse_data->orig_lines->len; i++)
  {
    if(max_cols < ((GPtrArray*)(parse_data->orig_lines->pdata[i]))->len)
      max_cols = ((GPtrArray*)(parse_data->orig_lines->pdata[i]))->len;
  }
  g_debug("max_cols %d\n", max_cols);

  if(guessColTypes)
  {
    if(parse_data->column_types != NULL)
      g_array_free(parse_data->column_types, TRUE);
    parse_data->column_types = g_array_sized_new(FALSE, FALSE, sizeof(int),
                                                 max_cols);
    g_array_set_size(parse_data->column_types, max_cols);
    for(i = 0; i < parse_data->column_types->len; i++)
    {
      parse_data->column_types->data[i] = GNC_CSV_NONE;
    }
  }
  else
  {
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
 * @return 0 on success, 1 on failure
 */
/* TODO Comment. */
int gnc_parse_to_trans(GncCsvParseData* parse_data, Account* account)
{
  int i, j;
  GArray* column_types = parse_data->column_types;
  GNCBook* book = gnc_account_get_book(account);

  if(parse_data->error_lines != NULL)
  {
    g_list_free(parse_data->error_lines);
    parse_data->error_lines = NULL;
  }
  if(parse_data->transactions != NULL)
  {
    g_list_free(parse_data->transactions);
    parse_data->transactions = NULL;
  }
  
  for(i = 0; i < parse_data->orig_lines->len; i++)
  {
    Transaction* trans = xaccMallocTransaction(book);
    gboolean noErrors = TRUE;
    GPtrArray* line = parse_data->orig_lines->pdata[i];
    time_t date;
    const char* description;
    gnc_numeric amount;
    Split* split;
    xaccTransBeginEdit(trans);
    for(j = column_types->len - 1; j >= 0; j--)
    {
      if(column_types->data[j] != GNC_CSV_NONE)
      {
        /* If this line is too short, it goes in errors list. */
        if(j >= line->len)
        {
          parse_data->error_lines = g_list_append(parse_data->error_lines,
                                                  line);
          xaccTransDestroy(trans);
          noErrors = FALSE;
          break;
        }
        /* Affect the transaction appropriately. */
        if(column_types->data[j] == GNC_CSV_DATE)
        {
          date = parse_date(line->pdata[j]);
          xaccTransSetDatePostedSecs(trans, date);
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
        }
      }
    }
    if(noErrors)
    {
      GncCsvTransLine* trans_line = g_malloc(sizeof(GncCsvTransLine));
      trans_line->line_no = i;
      trans_line->trans = trans;
      parse_data->transactions = g_list_append(parse_data->transactions, trans_line);
    }
  }
  return 0;
}
