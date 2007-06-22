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

#include <stdio.h> /* Get rid of this */

/* Returns a set of sensible defaults for parsing CSV files. */
static StfParseOptions_t* default_parse_options(void)
{
  StfParseOptions_t* options = stf_parse_options_new();
  stf_parse_options_set_type(options, PARSE_TYPE_CSV);
  stf_parse_options_csv_set_separators(options, ",", NULL);
  return options;
}

/* TODO This will be replaced by something more sophisticated. */
time_t parse_date(const char* date_str);

time_t parse_date(const char* date_str)
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

/* TODO Comment. */
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
    g_list_free(parse_data->transactions);
  g_free(parse_data);
}

/* TODO Comment */
int gnc_csv_convert_enc(GncCsvParseData* parse_data, const char* enc)
{
  GError* error;
  gsize bytes_read, bytes_written;
  if(parse_data->file_str.begin != NULL)
  {
    g_free(parse_data->file_str.begin);
  }
  parse_data->file_str.begin = g_convert(parse_data->raw_str.begin,
                                         parse_data->raw_str.end - parse_data->raw_str.begin,
                                         "UTF-8", enc, &bytes_read,
                                         &bytes_written, &error);
  g_debug("using %s got %p\n", enc, parse_data->file_str.begin);
  if(parse_data->file_str.begin == NULL)
  {
    return 1;
  }
  parse_data->file_str.end = parse_data->file_str.begin + bytes_written;
  parse_data->encoding = (gchar*)enc;
  return 0;
}

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
  gnc_csv_convert_enc(parse_data, guess_enc);
  if(parse_data->file_str.begin == NULL)
  {
    g_set_error(error, 0, GNC_CSV_ENCODING_ERR, "Encoding conversion failed.");
    return 1;
  }
  else
    return 0;
}

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
      parse_data->transactions = g_list_append(parse_data->transactions, trans);
  }
  return 0;
}
