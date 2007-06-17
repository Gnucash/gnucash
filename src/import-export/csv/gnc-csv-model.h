#ifndef GNC_CSV_MODEL_H
#define GNC_CSV_MODEL_H

#include "config.h"

#include "Account.h"
#include "Transaction.h"

#include "stf/stf-parse.h"

/* TODO Comment. */
enum GncCsvColumnType {GNC_CSV_NONE,
                       GNC_CSV_DATE,
                       GNC_CSV_DESCRIPTION,
                       GNC_CSV_AMOUNT};

enum GncCsvErrorType {GNC_CSV_FILE_OPEN_ERR,
                      GNC_CSV_ENCODING_ERR};

typedef struct
{
  char* begin;
  char* end;
} GncCsvStr;

typedef struct
{
  int line_no;
  Transaction* trans;
} GncCsvTransLine;

typedef struct
{
  gchar* encoding;
  GncCsvStr raw_str;
  GncCsvStr file_str;
  GPtrArray* orig_lines;
  StfParseOptions_t* options;
  GArray* column_types;
  GList* error_lines;
  GList* transactions;
} GncCsvParseData;

GncCsvParseData* gnc_csv_new_parse_data(void);

void gnc_csv_parse_data_free(GncCsvParseData* parse_data);

int gnc_csv_convert_enc(GncCsvParseData* parse_data, const char* enc);

int gnc_csv_load_file(GncCsvParseData* parse_data, const char* filename,
                      GError** error);

int gnc_csv_parse(GncCsvParseData* parse_data, gboolean guessColTypes, GError** error);

int gnc_parse_to_trans(GncCsvParseData* parse_data, Account* account);

GncCsvStr file_to_string(const char* filename, GError** error);

#endif
