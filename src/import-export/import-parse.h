/*
 * import-parse.h -- a generic "parser" API for importers..  Allows importers
 * 	to parse dates and numbers, and provides a UI to ask for users to
 * 	resolve ambiguities.
 *
 * Created by:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef IMPORT_PARSE_H
#define IMPORT_PARSE_H

#include "gnc-numeric.h"
#include "gnc-date.h"

typedef enum {
  /* number formats */
  GNCIF_NUM_PERIOD	= (1 << 1),
  GNCIF_NUM_COMMA	= (1 << 2),

  /* date formats */
  GNCIF_DATE_MDY	= (1 << 8),
  GNCIF_DATE_DMY	= (1 << 9),
  GNCIF_DATE_YMD	= (1 << 10),
  GNCIF_DATE_YDM	= (1 << 11)
} GncImportFormat;


GncImportFormat gnc_import_test_numeric(const char* str, GncImportFormat fmts);
GncImportFormat gnc_import_test_date(const char* str, GncImportFormat fmts);


GncImportFormat gnc_import_choose_fmt(const char* msg, GncImportFormat fmts,
				      gpointer user_data);

gboolean gnc_import_parse_numeric(const char* str, GncImportFormat fmt,
				  gnc_numeric *val);
gboolean gnc_import_parse_date(const char *date, GncImportFormat fmt,
			       Timespec *val);

/* Set and clear flags in bit-flags */
#define import_set_flag(i,f) (i |= f)
#define import_clear_flag(i,f) (i &= ~f)


#endif /* IMPORT_PARSE_H */
