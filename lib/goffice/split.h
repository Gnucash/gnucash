#ifndef SPLIT_H
#define SPLIT_H

#include "gnumeric.h"

struct _Workbook
{
};

GnmDateConventions const * workbook_date_conv( Workbook const *wb );
extern GnmExprConventions *gnm_expr_conventions_default;

struct _GnmExprConventions
{
  gboolean output_translated;
  /* If non-null, used to separate elements in lists.  */
  char const *output_argument_sep;
  /* If non-null, used to separate array columns.  */
  char const *output_array_col_sep;
};

#endif /*SPLIT_H*/
