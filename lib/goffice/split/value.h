#ifndef GNUMERIC_VALUE_H
#define GNUMERIC_VALUE_H

#include <glib.h>
#include "gnumeric.h"
#include "position.h"
#include "numbers.h"

typedef enum {
	/* Use magic values to act as a signature
	 * DO NOT CHANGE THESE NUMBERS
	 * As of version 0.57 they are using as keys
	 * in the xml
	 */
	VALUE_EMPTY	= 10,
	VALUE_BOOLEAN	= 20, /* Keep bool < int < float */
	VALUE_INTEGER	= 30,
	VALUE_FLOAT	= 40,
	VALUE_ERROR	= 50,
	VALUE_STRING	= 60,
	VALUE_CELLRANGE = 70,
	VALUE_ARRAY	= 80
} GnmValueType;

typedef struct {
	GnmValueType const type;
	GnmFormat *fmt;
} GnmValueAny;
struct _GnmValueBool {
	GnmValueType const type;
	GnmFormat *fmt;
	gboolean val;
};
struct _GnmValueInt {
	GnmValueType const type;
	GnmFormat *fmt;
	int val;
};
struct _GnmValueFloat {
	GnmValueType const type;
	GnmFormat *fmt;
	gnm_float val;
};
struct _GnmValueErr {
	GnmValueType const type;
	GnmFormat *fmt;
	GnmString   *mesg;
	/* Currently unused.  Intended to support audit functions */
	GnmEvalPos  src;
};
struct _GnmValueStr {
	GnmValueType const type;
	GnmFormat *fmt;
	GnmString   *val;
};
struct _GnmValueRange {
	GnmValueType const type;
	GnmFormat *fmt;
	GnmRangeRef cell;
};
struct _GnmValueArray {
	GnmValueType const type;
	GnmFormat *fmt;
	int x, y;
	GnmValue ***vals;  /* Array [x][y] */
};

/* FIXME */
union _GnmValue {
	GnmValueType const type;
	GnmValueAny	v_any;
	GnmValueBool	v_bool;
	GnmValueInt	v_int;
	GnmValueFloat	v_float;
	GnmValueErr	v_err;
	GnmValueStr	v_str;
	GnmValueRange	v_range;
	GnmValueArray	v_array;
};

#define	VALUE_TYPE(v)			((v)->v_any.type)
#define	VALUE_FMT(v)			((v)->v_any.fmt)
#define VALUE_IS_EMPTY(v)		(((v) == NULL) || ((v)->type == VALUE_EMPTY))
#define VALUE_IS_EMPTY_OR_ERROR(v)	(VALUE_IS_EMPTY(v) || (v)->type == VALUE_ERROR)
#define VALUE_IS_STRING(v)		((v)->type == VALUE_STRING)
#define VALUE_IS_NUMBER(v)		(((v)->type == VALUE_INTEGER) || \
					 ((v)->type == VALUE_FLOAT) || \
					 ((v)->type == VALUE_BOOLEAN))

typedef enum {
	IS_EQUAL,
	IS_LESS,
	IS_GREATER,
	TYPE_MISMATCH
} GnmValDiff;

GnmValue *value_new_empty            (void);
GnmValue *value_new_bool             (gboolean b);
GnmValue *value_new_int              (int i);
GnmValue *value_new_float            (gnm_float f);
GnmValue *value_new_error            (GnmEvalPos const *pos, char const *mesg);
GnmValue *value_new_error_str        (GnmEvalPos const *pos, GnmString *mesg);
GnmValue *value_new_error_std        (GnmEvalPos const *pos, GnmStdError err);
GnmValue *value_new_error_NULL       (GnmEvalPos const *pos);
GnmValue *value_new_error_DIV0       (GnmEvalPos const *pos);
GnmValue *value_new_error_VALUE      (GnmEvalPos const *pos);
GnmValue *value_new_error_REF        (GnmEvalPos const *pos);
GnmValue *value_new_error_NAME       (GnmEvalPos const *pos);
GnmValue *value_new_error_NUM        (GnmEvalPos const *pos);
GnmValue *value_new_error_NA         (GnmEvalPos const *pos);
GnmValue *value_new_error_RECALC     (GnmEvalPos const *pos);
GnmValue *value_new_string           (char const *str);
GnmValue *value_new_string_nocopy    (char *str);
GnmValue *value_new_string_str       (GnmString *str);
GnmValue *value_new_cellrange_unsafe (GnmCellRef const *a, GnmCellRef const *b);
GnmValue *value_new_cellrange        (GnmCellRef const *a, GnmCellRef const *b,
				      int eval_col, int eval_row);
GnmValue *value_new_cellrange_r      (Sheet *sheet, GnmRange const *r);
GnmValue *value_new_array            (guint cols, guint rows);
GnmValue *value_new_array_empty      (guint cols, guint rows);
GnmValue *value_new_array_non_init   (guint cols, guint rows);
GnmValue *value_new_from_string	     (GnmValueType t, char const *str,
				      GnmFormat *sf, gboolean translated);

void        value_release	   (GnmValue *v);
void	    value_set_fmt	   (GnmValue *v, GnmFormat const *fmt);
void        value_dump		   (GnmValue const *v);
GnmValue   *value_dup		   (GnmValue const *v);

gnm_float   value_diff		   (GnmValue const *a, GnmValue const *b);
GnmValDiff  value_compare	   (GnmValue const *a, GnmValue const *b,
				    gboolean case_sensitive);
int	    value_cmp		   (void const *ptr_a, void const *ptr_b);
int	    value_cmp_reverse	   (void const *ptr_a, void const *ptr_b);
gint	    value_equal		   (GnmValue const *a, GnmValue const *b);
guint       value_hash		   (GnmValue const *v);

char const *value_peek_string	   (GnmValue const *v);
char       *value_get_as_string	   (GnmValue const *v);
void        value_get_as_gstring   (GnmValue const *v, GString *target,
				    GnmExprConventions const *conv);

int         value_get_as_int	   (GnmValue const *v);
gnm_float   value_get_as_float	   (GnmValue const *v);
GnmValue   *value_coerce_to_number (GnmValue *v, gboolean *valid,
				    GnmEvalPos const *ep);

GnmValue   *value_error_set_pos    (GnmValueErr *err, GnmEvalPos const *pos);
GnmStdError value_error_classify   (GnmValue const *v);
char const *value_error_name       (GnmStdError err, gboolean translated);

gboolean    value_get_as_bool	      (GnmValue const *v, gboolean *err);
gboolean    value_get_as_checked_bool (GnmValue const *v);
GnmRangeRef const *value_get_rangeref (GnmValue const *v);

/* Area functions ( works on VALUE_RANGE or VALUE_ARRAY */
/* The GnmEvalPos provides a Sheet context; this allows
   calculation of relative references. 'x','y' give the position */
typedef GnmValue *(*ValueAreaFunc) (GnmValue const *v, GnmEvalPos const *ep,
				    int x, int y, gpointer user);
GnmValue       *value_area_foreach    (GnmValue const *v, GnmEvalPos const *ep,
				       CellIterFlags flags,
				       ValueAreaFunc func, gpointer user);
int             value_area_get_width  (GnmValue const *v, GnmEvalPos const *ep);
int             value_area_get_height (GnmValue const *v, GnmEvalPos const *ep);
GnmValue const *value_area_fetch_x_y  (GnmValue const *v, int x, int y,
				       GnmEvalPos const *ep);
GnmValue const *value_area_get_x_y    (GnmValue const *v, int x, int y,
				       GnmEvalPos const *ep);

/* A zero integer, not to be freed or changed.  */
extern GnmValue const *value_zero;
extern GnmValueErr const value_terminate_err;
#define VALUE_TERMINATE ((GnmValue *)&value_terminate_err)

void value_array_set       (GnmValue *array, int col, int row, GnmValue *v);
void value_array_resize    (GnmValue *v, int width, int height);

/* FIXME: this stuff below ought to go elsewhere.  */
typedef struct {
        int    row;
        GSList *conditions;
} database_criteria_t;
typedef gboolean (*criteria_test_fun_t) (GnmValue const *x, GnmValue const *y);
typedef struct {
        criteria_test_fun_t fun;
        GnmValue               *x;
        int                 column;
} func_criteria_t;
void	parse_criteria		(GnmValue *criteria,
				 criteria_test_fun_t *fun,
				 GnmValue **test_value,
				 CellIterFlags *iter_flags,
				 GnmDateConventions const *date_conv);
void	free_criterias		(GSList *criterias);
GSList *find_rows_that_match	(Sheet *sheet, int first_col,
				 int first_row, int last_col, int last_row,
				 GSList *criterias, gboolean unique_only);
GSList *parse_database_criteria (GnmEvalPos const *ep, GnmValue *database, GnmValue *criteria);
int     find_column_of_field	(GnmEvalPos const *ep, GnmValue *database, GnmValue *field);

/* Protected */
void value_init     (void);
void value_shutdown (void);

#endif /* GNUMERIC_VALUE_H */
