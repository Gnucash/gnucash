#ifndef GNUMERIC_RANGES_H
#define GNUMERIC_RANGES_H

#include "gnumeric.h"

/**
 * range_equal:
 * @a: First range
 * @b: Second range
 *
 * NB. commutative, symmetric, and transitive.
 *
 * Returns: True if both ranges are equal.
 **/
#define range_equal(a,b)   ((a)->start.row == (b)->start.row && \
			    (a)->end.row   == (b)->end.row && \
			    (a)->start.col == (b)->start.col && \
			    (a)->end.col   == (b)->end.col)

/**
 * range_overlap:
 * @a: First range
 * @b: Second range
 *
 * NB. commutative, symmetric, but not transitive.
 *
 * Returns: True if the ranges overlap at all.
 **/
#define range_overlap(a,b) (((a)->end.row >= (b)->start.row) && \
			    ((b)->end.row >= (a)->start.row) && \
			    ((a)->end.col >= (b)->start.col) && \
			    ((b)->end.col >= (a)->start.col))

/**
 * range_contains:
 * @r:   range to operate on
 * @x:   column,
 * @y:   row co-ordinate
 *
 * Determine if a range contains a col,row co-ordinate.
 *
 * Return value: TRUE if co-ordinate contained.
 **/
#define range_contains(r,x,y)	(((y) <= (r)->end.row) && \
				 ((y) >= (r)->start.row) && \
				 ((x) >= (r)->start.col) && \
				 ((x) <= (r)->end.col))

/*
 * Quickly Test if a range is valid
 */
#define range_valid(r)          ((r)->start.col <= (r)->end.col && \
				 (r)->start.row <= (r)->end.row)

GnmRange   *range_init_full_sheet   (GnmRange *r);
GnmRange   *range_init_rangeref	    (GnmRange *range, GnmRangeRef const *rr);
GnmRange   *range_init_value	    (GnmRange *range, GnmValue const *v);
GnmRange   *range_init_cellpos	    (GnmRange *r, GnmCellPos const *start, GnmCellPos const *end);

GnmRange   *range_init              (GnmRange *r, int start_col, int start_row,
				     int end_col, int end_row);
GnmValue   *range_parse             (Sheet *sheet, char const *range, gboolean strict);
gboolean    parse_range 	    (char const *text, GnmRange *r);
void        range_list_destroy      (GSList *ranges);

int	    range_width		(GnmRange const *r);
int	    range_height	(GnmRange const *r);
gboolean    range_is_singleton  (GnmRange const *r);
gboolean    range_is_infinite   (GnmRange const *r);
gboolean    range_is_full	(GnmRange const *r, gboolean is_cols);
void        range_clip_to_finite(GnmRange *range, Sheet *sheet);
gboolean    range_contained     (GnmRange const *a, GnmRange const *b);
gboolean    range_adjacent      (GnmRange const *a, GnmRange const *b);
GnmRange    range_merge         (GnmRange const *a, GnmRange const *b);
gboolean    range_intersection  (GnmRange *r,
				 GnmRange const *a,
				 GnmRange const *b);
void        range_normalize     (GnmRange *src);
GnmRange    range_union         (GnmRange const *a, GnmRange const *b);
void        range_ensure_sanity (GnmRange *range);
gboolean    range_is_sane	(GnmRange const *range);
gboolean    range_translate     (GnmRange *range, int col_offset, int row_offset);
gboolean    range_transpose     (GnmRange *range, GnmCellPos const *origin);

/* TODO : Do these 2 belong here ? or in sheet.h
 * Probably sheet.h but that is overfull.
 */
gboolean    range_trim		(Sheet const *sheet, GnmRange *r,
				 gboolean cols);
gboolean    range_has_header    (Sheet const *sheet, GnmRange const *src,
				 gboolean top, gboolean ignore_styles);

char const *range_name          (GnmRange const *src);
void        range_dump          (GnmRange const *src, char const *suffix);
GnmRange   *range_dup		(GnmRange const *src);

GSList     *range_split_ranges    (GnmRange const *hard, GnmRange const *soft);
GSList     *range_fragment        (GnmRange const *a, GnmRange const *b);
void        range_fragment_free   (GSList *fragments);

GnmSheetRange *global_range_new   (Sheet *sheet, GnmRange const *r);
GnmSheetRange *global_range_dup   (GnmSheetRange const *src);
gboolean     value_to_global_range  (GnmValue const *v, GnmSheetRange *res);
void         global_range_free      (GnmSheetRange *gr);
gboolean     global_range_overlap   (GnmSheetRange const *a, GnmSheetRange const *b);
GnmValue    *global_range_parse     (Sheet *sheet, char const *range);
char        *global_range_name      (Sheet *sheet, GnmRange const *r);
gboolean     global_range_contained (Sheet const *sheet,
				     GnmValue const *a, GnmValue const *b);
GSList      *global_range_list_parse   (Sheet *sheet, char const *str);
GnmValue    *global_range_list_foreach (GSList *gr_list, GnmEvalPos const *ep,
					CellIterFlags	flags,
					CellIterFunc	handler,
					gpointer	closure);

#endif /* GNUMERIC_RANGES_H */
