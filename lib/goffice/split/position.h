#ifndef GNUMERIC_POSITION_H
#define GNUMERIC_POSITION_H

#include "gnumeric.h"

struct _GnmEvalPos {
	GnmCellPos   eval;
	Sheet      *sheet;
	GnmDependent  *dep; /* optionally NULL */
};

struct _GnmParsePos {
	GnmCellPos  eval;
	Sheet	   *sheet;
	Workbook   *wb;
};

/**
 * Used for getting a valid Sheet *from a GnmCellRef
 * Syntax is GnmCellRef, valid Sheet *
 */
#define eval_sheet(a,b)     (((a) != NULL) ? (a) : (b))

/* Initialization routines for Evaluation Positions */
GnmEvalPos  *eval_pos_init	 (GnmEvalPos *ep, Sheet *s, GnmCellPos const *pos);
GnmEvalPos  *eval_pos_init_dep	 (GnmEvalPos *ep, GnmDependent const *dep);
GnmEvalPos  *eval_pos_init_cell	 (GnmEvalPos *ep, GnmCell const *cell);
GnmEvalPos  *eval_pos_init_sheet (GnmEvalPos *ep, Sheet *sheet);

/* Initialization routines for Parse Positions */
GnmParsePos *parse_pos_init         (GnmParsePos *pp, Workbook *wb,
				     Sheet *sheet, int col, int row);
GnmParsePos *parse_pos_init_dep	    (GnmParsePos *pp, GnmDependent const *dep);
GnmParsePos *parse_pos_init_cell    (GnmParsePos *pp, GnmCell const *cell);
GnmParsePos *parse_pos_init_evalpos (GnmParsePos *pp, GnmEvalPos const *pos);
GnmParsePos *parse_pos_init_editpos (GnmParsePos *pp, SheetView const *sv);
GnmParsePos *parse_pos_init_sheet   (GnmParsePos *pp, Sheet *sheet);

/*****************************************************************************/

struct _GnmCellRef {
	Sheet *sheet;
	int   col, row;

	unsigned char col_relative;
	unsigned char row_relative;
};
struct _GnmRangeRef {
	GnmCellRef a, b;
};

GnmCellRef *cellref_init        (GnmCellRef *ref, Sheet *sheet, int col, int row,
				 gboolean rel);
gboolean    cellref_equal	(GnmCellRef const *a, GnmCellRef const *b);
guint       cellref_hash        (GnmCellRef const *cr);
void        cellref_make_abs	(GnmCellRef *dest,
				 GnmCellRef const *src,
				 GnmEvalPos const *ep);
int         cellref_get_abs_col	(GnmCellRef const *ref,
				 GnmEvalPos const *pos);
int         cellref_get_abs_row	(GnmCellRef const *cell_ref,
				 GnmEvalPos const *src_fp);
void        cellref_get_abs_pos	(GnmCellRef const *cell_ref,
				 GnmCellPos const *pos,
				 GnmCellPos *res);

gboolean     rangeref_equal	(GnmRangeRef const *a, GnmRangeRef const *b);
guint	     rangeref_hash	(GnmRangeRef const *cr);
GnmRangeRef *rangeref_dup	(GnmRangeRef const *cr);
void         rangeref_normalize (GnmRangeRef const *ref, GnmEvalPos const *ep,
				 Sheet **start_sheet, Sheet **end_sheet,
				 GnmRange *dest);

guint cellpos_hash  (GnmCellPos const *key);
gint  cellpos_equal (GnmCellPos const *a, GnmCellPos const *b);

#endif /* GNUMERIC_POSITION_H */
