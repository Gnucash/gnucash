#ifndef GNUMERIC_H
#define GNUMERIC_H

#include <glib.h>

#ifndef __attribute__
# if !defined(__GNUC__) || __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 5)
/* OK, this compiler probably doesn't understand __attribute__ */
#  define __attribute__(Spec) /* empty */
# endif
#endif

#define SHEET_MAX_ROWS		(16*16*16*16)	/* 0, 1, ... */
#define SHEET_MAX_COLS		(4*4*4*4)	/* 0, 1, ... */

/*
 * Note: more than 364238 columns will introduce a column named TRUE.
 */

typedef struct _GnmApp			GnmApp;
typedef struct _Workbook		Workbook;
typedef struct _WorkbookView		WorkbookView;
typedef struct _WorkbookControl		WorkbookControl;

typedef struct _Sheet			Sheet;
typedef struct _SheetView		SheetView;
typedef struct _SheetControl		SheetControl;

typedef struct _SheetObject		SheetObject;
typedef struct _SheetObjectAnchor	SheetObjectAnchor;
typedef struct _SheetObjectView		 SheetObjectView;
typedef struct _SheetObjectViewContainer SheetObjectViewContainer;

typedef struct _GnmDepContainer		GnmDepContainer;
typedef struct _GnmDependent		GnmDependent;
typedef struct _GnmCell			GnmCell;
typedef struct _GnmComment		GnmComment;

typedef union  _GnmValue		GnmValue;
typedef struct _GnmValueBool		GnmValueBool;
typedef struct _GnmValueInt		GnmValueInt;
typedef struct _GnmValueFloat		GnmValueFloat;
typedef struct _GnmValueErr		GnmValueErr;
typedef struct _GnmValueStr		GnmValueStr;
typedef struct _GnmValueRange		GnmValueRange;
typedef struct _GnmValueArray		GnmValueArray;

typedef enum {
	GNM_ERROR_NULL,
	GNM_ERROR_DIV0,
	GNM_ERROR_VALUE,
	GNM_ERROR_REF,
	GNM_ERROR_NAME,
	GNM_ERROR_NUM,
	GNM_ERROR_NA,
	GNM_ERROR_RECALC,
	GNM_ERROR_UNKNOWN
} GnmStdError;

typedef struct _RenderedValue		RenderedValue;

typedef GSList 				GnmExprList;
typedef union  _GnmExpr	 		GnmExpr;
typedef struct _GnmExprConstant		GnmExprConstant;
typedef struct _GnmExprFunction		GnmExprFunction;
typedef struct _GnmExprUnary		GnmExprUnary;
typedef struct _GnmExprBinary		GnmExprBinary;
typedef struct _GnmExprName		GnmExprName;
typedef struct _GnmExprCellRef		GnmExprCellRef;
typedef struct _GnmExprArray		GnmExprArray;
typedef struct _GnmExprSet		GnmExprSet;

typedef struct _GnmExprRelocateInfo	GnmExprRelocateInfo;
typedef struct _GnmExprRewriteInfo 	GnmExprRewriteInfo;

typedef struct _GnmExprConventions      GnmExprConventions;
typedef struct _GnmDateConventions      GnmDateConventions;


typedef struct _GnmNamedExpr		GnmNamedExpr;
typedef struct _GnmNamedExprCollection	GnmNamedExprCollection;

typedef struct _GnmPasteTarget		GnmPasteTarget;
typedef struct _GnmCellRegion		GnmCellRegion;

typedef struct _ColRowInfo	 	ColRowInfo;
typedef struct _ColRowCollection	ColRowCollection;
typedef struct _ColRowSegment	 	ColRowSegment;
typedef GSList  ColRowVisList;
typedef GSList  ColRowStateGroup;
typedef GSList  ColRowStateList;
typedef GList   ColRowIndexList;
typedef struct _ColRowIndexSet          ColRowIndexSet;

typedef struct _GnmFormat	        GnmFormat;
typedef struct _GnmFont	        GnmFont;
typedef struct _GnmColor	        GnmColor;
typedef struct _GnmBorder	        GnmBorder;
typedef struct _GnmRow	        GnmRow;
typedef struct _GnmStyle		GnmStyle;

typedef struct _SheetStyleData	        SheetStyleData;
typedef struct _GnmStyleRegion	        GnmStyleRegion;
typedef GSList				GnmStyleList;

typedef struct _FormatTemplate          FormatTemplate;	/* does not really belong here */

typedef struct {
	int col, row;
} GnmCellPos;
typedef struct {
	GnmCellPos start, end;
} GnmRange;
typedef struct {
	Sheet *sheet;
	GnmRange  range;
} GnmSheetRange;
typedef struct _GnmCellRef	        GnmCellRef;	/* abs/rel point with sheet */
typedef struct _GnmRangeRef	        GnmRangeRef;	/* abs/rel range with sheet */
typedef struct _GnmEvalPos		GnmEvalPos;
typedef struct _GnmParsePos	        GnmParsePos;
typedef struct _GnmParseError	        GnmParseError;
typedef struct _FunctionEvalInfo        FunctionEvalInfo;
typedef struct _GnmFunc			GnmFunc;
typedef struct _ErrorInfo		ErrorInfo;

typedef enum {
	CELL_ITER_ALL			= 0,
	CELL_ITER_IGNORE_NONEXISTENT	= 1 << 0,
	CELL_ITER_IGNORE_EMPTY		= 1 << 1,
	CELL_ITER_IGNORE_BLANK		= (CELL_ITER_IGNORE_NONEXISTENT | CELL_ITER_IGNORE_EMPTY),
	CELL_ITER_IGNORE_HIDDEN		= 1 << 2, /* hidden manually */

	/* contains SUBTOTAL, or hidden row in a filter */
	CELL_ITER_IGNORE_SUBTOTAL	= 1 << 3
} CellIterFlags;
typedef GnmValue *(*CellIterFunc) (Sheet *sheet, int col, int row,
				   GnmCell *cell, gpointer user_data);

typedef enum {
	SPANCALC_SIMPLE 	= 0x0,	/* Just calc spans */
	SPANCALC_RESIZE		= 0x1,	/* Calculate sizes of all cells */
	SPANCALC_RE_RENDER	= 0x2,	/* Render and Size all cells */
	SPANCALC_RENDER		= 0x4,	/* Render and Size any unrendered cells */
	SPANCALC_ROW_HEIGHT	= 0x8	/* Resize the row height */
} SpanCalcFlags;

typedef enum {
	GNM_EXPR_EVAL_SCALAR_NON_EMPTY	= 0,
	GNM_EXPR_EVAL_PERMIT_NON_SCALAR	= 0x1,
	GNM_EXPR_EVAL_PERMIT_EMPTY	= 0x2
} GnmExprEvalFlags;

typedef struct _GnmMemChunk		GnmMemChunk;
typedef struct _GnmString 	        GnmString;

typedef struct _GnmCmdContext		GnmCmdContext;
typedef struct _IOContext		IOContext;
typedef struct _GnmFileSaver 		GnmFileSaver;
typedef struct _GnmFileOpener		GnmFileOpener;
typedef struct _XmlParseContext		XmlParseContext;

typedef struct _GnmPlugin		GnmPlugin;
typedef struct _GnmPluginService	GnmPluginService;
typedef struct _GnmPluginLoader		GnmPluginLoader;

typedef struct _GnmSortData		GnmSortData;
typedef struct _GnmSearchReplace	GnmSearchReplace;
typedef struct _GnmConsolidate		GnmConsolidate;
typedef struct _GnmValidation		GnmValidation;
typedef struct _GnmFilter		GnmFilter;
typedef struct _GnmFilterCondition	GnmFilterCondition;
typedef struct _GnmHLink		GnmHLink;
typedef struct _GnmInputMsg		GnmInputMsg;

typedef struct _PrintInformation        PrintInformation;
typedef struct _SolverParameters	SolverParameters;
typedef struct _GnmRelocUndo		GnmRelocUndo;

#endif /* GNUMERIC_H */
