
/* cell types */
enum {
  DATE,     
  PRICE,     /* two-digit float point display */
  AMOUNT,    /* three-digit float point display */
  TEXT,      /* string text */
  COMBO,     /* combobox */
};


/* The modify-verify callback is called when a user
 * makes a change to a cell.  The input is a changed string.
 * It must return a string, or void if it rejects the change.
 */

typedef struct _SingleCell {

  short type;     /* cell type */
  short row;      /* relative row position */
  short col;      /* relative column position */
  short width;    /* column width, in chars, not pixels */

  char * value;   /* current value */

  char * (*modify_verify) (char *)  /* modify verify callback */


} SingleCell;

typedef struct _CellArray {

  short numRows;
  short numCols;

  SingleCell **cells;  /* row-col array */

} CellArray;

SingleCell * xaccMallocSingleCell (void);
void         xaccInitSingleCell (SingleCell *);


CellArray * xaccMallocCellArray (void);
void        xaccInitCellArray (CellArray *, int numrows, int numcols);

/* add a cell to the array */
void        xaccAddCell (SingleCell *);


SingleCell * xaccMallocSingleCell (void)
{

}
