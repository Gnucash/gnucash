/*
 * FILE:
 * basiccell.h
 *
 * FUNCTION:
 * The BasicCell struct provides an abstract base class  
 * defining the handling of the editing of a cell of a table.
 * Classes that provide the actual handling for different
 * cell types should inherit from this class.
 *
 * MEMBERS:
 * The input_output member is zero if the cell is supposed 
 * to only display values, but not accept user input.  If
 * non-zero, then the callbacks below are used to when the
 * cell is entered.
 *
 * CALLBACKS:
 * The enter_cell() callback is called when the user first
 * makes a move to enter a cell.  The current value of the 
 * cell is passed as the argument.  If the callback wishes
 * to change the value of the cell, it can return a non-null
 * string.  Alternately, to leave the value of the cell 
 * unchanged, it can return NULL.  If a string is returned, 
 * the string must be as the result of a malloc.
 *
 * The leave_cell() callback is called when the user exits
 * a cell.  The current value of the cell is passed as the 
 * argument.  If the callback wishes to change the value of 
 * the cell, it can return a non-null string.  Alternately, 
 * to leave the value of the cell unchanged, it can return 
 * NULL.  If a string is returned, the string must be as the 
 * result of a malloc.
 *
 * The modify-verify callback is called when a user
 * makes a change to a cell.  
 * The three arguments passed in are :
 * "old", the string prior to user's attempted modification,
 * "add", the string the user is attemptiong to add
 *        (will be null if text is being deleted).
 * "new", the string that would result is user's changes
 *        are accepted.
 * It must return a string, or void if it rejects the change.
 * The returned string will be used to update the cell value.
 *
 * Some memory management rules:
 * (1) the callback must not modify the values of old, change, new
 * (2) if the callback likes the new string, it may return the
 *     pointer to "new".  It must *not* return the pointer to 
 *     "change" or "old"
 * (3) if the callback chooses to not return "new", it must 
 *     malloc the memory for a new string.  It does not need
 *     to worry about garbage collection.
 *
 * GUI STUFF:
 * The realize callback will be called when GUI-specific 
 * initalization needs to be done.  For Xt/Motif, the second
 * argument will be cast to the parent widget.
 */

#ifndef __XACC_BASIC_CELL_H__
#define __XACC_BASIC_CELL_H__

typedef struct _BasicCell {

  short width;     /* column width, in chars, not pixels */
  short alignment; /* column text alignment */
  char  input_output;  /* zero if output-only */

  char * value;   /* current value */

  const char * (*enter_cell) (struct _BasicCell *,
                              const char * current);
  const char * (*modify_verify) (struct _BasicCell *,
                                 const char *old, 
                                 const char *add, 
                                 const char *new); 
  const char * (*leave_cell) (struct _BasicCell *,
                              const char * current);

  /* private, GUI-specific initializer */
  void         (* realize) (struct _BasicCell *, void *gui_handle);

} BasicCell;


BasicCell * xaccMallocBasicCell (void);
void         xaccInitBasicCell (BasicCell *);

void         xaccSetBasicCellValue (BasicCell *, char *);

#endif /* __XACC_BASIC_CELL_H__ */
/* ------------------ end of file ---------------------- */
