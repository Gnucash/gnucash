/*
 * single.h
 */

#ifndef __XACC_SINGLE_H__
#define __XACC_SINGLE_H__
/* cell types */
enum {
  DATE,     
  PRICE,     /* two-digit float point display */
  AMOUNT,    /* three-digit float point display */
  TEXT,      /* string text */
  COMBO,     /* combobox */
};


/* The modify-verify callback is called when a user
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
 */

typedef struct _SingleCell {

  short type;     /* cell type */
  short row;      /* relative row position */
  short col;      /* relative column position */
  short width;    /* column width, in chars, not pixels */
  short alignment; /* column text alignment */

  char * value;   /* current value */

  const char * (*modify_verify) (const char *old, 
                                 const char *add, 
                                 const char *new); 


} SingleCell;


SingleCell * xaccMallocSingleCell (void);
void         xaccInitSingleCell (SingleCell *);

#endif /* __XACC_SINGLE_H__ */
/* ------------------ end of file ---------------------- */
