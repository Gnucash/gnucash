/*
 * FILE:
 * datecell.h
 *
 * FUNCTION:
 * Implements date handling
 *
 * The xaccSetDateCellValue() method accepts a numeric date and
 *    sets the contents of the date cell to that value.  The
 *    value day, month, year should follow the regular written
 *    conventions, i.e. day==(1..31), mon==(1..12), year==4 digits
 */
 
#ifndef __XACC_DATE_CELL_C__
#define __XACC_DATE_CELL_C__

#include <time.h>
#include "basiccell.h"

typedef struct _DateCell {
   BasicCell cell;
   struct tm date;
} DateCell;

/* installs a callback to handle date recording */
DateCell * xaccMallocDateCell (void);
void       xaccInitDateCell (DateCell *);

void       xaccSetDateCellValue (DateCell *, int day, int mon, int year);  

#endif /* __XACC_DATE_CELL_C__ */

/* --------------- end of file ---------------------- */
