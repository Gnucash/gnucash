/*
 * FILE:
 * datecell.h
 *
 * FUNCTION:
 * The DateCell object implements a date handling cell.  It
 * is able to display dates in several formats, it is also 
 * able to accept date input, and also supports a number of accelerator
 * keys.
 *
 * On output, this cell will display a date as mm/dd/yy
 * The actual date formate is compile-time configurable.
 *
 * hack alert -- make the display format run-time configurable, and 
 * appropriately internationalized.
 *
 * One input, this cell is able to parse dates that are entered.
 * It rejects any input that is not in the form of a date.  Input
 * also includes a number of accelerators:  
 *
 *     '+':
 *     '=':    increment day 
 *
 *     '_':
 *     '-':    decrement day 
 *
 *     '}':
 *     ']':    increment month 
 *
 *     '{':
 *     '[':    decrment month 
 *
 *     'M':
 *     'm':    begining of month 
 *
 *     'H':
 *     'h':    end of month 
 *
 *     'Y':
 *     'y':    begining of year 
 *
 *     'R':
 *     'r':    end of year 
 *
 *     'T':
 *     't':    today 
 *
 * 
 * METHODS:
 * The xaccSetDateCellValue() method accepts a numeric date and
 *    sets the contents of the date cell to that value.  The
 *    value day, month, year should follow the regular written
 *    conventions, i.e. day==(1..31), mon==(1..12), year==4 digits
 *
 * The xaccCommitDateCell() method commits any pending changes to the 
 *    value of the cell.  That is, it will take the cells current string 
 *    value, and parse it into a month-day-year value.
 *
 *    Why is this needed? Basically, while the user is typing into the
 *    cell, we do not even try to parse the date, lest we confuse things
 *    royally.  Normally, when the user leaves this cell, and moves to  
 *    another, we parse the date and print it nicely and cleanly into 
 *    the cell.  But it can happen that we want to get the accurate contentts 
 *    of the date cell before we've left it, e.g. if the user has clicked
 *    on the "commit" button.  This is the troutine to call for that.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
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
void       xaccDestroyDateCell (DateCell *);

void       xaccSetDateCellValue (DateCell *, int day, int mon, int year);  
void       xaccSetDateCellValueSecs (DateCell *, time_t secs);

void       xaccCommitDateCell (DateCell *);

#endif /* __XACC_DATE_CELL_C__ */

/* --------------- end of file ---------------------- */
