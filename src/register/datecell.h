
#ifndef __XACC_DATE_CELL_C__
#define __XACC_DATE_CELL_C__

#include <time.h>
#include "single.h"

typedef struct _DateCell {
   SingleCell cell;
   struct tm date;
} DateCell;

/* installs a callback to handle date recording */
DateCell * xaccMallocDateCell (void);
void         xaccInitDateCell (DateCell *);

#endif /* __XACC_DATE_CELL_C__ */

/* --------------- end of file ---------------------- */
