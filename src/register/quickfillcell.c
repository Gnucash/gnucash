
#include <string.h>

#include "basiccell.h"
#include "quickfillcell.h"

/* ================================================ */
/* when entering new cell, reset pointer to root    */

static const char * 
quick_enter (struct _BasicCell *_cell,
             const char *val) 
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   cell->qf = cell->qfRoot;
   return val;
}

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static const char * 
quick_modify (struct _BasicCell *_cell,
        const char *oldval, 
        const char *change, 
        const char *newval)
{
   
printf ("change is %s \n", change);
   if (change) {
   } 

   return newval;
}

/* ================================================ */
/* when leaving cell, make sure that text was put into the qf    */

static const char * 
quick_leave (struct _BasicCell *_cell,
             const char *val) 
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   cell->qf = cell->qfRoot;
   xaccQFInsertText (cell->qfRoot, val);
   return val;
}

/* ================================================ */

QuickFillCell *
xaccMallocQuickFillCell (void)
{
   QuickFillCell *cell;
   cell = xaccMallocQuickFillCell();
   xaccInitBasicCell (&(cell->cell));

   cell->qfRoot = xaccMallocQuickFill();
   cell->qf = cell->qfRoot;
   return cell;
}

/* ================================================ */

void
xaccInitQuickFillCell (QuickFillCell *cell)
{
  cell->cell.enter_cell    = quick_enter;
  cell->cell.modify_verify = quick_modify;
  cell->cell.leave_cell    = quick_leave;
}

/* ================================================ */

void
xaccSetQuickFillCellValue (QuickFillCell *cell, char * value)
{
   xaccQFInsertText (cell->qfRoot, value);
   xaccSetBasicCellValue (&(cell->cell), value);
}

/* =============== END OF FILE ==================== */
