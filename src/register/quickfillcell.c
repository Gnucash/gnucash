
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
   QuickFillCell *cell = (QuickFillCell *) _cell;
   char * retval = (char *) newval;

   /* if user typed the very first letter into this
    * cell, then make sure that the quick-fill is set to 
    * the root.  Alternately, if user erased all of the 
    * text in the cell, and has just started typing,
    * then make sure that the quick-fill root is also reset
    */
   if (newval) {
      if ((0x0 != newval[0]) && (0x0 == newval[1])) {
         cell->qf = cell->qfRoot;
      }
   }

   /* if change is null, then user is deleting text;
    * otehrwise, they are inserting text. */
   if (change) {
      int i;
      char c;
      
      /* search for best-matching quick-fill string */
      i=0;
      c = change[i];
      while (c) {
         cell->qf = xaccGetQuickFill (cell->qf, c);
         i++;
         c = change[i];
      }

      /* if a match found, return it */
      if (cell->qf) retval = strdup (cell->qf->text);
   }

   xaccSetBasicCellValue (&(cell->cell), retval);
   return retval;
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
   cell = ( QuickFillCell *) malloc (sizeof (QuickFillCell));

   xaccInitQuickFillCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitQuickFillCell (QuickFillCell *cell)
{
   xaccInitBasicCell (&(cell->cell));

   cell->qfRoot = xaccMallocQuickFill();
   cell->qf = cell->qfRoot;

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
