
#include <string.h>

#include "basiccell.h"
#include "quickfillcell.h"

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static const char * 
quickMV (struct _BasicCell *_cell,
        const char *oldval, 
        const char *change, 
        const char *newval)
{
   return newval;
}

/* ================================================ */

QuickFillCell *
xaccMallocQuickFillCell (void)
{
   QuickFillCell *cell;
   cell = xaccMallocQuickFillCell();
   xaccInitBasicCell (&(cell->cell));

   cell->qf = xaccMallocQuickFill();
   return cell;
}

/* ================================================ */

void
xaccInitQuickFillCell (QuickFillCell *cell)
{
  cell->cell.modify_verify = quickMV;
}

/* =============== END OF FILE ==================== */
