
#include <stdlib.h>
#include <string.h>

#include "basiccell.h"

/* ===================================================== */

BasicCell * xaccMallocBasicCell (void)
{
   BasicCell * cell;
   cell = (BasicCell *) malloc (sizeof (BasicCell));
   xaccInitBasicCell (cell);
   return cell;
}

/* ===================================================== */

void xaccInitBasicCell (BasicCell *cell)
{
   cell->input_output = 1;
   cell->width = 0;
   cell->alignment = 0;
   cell->value = 0x0;
   cell->set_value = NULL;
   cell->enter_cell = NULL;
   cell->modify_verify = NULL;
   cell->leave_cell = NULL;
   cell->realize = NULL;
   cell->move = NULL;
   cell->destroy = NULL;
   cell->gui_private = NULL;
}

/* ===================================================== */

void xaccSetBasicCellValue (BasicCell *cell, const char *val)
{
   void (*cb) (struct _BasicCell *, const char *);

   cb = cell->set_value;
   if (cb) {
      /* avoid recursion by disabling the  
       * callback while it'sbeing called. */
      cell->set_value = NULL;
      (*cb) (cell, val);
      cell->set_value = cb;
   } else {
      if (cell->value) free (cell->value);
      cell->value = strdup (val);
   }
}

/* ================== end of file ====================== */
