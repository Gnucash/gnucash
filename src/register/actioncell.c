
#include "actioncell.h"

static void realizeAction (struct _BasicCell *bcell, void *w);
static void destroyAction (struct _BasicCell *bcell);

/* =============================================== */

ActionCell *xaccMallocActionCell (void)
{
   ActionCell * cell;
   cell = (ActionCell *) malloc (sizeof (ActionCell));
   xaccInitActionCell (cell);
   return cell;
}

void xaccInitActionCell (ActionCell *cell)
{
   xaccInitComboCell ( &(cell->cell));
   cell->chain_realize = cell->cell.cell.realize;
   cell->cell.cell.realize = realizeAction;
}

/* =============================================== */

static
void realizeAction (struct _BasicCell *bcell, void *w)
{
   ActionCell *cell = (ActionCell *) bcell;

   /* first, call the combobox realize */
   cell->cell.cell.realize = cell->chain_realize;
   if (cell->chain_realize) {
      (cell->chain_realize) (bcell, w);
   }

   /* now, install our destroy */
   cell->chain_destroy = cell->cell.cell.destroy;
   cell->cell.cell.destroy = destroyAction;

   /* finally, add menu items */
   xaccAddComboCellMenuItem ( &(cell->cell), "yo dude");
   xaccAddComboCellMenuItem ( &(cell->cell), "he haw");
}

/* =============================================== */

static
void destroyAction (struct _BasicCell *bcell)
{
   ActionCell *cell = (ActionCell *) bcell;

   /* first, call the combobox destroy */
   cell->cell.cell.destroy = cell->chain_destroy;
   if (cell->chain_destroy) {
      (cell->chain_destroy) (bcell);
   }

   /* now, install our realize */
   cell->chain_realize = cell->cell.cell.realize;
   cell->cell.cell.realize = realizeAction;
}

/* =============== end of file =================== */
