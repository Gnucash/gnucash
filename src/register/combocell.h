/*
 * FILE:
 * combocell.h
 *
 * FUNCTION:
 * Implements a combobox cell
 *
 * HISTORY:
 * Created Jan 1998 Linas Vepstas 
 * Copyright (c) 1998 Linas Vepstas 
 */

#ifndef __XACC_COMBO_CELL_C__
#define __XACC_COMBO_CELL_C__

#include "basiccell.h"

typedef struct _ComboCell {
   BasicCell cell;
   char **   menuitems;
} ComboCell;

ComboCell *  xaccMallocComboCell (void);
void         xaccInitComboCell (ComboCell *);
void         xaccSetComboCellValue (ComboCell *, const char *);

void         xaccAddComboCellMenuItem (ComboCell *, char * menustr);

#endif /* __XACC_COMBO_CELL_C__ */

/* --------------- end of file ---------------------- */
