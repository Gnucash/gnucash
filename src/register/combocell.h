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
   struct _PopBox *gui;   /* gui-private data */
} ComboCell;

ComboCell *  xaccMallocComboCell (void);
void         xaccInitComboCell (ComboCell *);

#endif /* __XACC_COMBO_CELL_C__ */

/* --------------- end of file ---------------------- */
