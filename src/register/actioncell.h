/*
 * FILE:
 * actioncell.h
 *
 * FUNCTION:
 * Implements a actionbox cell
 *
 * HISTORY:
 * Created Jan 1998 Linas Vepstas 
 * Copyright (c) 1998 Linas Vepstas 
 */

#ifndef __XACC_ACTION_CELL_C__
#define __XACC_ACTION_CELL_C__

#include "combocell.h"

typedef struct _ActionCell {
   ComboCell cell;
   void  (* chain_realize) (struct _BasicCell *, void *gui_handle);
   void  (* chain_destroy) (struct _BasicCell *);
} ActionCell;

ActionCell * xaccMallocActionCell (void);
void         xaccInitActionCell (ActionCell *);

#endif /* __XACC_ACTION_CELL_C__ */

/* --------------- end of file ---------------------- */
