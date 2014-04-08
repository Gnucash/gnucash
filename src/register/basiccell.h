/*
 * FILE:
 * basiccell.h
 *
 * FUNCTION:
 * The BasicCell class provides an abstract base class  
 * defining the handling of the editing of a cell of a table.
 * Classes that provide the actual handling for different
 * cell types should inherit from this class.
 *
 * The BasicCell class encapsulates a single string value
 * which can be set & read by the programmer, and edited 
 * by the "user".  In the text below, the "user" is the 
 * person controlling the mouse and keyboard.  Thus, when 
 * the user makes a move, it means that they have somehow 
 * intereacted with the cell, by clikcing with mouse or by 
 * typing at the keyboard.  This class provides three 
 * callbacks which allow the programmer to understand what 
 * the user is doing.
 *
 * The programmer can create a custom GUI for editing the
 * contents of the cell. There are three callbacks to allow
 * a custom GUI to be created, destroyed and moved about.
 *
 * Since this class is implemented in C not C++, there is
 * a "minor" problem with inheritance.  To emulate the 
 * overloading of a virtual "SetValues" method, there is 
 * a set_value() callback, which will be called whenever
 * the xaccSetBasicCellValue() subroutine is called.
 *
 * VIRTUAL/OVERLOADED METHODS:
 * The set_value() callback will be called whenever the
 * xaccSetBasicCellValue() method is called.  Derived 
 * classes should provide a callback here if they need
 * to understand special cell formats.
 *
 * MEMBERS:
 * The input_output member controls how the cell accepts 
 *   input, and whether it displays it's value.  It is a 
 *   a flag of OR-ed together values. Flag bits include:
 *
 *     XACC_CELL_ALLOW_INPUT  accept keyboard & mouse
 *       input from the user. 
 *     XACC_CELL_ALLOW_SHADOW copy ("shadow") the contents
 *       of register cells.   
 *
 *   If ALLOW_INPUT is not set, the cell is supposed  to
 *   to only display values, but not accept user input.  If
 *   set, then the callbacks below are used to when the
 *   cell is entered.
 *
 *   If ALLOW_SHADOW is not set, then register contents
 *   will never be copied into this cell.  This may result
 *   in undesirable behaviour when the cell is moved around,
 *   as the cell contents will be imprinted onto every location 
 *   where it is moved.  On the other hand, this can be useful
 *   for, e.g. blanking out cells.
 *
 *
 * USER CALLBACKS:
 * The enter_cell() callback is called when the user first
 *    makes a move to enter a cell.  This might be by clicking
 *    on the cell with the mouse, by tabbing to it, using the 
 *    arrow keys, or otherwise "selecting" it as the current
 *    cell to edit.  
 *
 *    The current value of the cell is passed as the argument.  
 *    If the callback wishes to change the value of the cell, 
 *    it can return a non-null string.  Alternately, to leave 
 *    the value of the cell unchanged, it can return NULL.  
 *    If a string is returned, the string must be as the result 
 *    of a malloc.
 *
 * The leave_cell() callback is called when the user exits
 *    a cell.  This can be by tabbing or arrow-keying away 
 *    from it, or by using the mouse to specify a different 
 *    cell, etc.  The current value of the cell is passed as the 
 *    argument.  If the callback wishes to change the value of 
 *    the cell, it can return a non-null string.  Alternately, 
 *    to leave the value of the cell unchanged, it can return 
 *    NULL.  If a string is returned, the string must be as the 
 *    result of a malloc.
 *
 * The modify_verify() callback is called when a user makes a
 *    change to a cell.  It is called after every keystroke,
 *    (actually, after every X11 "input-method" type input,
 *    so that ctrl-alt-etc modifier keys are pre-processed in 
 *    the usual X11 fashion).
 *    
 *    The three arguments passed in are :
 *    "old", the string prior to user's attempted modification,
 *    "add", the string the user is attemptiong to add
 *           (will be null if text is being deleted).
 *    "new", the string that would result is user's changes
 *           are accepted.
 *    It must return a string, or void if it rejects the change.
 *    The returned string will be used to update the cell value.
 *
 * Some memory management rules:
 * (1) the callback must not modify the values of old, change, new
 * (2) if the callback likes the new string, it may return the
 *     pointer to "new".  It must *not* return the pointer to 
 *     "change" or "old"
 * (3) if the callback chooses to not return "new", it must 
 *     malloc the memory for a new string.  It does not need
 *     to worry about garbage collection.
 *
 *
 * GUI CALLBACKS:
 * The cell may have some specific GUI elements which need
 * to be initialized/positioned/etc.  There are three GUI
 * callbacks that allow the programmer to perform GUI-specific
 * initialization & changes.
 *
 * The realize() callback will be called when GUI-specific 
 *    initalization needs to be done.  For Xt/Motif, the second
 *    argument will be cast to the parent widget.  The third
 *    argument passes in the desired pixel-width for the GUI 
 *    element.  (Yes, the pixel-size thing is a hack that we
 *    allow for the moment. See below for more info.)
 *
 * The destroy() callback will be called when the GUI associated
 *    with the cell needs to be destroyed.
 *
 * The move() callback will be called when the GUI element needs
 *    to be positioned to a new location within the table grid.
 *    The second and third arguments are the physical (not virtual)
 *    row and column that the GUI elemnt should be moved to.
 *
 * The gui_private member may be used by the derived class to 
 *    store any additional GUI-specific data.
 *
 * GUI HACK ALERT NOTES:
 * The realize method takes a width argument only as a hack
 * to work around the fact that the combo-box requires a width
 * in pixels, rather than in characters.  It would be nice if 
 * ComboBox supported the XmNunits resource, but it doesn't.  
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */

/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __XACC_BASIC_CELL_H__
#define __XACC_BASIC_CELL_H__

/* define a bitmask */
#define XACC_CELL_ALLOW_NONE     0x0
#define XACC_CELL_ALLOW_SHADOW   0x1
#define XACC_CELL_ALLOW_INPUT    0x2
#define XACC_CELL_ALLOW_ALL      0x3

typedef struct _BasicCell BasicCell;
typedef unsigned int uint32;

struct _BasicCell {

  short  width;           /* column width, in chars, not pixels */
  short  alignment;       /* column text alignment */
  char   input_output;    /* zero if output-only */

  /* hack alert -- may want to redesign color to used named color strings. */
  uint32 bg_color;        /* background color, ARGB format */
  uint32 fg_color;        /* forground (text) color ARGB format */

  /* hack alert -- add support for e.g. bold fonts !?!?! italic fonts ?? */

  char * value;          /* current value */
  unsigned int changed;  /* 2^32-1 if value modified */

  /* "virtual", overloaded set-value method */
  void         (*set_value)     (BasicCell *,
                                 const char * new_value);

  /* cell-editing callbacks */
  const char * (*enter_cell)    (BasicCell *,
                                 const char * current);
  const char * (*modify_verify) (BasicCell *,
                                 const char *old, 
                                 const char *add, 
                                 const char *new); 
  const char * (*leave_cell)    (BasicCell *,
                                 const char * current);

  /* private, GUI-specific callbacks */
  void         (* realize) (BasicCell *, 
                            void *gui_handle,
                            int pixel_width);
  void         (* move)    (BasicCell *, 
                            int phys_row, int phys_col);
  void         (* destroy) (BasicCell *);

  /* general hook for gui-private data */
  void * gui_private;
};


BasicCell * xaccMallocBasicCell (void);
void         xaccInitBasicCell (BasicCell *);
void         xaccDestroyBasicCell (BasicCell *);

void         xaccSetBasicCellValue (BasicCell *, const char *);

#endif /* __XACC_BASIC_CELL_H__ */
/* ------------------ end of file ---------------------- */
