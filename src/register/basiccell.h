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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

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
 * interacted with the cell, by clicking with mouse or by 
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
 *   input, and whether it displays its value.  It is a 
 *   a flag of OR-ed together values. Flag bits include:
 *
 *     XACC_CELL_ALLOW_INPUT  accept keyboard & mouse
 *       input from the user. 
 *     XACC_CELL_ALLOW_SHADOW copy ("shadow") the contents
 *       of register cells.
 *     XACC_CELL_ALLOW_EXACT_ONLY the cell may only be
 *       entered by 'exact' selection, i.e., not by
 *       indirect selection by, for example, tabbing.
 *       Currently, the only exact method of entering
 *       a cell is via the mouse pointer.
 *
 *   If ALLOW_INPUT is not set, the cell is supposed to
 *   to only display values, but not accept user input. If
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
 *    The callback is also passed pointers to the cursor position
 *    and the start and end of the highlited region. If the callback
 *    returns NULL, it may also change these values and the GUI will
 *    update appropriately.
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
 *    The arguments passed in are :
 *    "old", the string prior to user's attempted modification,
 *    "add", the string the user is attempting to add
 *           (will be null if text is being deleted).
 *    "new", the string that would result is user's changes
 *           are accepted.
 *    "cursor_position", the position of the editing cursor
 *                       in the text. This may be modified by
 *                       the callback, in which case the GUI
 *                       will reflect the change. Set to -1
 *                       to make the cursor go to the end of
 *                       the text.
 *    "start_selection", the starting character of the highlited
 *                       selection.
 *    "end_selection",   the index immediately after the last
 *                       character in the selection. Set both
 *                       start and end to 0 for no selection.
 *                       Set the end to -1 to make the selection
 *                       go to the end of the text.
 *    It must return a string, or void if it rejects the change.
 *    The returned string will be used to update the cell value.
 *
 * The direct_update() callback is called to pass raw gui data
 *    to the cell. The exact format of the data is determined
 *    by the gui. The callback should return TRUE if the event
 *    was handled, i.e., there is no need to call the modify
 *    update. If the value needs to be changed, the newval_ptr
 *    should be set to a malloc'd new value. The other arguments
 *    work as above.
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
 *    initialization needs to be done. For Gnome, the second
 *    argument will be cast to the parent widget.
 *
 * The destroy() callback will be called when the GUI associated
 *    with the cell needs to be destroyed.
 *
 * The move() callback will be called when the GUI element needs
 *    to be positioned to a new location within the table grid.
 *    The second and third arguments are the physical (not virtual)
 *    row and column that the GUI element should be moved to.
 *
 * The gui_private member may be used by the derived class to 
 *    store any additional GUI-specific data.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas <dave@krondo.com>
 */

#ifndef __BASIC_CELL_H__
#define __BASIC_CELL_H__

#include <glib.h>

#include "gnc-common.h"
#include "gnc-ui-common.h"


/* define a bitmask */
#define XACC_CELL_ALLOW_NONE       0x0
#define XACC_CELL_ALLOW_SHADOW     0x1
#define XACC_CELL_ALLOW_INPUT      0x2
#define XACC_CELL_ALLOW_ALL        0x3
#define XACC_CELL_ALLOW_EXACT_ONLY 0x4

#define GNC_CELL_CHANGED 0xffffffff

typedef struct _BasicCell BasicCell;

typedef void (*CellSetValueFunc) (BasicCell *,
                                  const char * new_value);

typedef const char * (*CellEnterFunc) (BasicCell *,
                                       const char * current,
                                       int *cursor_position,
                                       int *start_selection,
                                       int *end_selection);

typedef const char * (*CellModifyVerifyFunc) (BasicCell *,
                                              const char *old_value, 
                                              const char *add_str, 
                                              const char *new_value,
                                              int *cursor_position,
                                              int *start_selection,
                                              int *end_selection);

typedef gboolean (*CellDirectUpdateFunc) (BasicCell *,
                                          const char *oldval,
                                          char **newval_ptr,
                                          int *cursor_position,
                                          int *start_selection,
                                          int *end_selection,
                                          void *gui_data);

typedef const char * (*CellLeaveFunc) (BasicCell *,
                                       const char * current);

typedef void (*CellRealizeFunc) (BasicCell *, 
                                 void *gui_handle);

typedef void (*CellMoveFunc) (BasicCell *,
                              int phys_row, int phys_col);

typedef void (*CellDestroyFunc) (BasicCell *);

typedef char * (*CellGetHelpFunc) (BasicCell *);

struct _BasicCell
{
  /* cell attributes */
  /* hack alert -- may want to redesign color to use named color strings. */
  guint32 bg_color;       /* background color, ARGB format */
  guint32 fg_color;       /* forground (text) color ARGB format */

  gboolean use_fg_color;  /* if 0, above is ignored */
  gboolean use_bg_color;  /* if 0, above is ignored */

  char * value;          /* current value */
  char * blank_help;     /* help when value is blank */

  guint32 changed;       /* 2^32-1 if value modified */
  guint8  input_output;  /* zero if output-only */

  /* "virtual", overloaded set-value method */
  CellSetValueFunc set_value;

  /* cell-editing callbacks */
  CellEnterFunc        enter_cell;
  CellModifyVerifyFunc modify_verify;
  CellDirectUpdateFunc direct_update;
  CellLeaveFunc        leave_cell;

  /* private, GUI-specific callbacks */
  CellRealizeFunc realize;
  CellMoveFunc    move;
  CellDestroyFunc destroy;

  CellGetHelpFunc get_help_value;

  /* general hook for gui-private data */
  void * gui_private;
};


BasicCell *  xaccMallocBasicCell (void);
void         xaccInitBasicCell (BasicCell *);
void         xaccDestroyBasicCell (BasicCell *);

void         xaccSetBasicCellValue (BasicCell *, const char *);
void         xaccSetBasicCellBlankHelp (BasicCell *, const char *);
char *       xaccBasicCellGetHelp (BasicCell *);

void         xaccBasicCellSetChanged (BasicCell *, gncBoolean);

#endif /* __BASIC_CELL_H__ */
/* ------------------ end of file ---------------------- */
