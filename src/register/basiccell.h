/********************************************************************\
 * basiccell.h -- base class for editable cell in a table           *
 *                                                                  *
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
 * USER CALLBACKS:
 * The enter_cell() callback is called when the user first
 *    makes a move to enter a cell.  This might be by clicking
 *    on the cell with the mouse, by tabbing to it, using the 
 *    arrow keys, or otherwise "selecting" it as the current
 *    cell to edit.  
 *
 *    The callback may change the value of the cell. The callback
 *    should return true if the cell should allow direct editing
 *    by the user, FALSE otherwise.
 *
 *    The callback is also passed pointers to the cursor position
 *    and the start and end of the highlited region. If the callback
 *    returns NULL, it may also change these values and the GUI will
 *    update appropriately.
 *
 * The leave_cell() callback is called when the user exits
 *    a cell.  This can be by tabbing or arrow-keying away 
 *    from it, or by using the mouse to specify a different 
 *    cell, etc. The callback may change the value of the cell.
 *
 * The modify_verify() callback is called when a user makes a
 *    change to a cell.  It is called after every keystroke,
 *    (actually, after every X11 "input-method" type input,
 *    so that ctrl-alt-etc modifier keys are pre-processed in 
 *    the usual X11 fashion).
 *    
 *    The arguments passed in are :
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
 *
 * The direct_update() callback is called to pass raw gui data
 *    to the cell. The exact format of the data is determined
 *    by the gui. The callback should return TRUE if the event
 *    was handled, i.e., there is no need to call the modify
 *    update. If the value needs to be changed, the cell should
 *    go ahead and change it.
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
 *    The second argument is the virtual location the GUI
 *    element should be moved to.
 *
 * The gui_private member may be used by the derived class to 
 *    store any additional GUI-specific data.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas <dave@krondo.com>
 */

#ifndef BASIC_CELL_H
#define BASIC_CELL_H

#include <gdk/gdk.h>
#include <glib.h>

#include "gnc-common.h"
#include "gnc-ui-common.h"
#include "register-common.h"


#define GNC_CELL_CHANGED 0xffffffff

typedef struct _BasicCell BasicCell;

typedef void (*CellSetValueFunc) (BasicCell *cell,
                                  const char * new_value);

typedef gboolean (*CellEnterFunc) (BasicCell *cell,
                                   int *cursor_position,
                                   int *start_selection,
                                   int *end_selection);

typedef void (*CellModifyVerifyFunc) (BasicCell *cell,
                                      const GdkWChar *add_str,
                                      int add_str_len,
                                      const GdkWChar *new_value,
                                      int new_value_len,
                                      int *cursor_position,
                                      int *start_selection,
                                      int *end_selection);

typedef gboolean (*CellDirectUpdateFunc) (BasicCell *cell,
                                          int *cursor_position,
                                          int *start_selection,
                                          int *end_selection,
                                          gpointer gui_data);

typedef void (*CellLeaveFunc) (BasicCell *cell);

typedef void (*CellRealizeFunc) (BasicCell *cell, gpointer gui_handle);

typedef void (*CellMoveFunc) (BasicCell *cell, VirtualLocation virt_loc);

typedef void (*CellDestroyFunc) (BasicCell *cell);

typedef char * (*CellGetHelpFunc) (BasicCell *cell);

struct _BasicCell
{
  char * value;                  /* current value */
  char * blank_help;             /* help when value is blank */

  GdkWChar * value_w;            /* value as wide chars */

  gint value_len;                /* length of wide chars value */

  guint32 changed;               /* 2^32-1 if value modified */
  guint32 conditionally_changed; /* value if modified conditionally */

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

  /* GUI flag indicated is a popup-widget */
  gboolean is_popup;

  /* general hook for gui-private data */
  gpointer gui_private;
};


BasicCell *  xaccMallocBasicCell (void);
void         xaccInitBasicCell (BasicCell *bcell);
void         xaccDestroyBasicCell (BasicCell *bcell);

void         xaccSetBasicCellValue (BasicCell *bcell, const char *value);

void         xaccSetBasicCellBlankHelp (BasicCell *bcell, const char *help);
char *       xaccBasicCellGetHelp (BasicCell *bcell);

void         xaccBasicCellSetChanged (BasicCell *bcell, gboolean changed);

/* for sub-class use only */
void         xaccSetBasicCellValueInternal (BasicCell *bcell,
                                            const char *value);

void         xaccSetBasicCellWCValueInternal (BasicCell *bcell,
                                              const GdkWChar *value);

/* helper function, allocates new wide char string for conversion */
gint         gnc_mbstowcs (GdkWChar **dest_p, const char *src);
char *       gnc_wcstombs (const GdkWChar *src);
gint         gnc_wcslen   (const GdkWChar *src);
GdkWChar *   gnc_wcsdup   (const GdkWChar *src);

#endif /* BASIC_CELL_H */
