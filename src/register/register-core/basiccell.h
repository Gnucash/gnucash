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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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
 * The gui_realize() callback will be called when GUI-specific
 *    initialization needs to be done. For Gnome, the second
 *    argument will be cast to the parent widget.
 *
 * The gui_destroy() callback will be called when the GUI associated
 *    with the cell needs to be destroyed.
 *
 * The gui_move() callback will be called when the GUI element needs
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

#include "gnc-ui-common.h"


typedef struct basic_cell BasicCell;

typedef BasicCell * (*CellCreateFunc) (void);

typedef void (*CellSetValueFunc) (BasicCell *cell,
                                  const char * new_value);

typedef gboolean (*CellEnterFunc) (BasicCell *cell,
                                   int *cursor_position,
                                   int *start_selection,
                                   int *end_selection);

typedef void (*CellModifyVerifyFunc) (BasicCell *cell,
                                      const char *add_str,
                                      int add_str_len,
                                      const char *new_value,
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

typedef void (*CellMoveFunc) (BasicCell *cell);

typedef void (*CellDestroyFunc) (BasicCell *cell);

typedef enum
{
    CELL_ALIGN_RIGHT,
    CELL_ALIGN_CENTER,
    CELL_ALIGN_LEFT
} CellAlignment;

struct basic_cell
{
    char * cell_name;
    gchar *cell_type_name;
    char * value;                  /* current value */
    guint value_chars;           /* number of characters in value */

    gboolean changed;               /* true if value modified */
    gboolean conditionally_changed; /* true if value modified conditionally */

    /* "virtual", overloaded methods */
    CellSetValueFunc set_value;
    CellDestroyFunc  destroy;

    /* cell-editing callbacks */
    CellEnterFunc        enter_cell;
    CellModifyVerifyFunc modify_verify;
    CellDirectUpdateFunc direct_update;
    CellLeaveFunc        leave_cell;

    /* private, GUI-specific callbacks */
    CellRealizeFunc gui_realize;
    CellMoveFunc    gui_move;
    CellDestroyFunc gui_destroy;

    /* GUI information */
    char *sample_text;       /* sample text for sizing purposes */
    CellAlignment alignment; /* horizontal alignment in column */
    gboolean expandable;     /* can fill with extra space */
    gboolean span;           /* can span multiple columns */
    gboolean is_popup;       /* is a popup widget */

    /* general hook for gui-private data */
    gpointer gui_private;
};


gboolean     gnc_cell_name_equal (const char * cell_name_1,
                                  const char * cell_name_2);

BasicCell *  gnc_basic_cell_new (void);
void         gnc_basic_cell_init (BasicCell *bcell);
void         gnc_basic_cell_destroy (BasicCell *bcell);

void         gnc_basic_cell_set_name (BasicCell *cell, const char *name);
gboolean     gnc_basic_cell_has_name (BasicCell *cell, const char *name);
void         gnc_basic_cell_set_type_name (BasicCell *cell, const gchar *type_name);
gboolean     gnc_basic_cell_has_type_name (BasicCell *cell, const gchar *type_name);



void         gnc_basic_cell_set_sample_text (BasicCell *cell,
        const char *sample_text);
void         gnc_basic_cell_set_alignment (BasicCell *cell,
        CellAlignment alignment);
void         gnc_basic_cell_set_expandable (BasicCell *cell,
        gboolean expandable);
void         gnc_basic_cell_set_span (BasicCell *cell,
                                      gboolean span);

const char * gnc_basic_cell_get_value (BasicCell *cell);
void         gnc_basic_cell_set_value (BasicCell *bcell, const char *value);

gboolean     gnc_basic_cell_get_changed (BasicCell *cell);
gboolean     gnc_basic_cell_get_conditionally_changed (BasicCell *cell);

void         gnc_basic_cell_set_changed (BasicCell *cell, gboolean changed);
void         gnc_basic_cell_set_conditionally_changed (BasicCell *cell,
        gboolean changed);

/* for sub-class use only */
void         gnc_basic_cell_set_value_internal (BasicCell *bcell,
        const char *value);

#endif /* BASIC_CELL_H */
