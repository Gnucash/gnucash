/*
 * FILE:
 * FileDialog.h
 *
 * FUNCTION:
 * A set of file-handling utility dialogs for GUI menubars and menubuttons.
 * These utilities will "do the right thing" when used in the "File..."
 * pulldown menu, for the "New", "Open", "Save", "SaveAs", etc. menu entires.
 * In particular, they will verify that old files don't get clobbered,
 * they'll put up dialogue boxes to ask the user to confirm thier actions,
 * etc. 
 * 
 * These utilities are written in a GUI-independent fashion, and should
 * work just fine with the Motif, gnome/gtk and Qt interfaces.
 * These utilities are appropriate for direct invocation from guile.
 * (they should be wrapped by swig).
 *
 * HISTORY:
 * Derived from Rob Clark's original MainWindow.c code, Dec 1998
 */

/********************************************************************\
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __GNC_FILE_DIALOG_H__
#define __GNC_FILE_DIALOG_H__

#include "Group.h"
#include "Session.h"
      
void gncFileNew (void);
void gncFileOpen (void);
void gncFileQIFImport (void);
void gncFileSave (void);
void gncFileSaveAs (void);

void gncFileOpenFile (const char *);

void gncFileQuerySave (void);
void gncFileQuit (void);

extern Session *current_session;
extern AccountGroup *topgroup;

#endif /* __GNC_FILE_DIALOG_H__ */
