/********************************************************************\
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998, 1999, 2000 Linas Vepstas (linas@linas.org)   *
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

/*
 * FILE:
 * FileDialog.h
 *
 * FUNCTION:
 * A set of file-handling utility dialogs for GUI menubars and menubuttons.
 * These utilities will "do the right thing" when used in the "File..."
 * pulldown menu, for the "New", "Open", "Save", "SaveAs", etc. menu entries.
 * In particular, they will verify that old files don't get clobbered,
 * they'll put up dialogue boxes to ask the user to confirm their actions,
 * etc. 
 * 
 * These utilities are written in a GUI-independent fashion, and should
 * work just fine with the Motif, gnome/gtk and Qt interfaces.
 * These utilities are appropriate for direct invocation from guile.
 * (they should be wrapped by g-wrap).
 *
 * These GUI dialogues implement and maintain a single global "session"
 * that defines the currently edited account group.  In a sense, these
 * functions provide the GUI for the xaccSession object.  The session
 * is essentially a file that is open for editing, with locks on it
 * to prevent other readers and writers from accessing it as long as its
 * open.
 *
 *
 * The gncFileSave() routine will check for an existing edit session,
 *    and if one exists, it will save the account info to a file.
 *    If an error occurs, a popup dialogue will inform the user of 
 *    the error.  If there is no existing filename open, then the
 *    user will be prompted for a file to save to (using the
 *    gncFileSaveAs() routine).  The existing session will remain 
 *    open for further editing.
 *
 * The gncFileSaveAs() routine will prompt the user for a filename
 *    to save the account data to (using the standard GUI file dialogue
 *    box).  If the user specifies a filename, the account data will be
 *    saved. If an error occurs, a popup dialogue will inform the user 
 *    of the error.  One possible error is that another user has 
 *    the indicated file already locked up in a different session
 *    (in which case it is up to the user to try again, or to pick
 *    a different filename).  If it is possible to save without 
 *    an error, then a new session is started for the indicated 
 *    filename, locking out other users.  This new session remains
 *    open for further editing.
 *
 * The gncFileQuerySave() routine will display a popup dialog asking
 *    the user if they wish to save their current work. If they answer
 *    "yes", their work will be saved (using the gncFileSave function),
 *    otherwise no action will be performed. If there is no currently
 *    locked session, a popup will query the user for a filename
 *    (using the gncFileSaveAs() routine). The routine will return
 *    TRUE if the user hits "Yes" or "No" and FALSE if the user
 *    hits "Cancel". If nothing needed to be saved, the routine
 *    will return TRUE.
 *
 * The gncFileNew() routine will check for an existing edit session.
 *    If one exists, it will ask the user if they want to save it, 
 *    (using the gncFileQuerySave() dialogue).  Then the current 
 *    session will be destroyed, file locks will be removed, and 
 *    account group structures will be set up for a new session.
 *
 * The gncFileOpen() routine check for an existing edit session.
 *    If one exists, it will ask the user if they want to save it.
 *    (using the gncFileQuerySave() dialogue).  Next, the user will
 *    be prompted with a GUI standard file-selection dialogue to 
 *    to pick a new file.  If no file is picked, this routine returns.
 *    If a new file was picked, then the current session will be 
 *    destroyed and file locks on it will be removed.  The new
 *    file will then be opened for editing, establishing locks, etc.
 *    If an error occurs, the user will be informed with a pop-up
 *    dialogue.  If the file cannot be found, or if a read
 *    error occurs, a popup describing the error will pop up.
 *    One possible error is that another user has the indicated 
 *    file already locked up in a different session (in which 
 *    case it is up to the user to try again, or to pick
 *    a different filename).
 *
 * The gncFileOpenFile() routine behaves much like the gncFileOpen()
 *    routine, except that the new file to open is passed as a char *
 *    argument.
 *
 * The gncFileQIFImport() routine will pop up a standard file selection
 *    dialogue asking the user to pick a QIF file.  If one is selected
 *    the the QIF file is opened and read. It's contents are merged
 *    into the existing session (if any).  The current session continues
 *    to remain open for editing.
 *
 * The gncFileQuit() routine will close out and destroy the current session.
 *    The user WILL NOT BE PROMPTED to confirm this action, or do do
 *    any kind of saving beforehand.
 *
 * The gncGetCurrentGroup() routine will return the account group associated
 *    with the current session.  It will always return a non-null value
 *    (barring system catastrophe such as out of memory).
 *
 * HISTORY:
 * Derived from Rob Clark's original MainWindow.c code, Dec 1998
 */

#ifndef __GNC_FILE_DIALOG_H__
#define __GNC_FILE_DIALOG_H__

#include "config.h"

#include "Group.h"
#include "gnc-book.h"

void gncFileNew (void);
gboolean gncFileOpen (void);
void gncFileQIFImport (void);
void gncFileSave (void);
void gncFileSaveAs (void);

gboolean gncFileOpenFile (const char *filename);

gboolean gncFileQuerySave (void);

void gncFileQuit (void);

AccountGroup *gncGetCurrentGroup (void);

Account *gncGetTemplateAccount(void);

GNCBook *gncGetCurrentBook (void);

#endif /* __GNC_FILE_DIALOG_H__ */
