/***************************************************************************
 *            qofundo-p.h
 *
 *  Thu Aug 25 09:20:14 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
 
#ifndef _QOFUNDO_P_H
#define _QOFUNDO_P_H

#include "qof.h"
#include "qofundo.h"

#ifdef __cplusplus
extern "C"
{
#endif

/* Undo is limited, not infinite. */
#define MAX_UNDO_LENGTH     300

/* Free the entire undo list for this book. */
void qof_book_clear_undo(QofBook *book);

/* reads the data from this parameter to allow undo

To be able to undo and then redo an action, QOF needs to know the
before and after states. Initially, the before state is the same as
the file but after that point, the state of the entity needs to be
tracked whenever it is opened for editing.
*/
qof_undo_entity* qof_prepare_undo (QofEntity *ent, QofParam *param);

/* Add the changes to be undone to the event.

Designed to be used with g_list_foreach, simply adds
any number of undo_entity pointers (representing the 
entity changes relating to this event) to the list 
of changes for this event.
*/
void qof_undo_new_entry(gpointer event, gpointer changes);

/* Add an undo event to the list.

type holds the type of event that has just occurred.

If the event follows a successful qof_commit_edit, then the
cached undo_entity changes are placed into this undo_event.
*/
qof_undo_operation* qof_undo_new_operation(char* label);

/* dummy routines for testing only */
void undo_edit_record(QofInstance *inst, QofParam *param);
void undo_edit_commit(QofInstance *inst, QofParam *param);
void undo_create_record(QofInstance *inst);
void undo_delete_record(QofInstance *inst);

#ifdef __cplusplus
}
#endif

#endif /* _QOFUNDO_P_H */
