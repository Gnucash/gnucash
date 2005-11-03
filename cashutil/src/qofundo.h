/***************************************************************************
 *            qofundo.h
 *
 *  Thu Aug 25 09:19:25 2005
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
 
 /** @addtogroup UNDO Undo: track and undo or redo entity changes
 @ingroup QOFCLI

\b EXPERIMENTAL!

 QOF Undo operates within a QofBook. In order to undo the changes to
 the entity, the initial state of each parameter is cached when an operation
 begins. If the entity changes are not successful, the lack of a
 ::qof_book_end_operation call before a ::qof_book_start_operation will cause
 the cached data to be freed. If the entity is changed successfully,
 ::qof_book_end_operation will create the undo data using the operation
 label and each of the entity changes that were successful.
 
 Undo data consists of a list of operations that have changed data in this book and a
 list of entity changes for each of those operations. Each operation can relate to
 more than one entity change and cover more than one entity but must only relate to
 one book.
 
-# Only QOF parameter changes can be undone or redone. Data from structs that
 are not QOF objects or which have no QofParam to get <b>and set</b> the data
 will not be available to the undo process.
-# Undo relates to 'user interface operations', not engine events. This is
because an operation (like an import or voiding a transaction) can involve
multiple, possibly conflicting, engine events - e.g. removing an entity from one
reference and inserting it as another. Therefore, the UI developer alone can
decide where an operation begins and ends. All changes between the two will be
undone or redone in one call to qof_book_undo.
-# Undo operations \b cannot be nested. Be careful where you start and end an undo operation,
if your application calls qof_book_start_operation() before calling qof_book_end_operation(),
the undo cache will be freed and QOF Undo will not notify you of this. The API is designed to
silently handle user aborts during a user operation. As undo data is cached as soon as editing
begins, if the edit is never completed the cache must be cleared before the next operation.
i.e. if the user starts to edit an entity but then cancels the operation, there are no changes
to undo. It follows that any one book can only be the subject of one operation at a time.

\todo Change operations to return a handler that can distinguish each operation then
make it: QofOperation qof_book_start_operation(QofBook* book, char *label) and
void qof_book_end_operation(QofOperation oper);

@{
 */
/** @file  qofundo.h
    @brief Experimental QOF undo handling
	@author Copyright (c) 2005  Neil Williams <linux@codehelp.co.uk>
*/
#ifndef _QOFUNDO_H
#define _QOFUNDO_H

/** @brief The parameter changes, >=1 per affected entity 

One per parameter change - the bottom level of any undo. A single click of 
Undo could use the data from one or many parameter changes - as determined by 
the event. Each parameter change can be for any entity of any registered type 
in the book and parameter changes can be repeated for the multiple changes to 
different parameters of the same entity. The combination of param, guid and 
type will be unique per event. (i.e. no event will ever set the same 
parameter of the same entity twice (with or without different data) in one 
undo operation.)
*/
typedef struct qof_undo_entity_t qof_undo_entity;

/** @brief The affected entities, >=1 per operation 

The top level of any undo. Contains a GList that keeps the type of operation and 
the GList of qof_undo_entity* instances relating to that operation. Some form of 
index / counter probably too in order to speed up freeing unwanted operations and 
undo data upon resumption of editing and in controlling the total number of 
operations that can be undone.

Each qof_undo_event.entity_list can contain data about >1 type of entity.
*/
typedef struct qof_undo_operation_t qof_undo_operation;

/** \brief Set a value in this parameter of the entity.

Setting an arbitrary parameter in an entity can involve
repetitive string comparisons and setter function prototypes.
This function accepts a QofParam (which determines the type of 
value) and a string representing the value. e.g. for a boolean,
pass "TRUE", for a GUID pass the result of guid_to_string_buff.

It's a convenience wrapper for routines that take values from 
files (e.g. XML) and need to convert into real data in the entity.

@param ent An initialized QofEntity from an accessible QofBook.
@param param The QofParam that needs to be set, including the 
get_fcn, set_fcn, param_type and param_name.
@param value A string representation of the required value - original
type as specified in param->param_type.

*/
void qof_entity_set_param(QofEntity *ent, QofParam *param, char *value);

/** @brief Set parameter values from before the previous event. */
void qof_book_undo(QofBook *book);

/** @brief Set parameter values from after the previous event. */
void qof_book_redo(QofBook *book);

/** @brief event handler for undo widget 

 @return FALSE if length == 0 or index_position == 0,
 otherwise TRUE.
*/
gboolean qof_book_can_undo(QofBook *book);

/** @brief event handler for redo widget

@return FALSE if index_position == 0 or index_position == length
otherwise TRUE.
*/
gboolean qof_book_can_redo(QofBook *book);

/** \brief Start recording operation.

*/
void qof_book_start_operation(QofBook *book, char *label);

/** \brief End recording the current operation. */
void qof_book_end_operation(QofBook *book);

/** \brief HIG compliance aid to report time of first change. */
Timespec qof_book_undo_first_modified(QofBook *book);

/** \brief Number of undo operations available. */
gint qof_book_undo_count(QofBook *book);

#endif /* _QOFUNDO_H */

/** @} */
