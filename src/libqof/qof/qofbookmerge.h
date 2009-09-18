/*********************************************************************
 * qofbookmerge.h -- api for QofBook merge with collision handling   *
 * Copyright (C) 2004 Neil Williams <linux@codehelp.co.uk>           *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation; either version 2 of    *
 * the License, or (at your option) any later version.               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License for more details.                      *
 *                                                                   *
 * You should have received a copy of the GNU General Public License *
 * along with this program; if not, contact:                         *
 *                                                                   *
 * Free Software Foundation           Voice:  +1-617-542-5942        *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                   *
 ********************************************************************/

#ifndef QOFBOOKMERGE_H
#define QOFBOOKMERGE_H

#define QOF_MOD_MERGE "qof.merge"

/** @addtogroup BookMerge

<b>Collision handling principles.</b>\n
\n
	-# Always check for a ::GUID first and compare. qofbookmerge only accepts
    valid ::QofBook	data and therefore ALL objects in the import book will
    include valid GUID's.
	-# If the original import data did not contain a GUID (e.g. an external
    non-GnuCash source)	the GUID values will have been created during the
    import and will not match any existing GUID's in the target book so objects
    that do not have a GUID match cannot be assumed to be ::MERGE_NEW - parameter
    values must be checked.
	-# If import contains data from closed books, store the data from the closed
	books in the current book as active. i.e. re-open the books.

- If a GUID match exists, set qof_book_merge_rule::mergeAbsolute to \a TRUE.
	-# If ALL parameters in the import object match the target object with
    the same \a GUID,
	set ::qof_book_merge_result to \a MERGE_ABSOLUTE.
	-# If any parameters differ, set ::MERGE_UPDATE.
- If the import object \a GUID does not match an existing object,
mergeAbsolute is unchanged from the default \a FALSE
The parameter values of the object are compared to other objects of the same
type in the target book.
	-# If the same data exists in the target book with a different GUID, the object
	is tagged as DUPLICATE.
	-# If the data has changed, the object is tagged as REPORT.
	-# If the data does not match, the object is tagged as NEW

More information is at http://code.neil.williamsleesmill.me.uk/

Each foreach function uses g_return_if_fail checks to protect the target book.
If any essential data is missing, the loop returns without changing the target
book. Note that this will not set or return an error value. However, g_return
is only used for critical errors that arise from programming errors, not for
invalid import data which should be cleaned up before creating the import
QofBook.

Only ::qof_book_merge_update_result and ::qof_book_merge_commit return
any error values to the calling process. ::qof_book_merge_init returns a
pointer to the ::QofBookMergeData struct - the calling process needs to
make sure this is non-NULL to know that the Init has been successful.

 @{
*/
/** @file  qofbookmerge.h
    @brief API for merging two \c QofBook structures with collision handling
    @author Copyright (c) 2004-2005 Neil Williams <linux@codehelp.co.uk>
*/

#include "qofutil.h"
#include "qofbook.h"
#include "qofclass.h"
#include "qofobject.h"
#include "qofinstance.h"
#include "qoflog.h"

/** \brief Results of collisions and user resolution.

All rules are initialised as ::MERGE_UNDEF.
Once the comparison is complete, each object within the import will be
updated.

::MERGE_ABSOLUTE, ::MERGE_NEW, ::MERGE_DUPLICATE and ::MERGE_UPDATE can be
reported to the user along with all ::MERGE_REPORT objects for confirmation.
It may be useful later to allow \a MERGE_ABSOLUTE, \a MERGE_NEW,
\a MERGE_DUPLICATE and \a MERGE_UPDATE to not be reported, if the user sets a
preferences option for each result. (Always accept new items: Y/N default NO,
ignores all MERGE_NEW if set to Y etc.) This option would not require any
changes to qofbookmerge.

\a MERGE_NEW, \a MERGE_DUPLICATE and \a MERGE_UPDATE are only actioned after
conflicts are resolved by the user using a dialog and all \a MERGE_REPORT
objects are re-assigned to one of MERGE_NEW, MERGE_DUPLICATE or MERGE_UPDATE.
There is no automatic merge, even if no entities are tagged as MERGE_REPORT,
the calling process must still check for REPORT items using
::qof_book_merge_rule_foreach and call ::qof_book_merge_commit.

\a MERGE_INVALID data should be rare and allows for user-abort - the imported
file/source may be corrupted and the prescence of invalid data should raise
concerns that the rest of the data may be corrupted, damaged or otherwise
altered. If any entity is tagged as MERGE_INVALID, the merge operation will
abort and leave the target book completely unchanged.

\a MERGE_ABSOLUTE is only used for a complete match. The import object contains
the same data in the same parameters with no omissions or amendments. If any
data is missing, amended or added, the data is labelled \a MERGE_UPDATE.

 Every piece of data has a corresponding result. Only when the count of items
 labelled \a MERGE_REPORT is equal to zero are \a MERGE_NEW and \a MERGE_UPDATE
 items added to the existing book.\n \a MERGE_DUPLICATE items are silently
 ignored. Aborting the dialogue/process (by the user or in a program crash) at
 any point before the final commit leaves the existing book completely untouched.
*/
typedef enum
{
    MERGE_UNDEF,     /**< default value before comparison is made. */
    MERGE_ABSOLUTE,  /**< GUID exact match, no new data - \b ignore */
    MERGE_NEW,       /**< import object does \b not exist in the target
                        book - \b add */
    MERGE_REPORT,    /**< import object needs user intervention - \b report */
    MERGE_DUPLICATE, /**< import object with different GUID exactly matches
                        existing GUID - \b ignore */
    MERGE_UPDATE,    /**< import object matches an existing entity but includes
                        new or modified parameter data - \b update */
    MERGE_INVALID    /**< import object didn't match registered object or
                        parameter types or user decided to abort - \b abort */
} QofBookMergeResult;

/** \brief One rule per entity, built into a single GList for the entire merge

All rules are stored in the GList QofBookMergeData::mergeList.

If the ::GUID matches it's the always same semantic object,
regardless of whether other data fields are changed.
\n
The boolean value mergeAbsolute defaults to \c FALSE

NOTE 1: if mergeAbsolute == \c TRUE, ::QofBookMergeResult will still be set
to ::MERGE_UPDATE if parameters within this entity have been modified.

NOTE 2: ::qof_book_merge_param_as_string returns \b string representations of
the parameter data that is causing a collision. These values must \b NOT be
used to set the target parameter - the function is provided for display
purposes only, to make it simple to explain the collision to the user using
MERGE_REPORT and the dialogue.

The GHashTable targetTable in QofBookMergeRule will probably replace the
GSList of the
same name in mergeData.

*/

typedef struct
{
    /* internal counters and reference variables */
    gboolean mergeAbsolute;   /**< Only set if the GUID of the import matches
                                the target */
    double difference;       /**< used to find best match in a book where no
                                GUID matches */
    gboolean updated;        /**< prevent the mergeResult from being
                                overwritten. */
    /* rule objects set from or by external calls */
    QofIdType mergeType;     /**< type of comparison required for check for
                                collision */
    const gchar* mergeLabel;  /**< Descriptive label for the object type,
                                useful for the user intervention dialogue. */
    GSList *mergeParam;      /**< list of usable parameters for the object type */
    GSList *linkedEntList;   /**< list of complex data types included in this object.

	linkedEntList contains an ::QofInstance reference to any parameter that is not
	one of the core QOF_TYPE data types. This entity must be already
    registered with QOF and the results of the comparison for the linked entity
    will modulate the mergeResult of this object. e.g. if an invoice is the
    same value but for a different customer, the invoice will be set to
    MERGE_REPORT and the customer as MERGE_NEW.
	*/
    QofBookMergeResult mergeResult; /**< result of comparison with main ::QofBook */
    QofInstance *importEnt;    /**< pointer to the current entity in the import book. */
    QofInstance *targetEnt;    /**< pointer to the corresponding entity in the
                                target book, if any. */
} QofBookMergeRule;


/** \brief 	mergeData contains the essential context data for any merge.

Used to dictate what to merge, how to merge it, where to get the new data and
where to put the amended data.

Combines lists of \a ::QofParam, \a ::QofInstance and \a ::QofBookMergeRule
into one struct that can be easily passed between callbacks. Also holds the
pointers to the import and target ::QofBook structures.

- targetList and mergeObjectParams change each time a new object type
is set for compare.
- mergeList is the complete list of rules for all objects in the import book.

*/
typedef struct
{
    GSList 	*mergeObjectParams;  /**< GSList of ::QofParam details for each
                                    parameter in the current object. */
    GList 	*mergeList;          /**< GList of all ::QofBookMergeRule rules
                                    for the merge operation. */
    GSList 	*targetList;         /**< GSList of ::QofInstance * for each object
                                    of this type in the target book */
    QofBook *mergeBook;          /**< pointer to the import book for this
                                    merge operation. */
    QofBook *targetBook;         /**< pointer to the target book for this
                                    merge operation. */
    gboolean abort;	             /**< set to TRUE if MERGE_INVALID is set. */
    QofBookMergeRule *currentRule; /**< placeholder for the rule currently
                                    being tested or applied. */
    GSList *orphan_list;         /**< List of QofInstance's that need to be rematched.

	When one QofInstance has a lower difference to the targetEnt than the
    previous best_match, the new match takes precedence. This list holds those
    orphaned entities that are not a good enough match so that these can be
    rematched later. The ranking is handled using the private QofInstanceRating
    struct and the GHashTable ::QofBookMergeData::target_table.
	*/
    GHashTable *target_table;    /**< The GHashTable to hold the
                                    QofInstanceRating values.  */

} QofBookMergeData;


/* ======================================================================== */
/** @name qof_book_merge API
 @{
*/
/** \brief Initialise the QofBookMerge process

	First function of the QofBookMerge API. Every merge must begin with init.

	Requires the book to import (::QofBook *) and the book to receive the
    import, the target book	(::QofBook *). Returns a pointer to
    ::QofBookMergeData which must be checked for a NULL before continuing. \n
Process:

 	-# Invoke the callback ::qof_book_merge_foreach_type on every registered
    object class definition.
	-# Callback obtains the registered parameter list for each object type.
    This provides run time access to all registered objects and all object
    parameters without any changes to QofBookMerge - no registered object or
    parameter is omitted from any merge operation.
	-# Use ::qof_object_foreach to invoke the callback ::qof_book_merge_foreach,
    one object at a time on every instance stored in mergeBook. This is the
    first point where real data from the import book is accessed.
	-# qof_book_merge_foreach obtains the ::GUID for the object from the import
    book and runs the first	check on the original book, checking for any exact
    GUID match. With the full parameter list, the rules for this object can be
    created. If there is a GUID match, the data in each parameter of the import
    object is compared with the same semantic object in the original book. If
    there is no GUID in the import object or no GUID match with the original
    book, the original book is searched to find a parameter match - checking
    for a ::MERGE_DUPLICATE result.
	-# ::qof_book_merge_compare sets the ::QofBookMergeResult of the comparison.
	-# Inserts the completed rule into QofBookMergeData::mergeList GSList.

\return NULL in case of error, otherwise a ::QofBookMergeData* metadata context.

*/
QofBookMergeData*
qof_book_merge_init( QofBook *importBook, QofBook *targetBook);


/** \brief Definition of the dialogue control callback routine

All ::MERGE_REPORT rules must be offered for user intervention using this
template.\n
Commit will fail if any rules are still tagged as \a MERGE_REPORT.

Calling processes are free to also offer MERGE_NEW, MERGE_UPDATE,
MERGE_DUPLICATE and MERGE_ABSOLUTE for user intervention. Attempting to query
MERGE_INVALID rules will cause an error.

For an example, consider test_rule_loop, declared as:

<tt>void test_rule_loop(QofBookMergeData *mergeData, QofBookMergeRule *rule, guint remainder);\n
void test_rule_loop(QofBookMergeData *mergeData, QofBookMergeRule *rule, guint remainder) \n
{\n
	g_return_if_fail(rule != NULL);\n
	g_return_if_fail(mergeData != NULL);
	printf("Rule Result %s", rule->mergeType);\n
	qof_book_merge_update_result(mergeData, rule, MERGE_UPDATE);\n
}</tt>

The dialogue is free to call ::qof_book_merge_update_result in the loop or at the end
as long as the link between the rule and the result is maintained, e.g. by using a
GHashTable.
\n
The parameters are:
	- data : pointer to the ::QofBookMergeData metadata context returned by init.
	- rule : pointer to the ::QofBookMergeRule that generated the collision report
	- remainder : guint value returned from g_slist_length for the number of other
		rules remaining with the same result. This might be useful for a
        progress dialogue, it might not. When updating MERGE_REPORT,
        remainder must equal zero before calling ::qof_book_merge_commit or
        the import will abort.
\n

If the dialogue sets \b any rule result to ::MERGE_INVALID, the import will
abort when ::qof_book_merge_commit is called. It is the responsibility of
the calling function to handle the error code from ::qof_book_merge_commit,
close the dialogue and return. The merge routines in these files will already
have halted the merge operation and freed any memory allocated to merge
structures before returning the error code. There is no need for the dialogue
process to report back to QofBookMerge in this situation.
*/
typedef void (* QofBookMergeRuleForeachCB)( QofBookMergeData*, QofBookMergeRule*, guint);

/** \brief Dialogue Control Callback

This function is designed to be used to iterate over all rules tagged with a
specific ::QofBookMergeResult value.

@param	callback	external loop of type QofBookMergeRuleForeachCB
@param	mergeResult	::QofBookMergeResult value to look up.
@param	mergeData	::QofBookMergeData merge context.

\b Note : MERGE_NEW causes a new entity to be created in the target book at
commit which is then assigned as the targetEnt of that rule. If
mergeResult == MERGE_NEW, the rules returned by qof_book_merge_rule_foreach
will have a NULL set for the targetEnt. This is because commit has not yet
been called and no changes can be made to the target book. The calling
process must handle the NULL targetEnt and NOT call any param_getfcn
routines for the target entity. The import entity is available for display.

Uses ::qof_book_get_collection with the QofBookMergeRule::mergeType object
type to return a collection of ::QofInstance entities from either the
QofBookMergeData::mergeBook or QofBookMergeData::targetBook. Then
uses ::qof_collection_lookup_entity to lookup the QofBookMergeRule::importEnt
and again the QofBookMergeRule::targetEnt to return the two specific entities.

*/
void qof_book_merge_rule_foreach( QofBookMergeData* mergeData,
                                  QofBookMergeRuleForeachCB callback ,
                                  QofBookMergeResult mergeResult);

/** \brief provides easy string access to parameter data for dialogue use

Uses the param_getfcn to retrieve the parameter value as a string, suitable for
display in dialogues and user intervention output. Within a QofBookMerge context,
only the parameters used in the merge are available, i.e. parameters where both
param_getfcn and param_setfcn are not NULL.

Note that the object type description (a full text version of the object name) is
also available to the dialogue as QofBookMergeRule::mergeLabel.

This allows the dialog to display the description of the object and all
parameter data.

*/
gchar* qof_book_merge_param_as_string(QofParam *qtparam, QofInstance *qtEnt);

/** \brief called by dialogue callback to set the result of user intervention

Set \b any rule result to ::MERGE_INVALID to abort the import when
::qof_book_merge_commit is called, without changing the target book.

The calling process should make it absolutely clear that a merge operation
\b cannot be undone and that a backup copy should always be available
\b before a merge is initialised.

Recommended method: Only offer three options to the user per rule:

-# Allow import data to be merged into target data
	- change MERGE_REPORT to MERGE_UPDATE
-# Allow import data without an exact match to be
	added as new
	- change MERGE_REPORT to MERGE_NEW \b IF mergeAbsolute = FALSE
-# Ignore import data and leave target data unchanged
	- change MERGE_REPORT to MERGE_ABSOLUTE or MERGE_DUPLICATE

Handle the required result changes in code: Check the value of
QofBookMergeRule::mergeAbsolute and use these principles:

To ignore entities tagged as:
- MERGE_REPORT, you must check the value of mergeAbsolute.
	- if mergeAbsolute is TRUE, change MERGE_REPORT to MERGE_ABSOLUTE
	- if mergeAbsolute is FALSE, change MERGE_REPORT to MERGE_DUPLICATE
- MERGE_NEW, set MERGE_DUPLICATE.
- MERGE_UPDATE, you must check the value of mergeAbsolute.
	- if mergeAbsolute is TRUE, change MERGE_UPDATE to MERGE_ABSOLUTE
	- if mergeAbsolute is FALSE, change MERGE_UPDATE to MERGE_DUPLICATE

To merge entities that are not pre-set to MERGE_NEW, set MERGE_UPDATE.\n
Attempting to merge an entity when the pre-set value was MERGE_NEW will
force a change back to MERGE_NEW because no suitable target exists for the
merge.

To add entities, check mergeAbsolute is FALSE and set MERGE_NEW.\n
An entity \b only be added if mergeAbsolute is FALSE. Attempting to
add an entity when mergeAbsolute is TRUE will always force a MERGE_UPDATE.

It is not possible to update the same rule more than once.

-# \b MERGE_NEW is reserved for new objects and is only pre-set if
all parameters, including GUID, have already failed to match any
relevant object. ::qof_book_merge_commit will create new
entities for all rules tagged as MERGE_NEW.
	- if mergeAbsolute is TRUE and the user wants to import the
		data, requests to set MERGE_NEW will be forced to MERGE_UPDATE
		because an entity with that GUID already exists in the target book.
	- if MERGE_NEW is pre-set, requests to change to MERGE_UPDATE will be
		ignored because a new entity is needed.
-# \b MERGE_UPDATE is reserved for existing objects - ::qof_book_merge_commit
will require a matching entity to update and will force a change to back to
MERGE_NEW if none is known to exist, using the principle above.
-# \b MERGE_INVALID will cause an abort of the merge process.
-# \b MERGE_UNDEF and \b MERGE_REPORT cannot be set - the entity result will
be unchanged.
-# \b MERGE_DUPLICATE and \b MERGE_ABSOLUTE are handled identically but are
    semantically different - QofBookMergeRule::mergeAbsolute is used to
    dictate which to set:
	- if mergeAbsolute is TRUE but MERGE_DUPLICATE is requested,
		force a change to MERGE_ABSOLUTE.
	- if mergeAbsolute is FALSE but MERGE_ABSOLUTE is requested,
		force a change to MERGE_DUPLICATE.

::qof_book_merge_commit only commits entities tagged
with MERGE_NEW and MERGE_UPDATE results.
\n
Entities tagged with MERGE_ABSOLUTE and MERGE_DUPLICATE results are ignored.

The calling process must check the return value and call
::qof_book_merge_abort(mergeData) if non-zero.

@param	mergeData	the merge context, ::QofBookMergeData*
@param	tag			the result to attempt to set, ::QofBookMergeResult

\return -1 if supplied parameters are invalid or NULL, 0 on success.

*/
QofBookMergeData*
qof_book_merge_update_result(QofBookMergeData *mergeData, QofBookMergeResult tag);

/** \brief Commits the import data to the target book

	The last function in the API and the final part of any QofBookMerge operation.

qof_book_merge_commit will abort the \b entire merge operation if any rule
is set to ::MERGE_INVALID. It is the responsibility of the calling
function to handle the error code from ::qof_book_merge_commit, close the
dialogue and return. qof_book_merge_commit will already have halted the merge
operation and freed any memory allocated to all merge structures before
returning the error code. There is no way for the dialogue process to report
back to qof_book_merge in this situation.

qof_book_merge_commit checks for any entities still tagged as
::MERGE_REPORT and then proceeds to import all entities tagged as
::MERGE_UPDATE or ::MERGE_NEW into the target book.
\n
<b>This final process cannot be UNDONE!</b>\n
\n

@param	mergeData	the merge context, ::QofBookMergeData*

\return
	- -2 if any rules are tagged as ::MERGE_INVALID
		- mergeData will have been g_free'd).
		- note that this will be before any operations are done on the target
			QofBook.
	- -1 if mergeData is invalid or no merge has been initialised with
		::qof_book_merge_init - the calling process must check the value of
        mergeData
	- +1 if some entities are still tagged as \a MERGE_REPORT - use
		::qof_book_merge_update_rule and try again (mergeData is retained).
	- 0 on success - mergeData will have been freed.
*/
gint
qof_book_merge_commit(QofBookMergeData *mergeData );

/** \brief Abort the merge and free all memory allocated by the merge

Sometimes, setting ::MERGE_INVALID is insufficient: e.g. if the user aborts the
merge from outside the functions dealing with the merge ruleset. This function
causes an immediate abort - the calling process must start again at Init if
a new merge is required.
*/
void
qof_book_merge_abort(QofBookMergeData *mergeData);

#endif // QOFBOOKMERGE_H
/** @} */
/** @} */
