/*********************************************************************
 * qof_book_merge.h -- api for QofBook merge with collision handling *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652        *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                    *
 *                                                                   *
 ********************************************************************/

#define _GNU_SOURCE
#ifndef QOFBOOKMERGE_H
#define QOFBOOKMERGE_H

/** @addtogroup QOF
	@{ */
/** @addtogroup BookMerge Merging QofBook structures.

<b>Collision handling principles.</b>\n
\n
	-# Always check for a ::GUID first and compare. qof_book_merge only accepts valid ::QofBook
	data  and therefore ALL objects in the import book will	include valid GUID's.
	-# If the original import data did not contain a GUID (e.g. an external non-GnuCash source)
	the GUID values will have been created during the import and will not match any existing
	GUID's in the target book so objects that do not have a GUID match cannot be assumed to
	be ::MERGE_NEW - parameter values must be checked.
	-# If import contains data from closed books, store the data from the closed
	books in the current book as active. i.e. re-open the books. 

- If a GUID match exists, set qof_book_mergeRule::mergeAbsolute to \a TRUE.
	-# If ALL parameters in the import object match the target object with the same \a GUID, 
	set ::qof_book_mergeResult to \a MERGE_ABSOLUTE.
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

Each foreach function uses g_return_if_fail checks to protect the target book. If
any essential data is missing, the loop returns without changing the target book.
Note that this will not set or return an error value. However, g_return is only 
used for critical errors that arise from programming errors, not for invalid import data 
which should be cleaned up before creating the import QofBook.

Only ::qof_book_mergeInit, ::qof_book_mergeUpdateResult and ::qof_book_mergeCommit return 
any error values to the calling process. 

	@{ */
/**@file  qof_book_merge.h
	@brief API for merging two \c QofBook* structures with collision handling
	@author Copyright (c) 2004 Neil Williams <linux@codehelp.co.uk>
*/


#include <glib.h>
#include "qof.h"
#include "qofinstance-p.h"
#include "gnc-trace.h"

/** \brief Results of collisions and user resolution.

All rules are initialised as ::MERGE_UNDEF.
Once the comparison is complete, each object within the import will be
updated.

::MERGE_ABSOLUTE, ::MERGE_NEW, ::MERGE_DUPLICATE and ::MERGE_UPDATE can be reported
to the user along with all ::MERGE_REPORT objects for confirmation.
It may be useful later to allow \a MERGE_ABSOLUTE, \a MERGE_NEW, \a MERGE_DUPLICATE and
\a MERGE_UPDATE to not be reported, if the user sets a preferences option
for each result. (Always accept new items: Y/N default NO, ignores all
MERGE_NEW if set to Y etc.) This option would not require any changes
to qof_book_merge.

\a MERGE_NEW, \a MERGE_DUPLICATE and \a MERGE_UPDATE are only actioned after 
conflicts are resolved by the user using a dialog and all \a MERGE_REPORT objects are
re-assigned to one of MERGE_NEW, MERGE_DUPLICATE or MERGE_UPDATE. There is no automatic
merge, even if no entities are tagged as MERGE_REPORT, the calling process must still
check for REPORT items using ::qof_book_mergeRuleForeach and call ::qof_book_mergeCommit.

\a MERGE_INVALID data should be rare and allows for user-abort - the imported file/source
 may be corrupted and the prescence of invalid data should raise concerns that
 the rest of the data may be corrupted, damaged or otherwise altered. If any entity is 
 tagged as MERGE_INVALID, the merge operation will abort and leave the target book
 completely unchanged.

\a MERGE_ABSOLUTE is only used for a complete match. The import object contains 
the same data in the same parameters with no omissions or amendments. If any data is missing, 
amended or added, the data is labelled \a MERGE_UPDATE. 

 Every piece of data has a corresponding result. Only when the count of items labelled
 \a MERGE_REPORT is equal to zero are \a MERGE_NEW and \a MERGE_UPDATE 
 items added to the existing book.\n \a MERGE_DUPLICATE items are silently ignored.
 Aborting the dialog/process (by the user or in a program crash) at any point before the
 final commit leaves the existing book completely untouched.
*/
typedef enum { 
	MERGE_UNDEF, 		/**< default value before comparison is made. */
	MERGE_ABSOLUTE, 	/**< GUID exact match, no new data - \b ignore */
	MERGE_NEW, 			/**< import object does \b not exist in the
							target book - \b add */
	MERGE_REPORT, 		/**< import object needs user intervention - \b report */
	MERGE_DUPLICATE, 	/**< import object with different GUID exactly
							matches existing GUID - \b ignore */
	MERGE_UPDATE, 		/**< import object matches an existing entity but 
							includes new or modified parameter data - \b update */
	MERGE_INVALID 		/**< import object didn't match registered object
							or parameter types or user decided to abort - \b abort */
}qof_book_mergeResult;


/** \brief 	mergeData contains the essential data for any merge.

Used to dictate what to merge, how to merge it, where to get the new data and
where to put the amended data. 

Combines lists of \a ::QofParam, \a ::QofEntity and \a ::qof_book_mergeRule into one struct that
can be easily passed between callbacks. Also holds the pointers to the import and target ::QofBook 
structures.
	
- targetList and mergeObjectParams change each time a new object type is set for compare. 
- mergeList is the complete list of rules for all objects in the import book.

*/
typedef struct
{
	GSList 	*mergeObjectParams;	/**< GSList of ::QofParam details for each parameter in the current object. */
	GList 	*mergeList;			/**< GSList of ::qof_book_mergeRule rules for the import data. */
	GSList 	*targetList;		/**< GSList of ::QofEntity * for each object of this type in the target book */
	QofBook *mergeBook;			/**< pointer to the import book for this merge operation. */
	QofBook *targetBook;		/**< pointer to the target book for this merge operation. */
	gboolean abort;				/**< set to TRUE if MERGE_INVALID is set. */
}qof_book_mergeData;


/** \brief One rule per entity, built into a single GSList for the entire merge 

All rules are stored in the GSList qof_book_mergeData::mergeList.

If the ::GUID matches it's the always same semantic object,
regardless of whether other data fields are changed.
\n	
The boolean value mergeAbsolute defaults to \c FALSE\n

NOTE 1: if mergeAbsolute == \c TRUE, ::qof_book_mergeResult will still be set to 
::MERGE_UPDATE if parameters within this entity have been modified.

NOTE 2: ::qof_book_merge_param_as_string returns \b string representations of the parameter
data that is causing a collision. These values must \b NOT be used to set the target
parameter - the function is provided for display purposes only, to make it simple to
explain the collision to the user using MERGE_REPORT and the dialog.

*/
typedef struct 
{
	/* internal counters and reference variables */
	gboolean mergeAbsolute;			/**< Only set if the GUID of the import matches the target */
	gint difference;				/**< used to find best match in a book where no GUID matches */
	gboolean updated;				/**< prevent the mergeResult from being overwritten. */
	/* rule objects set from or by external calls */
	QofIdType mergeType;			/**< type of comparison required for check for collision */
	const char* mergeLabel;			/**< Descriptive label for the object type, useful for the
											user intervention dialog. */
	GSList *mergeParam;				/**< list of usable parameters for the object type */
	GSList *linkedEntList;			/**< list of complex data types included in this object. 

	linkedEntList contains an ::QofEntity reference to any parameter that is not
	one of the core QOF_TYPE data types. This entity must be already registered with QOF
	and the results of the comparison for the linked entity will modulate the mergeResult
	of this object. e.g. if an invoice is the same value but for a different customer,
	the invoice will be set to MERGE_REPORT and the customer as MERGE_NEW.
	*/
	qof_book_mergeResult mergeResult; /**< result of comparison with main ::QofBook */
	QofEntity *importEnt;			/**< pointer to the current entity in the import book. */
	QofEntity *targetEnt;			/**< pointer to the corresponding entity in the target book, if any. */
}qof_book_mergeRule;


/* ======================================================================== */
/** @name qof_book_merge API */
/** @{
*/
/** \brief Initialise the qof_book_merge process

	First function of the qof_book_merge API. Every merge must begin with Init.

	Requires the book to import (::QofBook *) and the book to receive the import, the target book
	(::QofBook *). \n
Process:

 	-# Invoke the callback ::qof_book_mergeForeachType on every registered object class definition. 
	-# Callback obtains the registered parameter list for each object type. This provides run time 
	access to all registered objects and all object parameters without any changes to
	qof_book_merge - no registered object or parameter is omitted from any merge operation.
	-# Use ::qof_object_foreach to invoke the callback ::qof_book_mergeForeach, one object at a time 
	 on every instance stored in mergeBook. This is the first point where real data from the import 
	 book is accessed.
	-# qof_book_mergeForeach obtains the ::GUID for the object from the import book and runs the first
	check on the original book, checking for any exact GUID match. With the full parameter list, 
	the rules for this object can be created. If there is a GUID match, the data in each parameter 
	of the import object is compared with the same semantic object in the original book. If there is
	no GUID in the import object or no GUID match with the original book, the original book is 
	searched to find a parameter match - checking for a ::MERGE_DUPLICATE result.
	-# ::qof_book_mergeCompare sets the ::qof_book_mergeResult of the comparison.
	-# Inserts the completed rule into qof_book_mergeData::mergeList GSList.

\return -1 in case of error, otherwise 0.

*/
int
qof_book_mergeInit( QofBook *importBook, QofBook *targetBook);


/** \brief Definition of the dialog control callback routine

All ::MERGE_REPORT rules must be offered for user intervention using this template.\n
Commit will fail if any rules are still tagged as \a MERGE_REPORT.

Calling processes are free to also offer MERGE_NEW, MERGE_UPDATE, MERGE_DUPLICATE and 
MERGE_ABSOLUTE for user intervention. Attempting to query MERGE_INVALID rules
will cause an error.

For an example, consider test_rule_loop, declared as:

<tt>void test_rule_loop(qof_book_mergeRule *rule, guint remainder);\n
void test_rule_loop(qof_book_mergeRule *rule, guint remainder) \n
{\n
	g_return_if_fail(rule != NULL);\n
	printf("Rule Result %s", rule->mergeType);\n
	qof_book_mergeUpdateResult(rule,MERGE_UPDATE);\n
}</tt>

The dialog is free to call ::qof_book_mergeUpdateResult in the loop or at the end
as long as the link between the rule and the result is maintained, e.g. by using a
GHashTable. 
\n
The parameters are:
	- rule : pointer to the ::qof_book_mergeRule that generated the collision report
	- remainder : guint value returned from g_slist_length for the number of other
		rules remaining with the same result. This might be useful for a progress dialog, it might not.
		When updating MERGE_REPORT, remainder must equal zero before calling 
		::qof_book_mergeCommit or the import will abort.
\n

If the dialog sets \b any rule result to ::MERGE_INVALID, the import will abort when
::qof_book_mergeCommit is called. It is the responsibility of the calling 
function to handle the error code from ::qof_book_mergeCommit, close the dialog
and return to GnuCash. The merge routines in these files will already have halted the merge 
operation and freed any memory allocated to merge structures before returning the error code.
There is no need for the dialog process to report back to qof_book_merge in this situation.

*/
typedef void (* qof_book_mergeRuleForeachCB)(qof_book_mergeRule*, guint);

/** \brief Dialog Control Callback

This function is designed to be used to iterate over all rules tagged with a specific
::qof_book_mergeResult value.

Uses ::qof_book_get_collection with the qof_book_mergeRule::mergeType object type to
return a collection of ::QofEntity entities from either the qof_book_mergeData::mergeBook or
qof_book_mergeData::targetBook. Then uses ::qof_collection_lookup_entity to lookup 
the qof_book_mergeRule::importEnt and again the qof_book_mergeRule::targetEnt to 
return the two specific entities.

*/
void qof_book_mergeRuleForeach( qof_book_mergeRuleForeachCB, qof_book_mergeResult);


/** \brief Holds details of each rule as the callbacks iterate over the list.

*/
struct qof_book_mergeRuleIterate {
	qof_book_mergeRuleForeachCB   fcn;
	qof_book_mergeRule *data;
	GList *ruleList;
	guint remainder;
};

/** \brief provides easy string access to parameter data for dialog use

<b>Must only be used for display purposes!</b>

Uses the param_getfcn to retrieve the parameter value as a string, suitable for
display in dialogs and user intervention output. Only the parameters used in the merge
are available, i.e. parameters where both param_getfcn and param_setfcn are not NULL.

Note that the object type description (a full text version of the object name) is
also available to the dialog as qof_book_mergeRule::mergeLabel.

This allows the dialog to display the description of the object and all parameter data.

*/
char* qof_book_merge_param_as_string(QofParam *qtparam, QofEntity *qtEnt);

/** \brief called by dialog callback to set the result of user intervention

Set \b any rule result to ::MERGE_INVALID to abort the import when
::qof_book_mergeCommit is called, without changing the target book.

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
qof_book_mergeRule::mergeAbsolute and use these principles:

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
force a change back to MERGE_NEW.

To add entities, check mergeAbsolute is FALSE and set MERGE_NEW.\n
An entity \b only be added if mergeAbsolute is FALSE. Attempting to
add an entity when mergeAbsolute is TRUE will always force a MERGE_UPDATE.

It is not possible to update the same rule more than once.

-# \b MERGE_NEW is reserved for new objects and is only pre-set if
all parameters, including GUID, have already failed to match any 
relevant object. ::qof_book_mergeCommit will create new 
entities for all rules tagged as MERGE_NEW. 
	- if mergeAbsolute is TRUE and the user wants to import the 
		data, requests to set MERGE_NEW will be forced to MERGE_UPDATE 
		because an entity with that GUID already exists in the target book.
	- if MERGE_NEW is pre-set, requests to change to MERGE_UPDATE will be 
		ignored because a new entity is needed.
-# \b MERGE_UPDATE is reserved for existing objects - ::qof_book_mergeCommit 
will require a matching entity to update and will force a change to back to 
MERGE_NEW if none is known to exist, using the principle above.
-# \b MERGE_INVALID will cause an abort of the merge process.
-# \b MERGE_UNDEF and \b MERGE_REPORT cannot be set - the entity result will be unchanged.
-# \b MERGE_DUPLICATE and \b MERGE_ABSOLUTE are handled identically but are semantically
	different - qof_book_mergeRule::mergeAbsolute is used to dictate which to set:
	- if mergeAbsolute is TRUE but MERGE_DUPLICATE is requested,
		force a change to MERGE_ABSOLUTE.
	- if mergeAbsolute is FALSE but MERGE_ABSOLUTE is requested,
		force a change to MERGE_DUPLICATE.

::qof_book_mergeCommit only commits entities tagged 
with MERGE_NEW and MERGE_UPDATE results.
\n
Entities tagged with MERGE_ABSOLUTE and MERGE_DUPLICATE results are ignored.

\return -1 if supplied parameters are invalid or NULL, 0 on success.
		
*/
int qof_book_mergeUpdateResult(qof_book_mergeRule *resolved, qof_book_mergeResult tag);


/** \brief Commits the import data to the target book

	The last function in the API and the final part of any qof_book_merge operation.

qof_book_mergeCommit will abort the \b entire merge operation if any rule is set to
::MERGE_INVALID. It is the responsibility of the calling 
function to handle the error code from ::qof_book_mergeCommit, close the dialog
and return to GnuCash. qof_book_mergeCommit will already have halted the merge 
operation and freed any memory allocated to all merge structures before returning the error
code. There is no way for the dialog process to report back to qof_book_merge in this situation.

qof_book_mergeCommit checks for any entities still tagged as ::MERGE_REPORT and then proceeds
to import all entities tagged as ::MERGE_UPDATE or ::MERGE_NEW into the target book.
\n
<b>This final process cannot be UNDONE!</b>\n
\n

\return 
	- -1 if no merge has been initialised with ::qof_book_mergeInit or if any rules
	are tagged as ::MERGE_INVALID,
	- +1 if some entities are still tagged as \a MERGE_REPORT
	- 0 on success.
*/
int
qof_book_mergeCommit( void );

/** \brief Abort the merge and free all memory allocated by the merge

Sometimes, setting ::MERGE_INVALID is insufficient: e.g. if the user aborts the
merge from outside the functions dealing with the merge ruleset. This function
causes an immediate abort - the calling process must start again at Init if 
a new merge is required.
*/
void
qof_book_merge_abort(void);

/** @} */

/* ======================================================================== */
/* Internal callback routines */

/** @name Phase 1: Import book */
/** @{
*/

/** \brief Looks up all import objects and calls ::qof_book_mergeCompare for each. 

	This callback is used to obtain a list of all objects and their parameters
	in the book to be imported.\n

	Called by ::qof_book_mergeForeachType.\n
	Receives all instances of only those objects that exist in the import book,
	from ::qof_object_foreach. ::qof_book_mergeData contains a full list of all registered 
	parameters for each object in the mergeObjectParams GSList. \n
	Looks up the live parameter data (via ::QofEntity and ::QofParam), creates the rule, 
	compares the data and stores the result of the comparison.

Process:

	-# Sets default ::qof_book_mergeResult as MERGE_UNDEF - undefined.\n
	-# Obtains GUID, parameter data, type and rule.
	-# Compares GUID with original book, sets ::qof_book_mergeData .mergeAbsolute
	to TRUE if exact match.
	-# Inserts rule into ::qof_book_mergeData rule list.
	-# Runs the comparison for that data type using ::qof_book_mergeCompare.
	
*/
void qof_book_mergeForeach (QofEntity* mergeEnt, gpointer mergeData);

/** \brief Registered Object Callback.

	Receives one object at a time from ::qof_object_foreach_type.\n
	Note: generic type data only, no live data accesses.\n
	::qof_object_foreach_type called directly by ::qof_book_mergeInit.

	This callback is used to obtain a list of all registered
	objects, whether or not the objects exist in either the import or
	original books.\n
	
	Invokes the callback ::qof_book_mergeForeach on every instance of a particular object type.
	The callback will be invoked only for those instances stored in the import book and therefore
	qof_book_mergeForeach gains the first access to any live data.
*/
void qof_book_mergeForeachType (QofObject* merge_obj, gpointer mergeData);

/** \brief Iterates over each parameter name within the selected QofObject.

	 Receives the list of parameter names from ::QofParam and fills the GSList in
	 ::qof_book_mergeData.\n
	 No live data access - object typing and parameter listing only.\n
	 \b Note: This function is called by ::qof_book_mergeForeachType in the comparison
	 stage and ::qof_book_mergeCommitRuleLoop in the commit stage. Change with care!
*/
void qof_book_mergeForeachParam(QofParam* param_name, gpointer user_data);

/** @} */
/** @name Phase 2: Target book */
/** @{
*/

/** \brief Registered Object Callback for the \b target book.

	Receives one object at a time from ::qof_object_foreach_type.\n
\n
	This callback is used to iterate through all the registered
	objects, in the \b Target book. When the target object type
	matches the object type of the current import object, calls
	::qof_book_mergeForeachTarget to store details of the possible target
	matches in the GSList *targetList in ::qof_book_mergeData .
	\n
*/
void qof_book_mergeForeachTypeTarget ( QofObject* merge_obj, gpointer mergeData);


/** \brief Looks up all \b target objects of a specific type.

	This callback is used to obtain a list of all suitable objects and their parameters
	in the \b target book.\n
\n
	Called by ::qof_book_mergeForeachTypeTarget.\n
	Receives all instances of only those objects that exist in the target book,
	that match the object type of the current \b import object.
	This is done when there is no GUID match and there is no way to know if
	a corresponding object exists in the target book that would conflict with the
	data in the import object.
\n
	Stores details of the QofEntity* of each suitable object in the target book
	for later comparison by ::qof_book_mergeCompare .
	
*/
void qof_book_mergeForeachTarget (QofEntity* mergeEnt, gpointer mergeData);

/** \brief Omits target entities that have already been matched.

	It is possible for two entities in the import book to match a single entity in
	the target book, resulting in a loss of data during commit.
	
	qof_book_merge_target_check simply checks the GUID of all existing
	target entities against the full list of all entities of a suitable type
	in the ::qof_book_mergeForeachTarget iteration. Possible target entity
	matches are only added to the qof_book_mergeData::targetList if the GUID
	does not match.
*/
void qof_book_merge_target_check (QofEntity* targetEnt);

/** @} */
/** @name Phase 3: User Intervention
*/
/** @{
*/

/** \brief Iterates over the rules and declares the number of rules left to match

	The second argument is a guint holding the remainder which might be
	useful for progress feedback in the GUI. 
*/
void qof_book_mergeRuleCB(gpointer, gpointer);

/** @} */
/** @name Phase 4: Commit import to target book
*/
/** @{
*/

/** \brief Separates the rules according to the comparison results.

	Used to create a list of all rules that match a particular ::qof_book_mergeResult.
	Intended for ::MERGE_NEW and ::MERGE_UPDATE, it can be used for ::MERGE_ABSOLUTE or
	::MERGE_DUPLICATE if you want to maybe report on what will be ignored in the import.
	
	It can \b NOT be used for ::MERGE_UNDEF, ::MERGE_INVALID or ::MERGE_REPORT.
*/
void qof_book_mergeCommitForeach (qof_book_mergeRuleForeachCB cb, qof_book_mergeResult mergeResult );

/** \brief Iterates over the rules and declares the number of rules left to commit

	The second argument is a guint holding the remainder which might be
	useful for progress feedback in the GUI. 

*/
void qof_book_mergeCommitForeachCB(gpointer, gpointer);

/** \brief Commit the data from the import to the target QofBook.

	Called by ::qof_book_mergeCommit to commit data from each rule in turn.
	Uses QofParam->param_getfcn - ::QofAccessFunc to query the import book
	and param_setfcn - ::QofSetterFunc to update the target book.
\n	
	Note: Not all param_getfcn can have a matching param_setfcn.
	Getting the balance of an account is obviously necessary to other routines
	but is pointless in a comparison for a merge - the balance is calculated from
	transactions, it cannot be set by the account. A discrepancy in the calculated
	figures for an account object should not cause a MERGE_REPORT.
\n
 	Limits the comparison routines to only calling param_getfcn if 
	param_setfcn is not NULL. 
	
*/

void qof_book_mergeCommitRuleLoop(qof_book_mergeRule *rule, guint remainder);

/** @} */


/** @name Comparison and Rule routines
*/
/** @{
*/

/** \brief Set the ::qof_book_mergeResult for each QofIdType parameter, using ::GUID if any.

\n
GUID's used in comparisons of entities and instances.

If there is no GUID match, \a mergeData->mergeAbsolute will be set to FALSE.
::qof_book_mergeCompare will receive a GSList of ::QofEntity * targets instead of one
unique match and will iterate through the parameter comparisons for each member of 
the GSList *targetList.

Sets function pointers to the parameter_getter and parameter_setter routines from 
::QofParam *mergeObjectParams and matches the comparison to the incoming data type:\n

@param mergeRule - contains the GUID, the import book reference, ::QofIdType of the object 
that contains the parameter and mergeResult.

\return -1 in case of error, otherwise 0.

*/
int 
qof_book_mergeCompare( void );

/** \brief Makes the decisions about how matches and conflicts are tagged.

New rules start at:
	- ::MERGE_ABSOLUTE\n 
		(GUID's match; first parameter matches) OR
	- ::MERGE_DUPLICATE\n
		(GUID's do NOT match; first parameter DOES match) OR
	- ::MERGE_NEW\n
	(GUID's do NOT match; first parameters does NOT match).
	
If subsequent parameters in the same object FAIL a match:
	- \a MERGE_ABSOLUTE fallsback to ::MERGE_UPDATE \n
		(GUID matches but some parameters differ)\n
		(guidTarget will be updated with mergeEnt)
	- \a MERGE_DUPLICATE fallsback to ::MERGE_REPORT\n
		(GUID does not match and some parameters do NOT match)
	- \a MERGE_NEW fallsback to \a MERGE_REPORT
		(GUID does not match and some parameters now DO match)

<b>Comparisons without a GUID match.</b>
	Only sets a failed match if ALL objects fail to match.
	when absolute is FALSE, all suitable target objects are compared.
	mergeResult is not set until all targets checked.
	Identifies the closest match using a difference rank. This avoids 
	using non-generic tests for object similarities. difference has a 
	maximum value of the total number of comparable parameters and the
	value closest to zero is used. In the case of a tie, it is
	currently first-come-first-served. FIXME!

*/
void qof_book_mergeUpdateRule( gboolean match);

/** @} */
/** @} */
#endif // QOFBOOKMERGE_H
