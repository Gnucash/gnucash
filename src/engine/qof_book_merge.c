/*********************************************************************
 * qof_book_merge.c -- api for QoFBook merge with collision handling *
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

#include "qof_book_merge.h"
static short module = MOD_IMPORT; 

/* all qof_book_merge data is held in mergeData. */
static qof_book_mergeData* mergeData = NULL;

/*
currentRule is only used when a qof_book_mergeRule is being inspected or
tested, not when it is created. This is to avoid the need for g_new()
every time a single rule is checked.

Rules are created and freed separately, via the mergeData GList, mergeList.
*/
static qof_book_mergeRule* currentRule = NULL;

/* ================================================================ */
/* API functions. */
int
qof_book_mergeInit( QofBook *importBook, QofBook *targetBook) 
{
	GList *check;
	g_return_val_if_fail((importBook != NULL)&&(targetBook != NULL), -1);
	mergeData = g_new(qof_book_mergeData, 1);
	mergeData->abort = FALSE;
	mergeData->mergeList = NULL;
	mergeData->targetList = NULL;
	mergeData->mergeBook = importBook;
	mergeData->targetBook = targetBook;
	mergeData->mergeObjectParams = NULL;
	currentRule = g_new(qof_book_mergeRule, 1);
	qof_object_foreach_type(qof_book_mergeForeachType, NULL);
	check = g_list_copy(mergeData->mergeList);
	while(check != NULL) {
		currentRule = check->data;
		if(currentRule->mergeResult == MERGE_INVALID) {
			qof_book_merge_abort();
			return(-1);
		}
		check = g_list_next(check);
	}
	g_list_free(check);
	return 0;
}

void
qof_book_merge_abort (void)
{
	if(currentRule) {
		g_slist_free(currentRule->linkedEntList);
		g_slist_free(currentRule->mergeParam);
		g_free(currentRule);
	}
	while(mergeData->mergeList != NULL) {
		g_free(mergeData->mergeList->data);
		mergeData->mergeList = g_list_next(mergeData->mergeList);
	}
	while(mergeData->targetList != NULL) {
		g_free(mergeData->targetList->data);
		mergeData->targetList = g_slist_next(mergeData->targetList);
	}
	g_list_free(mergeData->mergeList);
	g_slist_free(mergeData->mergeObjectParams);
	g_slist_free(mergeData->targetList);
	g_free(mergeData);
}

char*
qof_book_merge_param_as_string(QofParam *qtparam, QofEntity *qtEnt)
{
	gchar 		*stringImport;
	char 		sa[GUID_ENCODING_LENGTH + 1];
	KvpFrame 	*kvpImport;
	QofType 	mergeType;
	const GUID *guidImport;
	gnc_numeric numericImport, 	(*numeric_getter)	(QofEntity*, QofParam*);
	Timespec 	tsImport, 		(*date_getter)		(QofEntity*, QofParam*);
	double 		doubleImport, 	(*double_getter)	(QofEntity*, QofParam*);
	gboolean 	booleanImport, 	(*boolean_getter)	(QofEntity*, QofParam*);
	gint32 		i32Import, 		(*int32_getter)		(QofEntity*, QofParam*);
	gint64 		i64Import, 		(*int64_getter)		(QofEntity*, QofParam*);
	
	stringImport = NULL;
	mergeType = qtparam->param_type;
	if(safe_strcmp(mergeType, QOF_TYPE_STRING) == 0)  { 
			stringImport = g_strdup(qtparam->param_getfcn(qtEnt,qtparam));
			if(stringImport == NULL) { stringImport = ""; }
			return stringImport;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_DATE) == 0) { 
			date_getter = (Timespec (*)(QofEntity*, QofParam*))qtparam->param_getfcn;
			tsImport = date_getter(qtEnt, qtparam);
			stringImport = g_strdup_printf("%llu", tsImport.tv_sec);
			return stringImport;
		}
		if((safe_strcmp(mergeType, QOF_TYPE_NUMERIC) == 0)  ||
		(safe_strcmp(mergeType, QOF_TYPE_DEBCRED) == 0)) { 
			numeric_getter = (gnc_numeric (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			numericImport = numeric_getter(qtEnt,qtparam);
			stringImport = g_strdup(gnc_numeric_to_string(numericImport));
			return stringImport;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_GUID) == 0) { 
			guidImport = qtparam->param_getfcn(qtEnt,qtparam);
			guid_to_string_buff(guidImport, sa);
			stringImport = g_strdup(sa);
			return stringImport;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_INT32) == 0) { 
			int32_getter = (gint32 (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			i32Import = int32_getter(qtEnt, qtparam);
			stringImport = g_strdup_printf("%u", i32Import);
			return stringImport;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_INT64) == 0) { 
			int64_getter = (gint64 (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			i64Import = int64_getter(qtEnt, qtparam);
			stringImport = g_strdup_printf("%llu", i64Import);
			return stringImport;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_DOUBLE) == 0) { 
			double_getter = (double (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			doubleImport = double_getter(qtEnt, qtparam);
			stringImport = g_strdup_printf("%f", doubleImport);
			return stringImport;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_BOOLEAN) == 0){ 
			boolean_getter = (gboolean (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			booleanImport = boolean_getter(qtEnt, qtparam);
			if(booleanImport == TRUE) { stringImport = g_strdup("TRUE"); }
			else { stringImport = g_strdup("FALSE"); }
			return stringImport;
		}
		/* "kvp" */
		/* FIXME: how can this be a string??? */
		if(safe_strcmp(mergeType, QOF_TYPE_KVP) == 0) { 
			kvpImport = kvp_frame_copy(qtparam->param_getfcn(qtEnt,qtparam));

			return stringImport;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_CHAR) == 0) { 
			stringImport = g_strdup(qtparam->param_getfcn(qtEnt,qtparam));
			return stringImport;
		}
	return NULL;
}

int
qof_book_mergeUpdateResult(qof_book_mergeRule *resolved, qof_book_mergeResult tag)
{
	g_return_val_if_fail((resolved != NULL), -1);
	g_return_val_if_fail((tag > 0), -1);
	g_return_val_if_fail((tag != MERGE_REPORT), -1);
	currentRule = resolved;
	if((currentRule->mergeAbsolute == TRUE)&&	(tag == MERGE_DUPLICATE)) 	{ tag = MERGE_ABSOLUTE; }
	if((currentRule->mergeAbsolute == TRUE)&&	(tag == MERGE_NEW)) 		{ tag = MERGE_UPDATE; }
	if((currentRule->mergeAbsolute == FALSE)&&	(tag == MERGE_ABSOLUTE)) 	{ tag = MERGE_DUPLICATE; }
	if((currentRule->mergeResult == MERGE_NEW)&&(tag == MERGE_UPDATE)) { tag = MERGE_NEW; }
	if(currentRule->updated == FALSE) { currentRule->mergeResult = tag;	}
	currentRule->updated = TRUE;
	if(tag == MERGE_INVALID) {
		mergeData->abort = TRUE;
		qof_book_merge_abort();
	}
	return 0;
}

int
qof_book_mergeCommit( void )
{
	GList *check;
	
	if(mergeData->abort == TRUE) return -1;
	g_return_val_if_fail(mergeData != NULL, -1);
	g_return_val_if_fail(mergeData->mergeList != NULL, -1);
	currentRule = mergeData->mergeList->data;
	check = g_list_copy(mergeData->mergeList);
	while(check != NULL) {
		currentRule = check->data;
		if(currentRule->mergeResult == MERGE_INVALID) {
			qof_book_merge_abort();
			return(-1);
		}
		if(currentRule->mergeResult == MERGE_REPORT) {
			g_list_free(check);
			return 1;
		}
		check = g_list_next(check);
	}
	g_list_free(check);
	qof_book_mergeCommitForeach( qof_book_mergeCommitRuleLoop, MERGE_NEW);
	qof_book_mergeCommitForeach( qof_book_mergeCommitRuleLoop, MERGE_UPDATE);
	while(mergeData->mergeList != NULL) {
		g_free(mergeData->mergeList->data);
		mergeData->mergeList = g_list_next(mergeData->mergeList);
	}
	g_list_free(mergeData->mergeList);
	g_slist_free(mergeData->mergeObjectParams);
	g_slist_free(mergeData->targetList);
	g_free(mergeData);
	return 0;
}

/* End of API functions. Internal code follows. */
/* ==================================================================== */

void qof_book_mergeRuleForeach( qof_book_mergeRuleForeachCB cb, qof_book_mergeResult mergeResult)
{
	struct qof_book_mergeRuleIterate iter;
	GList *matching_rules;

	g_return_if_fail(cb != NULL);
	g_return_if_fail(mergeData != NULL);
	g_return_if_fail(mergeResult > 0);
	g_return_if_fail(mergeResult != MERGE_INVALID);
	g_return_if_fail(mergeData->abort == FALSE);
	iter.fcn = cb;
	matching_rules = NULL;
	iter.ruleList = g_list_copy(mergeData->mergeList);
	while(iter.ruleList!=NULL) {
		currentRule = iter.ruleList->data;
		if(currentRule->mergeResult == mergeResult) {
			matching_rules = g_list_prepend(matching_rules, currentRule);
		}
		iter.ruleList = g_list_next(iter.ruleList);
	}
	iter.remainder = g_list_length(matching_rules);
	g_list_free(iter.ruleList);
	g_list_foreach (matching_rules, qof_book_mergeRuleCB, &iter);
	g_list_free(matching_rules);
}


void
qof_book_mergeUpdateRule(gboolean match)
{
	gboolean absolute;

	absolute = currentRule->mergeAbsolute;
	if(absolute && match && currentRule->mergeResult == MERGE_UNDEF) {
			currentRule->mergeResult = MERGE_ABSOLUTE;
	}
	if(absolute && !match) { currentRule->mergeResult = MERGE_UPDATE; }
	if(!absolute && match &&currentRule->mergeResult == MERGE_UNDEF) {
			currentRule->mergeResult = MERGE_DUPLICATE;
	}
	if(!absolute && !match) {
		currentRule->difference++;
		if(currentRule->mergeResult == MERGE_DUPLICATE) {
			currentRule->mergeResult = MERGE_REPORT;
		}
	}
}

int 
qof_book_mergeCompare( void ) 
{
	gchar 		*stringImport, *stringTarget, 
				*charImport, *charTarget;
	char 		sa[GUID_ENCODING_LENGTH + 1];
	const GUID 	*guidImport, *guidTarget;
	QofInstance *inst;
	gpointer 	unknown_obj;
	QofParam 	*qtparam;
	KvpFrame 	*kvpImport, *kvpTarget;
	QofIdType 	mergeParamName;
	QofType 	mergeType;
	GSList 		*paramList;
	QofEntity 	*mergeEnt, *targetEnt, *childEnt;
	Timespec 	tsImport, tsTarget, 			(*date_getter)		(QofEntity*, QofParam*);
	gnc_numeric numericImport, numericTarget, 	(*numeric_getter)	(QofEntity*, QofParam*);
	double 		doubleImport, doubleTarget, 	(*double_getter)	(QofEntity*, QofParam*);
	gboolean 	absolute, mergeError, 
				knowntype, mergeMatch, 
				booleanImport, booleanTarget,	(*boolean_getter)	(QofEntity*, QofParam*);
	gint32 		i32Import, i32Target, 			(*int32_getter)		(QofEntity*, QofParam*);
	gint64 		i64Import, i64Target, 			(*int64_getter)		(QofEntity*, QofParam*);

	g_return_val_if_fail((mergeData != NULL)||(currentRule != NULL), -1);
	absolute = currentRule->mergeAbsolute;
	mergeEnt = currentRule->importEnt;
	targetEnt = currentRule->targetEnt;
	paramList = currentRule->mergeParam;
	currentRule->difference = 0;
	currentRule->mergeResult = MERGE_UNDEF;
	g_return_val_if_fail((targetEnt)||(mergeEnt)||(paramList), -1);
	
	kvpImport = kvp_frame_new();
	kvpTarget = kvp_frame_new();
	mergeError = FALSE;
	while(paramList != NULL) {
		mergeMatch = FALSE;
		knowntype = FALSE;
		qtparam = paramList->data;
		mergeParamName = qtparam->param_name;

		g_return_val_if_fail(mergeParamName != NULL, -1);
		mergeType = qtparam->param_type;
		if(safe_strcmp(mergeType, QOF_TYPE_STRING) == 0)  { 
			stringImport = qtparam->param_getfcn(mergeEnt,qtparam);
			stringTarget = qtparam->param_getfcn(targetEnt,qtparam);
			/* very strict string matches may need to be relaxed. */
			if(stringImport == NULL) { stringImport = ""; }
			if(stringTarget == NULL) { stringTarget = ""; }
			if(safe_strcmp(stringImport,stringTarget) == 0) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch);
			stringImport = stringTarget = NULL;
			knowntype= TRUE;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_DATE) == 0) {
			date_getter = (Timespec (*)(QofEntity*, QofParam*))qtparam->param_getfcn;
			tsImport = date_getter(mergeEnt, qtparam);
			tsTarget = date_getter(targetEnt, qtparam);
			if(timespec_cmp(&tsImport, &tsTarget) == 0) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch);
			knowntype= TRUE;
		}
		if((safe_strcmp(mergeType, QOF_TYPE_NUMERIC) == 0)  ||
		(safe_strcmp(mergeType, QOF_TYPE_DEBCRED) == 0)) { 
			numeric_getter = (gnc_numeric (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			numericImport = numeric_getter(mergeEnt,qtparam);
			numericTarget = numeric_getter(targetEnt,qtparam);
			if(gnc_numeric_compare (numericImport, numericTarget) == 0) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch);
			knowntype= TRUE;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_GUID) == 0) { 
			guidImport = qtparam->param_getfcn(mergeEnt,qtparam);
			guidTarget = qtparam->param_getfcn(targetEnt,qtparam);
			if(guid_compare(guidImport, guidTarget) == 0) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch);
			knowntype= TRUE;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_INT32) == 0) { 
			int32_getter = (gint32 (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			i32Import = int32_getter(mergeEnt, qtparam);
			i32Target = int32_getter(targetEnt, qtparam);
			if(i32Target == i32Import) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch);
			knowntype= TRUE;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_INT64) == 0) { 
			int64_getter = (gint64 (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			i64Import = int64_getter(mergeEnt, qtparam);
			i64Target = int64_getter(targetEnt, qtparam);
			if(i64Target == i64Import) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch); 
			knowntype= TRUE;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_DOUBLE) == 0) { 
			double_getter = (double (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			doubleImport = double_getter(mergeEnt, qtparam);
			doubleTarget = double_getter(mergeEnt, qtparam);
			if(doubleImport == doubleTarget) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch); 
			knowntype= TRUE;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_BOOLEAN) == 0){ 
			boolean_getter = (gboolean (*)(QofEntity*, QofParam*)) qtparam->param_getfcn;
			booleanImport = boolean_getter(mergeEnt, qtparam);
			booleanTarget = boolean_getter(targetEnt, qtparam);
			if(booleanImport != FALSE && booleanImport != TRUE) { booleanImport = FALSE; }
			if(booleanTarget != FALSE && booleanTarget != TRUE) { booleanTarget = FALSE; }
			if(booleanImport == booleanTarget) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch);
			knowntype= TRUE;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_KVP) == 0) { 
			kvpImport = kvp_frame_copy(qtparam->param_getfcn(mergeEnt,qtparam));
			kvpTarget = kvp_frame_copy(qtparam->param_getfcn(targetEnt,qtparam));
			if(kvp_frame_compare(kvpImport, kvpTarget) == 0) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch); 
			knowntype= TRUE;
		}
		if(safe_strcmp(mergeType, QOF_TYPE_CHAR) == 0) { 
			charImport = qtparam->param_getfcn(mergeEnt,qtparam);
			charTarget = qtparam->param_getfcn(targetEnt,qtparam);
			if(charImport == charTarget) { mergeMatch = TRUE; }
			qof_book_mergeUpdateRule(mergeMatch); 
			knowntype= TRUE;
		}
		/* no need to verify the book */
		if(safe_strcmp(mergeType, QOF_ID_BOOK) == 0) { knowntype= TRUE;	}
		/* deal with non-QOF type parameters : */
		/* references to other registered QOF objects */
		if(knowntype == FALSE) {
			if(qof_class_is_registered(currentRule->mergeLabel)) {
				childEnt = g_new(QofEntity,1);
				unknown_obj = qtparam->param_getfcn(mergeEnt, qtparam);
				inst = ((QofInstance*)(unknown_obj));
				childEnt = &inst->entity;
				currentRule->linkedEntList = g_slist_prepend(currentRule->linkedEntList, childEnt);
				guidImport = qof_entity_get_guid(childEnt);
				if(guidImport != NULL) {
					guid_to_string_buff(guidImport, sa);
					stringImport = g_strdup(sa);
					printf("Test routine GUID: %s\n", stringImport);
				}
			}
		}
	g_return_val_if_fail(knowntype == TRUE, -1);
	paramList = g_slist_next(paramList);
	}
	g_free(kvpImport);
	g_free(kvpTarget);
	return 0;
}

void
qof_book_mergeCommitForeach (qof_book_mergeRuleForeachCB cb, qof_book_mergeResult mergeResult )
{
	struct qof_book_mergeRuleIterate iter;
	GList *subList;

	g_return_if_fail(cb != NULL);
	g_return_if_fail(mergeData != NULL);
	g_return_if_fail(mergeResult > 0);
	g_return_if_fail((mergeResult != MERGE_INVALID)||(mergeResult != MERGE_UNDEF)||(mergeResult != MERGE_REPORT));

	iter.fcn = cb;
	subList = NULL;
	iter.ruleList = g_list_copy(mergeData->mergeList);
	while(iter.ruleList!=NULL) {
		currentRule = iter.ruleList->data;
		if(currentRule->mergeResult == mergeResult) {
			subList = g_list_prepend(subList, currentRule);
		}
		iter.ruleList = g_list_next(iter.ruleList);
	}
	iter.remainder = g_list_length(subList);
	g_list_foreach (subList, qof_book_mergeCommitForeachCB, &iter);
}

void qof_book_mergeCommitForeachCB(gpointer lister, gpointer arg)
{
	struct qof_book_mergeRuleIterate *iter;
	
	iter = arg;
	iter->fcn ((qof_book_mergeRule*)lister, iter->remainder);
	iter->remainder--;
}


void 
qof_book_mergeForeach ( QofEntity* mergeEnt, gpointer user_data) 
{
	qof_book_mergeRule* mergeRule;
	QofEntity *targetEnt, *best_matchEnt;
	GUID *g;
	gint difference;
	GSList *c;
	
	g_return_if_fail(mergeEnt != NULL);
	g = guid_malloc();
	*g = mergeEnt->guid;
	mergeRule = g_new(qof_book_mergeRule,1);
	mergeRule->importEnt = 		mergeEnt;
	mergeRule->difference = 	difference = 0;
	mergeRule->mergeAbsolute = 	FALSE;
	mergeRule->mergeResult = 	MERGE_UNDEF;
	mergeRule->updated = 		FALSE;
	mergeRule->mergeType = 		mergeEnt->e_type;
	mergeRule->mergeLabel = 	qof_object_get_type_label(mergeEnt->e_type);
	mergeRule->mergeParam = 	g_slist_copy(mergeData->mergeObjectParams);
	mergeRule->linkedEntList =	NULL;
	currentRule = mergeRule;
	targetEnt = best_matchEnt = NULL;
	targetEnt = qof_collection_lookup_entity (
		qof_book_get_collection (mergeData->targetBook, mergeEnt->e_type), g);
	if( targetEnt != NULL) { 
		mergeRule->mergeAbsolute = TRUE;
		mergeRule->targetEnt = targetEnt;
		g_return_if_fail(qof_book_mergeCompare() != -1);
		mergeData->mergeList = g_list_prepend(mergeData->mergeList,mergeRule);
		return;
	}
	g_slist_free(mergeData->targetList);
	mergeData->targetList = NULL;
	qof_object_foreach_type(qof_book_mergeForeachTypeTarget, NULL);
	if(g_slist_length(mergeData->targetList) == 0) {
		mergeRule->mergeResult = MERGE_NEW;
	}
	difference = g_slist_length(mergeRule->mergeParam);
	c = g_slist_copy(mergeData->targetList);
	gnc_set_log_level(MOD_IMPORT, GNC_LOG_DEBUG);
	while(c != NULL) {
		mergeRule->targetEnt = c->data;
		currentRule = mergeRule;
		g_return_if_fail(qof_book_mergeCompare() != -1);
		if(mergeRule->difference == 0) {
			best_matchEnt = mergeRule->targetEnt;
			mergeRule->mergeResult = MERGE_DUPLICATE;
			g_slist_free(c);
			guid_free(g);
			return;
		}
		if(difference > mergeRule->difference) {
			best_matchEnt = mergeRule->targetEnt;
			difference = mergeRule->difference;
		}
		c = g_slist_next(c);
	}
	g_slist_free(c);
	if(best_matchEnt != NULL ) {
		mergeRule->targetEnt = best_matchEnt;
		mergeRule->difference = difference;
	}
	else {
		mergeRule->targetEnt = NULL;
		mergeRule->difference = 0;
		mergeRule->mergeResult = MERGE_NEW;
	}
	if(best_matchEnt != NULL ) {
		g_return_if_fail(qof_book_mergeCompare() != -1);
	}
	mergeData->mergeList = g_list_prepend(mergeData->mergeList,mergeRule);
	guid_free(g);
	/* return to qof_book_mergeInit */
}

void qof_book_mergeForeachTarget (QofEntity* targetEnt, gpointer user_data)
{
	g_return_if_fail(targetEnt != NULL);

	qof_book_merge_target_check(targetEnt);
}


void qof_book_merge_target_check (QofEntity* targetEnt)
{
	GList *checklist;
	qof_book_mergeRule *destination;
	const GUID *guid_ent, *guid_dest;
	gboolean exists;
	
	exists = FALSE;
	checklist = NULL;
	if(mergeData->mergeList == NULL) { return; }
	guid_ent = qof_entity_get_guid(targetEnt);
	checklist = g_list_copy(mergeData->mergeList);
	while(checklist != NULL) {
		destination = checklist->data;
		guid_dest = qof_entity_get_guid(destination->targetEnt);
		if(guid_compare(guid_ent,guid_dest) == 0) { exists = TRUE; }
		checklist = g_list_next(checklist);
	}
	if(exists == FALSE ) {
		mergeData->targetList = g_slist_prepend(mergeData->targetList,targetEnt);
	}
}

void 
qof_book_mergeForeachTypeTarget ( QofObject* merge_obj, gpointer user_data) 
{
	g_return_if_fail(merge_obj != NULL);
	if(safe_strcmp(merge_obj->e_type, currentRule->importEnt->e_type) == 0) {
		qof_object_foreach(currentRule->importEnt->e_type, mergeData->targetBook, 
			qof_book_mergeForeachTarget, NULL);
	}
}

void 
qof_book_mergeForeachType ( QofObject* merge_obj, gpointer user_data) 
{
	g_return_if_fail((merge_obj != NULL));
	/* Skip unsupported objects */
	if((merge_obj->create == NULL)||(merge_obj->foreach == NULL)){
		DEBUG (" merge_obj QOF support failed %s", merge_obj->e_type);
		return;
	}

	if(mergeData->mergeObjectParams != NULL) g_slist_free(mergeData->mergeObjectParams);
	mergeData->mergeObjectParams = NULL;
	qof_class_param_foreach(merge_obj->e_type, qof_book_mergeForeachParam , NULL);
	qof_object_foreach(merge_obj->e_type, mergeData->mergeBook, qof_book_mergeForeach, NULL);
}

void 
qof_book_mergeForeachParam( QofParam* param, gpointer user_data) 
{
	g_return_if_fail(param != NULL);
	if((param->param_getfcn != NULL)&&(param->param_setfcn != NULL)) {
		mergeData->mergeObjectParams = g_slist_append(mergeData->mergeObjectParams, param);
	}
}

void
qof_book_mergeRuleCB(gpointer lister, gpointer arg)
{
	struct qof_book_mergeRuleIterate *iter;

	g_return_if_fail(mergeData->abort == FALSE);
	iter = arg;
	iter->fcn ((qof_book_mergeRule*)lister, iter->remainder);
	iter->remainder--;
}

void qof_book_mergeCommitRuleLoop(qof_book_mergeRule *rule, guint remainder) 
{ 
	QofInstance 	*inst;
	gboolean		registered_type;
	/* cm_ prefix used for variables that hold the data to commit */
	QofParam 		*cm_param;
	char 			*cm_string, *cm_char;
	const GUID 		*cm_guid;
	KvpFrame 		*cm_kvp;
	/* function pointers and variables for parameter getters that don't use pointers normally */
	gnc_numeric 	cm_numeric, (*numeric_getter)	(QofEntity*, QofParam*);
	double 			cm_double, 	(*double_getter)	(QofEntity*, QofParam*);
	gboolean 		cm_boolean, (*boolean_getter)	(QofEntity*, QofParam*);
	gint32 			cm_i32, 	(*int32_getter)		(QofEntity*, QofParam*);
	gint64 			cm_i64, 	(*int64_getter)		(QofEntity*, QofParam*);
	Timespec 		cm_date, 	(*date_getter)		(QofEntity*, QofParam*);
	/* function pointers to the parameter setters */
	void	(*string_setter)	(QofEntity*, const char*);
	void	(*date_setter)		(QofEntity*, Timespec);
	void	(*numeric_setter)	(QofEntity*, gnc_numeric);
	void	(*guid_setter)		(QofEntity*, const GUID*);
	void	(*double_setter)	(QofEntity*, double);
	void	(*boolean_setter)	(QofEntity*, gboolean);
	void	(*i32_setter)		(QofEntity*, gint32);
	void	(*i64_setter)		(QofEntity*, gint64);
	void	(*char_setter)		(QofEntity*, char*);
	void	(*kvp_frame_setter)	(QofEntity*, KvpFrame*);

	g_return_if_fail(rule != NULL);
	g_return_if_fail((rule->mergeResult != MERGE_NEW)||(rule->mergeResult != MERGE_UPDATE));

	/* create a new object for MERGE_NEW */ 
	if(rule->mergeResult == MERGE_NEW) {
		inst = (QofInstance*)qof_object_new_instance(rule->importEnt->e_type, mergeData->targetBook);
		g_return_if_fail(inst != NULL);
		rule->targetEnt = &inst->entity;
	}
	/* currentRule->targetEnt is now set,
		1. by an absolute GUID match or
		2. by best_matchEnt and difference or
		3. by MERGE_NEW.
	*/
	registered_type = FALSE;
	while(rule->mergeParam != NULL) {
		g_return_if_fail(rule->mergeParam->data);		
		cm_param = rule->mergeParam->data;
		rule->mergeType = cm_param->param_type;
		if(safe_strcmp(rule->mergeType, QOF_TYPE_STRING) == 0)  { 
			cm_string = cm_param->param_getfcn(rule->importEnt, cm_param);
			string_setter = (void(*)(QofEntity*, const char*))cm_param->param_setfcn;
			if(string_setter != NULL) {	string_setter(rule->targetEnt, cm_string); }
			registered_type = TRUE;
		}
		if(safe_strcmp(rule->mergeType, QOF_TYPE_DATE) == 0) { 
			date_getter = (Timespec (*)(QofEntity*, QofParam*))cm_param->param_getfcn;
			cm_date = date_getter(rule->importEnt, cm_param);
			date_setter = (void(*)(QofEntity*, Timespec))cm_param->param_setfcn;
			if(date_setter != NULL) { date_setter(rule->targetEnt, cm_date); }
			registered_type = TRUE;
		}
		if((safe_strcmp(rule->mergeType, QOF_TYPE_NUMERIC) == 0)  ||
		(safe_strcmp(rule->mergeType, QOF_TYPE_DEBCRED) == 0)) { 
			numeric_getter = (gnc_numeric (*)(QofEntity*, QofParam*))cm_param->param_getfcn;
			cm_numeric = numeric_getter(rule->importEnt, cm_param);
			numeric_setter = (void(*)(QofEntity*, gnc_numeric))cm_param->param_setfcn;
			if(numeric_setter != NULL) { numeric_setter(rule->targetEnt, cm_numeric); }
			registered_type = TRUE;
		}
		if(safe_strcmp(rule->mergeType, QOF_TYPE_GUID) == 0) { 
			cm_guid = cm_param->param_getfcn(rule->importEnt, cm_param);
			guid_setter = (void(*)(QofEntity*, const GUID*))cm_param->param_setfcn;
			if(guid_setter != NULL) { guid_setter(rule->targetEnt, cm_guid); }
			registered_type = TRUE;
		}
		if(safe_strcmp(rule->mergeType, QOF_TYPE_INT32) == 0) { 
			int32_getter = (gint32 (*)(QofEntity*, QofParam*)) cm_param->param_getfcn;
			cm_i32 = int32_getter(rule->importEnt, cm_param);
			i32_setter = (void(*)(QofEntity*, gint32))cm_param->param_setfcn;
			if(i32_setter != NULL) { i32_setter(rule->targetEnt, cm_i32); }
			registered_type = TRUE;
		}
		if(safe_strcmp(rule->mergeType, QOF_TYPE_INT64) == 0) { 
			int64_getter = (gint64 (*)(QofEntity*, QofParam*)) cm_param->param_getfcn;
			cm_i64 = int64_getter(rule->importEnt, cm_param);
			i64_setter = (void(*)(QofEntity*, gint64))cm_param->param_setfcn;
			if(i64_setter != NULL) { i64_setter(rule->targetEnt, cm_i64); }
			registered_type = TRUE;
		}
		if(safe_strcmp(rule->mergeType, QOF_TYPE_DOUBLE) == 0) { 
			double_getter = (double (*)(QofEntity*, QofParam*)) cm_param->param_getfcn;
			cm_double = double_getter(rule->importEnt, cm_param);
			double_setter = (void(*)(QofEntity*, double))cm_param->param_setfcn;
			if(double_setter != NULL) { double_setter(rule->targetEnt, cm_double); }
			registered_type = TRUE;
		}
		if(safe_strcmp(rule->mergeType, QOF_TYPE_BOOLEAN) == 0){ 
			boolean_getter = (gboolean (*)(QofEntity*, QofParam*)) cm_param->param_getfcn;
			cm_boolean = boolean_getter(rule->importEnt, cm_param);
			boolean_setter = (void(*)(QofEntity*, gboolean))cm_param->param_setfcn;
			if(boolean_setter != NULL) { boolean_setter(rule->targetEnt, cm_boolean); }
			registered_type = TRUE;
		}
		if(safe_strcmp(rule->mergeType, QOF_TYPE_KVP) == 0) { 
			cm_kvp = kvp_frame_copy(cm_param->param_getfcn(rule->importEnt,cm_param));
			kvp_frame_setter = (void(*)(QofEntity*, KvpFrame*))cm_param->param_setfcn;
			if(kvp_frame_setter != NULL) { kvp_frame_setter(rule->targetEnt, cm_kvp); }
			registered_type = TRUE;
		}
		if(safe_strcmp(rule->mergeType, QOF_TYPE_CHAR) == 0) { 
			cm_char = cm_param->param_getfcn(rule->importEnt,cm_param);
			char_setter = (void(*)(QofEntity*, char*))cm_param->param_setfcn;
			if(char_setter != NULL) { char_setter(rule->targetEnt, cm_char); }
			registered_type = TRUE;
		}
		if(registered_type == FALSE) {
			if(qof_class_is_registered(rule->mergeLabel)) {
			/* need to lookup childEnt in the target book to
				ensure it has the right QofCollection */
				GSList *linkage = g_slist_copy(rule->linkedEntList);
				while(linkage != NULL) {
					QofEntity *childEnt = linkage->data;
					/* there may be more than one linked QofEntity for this rule */
					if(safe_strcmp(childEnt->e_type, rule->mergeType) == 0) {
						cm_guid = qof_entity_get_guid(childEnt);
						QofCollection *col;
    					col = qof_book_get_collection (mergeData->targetBook, rule->mergeType);
					    childEnt = qof_collection_lookup_entity (col, cm_guid);
						/* childEnt isn't used here yet. It may be too early to set */
						/* intention is to set the parameter if childEnt is not null.
						might have to store the param and set later, after Commit. */
					}
					linkage = g_slist_next(linkage);
				}
			}
		}
		rule->mergeParam = g_slist_next(rule->mergeParam);
	}
}
