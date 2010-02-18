/*********************************************************************
 * QofBookMerge.c -- api for QoFBook merge with collision handling   *
 * Copyright (C) 2004-2005 Neil Williams <linux@codehelp.co.uk>      *
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

#include "config.h"
#include <glib.h>
#include "qof.h"

static QofLogModule log_module = QOF_MOD_MERGE;

/* private rule iteration struct */
struct QofBookMergeRuleIterate
{
    QofBookMergeRuleForeachCB   fcn;
    QofBookMergeData *data;
    QofBookMergeRule *rule;
    GList *ruleList;
    guint remainder;
};

/* Make string type parameters 3 times more
	important in the match than default types.
	i.e. even if two other parameters differ,
	a string match will still provide a better target
	than when other types match and the string does not.
*/
#define DEFAULT_MERGE_WEIGHT    1
#define QOF_STRING_WEIGHT       3
#define QOF_DATE_STRING_LENGTH  MAX_DATE_LENGTH

static QofBookMergeRule*
qof_book_merge_update_rule(QofBookMergeRule *currentRule, gboolean match, gint weight)
{
    gboolean absolute;

    absolute = currentRule->mergeAbsolute;
    if (absolute && match && currentRule->mergeResult == MERGE_UNDEF)
    {
        currentRule->mergeResult = MERGE_ABSOLUTE;
    }
    if (absolute && !match)
    {
        currentRule->mergeResult = MERGE_UPDATE;
    }
    if (!absolute && match && currentRule->mergeResult == MERGE_UNDEF)
    {
        currentRule->mergeResult = MERGE_DUPLICATE;
    }
    if (!absolute && !match)
    {
        currentRule->difference += weight;
        if (currentRule->mergeResult == MERGE_DUPLICATE)
        {
            currentRule->mergeResult = MERGE_REPORT;
        }
    }
    return currentRule;
}

struct collect_list_s
{
    GSList *linkedEntList;
};

static void
collect_reference_cb (QofInstance *ent, gpointer user_data)
{
    struct collect_list_s *s;

    s = (struct collect_list_s*)user_data;
    if (!ent || !s)
    {
        return;
    }
    s->linkedEntList = g_slist_prepend(s->linkedEntList, ent);
}

static int
qof_book_merge_compare(QofBookMergeData *mergeData )
{
    QofBookMergeRule *currentRule;
    QofCollection *mergeColl, *targetColl;
    gchar      *stringImport, *stringTarget;
    QofInstance  *mergeEnt, *targetEnt, *referenceEnt;
    const GUID *guidImport, *guidTarget;
    QofParam   *qtparam;
    KvpFrame   *kvpImport, *kvpTarget;
    QofIdType  mergeParamName;
    QofType    mergeType;
    GSList    *paramList;
    gboolean  absolute, mergeError, knowntype, mergeMatch, booleanImport, booleanTarget,
           (*boolean_getter) (QofInstance*, QofParam*);
    Timespec      tsImport, tsTarget,            (*date_getter)    (QofInstance*, QofParam*);
    gnc_numeric   numericImport, numericTarget,  (*numeric_getter) (QofInstance*, QofParam*);
    double        doubleImport, doubleTarget,    (*double_getter)  (QofInstance*, QofParam*);
    gint32        i32Import, i32Target,          (*int32_getter)   (QofInstance*, QofParam*);
    gint64        i64Import, i64Target,          (*int64_getter)   (QofInstance*, QofParam*);
    gchar         charImport, charTarget,        (*char_getter)    (QofInstance*, QofParam*);

    ENTER (" ");

    g_return_val_if_fail((mergeData != NULL), -1);
    currentRule = mergeData->currentRule;
    g_return_val_if_fail((currentRule != NULL), -1);
    absolute = currentRule->mergeAbsolute;
    mergeEnt = currentRule->importEnt;
    targetEnt = currentRule->targetEnt;
    paramList = currentRule->mergeParam;
    currentRule->difference = 0;
    currentRule->mergeResult = MERGE_UNDEF;
    currentRule->linkedEntList = NULL;
    g_return_val_if_fail((targetEnt) || (mergeEnt) || (paramList), -1);
    kvpImport = kvp_frame_new();
    kvpTarget = kvp_frame_new();
    mergeError = FALSE;
    while (paramList != NULL)
    {
        mergeMatch = FALSE;
        knowntype = FALSE;
        qtparam = paramList->data;
        mergeParamName = qtparam->param_name;
        g_return_val_if_fail(mergeParamName != NULL, -1);
        mergeType = qtparam->param_type;
        if (safe_strcmp(mergeType, QOF_TYPE_STRING) == 0)
        {
            stringImport = qtparam->param_getfcn(mergeEnt, qtparam);
            stringTarget = qtparam->param_getfcn(targetEnt, qtparam);
            /* very strict string matches may need to be relaxed. */
            if (stringImport == NULL)
            {
                stringImport = "";
            }
            if (stringTarget == NULL)
            {
                stringTarget = "";
            }
            if (safe_strcmp(stringImport, stringTarget) == 0)
            {
                mergeMatch = TRUE;
            }
            /* Give special weight to a string match */
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, QOF_STRING_WEIGHT);
            stringImport = stringTarget = NULL;
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_DATE) == 0)
        {
            date_getter = (Timespec (*)(QofInstance*, QofParam*))qtparam->param_getfcn;
            tsImport = date_getter(mergeEnt, qtparam);
            tsTarget = date_getter(targetEnt, qtparam);
            if (timespec_cmp(&tsImport, &tsTarget) == 0)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if ((safe_strcmp(mergeType, QOF_TYPE_NUMERIC) == 0)  ||
                (safe_strcmp(mergeType, QOF_TYPE_DEBCRED) == 0))
        {
            numeric_getter = (gnc_numeric (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
            numericImport = numeric_getter(mergeEnt, qtparam);
            numericTarget = numeric_getter(targetEnt, qtparam);
            if (gnc_numeric_compare (numericImport, numericTarget) == 0)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_GUID) == 0)
        {
            guidImport = qtparam->param_getfcn(mergeEnt, qtparam);
            guidTarget = qtparam->param_getfcn(targetEnt, qtparam);
            if (guid_compare(guidImport, guidTarget) == 0)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_INT32) == 0)
        {
            int32_getter = (gint32 (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
            i32Import = int32_getter(mergeEnt, qtparam);
            i32Target = int32_getter(targetEnt, qtparam);
            if (i32Target == i32Import)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_INT64) == 0)
        {
            int64_getter = (gint64 (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
            i64Import = int64_getter(mergeEnt, qtparam);
            i64Target = int64_getter(targetEnt, qtparam);
            if (i64Target == i64Import)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_DOUBLE) == 0)
        {
            double_getter = (double (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
            doubleImport = double_getter(mergeEnt, qtparam);
            doubleTarget = double_getter(mergeEnt, qtparam);
            if (doubleImport == doubleTarget)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_BOOLEAN) == 0)
        {
            boolean_getter = (gboolean (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
            booleanImport = boolean_getter(mergeEnt, qtparam);
            booleanTarget = boolean_getter(targetEnt, qtparam);
            if (booleanImport != FALSE && booleanImport != TRUE)
            {
                booleanImport = FALSE;
            }
            if (booleanTarget != FALSE && booleanTarget != TRUE)
            {
                booleanTarget = FALSE;
            }
            if (booleanImport == booleanTarget)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_KVP) == 0)
        {
            kvpImport = kvp_frame_copy(qtparam->param_getfcn(mergeEnt, qtparam));
            kvpTarget = kvp_frame_copy(qtparam->param_getfcn(targetEnt, qtparam));
            if (kvp_frame_compare(kvpImport, kvpTarget) == 0)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_CHAR) == 0)
        {
            char_getter = (gchar (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
            charImport = char_getter(mergeEnt, qtparam);
            charTarget = char_getter(targetEnt, qtparam);
            if (charImport == charTarget)
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        /* No object should have QofSetterFunc defined for the book,
            but just to be safe, do nothing. */
        if (safe_strcmp(mergeType, QOF_ID_BOOK) == 0)
        {
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_COLLECT) == 0)
        {
            struct collect_list_s s;
            s.linkedEntList = NULL;
            mergeColl = qtparam->param_getfcn(mergeEnt, qtparam);
            targetColl = qtparam->param_getfcn(targetEnt, qtparam);
            s.linkedEntList = g_slist_copy(currentRule->linkedEntList);
            qof_collection_foreach(mergeColl, collect_reference_cb, &s);
            currentRule->linkedEntList = g_slist_copy(s.linkedEntList);
            if (0 == qof_collection_compare(mergeColl, targetColl))
            {
                mergeMatch = TRUE;
            }
            currentRule = qof_book_merge_update_rule(currentRule,
                          mergeMatch, DEFAULT_MERGE_WEIGHT);
            knowntype = TRUE;
        }
        if (safe_strcmp(mergeType, QOF_TYPE_CHOICE) == 0)
        {
            referenceEnt = qtparam->param_getfcn(mergeEnt, qtparam);
            currentRule->linkedEntList =
                g_slist_prepend(currentRule->linkedEntList, referenceEnt);
            if (referenceEnt == qtparam->param_getfcn(targetEnt, qtparam))
            {
                mergeMatch = TRUE;
            }
            knowntype = TRUE;
        }
        if (knowntype == FALSE)
        {
            referenceEnt = qtparam->param_getfcn(mergeEnt, qtparam);

            // XXX gncOwner is na object that could be returned, but does not have QofInstance
            if (safe_strcmp(qtparam->param_type, "gncOwner") == 0)
                referenceEnt = NULL;

            if ((referenceEnt != NULL)
                    && (safe_strcmp(referenceEnt->e_type, mergeType) == 0))
            {
                currentRule->linkedEntList =
                    g_slist_prepend(currentRule->linkedEntList, referenceEnt);
                if (referenceEnt == qtparam->param_getfcn(targetEnt, qtparam))
                {
                    mergeMatch = TRUE;
                }
                currentRule = qof_book_merge_update_rule(currentRule,
                              mergeMatch, DEFAULT_MERGE_WEIGHT);
            }
        }
        paramList = g_slist_next(paramList);
    }
    mergeData->currentRule = currentRule;
    g_free(kvpImport);
    g_free(kvpTarget);

    LEAVE (" ");
    return 0;
}

static void
qof_book_merge_commit_foreach_cb(gpointer rule, gpointer arg)
{
    struct QofBookMergeRuleIterate *iter;

    g_return_if_fail(arg != NULL);
    iter = (struct QofBookMergeRuleIterate*)arg;
    g_return_if_fail(iter->data != NULL);
    iter->fcn (iter->data, (QofBookMergeRule*)rule, iter->remainder);
    iter->remainder--;
}

static void
qof_book_merge_commit_foreach (QofBookMergeRuleForeachCB cb,
                               QofBookMergeResult mergeResult,
                               QofBookMergeData *mergeData)
{
    struct QofBookMergeRuleIterate iter;
    QofBookMergeRule *currentRule;
    GList *subList, *node;

    g_return_if_fail(cb != NULL);
    g_return_if_fail(mergeData != NULL);
    currentRule = mergeData->currentRule;
    g_return_if_fail(currentRule != NULL);
    g_return_if_fail(mergeResult > 0);
    g_return_if_fail((mergeResult != MERGE_INVALID) || (mergeResult != MERGE_UNDEF) || (mergeResult != MERGE_REPORT));

    iter.fcn = cb;
    subList = NULL;
    iter.ruleList = NULL;
    for (node = mergeData->mergeList; node != NULL; node = node->next)
    {
        currentRule = node->data;
        if (currentRule->mergeResult == mergeResult)
        {
            subList = g_list_prepend(subList, currentRule);
        }
    }
    iter.remainder = g_list_length(subList);
    iter.data = mergeData;
    g_list_foreach (subList, qof_book_merge_commit_foreach_cb, &iter);
}

/* build the table of target comparisons

This can get confusing, so bear with me. (!)

Whilst iterating through the entities in the mergeBook, qof_book_mergeForeach assigns
a targetEnt to each mergeEnt (until it runs out of targetEnt or mergeEnt). Each match
is made against the one targetEnt that best matches the mergeEnt. Fine so far.

Any mergeEnt is only ever assigned a targetEnt if the calculated difference between
the two is less than the difference between that targetEnt and any previous mergeEnt
match.

The next mergeEnt may be a much better match for that targetEnt and the target_table
is designed to solve the issues that result from this conflict. The previous match
must be re-assigned because if two mergeEnt's are matched with only one targetEnt,
data loss \b WILL follow. Equally, the current mergeEnt must replace the previous
one as it is a better match. qof_instance_rating holds the details required to identify
the correct mergeEnt to be re-assigned and these mergeEnt entities are therefore
orphaned - to be re-matched later.

Meanwhile, the current mergeEnt is entered into target_table with it's difference and
rule data, in case an even better match is found later in the mergeBook.

Finally, each mergeEnt in the orphan_list is now put through the comparison again.

*/
static gboolean
qof_book_merge_rule_cmp(gconstpointer a, gconstpointer b)
{
    QofBookMergeRule *ra = (QofBookMergeRule *) a;
    QofBookMergeRule *rb = (QofBookMergeRule *) b;
    if (ra->difference == rb->difference)
    {
        return TRUE;
    }
    else return FALSE;
}

static void
qof_book_merge_orphan_check(double difference, QofBookMergeRule *mergeRule,
                            QofBookMergeData *mergeData)
{
    /* Called when difference is lower than previous
    	Lookup target to find previous match
    	and re-assign mergeEnt to orphan_list */
    QofBookMergeRule *rule;

    g_return_if_fail(mergeRule != NULL);
    g_return_if_fail(mergeData != NULL);
    if (g_hash_table_size(mergeData->target_table) == 0)
    {
        return;
    }
    rule = (QofBookMergeRule*)g_hash_table_lookup(mergeData->target_table,
            mergeRule->targetEnt);
    /* If NULL, no match was found. */
    if (rule == NULL)
    {
        return;
    }
    /* Only orphan if this is a better match than already exists. */
    if (difference >= rule->difference)
    {
        return;
    }
    rule->targetEnt = NULL;
    rule->mergeResult = MERGE_UNDEF;
    mergeData->orphan_list = g_slist_append(mergeData->orphan_list, rule);
}

static void
qof_book_merge_match_orphans(QofBookMergeData *mergeData)
{
    GSList *orphans, *targets;
    QofBookMergeRule *rule, *currentRule;
    QofInstance *best_matchEnt;
    double difference;

    ENTER (" ");

    g_return_if_fail(mergeData != NULL);
    currentRule = mergeData->currentRule;
    g_return_if_fail(currentRule != NULL);
    /* This routine does NOT copy the orphan list, it
    	is used recursively until empty. */
    orphans = mergeData->orphan_list;
    targets = g_slist_copy(mergeData->targetList);
    while (orphans != NULL)
    {
        rule = orphans->data;
        g_return_if_fail(rule != NULL);
        difference = g_slist_length(mergeData->mergeObjectParams);
        if (rule->targetEnt == NULL)
        {
            rule->mergeResult = MERGE_NEW;
            rule->difference = 0;
            mergeData->mergeList = g_list_prepend(mergeData->mergeList, rule);
            orphans = g_slist_next(orphans);
            continue;
        }
        mergeData->currentRule = rule;
        g_return_if_fail(qof_book_merge_compare(mergeData) != -1);
        if (difference > mergeData->currentRule->difference)
        {
            best_matchEnt = currentRule->targetEnt;
            difference = currentRule->difference;
            rule = currentRule;
            mergeData->mergeList = g_list_prepend(mergeData->mergeList, rule);
            qof_book_merge_orphan_check(difference, rule, mergeData);
        }
        orphans = g_slist_next(orphans);
    }
    g_slist_free(mergeData->orphan_list);
    g_slist_free(targets);

    LEAVE (" ");
}

static void
qof_book_merge_foreach_target (QofInstance* targetEnt, gpointer user_data)
{
    QofBookMergeData *mergeData;

    g_return_if_fail(user_data != NULL);
    mergeData = (QofBookMergeData*)user_data;
    g_return_if_fail(targetEnt != NULL);
    mergeData->targetList = g_slist_prepend(mergeData->targetList, targetEnt);
}

static void
qof_book_merge_foreach_type_target ( QofObject* merge_obj, gpointer user_data)
{
    QofBookMergeData *mergeData;
    QofBookMergeRule *currentRule;

    g_return_if_fail(user_data != NULL);
    mergeData = (QofBookMergeData*)user_data;
    currentRule = mergeData->currentRule;
    g_return_if_fail(currentRule != NULL);
    g_return_if_fail(merge_obj != NULL);
    if (safe_strcmp(merge_obj->e_type, currentRule->importEnt->e_type) == 0)
    {
        qof_object_foreach(currentRule->importEnt->e_type, mergeData->targetBook,
                           qof_book_merge_foreach_target, user_data);
    }
}

static void
qof_book_merge_foreach ( QofInstance* mergeEnt, gpointer user_data)
{
    QofBookMergeRule *mergeRule, *currentRule;
    QofBookMergeData *mergeData;
    QofInstance *targetEnt, *best_matchEnt;
    GUID *g;
    double difference;
    GSList *c;

    ENTER (" ");

    g_return_if_fail(user_data != NULL);
    mergeData = (QofBookMergeData*)user_data;
    g_return_if_fail(mergeEnt != NULL);
    currentRule = mergeData->currentRule;
    g_return_if_fail(currentRule != NULL);
    g = guid_copy(qof_instance_get_guid(mergeEnt));
    mergeRule = g_new0(QofBookMergeRule, 1);
    mergeRule->importEnt = 		mergeEnt;
    mergeRule->difference = 	difference = 0;
    mergeRule->mergeAbsolute = 	FALSE;
    mergeRule->mergeResult = 	MERGE_UNDEF;
    mergeRule->updated = 		FALSE;
    mergeRule->mergeType = 		mergeEnt->e_type;
    mergeRule->mergeLabel = 	qof_object_get_type_label(mergeEnt->e_type);
    mergeRule->mergeParam = 	g_slist_copy(mergeData->mergeObjectParams);
    mergeRule->linkedEntList =	NULL;
    mergeData->currentRule = mergeRule;
    targetEnt = best_matchEnt = NULL;
    targetEnt = qof_collection_lookup_entity (
                    qof_book_get_collection (mergeData->targetBook, mergeEnt->e_type), g);
    if ( targetEnt != NULL)
    {
        mergeRule->mergeAbsolute = TRUE;
        mergeRule->targetEnt = targetEnt;
        g_return_if_fail(qof_book_merge_compare(mergeData) != -1);
        mergeRule->linkedEntList = g_slist_copy(currentRule->linkedEntList);
        mergeData->mergeList = g_list_prepend(mergeData->mergeList, mergeRule);
        return;
    }
    /* no absolute match exists */
    g_slist_free(mergeData->targetList);
    mergeData->targetList = NULL;
    qof_object_foreach_type(qof_book_merge_foreach_type_target, mergeData);
    if (g_slist_length(mergeData->targetList) == 0)
    {
        mergeRule->mergeResult = MERGE_NEW;
    }
    difference = g_slist_length(mergeRule->mergeParam);
    c = g_slist_copy(mergeData->targetList);
    while (c != NULL)
    {
        mergeRule->targetEnt = c->data;
        currentRule = mergeRule;
        /* compare two entities and sum the differences */
        g_return_if_fail(qof_book_merge_compare(mergeData) != -1);
        if (mergeRule->difference == 0)
        {
            /* check if this is a better match than one already assigned */
            best_matchEnt = mergeRule->targetEnt;
            mergeRule->mergeResult = MERGE_DUPLICATE;
            difference = 0;
            mergeRule->linkedEntList = g_slist_copy(currentRule->linkedEntList);
            g_slist_free(c);
            guid_free(g);
            /* exact match, return */
            return;
        }
        if (difference > mergeRule->difference)
        {
            /* The chosen targetEnt determines the parenting of any child object */
            /* check if this is a better match than one already assigned */
            best_matchEnt = mergeRule->targetEnt;
            difference = mergeRule->difference;
            /* Use match to lookup the previous entity that matched this targetEnt (if any)
            	and remove targetEnt from the rule for that mergeEnt.
            	Add the previous mergeEnt to orphan_list.
            */
            qof_book_merge_orphan_check(difference, mergeRule, mergeData);
        }
        c = g_slist_next(c);
    }
    g_slist_free(c);
    if (best_matchEnt != NULL )
    {
        mergeRule->targetEnt = best_matchEnt;
        mergeRule->difference = difference;
        /* Set this entity in the target_table in case a better match can be made
        	with the next mergeEnt. */
        g_hash_table_insert(mergeData->target_table, mergeRule->targetEnt, mergeRule);
        /* compare again with the best partial match */
        g_return_if_fail(qof_book_merge_compare(mergeData) != -1);
        mergeRule->linkedEntList = g_slist_copy(currentRule->linkedEntList);
    }
    else
    {
        mergeRule->targetEnt = NULL;
        mergeRule->difference = 0;
        mergeRule->mergeResult = MERGE_NEW;
        mergeRule->linkedEntList = g_slist_copy(currentRule->linkedEntList);
    }
    mergeData->mergeList = g_list_prepend(mergeData->mergeList, mergeRule);
    guid_free(g);
    /* return to qof_book_merge_init */

    LEAVE (" ");
}

static void
qof_book_merge_foreach_param( QofParam* param, gpointer user_data)
{
    QofBookMergeData *mergeData;

    g_return_if_fail(user_data != NULL);
    mergeData = (QofBookMergeData*)user_data;
    g_return_if_fail(param != NULL);
    if ((param->param_getfcn != NULL) && (param->param_setfcn != NULL))
    {
        mergeData->mergeObjectParams = g_slist_append(mergeData->mergeObjectParams, param);
    }
}

static void
qof_book_merge_foreach_type ( QofObject* merge_obj, gpointer user_data)
{
    QofBookMergeData *mergeData;

    g_return_if_fail(user_data != NULL);
    mergeData = (QofBookMergeData*)user_data;
    g_return_if_fail((merge_obj != NULL));
    /* Skip unsupported objects */
    if ((merge_obj->create == NULL) || (merge_obj->foreach == NULL))
    {
        DEBUG (" merge_obj QOF support failed %s", merge_obj->e_type);
        return;
    }
    if (mergeData->mergeObjectParams != NULL) g_slist_free(mergeData->mergeObjectParams);
    mergeData->mergeObjectParams = NULL;
    qof_class_param_foreach(merge_obj->e_type, qof_book_merge_foreach_param , mergeData);
    qof_object_foreach(merge_obj->e_type, mergeData->mergeBook,
                       qof_book_merge_foreach, mergeData);
}

static void
qof_book_merge_rule_cb(gpointer rule, gpointer arg)
{
    struct QofBookMergeRuleIterate *iter;
    QofBookMergeData *mergeData;

    g_return_if_fail(arg != NULL);
    iter = (struct QofBookMergeRuleIterate*)arg;
    mergeData = iter->data;
    g_return_if_fail(mergeData != NULL);
    g_return_if_fail(mergeData->abort == FALSE);
    iter->fcn (mergeData, (QofBookMergeRule*)rule, iter->remainder);
    iter->data = mergeData;
    iter->remainder--;
}


/**
 * Creates an object when the MergeResult is MERGE_NEW. Called for each MergeRule.
 */
static void
qof_book_merge_commit_rule_create_objects(QofBookMergeData *mergeData,
        QofBookMergeRule *rule,
        guint remainder)
{
    QofInstance *inst;

    g_return_if_fail(rule != NULL);
    g_return_if_fail(mergeData != NULL);
    g_return_if_fail(mergeData->targetBook != NULL);
    g_return_if_fail(rule->mergeResult == MERGE_NEW);

    /* The new object takes the GUID from the import to retain an absolute match */
    inst = qof_object_new_instance(rule->importEnt->e_type, mergeData->targetBook);
    g_return_if_fail(inst != NULL);
    rule->targetEnt = inst;
    qof_instance_copy_guid(rule->targetEnt, rule->importEnt);
}


/**
 * Returns the corresponding target entity to the given importEnt
 */
static QofInstance*
qof_book_merge_map_entity(const QofBookMergeData *mergeData, const QofInstance* importEnt)
{
    QofBookMergeRule *currentRule;
    GList *node;

    for (node = mergeData->mergeList; node != NULL; node = node->next)
    {
        currentRule = node->data;
        if (currentRule->importEnt == importEnt)
        {
            return currentRule->targetEnt;
        }
    }
    PINFO ("qof_book_merge_map_entity: Import Entity not found");
    return NULL;
}

typedef struct
{
    QofBookMergeData *mergeData;
    QofCollection *mapped_coll;
} QofBookMergeMapCollectionIterate;


/**
 * Map all entities given in importEnt and add them into the mapped_coll
 */
static void
qof_book_merge_map_collection_cb(QofInstance* importEnt, gpointer user_data)
{
    QofBookMergeMapCollectionIterate *mapped_coll_iter;
    QofInstance *targetEnt;

    mapped_coll_iter = (QofBookMergeMapCollectionIterate*)user_data;
    targetEnt = qof_book_merge_map_entity(mapped_coll_iter->mergeData, importEnt);
    qof_collection_add_entity(mapped_coll_iter->mapped_coll, targetEnt);
}


static void
qof_book_merge_commit_rule_loop(QofBookMergeData *mergeData,
                                QofBookMergeRule *rule,
                                guint remainder)
{
    gboolean    registered_type;
    QofInstance   *referenceEnt;
    /* cm_ prefix used for variables that hold the data to commit */
    QofCollection *cm_coll, *mapped_coll;
    QofBookMergeMapCollectionIterate mapped_coll_iter;
    QofParam    *cm_param;
    gchar       *cm_string;
    const GUID  *cm_guid;
    KvpFrame    *cm_kvp;
    /* function pointers and variables for parameter getters that don't use pointers normally */
    gnc_numeric  cm_numeric, (*numeric_getter)  (QofInstance*, QofParam*);
    double       cm_double,  (*double_getter)   (QofInstance*, QofParam*);
    gboolean     cm_boolean, (*boolean_getter)  (QofInstance*, QofParam*);
    gint32       cm_i32,     (*int32_getter)    (QofInstance*, QofParam*);
    gint64       cm_i64,     (*int64_getter)    (QofInstance*, QofParam*);
    Timespec     cm_date,    (*date_getter)     (QofInstance*, QofParam*);
    gchar        cm_char,    (*char_getter)     (QofInstance*, QofParam*);
    /* function pointers to the parameter setters */
    void (*string_setter)    (QofInstance*, const gchar*);
    void (*date_setter)      (QofInstance*, Timespec);
    void (*numeric_setter)   (QofInstance*, gnc_numeric);
    void (*guid_setter)      (QofInstance*, const GUID*);
    void (*double_setter)    (QofInstance*, double);
    void (*boolean_setter)   (QofInstance*, gboolean);
    void (*i32_setter)       (QofInstance*, gint32);
    void (*i64_setter)       (QofInstance*, gint64);
    void (*char_setter)      (QofInstance*, gchar);
    void (*kvp_frame_setter) (QofInstance*, KvpFrame*);
    void (*reference_setter) (QofInstance*, QofInstance*);
    void (*collection_setter)(QofInstance*, QofCollection*);

    g_return_if_fail(rule != NULL);
    g_return_if_fail(mergeData != NULL);
    g_return_if_fail(mergeData->targetBook != NULL);
    g_return_if_fail(rule->importEnt && rule->targetEnt);
    g_return_if_fail((rule->mergeResult != MERGE_NEW) || (rule->mergeResult != MERGE_UPDATE));

    DEBUG ("qof_book_merge_commit_rule_loop rule: type: %s, result: %s, importEnt Type: %s, guid: %s",
           rule->mergeType, rule->mergeResult == MERGE_NEW ? "New" : "Update",
           rule->importEnt->e_type,
           guid_to_string(qof_instance_get_guid(rule->importEnt)));
    DEBUG ("qof_book_merge_commit_rule_loop rule (cont.): targetEnt Type: %s, guid: %s",
           rule->targetEnt->e_type,
           guid_to_string(qof_instance_get_guid(rule->targetEnt)));

    /* currentRule->targetEnt is now set,
    	1. by an absolute GUID match or
    	2. by best_matchEnt and difference or
    	3. by MERGE_NEW.
    */
    while (rule->mergeParam != NULL)
    {
        registered_type = FALSE;
        g_return_if_fail(rule->mergeParam->data);
        cm_param = rule->mergeParam->data;
        rule->mergeType = cm_param->param_type;

        DEBUG ("qof_book_merge_commit_rule_loop param: Merge Type: %s, Param Name: %s",
               rule->mergeType, cm_param->param_name);

        if (safe_strcmp(rule->mergeType, QOF_TYPE_STRING) == 0)
        {
            cm_string = cm_param->param_getfcn(rule->importEnt, cm_param);
            string_setter = (void(*)(QofInstance*, const gchar*))cm_param->param_setfcn;
            if (string_setter != NULL)
            {
                string_setter(rule->targetEnt, cm_string);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_DATE) == 0)
        {
            date_getter = (Timespec (*)(QofInstance*, QofParam*))cm_param->param_getfcn;
            cm_date = date_getter(rule->importEnt, cm_param);
            date_setter = (void(*)(QofInstance*, Timespec))cm_param->param_setfcn;
            if (date_setter != NULL)
            {
                date_setter(rule->targetEnt, cm_date);
            }
            registered_type = TRUE;
        }
        if ((safe_strcmp(rule->mergeType, QOF_TYPE_NUMERIC) == 0)  ||
                (safe_strcmp(rule->mergeType, QOF_TYPE_DEBCRED) == 0))
        {
            numeric_getter = (gnc_numeric (*)(QofInstance*, QofParam*))cm_param->param_getfcn;
            cm_numeric = numeric_getter(rule->importEnt, cm_param);
            numeric_setter = (void(*)(QofInstance*, gnc_numeric))cm_param->param_setfcn;
            if (numeric_setter != NULL)
            {
                numeric_setter(rule->targetEnt, cm_numeric);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_GUID) == 0)
        {
            cm_guid = cm_param->param_getfcn(rule->importEnt, cm_param);
            guid_setter = (void(*)(QofInstance*, const GUID*))cm_param->param_setfcn;
            if (guid_setter != NULL)
            {
                guid_setter(rule->targetEnt, cm_guid);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_INT32) == 0)
        {
            int32_getter = (gint32 (*)(QofInstance*, QofParam*)) cm_param->param_getfcn;
            cm_i32 = int32_getter(rule->importEnt, cm_param);
            i32_setter = (void(*)(QofInstance*, gint32))cm_param->param_setfcn;
            if (i32_setter != NULL)
            {
                i32_setter(rule->targetEnt, cm_i32);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_INT64) == 0)
        {
            int64_getter = (gint64 (*)(QofInstance*, QofParam*)) cm_param->param_getfcn;
            cm_i64 = int64_getter(rule->importEnt, cm_param);
            i64_setter = (void(*)(QofInstance*, gint64))cm_param->param_setfcn;
            if (i64_setter != NULL)
            {
                i64_setter(rule->targetEnt, cm_i64);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_DOUBLE) == 0)
        {
            double_getter = (double (*)(QofInstance*, QofParam*)) cm_param->param_getfcn;
            cm_double = double_getter(rule->importEnt, cm_param);
            double_setter = (void(*)(QofInstance*, double))cm_param->param_setfcn;
            if (double_setter != NULL)
            {
                double_setter(rule->targetEnt, cm_double);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_BOOLEAN) == 0)
        {
            boolean_getter = (gboolean (*)(QofInstance*, QofParam*)) cm_param->param_getfcn;
            cm_boolean = boolean_getter(rule->importEnt, cm_param);
            boolean_setter = (void(*)(QofInstance*, gboolean))cm_param->param_setfcn;
            if (boolean_setter != NULL)
            {
                boolean_setter(rule->targetEnt, cm_boolean);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_KVP) == 0)
        {
            cm_kvp = kvp_frame_copy(cm_param->param_getfcn(rule->importEnt, cm_param));
            kvp_frame_setter = (void(*)(QofInstance*, KvpFrame*))cm_param->param_setfcn;
            if (kvp_frame_setter != NULL)
            {
                kvp_frame_setter(rule->targetEnt, cm_kvp);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_CHAR) == 0)
        {
            char_getter = (gchar (*)(QofInstance*, QofParam*)) cm_param->param_getfcn;
            cm_char = char_getter(rule->importEnt, cm_param);
            char_setter = (void(*)(QofInstance*, gchar))cm_param->param_setfcn;
            if (char_setter != NULL)
            {
                char_setter(rule->targetEnt, cm_char);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_COLLECT) == 0)
        {
            cm_coll = cm_param->param_getfcn(rule->importEnt, cm_param);

            /* Created mapped collection */
            mapped_coll = qof_collection_new(qof_collection_get_type(cm_coll));
            mapped_coll_iter.mergeData = mergeData;
            mapped_coll_iter.mapped_coll = mapped_coll;
            qof_collection_foreach(cm_coll, qof_book_merge_map_collection_cb, &mapped_coll_iter);

            collection_setter = (void(*)(QofInstance*, QofCollection*))cm_param->param_setfcn;
            if (collection_setter != NULL)
            {
                collection_setter(rule->targetEnt, mapped_coll);
            }
            registered_type = TRUE;
        }
        if (safe_strcmp(rule->mergeType, QOF_TYPE_CHOICE) == 0)
        {
            referenceEnt = cm_param->param_getfcn(rule->importEnt, cm_param);
            referenceEnt = qof_book_merge_map_entity(mergeData, referenceEnt);
            reference_setter = (void(*)(QofInstance*, QofInstance*))cm_param->param_setfcn;
            if (reference_setter != NULL)
            {
                reference_setter(rule->targetEnt, referenceEnt);
            }
            registered_type = TRUE;
        }
        if (registered_type == FALSE)
        {
            referenceEnt = cm_param->param_getfcn(rule->importEnt, cm_param);

            // XXX gncOwner is an object that could be returned, but does not have QofInstance
            if (safe_strcmp(cm_param->param_type, "gncOwner") == 0)
                referenceEnt = NULL;

            referenceEnt = qof_book_merge_map_entity(mergeData, referenceEnt);

            if (referenceEnt)
            {
                reference_setter = (void(*)(QofInstance*, QofInstance*))cm_param->param_setfcn;
                if (reference_setter != NULL)
                {
                    reference_setter(rule->targetEnt, referenceEnt);
                }
            }
        }
        rule->mergeParam = g_slist_next(rule->mergeParam);
    }
}
/* ================================================================ */
/* API functions. */

QofBookMergeData*
qof_book_merge_init( QofBook *importBook, QofBook *targetBook)
{
    QofBookMergeData *mergeData;
    QofBookMergeRule *currentRule;
    GList *node;

    ENTER (" ");

    g_return_val_if_fail((importBook != NULL) && (targetBook != NULL), NULL);
    mergeData = g_new0(QofBookMergeData, 1);
    mergeData->abort = FALSE;
    mergeData->mergeList = NULL;
    mergeData->targetList = NULL;
    mergeData->mergeBook = importBook;
    mergeData->targetBook = targetBook;
    mergeData->mergeObjectParams = NULL;
    mergeData->orphan_list = NULL;
    mergeData->target_table = g_hash_table_new( g_direct_hash, qof_book_merge_rule_cmp);
    currentRule = g_new0(QofBookMergeRule, 1);
    mergeData->currentRule = currentRule;
    qof_object_foreach_type(qof_book_merge_foreach_type, mergeData);
    g_return_val_if_fail(mergeData->mergeObjectParams, NULL);
    if (mergeData->orphan_list != NULL)
    {
        qof_book_merge_match_orphans(mergeData);
    }

    for (node = mergeData->mergeList; node != NULL; node = node->next)
    {
        currentRule = node->data;
        if (currentRule->mergeResult == MERGE_INVALID)
        {
            mergeData->abort = TRUE;
            return(NULL);
        }
    }

    LEAVE (" ");
    return mergeData;
}

void
qof_book_merge_abort (QofBookMergeData *mergeData)
{
    QofBookMergeRule *currentRule;

    g_return_if_fail(mergeData != NULL);
    while (mergeData->mergeList != NULL)
    {
        currentRule = mergeData->mergeList->data;
        g_slist_free(currentRule->linkedEntList);
        g_slist_free(currentRule->mergeParam);
        g_free(mergeData->mergeList->data);
        if (currentRule)
        {
            g_slist_free(currentRule->linkedEntList);
            g_slist_free(currentRule->mergeParam);
            g_free(currentRule);
        }
        mergeData->mergeList = g_list_next(mergeData->mergeList);
    }
    g_list_free(mergeData->mergeList);
    g_slist_free(mergeData->mergeObjectParams);
    g_slist_free(mergeData->targetList);
    if (mergeData->orphan_list != NULL)
    {
        g_slist_free(mergeData->orphan_list);
    }
    g_hash_table_destroy(mergeData->target_table);
    g_free(mergeData);
}

/* The QOF_TYPE_DATE output format from
qof_book_merge_param_as_string has been changed to QSF_XSD_TIME,
a UTC formatted timestring: 2005-01-01T10:55:23Z
If you change QOF_UTC_DATE_FORMAT, change
backend/file/qsf-xml.c : qsf_entity_foreach to
reformat to QSF_XSD_TIME or the QSF XML will
FAIL the schema validation and QSF exports will become invalid.

The QOF_TYPE_BOOLEAN is lowercase for the same reason.

\todo deprecate and replace with
gchar* qof_instance_param_as_string(const QofParam*, QofInstance*);
and then add
gchar* qof_class_get_param_as_string(QofIdTypeConst, QofInstance*); ?
*/
gchar*
qof_book_merge_param_as_string(QofParam *qtparam, QofInstance *qtEnt)
{
    gchar       *param_string, param_date[QOF_DATE_STRING_LENGTH];
    gchar       param_sa[GUID_ENCODING_LENGTH + 1];
    QofType     paramType;
    const GUID *param_guid;
    time_t      param_t;
    gnc_numeric param_numeric,  (*numeric_getter) (QofInstance*, QofParam*);
    Timespec    param_ts,       (*date_getter)    (QofInstance*, QofParam*);
    double      param_double,   (*double_getter)  (QofInstance*, QofParam*);
    gboolean    param_boolean,  (*boolean_getter) (QofInstance*, QofParam*);
    gint32      param_i32,      (*int32_getter)   (QofInstance*, QofParam*);
    gint64      param_i64,      (*int64_getter)   (QofInstance*, QofParam*);
    gchar       param_char,     (*char_getter)    (QofInstance*, QofParam*);

    param_string = NULL;
    paramType = qtparam->param_type;
    if (safe_strcmp(paramType, QOF_TYPE_STRING) == 0)
    {
        param_string = g_strdup(qtparam->param_getfcn(qtEnt, qtparam));
        if (param_string == NULL)
        {
            param_string = "";
        }
        return param_string;
    }
    if (safe_strcmp(paramType, QOF_TYPE_DATE) == 0)
    {
        date_getter = (Timespec (*)(QofInstance*, QofParam*))qtparam->param_getfcn;
        param_ts = date_getter(qtEnt, qtparam);
        param_t = timespecToTime_t(param_ts);
        qof_strftime(param_date, QOF_DATE_STRING_LENGTH, QOF_UTC_DATE_FORMAT, gmtime(&param_t));
        param_string = g_strdup(param_date);
        return param_string;
    }
    if ((safe_strcmp(paramType, QOF_TYPE_NUMERIC) == 0)  ||
            (safe_strcmp(paramType, QOF_TYPE_DEBCRED) == 0))
    {
        numeric_getter = (gnc_numeric (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
        param_numeric = numeric_getter(qtEnt, qtparam);
        param_string = g_strdup(gnc_numeric_to_string(param_numeric));
        return param_string;
    }
    if (safe_strcmp(paramType, QOF_TYPE_GUID) == 0)
    {
        param_guid = qtparam->param_getfcn(qtEnt, qtparam);
        guid_to_string_buff(param_guid, param_sa);
        param_string = g_strdup(param_sa);
        return param_string;
    }
    if (safe_strcmp(paramType, QOF_TYPE_INT32) == 0)
    {
        int32_getter = (gint32 (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
        param_i32 = int32_getter(qtEnt, qtparam);
        param_string = g_strdup_printf("%d", param_i32);
        return param_string;
    }
    if (safe_strcmp(paramType, QOF_TYPE_INT64) == 0)
    {
        int64_getter = (gint64 (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
        param_i64 = int64_getter(qtEnt, qtparam);
        param_string = g_strdup_printf("%" G_GINT64_FORMAT, param_i64);
        return param_string;
    }
    if (safe_strcmp(paramType, QOF_TYPE_DOUBLE) == 0)
    {
        double_getter = (double (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
        param_double = double_getter(qtEnt, qtparam);
        param_string = g_strdup_printf("%f", param_double);
        return param_string;
    }
    if (safe_strcmp(paramType, QOF_TYPE_BOOLEAN) == 0)
    {
        boolean_getter = (gboolean (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
        param_boolean = boolean_getter(qtEnt, qtparam);
        /* Boolean values need to be lowercase for QSF validation. */
        if (param_boolean == TRUE)
        {
            param_string = g_strdup("true");
        }
        else
        {
            param_string = g_strdup("false");
        }
        return param_string;
    }
    /* "kvp" contains repeating values, cannot be a single string for the frame. */
    if (safe_strcmp(paramType, QOF_TYPE_KVP) == 0)
    {
        return param_string;
    }
    if (safe_strcmp(paramType, QOF_TYPE_CHAR) == 0)
    {
        char_getter = (gchar (*)(QofInstance*, QofParam*)) qtparam->param_getfcn;
        param_char = char_getter(qtEnt, qtparam);
        param_string = g_strdup_printf("%c", param_char);
        return param_string;
    }
    return NULL;
}

QofBookMergeData*
qof_book_merge_update_result (QofBookMergeData *mergeData,
                              QofBookMergeResult tag)
{
    QofBookMergeRule *resolved;

    g_return_val_if_fail((mergeData != NULL), NULL);
    g_return_val_if_fail((tag > 0), NULL);
    g_return_val_if_fail((tag != MERGE_REPORT), NULL);
    resolved = mergeData->currentRule;
    g_return_val_if_fail((resolved != NULL), NULL);
    if ((resolved->mergeAbsolute == TRUE) && (tag == MERGE_DUPLICATE))
    {
        tag = MERGE_ABSOLUTE;
    }
    if ((resolved->mergeAbsolute == TRUE) && (tag == MERGE_NEW))
    {
        tag = MERGE_UPDATE;
    }
    if ((resolved->mergeAbsolute == FALSE) &&	(tag == MERGE_ABSOLUTE))
    {
        tag = MERGE_DUPLICATE;
    }
    if ((resolved->mergeResult == MERGE_NEW) && (tag == MERGE_UPDATE))
    {
        tag = MERGE_NEW;
    }
    if (resolved->updated == FALSE)
    {
        resolved->mergeResult = tag;
    }
    resolved->updated = TRUE;
    if (tag >= MERGE_INVALID)
    {
        mergeData->abort = TRUE;
        mergeData->currentRule = resolved;
        return NULL;
    }
    mergeData->currentRule = resolved;
    return mergeData;
}

gint
qof_book_merge_commit(QofBookMergeData *mergeData )
{
    QofBookMergeRule *currentRule;
    GList *check, *node;

    ENTER (" ");

    g_return_val_if_fail(mergeData != NULL, -10);
    g_return_val_if_fail(mergeData->mergeList != NULL, -11);
    g_return_val_if_fail(mergeData->targetBook != NULL, -12);
    if (mergeData->abort == TRUE) return -13;
    check = g_list_copy(mergeData->mergeList);
    g_return_val_if_fail(check != NULL, -14);
    for (node = check; node != NULL; node = node->next)
    {
        currentRule = node->data;

        if (currentRule->mergeResult == MERGE_INVALID)
        {
            qof_book_merge_abort(mergeData);
            g_list_free(check);
            return(-2);
        }
        if (currentRule->mergeResult == MERGE_REPORT)
        {
            g_list_free(check);
            return 1;
        }
    }
    g_list_free(check);
    qof_book_merge_commit_foreach(qof_book_merge_commit_rule_create_objects,
                                  MERGE_NEW, mergeData);
    qof_book_merge_commit_foreach(qof_book_merge_commit_rule_loop,
                                  MERGE_NEW, mergeData);
    qof_book_merge_commit_foreach(qof_book_merge_commit_rule_loop,
                                  MERGE_UPDATE, mergeData);

    /* Placeholder for QofObject merge_helper_cb - all objects
          and all parameters set */
    while (mergeData->mergeList != NULL)
    {
        currentRule = mergeData->mergeList->data;
        g_slist_free(currentRule->mergeParam);
        g_slist_free(currentRule->linkedEntList);
        mergeData->mergeList = g_list_next(mergeData->mergeList);
    }
    g_list_free(mergeData->mergeList);
    g_slist_free(mergeData->mergeObjectParams);
    g_slist_free(mergeData->targetList);
    if (mergeData->orphan_list != NULL)
    {
        g_slist_free(mergeData->orphan_list);
    }
    g_hash_table_destroy(mergeData->target_table);
    g_free(mergeData);

    LEAVE (" ");
    return 0;
}

void
qof_book_merge_rule_foreach (QofBookMergeData *mergeData,
                             QofBookMergeRuleForeachCB cb,
                             QofBookMergeResult mergeResult )
{
    struct QofBookMergeRuleIterate iter;
    QofBookMergeRule *currentRule;
    GList *matching_rules, *node;

    g_return_if_fail(cb != NULL);
    g_return_if_fail(mergeData != NULL);
    currentRule = mergeData->currentRule;
    g_return_if_fail(mergeResult > 0);
    g_return_if_fail(mergeResult != MERGE_INVALID);
    g_return_if_fail(mergeData->abort == FALSE);
    iter.fcn = cb;
    iter.data = mergeData;
    matching_rules = NULL;
    iter.ruleList = NULL;
    for (node = mergeData->mergeList; node != NULL; node = node->next)
    {
        currentRule = node->data;
        if (currentRule->mergeResult == mergeResult)
        {
            matching_rules = g_list_prepend(matching_rules, currentRule);
        }
    }
    iter.remainder = g_list_length(matching_rules);
    g_list_foreach (matching_rules, qof_book_merge_rule_cb, &iter);
    g_list_free(matching_rules);
}

/* End of file. */
/* ==================================================================== */
