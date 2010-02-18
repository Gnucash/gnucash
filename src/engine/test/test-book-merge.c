/*********************************************************************
 * test-book-merge.c -- test implementation api for QoFBook merge    *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652        *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                    *
 *                                                                   *
 ********************************************************************/
/* Test the qof_book_merge infrastructure. */

#include "config.h"
#include <glib.h>

#include "qof.h"
#include "test-stuff.h"
#include "gnc-engine.h"

#define TEST_MODULE_NAME "book-merge-test"
#define TEST_MODULE_DESC "Test Book Merge"
#define OBJ_NAME "somename"
#define OBJ_AMOUNT "anamount"
#define OBJ_DATE "nottoday"
#define OBJ_GUID "unique"
#define OBJ_DISCOUNT "hefty"
#define OBJ_VERSION "early"
#define OBJ_MINOR "tiny"
#define OBJ_ACTIVE "ofcourse"

static void test_rule_loop (QofBookMergeData*, QofBookMergeRule*, guint);
static void test_merge (void);
gboolean myobjRegister (void);

/* simple object structure */
typedef struct obj_s
{
    QofInstance inst;
    char     	*Name;
    gnc_numeric	Amount;
    const GUID 	*obj_guid;
    Timespec 	date;
    double 		discount; /* cheap pun, I know. */
    gboolean 	active;
    gint32   	version;
    gint64 		minor;
} myobj;

typedef struct objclass_s
{
    QofInstanceClass parent_class;
} myobjClass;

myobj* obj_create(QofBook*);

/* obvious setter functions */
void obj_setName(myobj*,	char*);
void obj_setGUID(myobj*,	const GUID*);
void obj_setAmount(myobj*,  gnc_numeric);
void obj_setDate(myobj*,	Timespec h);
void obj_setDiscount(myobj*, double);
void obj_setActive(myobj*,  gboolean);
void obj_setVersion(myobj*, gint32);
void obj_setMinor(myobj*,   gint64);

/* obvious getter functions */
char*		obj_getName(myobj*);
const GUID*	obj_getGUID(myobj*);
gnc_numeric obj_getAmount(myobj*);
Timespec   	obj_getDate(myobj*);
double		obj_getDiscount(myobj*);
gboolean	obj_getActive(myobj*);
gint32		obj_getVersion(myobj*);
gint64		obj_getMinor(myobj*);

/* --- type macros --- */
#define GNC_TYPE_MYOBJ            (gnc_myobj_get_type ())
#define GNC_MYOBJ(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_MYOBJ, myobj))
#define GNC_MYOBJ_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_MYOBJ, myobjClass))
#define GNC_IS_MYOBJ(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MYOBJ))
#define GNC_IS_MYOBJ_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_MYOBJ))
#define GNC_MYOBJ_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_MYOBJ, myobjClass))
GType gnc_myobj_get_type(void);

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_myobj, myobj, QOF_TYPE_INSTANCE);

static void
gnc_myobj_init(myobj* obj)
{
}

static void
gnc_myobj_dispose_real (GObject *objp)
{
}

static void
gnc_myobj_finalize_real(GObject* objp)
{
}

myobj*
obj_create(QofBook *book)
{
    myobj *g;
    g_return_val_if_fail(book, NULL);
    g = g_object_new(GNC_TYPE_MYOBJ, NULL);
    qof_instance_init_data (&g->inst, TEST_MODULE_NAME, book);
    obj_setGUID(g, qof_instance_get_guid(&g->inst));
    g->date.tv_nsec = 0;
    g->date.tv_sec = 0;
    g->discount = 0;
    g->active = TRUE;
    g->version = 1;
    g->minor = 1;
    qof_event_gen(&g->inst, QOF_EVENT_CREATE, NULL);
    return g;
}

void
obj_setMinor(myobj *g, gint64 h)
{
    g_return_if_fail(g != NULL);
    g->minor = h;
}

gint64
obj_getMinor(myobj *g)
{
    g_return_val_if_fail((g != NULL), 0);
    return g->minor;
}

void
obj_setVersion(myobj *g, gint32 h)
{
    g_return_if_fail(g != NULL);
    g->version = h;
}

gint32
obj_getVersion(myobj *g)
{
    if (!g) return 0;
    return g->version;
}

void
obj_setActive(myobj *g, gboolean h)
{
    if (!g) return;
    g->active = h;
}

gboolean
obj_getActive(myobj *g)
{
    if (!g) return FALSE;
    return g->active;
}

void
obj_setDiscount(myobj *g, double h)
{
    if (!g) return;
    g->discount = h;
}

double
obj_getDiscount(myobj *g)
{
    if (!g) return 0;
    return g->discount;
}

void
obj_setDate(myobj *g, Timespec h)
{
    if (!g) return;
    g->date = h;
}

Timespec
obj_getDate(myobj *g)
{
    Timespec ts = {0};
    if (!g) return ts;
    ts = g->date;
    return ts;
}

void
obj_setGUID(myobj* g, const GUID* h)
{
    if (!g) return;
    g->obj_guid = h;
}

const GUID*
obj_getGUID(myobj *g)
{
    if (!g) return NULL;
    return g->obj_guid;
}

void
obj_setName(myobj* g, char* h)
{
    if (!g || !h) return;
    g->Name = strdup(h);
}

char*
obj_getName(myobj *g)
{
    if (!g) return NULL;
    return g->Name;
}

void
obj_setAmount(myobj *g, gnc_numeric h)
{
    if (!g) return;
    g->Amount = h;
}

gnc_numeric
obj_getAmount(myobj *g)
{
    if (!g) return gnc_numeric_zero();
    return g->Amount;
}

static QofObject obj_object_def =
{
interface_version:
    QOF_OBJECT_VERSION,
e_type:
    TEST_MODULE_NAME,
type_label:
    TEST_MODULE_DESC,
create:
    (gpointer)obj_create,
book_begin:
    NULL,
book_end:
    NULL,
is_dirty:
    NULL,
mark_clean:
    NULL,
foreach:
    qof_collection_foreach,
printable:
    NULL,
version_cmp:
    (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean myobjRegister (void)
{
    static QofParam params[] =
    {
        { OBJ_NAME,		QOF_TYPE_STRING,	(QofAccessFunc)obj_getName,		(QofSetterFunc)obj_setName		},
        { OBJ_AMOUNT,   QOF_TYPE_NUMERIC,   (QofAccessFunc)obj_getAmount,   (QofSetterFunc)obj_setAmount	},
        { OBJ_GUID,		QOF_TYPE_GUID,		(QofAccessFunc)obj_getGUID,		(QofSetterFunc)obj_setGUID		},
        { OBJ_DATE,		QOF_TYPE_DATE,		(QofAccessFunc)obj_getDate,		(QofSetterFunc)obj_setDate		},
        { OBJ_DISCOUNT, QOF_TYPE_DOUBLE,	(QofAccessFunc)obj_getDiscount, (QofSetterFunc)obj_setDiscount  },
        { OBJ_ACTIVE,   QOF_TYPE_BOOLEAN,   (QofAccessFunc)obj_getActive,   (QofSetterFunc)obj_setActive	},
        { OBJ_VERSION,  QOF_TYPE_INT32,		(QofAccessFunc)obj_getVersion,  (QofSetterFunc)obj_setVersion   },
        { OBJ_MINOR,	QOF_TYPE_INT64,		(QofAccessFunc)obj_getMinor,	(QofSetterFunc)obj_setMinor		},
        { QOF_PARAM_BOOK, QOF_ID_BOOK,		(QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID,	(QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    qof_class_register (TEST_MODULE_NAME, NULL, params);

    return qof_object_register (&obj_object_def);
}

static void
test_merge (void)
{
    QofBook *target, *import;
    double init_value, discount;
    myobj *import_obj, *target_obj, *new_obj;
    int result;
    Timespec ts, tc;
    gboolean active;
    gint32 version;
    gint64 minor;
    gchar *import_init, *target_init;
    gnc_numeric obj_amount;
    QofBookMergeData *mergeData;

    target = qof_book_new();
    import = qof_book_new();
    init_value = 1.00;
    result = 0;
    discount = 0.5;
    active = TRUE;
    version = 1;
    minor = 1;
    import_init = "test";
    target_init = "testing";
    qof_date_format_set(QOF_DATE_FORMAT_UK);
    timespecFromTime_t(&ts, time(NULL));

    do_test ((NULL != target), "#1 target book is NULL");

    /* import book objects - tests used */
    do_test ((NULL != import), "#2 import book is NULL");
    import_obj = g_object_new(GNC_TYPE_MYOBJ, NULL);
    do_test ((NULL != import_obj), "#3 new object create");
    qof_instance_init_data (&import_obj->inst, TEST_MODULE_NAME, import);
    do_test ((NULL != &import_obj->inst), "#4 instance init");
    obj_setGUID(import_obj, qof_instance_get_guid(&import_obj->inst));
    do_test ((NULL != &import_obj->obj_guid), "#5 guid set");
    qof_event_gen(&import_obj->inst, QOF_EVENT_CREATE, NULL);
    do_test ((NULL != &import_obj->inst), "#6 gnc event create");
    obj_setName(import_obj, import_init);
    do_test ((NULL != &import_obj->Name), "#7 string set");
    obj_amount = double_to_gnc_numeric(init_value, 1, GNC_HOW_DENOM_EXACT);
    obj_setAmount(import_obj, obj_amount);
    do_test ((gnc_numeric_check(obj_getAmount(import_obj)) == GNC_ERROR_OK), "#8 gnc_numeric set");
    obj_setActive(import_obj, active);
    do_test ((FALSE != &import_obj->active), "#9 gboolean set");
    obj_setDiscount(import_obj, discount);
    do_test ((discount == import_obj->discount), "#10 double set");
    obj_setVersion(import_obj, version);
    do_test ((version == import_obj->version), "#11 gint32 set");
    obj_setMinor(import_obj, minor);
    do_test ((minor == import_obj->minor), "#12 gint64 set");
    obj_setDate(import_obj, ts );
    tc = import_obj->date;
    do_test ((timespec_cmp(&ts, &tc) == 0), "#13 date set");

    obj_amount = gnc_numeric_add(obj_amount, obj_amount, 1, GNC_HOW_DENOM_EXACT);
    discount = 0.25;
    version = 2;
    minor = 3;

    /* second import object - test results would be the same, so not tested. */
    new_obj = g_object_new(GNC_TYPE_MYOBJ, NULL);
    qof_instance_init_data (&new_obj->inst, TEST_MODULE_NAME, import);
    obj_setGUID(new_obj, qof_instance_get_guid(&new_obj->inst));
    qof_event_gen (&new_obj->inst, QOF_EVENT_CREATE, NULL);
    obj_setName(new_obj, import_init);
    obj_setAmount(new_obj, obj_amount);
    obj_setActive(new_obj, active);
    obj_setDiscount(new_obj, discount);
    obj_setVersion(new_obj, version);
    obj_setMinor(new_obj, minor);
    obj_setDate(new_obj, ts);

    obj_amount = gnc_numeric_add(obj_amount, obj_amount, 1, GNC_HOW_DENOM_EXACT);
    discount = 0.35;
    version = 2;
    minor = 3;
    tc.tv_sec = ts.tv_sec - 1;
    tc.tv_nsec = 0;

    /* target object - test results would be the same, so not tested. */
    target_obj = g_object_new(GNC_TYPE_MYOBJ, NULL);
    qof_instance_init_data (&target_obj->inst, TEST_MODULE_NAME, target);
    obj_setGUID(target_obj, qof_instance_get_guid(&target_obj->inst));
    qof_event_gen (&target_obj->inst, QOF_EVENT_CREATE, NULL);
    obj_setName(target_obj, target_init);
    obj_setAmount(target_obj, obj_amount);
    obj_setActive(target_obj, active);
    obj_setDiscount(target_obj, discount);
    obj_setVersion(target_obj, version);
    obj_setMinor(target_obj, minor);
    obj_setDate(target_obj, tc );

    mergeData = qof_book_merge_init(import, target);
    do_test ( mergeData != NULL, "FATAL: Merge could not be initialised!\t aborting . . ");
    g_return_if_fail(mergeData != NULL);
    qof_book_merge_rule_foreach(mergeData, test_rule_loop, MERGE_REPORT);
    qof_book_merge_rule_foreach(mergeData, test_rule_loop, MERGE_UPDATE);
    qof_book_merge_rule_foreach(mergeData, test_rule_loop, MERGE_NEW);
    /* reserved calls - test only */
    qof_book_merge_rule_foreach(mergeData, test_rule_loop, MERGE_ABSOLUTE);
    qof_book_merge_rule_foreach(mergeData, test_rule_loop, MERGE_DUPLICATE);

    /* import should not be in the target - pass if import_init fails match with target */
    do_test (((safe_strcmp(obj_getName(import_obj), obj_getName(target_obj))) != 0), "Init value test #1");

    /* a good commit returns zero */
    do_test (qof_book_merge_commit(mergeData) == 0, "Commit failed");

    /* import should be in the target - pass if import_init matches target */
    do_test (((safe_strcmp(import_init, obj_getName(target_obj))) == 0), "Merged value test #1");

    /* import should be the same as target - pass if values are the same */
    do_test (((safe_strcmp(obj_getName(target_obj), obj_getName(import_obj))) == 0), "Merged value test #2");

    /* check that the Amount really is a gnc_numeric */
    do_test ((gnc_numeric_check(obj_getAmount(import_obj)) == GNC_ERROR_OK), "import gnc_numeric check");
    do_test ((gnc_numeric_check(obj_getAmount(target_obj)) == GNC_ERROR_OK), "target gnc_numeric check");

    /* obj_amount was changed after the import object was set, so expect a difference. */
    do_test ((gnc_numeric_compare(obj_getAmount(import_obj), obj_amount) != GNC_ERROR_OK),
             "gnc_numeric value check #1");

    /* obj_amount is in the target object with the import value, expect a difference/ */
    do_test ((gnc_numeric_compare(obj_getAmount(target_obj), obj_amount) != GNC_ERROR_OK),
             "gnc_numeric value check #2");

    /* target had a different date, so import date should now be set */
    /* note: If sensible defaults are not set in the create:
    an empty Timespec caused problems with the update - fix */
    tc = target_obj->date;
    do_test ((timespec_cmp(&ts, &tc) == 0), "date value check: 1");
    tc = import_obj->date;
    do_test ((timespec_cmp(&tc, &ts) == 0), "date value check: 2");
    do_test ((timespec_cmp(&import_obj->date, &target_obj->date) == 0), "date value check: 3");

}

static void
test_rule_loop (QofBookMergeData *mergeData, QofBookMergeRule *rule, guint remainder)
{
    GSList *testing;
    QofParam *eachParam;
    char *importstring;
    char *targetstring;
    /* In this test rule_loop, any lines beginning with do_test() can be removed
    from a working rule_loop routine. It would be wise to still use some of the
    more obvious checks, e.g. that an entity or rule exists before querying the parameters.

    Take particular care with MERGE_NEW - targetEnt is always NULL until the Commit.
    Do not attempt to use param_getfcn on targetEnt in the loop called by
    QofBookMergeRuleForeach(rule_loop, MERGE_NEW);

    */
    gboolean skip_target;

    importstring = NULL;
    targetstring = NULL;
    skip_target = FALSE;
    mergeData->currentRule = rule;
    do_test ((rule != NULL), "loop:#1 Rule is NULL");
    do_test (remainder > 0, "loop:#2 remainder error.");
    do_test ((safe_strcmp(NULL, rule->mergeLabel) != 0), "loop:#3 object label\n");
    do_test ((rule->importEnt != NULL), "loop:#4 empty import entity");
    /* targetEnt is always NULL at this stage if MERGE_NEW is set */
    if (rule->targetEnt == NULL)
    {
        skip_target = TRUE;
    }
    if (!skip_target)
    {
        do_test ((safe_strcmp(rule->importEnt->e_type, rule->targetEnt->e_type) == 0),
                 "loop: entity type mismatch");
    }
    do_test ((rule->mergeParam != NULL), "loop: empty parameter list");
    testing = rule->mergeParam;

    while (testing != NULL)  // start of param loop
    {
        eachParam = testing->data;
        do_test ((eachParam != NULL), "loop:#8 no QofParam data");
        do_test ((eachParam->param_name != NULL), "loop:#9 no parameter name");
        do_test ((eachParam->param_getfcn != NULL), "loop:#10 no get function");
        do_test ((eachParam->param_setfcn != NULL), "loop:#11 no set function");
        /* non-generic - test routines only! */
        if (safe_strcmp(eachParam->param_type, QOF_TYPE_STRING) == 0)
        {
            /* if you use this format, you would need to check the QOF_TYPE and
            configure the get_fcn pointers yourself. This example only works for strings. */
            importstring = g_strdup(eachParam->param_getfcn(rule->importEnt, eachParam));
            do_test ((importstring != NULL), "loop:#12 direct get_fcn import");
            do_test ((safe_strcmp(importstring, "test") == 0), "loop:#13 direct import comparison");
            if (!skip_target)
            {
                targetstring = eachParam->param_getfcn(rule->targetEnt, eachParam);
                do_test ((targetstring != NULL), "loop:#14 direct get_fcn target");
                do_test ((safe_strcmp(targetstring, "testing") == 0), "loop:#15 direct target comparison");
            }
        }
        /* param_as_string does the conversion for display purposes only */
        /* do NOT use as_string for calculations or set_fcn */
        importstring = qof_book_merge_param_as_string(eachParam, rule->importEnt);
        do_test ((importstring != NULL), "loop:#16 import param_as_string is null");
        /*		printf("importstring %s\t%s Type\n", importstring, eachParam->param_type);*/
        if (!skip_target)
        {
            targetstring = qof_book_merge_param_as_string(eachParam, rule->targetEnt);
            do_test ((targetstring != NULL), "loop:#17 target param_as_string is null");
            /*		printf("targetstring %s\t%s Type\n", targetstring, eachParam->param_type);*/
        }
        /* add your own code for user involvement here. */
        /* either store the importstring and targetstring values and display separately,
        perhaps in alphabetical/object_type/priority order, or, obtain user input as each
        string is available. */

        testing = g_slist_next(testing);
    } // end param loop
    /* set each rule dependent on the user involvement response above. */
    /* test routine just sets all MERGE_REPORT to MERGE_UPDATE */
    mergeData = qof_book_merge_update_result(mergeData, MERGE_UPDATE);
    do_test ((rule->mergeResult != MERGE_REPORT), "update result fail");
}

int
main (int argc, char **argv)
{
    qof_init();
    myobjRegister();
    test_merge();
    print_test_results();
    qof_close();
    return get_rv();
}
