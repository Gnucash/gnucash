/********************************************************************\
 * gncBillTerm.c -- the Gnucash Billing Terms interface             *
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
 * Copyright (C) 2002 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include <config.h>

#include <glib.h>
#include <qofinstance-p.h>

#include "gnc-engine.h"
#include "gncBillTermP.h"

struct _gncBillTerm
{
    QofInstance     inst;

    /* 'visible' data fields directly manipulated by user */
    char *          name;
    char *          desc;
    GncBillTermType type;
    gint            due_days;
    gint            disc_days;
    gnc_numeric     discount;
    gint            cutoff;

    /* Internal management fields */
    /* See src/doc/business.txt for an explanation of the following */
    /* Code that handles this is *identical* to that in gncTaxTable */
    gint64          refcount;
    GncBillTerm *   parent;      /* if non-null, we are an immutable child */
    GncBillTerm *   child;       /* if non-null, we have not changed */
    gboolean        invisible;
    GList *         children;    /* list of children for disconnection */
};

struct _gncBillTermClass
{
    QofInstanceClass parent_class;
};

struct _book_info
{
    GList *         terms;        /* visible terms */
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_BILLTERM

#define SET_STR(obj, member, str) { \
        char * tmp; \
        \
        if (!g_strcmp0 (member, str)) return; \
        gncBillTermBeginEdit (obj); \
        tmp = CACHE_INSERT (str); \
        CACHE_REMOVE (member); \
        member = tmp; \
        }

AS_STRING_DEC(GncBillTermType, ENUM_TERMS_TYPE)
FROM_STRING_DEC(GncBillTermType, ENUM_TERMS_TYPE)

/* ============================================================== */
/* Misc inline utilities */

static inline void
mark_term (GncBillTerm *term)
{
    qof_instance_set_dirty(&term->inst);
    qof_event_gen (&term->inst, QOF_EVENT_MODIFY, NULL);
}

static inline void maybe_resort_list (GncBillTerm *term)
{
    struct _book_info *bi;

    if (term->parent || term->invisible) return;
    bi = qof_book_get_data (qof_instance_get_book(term), _GNC_MOD_NAME);
    bi->terms = g_list_sort (bi->terms, (GCompareFunc)gncBillTermCompare);
}

static inline void addObj (GncBillTerm *term)
{
    struct _book_info *bi;
    bi = qof_book_get_data (qof_instance_get_book(term), _GNC_MOD_NAME);
    bi->terms = g_list_insert_sorted (bi->terms, term,
                                      (GCompareFunc)gncBillTermCompare);
}

static inline void remObj (GncBillTerm *term)
{
    struct _book_info *bi;
    bi = qof_book_get_data (qof_instance_get_book(term), _GNC_MOD_NAME);
    bi->terms = g_list_remove (bi->terms, term);
}

static inline void
gncBillTermAddChild (GncBillTerm *table, GncBillTerm *child)
{
    g_return_if_fail(qof_instance_get_destroying(table) == FALSE);
    table->children = g_list_prepend(table->children, child);
}

static inline void
gncBillTermRemoveChild (GncBillTerm *table, GncBillTerm *child)
{
    if (qof_instance_get_destroying(table)) return;
    table->children = g_list_remove(table->children, child);
}

/* ============================================================== */

enum
{
    PROP_0,
    PROP_NAME
};

/* GObject Initialization */
G_DEFINE_TYPE(GncBillTerm, gnc_billterm, QOF_TYPE_INSTANCE);

static void
gnc_billterm_init(GncBillTerm* bt)
{
}

static void
gnc_billterm_dispose(GObject *btp)
{
    G_OBJECT_CLASS(gnc_billterm_parent_class)->dispose(btp);
}

static void
gnc_billterm_finalize(GObject* btp)
{
    G_OBJECT_CLASS(gnc_billterm_parent_class)->finalize(btp);
}

static void
gnc_billterm_get_property (GObject         *object,
                           guint            prop_id,
                           GValue          *value,
                           GParamSpec      *pspec)
{
    GncBillTerm *bt;

    g_return_if_fail(GNC_IS_BILLTERM(object));

    bt = GNC_BILLTERM(object);
    switch (prop_id)
    {
    case PROP_NAME:
        g_value_set_string(value, bt->name);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_billterm_set_property (GObject         *object,
                           guint            prop_id,
                           const GValue          *value,
                           GParamSpec      *pspec)
{
    GncBillTerm *bt;

    g_return_if_fail(GNC_IS_BILLTERM(object));

    bt = GNC_BILLTERM(object);
    g_assert (qof_instance_get_editlevel(bt));

    switch (prop_id)
    {
    case PROP_NAME:
        gncBillTermSetName(bt, g_value_get_string(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

/** Returns a list of my type of object which refers to an object.  For example, when called as
        qof_instance_get_typed_referring_object_list(taxtable, account);
    it will return the list of taxtables which refer to a specific account.  The result should be the
    same regardless of which taxtable object is used.  The list must be freed by the caller but the
    objects on the list must not.
 */
static GList*
impl_get_typed_referring_object_list(const QofInstance* inst, const QofInstance* ref)
{
    /* Bill term doesn't refer to anything except other billterms */
    return NULL;
}

static void
gnc_billterm_class_init (GncBillTermClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    QofInstanceClass* qof_class = QOF_INSTANCE_CLASS(klass);

    gobject_class->dispose = gnc_billterm_dispose;
    gobject_class->finalize = gnc_billterm_finalize;
    gobject_class->set_property = gnc_billterm_set_property;
    gobject_class->get_property = gnc_billterm_get_property;

    qof_class->get_display_name = NULL;
    qof_class->refers_to_object = NULL;
    qof_class->get_typed_referring_object_list = impl_get_typed_referring_object_list;

    g_object_class_install_property
    (gobject_class,
     PROP_NAME,
     g_param_spec_string ("name",
                          "BillTerm Name",
                          "The bill term name is an arbitrary string "
                          "assigned by the user.  It is intended to "
                          "a short, 10 to 30 character long string "
                          "that is displayed by the GUI as the "
                          "billterm mnemonic.",
                          NULL,
                          G_PARAM_READWRITE));
}

/* Create/Destroy Functions */
GncBillTerm * gncBillTermCreate (QofBook *book)
{
    GncBillTerm *term;
    if (!book) return NULL;

    term = g_object_new (GNC_TYPE_BILLTERM, NULL);
    qof_instance_init_data(&term->inst, _GNC_MOD_NAME, book);
    term->name = CACHE_INSERT ("");
    term->desc = CACHE_INSERT ("");
    term->discount = gnc_numeric_zero ();
    addObj (term);
    qof_event_gen (&term->inst,  QOF_EVENT_CREATE, NULL);
    return term;
}

void gncBillTermDestroy (GncBillTerm *term)
{
    gchar guidstr[GUID_ENCODING_LENGTH+1];
    if (!term) return;
    guid_to_string_buff(qof_instance_get_guid(&term->inst),guidstr);
    DEBUG("destroying bill term %s (%p)", guidstr, term);
    qof_instance_set_destroying(term, TRUE);
    qof_instance_set_dirty (&term->inst);
    gncBillTermCommitEdit (term);
}

static void gncBillTermFree (GncBillTerm *term)
{
    GncBillTerm *child;
    GList *list;

    if (!term) return;

    qof_event_gen (&term->inst,  QOF_EVENT_DESTROY, NULL);
    CACHE_REMOVE (term->name);
    CACHE_REMOVE (term->desc);
    remObj (term);

    if (!qof_instance_get_destroying(term))
        PERR("free a billterm without do_free set!");

    /* disconnect from parent */
    if (term->parent)
        gncBillTermRemoveChild(term->parent, term);

    /* disconnect from the children */
    for (list = term->children; list; list = list->next)
    {
        child = list->data;
        gncBillTermSetParent(child, NULL);
    }
    g_list_free(term->children);

    /* qof_instance_release(&term->inst); */
    g_object_unref (term);
}

/* ============================================================== */
/* Set Functions */

void gncBillTermSetName (GncBillTerm *term, const char *name)
{
    if (!term || !name) return;
    SET_STR (term, term->name, name);
    mark_term (term);
    maybe_resort_list (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermSetDescription (GncBillTerm *term, const char *desc)
{
    if (!term || !desc) return;
    SET_STR (term, term->desc, desc);
    mark_term (term);
    maybe_resort_list (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermSetType (GncBillTerm *term, GncBillTermType type)
{
    if (!term) return;
    if (term->type == type) return;
    gncBillTermBeginEdit (term);
    term->type = type;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

/** \brief Convert bill term types from text. */
FROM_STRING_FUNC(GncBillTermType, ENUM_TERMS_TYPE)

static
void qofBillTermSetType (GncBillTerm *term, const char *type_label)
{
    GncBillTermType type;

    type = GncBillTermTypefromString(type_label);
    gncBillTermSetType(term, type);
}

void gncBillTermSetDueDays (GncBillTerm *term, gint days)
{
    if (!term) return;
    if (term->due_days == days) return;
    gncBillTermBeginEdit (term);
    term->due_days = days;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermSetDiscountDays (GncBillTerm *term, gint days)
{
    if (!term) return;
    if (term->disc_days == days) return;
    gncBillTermBeginEdit (term);
    term->disc_days = days;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermSetDiscount (GncBillTerm *term, gnc_numeric discount)
{
    if (!term) return;
    if (gnc_numeric_eq (term->discount, discount)) return;
    gncBillTermBeginEdit (term);
    term->discount = discount;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermSetCutoff (GncBillTerm *term, gint cutoff)
{
    if (!term) return;
    if (term->cutoff == cutoff) return;
    gncBillTermBeginEdit (term);
    term->cutoff = cutoff;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

/* XXX this doesn't seem right. If the parent/child relationship
 * is a doubly-linked list, then there shouldn't be separate set-parent,
 * set-child routines, else misuse of the routines will goof up
 * relationships.  These ops should be atomic, I think.
 */
void gncBillTermSetParent (GncBillTerm *term, GncBillTerm *parent)
{
    if (!term) return;
    gncBillTermBeginEdit (term);
    if (term->parent)
        gncBillTermRemoveChild(term->parent, term);
    term->parent = parent;
    if (parent)
        gncBillTermAddChild(parent, term);
    term->refcount = 0;
    if ( parent != NULL )
    {
        gncBillTermMakeInvisible (term);
    }
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermSetChild (GncBillTerm *term, GncBillTerm *child)
{
    if (!term) return;
    gncBillTermBeginEdit (term);
    term->child = child;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermIncRef (GncBillTerm *term)
{
    if (!term) return;
    if (term->parent || term->invisible) return;        /* children dont need refcounts */
    gncBillTermBeginEdit (term);
    term->refcount++;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermDecRef (GncBillTerm *term)
{
    if (!term) return;
    if (term->parent || term->invisible) return;        /* children dont need refcounts */
    g_return_if_fail (term->refcount >= 1);
    gncBillTermBeginEdit (term);
    term->refcount--;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermSetRefcount (GncBillTerm *term, gint64 refcount)
{
    if (!term) return;
    gncBillTermBeginEdit (term);
    term->refcount = refcount;
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermMakeInvisible (GncBillTerm *term)
{
    if (!term) return;
    gncBillTermBeginEdit (term);
    term->invisible = TRUE;
    remObj (term);
    mark_term (term);
    gncBillTermCommitEdit (term);
}

void gncBillTermChanged (GncBillTerm *term)
{
    if (!term) return;
    term->child = NULL;
}

void gncBillTermBeginEdit (GncBillTerm *term)
{
    qof_begin_edit(&term->inst);
}

static void gncBillTermOnError (QofInstance *inst, QofBackendError errcode)
{
    PERR("BillTerm QofBackend Failure: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void bill_free (QofInstance *inst)
{
    GncBillTerm *term = (GncBillTerm *) inst;
    gncBillTermFree(term);
}

static void on_done (QofInstance *inst) {}

void gncBillTermCommitEdit (GncBillTerm *term)
{
    if (!qof_commit_edit (QOF_INSTANCE(term))) return;
    qof_commit_edit_part2 (&term->inst, gncBillTermOnError,
                           on_done, bill_free);
}

/* Get Functions */

GncBillTerm *gncBillTermLookupByName (QofBook *book, const char *name)
{
    GList *list = gncBillTermGetTerms (book);

    for ( ; list; list = list->next)
    {
        GncBillTerm *term = list->data;
        if (!g_strcmp0 (term->name, name))
            return list->data;
    }
    return NULL;
}

GList * gncBillTermGetTerms (QofBook *book)
{
    struct _book_info *bi;
    if (!book) return NULL;

    bi = qof_book_get_data (book, _GNC_MOD_NAME);
    return bi->terms;
}

const char *gncBillTermGetName (const GncBillTerm *term)
{
    if (!term) return NULL;
    return term->name;
}

const char *gncBillTermGetDescription (const GncBillTerm *term)
{
    if (!term) return NULL;
    return term->desc;
}

GncBillTermType gncBillTermGetType (const GncBillTerm *term)
{
    if (!term) return 0;
    return term->type;
}

/** \brief Convert bill term types to text. */
AS_STRING_FUNC(GncBillTermType, ENUM_TERMS_TYPE)

static
const char* qofBillTermGetType (const GncBillTerm *term)
{
    if (!term)
    {
        return NULL;
    }
    return GncBillTermTypeasString(term->type);
}

gint gncBillTermGetDueDays (const GncBillTerm *term)
{
    if (!term) return 0;
    return term->due_days;
}

gint gncBillTermGetDiscountDays (const GncBillTerm *term)
{
    if (!term) return 0;
    return term->disc_days;
}

gnc_numeric gncBillTermGetDiscount (const GncBillTerm *term)
{
    if (!term) return gnc_numeric_zero ();
    return term->discount;
}

gint gncBillTermGetCutoff (const GncBillTerm *term)
{
    if (!term) return 0;
    return term->cutoff;
}

static GncBillTerm *gncBillTermCopy (const GncBillTerm *term)
{
    GncBillTerm *t;

    if (!term) return NULL;
    t = gncBillTermCreate (qof_instance_get_book(term));

    gncBillTermBeginEdit(t);

    gncBillTermSetName (t, term->name);
    gncBillTermSetDescription (t, term->desc);

    t->type = term->type;
    t->due_days = term->due_days;
    t->disc_days = term->disc_days;
    t->discount = term->discount;
    t->cutoff = term->cutoff;

    mark_term (t);
    gncBillTermCommitEdit(t);

    return t;
}

GncBillTerm *gncBillTermReturnChild (GncBillTerm *term, gboolean make_new)
{
    GncBillTerm *child = NULL;

    if (!term) return NULL;
    if (term->child) return term->child;
    if (term->parent || term->invisible) return term;
    if (make_new)
    {
        child = gncBillTermCopy (term);
        gncBillTermSetChild (term, child);
        gncBillTermSetParent (child, term);
    }
    return child;
}

GncBillTerm *gncBillTermGetParent (const GncBillTerm *term)
{
    if (!term) return NULL;
    return term->parent;
}

gint64 gncBillTermGetRefcount (const GncBillTerm *term)
{
    if (!term) return 0;
    return term->refcount;
}

gboolean gncBillTermGetInvisible (const GncBillTerm *term)
{
    if (!term) return FALSE;
    return term->invisible;
}

int gncBillTermCompare (const GncBillTerm *a, const GncBillTerm *b)
{
    int ret;

    if (!a && !b) return 0;
    if (!a) return -1;
    if (!b) return 1;

    ret = g_strcmp0 (a->name, b->name);
    if (ret) return ret;

    return g_strcmp0 (a->desc, b->desc);
}

gboolean gncBillTermEqual(const GncBillTerm *a, const GncBillTerm *b)
{
    if (a == NULL && b == NULL) return TRUE;
    if (a == NULL || b == NULL) return FALSE;

    g_return_val_if_fail(GNC_IS_BILLTERM(a), FALSE);
    g_return_val_if_fail(GNC_IS_BILLTERM(b), FALSE);

    if (g_strcmp0(a->name, b->name) != 0)
    {
        PWARN("Names differ: %s vs %s", a->name, b->name);
        return FALSE;
    }

    if (g_strcmp0(a->desc, b->desc) != 0)
    {
        PWARN("Descriptions differ: %s vs %s", a->desc, b->desc);
        return FALSE;
    }

    if (a->type != b->type)
    {
        PWARN("Types differ");
        return FALSE;
    }

    if (a->due_days != b->due_days)
    {
        PWARN("Due days differ: %d vs %d", a->due_days, b->due_days);
        return FALSE;
    }

    if (a->disc_days != b->disc_days)
    {
        PWARN("Discount days differ: %d vs %d", a->disc_days, b->disc_days);
        return FALSE;
    }

    if (!gnc_numeric_equal(a->discount, b->discount))
    {
        PWARN("Discounts differ");
        return FALSE;
    }

    if (a->cutoff != b->cutoff)
    {
        PWARN("Cutoffs differ: %d vs %d", a->cutoff, b->cutoff);
        return FALSE;
    }

    if (a->invisible != b->invisible)
    {
        PWARN("Invisible flags differ");
        return FALSE;
    }

//    gint64          refcount;
//    GncBillTerm *   parent;      /* if non-null, we are an immutable child */
//    GncBillTerm *   child;       /* if non-null, we have not changed */
//    GList *         children;    /* list of children for disconnection */

    return TRUE;
}

gboolean gncBillTermIsFamily (const GncBillTerm *a, const GncBillTerm *b)
{
    if (!gncBillTermCompare (a, b))
        return TRUE;
    else
        return FALSE;
}

gboolean gncBillTermIsDirty (const GncBillTerm *term)
{
    if (!term) return FALSE;
    return qof_instance_get_dirty_flag(term);
}

/********************************************************/
/* functions to compute dates from Bill Terms           */

#define SECS_PER_DAY 86400

/* Based on the post date and a proximo type, compute the month and
 * year this is due.  The actual day is filled in below.
 *
 * A proximo billing term has multiple parameters:
 * * due day: day of the month the invoice/bill will be due
 * * cutoff: day of the month used to decide if the due date will be
 *           in the next month or in the month thereafter. This can be
 *           a negative number in which case the cutoff date is relative
 *           to the end of the month and counting backwards.
 *           Eg: cutoff = -3 would mean 25 in February or 28 in June
 *
 * How does it work:
 * Assume cutoff = 19 and due day = 20
 *
 * * Example 1 post date = 14-06-2010 (European date format)
 *   14 is less than the cutoff of 19, so the due date will be in the next
 *   month. Since the due day is set to 20, the due date will be
 *   20-07-2010
 *
 * * Example 2 post date = 22-06-2010 (European date format)
 *   22 is more than the cutoff of 19, so the due date will be in the month
 *   after next month. Since the due day is set to 20, the due date will be
 *   20-02-2010
 *
 */
static void
compute_monthyear (const GncBillTerm *term, Timespec post_date,
                   int *month, int *year)
{
    int iday, imonth, iyear;
    int cutoff = term->cutoff;

    g_return_if_fail (term->type == GNC_TERM_TYPE_PROXIMO);

    gnc_timespec2dmy (post_date, &iday, &imonth, &iyear);

    if (cutoff <= 0)
        cutoff += gnc_date_get_last_mday (imonth - 1, iyear);

    if (iday <= cutoff)
    {
        /* We apply this to next month */
        imonth++;
    }
    else
    {
        /* We apply to the following month */
        imonth += 2;
    }

    if (imonth > 12)
    {
        iyear++;
        imonth -= 12;
    }

    if (month) *month = imonth;
    if (year) *year = iyear;
}

/* There are two types of billing terms:
 *
 * Type DAYS defines a due date to be a fixed number of days passed the post
 * date. This is a straightforward calculation.
 *
 * The other type PROXIMO defines the due date as a fixed day of the month
 * (like always the 15th of the month). The proximo algorithm determines which
 * month based on the cutoff day and the post date. See above for a more
 * detailed explanation of proximo.
 */

static Timespec
compute_time (const GncBillTerm *term, Timespec post_date, int days)
{
    Timespec res = post_date;
    int day, month, year;

    switch (term->type)
    {
    case GNC_TERM_TYPE_DAYS:
        res.tv_sec += (SECS_PER_DAY * days);
        break;
    case GNC_TERM_TYPE_PROXIMO:
        compute_monthyear (term, post_date, &month, &year);
        day = gnc_date_get_last_mday (month - 1, year);
        if (days < day)
            day = days;
        res = gnc_dmy2timespec (day, month, year);
        break;
    }
    return res;
}

Timespec
gncBillTermComputeDueDate (const GncBillTerm *term, Timespec post_date)
{
    Timespec res = post_date;
    if (!term) return res;

    return compute_time (term, post_date, term->due_days);
}
/* Package-Private functions */

static void _gncBillTermCreate (QofBook *book)
{
    struct _book_info *bi;

    if (!book) return;

    bi = g_new0 (struct _book_info, 1);
    qof_book_set_data (book, _GNC_MOD_NAME, bi);
}

static void _gncBillTermDestroy (QofBook *book)
{
    struct _book_info *bi;

    if (!book) return;

    bi = qof_book_get_data (book, _GNC_MOD_NAME);

    g_list_free (bi->terms);
    g_free (bi);
}

static QofObject gncBillTermDesc =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) _GNC_MOD_NAME,
    DI(.type_label        = ) "Billing Term",
    DI(.create            = ) (gpointer)gncBillTermCreate,
    DI(.book_begin        = ) _gncBillTermCreate,
    DI(.book_end          = ) _gncBillTermDestroy,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) NULL,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncBillTermRegister (void)
{
    static QofParam params[] =
    {
        { GNC_BILLTERM_NAME, 		QOF_TYPE_STRING,  (QofAccessFunc)gncBillTermGetName,			(QofSetterFunc)gncBillTermSetName },
        { GNC_BILLTERM_DESC, 		QOF_TYPE_STRING,  (QofAccessFunc)gncBillTermGetDescription,		(QofSetterFunc)gncBillTermSetDescription },
        { GNC_BILLTERM_TYPE, QOF_TYPE_STRING, (QofAccessFunc)qofBillTermGetType, (QofSetterFunc)qofBillTermSetType },
        { GNC_BILLTERM_DUEDAYS, 	QOF_TYPE_INT32,   (QofAccessFunc)gncBillTermGetDueDays, 		(QofSetterFunc)gncBillTermSetDueDays },
        { GNC_BILLTERM_DISCDAYS, 	QOF_TYPE_INT32,   (QofAccessFunc)gncBillTermGetDiscountDays,	(QofSetterFunc)gncBillTermSetDiscountDays },
        { GNC_BILLTERM_DISCOUNT, 	QOF_TYPE_NUMERIC, (QofAccessFunc)gncBillTermGetDiscount,		(QofSetterFunc)gncBillTermSetDiscount },
        { GNC_BILLTERM_CUTOFF, 		QOF_TYPE_INT32,   (QofAccessFunc)gncBillTermGetCutoff, 			(QofSetterFunc)gncBillTermSetCutoff },
        { GNC_BILLTERM_REFCOUNT, 	QOF_TYPE_INT64,   (QofAccessFunc)gncBillTermGetRefcount, 		NULL },
        { QOF_PARAM_BOOK, 			QOF_ID_BOOK, 	  (QofAccessFunc)qof_instance_get_book, 		NULL },
        { QOF_PARAM_GUID, 			QOF_TYPE_GUID, 	  (QofAccessFunc)qof_instance_get_guid, 		NULL },
        { NULL },
    };

    qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncBillTermCompare, params);

    return qof_object_register (&gncBillTermDesc);
}
