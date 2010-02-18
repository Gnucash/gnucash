/***************************************************************************
 *            test-recursive.c
 *
 *  Wed Feb  1 21:54:49 2006
 *  Copyright  2006  Neil Williams
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor
 *  Boston, MA  02110-1301,  USA
 */

#include "config.h"
#include <glib.h>
#include <glib/gprintf.h>

#include "qof.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

#define GRAND_MODULE_NAME "recursive-grandparent"
#define PARENT_MODULE_NAME "recursive-parent"
#define CHILD_MODULE_NAME "recursive-child"
#define GRAND_MODULE_DESC "Recursive Grand Parent Test"
#define PARENT_MODULE_DESC "Recursive Parent Test"
#define CHILD_MODULE_DESC "Recursive Child Test"
#define OBJ_NAME "somename"
#define OBJ_AMOUNT "anamount"
#define OBJ_DATE "nottoday"
#define OBJ_DISCOUNT "hefty"
#define OBJ_VERSION "early"
#define OBJ_MINOR "tiny"
#define OBJ_ACTIVE "ofcourse"
#define OBJ_FLAG   "tiny_flag"
#define OBJ_RELATIVE "family"
#define OBJ_LIST "descendents"

/* set to TRUE to get QSF XML output
 * requires QSF available (i.e. make install) */
static gboolean debug = FALSE;

/* simple object structure */
typedef struct child_s
{
    QofInstance inst;
    gchar       *Name;
    gchar       flag;
    gnc_numeric Amount;
    Timespec    date;
    double      discount; /* cheap pun, I know. */
    gboolean    active;
    gint32      version;
    gint64 	    minor;
} mychild;

typedef struct childClass_s
{
    QofInstanceClass parent_class;
} mychildClass;

/* simple object structure */
typedef struct parent_s
{
    QofInstance inst;
    mychild     *child;
    gchar       *Name;
    gchar       flag;
    gnc_numeric Amount;
    Timespec    date;
    double      discount; /* cheap pun, I know. */
    gboolean    active;
    gint32      version;
    gint64      minor;
} myparent;

typedef struct parentClass_s
{
    QofInstanceClass parent_class;
} myparentClass;

/* simple object structure */
typedef struct grand_s
{
    QofInstance  inst;
    myparent     *child;
    GList        *descend;
    gchar        *Name;
    gchar        flag;
    gnc_numeric  Amount;
    Timespec     date;
    double       discount; /* cheap pun, I know. */
    gboolean     active;
    gint32       version;
    gint64       minor;
} mygrand;

typedef struct grandClass_s
{
    QofInstanceClass parent_class;
} mygrandClass;

mygrand* grand_create(QofBook*);
myparent* parent_create(QofBook*);
mychild* child_create(QofBook*);

gboolean mygrandRegister (void);
gboolean myparentRegister (void);
gboolean mychildRegister (void);

/* obvious setter functions */
void grand_setName(mygrand*,	gchar*);
void grand_setAmount(mygrand*,  gnc_numeric);
void grand_setDate(mygrand*,	Timespec h);
void grand_setDiscount(mygrand*, double);
void grand_setActive(mygrand*,  gboolean);
void grand_setVersion(mygrand*, gint32);
void grand_setMinor(mygrand*,   gint64);
void grand_setFlag(mygrand*,    gchar);

/* obvious getter functions */
gchar*      grand_getName(mygrand*);
gnc_numeric grand_getAmount(mygrand*);
Timespec    grand_getDate(mygrand*);
double	    grand_getDiscount(mygrand*);
gboolean    grand_getActive(mygrand*);
gint32	    grand_getVersion(mygrand*);
gint64	    grand_getMinor(mygrand*);
gchar       grand_getFlag(mygrand*);

/* obvious setter functions */
void parent_setName(myparent*,	   gchar*);
void parent_setAmount(myparent*,   gnc_numeric);
void parent_setDate(myparent*,	   Timespec h);
void parent_setDiscount(myparent*, double);
void parent_setActive(myparent*,   gboolean);
void parent_setVersion(myparent*,  gint32);
void parent_setMinor(myparent*,    gint64);
void parent_setFlag(myparent*,     gchar);

/* obvious getter functions */
gchar*	    parent_getName(myparent*);
gnc_numeric parent_getAmount(myparent*);
Timespec    parent_getDate(myparent*);
double	    parent_getDiscount(myparent*);
gboolean    parent_getActive(myparent*);
gint32	    parent_getVersion(myparent*);
gint64	    parent_getMinor(myparent*);
gchar       parent_getFlag(myparent*);

/* obvious setter functions */
void child_setName(mychild*,	   gchar*);
void child_setAmount(mychild*,   gnc_numeric);
void child_setDate(mychild*,	   Timespec h);
void child_setDiscount(mychild*, double);
void child_setActive(mychild*,   gboolean);
void child_setVersion(mychild*,  gint32);
void child_setMinor(mychild*,    gint64);
void child_setFlag(mychild*,     gchar);

/* obvious getter functions */
gchar*	    child_getName(mychild*);
gnc_numeric child_getAmount(mychild*);
Timespec    child_getDate(mychild*);
double	    child_getDiscount(mychild*);
gboolean    child_getActive(mychild*);
gint32	    child_getVersion(mychild*);
gint64	    child_getMinor(mychild*);
gchar       child_getFlag(mychild*);

/* --- type macros --- */
#define GNC_TYPE_MYCHILD            (gnc_mychild_get_type ())
#define GNC_MYCHILD(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_MYCHILD, mychild))
#define GNC_MYCHILD_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_MYCHILD, mychildClass))
#define GNC_IS_MYCHILD(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MYCHILD))
#define GNC_IS_MYCHILD_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_MYCHILD))
#define GNC_MYCHILD_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_MYCHILD, mychildClass))
GType gnc_mychild_get_type(void);

/* --- type macros --- */
#define GNC_TYPE_MYPARENT            (gnc_myparent_get_type ())
#define GNC_MYPARENT(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_MYPARENT, myparent))
#define GNC_MYPARENT_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_MYPARENT, myparentClass))
#define GNC_IS_MYPARENT(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MYPARENT))
#define GNC_IS_MYPARENT_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_MYPARENT))
#define GNC_MYPARENT_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_MYPARENT, myparentClass))
GType gnc_myparent_get_type(void);

/* --- type macros --- */
#define GNC_TYPE_MYGRAND            (gnc_mygrand_get_type ())
#define GNC_MYGRAND(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_MYGRAND, mygrand))
#define GNC_MYGRAND_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_MYGRAND, mygrandClass))
#define GNC_IS_MYGRAND(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MYGRAND))
#define GNC_IS_MYGRAND_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_MYGRAND))
#define GNC_MYGRAND_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_MYGRAND, mygrandClass))
GType gnc_mygrand_get_type(void);

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_mychild, mychild, QOF_TYPE_INSTANCE);

static void
gnc_mychild_init(mychild* obj)
{
}

static void
gnc_mychild_dispose_real (GObject *objp)
{
}

static void
gnc_mychild_finalize_real(GObject* objp)
{
}

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_myparent, myparent, QOF_TYPE_INSTANCE);

static void
gnc_myparent_init(myparent* obj)
{
}

static void
gnc_myparent_dispose_real (GObject *objp)
{
}

static void
gnc_myparent_finalize_real(GObject* objp)
{
}

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_mygrand, mygrand, QOF_TYPE_INSTANCE);

static void
gnc_mygrand_init(mygrand* obj)
{
}

static void
gnc_mygrand_dispose_real (GObject *objp)
{
}

static void
gnc_mygrand_finalize_real(GObject* objp)
{
}

mygrand*
grand_create(QofBook *book)
{
    mygrand *g;

    g_return_val_if_fail(book, NULL);
    g = g_object_new(GNC_TYPE_MYGRAND, NULL);
    qof_instance_init_data (&g->inst, GRAND_MODULE_NAME, book);
    g->date = *get_random_timespec();
    g->discount = get_random_double();;
    g->active = get_random_boolean();
    g->version = get_random_int_in_range(1, 10000);
    g->minor = get_random_int_in_range(100001, 99999999);
    g->flag = get_random_character();
    g->Name = get_random_string();
    g->Amount = get_random_gnc_numeric();
    g->child = NULL;
    g->descend = NULL;
    qof_event_gen(&g->inst, QOF_EVENT_CREATE, NULL);
    return g;
}

myparent*
parent_create(QofBook *book)
{
    myparent *g;

    g_return_val_if_fail(book, NULL);
    g = g_object_new(GNC_TYPE_MYPARENT, NULL);
    qof_instance_init_data (&g->inst, PARENT_MODULE_NAME, book);
    g->date = *get_random_timespec();
    g->discount = get_random_double();
    g->active = get_random_boolean();
    g->version = get_random_int_in_range(1, 10000);
    g->minor = get_random_int_in_range(100001, 99999999);
    g->flag = get_random_character();
    g->Name = get_random_string();
    g->Amount = get_random_gnc_numeric();
    g->child = NULL;
    qof_event_gen(&g->inst, QOF_EVENT_CREATE, NULL);
    return g;
}

mychild*
child_create(QofBook *book)
{
    mychild *g;

    g_return_val_if_fail(book, NULL);
    g = g_object_new(GNC_TYPE_MYCHILD, NULL);
    qof_instance_init_data (&g->inst, CHILD_MODULE_NAME, book);
    g->date = *get_random_timespec();
    g->discount = get_random_double();
    g->active = get_random_boolean();
    g->version = get_random_int_in_range(1, 10000);
    g->minor = get_random_int_in_range(100001, 99999999);
    g->flag = get_random_character();
    g->Name = get_random_string();
    g->Amount = get_random_gnc_numeric();
    qof_event_gen(&g->inst, QOF_EVENT_CREATE, NULL);
    return g;
}

static void
descend_cb (QofInstance *ent, gpointer user_data)
{
    mygrand *g = (mygrand*)user_data;

    g_return_if_fail(g || ent);
    g->descend = g_list_prepend(g->descend, (mychild*)ent);
}

static void
grand_setDescend(mygrand *g, QofCollection *coll)
{
    g_return_if_fail(g || coll);
    if (0 != safe_strcmp(qof_collection_get_type(coll), CHILD_MODULE_NAME))
    {
        return;
    }
    qof_collection_foreach(coll, descend_cb, g);
}

static QofCollection*
grand_getDescend(mygrand *g)
{
    QofCollection *col;
    QofInstance *ent;
    GList *list;

    g_return_val_if_fail(g, NULL);
    col = qof_collection_new(CHILD_MODULE_NAME);
    for (list = g_list_copy(g->descend); list; list = list->next)
    {
        ent = QOF_INSTANCE(list->data);
        if (!ent)
        {
            break;
        }
        do_test(0 == safe_strcmp(ent->e_type, CHILD_MODULE_NAME), "wrong entity");
        qof_collection_add_entity(col, ent);
    }
    return col;
}

static void
grand_setChild(mygrand *g, myparent *p)
{
    g_return_if_fail(g || p);
    g->child = p;
}

static myparent*
grand_getChild(mygrand *g)
{
    g_return_val_if_fail(g, NULL);
    return g->child;
}

void
grand_setFlag(mygrand *g, gchar f)
{
    g_return_if_fail(g);
    g->flag = f;
}

gchar
grand_getFlag(mygrand *g)
{
    g_return_val_if_fail(g, 'n');
    return g->flag;
}

void
grand_setMinor(mygrand *g, gint64 h)
{
    g_return_if_fail(g != NULL);
    g->minor = h;
}

gint64
grand_getMinor(mygrand *g)
{
    g_return_val_if_fail((g != NULL), 0);
    return g->minor;
}

void
grand_setVersion(mygrand *g, gint32 h)
{
    g_return_if_fail(g != NULL);
    g->version = h;
}

gint32
grand_getVersion(mygrand *g)
{
    if (!g) return 0;
    return g->version;
}

void
grand_setActive(mygrand *g, gboolean h)
{
    if (!g) return;
    g->active = h;
}

gboolean
grand_getActive(mygrand *g)
{
    if (!g) return FALSE;
    return g->active;
}

void
grand_setDiscount(mygrand *g, double h)
{
    if (!g) return;
    g->discount = h;
}

double
grand_getDiscount(mygrand *g)
{
    if (!g) return 0;
    return g->discount;
}

void
grand_setDate(mygrand *g, Timespec h)
{
    if (!g) return;
    g->date = h;
}

Timespec
grand_getDate(mygrand *g)
{
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!g) return ts;
    ts = g->date;
    return ts;
}

void
grand_setName(mygrand* g, gchar* h)
{
    if (!g || !h) return;
    g->Name = strdup(h);
}

gchar*
grand_getName(mygrand *g)
{
    if (!g) return NULL;
    return g->Name;
}

void
grand_setAmount(mygrand *g, gnc_numeric h)
{
    if (!g) return;
    g->Amount = h;
}

gnc_numeric
grand_getAmount(mygrand *g)
{
    if (!g) return gnc_numeric_zero();
    return g->Amount;
}

static void
parent_setChild(myparent *p, mychild *c)
{
    g_return_if_fail(p || c);
    p->child = c;
}

static mychild*
parent_getChild(myparent* p)
{
    g_return_val_if_fail(p, NULL);
    return p->child;
}

void
parent_setFlag(myparent *p, gchar f)
{
    g_return_if_fail(p);
    p->flag = f;
}

gchar
parent_getFlag(myparent *p)
{
    g_return_val_if_fail(p, 'n');
    return p->flag;
}

void
parent_setMinor(myparent *p, gint64 h)
{
    g_return_if_fail(p != NULL);
    p->minor = h;
}

gint64
parent_getMinor(myparent *p)
{
    g_return_val_if_fail((p != NULL), 0);
    return p->minor;
}

void
parent_setVersion(myparent *p, gint32 h)
{
    g_return_if_fail(p != NULL);
    p->version = h;
}

gint32
parent_getVersion(myparent *p)
{
    if (!p) return 0;
    return p->version;
}

void
parent_setActive(myparent *p, gboolean h)
{
    if (!p) return;
    p->active = h;
}

gboolean
parent_getActive(myparent *p)
{
    if (!p) return FALSE;
    return p->active;
}

void
parent_setDiscount(myparent *p, double h)
{
    if (!p) return;
    p->discount = h;
}

double
parent_getDiscount(myparent *p)
{
    if (!p) return 0;
    return p->discount;
}

void
parent_setDate(myparent *p, Timespec h)
{
    if (!p) return;
    p->date = h;
}

Timespec
parent_getDate(myparent *p)
{
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!p) return ts;
    ts = p->date;
    return ts;
}

void
parent_setName(myparent* p, gchar* h)
{
    if (!p || !h) return;
    p->Name = strdup(h);
}

gchar*
parent_getName(myparent *p)
{
    if (!p) return NULL;
    return p->Name;
}

void
parent_setAmount(myparent *p, gnc_numeric h)
{
    if (!p) return;
    p->Amount = h;
}

gnc_numeric
parent_getAmount(myparent *p)
{
    if (!p) return gnc_numeric_zero();
    return p->Amount;
}

void
child_setFlag(mychild *c, gchar f)
{
    g_return_if_fail(c);
    c->flag = f;
}

gchar
child_getFlag(mychild *c)
{
    g_return_val_if_fail(c, 'n');
    return c->flag;
}

void
child_setMinor(mychild *c, gint64 h)
{
    g_return_if_fail(c != NULL);
    c->minor = h;
}

gint64
child_getMinor(mychild *c)
{
    g_return_val_if_fail((c != NULL), 0);
    return c->minor;
}

void
child_setVersion(mychild *c, gint32 h)
{
    g_return_if_fail(c != NULL);
    c->version = h;
}

gint32
child_getVersion(mychild *c)
{
    if (!c) return 0;
    return c->version;
}

void
child_setActive(mychild *c, gboolean h)
{
    if (!c) return;
    c->active = h;
}

gboolean
child_getActive(mychild *c)
{
    if (!c) return FALSE;
    return c->active;
}

void
child_setDiscount(mychild *c, double h)
{
    if (!c) return;
    c->discount = h;
}

double
child_getDiscount(mychild *c)
{
    if (!c) return 0;
    return c->discount;
}

void
child_setDate(mychild *c, Timespec h)
{
    if (!c) return;
    c->date = h;
}

Timespec
child_getDate(mychild *c)
{
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!c) return ts;
    ts = c->date;
    return ts;
}

void
child_setName(mychild* c, gchar* h)
{
    if (!c || !h) return;
    c->Name = strdup(h);
}

gchar*
child_getName(mychild *c)
{
    if (!c) return NULL;
    return c->Name;
}

void
child_setAmount(mychild *c, gnc_numeric h)
{
    if (!c) return;
    c->Amount = h;
}

gnc_numeric
child_getAmount(mychild *c)
{
    if (!c) return gnc_numeric_zero();
    return c->Amount;
}

static QofObject grand_object_def =
{
interface_version:
    QOF_OBJECT_VERSION,
e_type:
    GRAND_MODULE_NAME,
type_label:
    GRAND_MODULE_DESC,
create:
    (gpointer)grand_create,
book_begin:
    NULL,
book_end:
    NULL,
is_dirty:
    qof_collection_is_dirty,
mark_clean:
    qof_collection_mark_clean,
foreach:
    qof_collection_foreach,
printable:
    NULL,
version_cmp:
    (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean mygrandRegister (void)
{
    static QofParam params[] =
    {
        {
            OBJ_NAME,     QOF_TYPE_STRING,  (QofAccessFunc)grand_getName,
            (QofSetterFunc)grand_setName
        },
        {
            OBJ_AMOUNT,   QOF_TYPE_NUMERIC, (QofAccessFunc)grand_getAmount,
            (QofSetterFunc)grand_setAmount
        },
        {
            OBJ_DATE,     QOF_TYPE_DATE,    (QofAccessFunc)grand_getDate,
            (QofSetterFunc)grand_setDate
        },
        {
            OBJ_DISCOUNT, QOF_TYPE_DOUBLE,  (QofAccessFunc)grand_getDiscount,
            (QofSetterFunc)grand_setDiscount
        },
        {
            OBJ_ACTIVE,   QOF_TYPE_BOOLEAN, (QofAccessFunc)grand_getActive,
            (QofSetterFunc)grand_setActive
        },
        {
            OBJ_VERSION,  QOF_TYPE_INT32,   (QofAccessFunc)grand_getVersion,
            (QofSetterFunc)grand_setVersion
        },
        {
            OBJ_MINOR,    QOF_TYPE_INT64,	  (QofAccessFunc)grand_getMinor,
            (QofSetterFunc)grand_setMinor
        },
        {
            OBJ_FLAG,     QOF_TYPE_CHAR,    (QofAccessFunc)grand_getFlag,
            (QofSetterFunc)grand_setFlag
        },
        {
            OBJ_RELATIVE,	PARENT_MODULE_NAME, (QofAccessFunc)grand_getChild,
            (QofSetterFunc)grand_setChild
        },
        {
            OBJ_LIST,    QOF_TYPE_COLLECT,  (QofAccessFunc)grand_getDescend,
            (QofSetterFunc)grand_setDescend
        },
        { QOF_PARAM_BOOK, QOF_ID_BOOK,	(QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID,	(QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    qof_class_register (GRAND_MODULE_NAME, NULL, params);
    /*  if(!qof_choice_create(GRAND_MODULE_NAME)) { return FALSE; }*/

    return qof_object_register (&grand_object_def);
}

static QofObject parent_object_def =
{
interface_version:
    QOF_OBJECT_VERSION,
e_type:
    PARENT_MODULE_NAME,
type_label:
    PARENT_MODULE_DESC,
create:
    (gpointer)parent_create,
book_begin:
    NULL,
book_end:
    NULL,
is_dirty:
    qof_collection_is_dirty,
mark_clean:
    qof_collection_mark_clean,
foreach:
    qof_collection_foreach,
printable:
    NULL,
version_cmp:
    (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean myparentRegister (void)
{
    static QofParam params[] =
    {
        {
            OBJ_NAME,     QOF_TYPE_STRING,  (QofAccessFunc)parent_getName,
            (QofSetterFunc)parent_setName
        },
        {
            OBJ_AMOUNT,   QOF_TYPE_NUMERIC, (QofAccessFunc)parent_getAmount,
            (QofSetterFunc)parent_setAmount
        },
        {
            OBJ_DATE,     QOF_TYPE_DATE,    (QofAccessFunc)parent_getDate,
            (QofSetterFunc)parent_setDate
        },
        {
            OBJ_DISCOUNT, QOF_TYPE_DOUBLE,  (QofAccessFunc)parent_getDiscount,
            (QofSetterFunc)parent_setDiscount
        },
        {
            OBJ_ACTIVE,   QOF_TYPE_BOOLEAN, (QofAccessFunc)parent_getActive,
            (QofSetterFunc)parent_setActive
        },
        {
            OBJ_VERSION,  QOF_TYPE_INT32,   (QofAccessFunc)parent_getVersion,
            (QofSetterFunc)parent_setVersion
        },
        {
            OBJ_MINOR,    QOF_TYPE_INT64,	  (QofAccessFunc)parent_getMinor,
            (QofSetterFunc)parent_setMinor
        },
        {
            OBJ_FLAG,     QOF_TYPE_CHAR,    (QofAccessFunc)parent_getFlag,
            (QofSetterFunc)parent_setFlag
        },
        {
            OBJ_RELATIVE,	CHILD_MODULE_NAME, (QofAccessFunc)parent_getChild,
            (QofSetterFunc)parent_setChild
        },
        { QOF_PARAM_BOOK, QOF_ID_BOOK,	(QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID,	(QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    qof_class_register (PARENT_MODULE_NAME, NULL, params);

    return qof_object_register (&parent_object_def);
}

static QofObject child_object_def =
{
interface_version:
    QOF_OBJECT_VERSION,
e_type:
    CHILD_MODULE_NAME,
type_label:
    CHILD_MODULE_DESC,
create:
    (gpointer)child_create,
book_begin:
    NULL,
book_end:
    NULL,
is_dirty:
    qof_collection_is_dirty,
mark_clean:
    qof_collection_mark_clean,
foreach:
    qof_collection_foreach,
printable:
    NULL,
version_cmp:
    (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean mychildRegister (void)
{
    static QofParam params[] =
    {
        {
            OBJ_NAME,     QOF_TYPE_STRING,  (QofAccessFunc)child_getName,
            (QofSetterFunc)child_setName
        },
        {
            OBJ_AMOUNT,   QOF_TYPE_NUMERIC, (QofAccessFunc)child_getAmount,
            (QofSetterFunc)child_setAmount
        },
        {
            OBJ_DATE,     QOF_TYPE_DATE,    (QofAccessFunc)child_getDate,
            (QofSetterFunc)child_setDate
        },
        {
            OBJ_DISCOUNT, QOF_TYPE_DOUBLE,  (QofAccessFunc)child_getDiscount,
            (QofSetterFunc)child_setDiscount
        },
        {
            OBJ_ACTIVE,   QOF_TYPE_BOOLEAN, (QofAccessFunc)child_getActive,
            (QofSetterFunc)child_setActive
        },
        {
            OBJ_VERSION,  QOF_TYPE_INT32,   (QofAccessFunc)child_getVersion,
            (QofSetterFunc)child_setVersion
        },
        {
            OBJ_MINOR,    QOF_TYPE_INT64,	  (QofAccessFunc)child_getMinor,
            (QofSetterFunc)child_setMinor
        },
        {
            OBJ_FLAG,     QOF_TYPE_CHAR,    (QofAccessFunc)child_getFlag,
            (QofSetterFunc)child_setFlag
        },
        { QOF_PARAM_BOOK, QOF_ID_BOOK,	(QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID,	(QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    qof_class_register (CHILD_MODULE_NAME, NULL, params);

    return qof_object_register (&child_object_def);
}

static void
create_data (QofSession *original, guint counter)
{
    QofCollection *coll;
    QofBook *start;
    mygrand *grand1;
    myparent *parent1;
    mychild *child1;

    start = qof_session_get_book(original);
    grand1 = (mygrand*)qof_object_new_instance(GRAND_MODULE_NAME, start);
    do_test ((NULL != &grand1->inst), "instance init");
    switch (counter)
    {
    case 0 :   /* NULL tree */
    {
        do_test((grand1 != NULL), "empty tree check");
        coll = qof_book_get_collection(start, GRAND_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Too many grandparents found - should be 1");
        coll = qof_book_get_collection(start, CHILD_MODULE_NAME);
        do_test((qof_collection_count(coll) == 0),
                "child found, should be empty");
        coll = qof_book_get_collection(start, PARENT_MODULE_NAME);
        do_test((qof_collection_count(coll) == 0),
                "tree not empty: parent found");
        break;
    }
    case 1 :   /* one parent, no child */
    {
        parent1 = (myparent*)qof_object_new_instance(PARENT_MODULE_NAME, start);
        grand_setChild(grand1, parent1);
        do_test((parent1 != NULL), "single parent check");
        do_test((grand_getChild(grand1) == parent1), "set child in grandparent");
        coll = qof_book_get_collection(start, GRAND_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Wrong number of grandparents, should be 1");
        coll = qof_book_get_collection(start, CHILD_MODULE_NAME);
        do_test((qof_collection_count(coll) == 0),
                "Should be no child entities this iteration.");
        coll = qof_book_get_collection(start, PARENT_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Wrong number of parents found, should be 1");
        break;
    }
    case 2 :   /* one parent, one child */
    {
        parent1 = (myparent*)qof_object_new_instance(PARENT_MODULE_NAME, start);
        grand_setChild(grand1, parent1);
        child1 = (mychild*)qof_object_new_instance(CHILD_MODULE_NAME, start);
        parent1 = grand_getChild(grand1);
        parent_setChild(parent1, child1);
        do_test((child1 != NULL), "one parent with one related child");
        do_test((child1 == parent_getChild(parent1)), "child of single parent");
        coll = qof_book_get_collection(start, GRAND_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Wrong number of grandparents. Should be 1");
        coll = qof_book_get_collection(start, CHILD_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Wrong number of child entities, should be 1");
        coll = qof_book_get_collection(start, PARENT_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Wrong number of parents. Should be 1");
        break;
    }
    case 3 :   /* same grand, new parent, same child */
    {
        child1 = (mychild*)qof_object_new_instance(CHILD_MODULE_NAME, start);
        parent1 = (myparent*)qof_object_new_instance(PARENT_MODULE_NAME, start);
        grand_setChild(grand1, parent1);
        parent_setChild(parent1, child1);
        do_test((parent1 == grand_getChild(grand1)), "same grandparent, new parent");
        do_test((child1 == parent_getChild(parent1)), "new parent, same child");
        coll = qof_book_get_collection(start, GRAND_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Wrong number of grandparents. Should be 1, Iteration 3.");
        coll = qof_book_get_collection(start, CHILD_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Wrong number of child entities, should be 1. Iteration 3.");
        coll = qof_book_get_collection(start, PARENT_MODULE_NAME);
        do_test((qof_collection_count(coll) == 1),
                "Wrong number of parents. Should be 1. Iteration 3.");
        break;
    }
    case 4 :   /* new grand, unrelated parent, child unrelated to grand */
    {
        grand1 = (mygrand*)qof_object_new_instance(GRAND_MODULE_NAME, start);
        parent1 = (myparent*)qof_object_new_instance(PARENT_MODULE_NAME, start);
        child1 = (mychild*)qof_object_new_instance(CHILD_MODULE_NAME, start);
        parent_setChild(parent1, child1);
        do_test((NULL == grand_getChild(grand1)), "new grand, unrelated parent");
        do_test((child1 == parent_getChild(parent1)), "child unrelated to grand");
        coll = grand_getDescend(grand1);
        do_test((coll != NULL), "grandparent not valid");
        if (coll)
        {
            QofInstance *ent;

            ent = QOF_INSTANCE(child1);
            qof_collection_add_entity(coll, ent);
            grand_setDescend(grand1, coll);
            qof_collection_destroy(coll);
            do_test((g_list_length(grand1->descend) > 0), "entity not added");
            do_test((qof_collection_count(grand_getDescend(grand1)) > 0),
                    "empty collection returned");
        }
        break;
    }
    }
}

struct tally
{
    guint nulls, total, collect;
    QofBook *book;
};

static void
check_cb (QofInstance *ent, gpointer data)
{
    QofInstance *parent, *child;
    QofCollection *coll;
    struct tally *c;
    const QofParam *param;
    mygrand  *testg;
    myparent *testp;
    mychild  *testc;

    c = (struct tally*)data;
    /* check the same number and type of entities
    exist in the copied book */
    testg = (mygrand*)ent;
    /* we always have a grandparent */
    do_test((testg != NULL), "grandparent not found");
    c->total++;
    param = qof_class_get_parameter(GRAND_MODULE_NAME, OBJ_LIST);
    coll = (QofCollection*)param->param_getfcn(ent, param);
    c->collect = qof_collection_count(coll);
    if (c->book)
    {
        qof_book_set_references(c->book);
    }
    param = qof_class_get_parameter(GRAND_MODULE_NAME, OBJ_RELATIVE);
    parent = QOF_INSTANCE(param->param_getfcn(ent, param));
    testp = grand_getChild((mygrand*)ent);
    /* not all grandparents have family so just keep count. */
    if (!parent)
    {
        c->nulls++;
        return;
    }
    do_test((0 == safe_strcmp(parent_getName(testp),
                              parent_getName((myparent*)parent))), "parent copy test");
    param = qof_class_get_parameter(PARENT_MODULE_NAME, OBJ_RELATIVE);
    child = param->param_getfcn(parent, param);
    testc = parent_getChild((myparent*)parent);
    if (!child)
    {
        c->nulls++;
        return;
    }
    do_test((0 == safe_strcmp(child_getName(testc),
                              child_getName((mychild*)child))), "child copy test");
}

static void
test_recursion (QofSession *original, guint counter)
{
    QofSession *copy;
    QofCollection *grand_coll;
    struct tally c;
    QofBook *book;
    guint d, e, f;

    c.nulls = 0;
    c.total = 0;
    c.collect = 0;
    c.book = NULL;
    book = qof_session_get_book(original);
    grand_coll = qof_book_get_collection(book, GRAND_MODULE_NAME);
    copy = qof_session_new();
    if (debug)
    {
        qof_session_begin(copy, QOF_STDOUT, TRUE, FALSE);
    }
    /* TODO: implement QOF_TYPE_CHOICE testing. */
    qof_instance_copy_coll_r(copy, grand_coll);
    /* test the original */
    qof_object_foreach(GRAND_MODULE_NAME, book, check_cb, &c);
    book = qof_session_get_book(copy);
    /* test the copy */
    d = c.nulls;
    e = c.total;
    f = c.collect;
    c.nulls = 0;
    c.total = 0;
    c.collect = 0;
    c.book = book;
    qof_object_foreach(GRAND_MODULE_NAME, book, check_cb, &c);
    do_test((d == c.nulls), "Null parents do not match");
    do_test((e == c.total), "Total parents do not match");
    do_test((f == c.collect), "Number of children in descendents does not match");
    if (counter == 4 && debug == TRUE)
    {
        qof_session_save(copy, NULL);
        qof_session_save(original, NULL);
    }
    qof_session_end(copy);
    copy = NULL;
}

int
main (int argc, const char *argv[])
{
    QofSession *original;
    guint counter;

    qof_init ();
    mygrandRegister();
    myparentRegister();
    mychildRegister();
    for (counter = 0; counter < 35; counter++)
    {
        original = qof_session_new();
        if (debug)
        {
            qof_session_begin(original, QOF_STDOUT, TRUE, FALSE);
        }
        create_data(original, (counter % 5));
        test_recursion(original, (counter % 5));
        qof_session_end(original);
    }
    print_test_results();
    qof_close();
    return EXIT_SUCCESS;
}
