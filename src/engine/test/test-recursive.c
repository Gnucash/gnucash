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
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <glib.h>
#include <glib/gprintf.h>
#define _GNU_SOURCE

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
#define OBJ_GUID "unique"
#define OBJ_DISCOUNT "hefty"
#define OBJ_VERSION "early"
#define OBJ_MINOR "tiny"
#define OBJ_ACTIVE "ofcourse"
#define OBJ_FLAG   "tiny_flag"
#define OBJ_RELATIVE "family"

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
	const GUID  *child_guid;
	Timespec    date;
	double      discount; /* cheap pun, I know. */
	gboolean    active;
	gint32      version;
	gint64 	    minor;
}mychild;

/* simple object structure */
typedef struct parent_s
{
	QofInstance inst;
	mychild     *child;
	gchar       *Name;
	gchar       flag;
	gnc_numeric Amount;
	const GUID  *parent_guid;
	Timespec    date;
	double      discount; /* cheap pun, I know. */
	gboolean    active;
	gint32      version;
	gint64      minor;
}myparent;

	/* simple object structure */
typedef struct grand_s
{
	QofInstance  inst;
	myparent     *child;
	gchar        *Name;
	gchar        flag;
	gnc_numeric  Amount;
	const GUID   *grand_guid;
	Timespec     date;
	double       discount; /* cheap pun, I know. */
	gboolean     active;
	gint32       version;
	gint64       minor;
}mygrand;

mygrand* grand_create(QofBook*);
myparent* parent_create(QofBook*);
mychild* child_create(QofBook*);

gboolean mygrandRegister (void);
gboolean myparentRegister (void);
gboolean mychildRegister (void);

/* obvious setter functions */
void grand_setName(mygrand*,	gchar*);
void grand_setGUID(mygrand*,	const GUID*);
void grand_setAmount(mygrand*,  gnc_numeric);
void grand_setDate(mygrand*,	Timespec h);
void grand_setDiscount(mygrand*, double);
void grand_setActive(mygrand*,  gboolean);
void grand_setVersion(mygrand*, gint32);
void grand_setMinor(mygrand*,   gint64);
void grand_setFlag(mygrand*,    gchar);

/* obvious getter functions */
gchar*      grand_getName(mygrand*);
const GUID* grand_getGUID(mygrand*);
gnc_numeric grand_getAmount(mygrand*);
Timespec    grand_getDate(mygrand*);
double	    grand_getDiscount(mygrand*);
gboolean    grand_getActive(mygrand*);
gint32	    grand_getVersion(mygrand*);
gint64	    grand_getMinor(mygrand*);
gchar       grand_getFlag(mygrand*);

/* obvious setter functions */
void parent_setName(myparent*,	   gchar*);
void parent_setGUID(myparent*,	   const GUID*);
void parent_setAmount(myparent*,   gnc_numeric);
void parent_setDate(myparent*,	   Timespec h);
void parent_setDiscount(myparent*, double);
void parent_setActive(myparent*,   gboolean);
void parent_setVersion(myparent*,  gint32);
void parent_setMinor(myparent*,    gint64);
void parent_setFlag(myparent*,     gchar);

/* obvious getter functions */
gchar*	    parent_getName(myparent*);
const GUID* parent_getGUID(myparent*);
gnc_numeric parent_getAmount(myparent*);
Timespec    parent_getDate(myparent*);
double	    parent_getDiscount(myparent*);
gboolean    parent_getActive(myparent*);
gint32	    parent_getVersion(myparent*);
gint64	    parent_getMinor(myparent*);
gchar       parent_getFlag(myparent*);

/* obvious setter functions */
void child_setName(mychild*,	   gchar*);
void child_setGUID(mychild*,	   const GUID*);
void child_setAmount(mychild*,   gnc_numeric);
void child_setDate(mychild*,	   Timespec h);
void child_setDiscount(mychild*, double);
void child_setActive(mychild*,   gboolean);
void child_setVersion(mychild*,  gint32);
void child_setMinor(mychild*,    gint64);
void child_setFlag(mychild*,     gchar);

/* obvious getter functions */
gchar*	    child_getName(mychild*);
const GUID* child_getGUID(mychild*);
gnc_numeric child_getAmount(mychild*);
Timespec    child_getDate(mychild*);
double	    child_getDiscount(mychild*);
gboolean    child_getActive(mychild*);
gint32	    child_getVersion(mychild*);
gint64	    child_getMinor(mychild*);
gchar       child_getFlag(mychild*);

mygrand*
grand_create(QofBook *book)
{
	mygrand *g;

	g_return_val_if_fail(book, NULL);
	g = g_new0(mygrand, 1);
	qof_instance_init (&g->inst, GRAND_MODULE_NAME, book);
	g->grand_guid = get_random_guid();
	g->date = *get_random_timespec();
	g->discount = get_random_double();
	g->active = get_random_boolean();
	g->version = get_random_int_in_range(1,10000);
	g->minor = get_random_int_in_range(100000,99999999);
	g->flag = get_random_character();
	g->Name = get_random_string();
	g->Amount = get_random_gnc_numeric();
	g->child = NULL;
	gnc_engine_gen_event(&g->inst.entity, GNC_EVENT_CREATE);
	return g;
}

myparent*
parent_create(QofBook *book)
{
	myparent *g;

	g_return_val_if_fail(book, NULL);
	g = g_new0(myparent, 1);
	qof_instance_init (&g->inst, PARENT_MODULE_NAME, book);
	g->parent_guid = get_random_guid();
	g->date = *get_random_timespec();
	g->discount = get_random_double();
	g->active = get_random_boolean();
	g->version = get_random_int_in_range(1,10000);
	g->minor = get_random_int_in_range(100000,99999999);
	g->flag = get_random_character();
	g->Name = get_random_string();
	g->Amount = get_random_gnc_numeric();
	g->child = NULL;
	gnc_engine_gen_event(&g->inst.entity, GNC_EVENT_CREATE);
	return g;
}

mychild*
child_create(QofBook *book)
{
	mychild *g;

	g_return_val_if_fail(book, NULL);
	g = g_new0(mychild, 1);
	qof_instance_init (&g->inst, CHILD_MODULE_NAME, book);
	g->child_guid = get_random_guid();
	g->date = *get_random_timespec();
	g->discount = get_random_double();
	g->active = get_random_boolean();
	g->version = get_random_int_in_range(1,10000);
	g->minor = get_random_int_in_range(100000,99999999);
	g->flag = get_random_character();
	g->Name = get_random_string();
	g->Amount = get_random_gnc_numeric();
	gnc_engine_gen_event(&g->inst.entity, GNC_EVENT_CREATE);
	return g;
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
	g_return_val_if_fail((g != NULL),0);
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
	if(!g) return 0;
	return g->version;
}

void
grand_setActive(mygrand *g, gboolean h)
{
	if(!g) return;
	g->active = h;
}

gboolean
grand_getActive(mygrand *g)
{
	if(!g) return FALSE;
	return g->active;
}

void
grand_setDiscount(mygrand *g, double h)
{
	if(!g) return;
	g->discount = h;
}

double
grand_getDiscount(mygrand *g)
{
	if(!g) return 0;
	return g->discount;
}

void
grand_setDate(mygrand *g, Timespec h)
{
	if(!g) return;
	g->date = h;
}

Timespec
grand_getDate(mygrand *g)
{
	Timespec ts;
	ts.tv_sec = 0;
	ts.tv_nsec = 0;
	if(!g) return ts;
	ts = g->date;
	return ts;
}

void
grand_setGUID(mygrand* g, const GUID* h)
{
	if(!g) return;
	g->grand_guid = h;
}

const GUID*
grand_getGUID(mygrand *g)
{
	if(!g) return NULL;
	return g->grand_guid;
}

void
grand_setName(mygrand* g, gchar* h)
{
	if(!g || !h) return;
	g->Name = strdup(h);
}

gchar*
grand_getName(mygrand *g)
{
	if(!g) return NULL;
	return g->Name;
}

void
grand_setAmount(mygrand *g, gnc_numeric h)
{
	if(!g) return;
	g->Amount = h;
}

gnc_numeric
grand_getAmount(mygrand *g)
{
	if(!g) return gnc_numeric_zero();
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
	g_return_val_if_fail((p != NULL),0);
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
	if(!p) return 0;
	return p->version;
}

void
parent_setActive(myparent *p, gboolean h)
{
	if(!p) return;
	p->active = h;
}

gboolean
parent_getActive(myparent *p)
{
	if(!p) return FALSE;
	return p->active;
}

void
parent_setDiscount(myparent *p, double h)
{
	if(!p) return;
	p->discount = h;
}

double
parent_getDiscount(myparent *p)
{
	if(!p) return 0;
	return p->discount;
}

void
parent_setDate(myparent *p, Timespec h)
{
	if(!p) return;
	p->date = h;
}

Timespec
parent_getDate(myparent *p)
{
	Timespec ts;
	ts.tv_sec = 0;
	ts.tv_nsec = 0;
	if(!p) return ts;
	ts = p->date;
	return ts;
}

void
parent_setGUID(myparent* p, const GUID* h)
{
	if(!p) return;
	p->parent_guid = h;
}

const GUID*
parent_getGUID(myparent *p)
{
	if(!p) return NULL;
	return p->parent_guid;
}

void
parent_setName(myparent* p, gchar* h)
{
	if(!p || !h) return;
	p->Name = strdup(h);
}

gchar*
parent_getName(myparent *p)
{
	if(!p) return NULL;
	return p->Name;
}

void
parent_setAmount(myparent *p, gnc_numeric h)
{
	if(!p) return;
	p->Amount = h;
}

gnc_numeric
parent_getAmount(myparent *p)
{
	if(!p) return gnc_numeric_zero();
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
	g_return_val_if_fail((c != NULL),0);
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
	if(!c) return 0;
	return c->version;
}

void
child_setActive(mychild *c, gboolean h)
{
	if(!c) return;
	c->active = h;
}

gboolean
child_getActive(mychild *c)
{
	if(!c) return FALSE;
	return c->active;
}

void
child_setDiscount(mychild *c, double h)
{
	if(!c) return;
	c->discount = h;
}

double
child_getDiscount(mychild *c)
{
	if(!c) return 0;
	return c->discount;
}

void
child_setDate(mychild *c, Timespec h)
{
	if(!c) return;
	c->date = h;
}

Timespec
child_getDate(mychild *c)
{
	Timespec ts;
	ts.tv_sec = 0;
	ts.tv_nsec = 0;
	if(!c) return ts;
	ts = c->date;
	return ts;
}

void
child_setGUID(mychild* c, const GUID* h)
{
	if(!c) return;
	c->child_guid = h;
}

const GUID*
child_getGUID(mychild *c)
{
	if(!c) return NULL;
	return c->child_guid;
}

void
child_setName(mychild* c, gchar* h)
{
	if(!c || !h) return;
	c->Name = strdup(h);
}

gchar*
child_getName(mychild *c)
{
	if(!c) return NULL;
	return c->Name;
}

void
child_setAmount(mychild *c, gnc_numeric h)
{
	if(!c) return;
	c->Amount = h;
}

gnc_numeric
child_getAmount(mychild *c)
{
	if(!c) return gnc_numeric_zero();
	return c->Amount;
}

static QofObject grand_object_def = {
  interface_version:     QOF_OBJECT_VERSION,
  e_type:                GRAND_MODULE_NAME,
  type_label:            GRAND_MODULE_DESC,
  create:                (gpointer)grand_create,
  book_begin:            NULL,
  book_end:              NULL,
  is_dirty:              NULL,
  mark_clean:            NULL,
  foreach:               qof_collection_foreach,
  printable:             NULL,
  version_cmp:           (int (*)(gpointer,gpointer)) qof_instance_version_cmp,
};

gboolean mygrandRegister (void)
{
  static QofParam params[] = {
    { OBJ_NAME,     QOF_TYPE_STRING,  (QofAccessFunc)grand_getName, 
	(QofSetterFunc)grand_setName },
    { OBJ_AMOUNT,   QOF_TYPE_NUMERIC, (QofAccessFunc)grand_getAmount,
 	(QofSetterFunc)grand_setAmount },
/*   { OBJ_GUID,     QOF_TYPE_GUID,    (QofAccessFunc)grand_getGUID,	
	(QofSetterFunc)grand_setGUID },*/
    { OBJ_DATE,     QOF_TYPE_DATE,    (QofAccessFunc)grand_getDate,	
	(QofSetterFunc)grand_setDate },
    { OBJ_DISCOUNT, QOF_TYPE_DOUBLE,  (QofAccessFunc)grand_getDiscount, 
	(QofSetterFunc)grand_setDiscount },
    { OBJ_ACTIVE,   QOF_TYPE_BOOLEAN, (QofAccessFunc)grand_getActive,   
	(QofSetterFunc)grand_setActive },
    { OBJ_VERSION,  QOF_TYPE_INT32,   (QofAccessFunc)grand_getVersion,  
	(QofSetterFunc)grand_setVersion },
    { OBJ_MINOR,    QOF_TYPE_INT64,	  (QofAccessFunc)grand_getMinor,	
	(QofSetterFunc)grand_setMinor },
    { OBJ_FLAG,     QOF_TYPE_CHAR,    (QofAccessFunc)grand_getFlag,
	(QofSetterFunc)grand_setFlag },
    { OBJ_RELATIVE,	PARENT_MODULE_NAME, (QofAccessFunc)grand_getChild,
	(QofSetterFunc)grand_setChild },
    { QOF_PARAM_BOOK, QOF_ID_BOOK,	(QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID,	(QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  qof_class_register (GRAND_MODULE_NAME, NULL, params);

  return qof_object_register (&grand_object_def);
}

static QofObject parent_object_def = {
  interface_version:     QOF_OBJECT_VERSION,
  e_type:                PARENT_MODULE_NAME,
  type_label:            PARENT_MODULE_DESC,
  create:                (gpointer)parent_create,
  book_begin:            NULL,
  book_end:              NULL,
  is_dirty:              NULL,
  mark_clean:            NULL,
  foreach:               qof_collection_foreach,
  printable:             NULL,
  version_cmp:           (int (*)(gpointer,gpointer)) qof_instance_version_cmp,
};

gboolean myparentRegister (void)
{
  static QofParam params[] = {
    { OBJ_NAME,     QOF_TYPE_STRING,  (QofAccessFunc)parent_getName, 
	(QofSetterFunc)parent_setName },
    { OBJ_AMOUNT,   QOF_TYPE_NUMERIC, (QofAccessFunc)parent_getAmount,
	(QofSetterFunc)parent_setAmount },
/*   { OBJ_GUID,     QOF_TYPE_GUID,    (QofAccessFunc)parent_getGUID,	
	(QofSetterFunc)parent_setGUID },*/
    { OBJ_DATE,     QOF_TYPE_DATE,    (QofAccessFunc)parent_getDate,	
	(QofSetterFunc)parent_setDate },
    { OBJ_DISCOUNT, QOF_TYPE_DOUBLE,  (QofAccessFunc)parent_getDiscount, 
	(QofSetterFunc)parent_setDiscount },
    { OBJ_ACTIVE,   QOF_TYPE_BOOLEAN, (QofAccessFunc)parent_getActive,   
	(QofSetterFunc)parent_setActive },
    { OBJ_VERSION,  QOF_TYPE_INT32,   (QofAccessFunc)parent_getVersion,  
	(QofSetterFunc)parent_setVersion },
    { OBJ_MINOR,    QOF_TYPE_INT64,	  (QofAccessFunc)parent_getMinor,	
	(QofSetterFunc)parent_setMinor },
    { OBJ_FLAG,     QOF_TYPE_CHAR,    (QofAccessFunc)parent_getFlag,
	(QofSetterFunc)parent_setFlag },
    { OBJ_RELATIVE,	CHILD_MODULE_NAME, (QofAccessFunc)parent_getChild,
	(QofSetterFunc)parent_setChild },
    { QOF_PARAM_BOOK, QOF_ID_BOOK,	(QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID,	(QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  qof_class_register (PARENT_MODULE_NAME, NULL, params);

  return qof_object_register (&parent_object_def);
}

static QofObject child_object_def = {
  interface_version:     QOF_OBJECT_VERSION,
  e_type:                CHILD_MODULE_NAME,
  type_label:            CHILD_MODULE_DESC,
  create:                (gpointer)child_create,
  book_begin:            NULL,
  book_end:              NULL,
  is_dirty:              NULL,
  mark_clean:            NULL,
  foreach:               qof_collection_foreach,
  printable:             NULL,
  version_cmp:           (int (*)(gpointer,gpointer)) qof_instance_version_cmp,
};

gboolean mychildRegister (void)
{
  static QofParam params[] = {
    { OBJ_NAME,     QOF_TYPE_STRING,  (QofAccessFunc)child_getName, 
	(QofSetterFunc)child_setName },
    { OBJ_AMOUNT,   QOF_TYPE_NUMERIC, (QofAccessFunc)child_getAmount,
	(QofSetterFunc)child_setAmount },
/*   { OBJ_GUID,     QOF_TYPE_GUID,    (QofAccessFunc)child_getGUID,	
	(QofSetterFunc)child_setGUID },*/
    { OBJ_DATE,     QOF_TYPE_DATE,    (QofAccessFunc)child_getDate,	
	(QofSetterFunc)child_setDate },
    { OBJ_DISCOUNT, QOF_TYPE_DOUBLE,  (QofAccessFunc)child_getDiscount, 
	(QofSetterFunc)child_setDiscount },
    { OBJ_ACTIVE,   QOF_TYPE_BOOLEAN, (QofAccessFunc)child_getActive,   
	(QofSetterFunc)child_setActive },
    { OBJ_VERSION,  QOF_TYPE_INT32,   (QofAccessFunc)child_getVersion,  
	(QofSetterFunc)child_setVersion },
    { OBJ_MINOR,    QOF_TYPE_INT64,	  (QofAccessFunc)child_getMinor,	
	(QofSetterFunc)child_setMinor },
    { OBJ_FLAG,     QOF_TYPE_CHAR,    (QofAccessFunc)child_getFlag,
	(QofSetterFunc)child_setFlag },
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
	QofBook *start;
	mygrand *grand1;
	myparent *parent1;
	mychild *child1;

	start = qof_session_get_book(original);
	grand1 = (mygrand*)qof_object_new_instance(GRAND_MODULE_NAME, start);
	do_test ((NULL != &grand1->inst), "#2 instance init");
	switch (counter)
	{
		case 0 : { /* NULL tree */ 
			do_test((grand1 != NULL), "empty tree check");
			break;
		}
		case 1 : { /* one parent, no child */ 
			parent1 = (myparent*)qof_object_new_instance(PARENT_MODULE_NAME, start);
			grand_setChild(grand1, parent1);
			do_test((parent1 != NULL), "single parent check");
			do_test((grand_getChild(grand1) == parent1), "set child in grandparent");
			break;
		}
		case 2 : { /* one parent, one child */ 
			parent1 = (myparent*)qof_object_new_instance(PARENT_MODULE_NAME, start);
			grand_setChild(grand1, parent1);
			child1 = (mychild*)qof_object_new_instance(CHILD_MODULE_NAME, start);
			parent1 = grand_getChild(grand1);
			parent_setChild(parent1, child1);
			do_test((child1 != NULL), "one parent with one related child");
			do_test((child1 == parent_getChild(parent1)), "child of single parent");
			break;
		}
		case 3 : { /* same grand, new parent, same child */
			child1 = (mychild*)qof_object_new_instance(CHILD_MODULE_NAME, start);
			parent1 = (myparent*)qof_object_new_instance(PARENT_MODULE_NAME, start);
			grand_setChild(grand1, parent1);
			parent_setChild(parent1, child1);
			do_test((parent1 == grand_getChild(grand1)), "same grandparent, new parent");
			do_test((child1 == parent_getChild(parent1)), "new parent, same child");
			break; 
		}
		case 4 : { /* new grand, unrelated parent, child unrelated to grand */
			grand1 = (mygrand*)qof_object_new_instance(GRAND_MODULE_NAME, start);
			parent1 = (myparent*)qof_object_new_instance(PARENT_MODULE_NAME, start);
			child1 = (mychild*)qof_object_new_instance(CHILD_MODULE_NAME, start);
			parent_setChild(parent1, child1);
			do_test((NULL == grand_getChild(grand1)), "new grand, unrelated parent");
			do_test((child1 == parent_getChild(parent1)), "child unrelated to grand");
			break;
		}
	}
}

/** \brief Read QofEntityReference data for this entity and set values.

@param partial_book The partial book containing the referenceList
@param ent The parent entity to hold the converted reference.

The referenceList is a GList of QofEntityReference structures that contain
the GUID of each end of a reference. e.g. where one entity refers to another.

The referenceList is used in partial books to store relationships between
entities when the entities themselves might not exist in the partial book.

If the book is not marked as a partial book, an assertion error is generated.

This routine tries to lookup the given entity in the referenceList for the
book and then tries to lookup the reference - to find the child entity that
was originally linked to this parent. The child entity is then set in the
parent so that it can be located as normal.

If the child entity does not exist in this partial book, the parent entity
is not updated. The referenceList is unchanged (in case the child is added
later).

*/
void
qof_entity_set_reference_data(QofBook *partial_book, QofEntity *ent);

void
qof_entity_set_reference_data(QofBook *partial_book, QofEntity *ent)
{
	void (*reference_setter) (QofEntity*, QofEntity*);
	QofEntityReference *ref;
	GList *book_ref_list;
	QofCollection *coll;
	QofEntity *reference;
	gboolean partial;

	g_return_if_fail(partial_book || ent);
	partial =
	  (gboolean)GPOINTER_TO_INT(qof_book_get_data(partial_book, PARTIAL_QOFBOOK));
	g_return_if_fail(partial);
	book_ref_list = qof_book_get_data(partial_book, ENTITYREFERENCE);
	while(book_ref_list)
	{
		ref = (QofEntityReference*)book_ref_list->data;
		if(0 != guid_compare(ref->ref_guid, qof_entity_get_guid(ent)))
		{ 
			book_ref_list = g_list_next(book_ref_list);
			continue; 
		}
		coll = qof_book_get_collection(partial_book, ref->type);
		reference = qof_collection_lookup_entity(coll, ref->ref_guid);
		do_test((reference == NULL), "reference is null");
		reference_setter = (void(*)(QofEntity*, QofEntity*))ref->param->param_setfcn;
		if(reference_setter != NULL)
		{
			qof_begin_edit((QofInstance*)ent);
			qof_begin_edit((QofInstance*)reference);
			reference_setter(ent, reference);
			qof_commit_edit((QofInstance*)ent);
			qof_commit_edit((QofInstance*)reference);
		}
		book_ref_list = g_list_next(book_ref_list);
	}
}

struct tally
{
	guint nulls, total;
	QofBook *book;
};

static void
check_cb (QofEntity *ent, gpointer data)
{
	QofEntity *parent, *child;
	struct tally *c;
	const QofParam *param;
	mygrand  *testg;
	myparent *testp;
	mychild  *testc;

	c = (struct tally*)data;
	/* find the child entity in the copied book */
	testg = (mygrand*)ent;
	do_test((testg != NULL), "grandparent not found");
	c->total++;
	if(c->book) { qof_entity_set_reference_data(c->book, ent); }
	testp = grand_getChild(testg);
	param = qof_class_get_parameter(GRAND_MODULE_NAME, OBJ_RELATIVE);
	parent = param->param_getfcn(ent, param);
	if(!parent || !testp) { c->nulls++; return; }
	do_test((0 == safe_strcmp(parent_getName(testp), 
		parent_getName((myparent*)parent))), "parent copy test");
	param = qof_class_get_parameter(PARENT_MODULE_NAME, OBJ_RELATIVE);
	testc = parent_getChild(testp);
	child = param->param_getfcn(parent, param);
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
	guint d, e;

	c.nulls = 0;
	c.total = 0;
	c.book = NULL;
	book = qof_session_get_book(original);
	grand_coll = qof_book_get_collection(book, GRAND_MODULE_NAME);
	copy = qof_session_new();
	if(debug) { qof_session_begin(copy, QOF_STDOUT, TRUE, FALSE); }
	qof_entity_copy_coll_r(copy, grand_coll);
	/* test the original */
	qof_object_foreach(GRAND_MODULE_NAME, book, check_cb, &c);
	book = qof_session_get_book(copy);
	/* test the copy */
	d = c.nulls;
	e = c.total;
	c.nulls = 0;
	c.total = 0;
	c.book = book;
	qof_object_foreach(GRAND_MODULE_NAME, book, check_cb, &c);
//	do_test((d == c.nulls), "Null parents do not match");
	do_test((e == c.total), "Total parents do not match");
	if(debug) { qof_session_save(copy, NULL); }
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
	original = qof_session_new();
	if(debug) { qof_session_begin(original, QOF_STDOUT, TRUE, FALSE); }
	for(counter = 0; counter < 25; counter++)
	{
		create_data(original, (counter % 5));
		test_recursion(original, (counter % 5));
	}
	print_test_results();
	qof_close();
	return EXIT_SUCCESS;
}
