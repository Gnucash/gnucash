/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <gnome.h>

#include "QueryNew.h"

#include "search-core-type.h"
#include "search-string.h"
#include "search-date.h"
#include "search-double.h"
#include "search-int64.h"
#include "search-numeric.h"
#include "search-boolean.h"
#include "search-account.h"

static void grab_focus (GNCSearchCoreType *fe);
static void editable_enters (GNCSearchCoreType *fe, GnomeDialog *dialog);
static gboolean validate (GNCSearchCoreType *fe);

static void gnc_search_core_type_class_init	(GNCSearchCoreTypeClass *class);
static void gnc_search_core_type_init	(GNCSearchCoreType *gspaper);
static void gnc_search_core_type_finalise	(GtkObject *obj);

#define _PRIVATE(x) (((GNCSearchCoreType *)(x))->priv)
struct _GNCSearchCoreTypePrivate {
};

static GtkObjectClass *parent_class;

enum {
  LAST_SIGNAL
};

static GHashTable *typeTable = NULL;

static guint signals[LAST_SIGNAL] = { 0 };

guint
gnc_search_core_type_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GtkTypeInfo type_info = {
      "GNCSearchCoreType",
      sizeof(GNCSearchCoreType),
      sizeof(GNCSearchCoreTypeClass),
      (GtkClassInitFunc)gnc_search_core_type_class_init,
      (GtkObjectInitFunc)gnc_search_core_type_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
		
    type = gtk_type_unique(gtk_object_get_type (), &type_info);
  }
	
  return type;
}

static void
gnc_search_core_type_class_init (GNCSearchCoreTypeClass *class)
{
  GtkObjectClass *object_class;
	
  object_class = (GtkObjectClass *)class;
  parent_class = gtk_type_class (gtk_object_get_type ());

  object_class->finalize = gnc_search_core_type_finalise;

  /* override methods */
  class->validate = validate;
  class->grab_focus = grab_focus;
  class->editable_enters = editable_enters;

  /* signals */

  gtk_object_class_add_signals (object_class, signals, LAST_SIGNAL);
}

static void
gnc_search_core_type_init (GNCSearchCoreType *o)
{
  o->priv = g_malloc0 (sizeof (*o->priv));
}

static void
gnc_search_core_type_finalise (GtkObject *obj)
{
  GNCSearchCoreType *o = (GNCSearchCoreType *)obj;
  g_free(o->priv);
  ((GtkObjectClass *)(parent_class))->finalize(obj);
}

/**
 * gnc_search_core_type_new:
 *
 * Create a new GNCSearchCoreType object.
 * 
 * Return value: A new #GNCSearchCoreType object.
 **/
GNCSearchCoreType *
gnc_search_core_type_new (void)
{
  GNCSearchCoreType *o = (GNCSearchCoreType *)gtk_type_new(gnc_search_core_type_get_type ());
  return o;
}

void
gnc_search_core_type_editable_enters (GNCSearchCoreType *fe,
				      GnomeDialog *dialog)
{
  return ((GNCSearchCoreTypeClass *)((GtkObject *)fe)->klass)->editable_enters (fe, dialog);
}

void
gnc_search_core_type_grab_focus (GNCSearchCoreType *fe)
{
  return ((GNCSearchCoreTypeClass *)((GtkObject *)fe)->klass)->grab_focus (fe);
}

gboolean
gnc_search_core_type_validate (GNCSearchCoreType *fe)
{
  return ((GNCSearchCoreTypeClass *)((GtkObject *)fe)->klass)->validate (fe);
}

/**
 * gnc_search_core_type_clone:
 * @fe: search core_type
 * 
 * Clones the GNCSearchCoreType @fe.
 * 
 * Return value: 
 **/
GNCSearchCoreType *
gnc_search_core_type_clone (GNCSearchCoreType *fe)
{
  return ((GNCSearchCoreTypeClass *)((GtkObject *)fe)->klass)->clone(fe);
}

/**
 * gnc_search_core_type_get_widget:
 * @fe: search core_type
 * @node: xml node
 * 
 * Create a widget to represent this core_type.
 * 
 * Return value: 
 **/
GtkWidget *
gnc_search_core_type_get_widget (GNCSearchCoreType *fe)
{
  return ((GNCSearchCoreTypeClass *)((GtkObject *)fe)->klass)->get_widget(fe);
}

/**
 * gnc_search_core_type_get_predicate:
 * @fe: search core_type
 * 
 * Create a Predicate Data that matches this core_type
 * 
 * Return value: 
 **/
QueryPredData_t
gnc_search_core_type_get_predicate (GNCSearchCoreType *fe)
{
  return ((GNCSearchCoreTypeClass *)((GtkObject *)fe)->klass)->get_predicate(fe);
}

/**
 * gnc_search_core_type_new_type_name:
 * @type: search core_type type
 * 
 * Create a new search core_type based on its type name.
 * 
 * Return value: 
 **/
GNCSearchCoreType *
gnc_search_core_type_new_type_name (const char *type)
{
  GNCSearchCoreNew fcn;

  g_return_val_if_fail (typeTable, NULL);

  if (type == NULL)
    return NULL;

  fcn = g_hash_table_lookup (typeTable, type);
  if (fcn) {
    return ((fcn)());
  } else {
    g_warning("Unknown search type '%s'", type);
    return NULL;
  }
}

/* default implementations */
static gboolean
validate (GNCSearchCoreType *fe)
{
  return TRUE;
}

static void
grab_focus (GNCSearchCoreType *fe)
{
  return;
}

static void
editable_enters (GNCSearchCoreType *fe, GnomeDialog *dialog)
{
  return;
}

void
gnc_search_core_register_type (const char *type_name, GNCSearchCoreNew fcn)
{
  g_return_if_fail (type_name || *type_name || fcn);
  g_return_if_fail (typeTable);
  g_hash_table_insert (typeTable, (char *) type_name, (gpointer) fcn);
}

static void
init_table (void)
{
  gnc_search_core_register_type (QUERYCORE_STRING,
				 (GNCSearchCoreNew) gnc_search_string_new);
  gnc_search_core_register_type (QUERYCORE_DATE,
				 (GNCSearchCoreNew) gnc_search_date_new);
  gnc_search_core_register_type (QUERYCORE_INT64,
				 (GNCSearchCoreNew) gnc_search_int64_new);
  gnc_search_core_register_type (QUERYCORE_DOUBLE,
				 (GNCSearchCoreNew) gnc_search_double_new);
  gnc_search_core_register_type (QUERYCORE_NUMERIC,
				 (GNCSearchCoreNew) gnc_search_numeric_new);
  gnc_search_core_register_type (QUERYCORE_DEBCRED,
				 (GNCSearchCoreNew)
				 gnc_search_numeric_debcred_new);
  gnc_search_core_register_type (QUERYCORE_BOOLEAN,
				 (GNCSearchCoreNew) gnc_search_boolean_new);
  gnc_search_core_register_type (GNC_ID_ACCOUNT,
				 (GNCSearchCoreNew) gnc_search_account_new);
  gnc_search_core_register_type ("account-match-all",
				 (GNCSearchCoreNew) 
				 gnc_search_account_matchall_new);
}

void
gnc_search_core_initialize (void)
{
  g_return_if_fail (typeTable == NULL);
  typeTable = g_hash_table_new (g_str_hash, g_str_equal);
  init_table ();
}

void
gnc_search_core_finalize (void)
{
  g_return_if_fail (typeTable);
  g_hash_table_destroy (typeTable);
  typeTable = NULL;
}
