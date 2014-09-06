/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <gtk/gtk.h>

#include "qof.h"
#include "Account.h"		/* for ACCOUNT_MATCH_ALL_TYPE */
#include "Transaction.h"	/* for RECONCILED_MATCH_TYPE */

#include "search-core-type.h"
#include "search-string.h"
#include "search-reconciled.h"
#include "search-date.h"
#include "search-double.h"
#include "search-int64.h"
#include "search-numeric.h"
#include "search-boolean.h"
#include "search-account.h"

static void grab_focus (GNCSearchCoreType *fe);
static void editable_enters (GNCSearchCoreType *fe);
static gboolean validate (GNCSearchCoreType *fe);

static void gnc_search_core_type_class_init	(GNCSearchCoreTypeClass *klass);
static void gnc_search_core_type_init	(GNCSearchCoreType *gspaper);
static void gnc_search_core_type_finalize	(GObject *obj);

typedef struct _GNCSearchCoreTypePrivate GNCSearchCoreTypePrivate;

struct _GNCSearchCoreTypePrivate
{
    gpointer dummy;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_CORE_TYPE, GNCSearchCoreTypePrivate))

static GtkObjectClass *parent_class;

static GHashTable *typeTable = NULL;

GType
gnc_search_core_type_get_type (void)
{
    static GType type = 0;

    if (type == 0)
    {
        GTypeInfo type_info =
        {
            sizeof (GNCSearchCoreTypeClass),
            NULL,
            NULL,
            (GClassInitFunc)gnc_search_core_type_class_init,
            NULL,
            NULL,
            sizeof (GNCSearchCoreType),
            0,
            (GInstanceInitFunc)gnc_search_core_type_init
        };

        type = g_type_register_static (G_TYPE_OBJECT, "GNCSearchCoreType", &type_info, 0);
    }

    return type;
}

static void
gnc_search_core_type_class_init (GNCSearchCoreTypeClass *klass)
{
    GObjectClass *object_class;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_core_type_finalize;

    /* override methods */
    klass->validate = validate;
    klass->grab_focus = grab_focus;
    klass->editable_enters = editable_enters;

    g_type_class_add_private(klass, sizeof(GNCSearchCoreTypePrivate));
}

static void
gnc_search_core_type_init (GNCSearchCoreType *o)
{
}

static void
gnc_search_core_type_finalize (GObject *obj)
{
    GNCSearchCoreType *o = (GNCSearchCoreType *)obj;
    g_assert (GNC_IS_SEARCH_CORE_TYPE (o));

    G_OBJECT_CLASS (parent_class)->finalize(obj);
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
    GNCSearchCoreType *o;

    o = g_object_new (GNC_TYPE_SEARCH_CORE_TYPE, NULL);

    return o;
}

void
gnc_search_core_type_editable_enters (GNCSearchCoreType *fe)
{
    GNC_SEARCH_CORE_TYPE_GET_CLASS (fe)->editable_enters (fe);
}

void
gnc_search_core_type_grab_focus (GNCSearchCoreType *fe)
{
    GNC_SEARCH_CORE_TYPE_GET_CLASS (fe)->grab_focus (fe);
}

gboolean
gnc_search_core_type_validate (GNCSearchCoreType *fe)
{
    return GNC_SEARCH_CORE_TYPE_GET_CLASS (fe)->validate (fe);
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
    return GNC_SEARCH_CORE_TYPE_GET_CLASS (fe)->clone(fe);
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
    return GNC_SEARCH_CORE_TYPE_GET_CLASS (fe)->get_widget(fe);
}

/**
 * gnc_search_core_type_get_predicate:
 * @fe: search core_type
 *
 * Create a Predicate Data that matches this core_type
 *
 * Return value:
 **/
QofQueryPredData*
gnc_search_core_type_get_predicate (GNCSearchCoreType *fe)
{
    return GNC_SEARCH_CORE_TYPE_GET_CLASS (fe)->get_predicate(fe);
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

    g_return_val_if_fail (typeTable != NULL, NULL);

    if (type == NULL)
        return NULL;

    fcn = g_hash_table_lookup (typeTable, type);
    if (fcn)
    {
        return ((fcn)());
    }
    else
    {
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
editable_enters (GNCSearchCoreType *fe)
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
    gnc_search_core_register_type (QOF_TYPE_STRING,
                                   (GNCSearchCoreNew) gnc_search_string_new);
    gnc_search_core_register_type (QOF_TYPE_DATE,
                                   (GNCSearchCoreNew) gnc_search_date_new);
    gnc_search_core_register_type (QOF_TYPE_INT64,
                                   (GNCSearchCoreNew) gnc_search_int64_new);
    gnc_search_core_register_type (QOF_TYPE_DOUBLE,
                                   (GNCSearchCoreNew) gnc_search_double_new);
    gnc_search_core_register_type (QOF_TYPE_NUMERIC,
                                   (GNCSearchCoreNew) gnc_search_numeric_new);
    gnc_search_core_register_type (QOF_TYPE_DEBCRED,
                                   (GNCSearchCoreNew)
                                   gnc_search_numeric_debcred_new);
    gnc_search_core_register_type (QOF_TYPE_BOOLEAN,
                                   (GNCSearchCoreNew) gnc_search_boolean_new);
    gnc_search_core_register_type (GNC_ID_ACCOUNT,
                                   (GNCSearchCoreNew) gnc_search_account_new);
    gnc_search_core_register_type (ACCOUNT_MATCH_ALL_TYPE,
                                   (GNCSearchCoreNew)
                                   gnc_search_account_matchall_new);
    gnc_search_core_register_type (RECONCILED_MATCH_TYPE,
                                   (GNCSearchCoreNew) gnc_search_reconciled_new);
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
    g_return_if_fail (typeTable != NULL);

    g_hash_table_destroy (typeTable);
    typeTable = NULL;
}
