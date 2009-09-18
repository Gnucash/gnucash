/********************************************************************\
 * qofclass.c -- provide QOF parameterized data objects             *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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

#include "config.h"

#include <glib.h>

#include "qof.h"
#include "qofclass-p.h"

static QofLogModule log_module = QOF_MOD_CLASS;

static GHashTable *classTable = NULL;
static GHashTable *sortTable = NULL;
static gboolean initialized = FALSE;

static gboolean clear_table (gpointer key, gpointer value, gpointer user_data)
{
    g_hash_table_destroy (value);
    return TRUE;
}

/* *******************************************************************/
/* PRIVATE FUNCTIONS */

static gboolean check_init (void)
{
    if (initialized) return TRUE;

    PERR("You must call qof_class_init() before using qof_class.");
    return FALSE;
}

void
qof_class_init(void)
{
    if (initialized) return;
    initialized = TRUE;

    classTable = g_hash_table_new (g_str_hash, g_str_equal);
    sortTable = g_hash_table_new (g_str_hash, g_str_equal);
}

void
qof_class_shutdown (void)
{
    if (!initialized) return;
    initialized = FALSE;

    g_hash_table_foreach_remove (classTable, clear_table, NULL);
    g_hash_table_destroy (classTable);
    g_hash_table_destroy (sortTable);
}

QofSortFunc
qof_class_get_default_sort (QofIdTypeConst obj_name)
{
    if (!obj_name) return NULL;
    return g_hash_table_lookup (sortTable, obj_name);
}

/* *******************************************************************/
/* PUBLISHED API FUNCTIONS */

void
qof_class_register (QofIdTypeConst obj_name,
                    QofSortFunc default_sort_function,
                    const QofParam *params)
{
    GHashTable *ht;
    int i;

    if (!obj_name) return;
    if (!check_init()) return;

    if (default_sort_function)
    {
        g_hash_table_insert (sortTable, (char *)obj_name, default_sort_function);
    }

    ht = g_hash_table_lookup (classTable, obj_name);

    /* If it doesn't already exist, create a new table for this object */
    if (!ht)
    {
        ht = g_hash_table_new (g_str_hash, g_str_equal);
        g_hash_table_insert (classTable, (char *)obj_name, ht);
    }

    /* At least right now, we allow dummy, parameterless objects,
     * for testing purposes.  Although I suppose that should be
     * an error..  */
    /* Now insert all the parameters */
    if (params)
    {
        for (i = 0; params[i].param_name; i++)
            g_hash_table_insert (ht,
                                 (char *)params[i].param_name,
                                 (gpointer)&(params[i]));
    }
}

gboolean
qof_class_is_registered (QofIdTypeConst obj_name)
{
    if (!obj_name) return FALSE;
    if (!check_init()) return FALSE;

    if (g_hash_table_lookup (classTable, obj_name)) return TRUE;

    return FALSE;
}

const QofParam *
qof_class_get_parameter (QofIdTypeConst obj_name,
                         const char *parameter)
{
    GHashTable *ht;

    g_return_val_if_fail (obj_name, NULL);
    g_return_val_if_fail (parameter, NULL);
    if (!check_init()) return NULL;

    ht = g_hash_table_lookup (classTable, obj_name);
    if (!ht)
    {
        PWARN ("no object of type %s", obj_name);
        return NULL;
    }

    return (g_hash_table_lookup (ht, parameter));
}

QofAccessFunc
qof_class_get_parameter_getter (QofIdTypeConst obj_name,
                                const char *parameter)
{
    const QofParam *prm;

    g_return_val_if_fail (obj_name, NULL);
    g_return_val_if_fail (parameter, NULL);

    prm = qof_class_get_parameter (obj_name, parameter);
    if (prm)
        return prm->param_getfcn;

    return NULL;
}

QofSetterFunc
qof_class_get_parameter_setter (QofIdTypeConst obj_name,
                                const char *parameter)
{
    const QofParam *prm;

    g_return_val_if_fail (obj_name, NULL);
    g_return_val_if_fail (parameter, NULL);

    prm = qof_class_get_parameter (obj_name, parameter);
    if (prm)
        return prm->param_setfcn;

    return NULL;
}

QofType
qof_class_get_parameter_type (QofIdTypeConst obj_name,
                              const char *param_name)
{
    const QofParam *prm;

    if (!obj_name || !param_name) return NULL;

    prm = qof_class_get_parameter (obj_name, param_name);
    if (!prm) return NULL;

    return (prm->param_type);
}

/* ================================================================ */

struct class_iterate
{
    QofClassForeachCB   fcn;
    gpointer            data;
};

static void
class_foreach_cb (gpointer key, gpointer item, gpointer arg)
{
    struct class_iterate *iter = arg;
    QofIdTypeConst id = key;

    iter->fcn (id, iter->data);
}

void
qof_class_foreach (QofClassForeachCB cb, gpointer user_data)
{
    struct class_iterate iter;

    if (!cb) return;
    if (!classTable) return;

    iter.fcn = cb;
    iter.data = user_data;

    g_hash_table_foreach (classTable, class_foreach_cb, &iter);
}

/* ================================================================ */

struct parm_iterate
{
    QofParamForeachCB   fcn;
    gpointer            data;
};

static void
param_foreach_cb (gpointer key, gpointer item, gpointer arg)
{
    struct parm_iterate *iter = arg;
    QofParam *parm = item;

    iter->fcn (parm, iter->data);
}

void
qof_class_param_foreach (QofIdTypeConst obj_name,
                         QofParamForeachCB cb, gpointer user_data)
{
    struct parm_iterate iter;
    GHashTable *param_ht;

    if (!obj_name || !cb) return;
    if (!classTable) return;
    param_ht = g_hash_table_lookup (classTable, obj_name);
    if (!param_ht) return;

    iter.fcn = cb;
    iter.data = user_data;

    g_hash_table_foreach (param_ht, param_foreach_cb, &iter);
}

struct param_ref_list
{
    GList *list;
};

static void
find_reference_param_cb(QofParam *param, gpointer user_data)
{
    struct param_ref_list *b;

    b = (struct param_ref_list*)user_data;
    if ((param->param_getfcn == NULL) || (param->param_setfcn == NULL))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_STRING))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_NUMERIC))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_DATE))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_CHAR))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_DEBCRED))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_GUID))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_INT32))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_INT64))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_DOUBLE))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_KVP))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_TYPE_BOOLEAN))
    {
        return;
    }
    if (0 == safe_strcmp(param->param_type, QOF_ID_BOOK))
    {
        return;
    }
    b->list = g_list_append(b->list, param);
}

GList*
qof_class_get_referenceList(QofIdTypeConst type)
{
    GList *ref_list;
    struct param_ref_list b;

    ref_list = NULL;
    b.list = NULL;
    qof_class_param_foreach(type, find_reference_param_cb, &b);
    ref_list = g_list_copy(b.list);
    return ref_list;
}


/* ============================= END OF FILE ======================== */
