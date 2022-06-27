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

#include <unordered_map>
#include <string>
#include <algorithm>

#include <config.h>

#include "qof.h"
#include "qofclass-p.h"

static QofLogModule log_module = QOF_MOD_CLASS;

using QofParamMap = std::unordered_map <std::string,const QofParam*>;
static std::unordered_map<std::string,QofParamMap> classTable;
static std::unordered_map<std::string,QofSortFunc> sortTable;

/* *******************************************************************/
/* PRIVATE FUNCTIONS */

QofSortFunc
qof_class_get_default_sort (QofIdTypeConst obj_name)
{
    if (!obj_name) return NULL;
    auto sort_iter = sortTable.find (obj_name);
    return sort_iter->second;
}

/* *******************************************************************/
/* PUBLISHED API FUNCTIONS */

void
qof_class_register (QofIdTypeConst obj_name,
                    QofSortFunc default_sort_function,
                    const QofParam *params)
{
    if (!obj_name) return;

    if (default_sort_function)
        sortTable.insert_or_assign (obj_name, default_sort_function);

    auto [class_iter, result] = classTable.insert ({obj_name, QofParamMap {}});

    auto& param_map = class_iter->second;

    /* At least right now, we allow dummy, parameterless objects,
     * for testing purposes.  Although I suppose that should be
     * an error..  */
    /* Now insert all the parameters */
    if (params)
    {
        for (int i = 0; params[i].param_name; i++)
            param_map.insert_or_assign (params[i].param_name, &params[i]);
    }
}

gboolean
qof_class_is_registered (QofIdTypeConst obj_name)
{
    if (!obj_name) return FALSE;

    auto class_iter = classTable.find (obj_name);

    return (class_iter != classTable.end());
}

const QofParam *
qof_class_get_parameter (QofIdTypeConst obj_name,
                         const char *parameter)
{
    g_return_val_if_fail (obj_name, NULL);
    g_return_val_if_fail (parameter, NULL);

    auto class_iter = classTable.find (obj_name);
    if (class_iter == classTable.end())
    {
        PWARN ("no object of type %s", obj_name);
        return NULL;
    }

    auto& param_map = class_iter->second;
    auto param_iter = param_map.find (parameter);

    return param_iter != param_map.end() ? param_iter->second : nullptr;
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

void
qof_class_foreach (QofClassForeachCB cb, gpointer user_data)
{
    if (!cb) return;

    std::for_each (classTable.begin(), classTable.end(),
                   [&cb, &user_data](const auto& it)
                   { cb (it.first.c_str(), user_data); });
}

/* ================================================================ */

void
qof_class_param_foreach (QofIdTypeConst obj_name,
                         QofParamForeachCB cb, gpointer user_data)
{
    if (!obj_name || !cb) return;

    auto class_iter = classTable.find (obj_name);
    if (class_iter == classTable.end())
        return;

    auto& param_map = class_iter->second;

    std::for_each (param_map.begin(), param_map.end(),
                   [&cb, &user_data](const auto& it)
                   { cb (const_cast<QofParam*>(it.second), user_data); });
}

static void
find_reference_param_cb(QofParam *param, gpointer user_data)
{
    if ((param->param_getfcn == NULL) || (param->param_setfcn == NULL))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_STRING))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_NUMERIC))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_DATE))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_CHAR))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_DEBCRED))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_GUID))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_INT32))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_INT64))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_DOUBLE))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_KVP))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_TYPE_BOOLEAN))
    {
        return;
    }
    if (0 == g_strcmp0(param->param_type, QOF_ID_BOOK))
    {
        return;
    }
    auto list = static_cast<GList*>(user_data);
    list = g_list_prepend (list, param);
}

GList*
qof_class_get_referenceList(QofIdTypeConst type)
{
    GList *list = nullptr;
    qof_class_param_foreach(type, find_reference_param_cb, &list);
    return g_list_reverse (list);
}


/* ============================= END OF FILE ======================== */
