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

#include <config.h>
#include <string>
#include <algorithm>
#include <unordered_set>
#include <unordered_map>

#include "qof.h"
#include "qofclass-p.h"

static QofLogModule log_module = QOF_MOD_CLASS;

using StringQofParamMap = std::unordered_map<std::string, const QofParam*>;
static std::unordered_map<std::string,StringQofParamMap> classTable;
static std::unordered_map<std::string,QofSortFunc> sortTable;

/* *******************************************************************/
/* PRIVATE FUNCTIONS */

QofSortFunc
qof_class_get_default_sort (QofIdTypeConst obj_name)
{
    g_return_val_if_fail (obj_name, nullptr);
    auto it = sortTable.find (obj_name);
    g_return_val_if_fail (it != sortTable.end(), nullptr);
    return it->second;
}

/* *******************************************************************/
/* PUBLISHED API FUNCTIONS */

void
qof_class_register (QofIdTypeConst obj_name, QofSortFunc default_sort_function,
                    const QofParam *params)
{
    g_return_if_fail (obj_name);

    if (default_sort_function)
        sortTable[obj_name] = default_sort_function;

    auto& param_map = classTable[obj_name];

    /* At least right now, we allow dummy, parameterless objects,
     * for testing purposes.  Although I suppose that should be
     * an error..  */
    /* Now insert all the parameters */
    if (params)
        for (auto i = 0; params[i].param_name; i++)
            param_map[params[i].param_name] = &params[i];
}

const QofParam *
qof_class_get_parameter (QofIdTypeConst obj_name, const char *parameter)
{
    g_return_val_if_fail (obj_name && parameter, nullptr);

    auto class_it = classTable.find (obj_name);
    if (class_it == classTable.end())
    {
        PWARN ("no object of type %s", obj_name);
        return NULL;
    }

    auto& param_map = class_it->second;
    auto p_it = param_map.find (parameter);

    return p_it == param_map.end() ? nullptr : p_it->second;
}

QofAccessFunc
qof_class_get_parameter_getter (QofIdTypeConst obj_name, const char *parameter)
{
    g_return_val_if_fail (obj_name && parameter, nullptr);
    auto prm = qof_class_get_parameter (obj_name, parameter);
    return prm ? prm->param_getfcn : nullptr;
}

QofSetterFunc
qof_class_get_parameter_setter (QofIdTypeConst obj_name, const char *parameter)
{
    g_return_val_if_fail (obj_name && parameter, nullptr);
    auto prm = qof_class_get_parameter (obj_name, parameter);
    return prm ? prm->param_setfcn : nullptr;
}

/* ============================= END OF FILE ======================== */
