/***************************************************************************
 *            qofchoice.c
 *
 *  Thu Jul  7 12:24:30 2005
 *  Copyright  2005  Neil Williams
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
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <algorithm>
#include <numeric>
#include <vector>
#include <string>
#include <unordered_map>
#include <config.h>

#include "qof.h"
#include "qofchoice.h"

using OptionVec = std::vector<char*>;
using ParamMap = std::unordered_map<std::string, OptionVec>;
using ChoiceMap = std::unordered_map<std::string, ParamMap>;
static QofLogModule log_module = QOF_MOD_CHOICE;
static ChoiceMap qof_choice_table;

gboolean qof_object_is_choice(QofIdTypeConst type)
{
    g_return_val_if_fail(type != NULL, false);

    auto choice_it = qof_choice_table.find (type);
    if (choice_it != qof_choice_table.end())
    {
        return true;
    }
    DEBUG (" QOF_TYPE_CHOICE setup failed for %s\n", type);
    return false;
}

gboolean
qof_choice_create(char* type)
{
    g_return_val_if_fail(type != NULL, false);
    qof_choice_table.insert_or_assign (type, ParamMap{});
    return TRUE;
}

gboolean qof_choice_add_class(const char* select,
                              char* option,
                              char* param_name)
{
    g_return_val_if_fail(select != NULL, false);
    g_return_val_if_fail(qof_object_is_choice(select), false);

    auto choices_it = qof_choice_table.find (select);
    g_return_val_if_fail(choices_it != qof_choice_table.end(), false);

    auto& param_map = choices_it->second;
    auto [param_it, result] = param_map.insert ({param_name, OptionVec{}});

    auto& optionvec = param_it->second;
    optionvec.push_back (option);

    return TRUE;
}

GList* qof_object_get_choices(QofIdType type, QofParam *param)
{
    g_return_val_if_fail(type != NULL, NULL);

    auto choices_it = qof_choice_table.find (type);
    g_return_val_if_fail(choices_it != qof_choice_table.end(), nullptr);

    auto& param_map = choices_it->second;
    auto& name = param->param_name;

    auto param_it = param_map.find (name);
    g_return_val_if_fail(param_it != param_map.end(), nullptr);

    auto& optionvec = param_it->second;
    return std::accumulate (optionvec.rbegin(), optionvec.rend(), (GList*) nullptr,
                            g_list_prepend);
}

gboolean qof_choice_check(const char* choice_obj,
                          const char *param_name,
                          const char* choice )
{
    g_return_val_if_fail(qof_object_is_choice(choice_obj), FALSE);

    auto choices_it = qof_choice_table.find (choice_obj);
    g_return_val_if_fail(choices_it != qof_choice_table.end(), false);

    auto& param_map = choices_it->second;

    auto param_it = param_map.find (param_name);
    g_return_val_if_fail(param_it != param_map.end(), false);

    auto& optionvec = param_it->second;
    auto option_it = std::find (optionvec.begin(), optionvec.end(), choice);

    return (option_it != optionvec.end());
}
