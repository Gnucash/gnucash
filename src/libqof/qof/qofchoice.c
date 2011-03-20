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

#include "config.h"
#include <glib.h>
#include "qof.h"
#include "qofchoice.h"

static QofLogModule log_module = QOF_MOD_CHOICE;
static GHashTable *qof_choice_table = NULL;

/* To initialise, call qof_choice_add_class in
qof_object_register for the choice object. */
static gboolean qof_choice_is_initialized(void)
{
    if (!qof_choice_table)
    {
        qof_choice_table = g_hash_table_new(g_str_hash, g_str_equal);
    }
    if (!qof_choice_table)
    {
        return FALSE;
    }
    return TRUE;
}

gboolean qof_object_is_choice(QofIdTypeConst type)
{
    gpointer value, check;

    value = NULL;
    check = NULL;
    if (!qof_choice_is_initialized())
    {
        return FALSE;
    }
    g_return_val_if_fail(type != NULL, FALSE);
    value = g_hash_table_lookup(qof_choice_table, type);
    if ((GHashTable*)value)
    {
        return TRUE;
    }
    DEBUG (" QOF_TYPE_CHOICE setup failed for %s\n", type);
    return FALSE;
}

gboolean
qof_choice_create(char* type)
{
    GHashTable *param_table;

    g_return_val_if_fail(type != NULL, FALSE);
    g_return_val_if_fail(qof_choice_is_initialized() == TRUE, FALSE);
    param_table = g_hash_table_new(g_str_hash, g_str_equal);
    g_hash_table_insert(qof_choice_table, type, param_table);
    return TRUE;
}

gboolean qof_choice_add_class(const char* select,
                              char* option,
                              char* param_name)
{
    GHashTable *param_table;
    GList *option_list;

    option_list = NULL;
    param_table = NULL;
    g_return_val_if_fail(select != NULL, FALSE);
    g_return_val_if_fail(qof_object_is_choice(select), FALSE);
    param_table = (GHashTable*)g_hash_table_lookup(qof_choice_table, select);
    g_return_val_if_fail(param_table, FALSE);
    option_list = (GList*)g_hash_table_lookup(param_table, param_name);
    option_list = g_list_append(option_list, option);
    g_hash_table_insert(param_table, param_name, option_list);
    return TRUE;
}

GList* qof_object_get_choices(QofIdType type, QofParam *param)
{
    GList *choices;
    GHashTable *param_table;

    g_return_val_if_fail(type != NULL, NULL);
    g_return_val_if_fail(qof_choice_is_initialized() == TRUE, FALSE);
    choices = NULL;
    param_table = g_hash_table_lookup(qof_choice_table, type);
    choices = g_hash_table_lookup(param_table, param->param_name);
    return choices;
}

gboolean qof_choice_check(const char* choice_obj,
                          const char *param_name,
                          const char* choice )
{
    GList *choices, *result;
    GHashTable *param_table;

    choices = result = NULL;
    g_return_val_if_fail(qof_object_is_choice(choice_obj), FALSE);
    param_table = g_hash_table_lookup(qof_choice_table, choice_obj);
    choices = g_hash_table_lookup(param_table, param_name);
    result = g_list_find(choices, choice);
    if (!result)
    {
        return FALSE;
    }
    return TRUE;
}
