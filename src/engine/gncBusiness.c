/*
 * gncBusiness.c -- Business helper functions
 * Copyright (C) 2010 Christian Stimming
 * Author: Christian Stimming <christian@cstimming.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include "gncBusiness.h"

/* The initialization of the business objects is done in
 * cashobjects_register() of <engine/cashobjects.h>. */

struct _get_list_userdata
{
        GList *result;
        QofAccessFunc is_active_accessor_func;
};
static void get_list_cb (QofInstance *inst, gpointer user_data)
{
    struct _get_list_userdata* data = user_data;
    if (!data->is_active_accessor_func || data->is_active_accessor_func(inst, NULL))
        data->result = g_list_prepend(data->result, inst);
}

GList * gncBusinessGetList (QofBook *book, const char *type_name,
                            gboolean all_including_inactive)
{
    struct _get_list_userdata data;
    data.result = NULL;
    data.is_active_accessor_func = NULL;

    if (!all_including_inactive)
    {
        data.is_active_accessor_func =
            qof_class_get_parameter_getter(type_name, QOF_PARAM_ACTIVE);
    }

    qof_object_foreach(type_name, book, &get_list_cb, &data);

    return data.result;
}
