/********************************************************************\
 * gnc-option.cpp -- Application options system                     *
 * Copyright (C) 2019 John Ralls <jralls@ceridwen.us>               *
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

//#include "options.h"
#include "gnc-option.hpp"

template<> SCM
scm_from_value<std::string>(std::string value)
{
    return scm_from_utf8_string(value.c_str());
}

template<> SCM
scm_from_value<bool>(bool value)
{
    return value ? SCM_BOOL_T : SCM_BOOL_F;
}

template<> SCM
scm_from_value<int64_t>(int64_t value)
{
    return scm_from_int64(value);
}

template<> SCM
scm_from_value<QofInstance*>(QofInstance* value)
{
    auto guid = guid_to_string(qof_instance_get_guid(value));
    auto scm_guid = scm_from_utf8_string(guid);
    g_free(guid);
    return scm_guid;
}

