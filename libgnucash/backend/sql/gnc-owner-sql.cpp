/********************************************************************\
 * gnc-owner-sql.c -- owner sql implementation                      *
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

/** @file gnc-owner-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */
#include <guid.hpp>
extern "C"
{
#include <config.h>
#include <qof.h>
#include <glib.h>
#include "gncCustomerP.h"
#include "gncJobP.h"
#include "gncEmployeeP.h"
#include "gncVendorP.h"
}
#include <cstdlib>
#include <cstring>
#include <sstream>
#include "gnc-sql-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"

static QofLogModule log_module = G_LOG_DOMAIN;

typedef void (*OwnerSetterFunc) (gpointer, GncOwner*);
typedef GncOwner* (*OwnerGetterFunc) (const gpointer);

template<> void
GncSqlColumnTableEntryImpl<CT_OWNERREF>::load (const GncSqlBackend* sql_be,
                                               GncSqlRow& row,
                                               QofIdTypeConst obj_name,
                                               gpointer pObject) const noexcept
{
    GncOwnerType type;
    GncGUID guid;
    GncOwner owner;
    GncGUID* pGuid = nullptr;

    g_return_if_fail (sql_be != nullptr);
    g_return_if_fail (pObject != nullptr);

    auto book = sql_be->book();
    auto buf = std::string{m_col_name} + "_type";
    try
    {
        type = static_cast<decltype(type)>(row.get_int_at_col (buf.c_str()));
        buf = std::string{m_col_name} + "_guid";
        auto val = row.get_string_at_col (buf.c_str());
        if (string_to_guid (val.c_str(), &guid))
            pGuid = &guid;
    }
    catch (std::invalid_argument&)
    {
        return;
    }
    if (type == GNC_OWNER_NONE || pGuid == nullptr)
        return;
    
    switch (type)
    {
    case GNC_OWNER_CUSTOMER:
    {
        GncCustomer* cust = NULL;

        if (pGuid != NULL)
        {
            cust = gncCustomerLookup (book, pGuid);
            if (cust == NULL)
            {
                cust = gncCustomerCreate (book);
                gncCustomerSetGUID (cust, &guid);
            }
        }
        gncOwnerInitCustomer (&owner, cust);
        break;
    }

    case GNC_OWNER_JOB:
    {
        GncJob* job = NULL;

        if (pGuid != NULL)
        {
            job = gncJobLookup (book, pGuid);
            if (job == NULL)
            {
                job = gncJobCreate (book);
                gncJobSetGUID (job, &guid);
            }
        }
        gncOwnerInitJob (&owner, job);
        break;
    }

    case GNC_OWNER_VENDOR:
    {
        GncVendor* vendor = NULL;

        if (pGuid != NULL)
        {
            vendor = gncVendorLookup (book, pGuid);
            if (vendor == NULL)
            {
                vendor = gncVendorCreate (book);
                gncVendorSetGUID (vendor, &guid);
            }
        }
        gncOwnerInitVendor (&owner, vendor);
        break;
    }

    case GNC_OWNER_EMPLOYEE:
    {
        GncEmployee* employee = NULL;

        if (pGuid != NULL)
        {
            employee = gncEmployeeLookup (book, pGuid);
            if (employee == NULL)
            {
                employee = gncEmployeeCreate (book);
                gncEmployeeSetGUID (employee, &guid);
            }
        }
        gncOwnerInitEmployee (&owner, employee);
        break;
    }

    default:
        PWARN ("Invalid owner type: %d\n", type);
    }
    set_parameter (pObject, &owner, get_setter(obj_name), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_OWNERREF>::add_to_table(ColVec& vec) const noexcept
{
    auto buf = g_strdup_printf ("%s_type", m_col_name);
    GncSqlColumnInfo info(buf, BCT_INT, 0, false, false,
                          m_flags & COL_PKEY, m_flags & COL_NNUL);
    vec.emplace_back(std::move(info));
/* Buf isn't leaking, it belongs to ColVec now. */
    buf = g_strdup_printf ("%s_guid", m_col_name);
    GncSqlColumnInfo info2(buf, BCT_STRING, GUID_ENCODING_LENGTH, false, false,
                           m_flags & COL_PKEY, m_flags & COL_NNUL);
    vec.emplace_back(std::move(info2));
}

template<> void
GncSqlColumnTableEntryImpl<CT_OWNERREF>::add_to_query(QofIdTypeConst obj_name,
                                                      const gpointer pObject,
                                                      PairVec& vec) const noexcept
{
    g_return_if_fail (obj_name != NULL);
    g_return_if_fail (pObject != NULL);

    auto getter = (OwnerGetterFunc)get_getter (obj_name);
    auto owner = (*getter) (pObject);

    QofInstance* inst = nullptr;
    GncOwnerType type;

    auto type_hdr = std::string{m_col_name} + "_type";
    auto guid_hdr = std::string{m_col_name} + "_guid";

    if (owner != nullptr)
    {
        type = gncOwnerGetType (owner);
        switch (type)
        {
        case GNC_OWNER_CUSTOMER:
            inst = QOF_INSTANCE (gncOwnerGetCustomer (owner));
            break;

        case GNC_OWNER_JOB:
            inst = QOF_INSTANCE (gncOwnerGetJob (owner));
            break;

        case GNC_OWNER_VENDOR:
            inst = QOF_INSTANCE (gncOwnerGetVendor (owner));
            break;

        case GNC_OWNER_EMPLOYEE:
            inst = QOF_INSTANCE (gncOwnerGetEmployee (owner));
            break;

        default:
            PWARN ("Invalid owner type: %d\n", type);
        }
    }

    if (inst == nullptr)
    {
        /* Twice, once for type, once for guid. */
        vec.emplace_back (std::make_pair (type_hdr, std::string{"NULL"}));
        vec.emplace_back (std::make_pair (guid_hdr, std::string{"NULL"}));

        return;
    }
    std::ostringstream buf;

    buf << type;
    vec.emplace_back(std::make_pair(type_hdr, quote_string(buf.str())));
    buf.str("");
    auto guid = qof_instance_get_guid(inst);
    if (guid != nullptr)
        buf << guid_to_string(guid);
    else
        buf << "NULL";
    vec.emplace_back(std::make_pair(guid_hdr, quote_string(buf.str())));
}
