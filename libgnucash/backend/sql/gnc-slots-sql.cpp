/********************************************************************
 * gnc-slots-sql.c: load and save data to SQL                       *
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
\********************************************************************/
/** @file gnc-slots-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
#include <guid.hpp>
extern "C"
{
#include <config.h>

#include <glib.h>

#include <qof.h>
#include <gnc-engine.h>

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif
}

#include <string>
#include <sstream>

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-slots-sql.h"

#include <kvp-frame.hpp>

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "slots"
#define TABLE_VERSION 4

typedef enum
{
    NONE,
    FRAME,
    LIST
} context_t;

struct slot_info_t
{
    GncSqlBackend* be;
    const GncGUID* guid;
    gboolean is_ok;
    KvpFrame* pKvpFrame;
    KvpValue::Type value_type;
    GList* pList;
    context_t context;
    KvpValue* pKvpValue;
    std::string path;
};


static  gpointer get_obj_guid (gpointer pObject);
static void set_obj_guid (void);
static  gpointer get_path (gpointer pObject);
static void set_path (gpointer pObject,  gpointer pValue);
static KvpValue::Type get_slot_type (gpointer pObject);
static void set_slot_type (gpointer pObject,  gpointer pValue);
static gint64 get_int64_val (gpointer pObject);
static void set_int64_val (gpointer pObject, gint64 pValue);
static  gpointer get_string_val (gpointer pObject);
static void set_string_val (gpointer pObject,  gpointer pValue);
static  gpointer get_double_val (gpointer pObject);
static void set_double_val (gpointer pObject,  gpointer pValue);
static Timespec get_timespec_val (gpointer pObject);
static void set_timespec_val (gpointer pObject, Timespec *ts);
static  gpointer get_guid_val (gpointer pObject);
static void set_guid_val (gpointer pObject,  gpointer pValue);
static gnc_numeric get_numeric_val (gpointer pObject);
static void set_numeric_val (gpointer pObject, gnc_numeric *value);
static GDate* get_gdate_val (gpointer pObject);
static void set_gdate_val (gpointer pObject, GDate* value);
static slot_info_t* slot_info_copy (slot_info_t* pInfo, GncGUID* guid);
static void slots_load_info (slot_info_t* pInfo);

#define SLOT_MAX_PATHNAME_LEN 4096
#define SLOT_MAX_STRINGVAL_LEN 4096
enum
{
    id_col = 0,
    obj_guid_col,
    name_col,
    slot_type_col,
    int64_val_col,
    string_val_col,
    double_val_col,
    timespec_val_col,
    guid_val_col,
    numeric_val_col,
    gdate_val_col
};

static const EntryVec col_table
{
    /* col_name, col_type, size, flags, g0bj_param_name, qof_param_name, getter, setter */
    gnc_sql_make_table_entry<CT_INT>(
        "id", 0, COL_PKEY | COL_NNUL | COL_AUTOINC),
    gnc_sql_make_table_entry<CT_GUID>("obj_guid", 0, COL_NNUL,
                                      (QofAccessFunc)get_obj_guid,
                                      (QofSetterFunc)set_obj_guid),
    gnc_sql_make_table_entry<CT_STRING>("name", SLOT_MAX_PATHNAME_LEN, COL_NNUL,
                                        (QofAccessFunc)get_path, set_path),
    gnc_sql_make_table_entry<CT_INT>("slot_type", 0, COL_NNUL,
                                     (QofAccessFunc)get_slot_type,
                                     set_slot_type),
    gnc_sql_make_table_entry<CT_INT64>("int64_val", 0, 0,
                                       (QofAccessFunc)get_int64_val,
                                       (QofSetterFunc)set_int64_val),
    gnc_sql_make_table_entry<CT_STRING>("string_val", SLOT_MAX_PATHNAME_LEN, 0,
                                        (QofAccessFunc)get_string_val,
                                        set_string_val),
    gnc_sql_make_table_entry<CT_DOUBLE>("double_val", 0, 0,
                                        (QofAccessFunc)get_double_val,
                                        set_double_val),
    gnc_sql_make_table_entry<CT_TIMESPEC>("timespec_val", 0, 0,
                                          (QofAccessFunc)get_timespec_val,
                                          (QofSetterFunc)set_timespec_val),
    gnc_sql_make_table_entry<CT_GUID>("guid_val", 0, 0,
                                      (QofAccessFunc)get_guid_val,
                                      set_guid_val),
    gnc_sql_make_table_entry<CT_NUMERIC>("numeric_val", 0, 0,
                                         (QofAccessFunc)get_numeric_val,
                                         (QofSetterFunc)set_numeric_val),
    gnc_sql_make_table_entry<CT_GDATE>("gdate_val", 0, 0,
                                       (QofAccessFunc)get_gdate_val,
                                       (QofSetterFunc)set_gdate_val),
};

static void
_retrieve_guid_ (gpointer pObject,  gpointer pValue)
{
    GncGUID* pGuid = (GncGUID*)pObject;
    GncGUID* guid = (GncGUID*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pValue != NULL);

    memcpy (pGuid, guid, sizeof (GncGUID));
}

/* Special column table because we need to be able to access the table by
a column other than the primary key */
static const EntryVec obj_guid_col_table
{
    gnc_sql_make_table_entry<CT_GUID>("obj_guid", 0, 0,
                                      (QofAccessFunc)get_obj_guid,
                                      _retrieve_guid_),
};

static const EntryVec gdate_col_table
{
    gnc_sql_make_table_entry<CT_GDATE>("gdate_val", 0, 0),
};

GncSqlSlotsBackend::GncSqlSlotsBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_ACCOUNT,
                        TABLE_NAME, col_table) {}

/* ================================================================= */

inline static std::string::size_type
get_final_delim(std::string& path)
{
    auto idx = path.rfind('/');
    while (idx == path.length())
    {
        path.erase(idx);
        idx = path.rfind('/');
    }
    return idx;
}

static std::string
get_key_from_path (std::string path)
{
    auto idx = get_final_delim(path);
    if (idx == std::string::npos)
        return path;
    return path.substr(idx + 1);
}

static std::string
get_path_from_path (std::string path)
{
    auto idx = get_final_delim(path);
    if (idx == std::string::npos)
        return "";
    return path.substr(0, idx);
}

static void
set_slot_from_value (slot_info_t* pInfo, KvpValue* pValue)
{
    g_return_if_fail (pInfo != NULL);
    g_return_if_fail (pValue != NULL);

    switch (pInfo->context)
    {
    case FRAME:
    {
        pInfo->pKvpFrame->set ({pInfo->path}, pValue);
        break;
    }
    case LIST:
    {
        pInfo->pList = g_list_append (pInfo->pList, pValue);
        break;
    }
    case NONE:
    default:
    {
        auto key = get_key_from_path (pInfo->path);
        auto path = get_path_from_path (pInfo->path);
        auto frame = pInfo->pKvpFrame;
        if (!path.empty())
        {
            frame->set_path ({path, key}, pValue);
        }
        else
            frame->set ({key}, pValue);
        break;
    }
    }
}

static  gpointer
get_obj_guid (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, NULL);

    return (gpointer)pInfo->guid;
}

static void
set_obj_guid (void)
{
    // Nowhere to put the GncGUID
}

static  gpointer
get_path (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, NULL);

    return (gpointer)pInfo->path.c_str();
}

static void
set_path (gpointer pObject,  gpointer pValue)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    pInfo->path = static_cast<char*>(pValue);
}

static KvpValue::Type
get_slot_type (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, KvpValue::Type::INVALID);

//    return (gpointer)kvp_value_get_type( pInfo->pKvpValue );
    return pInfo->value_type;
}

static void
set_slot_type (gpointer pObject,  gpointer pValue)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pValue != NULL);

    pInfo->value_type = static_cast<KvpValue::Type> (GPOINTER_TO_INT (pValue));
}

static gint64
get_int64_val (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, 0);

    if (pInfo->pKvpValue->get_type () == KvpValue::Type::INT64)
    {
        return pInfo->pKvpValue->get<int64_t> ();
    }
    else
    {
        return 0;
    }
}

static void
set_int64_val (gpointer pObject, gint64 value)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue* pValue = NULL;

    g_return_if_fail (pObject != NULL);

    if (pInfo->value_type != KvpValue::Type::INT64) return;
    pValue = new KvpValue {value};
    set_slot_from_value (pInfo, pValue);
}

static  gpointer
get_string_val (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, NULL);

    if (pInfo->pKvpValue->get_type () == KvpValue::Type::STRING)
    {
        return (gpointer)pInfo->pKvpValue->get<const char*> ();
    }
    else
    {
        return NULL;
    }
}

static void
set_string_val (gpointer pObject,  gpointer pValue)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    g_return_if_fail (pObject != NULL);

    if (pInfo->value_type != KvpValue::Type::STRING || pValue == NULL)
        return;
    auto value = new KvpValue {g_strdup(static_cast<const char*> (pValue))};
    set_slot_from_value (pInfo, value);
}

static  gpointer
get_double_val (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    static double d_val;

    g_return_val_if_fail (pObject != NULL, NULL);

    if (pInfo->pKvpValue->get_type () == KvpValue::Type::DOUBLE)
    {
        d_val = pInfo->pKvpValue->get<double> ();
        return (gpointer)&d_val;
    }
    else
    {
        return NULL;
    }
}

static void
set_double_val (gpointer pObject,  gpointer pValue)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue* value = NULL;

    g_return_if_fail (pObject != NULL);

    if (pInfo->value_type != KvpValue::Type::DOUBLE || pValue == NULL) return;
    value = new KvpValue {* (static_cast<double*> (pValue))};
    set_slot_from_value (pInfo, value);
}

static Timespec
get_timespec_val (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, gnc_dmy2timespec (1, 1, 1970));

//if( kvp_value_get_type( pInfo->pKvpValue ) == KvpValue::Type::TIMESPEC ) {
    return pInfo->pKvpValue->get<Timespec> ();
}

static void
set_timespec_val (gpointer pObject, Timespec *ts)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue* value = NULL;

    g_return_if_fail (pObject != NULL);

    if (pInfo->value_type != KvpValue::Type::TIMESPEC) return;
    value = new KvpValue {*ts};
    set_slot_from_value (pInfo, value);
}

static  gpointer
get_guid_val (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, NULL);

    if (pInfo->pKvpValue->get_type () == KvpValue::Type::GUID)
    {
        return (gpointer)pInfo->pKvpValue->get<GncGUID*> ();
    }
    else
    {
        return NULL;
    }
}

static void
set_guid_val (gpointer pObject,  gpointer pValue)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_if_fail (pObject != NULL);
    if (pValue == NULL) return;

    switch (pInfo->value_type)
    {
    case KvpValue::Type::GUID:
    {
        auto new_guid = guid_copy (static_cast<GncGUID*> (pValue));
        set_slot_from_value (pInfo, new KvpValue {new_guid});
        break;
    }
    case KvpValue::Type::GLIST:
    {
        slot_info_t* newInfo = slot_info_copy (pInfo, (GncGUID*)pValue);
        KvpValue* pValue = NULL;
        auto key = get_key_from_path (pInfo->path);

        newInfo->context = LIST;

        slots_load_info (newInfo);
        pValue = new KvpValue {newInfo->pList};
        pInfo->pKvpFrame->set ({key.c_str()}, pValue);
	delete newInfo;
        break;
    }
    case KvpValue::Type::FRAME:
    {
        slot_info_t* newInfo = slot_info_copy (pInfo, (GncGUID*)pValue) ;
        auto newFrame = new KvpFrame;
        newInfo->pKvpFrame = newFrame;

        switch (pInfo->context)
        {
        case LIST:
        {
            auto value = new KvpValue {newFrame};
            newInfo->path = get_key_from_path (pInfo->path);
            pInfo->pList = g_list_append (pInfo->pList, value);
            break;
        }
        case FRAME:
        default:
        {
            auto key = get_key_from_path (pInfo->path);
            pInfo->pKvpFrame->set ({key.c_str()}, new KvpValue {newFrame});
            break;
        }
        }

        newInfo->context = FRAME;
        slots_load_info (newInfo);
        delete newInfo;
        break;
    }
    default:
        break;
    }
}

static gnc_numeric
get_numeric_val (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail (pObject != NULL, gnc_numeric_zero ());

    if (pInfo->pKvpValue->get_type () == KvpValue::Type::NUMERIC)
    {
        return pInfo->pKvpValue->get<gnc_numeric> ();
    }
    else
    {
        return gnc_numeric_zero ();
    }
}

static void
set_numeric_val (gpointer pObject, gnc_numeric *value)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue* pValue = NULL;

    g_return_if_fail (pObject != NULL);

    if (pInfo->value_type != KvpValue::Type::NUMERIC) return;
    set_slot_from_value (pInfo, new KvpValue {*value});
}

static GDate*
get_gdate_val (gpointer pObject)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    static GDate date;

    g_return_val_if_fail (pObject != NULL, NULL);

    if (pInfo->pKvpValue->get_type () == KvpValue::Type::GDATE)
    {
        date = pInfo->pKvpValue->get<GDate> ();
        return &date;
    }
    else
    {
        return NULL;
    }
}

static void
set_gdate_val (gpointer pObject, GDate* value)
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue* pValue = NULL;

    g_return_if_fail (pObject != NULL);

    if (pInfo->value_type != KvpValue::Type::GDATE) return;
    set_slot_from_value (pInfo, new KvpValue {*value});
}

static slot_info_t*
slot_info_copy (slot_info_t* pInfo, GncGUID* guid)
{
    g_return_val_if_fail (pInfo != NULL, NULL);
    auto newSlot = new slot_info_t;

    newSlot->be = pInfo->be;
    newSlot->guid = guid == NULL ? pInfo->guid : guid;
    newSlot->is_ok = pInfo->is_ok;
    newSlot->pKvpFrame = pInfo->pKvpFrame;
    newSlot->value_type = pInfo->value_type;
    newSlot->pList = pInfo->pList;
    newSlot->context = pInfo->context;
    newSlot->pKvpValue = pInfo->pKvpValue;
    newSlot->path.clear();
    return newSlot;
}

static void
save_slot (const char* key, KvpValue* value, slot_info_t & slot_info)
{
    g_return_if_fail (value != NULL);

    // Ignore if we've already run into a failure
    if (!slot_info.is_ok)
    {
        return;
    }
    auto curlen = slot_info.path.length();
    slot_info.pKvpValue = value;
    if (curlen != 0)
        slot_info.path += "/";

    slot_info.path += key;
    slot_info.value_type = value->get_type ();

    switch (slot_info.value_type)
    {
    case KvpValue::Type::FRAME:
    {
        auto pKvpFrame = value->get<KvpFrame*> ();
        auto guid = guid_new ();
        slot_info_t* pNewInfo = slot_info_copy (&slot_info, guid);
        KvpValue* oldValue = slot_info.pKvpValue;
        slot_info.pKvpValue = new KvpValue {guid};
        slot_info.is_ok = slot_info.be->do_db_operation(OP_DB_INSERT,
                                                            TABLE_NAME,
                                                            TABLE_NAME,
                                                            &slot_info,
                                                            col_table);
        g_return_if_fail (slot_info.is_ok);
        pKvpFrame->for_each_slot_temp (save_slot, *pNewInfo);
        delete slot_info.pKvpValue;
        slot_info.pKvpValue = oldValue;
        delete pNewInfo;
    }
    break;
    case KvpValue::Type::GLIST:
    {
        GncGUID* guid = guid_new ();
        slot_info_t* pNewInfo = slot_info_copy (&slot_info, guid);
        KvpValue* oldValue = slot_info.pKvpValue;
        slot_info.pKvpValue = new KvpValue {guid};  // Transfer ownership!
        slot_info.is_ok = slot_info.be->do_db_operation(OP_DB_INSERT,
                                                            TABLE_NAME,
                                                            TABLE_NAME,
                                                            &slot_info,
                                                            col_table);
        g_return_if_fail (slot_info.is_ok);
        for (auto cursor = value->get<GList*> (); cursor; cursor = cursor->next)
        {
            auto val = static_cast<KvpValue*> (cursor->data);
            save_slot ("", val, *pNewInfo);
        }
        delete slot_info.pKvpValue;
        slot_info.pKvpValue = oldValue;
        delete pNewInfo;
    }
    break;
    default:
    {
        slot_info.is_ok = slot_info.be->do_db_operation (OP_DB_INSERT,
                                                             TABLE_NAME,
                                                             TABLE_NAME,
                                                             &slot_info,
                                                             col_table);
    }
    break;
    }

    slot_info.path.erase(curlen);
}

gboolean
gnc_sql_slots_save (GncSqlBackend* sql_be, const GncGUID* guid, gboolean is_infant,
                    QofInstance* inst)
{
    slot_info_t slot_info = { NULL, NULL, TRUE, NULL, KvpValue::Type::INVALID,
                              NULL, FRAME, NULL, "" };
    KvpFrame* pFrame = qof_instance_get_slots (inst);

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (guid != NULL, FALSE);
    g_return_val_if_fail (pFrame != NULL, FALSE);

    // If this is not saving into a new db, clear out the old saved slots first
    if (!sql_be->pristine() && !is_infant)
    {
        (void)gnc_sql_slots_delete (sql_be, guid);
    }

    slot_info.be = sql_be;
    slot_info.guid = guid;
    pFrame->for_each_slot_temp (save_slot, slot_info);

    return slot_info.is_ok;
}

gboolean
gnc_sql_slots_delete (GncSqlBackend* sql_be, const GncGUID* guid)
{
    gchar* buf;
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];
    slot_info_t slot_info = { NULL, NULL, TRUE, NULL, KvpValue::Type::INVALID,
                              NULL, FRAME, NULL, "" };

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (guid != NULL, FALSE);

    (void)guid_to_string_buff (guid, guid_buf);

    buf = g_strdup_printf ("SELECT * FROM %s WHERE obj_guid='%s' and slot_type in ('%d', '%d') and not guid_val is null",
                           TABLE_NAME, guid_buf, KvpValue::Type::FRAME, KvpValue::Type::GLIST);
    auto stmt = sql_be->create_statement_from_sql(buf);
    g_free (buf);
    if (stmt != nullptr)
    {
        auto result = sql_be->execute_select_statement(stmt);
        for (auto row : *result)
        {
            try
            {
                const GncSqlColumnTableEntryPtr table_row =
                    col_table[guid_val_col];
                GncGUID child_guid;
                auto val = row.get_string_at_col (table_row->name());
                if (string_to_guid (val.c_str(), &child_guid))
                    gnc_sql_slots_delete (sql_be, &child_guid);
            }
            catch (std::invalid_argument)
            {
                continue;
            }
        }
    }

    slot_info.be = sql_be;
    slot_info.guid = guid;
    slot_info.is_ok = TRUE;
    slot_info.is_ok = sql_be->do_db_operation(OP_DB_DELETE, TABLE_NAME,
                                              TABLE_NAME, &slot_info,
                                              obj_guid_col_table);

    return slot_info.is_ok;
}

static void
load_slot (slot_info_t* pInfo, GncSqlRow& row)
{
    slot_info_t* slot_info;

    g_return_if_fail (pInfo != NULL);
    g_return_if_fail (pInfo->be != NULL);
    g_return_if_fail (pInfo->pKvpFrame != NULL);

    slot_info = slot_info_copy (pInfo, NULL);

    gnc_sql_load_object (pInfo->be, row, TABLE_NAME, slot_info, col_table);

    if (slot_info->pList != pInfo->pList)
    {
        if (pInfo->pList != NULL)
        {
            PWARN ("Load slot returned a different list than the original");
        }
        else
        {
            pInfo->pList = slot_info->pList;
        }
    }
    delete slot_info;
}

void
gnc_sql_slots_load (GncSqlBackend* sql_be, QofInstance* inst)
{
    slot_info_t info = { NULL, NULL, TRUE, NULL, KvpValue::Type::INVALID,
                         NULL, FRAME, NULL, "" };
    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (inst != NULL);

    info.be = sql_be;
    info.guid = qof_instance_get_guid (inst);
    info.pKvpFrame = qof_instance_get_slots (inst);
    info.context = NONE;

    slots_load_info (&info);
}

static void
slots_load_info (slot_info_t* pInfo)
{
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];

    g_return_if_fail (pInfo != NULL);
    g_return_if_fail (pInfo->be != NULL);
    g_return_if_fail (pInfo->guid != NULL);
    g_return_if_fail (pInfo->pKvpFrame != NULL);

    (void)guid_to_string_buff (pInfo->guid, guid_buf);

    std::stringstream buf;
    buf << "SELECT * FROM " << TABLE_NAME <<
        " WHERE obj_guid='" << guid_buf << "'";
    auto stmt = pInfo->be->create_statement_from_sql (buf.str());
    if (stmt != nullptr)
    {
        auto result = pInfo->be->execute_select_statement (stmt);
        for (auto row : *result)
            load_slot (pInfo, row);
        delete result;
    }
}

static  const GncGUID*
load_obj_guid (const GncSqlBackend* sql_be, GncSqlRow& row)
{
    static GncGUID guid;

    g_return_val_if_fail (sql_be != NULL, NULL);

    gnc_sql_load_object (sql_be, row, NULL, &guid, obj_guid_col_table);

    return &guid;
}

static void
load_slot_for_list_item (GncSqlBackend* sql_be, GncSqlRow& row,
                         QofCollection* coll)
{
    slot_info_t slot_info = { NULL, NULL, TRUE, NULL, KvpValue::Type::INVALID,
                              NULL, FRAME, NULL, "" };
    const GncGUID* guid;
    QofInstance* inst;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (coll != NULL);

    guid = load_obj_guid (sql_be, row);
    g_assert (guid != NULL);
    inst = qof_collection_lookup_entity (coll, guid);

    slot_info.be = sql_be;
    slot_info.pKvpFrame = qof_instance_get_slots (inst);
    slot_info.context = NONE;

    gnc_sql_load_object (sql_be, row, TABLE_NAME, &slot_info, col_table);


}

void
gnc_sql_slots_load_for_instancevec (GncSqlBackend* sql_be, InstanceVec& instances)
{
    QofCollection* coll;
    std::stringstream sql;

    g_return_if_fail (sql_be != NULL);

    // Ignore empty list
    if (instances.empty()) return;

    coll = qof_instance_get_collection (instances[0]);

    // Create the query for all slots for all items on the list

    sql << "SELECT * FROM " << TABLE_NAME << " WHERE " <<
                            obj_guid_col_table[0]->name();
    if (instances.size() != 1)
        sql << " IN (";
    else
        sql << " = ";

    gnc_sql_append_guids_to_sql (sql, instances);
    if (instances.size() > 1)
        sql << ")";

    // Execute the query and load the slots
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    if (stmt == nullptr)
    {
        PERR ("stmt == NULL, SQL = '%s'\n", sql.str().c_str());
        return;
    }
    auto result = sql_be->execute_select_statement (stmt);
    for (auto row : *result)
        load_slot_for_list_item (sql_be, row, coll);
}

static void
load_slot_for_book_object (GncSqlBackend* sql_be, GncSqlRow& row,
                           BookLookupFn lookup_fn)
{
    slot_info_t slot_info = { NULL, NULL, TRUE, NULL, KvpValue::Type::INVALID,
                              NULL, FRAME, NULL, "" };
    const GncGUID* guid;
    QofInstance* inst;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (lookup_fn != NULL);

    guid = load_obj_guid (sql_be, row);
    g_return_if_fail (guid != NULL);
    inst = lookup_fn (guid, sql_be->book());
    if (inst == NULL) return; /* Silently bail if the guid isn't loaded yet. */

    slot_info.be = sql_be;
    slot_info.pKvpFrame = qof_instance_get_slots (inst);
    slot_info.path.clear();

    gnc_sql_load_object (sql_be, row, TABLE_NAME, &slot_info, col_table);
}

/**
 * gnc_sql_slots_load_for_sql_subquery - Loads slots for all objects whose guid is
 * supplied by a subquery.  The subquery should be of the form "SELECT DISTINCT guid FROM ...".
 * This is faster than loading for one object at a time because fewer SQL queries * are used.
 *
 * @param sql_be SQL backend
 * @param subquery Subquery SQL string
 * @param lookup_fn Lookup function
 */
void gnc_sql_slots_load_for_sql_subquery (GncSqlBackend* sql_be,
                                          const gchar* subquery,
                                          BookLookupFn lookup_fn)
{
    gchar* sql;

    g_return_if_fail (sql_be != NULL);

    // Ignore empty subquery
    if (subquery == NULL) return;

    sql = g_strdup_printf ("SELECT * FROM %s WHERE %s IN (%s)",
                           TABLE_NAME, obj_guid_col_table[0]->name(),
                           subquery);

    // Execute the query and load the slots
    auto stmt = sql_be->create_statement_from_sql(sql);
    if (stmt == nullptr)
    {
        PERR ("stmt == NULL, SQL = '%s'\n", sql);
        g_free (sql);
        return;
    }
    g_free (sql);
    auto result = sql_be->execute_select_statement(stmt);
    for (auto row : *result)
        load_slot_for_book_object (sql_be, row, lookup_fn);
    delete result;
}

/* ================================================================= */
void
GncSqlSlotsBackend::create_tables (GncSqlBackend* sql_be)
{
    gint version;
    gboolean ok;

    g_return_if_fail (sql_be != NULL);

    version = sql_be->get_table_version( TABLE_NAME);
    if (version == 0)
    {
        (void)sql_be->create_table(TABLE_NAME, TABLE_VERSION, col_table);

        ok = sql_be->create_index ("slots_guid_index", TABLE_NAME,
                               obj_guid_col_table);
        if (!ok)
        {
            PERR ("Unable to create index\n");
        }
    }
    else if (version < m_version)
    {
        /* Upgrade:
            1->2: 64-bit int values to proper definition, add index
            2->3: Add gdate field
            3->4: Use DATETIME instead of TIMESTAMP in MySQL
        */
        if (version == 1)
        {
            sql_be->upgrade_table(TABLE_NAME, col_table);
            ok = sql_be->create_index ("slots_guid_index", TABLE_NAME,
                                   obj_guid_col_table);
            if (!ok)
            {
                PERR ("Unable to create index\n");
            }
        }
        else if (version == 2)
        {
            ok = sql_be->add_columns_to_table(TABLE_NAME, gdate_col_table);
            if (!ok)
            {
                PERR ("Unable to add gdate column\n");
            }
        }
        else
        {
            sql_be->upgrade_table(TABLE_NAME, col_table);
        }
        sql_be->set_table_version (TABLE_NAME, TABLE_VERSION);
        PINFO ("Slots table upgraded from version %d to version %d\n", version,
               TABLE_VERSION);
    }
}

/* ========================== END OF FILE ===================== */
