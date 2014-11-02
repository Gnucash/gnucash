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

#include "config.h"

#include <glib.h>

#include "qof.h"
#include "gnc-engine.h"

#include "gnc-backend-sql.h"

#include "gnc-slots-sql.h"

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif

/*@ unused @*/ static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "slots"
#define TABLE_VERSION 3

typedef enum
{
    NONE,
    FRAME,
    LIST
} context_t;

typedef struct
{
    /*@ dependent @*/ 	GncSqlBackend* be;
    /*@ dependent @*/
    const GncGUID* guid;
    gboolean is_ok;
    /*@ dependent @*/
    KvpFrame* pKvpFrame;
    KvpValueType value_type;
    GList *pList;
    context_t context;
    /*@ dependent @*/
    KvpValue* pKvpValue;
    GString* path;
} slot_info_t;


static /*@ null @*/ gpointer get_obj_guid( gpointer pObject );
static void set_obj_guid( void );
static /*@ null @*/ gpointer get_path( gpointer pObject );
static void set_path( gpointer pObject, /*@ null @*/ gpointer pValue );
static KvpValueType get_slot_type( gpointer pObject );
static void set_slot_type( gpointer pObject, /*@ null @*/ gpointer pValue );
static gint64 get_int64_val( gpointer pObject );
static void set_int64_val( gpointer pObject, gint64 pValue );
static /*@ null @*/ gpointer get_string_val( gpointer pObject );
static void set_string_val( gpointer pObject, /*@ null @*/ gpointer pValue );
static /*@ dependent @*//*@ null @*/ gpointer get_double_val( gpointer pObject );
static void set_double_val( gpointer pObject, /*@ null @*/ gpointer pValue );
static Timespec get_timespec_val( gpointer pObject );
static void set_timespec_val( gpointer pObject, Timespec ts );
static /*@ null @*/ gpointer get_guid_val( gpointer pObject );
static void set_guid_val( gpointer pObject, /*@ null @*/ gpointer pValue );
static gnc_numeric get_numeric_val( gpointer pObject );
static void set_numeric_val( gpointer pObject, gnc_numeric value );
static GDate* get_gdate_val( gpointer pObject );
static void set_gdate_val( gpointer pObject, GDate* value );
static slot_info_t *slot_info_copy( slot_info_t *pInfo, GncGUID *guid );
static void slots_load_info( slot_info_t *pInfo );

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

static const GncSqlColumnTableEntry col_table[] =
{
    /* col_name, col_type, size, flags, g0bj_param_name, qof_param_name, getter, setter */
    /*@ -full_init_block @*/
    { "id",             CT_INT,      0, COL_PKEY | COL_NNUL | COL_AUTOINC },
    {
        "obj_guid",     CT_GUID,     0,                     COL_NNUL, NULL, NULL,
        (QofAccessFunc)get_obj_guid,     (QofSetterFunc)set_obj_guid
    },
    {
        "name",         CT_STRING,   SLOT_MAX_PATHNAME_LEN, COL_NNUL, NULL, NULL,
        (QofAccessFunc)get_path,         set_path
    },
    {
        "slot_type",    CT_INT,      0,                     COL_NNUL, NULL, NULL,
        (QofAccessFunc)get_slot_type,    set_slot_type,
    },
    {
        "int64_val",    CT_INT64,    0,                     0,        NULL, NULL,
        (QofAccessFunc)get_int64_val,    (QofSetterFunc)set_int64_val
    },
    {
        "string_val",   CT_STRING,   SLOT_MAX_PATHNAME_LEN, 0,        NULL, NULL,
        (QofAccessFunc)get_string_val,   set_string_val
    },
    {
        "double_val",   CT_DOUBLE,   0,                     0,        NULL, NULL,
        (QofAccessFunc)get_double_val,   set_double_val
    },
    {
        "timespec_val", CT_TIMESPEC, 0,                     0,        NULL, NULL,
        (QofAccessFunc)get_timespec_val, (QofSetterFunc)set_timespec_val
    },
    {
        "guid_val",     CT_GUID,     0,                     0,        NULL, NULL,
        (QofAccessFunc)get_guid_val,     set_guid_val
    },
    {
        "numeric_val",  CT_NUMERIC,  0,                     0,        NULL, NULL,
        (QofAccessFunc)get_numeric_val, (QofSetterFunc)set_numeric_val
    },
    {
        "gdate_val",    CT_GDATE,    0,                     0,        NULL, NULL,
        (QofAccessFunc)get_gdate_val, (QofSetterFunc)set_gdate_val
    },
    { NULL }
    /*@ +full_init_block @*/
};

/* Special column table because we need to be able to access the table by
a column other than the primary key */
static const GncSqlColumnTableEntry obj_guid_col_table[] =
{
    /*@ -full_init_block @*/
    { "obj_guid", CT_GUID, 0, 0, NULL, NULL, (QofAccessFunc)get_obj_guid, _retrieve_guid_ },
    { NULL }
    /*@ +full_init_block @*/
};

static const GncSqlColumnTableEntry gdate_col_table[] =
{
    /*@ -full_init_block @*/
    { "gdate_val", CT_GDATE, 0, 0, },
    { NULL }
    /*@ +full_init_block @*/
};

/* ================================================================= */

static gchar *
get_key_from_path( GString *path )
{
    gchar *str = NULL, *key = NULL, *ret = NULL;

    g_return_val_if_fail( path != NULL, strdup("") );

    if ( path->str == NULL ) return strdup("");
    str = g_strdup( path->str );
    key = strrchr( str, '/');

    /* Remove trailing /es */
    if ( key == NULL ) return str;
    while ( str + strlen(str) - key == 1 )
    {
        *key = '\0';
        key = strrchr( str, '/' );
    }
    if ( key == NULL ) return str;
    /* Now advance key past the last intermediate / to get the post-delimiter string */
    while ( *key == '/') ++key;

    ret = strdup( key );
    g_free( str );
    return ret;
}

static gchar *
get_path_from_path( GString *path )
{
    gchar *str = NULL, *key = NULL;

    g_return_val_if_fail( path != NULL, NULL );

    if ( path->str == NULL ) return NULL;
    str = g_strdup( path->str );
    key = strrchr( str, '/');
    /* No /es means no path, just a key */
    if ( key == NULL )
    {
        g_free( str );
        return NULL;
    }
    /* Remove trailing /es */
    while ( str + strlen(str) - key == 1 )
    {
        *key = '\0';
        key = strrchr( str, '/' );
    }
    if ( key == NULL )
    {
        g_free(str);
        return NULL;
    }
    /* reterminate the string at the slash */
    *key = '\0';

    return str;
}

static void
set_slot_from_value( slot_info_t *pInfo, KvpValue *pValue)
{
    g_return_if_fail( pInfo != NULL );
    g_return_if_fail( pValue != NULL );

    switch ( pInfo->context)
    {
    case FRAME:
    {
        gchar *key = get_key_from_path( pInfo->path );
        kvp_frame_set_value_nc( pInfo->pKvpFrame, key, pValue );
        g_free( key );
        break;
    }
    case LIST:
    {
        pInfo->pList = g_list_append(pInfo->pList, pValue);
        break;
    }
    case NONE:
    default:
    {
        gchar *key = get_key_from_path( pInfo->path );
        gchar *path = get_path_from_path( pInfo->path );
        KvpFrame* frame = pInfo->pKvpFrame;
        if ( path )
        {
            frame = kvp_frame_get_frame_slash( frame, path );
            g_free( path );
        }
        kvp_frame_set_value_nc( frame, key, pValue );
        g_free( key );
        break;
    }
    }
}

static /*@ null @*/ gpointer
get_obj_guid( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail( pObject != NULL, NULL );

    return (gpointer)pInfo->guid;
}

static void
set_obj_guid( void )
{
    // Nowhere to put the GncGUID
}

static /*@ null @*/ gpointer
get_path( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail( pObject != NULL, NULL );

    return (gpointer)pInfo->path->str;
}

static void
set_path( gpointer pObject, /*@ null @*/ gpointer pValue )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_if_fail( pObject != NULL );
    g_return_if_fail( pValue != NULL );

    if ( pInfo->path != NULL )
    {
        (void)g_string_free( pInfo->path, TRUE );
    }
    pInfo->path = g_string_new( (gchar*)pValue );
}

static KvpValueType
get_slot_type( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail( pObject != NULL, 0 );

//    return (gpointer)kvp_value_get_type( pInfo->pKvpValue );
    return pInfo->value_type;
}

static void
set_slot_type( gpointer pObject, /*@ null @*/ gpointer pValue )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_if_fail( pObject != NULL );
    g_return_if_fail( pValue != NULL );

    pInfo->value_type = (KvpValueType)pValue;
}

static gint64
get_int64_val( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail( pObject != NULL, 0 );

    if ( kvp_value_get_type( pInfo->pKvpValue ) == KVP_TYPE_GINT64 )
    {
        return kvp_value_get_gint64( pInfo->pKvpValue );
    }
    else
    {
        return 0;
    }
}

static void
set_int64_val( gpointer pObject, gint64 value )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue *pValue = NULL;

    g_return_if_fail( pObject != NULL );

    if ( pInfo->value_type != KVP_TYPE_GINT64 ) return;
    pValue = kvp_value_new_gint64( value );
    set_slot_from_value( pInfo, pValue );
}

static /*@ null @*/ gpointer
get_string_val( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail( pObject != NULL, NULL );

    if ( kvp_value_get_type( pInfo->pKvpValue ) == KVP_TYPE_STRING )
    {
        return (gpointer)kvp_value_get_string( pInfo->pKvpValue );
    }
    else
    {
        return NULL;
    }
}

static void
set_string_val( gpointer pObject, /*@ null @*/ gpointer pValue )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue *value = NULL;

    g_return_if_fail( pObject != NULL );

    if ( pInfo->value_type != KVP_TYPE_STRING || pValue == NULL ) return;
    value = kvp_value_new_string( (gchar*)pValue );
    set_slot_from_value( pInfo, value );
}

static /*@ dependent @*//*@ null @*/ gpointer
get_double_val( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    static double d_val;

    g_return_val_if_fail( pObject != NULL, NULL );

    if ( kvp_value_get_type( pInfo->pKvpValue ) == KVP_TYPE_DOUBLE )
    {
        d_val = kvp_value_get_double( pInfo->pKvpValue );
        return (gpointer)&d_val;
    }
    else
    {
        return NULL;
    }
}

static void
set_double_val( gpointer pObject, /*@ null @*/ gpointer pValue )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue *value = NULL;

    g_return_if_fail( pObject != NULL );

    if ( pInfo->value_type != KVP_TYPE_DOUBLE || pValue == NULL ) return;
    value = kvp_value_new_double( *((double*)pValue) );
    set_slot_from_value( pInfo, value );
}

static Timespec
get_timespec_val( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail( pObject != NULL, gnc_dmy2timespec( 1, 1, 1970 ) );

//if( kvp_value_get_type( pInfo->pKvpValue ) == KVP_TYPE_TIMESPEC ) {
    return kvp_value_get_timespec( pInfo->pKvpValue );
}

static void
set_timespec_val( gpointer pObject, Timespec ts )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue *value = NULL;

    g_return_if_fail( pObject != NULL );

    if ( pInfo->value_type != KVP_TYPE_TIMESPEC ) return;
    value = kvp_value_new_timespec( ts );
    set_slot_from_value( pInfo, value );
}

static /*@ null @*/ gpointer
get_guid_val( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail( pObject != NULL, NULL );

    if ( kvp_value_get_type( pInfo->pKvpValue ) == KVP_TYPE_GUID )
    {
        return (gpointer)kvp_value_get_guid( pInfo->pKvpValue );
    }
    else
    {
        return NULL;
    }
}

static void
set_guid_val( gpointer pObject, /*@ null @*/ gpointer pValue )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_if_fail( pObject != NULL );
    if ( pValue == NULL ) return;

    switch ( pInfo->value_type)
    {
    case KVP_TYPE_GUID:
    {
        KvpValue *value = kvp_value_new_guid( (GncGUID*)pValue );
        set_slot_from_value( pInfo, value );
        break;
    }
    case KVP_TYPE_GLIST:
    {
        slot_info_t *newInfo = slot_info_copy( pInfo, (GncGUID*)pValue );
        KvpValue *pValue = NULL;
        gchar *key = get_key_from_path( pInfo->path );

        newInfo->context = LIST;

        slots_load_info( newInfo );
        pValue = kvp_value_new_glist_nc( newInfo->pList );
        kvp_frame_set_slot_nc(pInfo->pKvpFrame, key, pValue);
        g_string_free( newInfo->path, TRUE );
        g_slice_free( slot_info_t, newInfo );
        g_free( key );
        break;
    }
    case KVP_TYPE_FRAME:
    {
        slot_info_t *newInfo = slot_info_copy( pInfo, (GncGUID*)pValue ) ;
        KvpFrame *newFrame = kvp_frame_new();
        newInfo->pKvpFrame = newFrame;

        switch ( pInfo->context )
        {
        case LIST:
        {
            KvpValue *value = kvp_value_new_frame_nc( newFrame );
            gchar *key = get_key_from_path( pInfo->path );
            newInfo->path = g_string_assign( newInfo->path, key );
            pInfo->pList = g_list_append( pInfo->pList, value );
            g_free( key );
            break;
        }
        case FRAME:
        default:
        {
            gchar *key = get_key_from_path( pInfo->path );
            kvp_frame_set_frame_nc( pInfo->pKvpFrame, key, newFrame );
            g_free( key );
            break;
        }
        }

        newInfo->context = FRAME;
        slots_load_info ( newInfo );
        g_string_free( newInfo->path, TRUE );
        g_slice_free( slot_info_t, newInfo );
        break;
    }
    default:
        break;
    }
}

static gnc_numeric
get_numeric_val( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;

    g_return_val_if_fail( pObject != NULL, gnc_numeric_zero() );

    if ( kvp_value_get_type( pInfo->pKvpValue ) == KVP_TYPE_NUMERIC )
    {
        return kvp_value_get_numeric( pInfo->pKvpValue );
    }
    else
    {
        return gnc_numeric_zero();
    }
}

static void
set_numeric_val( gpointer pObject, gnc_numeric value )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue *pValue = NULL;

    g_return_if_fail( pObject != NULL );

    if ( pInfo->value_type != KVP_TYPE_NUMERIC ) return;
    pValue = kvp_value_new_numeric( value );
    set_slot_from_value( pInfo, pValue );
}

static GDate*
get_gdate_val( gpointer pObject )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    static GDate date;

    g_return_val_if_fail( pObject != NULL, NULL );

    if ( kvp_value_get_type( pInfo->pKvpValue ) == KVP_TYPE_GDATE )
    {
        date = kvp_value_get_gdate( pInfo->pKvpValue );
        return &date;
    }
    else
    {
        return NULL;
    }
}

static void
set_gdate_val( gpointer pObject, GDate* value )
{
    slot_info_t* pInfo = (slot_info_t*)pObject;
    KvpValue *pValue = NULL;

    g_return_if_fail( pObject != NULL );

    if ( pInfo->value_type != KVP_TYPE_GDATE ) return;
    pValue = kvp_value_new_gdate( *value );
    set_slot_from_value( pInfo, pValue );
}

static slot_info_t *
slot_info_copy( slot_info_t *pInfo, GncGUID *guid )
{
    slot_info_t *newSlot;
    g_return_val_if_fail( pInfo != NULL, NULL );
    newSlot = g_slice_new0(slot_info_t);

    newSlot->be = pInfo->be;
    newSlot->guid = guid == NULL ? pInfo->guid : guid;
    newSlot->is_ok = pInfo->is_ok;
    newSlot->pKvpFrame = pInfo->pKvpFrame;
    newSlot->value_type = pInfo->value_type;
    newSlot->pList = pInfo->pList;
    newSlot->context = pInfo->context;
    newSlot->pKvpValue = pInfo->pKvpValue;
    newSlot->path = g_string_new(pInfo->path->str);
    return newSlot;
}

static void
save_slot( const gchar* key, KvpValue* value, gpointer data )
{
    slot_info_t* pSlot_info = (slot_info_t*)data;
    gsize curlen;

    g_return_if_fail( key != NULL );
    g_return_if_fail( value != NULL );
    g_return_if_fail( data != NULL );

    // Ignore if we've already run into a failure
    if ( !pSlot_info->is_ok )
    {
        return;
    }

    curlen = pSlot_info->path->len;
    pSlot_info->pKvpValue = value;
    if ( curlen != 0 )
    {
        (void)g_string_append( pSlot_info->path, "/" );
    }
    (void)g_string_append( pSlot_info->path, key );
    pSlot_info->value_type = kvp_value_get_type( value );

    switch ( pSlot_info->value_type )
    {
    case KVP_TYPE_FRAME:
    {
        KvpFrame* pKvpFrame = kvp_value_get_frame( value );
        GncGUID guid = guid_new_return();
        slot_info_t *pNewInfo = slot_info_copy( pSlot_info, &guid );
        KvpValue *oldValue = pSlot_info->pKvpValue;
        pSlot_info->pKvpValue = kvp_value_new_guid( &guid );
        pSlot_info->is_ok = gnc_sql_do_db_operation( pSlot_info->be,
                            OP_DB_INSERT, TABLE_NAME,
                            TABLE_NAME, pSlot_info,
                            col_table );
        g_return_if_fail( pSlot_info->is_ok );
        kvp_frame_for_each_slot( pKvpFrame, save_slot, pNewInfo );
        kvp_value_delete( pSlot_info->pKvpValue );
        pSlot_info->pKvpValue = oldValue;
        g_string_free( pNewInfo->path, TRUE );
        g_slice_free( slot_info_t, pNewInfo );
    }
    break;
    case KVP_TYPE_GLIST:
    {
        GList *cursor;
        GncGUID guid = guid_new_return();
        slot_info_t *pNewInfo = slot_info_copy( pSlot_info, &guid );
        KvpValue *oldValue = pSlot_info->pKvpValue;
        pSlot_info->pKvpValue = kvp_value_new_guid( &guid );
        pSlot_info->is_ok = gnc_sql_do_db_operation( pSlot_info->be,
                            OP_DB_INSERT, TABLE_NAME,
                            TABLE_NAME, pSlot_info,
                            col_table );
        g_return_if_fail( pSlot_info->is_ok );
        for (cursor = kvp_value_get_glist(value); cursor; cursor = cursor->next)
        {
            KvpValue *val = (KvpValue*)cursor->data;
            save_slot("", val, pNewInfo);
        }
        kvp_value_delete( pSlot_info->pKvpValue );
        pSlot_info->pKvpValue = oldValue;
        g_string_free( pNewInfo->path, TRUE );
        g_slice_free( slot_info_t, pNewInfo );
    }
    break;
    default:
    {
        pSlot_info->is_ok = gnc_sql_do_db_operation( pSlot_info->be,
                            OP_DB_INSERT, TABLE_NAME,
                            TABLE_NAME, pSlot_info,
                            col_table );
    }
    break;
    }

    (void)g_string_truncate( pSlot_info->path, curlen );
}

gboolean
gnc_sql_slots_save( GncSqlBackend* be, const GncGUID* guid, gboolean is_infant, KvpFrame* pFrame )
{
    slot_info_t slot_info = { NULL, NULL, TRUE, NULL, 0, NULL, FRAME, NULL, g_string_new(NULL) };

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( guid != NULL, FALSE );
    g_return_val_if_fail( pFrame != NULL, FALSE );

    // If this is not saving into a new db, clear out the old saved slots first
    if ( !be->is_pristine_db && !is_infant )
    {
        (void)gnc_sql_slots_delete( be, guid );
    }

    slot_info.be = be;
    slot_info.guid = guid;
    kvp_frame_for_each_slot( pFrame, save_slot, &slot_info );
    (void)g_string_free( slot_info.path, TRUE );

    return slot_info.is_ok;
}

gboolean
gnc_sql_slots_delete( GncSqlBackend* be, const GncGUID* guid )
{
    gchar* buf;
    GncSqlResult* result;
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];
    GncSqlStatement* stmt;
    slot_info_t slot_info = { NULL, NULL, TRUE, NULL, 0, NULL, FRAME, NULL, g_string_new(NULL) };

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( guid != NULL, FALSE );

    (void)guid_to_string_buff( guid, guid_buf );

    buf = g_strdup_printf( "SELECT * FROM %s WHERE obj_guid='%s' and slot_type in ('%d', '%d') and not guid_val is null",
                           TABLE_NAME, guid_buf, KVP_TYPE_FRAME, KVP_TYPE_GLIST );
    stmt = gnc_sql_create_statement_from_sql( be, buf );
    g_free( buf );
    if ( stmt != NULL )
    {
        result = gnc_sql_execute_select_statement( be, stmt );
        gnc_sql_statement_dispose( stmt );
        if ( result != NULL )
        {
            GncSqlRow* row = gnc_sql_result_get_first_row( result );

            while ( row != NULL )
            {
                GncSqlColumnTableEntry table_row = col_table[guid_val_col];
                GncGUID child_guid;
                const GValue* val =
                    gnc_sql_row_get_value_at_col_name( row, table_row.col_name);
                if ( val == NULL )
                    continue;

                (void)string_to_guid( g_value_get_string( val ), &child_guid );
                gnc_sql_slots_delete( be, &child_guid );
                row = gnc_sql_result_get_next_row( result );
            }
            gnc_sql_result_dispose( result );
        }
    }

    slot_info.be = be;
    slot_info.guid = guid;
    slot_info.is_ok = TRUE;
    slot_info.is_ok = gnc_sql_do_db_operation( be, OP_DB_DELETE, TABLE_NAME,
                      TABLE_NAME, &slot_info, obj_guid_col_table );

    return slot_info.is_ok;
}

static void
load_slot( slot_info_t *pInfo, GncSqlRow* row )
{
    slot_info_t *slot_info;

    g_return_if_fail( pInfo != NULL );
    g_return_if_fail( pInfo->be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pInfo->pKvpFrame != NULL );

    slot_info = slot_info_copy( pInfo, NULL );
    g_string_free( slot_info->path, TRUE );
    slot_info->path = NULL;

    gnc_sql_load_object( pInfo->be, row, TABLE_NAME, slot_info, col_table );

    if ( slot_info->path != NULL )
    {
        (void)g_string_free( slot_info->path, TRUE );
    }
    if ( slot_info->pList != pInfo->pList )
    {
        if (pInfo->pList != NULL)
        {
            PWARN("Load slot returned a different list than the original");
        }
        else
        {
            pInfo->pList = slot_info->pList;
        }
    }
    g_slice_free( slot_info_t, slot_info );
}

void
gnc_sql_slots_load( GncSqlBackend* be, QofInstance* inst )
{
    slot_info_t info = { NULL, NULL, TRUE, NULL, 0, NULL, FRAME, NULL, g_string_new(NULL) };
    g_return_if_fail( be != NULL );
    g_return_if_fail( inst != NULL );

    info.be = be;
    info.guid = qof_instance_get_guid( inst );
    info.pKvpFrame = qof_instance_get_slots( inst );
    info.context = NONE;

    slots_load_info( &info );
}

static void
slots_load_info ( slot_info_t *pInfo )
{
    gchar* buf;
    GncSqlResult* result;
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];
    GncSqlStatement* stmt;

    g_return_if_fail( pInfo != NULL );
    g_return_if_fail( pInfo->be != NULL );
    g_return_if_fail( pInfo->guid != NULL );
    g_return_if_fail( pInfo->pKvpFrame != NULL );

    (void)guid_to_string_buff( pInfo->guid, guid_buf );

    buf = g_strdup_printf( "SELECT * FROM %s WHERE obj_guid='%s'",
                           TABLE_NAME, guid_buf );
    stmt = gnc_sql_create_statement_from_sql( pInfo->be, buf );
    g_free( buf );
    if ( stmt != NULL )
    {
        result = gnc_sql_execute_select_statement( pInfo->be, stmt );
        gnc_sql_statement_dispose( stmt );
        if ( result != NULL )
        {
            GncSqlRow* row = gnc_sql_result_get_first_row( result );

            while ( row != NULL )
            {
                load_slot( pInfo, row );
                row = gnc_sql_result_get_next_row( result );
            }
            gnc_sql_result_dispose( result );
        }
    }
}

static /*@ dependent @*//*@ null @*/ const GncGUID*
load_obj_guid( const GncSqlBackend* be, GncSqlRow* row )
{
    static GncGUID guid;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( row != NULL, NULL );

    gnc_sql_load_object( be, row, NULL, &guid, obj_guid_col_table );

    return &guid;
}

static void
load_slot_for_list_item( GncSqlBackend* be, GncSqlRow* row, QofCollection* coll )
{
    slot_info_t slot_info = { NULL, NULL, TRUE, NULL, 0, NULL, FRAME, NULL, NULL };
    const GncGUID* guid;
    QofInstance* inst;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( coll != NULL );

    guid = load_obj_guid( be, row );
    g_assert( guid != NULL );
    inst = qof_collection_lookup_entity( coll, guid );

    slot_info.be = be;
    slot_info.pKvpFrame = qof_instance_get_slots( inst );
    slot_info.context = NONE;

    gnc_sql_load_object( be, row, TABLE_NAME, &slot_info, col_table );

    if ( slot_info.path != NULL )
    {
        (void)g_string_free( slot_info.path, TRUE );
    }
}

void
gnc_sql_slots_load_for_list( GncSqlBackend* be, GList* list )
{
    QofCollection* coll;
    GncSqlStatement* stmt;
    GString* sql;
    GncSqlResult* result;
    gboolean single_item;

    g_return_if_fail( be != NULL );

    // Ignore empty list
    if ( list == NULL ) return;

    coll = qof_instance_get_collection( QOF_INSTANCE(list->data) );

    // Create the query for all slots for all items on the list
    sql = g_string_sized_new( 40 + (GUID_ENCODING_LENGTH + 3) * g_list_length( list ) );
    g_string_append_printf( sql, "SELECT * FROM %s WHERE %s ", TABLE_NAME, obj_guid_col_table[0].col_name );
    if ( g_list_length( list ) != 1 )
    {
        (void)g_string_append( sql, "IN (" );
        single_item = FALSE;
    }
    else
    {
        (void)g_string_append( sql, "= " );
        single_item = TRUE;
    }
    (void)gnc_sql_append_guid_list_to_sql( sql, list, G_MAXUINT );
    if ( !single_item )
    {
        (void)g_string_append( sql, ")" );
    }

    // Execute the query and load the slots
    stmt = gnc_sql_create_statement_from_sql( be, sql->str );
    if ( stmt == NULL )
    {
        PERR( "stmt == NULL, SQL = '%s'\n", sql->str );
        (void)g_string_free( sql, TRUE );
        return;
    }
    (void)g_string_free( sql, TRUE );
    result = gnc_sql_execute_select_statement( be, stmt );
    gnc_sql_statement_dispose( stmt );
    if ( result != NULL )
    {
        GncSqlRow* row = gnc_sql_result_get_first_row( result );

        while ( row != NULL )
        {
            load_slot_for_list_item( be, row, coll );
            row = gnc_sql_result_get_next_row( result );
        }
        gnc_sql_result_dispose( result );
    }
}

static void
load_slot_for_book_object( GncSqlBackend* be, GncSqlRow* row, BookLookupFn lookup_fn )
{
    slot_info_t slot_info = { NULL, NULL, TRUE, NULL, 0, NULL, FRAME, NULL, NULL };
    const GncGUID* guid;
    QofInstance* inst;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( lookup_fn != NULL );

    guid = load_obj_guid( be, row );
    g_return_if_fail( guid != NULL );
    inst = lookup_fn( guid, be->book );
    g_return_if_fail( inst != NULL );

    slot_info.be = be;
    slot_info.pKvpFrame = qof_instance_get_slots( inst );
    slot_info.path = NULL;

    gnc_sql_load_object( be, row, TABLE_NAME, &slot_info, col_table );

    if ( slot_info.path != NULL )
    {
        (void)g_string_free( slot_info.path, TRUE );
    }
}

/**
 * gnc_sql_slots_load_for_sql_subquery - Loads slots for all objects whose guid is
 * supplied by a subquery.  The subquery should be of the form "SELECT DISTINCT guid FROM ...".
 * This is faster than loading for one object at a time because fewer SQL queries * are used.
 *
 * @param be SQL backend
 * @param subquery Subquery SQL string
 * @param lookup_fn Lookup function
 */
void gnc_sql_slots_load_for_sql_subquery( GncSqlBackend* be, const gchar* subquery,
        BookLookupFn lookup_fn )
{
    gchar* sql;
    GncSqlStatement* stmt;
    GncSqlResult* result;

    g_return_if_fail( be != NULL );

    // Ignore empty subquery
    if ( subquery == NULL ) return;

    sql = g_strdup_printf( "SELECT * FROM %s WHERE %s IN (%s)",
                           TABLE_NAME, obj_guid_col_table[0].col_name,
                           subquery );

    // Execute the query and load the slots
    stmt = gnc_sql_create_statement_from_sql( be, sql );
    if ( stmt == NULL )
    {
        PERR( "stmt == NULL, SQL = '%s'\n", sql );
        g_free( sql );
        return;
    }
    g_free( sql );
    result = gnc_sql_execute_select_statement( be, stmt );
    gnc_sql_statement_dispose( stmt );
    if ( result != NULL )
    {
        GncSqlRow* row = gnc_sql_result_get_first_row( result );

        while ( row != NULL )
        {
            load_slot_for_book_object( be, row, lookup_fn );
            row = gnc_sql_result_get_next_row( result );
        }
        gnc_sql_result_dispose( result );
    }
}

/* ================================================================= */
static void
create_slots_tables( GncSqlBackend* be )
{
    gint version;
    gboolean ok;

    g_return_if_fail( be != NULL );

    version = gnc_sql_get_table_version( be, TABLE_NAME );
    if ( version == 0 )
    {
        (void)gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );

        ok = gnc_sql_create_index( be, "slots_guid_index", TABLE_NAME, obj_guid_col_table );
        if ( !ok )
        {
            PERR( "Unable to create index\n" );
        }
    }
    else if ( version < TABLE_VERSION )
    {
        /* Upgrade:
            1->2: 64-bit int values to proper definition, add index
            2->3: Add gdate field
        */
        if ( version == 1 )
        {
            gnc_sql_upgrade_table( be, TABLE_NAME, col_table );
            ok = gnc_sql_create_index( be, "slots_guid_index", TABLE_NAME, obj_guid_col_table );
            if ( !ok )
            {
                PERR( "Unable to create index\n" );
            }
        }
        else if ( version == 2 )
        {
            ok = gnc_sql_add_columns_to_table( be, TABLE_NAME, gdate_col_table );
            if ( !ok )
            {
                PERR( "Unable to add gdate column\n" );
            }
        }
        (void)gnc_sql_set_table_version( be, TABLE_NAME, TABLE_VERSION );
        PINFO("Slots table upgraded from version %d to version %d\n", version, TABLE_VERSION);
    }
}

/* ================================================================= */
void
gnc_sql_init_slots_handler( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_ACCOUNT,
        NULL,                    /* commit - cannot occur */
        NULL,                    /* initial_load - cannot occur */
        create_slots_tables,     /* create_tables */
        NULL,                    /* compile_query */
        NULL,                    /* run_query */
        NULL,                    /* free_query */
        NULL                     /* write */
    };

    (void)qof_object_register_backend( TABLE_NAME, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
