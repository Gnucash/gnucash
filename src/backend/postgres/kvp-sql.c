/********************************************************************\
 * kvp-sql.c : store KVP frames in SQL db                           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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

/*
 * FILE:
 * kvp-sql.c
 *
 * FUNCTION:
 * save & restore of KVP frames
 *
 * HISTORY:
 * Copyright (c) 2001 Linas Vepstas
 */

#include "config.h"
#include <stdlib.h>
#include <string.h>

#include "escape.h"
#include "gnc-engine.h"
#include "kvp-sql.h"
#include "PostgresBackend.h"
#include "putil.h"

static QofLogModule log_module = QOF_MOD_KVP;

/* =========================================================== */
/* get a unique iguid index */

static gpointer
iguid_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    int iguid = atoi (DB_GET_VAL ("iguid", 0));
    guint32 ret = iguid;
    return GUINT_TO_POINTER (ret);
}


guint32
pgendNewGUIDidx (PGBackend *be)
{
    guint32 iguid;
    char * p;

    p = "SELECT nextval('gnc_iguid_seq') AS iguid;";
    SEND_QUERY (be, p, 0);
    iguid = GPOINTER_TO_UINT (pgendGetResults (be, iguid_cb,
                              GUINT_TO_POINTER (0)));
    return iguid;
}

/* =========================================================== */
/* given integer ipath (path id) and a string, poke the string
 * into a cache in local memory
 */

static void
pgendPokePathCache (PGBackend *be, int ipath, const char *path_str)
{
    int i;

    /* get more memory for cache if needed */
    if (ipath >= be->path_cache_size)
    {
        be->path_cache =
            (char **) g_realloc (be->path_cache, (ipath + 100) * sizeof (char *));
        for (i = be->path_cache_size; i < ipath + 100; i++)
        {
            (be->path_cache)[i] = NULL;
        }
        be->path_cache_size = ipath + 100;
    }

    /* poke string into slot ipath */
    if (NULL == (be->path_cache)[ipath])
    {
        (be->path_cache)[ipath] = g_strdup (path_str);
    }

    if (be->ipath_max < ipath) be->ipath_max = ipath;
}

/* =========================================================== */
/* given an integer ipath (path id), return the path string
 * from local memory
 */

static char *
pgendPeekPathCache (PGBackend *be, int ipath)
{
    if (ipath > be->ipath_max) return NULL;
    if (0 >= ipath) return NULL;
    return (be->path_cache)[ipath];
}

/* =========================================================== */
/* utility function, used to access the two cache tables */

static gpointer
ival_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    int ival = atoi (DB_GET_VAL ("ipath", 0));
    return GINT_TO_POINTER(ival);
}


static int
pgendGetCache (PGBackend *be, const char *val_str, sqlEscape *escape)
{
    char *p;
    int ival = 0;

    if (!be || !val_str || !escape) return 0;

    val_str = sqlEscapeString (escape, val_str);

    /* first, lets see if we can find the guid or path.
     * If we can then  just return it */
    p = be->buff;
    *p = 0;
    p = stpcpy (p, "SELECT ipath FROM gncPathCache WHERE path='");
    p = stpcpy (p, val_str);
    p = stpcpy (p, "';");

    SEND_QUERY (be, be->buff, 0);
    ival = GPOINTER_TO_INT(pgendGetResults (be, ival_cb, (gpointer) 0));
    if (ival) return ival;

    /* Else, this guid has never been stored before.
     * Poke it into the the database */

    p = be->buff;
    *p = 0;
    p = stpcpy (p, "INSERT INTO gncPathCache (path) VALUES ('");
    p = stpcpy (p, val_str);
    p = stpcpy (p, "');");

    SEND_QUERY (be, be->buff, 0);
    FINISH_QUERY(be->connection);

    /* and requery to get the serial number ... */
    ival = pgendGetCache (be, val_str, escape);
    return ival;
}

/* =========================================================== */
/* given a string, return the corresponding int from the sql db. */

static int
pgendGetPathCache (PGBackend *be, const char *path_str, sqlEscape *escape)
{
    int ival;
    ival = pgendGetCache (be, path_str, escape);
    PINFO ("cached %d for %s", ival, path_str ? path_str : "(null)");

    if (0 >= ival) return ival;
    pgendPokePathCache (be, ival, path_str);
    return ival;
}

/* =========================================================== */
/* storage of the kvp data to the database is done with the aid
 * of a traversal callback.  The store_cb() routine is the callback.
 */

typedef struct store_data_s
{
    PGBackend *be;
    sqlEscape *escape;
    int iguid;
    int ipath;
    char *path;
    char *stype;
    union
    {
        gint64 ival;
        double dbl;
        gnc_numeric numeric;
        const char *str;
        const GUID *guid;
        Timespec ts;
        GList *list;
    } u;
} store_data_t;

#include "kvp-autogen.h"
#include "kvp-autogen.c"

static void
store_cb (const char *key, KvpValue *val, gpointer p)
{
    store_data_t *cb_data = (store_data_t *) p;
    PGBackend *be = cb_data->be;
    int ipath;
    char *path_save;

    path_save = cb_data->path;
    cb_data->path = g_strjoin ("/", path_save, key, NULL);

    ipath = pgendGetPathCache (be, cb_data->path, cb_data->escape);
    cb_data->ipath = ipath;

    if (ipath)
    {

        switch (kvp_value_get_type (val))
        {
        case KVP_TYPE_GINT64:
        {
            gint64 ival = kvp_value_get_gint64 (val);
            PINFO ("path=%s type=gint64 val=%" G_GINT64_FORMAT,
                   cb_data->path,
                   ival);

            cb_data->stype = "int8";
            cb_data->u.ival = ival;
            pgendPutOneKVPint64Only (be, cb_data);
        }
        break;

        case KVP_TYPE_DOUBLE:
        {
            double ival = kvp_value_get_double (val);
            PINFO ("path=%s type=double val=%g", cb_data->path, ival);

            cb_data->stype = "flt8";
            cb_data->u.dbl = ival;
            pgendPutOneKVPdoubleOnly (be, cb_data);
        }
        break;

        case KVP_TYPE_NUMERIC:
        {
            gnc_numeric ival = kvp_value_get_numeric (val);
            PINFO ("path=%s type=numeric val=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT,
                   cb_data->path,
                   ival.num,
                   ival.denom);

            cb_data->stype = "frac";
            cb_data->u.numeric = ival;
            pgendPutOneKVPnumericOnly (be, cb_data);
        }
        break;

        case KVP_TYPE_STRING:
        {
            const char *str = kvp_value_get_string (val);
            PINFO ("path=%s type=str val=%s", cb_data->path, str);

            cb_data->stype = "text";
            cb_data->u.str = str;
            pgendPutOneKVPstringOnly (be, cb_data);
        }
        break;

        case KVP_TYPE_GUID:
        {
            char guid_str[GUID_ENCODING_LENGTH+1];
            const GUID *guid = kvp_value_get_guid(val);
            guid_to_string_buff (guid, guid_str);
            PINFO ("path=%s type=guid val=%s", cb_data->path, guid_str);

            cb_data->stype = "guid";
            cb_data->u.str = guid_str;
            pgendPutOneKVPguidOnly (be, cb_data);
        }
        break;

        case KVP_TYPE_TIMESPEC:
        {
            PINFO ("path=%s type=timespec", cb_data->path);
            cb_data->stype = "time";
            cb_data->u.ts = kvp_value_get_timespec (val);
            pgendPutOneKVPtimespecOnly (be, cb_data);
        }
        break;

        case KVP_TYPE_BINARY:
            PERR ("Binary KVP Type not yet implemented\n");
            break;

        case KVP_TYPE_GLIST:
        {
            GList *start;
            start = kvp_value_get_glist (val);
            PINFO ("path=%s type=glist", cb_data->path);

            cb_data->stype = "list";
            cb_data->u.list = start;
            PERR ("List KVP Type not yet implemented\n");
        }
        break;

        case KVP_TYPE_FRAME:
        {
            KvpFrame *frame;
            PINFO ("path=%s type=frame", cb_data->path);
            frame = kvp_value_get_frame (val);
            kvp_frame_for_each_slot (frame, store_cb, p);
        }
        break;

        default:
            PERR("Unknown type %d for path=%s\n",
                 kvp_value_get_type (val), cb_data->path);
        }
    }

    g_free (cb_data->path);
    cb_data->path = path_save;
}

void
pgendKVPStore (PGBackend *be, guint32 iguid, KvpFrame *kf)
{
    store_data_t cb_data;
    if (!be || 0 == iguid || !kf) return;
    ENTER (" ");

    cb_data.be = be;
    cb_data.escape = sqlEscape_new ();
    cb_data.iguid = iguid;
    cb_data.path = "";

    kvp_frame_for_each_slot (kf, store_cb, &cb_data);

    sqlEscape_destroy (cb_data.escape);

    LEAVE (" ");
}

/* =========================================================== */
/* These functions suck new, unknown paths out of the database
 * and poke them into our local cache.
 */

static gpointer
path_loader (PGBackend *be, PGresult *result, int j, gpointer data)
{
    int ipath = atoi (DB_GET_VAL ("ipath", j));
    char *path = DB_GET_VAL ("path", j);
    pgendPokePathCache (be, ipath, path);
    return 0;
}

void
pgendKVPInit (PGBackend *be)
{
    char *p;

    /* don't re-init multiple times in single-user mode.
     * Once is enough.  But in multi-user mode, we need to
     * check constantly, since other users may have added
     * more paths.
     */
    if (((MODE_SINGLE_UPDATE == be->session_mode) ||
            (MODE_SINGLE_FILE   == be->session_mode)) &&
            (0 < be->ipath_max)) return;

    /* get new paths out of the database */
    p = be->buff;
    *p = 0;
    p = stpcpy (p, "SELECT * FROM gncPathCache WHERE ipath > ");
    p += sprintf (p, "%d", be->ipath_max);
    p = stpcpy (p, ";");
    SEND_QUERY (be, be->buff, );
    pgendGetResults (be, path_loader, NULL);
}

/* =========================================================== */
/* hack alert -- this code assumed that the path cache
 * is up to date, which it might not be in a multi-user world
 */

#define KVP_HANDLER_SETUP				\
   KvpFrame *kf = (KvpFrame *) data;			\
   KvpFrame *final;					\
   KvpValue * kv=NULL;					\
   char *path, *tail;					\
   int ipath;						\
							\
   ipath = atoi (DB_GET_VAL ("ipath", j));		\
   path = pgendPeekPathCache (be, ipath);		\
   if (!path) return kf;  /* should never happen */	\
   tail = strrchr (path, '/');				\
   *tail = 0x0;						\
   tail ++;						\
							\
   if (!kf) kf = kvp_frame_new();			\
 


#define KVP_HANDLER_TAKEDOWN				\
   final = kvp_frame_get_frame_slash (kf, path);	\
   kvp_frame_set_slot_nc (final, tail, kv);		\
							\
   /* put the slash back */				\
   tail --;						\
   *tail = '/';						\
							\
   return kf;						\
 


static gpointer
int64_handler (PGBackend *be, PGresult *result, int j, gpointer data)
{
    KVP_HANDLER_SETUP;
    kv = kvp_value_new_gint64 (strtoll (DB_GET_VAL ("data", j), NULL, 0));
    KVP_HANDLER_TAKEDOWN;
}

static gpointer
dbl_handler (PGBackend *be, PGresult *result, int j, gpointer data)
{
    KVP_HANDLER_SETUP;
    kv = kvp_value_new_double (atof (DB_GET_VAL ("data", j)));
    KVP_HANDLER_TAKEDOWN;
}

static gpointer
numeric_handler (PGBackend *be, PGresult *result, int j, gpointer data)
{
    gnc_numeric gn;
    KVP_HANDLER_SETUP;
    gn.num = strtoll (DB_GET_VAL ("num", j), NULL, 0);
    gn.denom = strtoll (DB_GET_VAL ("denom", j), NULL, 0);
    kv = kvp_value_new_gnc_numeric (gn);
    KVP_HANDLER_TAKEDOWN;
}


static gpointer
str_handler (PGBackend *be, PGresult *result, int j, gpointer data)
{
    KVP_HANDLER_SETUP;
    kv = kvp_value_new_string (DB_GET_VAL ("data", j));
    KVP_HANDLER_TAKEDOWN;
}


static gpointer
guid_handler (PGBackend *be, PGresult *result, int j, gpointer data)
{
    gboolean rc;
    GUID guid;
    KVP_HANDLER_SETUP;
    rc = string_to_guid ((DB_GET_VAL ("data", j)), &guid);
    if (rc) kv = kvp_value_new_guid (&guid);
    KVP_HANDLER_TAKEDOWN;
}

static gpointer
timespec_handler (PGBackend *be, PGresult *result, int j, gpointer data)
{
    Timespec ts;
    KVP_HANDLER_SETUP;
    ts = gnc_iso8601_to_timespec_gmt (DB_GET_VAL ("data", j));
    kv = kvp_value_new_timespec (ts);
    KVP_HANDLER_TAKEDOWN;
}

static gpointer
list_handler (PGBackend *be, PGresult *result, int j, gpointer data)
{
    KVP_HANDLER_SETUP;
    PERR ("not implemented");
    // kv = kvp_value_new_glist ();
    KVP_HANDLER_TAKEDOWN;
}

#define GET_KVP(TYPE)					\
{							\
   p = be->buff; *p = 0;				\
   p = stpcpy (p, "SELECT * FROM gncKVPValue_" #TYPE " WHERE iguid="); \
   p = stpcpy (p, iguid_str);				\
							\
   SEND_QUERY (be,be->buff, kf);			\
   kf = pgendGetResults (be, TYPE##_handler, kf);	\
}

KvpFrame *
pgendKVPFetch (PGBackend *be, guint32 iguid, KvpFrame *kf)
{
    char * p;
    char iguid_str[40];
    if (!be || 0 == iguid) return kf;

    ENTER (" ");

    /* update the path cache; other users may have added more paths */
    pgendKVPInit (be);

    /* get the effective iguid for this object */
    snprintf (iguid_str, 40, "%d;", iguid);

    /* now troll the individual tables for data */
    GET_KVP(int64);
    GET_KVP(dbl);
    GET_KVP(numeric);
    GET_KVP(str);
    GET_KVP(guid);
    GET_KVP(timespec);
    GET_KVP(list);

    LEAVE (" ");
    return kf;
}

/* =========================================================== */

#define CPY_KVP(TYPE)							\
{									\
   p = stpcpy (p, "INSERT INTO gncKVPValue" TYPE "Trail SELECT '");	\
   p = stpcpy (p, sess_str);						\
   p = stpcpy (p, "' as sessionGuid, now() as date_changed, "	\
                  "'d' as change, 'k' as objtype, ");                   \
   p = stpcpy (p, "* from gncKVPValue" TYPE " WHERE iguid=");           \
   p = stpcpy (p, iguid_str);						\
}

void
pgendKVPDelete (PGBackend *be, guint32 iguid)
{
    char iguid_str[80], sess_str[80];
    char * p;

    if (!be || 0 == iguid)
        return;

    sprintf (iguid_str, "%d;\n", iguid);
    guid_to_string_buff (be->sessionGuid, sess_str);

    /* first, copy values to the audit tables */
    p = be->buff;
    *p = 0;
    CPY_KVP("");
    CPY_KVP("_dbl");
    CPY_KVP("_guid");
    CPY_KVP("_timespec");
    CPY_KVP("_int64");
    CPY_KVP("_list");
    CPY_KVP("_numeric");
    CPY_KVP("_str");

    /* then delete the values */
    p = stpcpy (p, "DELETE FROM gncKVPValue WHERE iguid=");
    p = stpcpy (p, iguid_str);
    p = stpcpy (p, "DELETE FROM gncKVPValue_dbl WHERE iguid=");
    p = stpcpy (p, iguid_str);
    p = stpcpy (p, "DELETE FROM gncKVPValue_guid WHERE iguid=");
    p = stpcpy (p, iguid_str);
    p = stpcpy (p, "DELETE FROM gncKVPValue_timespec WHERE iguid=");
    p = stpcpy (p, iguid_str);
    p = stpcpy (p, "DELETE FROM gncKVPValue_int64 WHERE iguid=");
    p = stpcpy (p, iguid_str);
    p = stpcpy (p, "DELETE FROM gncKVPValue_list WHERE iguid=");
    p = stpcpy (p, iguid_str);
    p = stpcpy (p, "DELETE FROM gncKVPValue_numeric WHERE iguid=");
    p = stpcpy (p, iguid_str);
    p = stpcpy (p, "DELETE FROM gncKVPValue_str WHERE iguid=");
    p = stpcpy (p, iguid_str);

    SEND_QUERY (be, be->buff, );
    FINISH_QUERY(be->connection);
}

/* =========================== END OF FILE ===================== */
