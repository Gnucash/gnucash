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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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

#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>

#include "kvp-sql.h"
#include "PostgresBackend.h"
#include "putil.h"

static short module = MOD_BACKEND; 

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
         (char **) g_realloc (be->path_cache, (ipath+100)*sizeof (char *));
      for (i=be->path_cache_size; i<ipath+100; i++) {
        (be->path_cache)[i] = NULL;
      }
      be->path_cache_size = ipath+100;
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
  int ival = atoi (DB_GET_VAL ((char *)data, 0));
  return (gpointer) ival;
}


static int
pgendGetCache (PGBackend *be, 
               const char *table_name,
               const char *key_name,
               const char *cache_name,
               const char *val_str)
{
   char *p;
   int ival =0;

   if (!be || !val_str) return 0;

   /* first, lets see if we can find the guid. 
    * If we can then  just return it */
   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT ");
   p = stpcpy (p, cache_name);
   p = stpcpy (p, " FROM ");
   p = stpcpy (p, table_name);
   p = stpcpy (p, " WHERE ");
   p = stpcpy (p, key_name);
   p = stpcpy (p, " ='");
   p = stpcpy (p, val_str);
   p = stpcpy (p, "';");

   SEND_QUERY (be,be->buff, 0);
   ival = (int) pgendGetResults (be, ival_cb, (gpointer) cache_name);
   if (ival && (ival != (int)cache_name)) return ival;

   /* Else, this guid has never been stored before. 
    * Poke it into the the database */

   p = be->buff; *p = 0;
   p = stpcpy (p, "INSERT INTO ");
   p = stpcpy (p, table_name);
   p = stpcpy (p, " (");
   p = stpcpy (p, key_name);
   p = stpcpy (p, ") VALUES ('");
   p = stpcpy (p, val_str);
   p = stpcpy (p, "');");

   SEND_QUERY (be,be->buff, 0);
   FINISH_QUERY(be->connection);

   /* and requery to get the serial number ... */
   ival = pgendGetCache (be, table_name, key_name, cache_name, val_str);
   return ival;
}

/* =========================================================== */
/* given a string, return the corresponding int from the sql db. */

static int
pgendGetPathCache (PGBackend *be, const char *path_str)
{
   int ival;
   ival = pgendGetCache (be, "gncPathCache", "path", "ipath", path_str);
   PINFO ("cached %d for %s", ival, path_str ? path_str : "(null)");

   if (0 >= ival) return ival;
   pgendPokePathCache (be, ival, path_str);
   return ival;
}

/* =========================================================== */
/* given a string, return the corresponding int from the sql db. */

static int
pgendGetGUIDCacheIDStr (PGBackend *be, const char *guid_str)
{
   int ival;
   ival = pgendGetCache (be, "gncGUIDCache", "guid", "iguid", guid_str);
   PINFO ("cached %d for %s", ival, guid_str ? guid_str : "(null)");
   return ival;
}

/* =========================================================== */
/* given a guid, return the corresponding int from the sql db. */

static int
pgendGetGUIDCacheID (PGBackend *be, const GUID *guid)
{
   char guid_str[GUID_ENCODING_LENGTH+1];
   if (!be || !guid_str) return 0;

   guid_to_string_buff (guid, guid_str);
   return pgendGetGUIDCacheIDStr (be, guid_str);
}

/* =========================================================== */
/* storage of the kvp data to the database is done with the aid
 * of a traversal callback.  The store_cb() routine is the callback.
 */

typedef struct store_data_s {
   PGBackend *be;
   int iguid;
   int ipath;
   char *path;
   char *stype;
   union {
      gint64 ival;
      double dbl;
      gnc_numeric numeric;
      const char *str;
      const GUID *guid;
      GList *list;
   } u;
   
} store_data_t;

#include "kvp-autogen.c"

static void 
store_cb (const char *key, kvp_value *val, gpointer p)
{
   store_data_t *cb_data = (store_data_t *) p;
   PGBackend *be = cb_data->be;
   int ipath;
   char *path_save;

   path_save = cb_data->path;
   cb_data->path = g_strjoin ("/", path_save, key, 0);

   ipath = pgendGetPathCache (be, cb_data->path);
   cb_data->ipath = ipath;

   if (ipath)
   {

      switch (kvp_value_get_type (val))
      {
         case KVP_TYPE_GINT64:
            {
               gint64 ival = kvp_value_get_gint64 (val);
               PINFO ("path=%s type=gint64 val=%lld",
                      cb_data->path,
                      (long long int) ival);

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
               PINFO ("path=%s type=numeric val=%lld/%lld",
                      cb_data->path,
                      (long long int) ival.num,
                      (long long int) ival.denom);

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
               kvp_frame *frame;
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
pgendKVPStore (PGBackend *be, const GUID *guid, kvp_frame *kf)
{
   store_data_t cb_data;
   int iguid;
   if (!be || !guid || !kf) return;
   ENTER (" ");

   iguid = pgendGetGUIDCacheID (be, guid);
   if (0 == iguid) return;

   cb_data.be = be;
   cb_data.iguid = iguid;
   cb_data.path = "";

   kvp_frame_for_each_slot (kf, store_cb, &cb_data);
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

   p = be->buff; *p=0;
   p = stpcpy (p, "SELECT * FROM gncPathCache WHERE ipath > ");
   p += sprintf (p, "%d", be->ipath_max);
   p = stpcpy (p, ";");
   SEND_QUERY (be,be->buff, );
   pgendGetResults (be, path_loader, NULL);
}

/* =========================================================== */
/* hack alert -- this code assumed that the path cache
 * is up to date, which it might not be in a multi-user world 
 */

#define KVP_HANDLER_SETUP				\
   kvp_frame *kf = (kvp_frame *) data;			\
   kvp_frame *final;					\
   kvp_value * kv=NULL;					\
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
   kv = kvp_value_new_gint64 (atoll (DB_GET_VAL ("data", j)));
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
   gn.num = atoll (DB_GET_VAL ("num", j));
   gn.denom = atoll (DB_GET_VAL ("denom", j));
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

static gpointer 
count_handler (PGBackend *be, PGresult *result, int j, gpointer data)
{
   int *cnt = (int *) data;
   *cnt += atoi (DB_GET_VAL ("count", j));
   return data;
}


kvp_frame * 
pgendKVPFetch (PGBackend *be, const GUID *guid, kvp_frame *kf)
{
   char * p;
   char iguid_str[40];
   int iguid = 0;
   int count = 0;
   if (!be || !guid) return kf;

   ENTER (" ");

   /* update the path cache; other users may have added more paths */
   pgendKVPInit (be);   

   /* get the effective iguid for this object */
   iguid = pgendGetGUIDCacheID (be, guid);
   if (0 == iguid) return kf;
   snprintf (iguid_str, 40, "%d;", iguid);

   /* save on some sql queries by avoiding kvp data fetches when 
    * tehre is no data */
   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT count(*) FROM gncKVPValue WHERE iguid="); 
   p = stpcpy (p, iguid_str);
   SEND_QUERY (be,be->buff, kf);
   pgendGetResults (be, count_handler, &count);	
   if (0 == count) return kf;

   /* now troll the individual tables for data */
   GET_KVP(int64);
   GET_KVP(dbl);
   GET_KVP(numeric);
   GET_KVP(str);
   GET_KVP(guid);
   GET_KVP(list);

   LEAVE (" ");
   return kf;
}

/* =========================================================== */

#define CPY_KVP(TYPE)							\
{									\
   p = stpcpy (p, "INSERT INTO gncKVPValue" TYPE "Trail SELECT '");	\
   p = stpcpy (p, sess_str);						\
   p = stpcpy (p, "' as sessionGuid, datetime('NOW') as date_changed, "	\
                  "'d' as change, * from gncKVPValue" TYPE " WHERE iguid=");\
   p = stpcpy (p, iguid_str);						\
}

void 
pgendKVPDeleteStr (PGBackend *be, const char *guid)
{
   char iguid_str[80], sess_str[80];
   char * p;
   int iguid = 0;
   if (!be || !guid) return;

   iguid = pgendGetGUIDCacheIDStr (be, guid);
   if (0 == iguid) return;

   sprintf (iguid_str, "%d;\n", iguid);
   guid_to_string_buff (be->sessionGuid, sess_str);

   /* first, copy values to the audit tables */
   p = be->buff; *p = 0;
   CPY_KVP("");
   CPY_KVP("_dbl");
   CPY_KVP("_guid");
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
   p = stpcpy (p, "DELETE FROM gncKVPValue_int64 WHERE iguid=");
   p = stpcpy (p, iguid_str);
   p = stpcpy (p, "DELETE FROM gncKVPValue_list WHERE iguid=");
   p = stpcpy (p, iguid_str);
   p = stpcpy (p, "DELETE FROM gncKVPValue_numeric WHERE iguid=");
   p = stpcpy (p, iguid_str);
   p = stpcpy (p, "DELETE FROM gncKVPValue_str WHERE iguid=");
   p = stpcpy (p, iguid_str);

   SEND_QUERY (be,be->buff, );
   FINISH_QUERY(be->connection);

}

/* =========================================================== */

void 
pgendKVPDelete (PGBackend *be, const GUID *guid)
{
   char guid_str[33];
   if (!be || !guid) return;

   guid_to_string_buff (guid, guid_str);
   return pgendKVPDeleteStr (be, guid_str);
}

/* =========================== END OF FILE ===================== */
