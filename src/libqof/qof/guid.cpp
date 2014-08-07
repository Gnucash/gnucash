/********************************************************************\
 * guid.c -- globally unique ID implementation                      *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
 * Copyright (C) 2014 Aaron Laws <dartmetrash@gmail.com>            *
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

extern "C"
{

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#include <ctype.h>
#include <stdint.h>
#ifdef HAVE_DIRENT_H
# include <dirent.h>
#endif
#include <glib.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif
#include <time.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include "qof.h"

}
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <sstream>
#include <string>

using namespace std;

typedef boost::uuids::uuid gg;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = QOF_MOD_ENGINE;

/**
 * gnc_value_get_guid
 *
 * @param value a @c GValue whose value we want to get.
 *
 * @return the value stored in @a value
 */
const GncGUID*
gnc_value_get_guid (const GValue *value)
{
    GncGUID *val;

    g_return_val_if_fail (value && G_IS_VALUE (value), NULL);
    g_return_val_if_fail (GNC_VALUE_HOLDS_GUID (value), NULL);

    val = (GncGUID*) g_value_get_boxed (value);

    return val;
}

static GncGUID * nullguid {reinterpret_cast<GncGUID*> (new boost::uuids::uuid{{0}})};

/*It looks like we are expected to provide the same pointer every time from this function*/
const GncGUID *
guid_null (void)
{
    return nullguid;
}

/* Memory management routines ***************************************/

GncGUID *
guid_malloc (void)
{
    /*Note, the Boost uuid is a POD, so its constructor is trivial*/
    return reinterpret_cast<GncGUID*> (new boost::uuids::uuid);
}

void
guid_free (GncGUID *guid)
{
    if (!guid)
        return;
    if (guid == nullguid)
        /*!!Don't delete that!!*/
        return;
    delete reinterpret_cast<boost::uuids::uuid*> (guid);
    guid = nullptr;
}


GncGUID *
guid_copy (const GncGUID *guid)
{
    const boost::uuids::uuid * old {reinterpret_cast<const boost::uuids::uuid*> (guid)};
    boost::uuids::uuid * ret {new boost::uuids::uuid (*old)};
    return reinterpret_cast<GncGUID*> (ret);
}

/*Takes an allocated guid pointer and constructs it in place*/
void
guid_replace (GncGUID *guid)
{
    static boost::uuids::random_generator gen;
    boost::uuids::uuid * val {reinterpret_cast<boost::uuids::uuid*> (guid)};
    val->boost::uuids::uuid::~uuid ();
    boost::uuids::uuid temp (gen ());
    val->swap (temp);
}

GncGUID *
guid_new (void)
{
    GncGUID * ret {guid_malloc ()};
    guid_replace (ret);
    return ret;
}

GncGUID
guid_new_return (void)
{
    /*we need to construct our value as a boost guid so that
    it can be deconstructed (in place) in guid_replace*/
    boost::uuids::uuid guid;
    GncGUID * ret {reinterpret_cast<GncGUID*> (&guid)};
    guid_replace (ret);
    /*return a copy*/
    return *ret;
}

gchar *
guid_to_string (const GncGUID * guid)
{
    /* We need to malloc here, not 'new' because it will be freed
    by the caller which will use free (not delete).*/
    gchar * ret {reinterpret_cast<gchar*> (g_malloc (sizeof (gchar)*GUID_ENCODING_LENGTH+1))};
    gchar * temp {guid_to_string_buff (guid, ret)};
    if (!temp){
        g_free (ret);
        return nullptr;
    }
    return ret;
}

gchar *
guid_to_string_buff (const GncGUID * guid, gchar *str)
{
    if (!str || !guid) return NULL;

    boost::uuids::uuid const & tempg = *reinterpret_cast<boost::uuids::uuid const *> (guid);
    unsigned destspot {0};
    string const & val {to_string (tempg)};
    for (auto val_char : val)
        if (val_char != '-')
            str [destspot++] = val_char;

    str[GUID_ENCODING_LENGTH] = '\0';
    return &str[GUID_ENCODING_LENGTH];
}

gboolean
string_to_guid (const char * str, GncGUID * guid)
{
    if (!guid || !str)
        return false;

    try 
    {
        static boost::uuids::string_generator strgen;
        boost::uuids::uuid * converted {reinterpret_cast<boost::uuids::uuid*> (guid)};
        new (converted) boost::uuids::uuid (strgen (str));
    }
    catch (...)
    {
        return false;
    }
    return true;
}

gboolean
guid_equal (const GncGUID *guid_1, const GncGUID *guid_2)
{
    if (!guid_1 || !guid_2)
        return false;
    boost::uuids::uuid const * g1 {reinterpret_cast<boost::uuids::uuid const *> (guid_1)};
    boost::uuids::uuid const * g2 {reinterpret_cast<boost::uuids::uuid const *> (guid_2)};
    return *g1 == *g2;
}

gint
guid_compare (const GncGUID *guid_1, const GncGUID *guid_2)
{
    boost::uuids::uuid const * g1 {reinterpret_cast<boost::uuids::uuid const *> (guid_1)};
    boost::uuids::uuid const * g2 {reinterpret_cast<boost::uuids::uuid const *> (guid_2)};
    if (*g1 < *g2)
        return -1;
    if (*g1 == *g2)
        return 0;
    return 1;
}

guint
guid_hash_to_guint (gconstpointer ptr)
{
    const boost::uuids::uuid * guid = reinterpret_cast<const boost::uuids::uuid*> (ptr);

    if (!guid)
    {
        PERR ("received NULL guid pointer.");
        return 0;
    }

    guint hash {0};
    unsigned retspot {0};
    for (auto guidspot : *guid)
    {
        hash <<= 4;
        hash |= guidspot;
    }
    return hash;
}

gint
guid_g_hash_table_equal (gconstpointer guid_a, gconstpointer guid_b)
{
    return guid_equal (reinterpret_cast<const GncGUID*> (guid_a),
		       reinterpret_cast<const GncGUID*> (guid_b));
}

GHashTable *
guid_hash_table_new (void)
{
    return g_hash_table_new (guid_hash_to_guint, guid_g_hash_table_equal);
}

/***************************/
static void
gnc_string_to_guid (const GValue *src, GValue *dest)
{
    /* FIXME: add more checks*/
    GncGUID *guid;
    const gchar *as_string;

    g_return_if_fail (G_VALUE_HOLDS_STRING (src) &&
                      GNC_VALUE_HOLDS_GUID (dest));

    as_string = g_value_get_string (src);

    guid = g_new0 (GncGUID, 1);
    string_to_guid (as_string, guid);

    g_value_take_boxed (dest, guid);
}

static void
gnc_guid_to_string (const GValue *src, GValue *dest)
{
    const gchar *str;

    g_return_if_fail (G_VALUE_HOLDS_STRING (dest) &&
                      GNC_VALUE_HOLDS_GUID (src));

    str = guid_to_string(gnc_value_get_guid (src));

    g_value_set_string (dest, str);
}

GType
gnc_guid_get_type (void)
{
    static GType type = 0;

    if (G_UNLIKELY (type == 0))
    {
        type = g_boxed_type_register_static ("GncGUID",
                                             (GBoxedCopyFunc)guid_copy,
                                             (GBoxedFreeFunc)guid_free);

        g_value_register_transform_func (G_TYPE_STRING,
                                         type,
                                         gnc_string_to_guid);

        g_value_register_transform_func (type,
                                         G_TYPE_STRING,
                                         gnc_guid_to_string);
    }

    return type;
}
