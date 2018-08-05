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

#include "guid.hpp"
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
    if (!value) return nullptr;
    GncGUID *val;

    g_return_val_if_fail (value && G_IS_VALUE (value), NULL);
    g_return_val_if_fail (GNC_VALUE_HOLDS_GUID (value), NULL);

    val = (GncGUID*) g_value_get_boxed (value);

    return val;
}

GncGUID * guid_convert_create (gnc::GUID const &);

static gnc::GUID s_null_guid {boost::uuids::uuid { {0}}};
static GncGUID * s_null_gncguid {guid_convert_create (s_null_guid)};

/* Memory management routines ***************************************/

/**
 * Allocates and returns a new GncGUID containing the same value as the
 * gnc::GUID passed in.
 */
GncGUID *
guid_convert_create (gnc::GUID const & guid)
{
    GncGUID temp = guid;
    return guid_copy (&temp);
}

GncGUID *
guid_malloc (void)
{
    return new GncGUID;
}

void
guid_free (GncGUID *guid)
{
    if (!guid) return;
    if (guid == s_null_gncguid)
        /* Don't delete that! */
        return;
    delete guid;
}

GncGUID *
guid_copy (const GncGUID *guid)
{
    if (!guid) return nullptr;
    auto ret = guid_malloc ();
    memcpy (ret, guid, sizeof (GncGUID));
    return ret;
}

/*It looks like we are expected to provide the same pointer every time from this function*/
const GncGUID *
guid_null (void)
{
    return s_null_gncguid;
}

static void
guid_assign (GncGUID & target, gnc::GUID const & source)
{
    memcpy (&target, &source, sizeof (GncGUID));
}

/*Takes an allocated guid pointer and constructs it in place*/
void
guid_replace (GncGUID *guid)
{
    if (!guid) return;
    gnc::GUID temp_random {gnc::GUID::create_random ()};
    guid_assign (*guid, temp_random);
}

GncGUID *
guid_new (void)
{
    auto ret = guid_new_return ();
    return guid_copy (&ret);
}

GncGUID
guid_new_return (void)
{
    return gnc::GUID::create_random ();
}

gchar *
guid_to_string (const GncGUID * guid)
{
    if (!guid) return nullptr;
    gnc::GUID temp {*guid};
    auto temp_str = temp.to_string ();
    return g_strdup (temp_str.c_str ());
}

gchar *
guid_to_string_buff (const GncGUID * guid, gchar *str)
{
    if (!str || !guid) return NULL;

    gnc::GUID temp {*guid};
    auto val = temp.to_string ();
    /*We need to be sure to copy the terminating null character.
     * The standard guarantees that std::basic_string::c_str ()
     * returns with a terminating null character, too.*/
    std::copy (val.c_str (), val.c_str () + val.size () + 1, str);
    return str + val.size ();
}

gboolean
string_to_guid (const char * str, GncGUID * guid)
{
    if (!guid || !str) return false;

    try
    {
        guid_assign (*guid, gnc::GUID::from_string (str));
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
        return !guid_1 && !guid_2;
    gnc::GUID temp1 {*guid_1};
    gnc::GUID temp2 {*guid_2};
    return temp1 == temp2;
}

gint
guid_compare (const GncGUID *guid_1, const GncGUID *guid_2)
{
    if (!guid_1 || !guid_2)
        return !guid_1 && !guid_2;
    gnc::GUID temp1 {*guid_1};
    gnc::GUID temp2 {*guid_2};
    if (temp1 < temp2)
        return -1;
    if (temp1 == temp2)
        return 0;
    return 1;
}

guint
guid_hash_to_guint (gconstpointer ptr)
{
    if (!ptr)
    {
        PERR ("received NULL guid pointer.");
        return 0;
    }
    GncGUID const & guid = * reinterpret_cast <GncGUID const *> (ptr);
    gnc::GUID const & temp {guid};

    guint hash {0};
    unsigned retspot {0};
    std::for_each (temp.begin (), temp.end (), [&hash] (unsigned char a) {
        hash <<=4;
        hash |= a;
    });
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

    str = guid_to_string (gnc_value_get_guid (src));

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

namespace gnc
{

GUID
GUID::create_random () noexcept
{
    static boost::uuids::random_generator gen;
    return {gen ()};
}

GUID::GUID (boost::uuids::uuid const & other) noexcept
    : implementation (other)
{
}

GUID const &
GUID::null_guid () noexcept
{
    return s_null_guid;
}

std::string
GUID::to_string () const noexcept
{
    auto const & val = boost::uuids::to_string (implementation);
    std::string ret;
    std::for_each (val.begin (), val.end (), [&ret] (char a) {
        if (a != '-') ret.push_back (a);
    });
    return ret;
}

GUID
GUID::from_string (std::string const & str)
{
    try
    {
        static boost::uuids::string_generator strgen;
        return strgen (str);
    }
    catch (...)
    {
        throw guid_syntax_exception {};
    }
}

bool
GUID::is_valid_guid (std::string const & str)
{
    try
    {
        static boost::uuids::string_generator strgen;
        auto a = strgen (str);
        return true;
    }
    catch (...)
    {
        return false;
    }
}

guid_syntax_exception::guid_syntax_exception () noexcept
    : invalid_argument {"Invalid syntax for guid."}
{
}

GUID::GUID (GncGUID const & other) noexcept
: implementation {{other.reserved[0] , other.reserved[1]
            , other.reserved[2], other.reserved[3]
            , other.reserved[4], other.reserved[5]
            , other.reserved[6], other.reserved[7]
            , other.reserved[8], other.reserved[9]
            , other.reserved[10], other.reserved[11]
            , other.reserved[12], other.reserved[13]
            , other.reserved[14], other.reserved[15]}
    }
{

}

auto
GUID::end () const noexcept -> decltype (implementation.end ())
{
    return implementation.end ();
}

auto
GUID::begin () const noexcept -> decltype (implementation.begin ())
{
    return implementation.begin ();
}

bool
GUID::operator < (GUID const & other) noexcept
{
    return implementation < other.implementation;
}

bool operator == (GUID const & lhs, GncGUID const & rhs) noexcept
{
    return lhs.implementation == GUID(rhs).implementation;
}

bool
operator != (GUID const & one, GUID const & two) noexcept
{
    return one.implementation != two.implementation;
}

GUID & GUID::operator = (GUID && other) noexcept
{
    boost::uuids::swap (other.implementation, implementation);
    return *this;
}

GUID::operator GncGUID () const noexcept
{
    GncGUID ret;
    guid_assign (ret, *this);
    return ret;
}

} // namespace gnc
