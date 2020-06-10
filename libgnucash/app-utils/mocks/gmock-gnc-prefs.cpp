#include <config.h>

#include "gmock-gnc-prefs.h"

static MockPrefsBackend* prefsbackend = nullptr;

void
gmock_gnc_prefs_set_backend(MockPrefsBackend *backend)
{
    prefsbackend = backend;
}

gboolean
gnc_prefs_get_bool (const gchar *group, const gchar *pref_name)
{
    EXPECT_NE(prefsbackend, nullptr);
    return prefsbackend ? prefsbackend->getBool(group, pref_name) : FALSE;
}

gint
gnc_prefs_get_int (const gchar *group, const gchar *pref_name)
{
    EXPECT_NE(prefsbackend, nullptr);
    return prefsbackend ? prefsbackend->getInt(group, pref_name) : 0;
}

gint64
gnc_prefs_get_int64 (const gchar *group, const gchar *pref_name)
{
    EXPECT_NE(prefsbackend, nullptr);
    return prefsbackend ? prefsbackend->getInt64(group, pref_name) : 0;
}

gdouble
gnc_prefs_get_float (const gchar *group, const gchar *pref_name)
{
    EXPECT_NE(prefsbackend, nullptr);
    return prefsbackend ? prefsbackend->getFloat(group, pref_name) : 0.0;
}

gchar *
gnc_prefs_get_string (const gchar *group, const gchar *pref_name)
{
    EXPECT_NE(prefsbackend, nullptr);
    return prefsbackend ? prefsbackend->getString(group, pref_name) : NULL;
}

gint
gnc_prefs_get_enum (const gchar *group, const gchar *pref_name)
{
    EXPECT_NE(prefsbackend, nullptr);
    return prefsbackend ? prefsbackend->getEnum(group, pref_name) : 0;
}

void
gnc_prefs_get_coords (const gchar *group, const gchar *pref_name, gdouble *x, gdouble *y)
{
    EXPECT_NE(prefsbackend, nullptr);

    *x = 0.0;
    *y = 0.0;

    if (prefsbackend != nullptr)
        prefsbackend->getCoords(group, pref_name, x, y);
}
