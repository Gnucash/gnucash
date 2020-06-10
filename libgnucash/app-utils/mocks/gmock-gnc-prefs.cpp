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
    return prefsbackend->getBool(group, pref_name);
}

gint
gnc_prefs_get_int (const gchar *group, const gchar *pref_name)
{
    return prefsbackend->getInt(group, pref_name);
}

gint64
gnc_prefs_get_int64 (const gchar *group, const gchar *pref_name)
{
    return prefsbackend->getInt64(group, pref_name);
}

gdouble
gnc_prefs_get_float (const gchar *group, const gchar *pref_name)
{
    return prefsbackend->getFloat(group, pref_name);
}

gchar *
gnc_prefs_get_string (const gchar *group, const gchar *pref_name)
{
    return prefsbackend->getString(group, pref_name);
}

gint
gnc_prefs_get_enum (const gchar *group, const gchar *pref_name)
{
    return prefsbackend->getEnum(group, pref_name);
}

void
gnc_prefs_get_coords (const gchar *group, const gchar *pref_name, gdouble *x, gdouble *y)
{
    prefsbackend->getCoords(group, pref_name, x, y);
}
