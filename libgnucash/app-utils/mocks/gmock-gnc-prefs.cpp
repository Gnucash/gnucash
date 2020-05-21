#include <config.h>

#include "gmock-gnc-prefs.h"

PrefsBackend* prefsbackend = NULL;

gboolean
gnc_prefs_get_bool (const gchar *group, const gchar *pref_name)
{
    return ((MockPrefsBackend*)prefsbackend)->getBool(group, pref_name);
}

gint
gnc_prefs_get_int (const gchar *group, const gchar *pref_name)
{
    return ((MockPrefsBackend*)prefsbackend)->getInt(group, pref_name);
}

gint64
gnc_prefs_get_int64 (const gchar *group, const gchar *pref_name)
{
    return ((MockPrefsBackend*)prefsbackend)->getInt64(group, pref_name);
}

gdouble
gnc_prefs_get_float (const gchar *group, const gchar *pref_name)
{
    return ((MockPrefsBackend*)prefsbackend)->getFloat(group, pref_name);
}

gchar *
gnc_prefs_get_string (const gchar *group, const gchar *pref_name)
{
    return ((MockPrefsBackend*)prefsbackend)->getString(group, pref_name);
}

gint
gnc_prefs_get_enum (const gchar *group, const gchar *pref_name)
{
    return ((MockPrefsBackend*)prefsbackend)->getEnum(group, pref_name);
}

void
gnc_prefs_get_coords (const gchar *group, const gchar *pref_name, gdouble *x, gdouble *y)
{
    ((MockPrefsBackend*)prefsbackend)->getCoords(group, pref_name, x, y);
}
