#ifndef GMOCK_GNC_PREFS_H
#define GMOCK_GNC_PREFS_H

#include <gmock/gmock.h>

#include <gnc-prefs.h>


// mock up class implementing preferences backend (see struct PrefBackend in gnc-prefs-p.h)
class MockPrefsBackend
{
public:
    MOCK_METHOD2(get_bool, gboolean(const gchar *, const gchar *));
    MOCK_METHOD2(get_int, gint(const gchar *, const gchar *));
    MOCK_METHOD2(get_int64, gint64(const gchar *, const gchar *));
    MOCK_METHOD2(get_float, gdouble(const gchar *, const gchar *));
    MOCK_METHOD2(get_string, gchar*(const gchar *, const gchar *));
    MOCK_METHOD2(get_enum, gint(const gchar *, const gchar *));
    MOCK_METHOD4(get_coords, void(const gchar *, const gchar *, gdouble *, gdouble *));
};

/** Define a preferences backend.
 *
 * \attention Each call to this function overwrites a previously set backend.
 */
void gmock_gnc_prefs_set_backend(MockPrefsBackend *backend);

#endif
