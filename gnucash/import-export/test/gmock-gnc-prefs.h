#ifndef GMOCK_GNC_PREFS_H
#define GMOCK_GNC_PREFS_H

#include <gmock/gmock.h>

extern "C"
{
#include <gnc-prefs.h>
#include <gnc-prefs-p.h>
}


// mock up for PrefsBackend (singleton class)
class MockPrefsBackend : PrefsBackend
{
public:
    MockPrefsBackend(MockPrefsBackend const&) = delete;
    MockPrefsBackend& operator=(MockPrefsBackend const&) = delete;

    static MockPrefsBackend* getInstance()
    {
        static MockPrefsBackend prefs;  // preferences object

        // register preferences object
        if (prefsbackend == NULL)
            prefsbackend = (PrefsBackend*)&prefs;

        // check that preferences object is correctly registered
        EXPECT_EQ((MockPrefsBackend*)prefsbackend, &prefs);

        return &prefs;
    }

    MOCK_METHOD2(getBool, gboolean(const gchar *, const gchar *));
    MOCK_METHOD2(getInt, gint(const gchar *, const gchar *));
    MOCK_METHOD2(getInt64, gint64(const gchar *, const gchar *));
    MOCK_METHOD2(getFloat, gdouble(const gchar *, const gchar *));
    MOCK_METHOD2(getString, gchar*(const gchar *, const gchar *));
    MOCK_METHOD2(getEnum, gint(const gchar *, const gchar *));
    MOCK_METHOD4(getCoords, void(const gchar *, const gchar *, gdouble *, gdouble *));

private:
    MockPrefsBackend() {}
    ~MockPrefsBackend() {}
};

#endif
