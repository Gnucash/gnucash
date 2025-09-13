#include <glib.h>

#include <config.h>
#include <gncla-dir.h>
#include <gnc-path.h>
#include <binreloc.h>
#include <gnc-filepath-utils.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop


/* Variant of EXPECT_STREQ that calls g_free()
 * on its first argument after the check */
#define EXPECT_STREQ_GFREE(a, b) do { char *p_; EXPECT_STREQ(p_ = (a), (b)); g_free(p_); } while (0)


struct PathTest : public testing::Test
{
    PathTest() : m_prefix{nullptr} {}
    void SetUp()
    {
#ifdef ENABLE_BINRELOC
        gnc_gbr_init(nullptr);
#endif
        char *builddir = g_strdup(g_getenv("GNC_BUILDDIR"));
        if (builddir)
            m_prefix = builddir;
        else
            m_prefix = g_get_current_dir();
    }
    void TearDown()
    {
        if (m_prefix)
            g_free(m_prefix);
        /* Clear the statically allocated exe string */
        gnc_gbr_set_exe(NULL);
    }
    char *m_prefix;
};


TEST_F(PathTest, gnc_path_get_prefix)
{
#ifdef ENABLE_BINRELOC
    EXPECT_STREQ_GFREE(gnc_path_get_prefix(), m_prefix);
#else
    g_setenv("GNC_UNINSTALLED", "1", TRUE);
    g_setenv("GNC_BUILDDIR", m_prefix, 1);
    EXPECT_STREQ_GFREE(gnc_path_get_prefix(), m_prefix);
    g_unsetenv("GNC_UNINSTALLED");
    g_unsetenv("GNC_BUILDDIR");
    EXPECT_STREQ_GFREE(gnc_path_get_prefix(), PREFIX);
#endif
}

TEST_F(PathTest, gnc_path_get_bindir)
{
    gchar *dirname = gnc_file_path_relative_part(PREFIX, BINDIR);
    gchar *binpath = g_build_filename(m_prefix, dirname, NULL);
    g_free(dirname);
#ifdef ENABLE_BINRELOC
    EXPECT_STREQ_GFREE(gnc_path_get_bindir(), binpath);
    g_free(binpath);
#else
    g_setenv("GNC_UNINSTALLED", "1", TRUE);
    g_setenv("GNC_BUILDDIR", m_prefix, 1);
    EXPECT_STREQ_GFREE(gnc_path_get_bindir(), binpath);
    g_free(binpath);
    g_unsetenv("GNC_UNINSTALLED");
    g_unsetenv("GNC_BUILDDIR");
    EXPECT_STREQ_GFREE(gnc_path_get_bindir(), BINDIR);
#endif
}

TEST_F(PathTest, gnc_path_get_libdir)
{
    gchar *dirname = gnc_file_path_relative_part(PREFIX, LIBDIR);
    gchar *libpath = g_build_filename(m_prefix, dirname, NULL);
    g_free(dirname);
#ifdef ENABLE_BINRELOC
    EXPECT_STREQ_GFREE(gnc_path_get_libdir(), libpath);
    g_free(libpath);
#else
    g_setenv("GNC_UNINSTALLED", "1", TRUE);
    g_setenv("GNC_BUILDDIR", m_prefix, 1);
    EXPECT_STREQ_GFREE(gnc_path_get_libdir(), libpath);
    g_free(libpath);
    g_unsetenv("GNC_UNINSTALLED");
    g_unsetenv("GNC_BUILDDIR");
    EXPECT_STREQ_GFREE(gnc_path_get_libdir(), LIBDIR);
#endif
}

TEST_F(PathTest, gnc_path_get_datadir)
{
    gchar *dirname = gnc_file_path_relative_part(PREFIX, DATADIR);
    gchar *datapath = g_build_filename(m_prefix, dirname, NULL);
    g_free(dirname);
#ifdef ENABLE_BINRELOC
    EXPECT_STREQ_GFREE(gnc_path_get_datadir(), datapath);
    g_free(datapath);
#else
    g_setenv("GNC_UNINSTALLED", "1", TRUE);
    g_setenv("GNC_BUILDDIR", m_prefix, 1);
    EXPECT_STREQ_GFREE(gnc_path_get_datadir(), datapath);
    g_free(datapath);
    g_unsetenv("GNC_UNINSTALLED");
    g_unsetenv("GNC_BUILDDIR");
    EXPECT_STREQ_GFREE(gnc_path_get_datadir(), DATADIR);
#endif
}

TEST_F(PathTest, gnc_path_get_sysconfdir)
{
    gchar *dirname = gnc_file_path_relative_part(PREFIX, SYSCONFDIR);
    gchar *sysconfpath = g_build_filename(m_prefix, dirname, PROJECT_NAME, NULL);
    g_free(dirname);
#ifdef ENABLE_BINRELOC
    EXPECT_STREQ_GFREE(gnc_path_get_pkgsysconfdir(), sysconfpath);
    g_free(sysconfpath);
#else
    g_setenv("GNC_UNINSTALLED", "1", TRUE);
    g_setenv("GNC_BUILDDIR", m_prefix, 1);
    EXPECT_STREQ_GFREE(gnc_path_get_pkgsysconfdir(), sysconfpath);
    g_free(sysconfpath);
    g_unsetenv("GNC_UNINSTALLED");
    g_unsetenv("GNC_BUILDDIR");
    sysconfpath = g_build_filename(SYSCONFDIR, PROJECT_NAME, NULL);
    EXPECT_STREQ_GFREE(gnc_path_get_pkgsysconfdir(), sysconfpath);
    g_free(sysconfpath);
#endif
}

TEST_F (PathTest, gnc_filename_is_backup)
{
    EXPECT_EQ (gnc_filename_is_backup (""), false);
    EXPECT_EQ (gnc_filename_is_backup ("a.gnucash"), false);
    EXPECT_EQ (gnc_filename_is_backup ("a.gnucash.20201131010203.gnucash"), true);

    EXPECT_EQ (gnc_filename_is_datafile (""), false);
    EXPECT_EQ (gnc_filename_is_datafile ("a.gnucash"), true);
    EXPECT_EQ (gnc_filename_is_datafile ("a.gnucash.20201131010203.gnucash"), false);
}
