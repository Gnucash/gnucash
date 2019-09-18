extern "C" {
#include <config.h>
#include <unittest-support.h>

#include <glib.h>
#include <gtk/gtk.h> /* for references in import-backend.h */
#include "import-backend.h"
#include "import-pending-matches.h"
#include "Split.h"
#include "test-engine-stuff.h"
}

static const gchar *suitename = "/import-export/import-pending-matches";

typedef struct
{
    QofBook *book;
    Account *account1;
    Account *account2;
    Transaction *txn;
    Split *split;
    GNCImportMatchInfo *match_info;
} Fixture;

static void
setup (Fixture *fixture, gconstpointer pData)
{
    fixture->book = qof_book_new();
    fixture->account1 = get_random_account (fixture->book);
    fixture->account2 = get_random_account (fixture->book);
    fixture->txn = get_random_transaction (fixture->book);
    fixture->split = get_random_split (fixture->book, fixture->account1,
                                       fixture->txn);
    fixture->match_info = g_new0 (GNCImportMatchInfo, 1);

    fixture->match_info->split = fixture->split;
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    g_free (fixture->match_info);
    xaccSplitDestroy (fixture->split);
    xaccTransDestroy (fixture->txn);
    xaccAccountBeginEdit (fixture->account1);
    xaccAccountDestroy (fixture->account1);
    xaccAccountBeginEdit (fixture->account2);
    xaccAccountDestroy (fixture->account2);
    qof_book_destroy (fixture->book);

    test_clear_error_list();
}

/* The excluded tests all rely on g_assert_true which was only introduced
 * in glib 2.38 */
#ifdef HAVE_GLIB_2_38
static void
test_pending_matches_match_types (Fixture *fixture, gconstpointer pData)
{
    GNCImportPendingMatches *matches = gnc_import_PendingMatches_new();
    GNCImportPendingMatchType match_types;

    match_types = gnc_import_PendingMatches_get_match_type (matches,
                                                            fixture->match_info);
    g_assert_true (match_types == GNCImportPending_NONE);

    gnc_import_PendingMatches_add_match (matches, fixture->match_info, FALSE);    
    match_types = gnc_import_PendingMatches_get_match_type (matches,
                                                            fixture->match_info);
    g_assert_true (match_types == GNCImportPending_AUTO);

    gnc_import_PendingMatches_add_match (matches, fixture->match_info, TRUE);    
    match_types = gnc_import_PendingMatches_get_match_type (matches,
                                                            fixture->match_info);
    g_assert_true (match_types == GNCImportPending_MANUAL);

    gnc_import_PendingMatches_delete (matches);
}

static void
test_pending_matches_prefer_manual_match (Fixture *fixture, gconstpointer pData)
{
    GNCImportPendingMatchType match_type;
    GNCImportPendingMatches *matches = gnc_import_PendingMatches_new();
    gnc_import_PendingMatches_add_match (matches, fixture->match_info, TRUE);
    gnc_import_PendingMatches_add_match (matches, fixture->match_info, FALSE);
    match_type = gnc_import_PendingMatches_get_match_type (matches,
                                                           fixture->match_info);

    g_assert_true (match_type == GNCImportPending_MANUAL);

    gnc_import_PendingMatches_delete (matches);
}

static void
test_pending_matches_keeps_count (Fixture *fixture, gconstpointer pData)
{
    GNCImportPendingMatchType auto_match;
    GNCImportPendingMatchType no_match;
    GNCImportPendingMatches *matches = gnc_import_PendingMatches_new();

    gnc_import_PendingMatches_add_match (matches, fixture->match_info, TRUE);
    gnc_import_PendingMatches_add_match (matches, fixture->match_info, TRUE);
    gnc_import_PendingMatches_remove_match (matches, fixture->match_info, TRUE);

    auto_match = gnc_import_PendingMatches_get_match_type (matches,
                                                           fixture->match_info);

    gnc_import_PendingMatches_remove_match (matches, fixture->match_info, TRUE);

    no_match = gnc_import_PendingMatches_get_match_type (matches,
                                                         fixture->match_info);

    g_assert_true (auto_match != no_match);
    g_assert_true (no_match == GNCImportPending_NONE);

    gnc_import_PendingMatches_delete (matches);
}
#endif

int
main (int argc, char *argv[])
{
    int result;
    qof_init();
    g_test_init (&argc, &argv, NULL);

    /* The excluded tests all rely on g_assert_true which was only introduced
     * in glib 2.38 */
#ifdef HAVE_GLIB_2_38
    GNC_TEST_ADD (suitename, "match_types", Fixture, NULL, setup,
                  test_pending_matches_match_types, teardown);
    GNC_TEST_ADD (suitename, "prefer_manual_match", Fixture, NULL, setup,
                  test_pending_matches_prefer_manual_match, teardown);
    GNC_TEST_ADD (suitename, "keeps_count", Fixture, NULL, setup,
                  test_pending_matches_keeps_count, teardown);
#endif
    result =  g_test_run();

    qof_close();
}
