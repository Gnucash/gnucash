
/* Test file created by Linas Vepstas <linas@linas.org>
 * Try to create duplicate GUID's, which should never happen.
 *
 * October 2003
 * License: GPL
 */

#include <ctype.h>
#include <glib.h>

#include "gnc-module.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "qofbook.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofsession.h"
#include "guid.h"

static void test_null_guid(void)
{
  GUID g;
  GUID *gp;

  g = guid_new_return();
  gp = guid_malloc();
  guid_new(gp);

  do_test(guid_equal(guid_null(), guid_null()), "null guids equal");
  do_test(!guid_equal(&g, gp), "two guids equal");
}

static void
run_test (void)
{
  int i;
  QofSession *sess;
  QofBook *book;
  QofEntity *eblk;
  QofCollection *col;
  QofIdType type;

  sess = get_random_session ();
  book = qof_session_get_book (sess);
  do_test ((NULL != book), "book not created");

  col = qof_book_get_collection (book, "asdf");
  type = qof_collection_get_type (col);
  
#define NENT 500123
  eblk = g_new0(QofEntity, NENT);
  for (i=0; i<NENT; i++)
  {
    QofEntity *ent = &eblk[i];
    guid_new(&ent->guid);
    do_test ((NULL == qof_collection_lookup_entity (col, &ent->guid)),
						  "duplicate guid");
	 ent->e_type = type;
	 qof_collection_insert_entity (col, ent);
  }

  /* Make valgrind happy -- destroy the session. */
  qof_session_destroy(sess);
}

static void
main_helper (void *closure, int argc, char **argv)
{
  g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );
  do_test((NULL!=gnc_module_load("gnucash/engine", 0)), "couldn't load engine");

  test_null_guid();
  run_test();

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile(argc, argv, main_helper, NULL);
  return 0;
}
