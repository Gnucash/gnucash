#include "config.h"

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "gnc-xml-helper.h"
#include "gnc-xml.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-pricedb.h"

#include "sixtp-parsers.h"
#include "sixtp-dom-parsers.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#include "GNCIdP.h"
#include "gnc-book.h"


struct pricedb_data_struct
{
  GNCPriceDB *db;
  int value;
};
typedef struct pricedb_data_struct pricedb_data;

static gboolean
test_add_pricedb (const char *tag, gpointer globaldata, gpointer data)
{
  pricedb_data *gdata = globaldata;

  do_test_args (gnc_pricedb_equal(data, gdata->db),
                "gnc_pricedb_sixtp_parser_create", 
                __FILE__, __LINE__, "%d", gdata->value);

  return TRUE;
}

static void
test_db (int i, GNCPriceDB *db)
{
  xmlNodePtr test_node;
  gchar *filename1;
  gchar *compare_msg;
  FILE *cmp_file;
  int fd;

  test_node = gnc_pricedb_dom_tree_create (db);

  if (!test_node && db)
  {
    failure_args ("pricedb_xml", __FILE__, __LINE__, 
                  "gnc_pricedb_dom_tree_create returned NULL");
    return;
  }

  if (!db)
    return;

  filename1 = g_strdup_printf ("test_file_XXXXXX");
        
  fd = mkstemp (filename1);
        
  write_dom_node_to_file (test_node, fd);

  close (fd);

  {
    sixtp *parser;
    pricedb_data data;

    data.db = db;
    data.value = i;

    parser = sixtp_new ();

    if (!sixtp_add_some_sub_parsers
        (parser, TRUE,
         "gnc:pricedb", gnc_pricedb_sixtp_parser_create(),
         NULL, NULL))
    {
      failure_args ("sixtp_add_some_sub_parsers failed",
                    __FILE__, __LINE__, "%d", i);
    }
    else if (!gnc_xml_parse_file (parser, filename1, test_add_pricedb,
                                  (gpointer)&data))
    {
      failure_args ("gnc_xml_parse_file returned FALSE",
                    __FILE__, __LINE__, "%d", i);
    }
  }

  unlink (filename1);
  g_free (filename1);
  xmlFreeNode (test_node);
}

static void
test_generation (void)
{
  int i;

  for (i = 0; i < 20; i++)
  {
    GNCPriceDB *db;

    db = get_random_pricedb ();

    if (gnc_pricedb_get_num_prices (db))
      test_db (i, db);

    gnc_pricedb_destroy (db);
  }
}

int
main (int argc, char **argv)
{
  gnc_engine_init (argc, argv);

  test_generation ();

  print_test_results ();
  exit (get_rv ());
}
