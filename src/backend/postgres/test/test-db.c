#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>

#include "Backend.h"
#include "TransLog.h"
#include "gnc-book.h"
#include "gnc-engine.h"
#include "gnc-module.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"

static void
guile_main (int argc, char **argv)
{
  gnc_module_system_init ();
  gnc_module_load ("gnucash/engine", 0);

  gnc_engine_init (argc, argv);

  xaccLogDisable ();

  print_test_results ();
  exit (get_rv ());
}

int
main (int argc, char ** argv)
{
  gh_enter (argc, argv, guile_main);
  return 0;
}
