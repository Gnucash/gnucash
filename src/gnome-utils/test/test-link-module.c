#include <stdlib.h>
#include <guile/gh.h>
#include <gnc-module.h>

static void
guile_main(int argc, char ** argv) {
  gnc_module_system_init();
/*  gnc_module_load("gnucash/gnome-utils", 0); */
  exit(0);
}

int
main(int argc, char ** argv) {
  gh_enter(argc, argv, guile_main);
  return 0;
}
