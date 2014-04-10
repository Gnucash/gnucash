#include <stdio.h>
#include <stdlib.h>
#include <guile/gh.h>

#include "gnc-module.h"

static void
guile_main(int argc, char ** argv) {
  GNCModule module;

  printf("  test-load-module.c: loading/unloading business-core module ... ");

  gnc_module_system_init();
  module = gnc_module_load("gnucash/business-core", 0);
  
  if(!module) {
    printf("  Failed to load engine\n");
    exit(-1);
  }
  
  if(!gnc_module_unload(module)) {
    printf("  Failed to unload engine\n");
    exit(-1);
  }
  printf(" successful.\n");

  exit(0);
}

int
main(int argc, char ** argv) {
  gh_enter(argc, argv, guile_main);
  return 0;
}
