#include <stdio.h>
#include <stdlib.h>
#include <guile/gh.h>

#include "gnc-module.h"

static void
guile_main(int argc, char ** argv) {
  GNCModule foo;

  printf("  test-incompatdep.c:  loading a module with bad deps ...\n");

  gnc_module_system_init();

  foo = gnc_module_load("gnucash/incompatdeps", 0);
  
  if(!foo) {
    printf("  ok\n");
    exit(0);
  }
  else {
    printf("  oops! loaded incompatible module\n");
    exit(-1);
  }
}

int
main(int argc, char ** argv) {
  gh_enter(argc, argv, guile_main);
  return 0;
}
