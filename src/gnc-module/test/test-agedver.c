#include <stdio.h>
#include <stdlib.h>
#include <guile/gh.h>

#include "gnc-module.h"

static void
guile_main(int argc, char ** argv) {
  GNCModule foo;

  printf("  test-agedver.c:  asking for an old but supported interface ...");

  gnc_module_system_init();

  foo = gnc_module_load("gnucash/agedver", 5);
  
  if(foo) {
    printf("  ok\n");
    exit(0);
  }
  else {
    printf(" failed\n");
    exit(-1);
  }
}

int
main(int argc, char ** argv) {
  gh_enter(argc, argv, guile_main);
  return 0;
}
