
#include <stdio.h>
#include <stdlib.h>
#include <guile/gh.h>

#include "gnc-module.h"

static void
guile_main(int argc, char ** argv) {
  int       (*foo_hello)(void);
  int helloval; 
  GNCModule foo;

  printf("  test-load-c.c: testing module load/unload from C ... ");

  gnc_module_system_init();

  foo = gnc_module_load("gnucash/foo", 0);
  
  if(!foo) {
    printf("  Failed to load foo\n");
    exit(-1);
  }
  
  foo_hello = gnc_module_lookup(foo, "foo_hello");
  helloval = foo_hello();
  if(helloval != 10) {
    printf("  Call of module function failed.\n");
    exit(-1);
  }

  if(!gnc_module_unload(foo)) {
    printf("  Failed to unload foo\n");
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
