/*********************************************************************
 * test-dynload.c
 * test the ability to dlopen the gnc_module library and initialize
 * it via dlsym 
 *********************************************************************/

#include <stdio.h>
#include <ltdl.h>
#include <guile/gh.h>

#include "gnc-module.h"

static void
guile_main(int argc, char ** argv)
{
  lt_dlhandle handle;

  lt_dlinit();

  printf("  test-dynload.c: testing dynamic linking of libgncmodule ...");
  handle = lt_dlopen("libgncmodule.la");
  if(handle) {
    lt_ptr ptr = lt_dlsym(handle, "gnc_module_system_init");
    if(ptr) {
      void (* fn)(void) = ptr;
      fn();
      printf(" OK\n");
      exit(0);
    }
    else {
      printf(" failed to find gnc_module_system_init\n");
      exit(-1);
    }
  }
  else {
    printf(" failed to open library.\n");
    printf("%s\n", lt_dlerror());
    exit(-1);
  }
}

int
main(int argc, char ** argv) {
  gh_enter(argc, argv, guile_main);
  return 0;
}

