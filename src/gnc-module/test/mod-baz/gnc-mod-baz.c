/* gnc-mod-baz.c : the Gnucash plugin that wraps the library
 * 'libbaz.so'. it does this by being linked against libbaz.so */

#include <stdio.h>
#include <guile/gh.h>
#include "gnc-module.h"
#include "baz-gwrap.h"

int gnc_module_system_interface = 0;

int gnc_module_current = 0;
int gnc_module_age = 0;
int gnc_module_revision = 0;

char * 
gnc_module_path(void) {
  return g_strdup("gnucash/baz");
}

char * 
gnc_module_description(void) {
  return g_strdup("this is the baz module");
}

int
gnc_module_init(int refcount) {
  /* load libfoo */
  if(gnc_module_load("gnucash/foo", 0)) {
    /* publish the g-wrapped Scheme bindings for libbaz */
    gw_init_module_baz_gwrap();
    
    /* use the scheme module */
    gh_eval_str("(use-modules (gnucash baz))");

    return TRUE;
  }
  else {
    return FALSE;
  }
}

