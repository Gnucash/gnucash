/* gnc-mod-foo.c : the Gnucash plugin that wraps the library
 * 'libfoo.so'. it does this by being linked against libfoo.so */

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>
#include "foo-gwrap.h"

int gnc_module_system_interface = 0;

int gnc_module_current = 0;
int gnc_module_age = 0;
int gnc_module_revision = 0;

char *
gnc_module_path(void) {
  return g_strdup("gnucash/foo");
}

char *
gnc_module_description(void) {
  return g_strdup("this is a foo module");
}

int 
gnc_module_init(int refcount) {
  /* publish the g-wrapped Scheme bindings for libfoo */
  gw_init_module_foo_gwrap();
  
  /* use the Scheme "foo" module */
  gh_eval_str("(use-modules (gnucash foo))");
  
  return TRUE;
}
