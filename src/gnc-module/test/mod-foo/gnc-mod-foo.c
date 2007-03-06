/* gnc-mod-foo.c : the Gnucash plugin that wraps the library
 * 'libfoo.so'. it does this by being linked against libfoo.so */

#include "config.h"
#include <stdio.h>
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module-api.h"
#include "swig-foo.c"


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
  /* publish the wrapped Scheme bindings for libfoo */
  scm_init_sw_foo_module();
  scm_c_eval_string("(use-modules (sw_foo))");

  /* use the Scheme "foo" module */
  scm_c_eval_string("(use-modules (gnucash foo))");

  return TRUE;
}
