/* gnc-mod-bar.c : the Gnucash plugin that wraps the library
 * 'libbar.so'. it does this by being linked against libbar.so */

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>
#include "bar-gwrap.h"

int gnc_module_system_interface = 0;

int gnc_module_current = 0;
int gnc_module_age = 0;
int gnc_module_revision = 0;

char *
gnc_module_path(void) {
  return g_strdup("gnucash/bar");
}

char *
gnc_module_description(void) {
  return g_strdup("this is a bar module");
}

int
gnc_module_init(int refcount) {
  /* publish the g-wrapped Scheme bindings for libbar */
  gw_init_module_bar_gwrap();
  
  /* use the (bar) module */ 
  gh_eval_str("(use-modules (gnucash bar))");

  return TRUE;
}


