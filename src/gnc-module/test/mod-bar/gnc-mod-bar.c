/* gnc-mod-bar.c : the Gnucash plugin that wraps the library
 * 'libbar.so'. it does this by being linked against libbar.so */

#include "config.h"
#include <stdio.h>
#include <glib.h>
#include <libguile.h>
#include "guile-mappings.h"

#include "bar-gwrap.h"
#include "gnc-module-api.h"

int libgncmodbar_LTX_gnc_module_system_interface = 0;

int libgncmodbar_LTX_gnc_module_current = 0;
int libgncmodbar_LTX_gnc_module_age = 0;
int libgncmodbar_LTX_gnc_module_revision = 0;

/* forward references */
char *libgncmodbar_LTX_gnc_module_path(void);
char *libgncmodbar_LTX_gnc_module_description(void);
int libgncmodbar_LTX_gnc_module_init(int refcount);

char *
libgncmodbar_LTX_gnc_module_path(void) {
  return g_strdup("gnucash/bar");
}

char *
libgncmodbar_LTX_gnc_module_description(void) {
  return g_strdup("this is a bar module");
}

int
libgncmodbar_LTX_gnc_module_init(int refcount) {
  /* publish the g-wrapped Scheme bindings for libbar */
  gw_init_wrapset_bar_gwrap();
  
  /* use the (bar) module */ 
  scm_c_eval_string("(use-modules (gnucash bar))");

  return TRUE;
}


