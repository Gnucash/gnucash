/* agedver.c : testing module age? */

#include <stdio.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

int libagedver_LTX_gnc_module_system_interface = 0;

int libagedver_LTX_gnc_module_current = 12;
int libagedver_LTX_gnc_module_age = 9;
int libagedver_LTX_gnc_module_revision = 0;
char* libagedver_LTX_gnc_module_path(void);
char* libagedver_LTX_gnc_module_description(void);
int libagedver_LTX_gnc_module_init(int refcount);
char *
libagedver_LTX_gnc_module_path(void) {
  return g_strdup("gnucash/agedver");
}

char *
libagedver_LTX_gnc_module_description(void) {
  return g_strdup("this is a frequently extended module");
}

int 
libagedver_LTX_gnc_module_init(int refcount) {
  return TRUE;
}

