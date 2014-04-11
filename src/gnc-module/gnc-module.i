%module sw_gnc_module
%{
#include <gnc-module.h>

SCM scm_init_sw_gnc_module_module (void);
%}

typedef char gchar;
typedef int gint;

void            gnc_module_system_init(void);
void            gnc_module_system_refresh(void);
GNCModule       gnc_module_load(gchar * module_name, gint interface);
GNCModule       gnc_module_load_optional(gchar * module_name, gint interface);
int             gnc_module_unload(GNCModule mod);
