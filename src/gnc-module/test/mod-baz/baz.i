%module sw_baz
%{
#include <baz.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_baz_module (void);
%}
#endif

int baz_hello(void);
