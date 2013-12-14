%module sw_bar
%{
#include <bar.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_bar_module (void);
%}
#endif

int bar_hello(void);
