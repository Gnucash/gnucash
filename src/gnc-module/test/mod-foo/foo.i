%module sw_foo
%{
#include <foo.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_foo_module (void);
%}
#endif

int foo_hello(void);
