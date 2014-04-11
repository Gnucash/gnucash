%module sw_foo
%{
#include <foo.h>

SCM scm_init_sw_foo_module (void);
%}

int foo_hello(void);
