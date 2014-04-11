%module sw_baz
%{
#include <baz.h>

SCM scm_init_sw_baz_module (void);
%}

int baz_hello(void);
