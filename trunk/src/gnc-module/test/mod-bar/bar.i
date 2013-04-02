%module sw_bar
%{
#include <bar.h>

SCM scm_init_sw_bar_module (void);
%}

int bar_hello(void);
