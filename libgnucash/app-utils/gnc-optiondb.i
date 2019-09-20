/*
 * Temporary swig interface file while developing C++ options.
 */

%module sw_gnc_optiondb
%{
#include <libguile.h>
#include "gnc-optiondb.hpp"
extern "C" SCM scm_init_sw_gnc_optiondb_module(void);
%}

%include <std_string.i>

%ignore OptionClassifier;
%ignore OptionUIItem;
%ignore GncOption;

%include "gnc-optiondb.hpp"
