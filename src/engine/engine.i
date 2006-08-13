%module sw_engine
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <glib.h>
#include <qof.h>
#include <Group.h>
#include <Query.h>
#include <gnc-budget.h>
#include <gnc-commodity.h>
#include <gnc-engine.h>
#include <gnc-filepath-utils.h>
#include <gnc-pricedb.h>
#include <gnc-lot.h>
#include <gnc-session-scm.h>
#include <gnc-hooks-scm.h>
#include <engine-helpers.h>
#include <SX-book.h>

%}

%typemap(in) gboolean {
  $1 = SCM_NFALSEP($input) ? TRUE : FALSE;
}

%typemap(out) gboolean {
  $result = $1 ? SCM_BOOL_T : SCM_BOOL_F;
}

%typemap(in) Timespec {
  $1 = gnc_timepair2timespec($input);
}

%typemap(out) Timespec {
  $result = gnc_timespec2timepair($1);
}

%typemap(in) GUID {
  $1 = gnc_scm2guid($input);
}

%typemap(out) GUID {
  $result = gnc_guid2scm($1);
}

%typemap(in) gnc_numeric {
  $1 = gnc_scm_to_numeric($input);
}

%typemap(out) gnc_numeric {
  $result = gnc_numeric_to_scm($1);
}

/* Parse the header file to generate wrappers */
%include <Group.h>
%include <Query.h>
%include <gnc-budget.h>
%include <gnc-commodity.h>
%include <gnc-engine.h>
%include <gnc-filepath-utils.h>
%include <gnc-pricedb.h>
%include <gnc-lot.h>
%include <gnc-session-scm.h>
%include <gnc-hooks-scm.h>
%include <engine-helpers.h>
%include <SX-book.h>

