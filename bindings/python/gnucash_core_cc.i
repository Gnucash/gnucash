%module(package="gnucash") gnucash_core_cc

%include <std_string.i>

/* rvalue std::string is not being provided by std_string.i */
/* QofBackend.get_message() returns std::string&& */
/* Question regarding this approach:
   https://stackoverflow.com/questions/62119785/python-swig-wrapper-for-c-rvalue-stdstring*/
%typemap(out) std::string&& {
  std::string s = *$1;
  $result = SWIG_From_std_string(s);
}
/* I assume that QofBackend::set_message(std::string &&) will not be used from python */
%ignore QofBackend::set_message;

/* renames for qofsession.hpp */
/* prevent Warning 314: 'from' is a python keyword, renaming to '_from' */
/* TODO: restrict to this class */
%rename(_from) from; // qofsession.hpp:118 qof_instance_copy_data.from -> qof_instance_copy_data._from as from is reserved keyword in python

%{
  #include <string>
  #include "qofsession.h"
  #include "qofsession.hpp"
  #include "qof-backend.hpp"
  #include "qofbackend.h"
%}

%ignore save_in_progress;
%ignore qof_session_get_book_id;
%ignore qof_session_get_uri;
%include <qofsession.h>

%include <qof-backend.hpp>

%include <qofsession.hpp>
/* include to get QofBackendError as int */
%include <qofbackend.h>
