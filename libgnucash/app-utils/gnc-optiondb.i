/*
 * Temporary swig interface file while developing C++ options.
 *
 * unique_ptr SWIG wrapper from https://stackoverflow.com/questions/27693812/how-to-handle-unique-ptrs-with-swig
 */

namespace std {
  %feature("novaluewrapper") unique_ptr;
  template <typename Type>
  struct unique_ptr {
     typedef Type* pointer;

     explicit unique_ptr( pointer Ptr );
     unique_ptr (unique_ptr&& Right);
     template<class Type2, Class Del2> unique_ptr( unique_ptr<Type2, Del2>&& Right );
     unique_ptr( const unique_ptr& Right) = delete;


     pointer operator-> () const;
     pointer release ();
     void reset (pointer __p=pointer());
     void swap (unique_ptr &__u);
     pointer get () const;
//     operator bool () const;

     ~unique_ptr();
  };
}

%define wrap_unique_ptr(Name, Type)
  %template(Name) std::unique_ptr<Type>;
  %newobject std::unique_ptr<Type>::release;

  %typemap(out) std::unique_ptr<Type> %{
    $result = SWIG_NewPointerObj(new $1_ltype(std::move($1)), $&1_descriptor, SWIG_POINTER_OWN);
  %}

%enddef

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

wrap_unique_ptr(GncOptionDBPtr, GncOptionDB);

%include "gnc-optiondb.hpp"
