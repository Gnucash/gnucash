# Copied & modified from https://android.googlesource.com/platform/external/eigen/+/master/cmake/FindStandardMathLibrary.cmake
# Copyright (c) 2010 Benoit Jacob <jacob.benoit.1@gmail.com>
# Redistribution and use is allowed according to the terms of the 2-clause BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

#Detect whether this platform requires libm for pow().

include(CheckCXXSourceCompiles)
macro (gnc_check_standard_math_library)
  set(find_standard_math_library_test_program
"
#include <math.h>
int main(int argc, char** argv)
{
    double foo = pow(2.0, 2.0);
    return foo == 4.0;
}"
  )

  set(CMAKE_REQUIRED_FLAGS "")
  set(CMAKE_REQUIRED_LIBRARIES "")
  check_c_source_compiles(
    "${find_standard_math_library_test_program}"
    standard_math_library_linked_to_automatically
    )
  if(standard_math_library_linked_to_automatically)
    # the test program linked successfully without any linker flag.
    set(STANDARD_MATH_LIBRARY "")
    set(STANDARD_MATH_LIBRARY_FOUND TRUE)
  else()
    # the test program did not link successfully without any linker flag.
    # Try again with standard name 'm' for the standard math library.
    set(CMAKE_REQUIRED_LIBRARIES "m")
    check_c_source_compiles(
      "${find_standard_math_library_test_program}"
      standard_math_library_linked_to_as_m)
    if(standard_math_library_linked_to_as_m)
      # the test program linked successfully when linking to the 'm' library
      set(STANDARD_MATH_LIBRARY "m")
      set(STANDARD_MATH_LIBRARY_FOUND TRUE)
    else()
      # the test program still doesn't link successfully
      set(STANDARD_MATH_LIBRARY_FOUND FALSE)
    endif()
  endif()
endmacro()
