# - macro_add_source_file_compile_flags(<_target> "flags...")

# Copyright (c) 2006, Oswald Buddenhagen, <ossi@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.


macro (macro_add_source_file_compile_flags _sourcefile _additionalflags)

   get_source_file_property (_flags ${_sourcefile} COMPILE_FLAGS)
   if (_flags)
      set(_flags "${_flags} ${_additionalflags}")
   else()
      set(_flags "${_additionalflags}")
   endif()
   set_source_files_properties (${_sourcefile} PROPERTIES COMPILE_FLAGS "${_flags}")

endmacro (macro_add_source_file_compile_flags)
