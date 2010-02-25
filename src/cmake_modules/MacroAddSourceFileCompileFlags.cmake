# - MACRO_ADD_SOURCE_FILE_COMPILE_FLAGS(<_target> "flags...")

# Copyright (c) 2006, Oswald Buddenhagen, <ossi@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.


MACRO (MACRO_ADD_SOURCE_FILE_COMPILE_FLAGS _sourcefile _additionalflags)

   GET_SOURCE_FILE_PROPERTY (_flags ${_sourcefile} COMPILE_FLAGS)
   if (_flags)
      set(_flags "${_flags} ${_additionalflags}")
   else (_flags)
      set(_flags "${_additionalflags}")
   endif (_flags)
   SET_SOURCE_FILES_PROPERTIES (${_sourcefile} PROPERTIES COMPILE_FLAGS "${_flags}")

ENDMACRO (MACRO_ADD_SOURCE_FILE_COMPILE_FLAGS)
