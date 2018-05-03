# - macro_append_foreach(<_targetlist> _prefix _suffix <_sourcelist> )

# Copyright (c) 2009, Christian Stimming

# Appends each element of the <_sourcelist> to the <_targetlist>, but
# with the _prefix prepended and _suffix appended. Note: If no suffix
# is desired, pass an empty string ("") there.

# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

macro (macro_append_foreach _target _prefix _suffix)

  foreach (_loop_var ${ARGN})

	set (${_target} ${${_target}} "${_prefix}${_loop_var}${_suffix}")
	
  endforeach (_loop_var)

endmacro (macro_append_foreach)
