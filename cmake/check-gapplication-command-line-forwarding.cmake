if(NOT DEFINED SOURCE_ROOT)
  message(FATAL_ERROR "SOURCE_ROOT is required")
endif()

file(READ "${SOURCE_ROOT}/gnucash/gnucash.cpp" gnucash_source)

string(FIND "${gnucash_source}"
  "Gnucash::Gnucash::command_line" command_line_start)
string(FIND "${gnucash_source}"
  "Gnucash::Gnucash::run" command_line_end)

if(command_line_start EQUAL -1 OR command_line_end EQUAL -1
   OR command_line_end LESS_EQUAL command_line_start)
  message(FATAL_ERROR "Unable to locate the GApplication command-line handler")
endif()

math(EXPR command_line_length
  "${command_line_end} - ${command_line_start}")
string(SUBSTRING "${gnucash_source}" ${command_line_start}
  ${command_line_length} command_line_source)

foreach(required_call IN ITEMS
    g_application_command_line_create_file_for_arg
    g_file_get_uri
    g_application_command_line_printerr)
  string(FIND "${command_line_source}" "${required_call}" required_position)
  if(required_position EQUAL -1)
    message(FATAL_ERROR
      "GApplication command-line handler must call ${required_call}()")
  endif()
endforeach()

foreach(forbidden_call IN ITEMS g_filename_to_uri g_printerr)
  string(FIND "${command_line_source}" "${forbidden_call}" forbidden_position)
  if(NOT forbidden_position EQUAL -1)
    message(FATAL_ERROR
      "Forwarded command lines must not call ${forbidden_call}()")
  endif()
endforeach()

string(FIND "${command_line_source}" "return 1;" failure_status)
if(failure_status EQUAL -1)
  message(FATAL_ERROR
    "Unsupported forwarded command lines must return a failure status")
endif()

message(STATUS
  "Checked forwarded GApplication path, diagnostic, and status handling")
