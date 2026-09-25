if(NOT DEFINED SOURCE_ROOT)
  message(FATAL_ERROR "SOURCE_ROOT is required")
endif()

file(GLOB_RECURSE gtk_sources
  LIST_DIRECTORIES FALSE
  "${SOURCE_ROOT}/gnucash/*.c"
  "${SOURCE_ROOT}/gnucash/*.cc"
  "${SOURCE_ROOT}/gnucash/*.cpp"
  "${SOURCE_ROOT}/gnucash/*.mm")

set(window_count 0)
set(ofx_test_seam_count 0)
set(violations)

foreach(source IN LISTS gtk_sources)
  file(RELATIVE_PATH relative_source "${SOURCE_ROOT}/gnucash" "${source}")
  file(TO_CMAKE_PATH "${relative_source}" relative_source)
  if(relative_source MATCHES "(^|/)tests?(/|$)")
    continue()
  endif()

  file(STRINGS "${source}" source_lines)
  set(in_block_comment FALSE)
  set(pending_binding FALSE)
  set(pending_line 0)
  set(binding_lines_left 0)
  set(ofx_test_seam_context 0)
  set(line_number 0)

  foreach(raw_line IN LISTS source_lines)
    math(EXPR line_number "${line_number} + 1")
    set(line "${raw_line}")
    set(clean_line "")
    string(LENGTH "${line}" line_length)
    set(cursor 0)

    while(cursor LESS line_length)
      string(SUBSTRING "${line}" ${cursor} -1 tail)
      if(in_block_comment)
        string(FIND "${tail}" "*/" block_end)
        if(block_end EQUAL -1)
          set(cursor ${line_length})
        else()
          math(EXPR cursor "${cursor} + ${block_end} + 2")
          set(in_block_comment FALSE)
        endif()
      else()
        string(FIND "${tail}" "/*" block_start)
        string(FIND "${tail}" "//" line_comment)
        if(block_start EQUAL -1 AND line_comment EQUAL -1)
          string(APPEND clean_line "${tail}")
          set(cursor ${line_length})
        elseif(line_comment GREATER_EQUAL 0
               AND (block_start EQUAL -1 OR line_comment LESS block_start))
          if(line_comment GREATER 0)
            string(SUBSTRING "${tail}" 0 ${line_comment} prefix)
            string(APPEND clean_line "${prefix}")
          endif()
          set(cursor ${line_length})
        else()
          if(block_start GREATER 0)
            string(SUBSTRING "${tail}" 0 ${block_start} prefix)
            string(APPEND clean_line "${prefix}")
          endif()
          math(EXPR cursor "${cursor} + ${block_start} + 2")
          set(in_block_comment TRUE)
        endif()
      endif()
    endwhile()

    if(clean_line MATCHES "gnc_ofx_import_test_attach_reconcile")
      set(ofx_test_seam_context 6)
    elseif(ofx_test_seam_context GREATER 0)
      math(EXPR ofx_test_seam_context "${ofx_test_seam_context} - 1")
    endif()

    if(pending_binding)
      if(clean_line MATCHES "gnc_window_bind_to_application[ \t]*\\(")
        set(pending_binding FALSE)
      elseif(NOT clean_line MATCHES "^[ \t]*$")
        math(EXPR binding_lines_left "${binding_lines_left} - 1")
        if(binding_lines_left LESS 0)
          list(APPEND violations "${relative_source}:${pending_line}")
          set(pending_binding FALSE)
        endif()
      endif()
    endif()

    if(clean_line MATCHES "gtk_window_new[ \t]*\\(")
      math(EXPR window_count "${window_count} + 1")
      if(pending_binding)
        list(APPEND violations "${relative_source}:${pending_line}")
        set(pending_binding FALSE)
      endif()

      if(relative_source STREQUAL "import-export/ofx/gnc-ofx-import.cpp"
         AND ofx_test_seam_context GREATER 0)
        math(EXPR ofx_test_seam_count "${ofx_test_seam_count} + 1")
      elseif(clean_line MATCHES "gnc_window_bind_to_application[ \t]*\\(")
        # A compact helper expression may create and bind on the same line.
      else()
        set(pending_binding TRUE)
        set(pending_line ${line_number})
        set(binding_lines_left 3)
      endif()
    endif()
  endforeach()

  if(pending_binding)
    list(APPEND violations "${relative_source}:${pending_line}")
  endif()
endforeach()

if(NOT ofx_test_seam_count EQUAL 1)
  message(FATAL_ERROR
    "Expected exactly one standalone OFX test-seam window, found ${ofx_test_seam_count}")
endif()

file(READ "${SOURCE_ROOT}/gnucash/gnome-utils/dialog-utils.c" builder_loader)
if(NOT builder_loader MATCHES
   "GTK_IS_WINDOW[ \t]*\\(node->data\\)[^\n]*\n[ \t]*gnc_window_bind_to_application")
  list(APPEND violations
    "gnome-utils/dialog-utils.c: central Builder GtkWindow binding is missing")
endif()

if(violations)
  list(JOIN violations "\n  " violation_list)
  message(FATAL_ERROR
    "Productive GtkWindow constructors must immediately call "
    "gnc_window_bind_to_application():\n  ${violation_list}")
endif()

message(STATUS
  "Checked ${window_count} productive GtkWindow constructors; "
  "the single standalone OFX test seam remains isolated")
