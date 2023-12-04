#lcov command options:
# -a, --add-tracefile (takes glob)
# -c, --capture; create a trace file from the .da files
# -e, --extract; a reduced scope for analysis
# -l, --list; the contents of a tracefile
# -r, --remove; remove pattern-match from tracefile
# -z, --zerocounters; run this first, then -c -i
# --diff
# --summary
# other necessary options:
# --directory; points to the source root. If left off it analyzes the kernel
# --no-external; only analyze code in --directory
# --build-directory; where the .no when different from the .da files
# --branch-coverage; to ensure branch info is saved
# --demangle-cpp; requires c++filt

if (COVERAGE)
  find_program(LCOV lcov)
  find_program(GENINFO geninfo)
  find_program(GENHTML genhtml)
  find_program(CPPFILT c++filt)

  if (NOT LCOV OR NOT GENINFO OR NOT GENHTML OR NOT CPPFILT)
    MESSAGE(WARNING "A required program for presenting coverage information isn't available, disabling coverage")
    set(COVERAGE OFF CACHE INTERNAL "")
    return()
  endif()
else()
  return()
endif()

execute_process(COMMAND lcov --version OUTPUT_VARIABLE lcov_version_response)
string(REGEX MATCH "[-.0-9]+" lcov_version ${lcov_version_response})
set(LCOV_VERSION ${lcov_version} CACHE INTERNAL "")

set(excludes_arg "")
foreach (sys_path ${CMAKE_SYSTEM_PREFIX_PATH})
  list(APPEND excludes_arg "--exclude" "${sys_path}/*")
endforeach()

set(ignores_arg "--ignore-errors" "unused,unused" "--ignore-errors" "mismatch,mismatch"
  "--ignore-errors" "empty,empty")
list(APPEND ignores_arg "--rc" "geninfo_unexecuted_blocks=1")
set(generate_flags "")
set(geninfo_flags --quiet ${excludes_arg})
if (LCOV_VERSION VERSION_GREATER_EQUAL "2.0")
  list(APPEND geninfo_flags ${ignores_arg})
  list(APPEND generate_flags "--branch-coverage" "--demangle-cpp" "c++filt")
else()
  list(APPEND generate_flags "--rc" "lcov_branch_coverage=1" "--rc" "genhtml_demangle_cpp=1")
endif()
set(coverage_dir "${CMAKE_BINARY_DIR}/Coverage" CACHE INTERNAL "Directory to accumulate coverage tracefiles")
set(coverage_html_dir "${CMAKE_BINARY_DIR}/Coverage-HTML" CACHE INTERNAL "Directory to place HTML coverage results pages")

file(MAKE_DIRECTORY ${coverage_dir})

add_custom_target(lcov-initialize)
if (LCOV_VERSION VERSION_GREATER_EQUAL "2.0")
  add_custom_target(lcov-collect
    COMMAND lcov ${geninfo_flags} -a ${coverage_dir}/*.info -o ${coverage_dir}/gnucash.info
    COMMAND lcov --summary ${coverage_dir}/gnucash.info
    VERBATIM
    COMMAND_EXPAND_LISTS)
else()
  file(GENERATE OUTPUT ${CMAKE_BINARY_DIR}/collect.sh
    CONTENT
    "#!/bin/bash
if [ -e $2 ]
  then rm $2
fi
j=\"\"
for i in $1/*.info
do j=\"$j -a $i\"
done
lcov $j -o $2
"
    FILE_PERMISSIONS OWNER_EXECUTE OWNER_READ OWNER_WRITE WORLD_EXECUTE)

  add_custom_target(lcov-collect
    COMMAND ${CMAKE_COMMAND} -E env ${CMAKE_BINARY_DIR}/collect.sh ${coverage_dir} ${coverage_dir}/gnucash.info
    DEPENDS ${CMAKE_BINARY_DIR}/collect.sh
    VERBATIM
    COMMAND_EXPAND_LISTS)
endif()
set_target_properties(lcov-collect PROPERTIES ADDITIONAL_CLEAN_FILES "${coverage_dir}/gnucash.info")

add_custom_target(lcov-generate-html
  genhtml --quiet --output-directory "${coverage_html_dir}" --legend --function-coverage ${generate_flags} ${coverage_dir}/gnucash.info
  VERBATIM
  COMMAND_EXPAND_LISTS)
set_target_properties(lcov-generate-html PROPERTIES ADDITIONAL_CLEAN_FILES "${coverage_html_dir}")

function (add_coverage_target tgt)
  get_target_property(build_dir ${tgt} BINARY_DIR)
  set(target_dir "${build_dir}/CMakeFiles/${tgt}.dir")

  add_custom_target(lcov-initialize-${tgt}
    lcov ${geninfo_flags} -z --directory ${target_dir}
    COMMAND lcov ${geninfo_flags} ${generate_flags} -c -i --directory ${target_dir} -o "${coverage_dir}/${tgt}_base.info"
    VERBATIM
    COMMAND_EXPAND_LISTS)
  add_dependencies(lcov-initialize lcov-initialize-${tgt})
  add_dependencies(lcov-initialize-${tgt} ${tgt})

  add_custom_target(lcov-collect-${tgt}
    COMMAND lcov ${geninfo_flags} ${generate_flags} -c --directory ${target_dir} -o "${coverage_dir}/${tgt}_result.info"
    VERBATIM
    COMMAND_EXPAND_LISTS)
  add_dependencies(lcov-collect lcov-collect-${tgt})
  set_target_properties(${tgt} PROPERTIES ADDITIONAL_CLEAN_FILES "${coverage_dir}/${tgt}_base.info;${coverage_dir}/${tgt}_result.info")
endFunction()
