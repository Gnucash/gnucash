
set(cmake_FILES
  GncAddGSchemaTargets.cmake
  GncAddSchemeTargets.cmake
  GncAddSwigCommand.cmake
  GncAddTest.cmake
  GncFindLibm.cmake
  GncGenerateGResources.cmake
  MacroAddSourceFileCompileFlags.cmake
  MacroAppendForeach.cmake
  MakeDist.cmake
  MakeDistFiles.cmake
  MakeDistCheck.cmake
)

set_dist_list(cmake_modules_DIST CMakeLists.txt COPYING-CMAKE-SCRIPTS.txt ${cmake_FILES})
