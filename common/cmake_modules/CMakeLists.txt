
SET(cmake_FILES GncAddGSchemaTargets.cmake GncAddSchemeTargets.cmake GncAddSwigCommand.cmake GncAddTest.cmake
        GncFindPkgConfig.cmake MacroAddSourceFileCompileFlags.cmake MacroAppendForeach.cmake
        MakeDist.cmake MakeDistFiles.cmake MakeDistCheck.cmake)

SET_DIST_LIST(cmake_modules_DIST CMakeLists.txt COPYING-CMAKE-SCRIPTS.txt ${cmake_FILES})
