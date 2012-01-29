echo off
setlocal
set CWD=%~f1
set PATH=%CWD%\bin;%PATH%
set GUILE_HOME=%CWD%\share\guile\1.8
set GUILE_IMPLEMENTATION_PATH=%GUILE_HOME%
set GUILE_LOAD_PATH=%GUILE_HOME%
guile -c "(use-modules (ice-9 slib)) (require 'printf)"
endlocal