@echo off

if not exist ssleay32.dll (
  echo.
  echo Did not find ssleay32.dll in current directory.
  echo Please start this batch file in the bin directory created by the installer.
  goto error
)

echo * Install DateManip
echo.
perl -x -S ppm install DateManip
if %errorlevel% neq 0 goto error

echo.
echo * Install Crypt-SSLeay
echo.
set OLDPATH=%PATH%
set PATH=%CD%;%PATH%
echo anything | perl -x -S ppm install http://theoryx5.uwinnipeg.ca/ppms/Crypt-SSLeay.ppd
set errlvlbak=%errorlevel%
set PATH=%OLDPATH%
if %errlvlbak% neq 0 goto error

echo.
echo * Install Finance-Quote
echo.
perl -x -S ppm install Finance-Quote

echo.
echo * Run gnc-fq-check
echo.
perl -w gnc-fq-check
if %errorlevel% neq 0 goto error

set OLDTZ=%TZ%
set TZ
if %errorlevel% neq 1 goto cfqh
set TZ=UTC
:cfqh
echo.
echo * Run gnc-fq-helper
echo.
echo (yahoo "AMZN") | perl -w gnc-fq-helper
set errlvlbak=%errorlevel%
set TZ=%OLDTZ%
if %errlvlbak% neq 0 goto error

echo.
echo * Installation succeeded
echo.
goto end

:error:
echo.
echo An error occured, see above.
echo.

:end
pause
