@echo off

if not exist ssleay32.dll (
  echo.
  echo Did not find ssleay32.dll in current directory.
  echo Please start this batch file in the bin directory created by the setup.exe.
  goto error
)

echo.
echo * Check Perl
echo.
perl -e "$ver=1000*sprintf(\"%%.3f\", $]); exit(int($ver/100)+$ver%%1000);"
if %errorlevel% equ 58 (
  set _haveperl58=1
  goto ccp
)
if %errorlevel% equ 56 (
  set _haveperl58=
  goto ccp
)
echo.
echo Did not find a usable perl.
echo Please install ActivePerl (http://www.activestate.com/store/activeperl)
echo and add the bin directory to your Path environment variable.
goto error
:ccp

echo * Install DateManip
echo.
perl -x -S ppm install DateManip
if %errorlevel% neq 0 goto error

echo.
echo * Install Crypt-SSLeay
echo.
set OLDPATH=%PATH%
set PATH=%CD%;%PATH%
if defined _haveperl58 (
  echo anything | perl -x -S ppm install http://theoryx5.uwinnipeg.ca/ppms/Crypt-SSLeay.ppd
) else (
  perl -x -S ppm install http://theoryx5.uwinnipeg.ca/ppmpackages/Crypt-SSLeay.ppd
)
set errlvlbak=%errorlevel%
set PATH=%OLDPATH%
if %errlvlbak% neq 0 goto error

echo.
echo * Install Finance-Quote
echo.
perl -x -S ppm install Finance-Quote
if %errorlevel% neq 0 goto error

echo.
echo * Run gnc-fq-check
echo.
perl -w gnc-fq-check
if %errorlevel% neq 0 goto error

echo.
echo * Run gnc-fq-helper
echo.
echo (yahoo "AMZN") | perl -w gnc-fq-helper
if %errorlevel% neq 0 goto error

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
