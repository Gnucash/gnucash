@echo off
setlocal

if not exist ssleay32.dll (
  echo.
  echo Did not find ssleay32.dll in current directory.
  echo Please start this cmd file in the bin directory created by the setup.exe.
  goto error
)

echo.
echo * Check Perl
echo.
perl -e "exit(int($]));"
set _perlmajor=%errorlevel%
perl -e "$ver=1000*sprintf(\"%%.3f\", $]); exit(int($ver)-5000);"
set _perlminor=%errorlevel%
if %_perlmajor% equ 5 (
  if %_perlminor% equ 10 (
    set _perlversion=5.10
    goto ccp
  )
  if %_perlminor% equ 8 (
    set _perlversion=5.8
    goto ccp
  )
  if %_perlminor% equ 6 (
    set _perlversion=5.6
    goto ccp
  )
)
echo.
echo Did not find a usable perl.
echo Please install ActivePerl 5.8 (http://www.activestate.com/store/activeperl)
echo and add the bin directory to your Path environment variable.
goto error
:ccp

echo * Install DateManip
echo.
perl -x -S ppm install Date-Manip
if %errorlevel% neq 0 (
  perl -x -S ppm install DateManip
  if %errorlevel% neq 0 goto error
)

echo.
echo * Install Crypt-SSLeay
echo.
set OLDPATH=%PATH%
set PATH=%CD%;%PATH%
if %_perlversion% == 5.10 (
  perl -x -S ppm install Crypt-SSLeay
) else if %_perlversion% == 5.8 (
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
