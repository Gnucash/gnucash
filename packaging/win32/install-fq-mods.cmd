@echo off
setlocal

REM ----------------------------------------------------------------------------
if not exist ssleay32.dll (
  echo.
  echo Did not find ssleay32.dll in current directory.
  echo Please start this cmd file in the bin directory created by the setup.exe.
  goto error
)

REM ----------------------------------------------------------------------------
echo.
echo * Check Perl
echo.
perl -v > NUL 2>&1 
if %errorlevel% equ 0 goto chkver
echo. 
echo   No Perl executable found, attempt to install Strawberry Perl
echo   This may take a while depending on your network speed

REM ----------------------------------------------------------------------------
echo.
echo * Download Strawberry Perl package
echo.
call cscript//nologo getperl.vbs %TEMP%\Perl.msi
if %errorlevel% neq 0 (
   echo   Return Value: "%errorlevel%"
   echo.
   echo   failed to download perl install file
   echo.
   goto error
)

REM ----------------------------------------------------------------------------
echo.
echo * Run automated Perl install
echo.
msiexec /qb /l* %TEMP%\perl-log.txt /i %TEMP%\Perl.msi PERL_PATH=Yes PERL_EXT=Yes
if %errorlevel% neq 0 (
   echo   Return Value: "%errorlevel%"
   echo.
   echo   failed to install perl from %TEMP%\Perl.msi
   echo.
   del  %TEMP%\Perl.msi
   goto error
)
perl -v
del  %TEMP%\Perl.msi

REM ----------------------------------------------------------------------------
echo.
echo * Update PATH variable to include Perl
echo.
:: delims is a TAB followed by a space
FOR /F "tokens=2* delims=	 " %%A IN ('REG QUERY "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v Path') DO SET NewPath=%%B
ECHO NewPath = %NewPath%
set Path=%NewPath%

REM ----------------------------------------------------------------------------
:chkver
echo.
echo * Check Perl version
echo.
perl -e "exit(int($]));"
set _perlmajor=%errorlevel%
perl -e "$ver=1000*sprintf(\"%%.3f\", $]); exit(int($ver)-5000);"
set _perlminor=%errorlevel%
if %_perlmajor% equ 5 (
  if %_perlminor% geq 10 (
    set _perlversion=5.10
    goto pchk
  )
  if %_perlminor% equ 8 (
    set _perlversion=5.8
    goto pchk
  )
REM Note: GnuCash no longer "officially" supports perl 5.6, but as long as it works it will be allowed...
  if %_perlminor% equ 6 (
    set _perlversion=5.6
    goto pchk
  )
)
echo.
echo Found perl version %_perlmajor%.%_perlminor%, but GnuCash requires at least version 5.8.
echo Please install version 5.8 or above of
echo * ActivePerl (http://www.activestate.com/store/activeperl) or
echo * Strawberry Perl (http://code.google.com/p/strawberry-perl/downloads/)
echo and add the bin directory to your Path environment variable.
goto error

REM ----------------------------------------------------------------------------
:pchk
REM echo.
REM echo * Run gnc-path-check
REM echo.
REM perl -w gnc-path-check
REM if %errorlevel% neq 0 goto error

REM ----------------------------------------------------------------------------
echo.
echo * Determine which Perl flavour we have found
echo.
perl -e "use Win32;if(defined &Win32::BuildNumber){exit 2;}else{exit 3;};"
REM echo status = %errorlevel%
if %errorlevel% equ 2 (
  echo   => ActivePerl
  goto inst_mod_as
) else if %errorlevel% equ 3 (
  echo   => Other, probably Strawberry perl ?
  goto inst_mod_oth
) else if %errorlevel% neq 0 goto error

REM ----------------------------------------------------------------------------
:inst_mod_oth
echo.
echo * Install required perl modules
echo.
perl -w gnc-fq-update
if %errorlevel% neq 0 goto error
goto fqchk

REM ----------------------------------------------------------------------------
:inst_mod_as
echo * Install DateManip
echo.
perl -x -S ppm install Date-Manip
if %errorlevel% neq 0 (
  perl -x -S ppm install DateManip
  if %errorlevel% neq 0 goto error
)

REM ----------------------------------------------------------------------------
echo.
echo * Install Crypt-SSLeay
echo.

set OLDPATH=%PATH%
set PATH=%CD%;%PATH%
if %_perlversion% == 5.6 (
  perl -x -S ppm install http://theoryx5.uwinnipeg.ca/ppmpackages/Crypt-SSLeay.ppd
) else if %_perlversion% == 5.8 (
  echo anything | perl -x -S ppm install http://theoryx5.uwinnipeg.ca/ppms/Crypt-SSLeay.ppd
) else (
  perl -x -S ppm install Crypt-SSLeay
)
set errlvlbak=%errorlevel%
set PATH=%OLDPATH%
if "%errlvlbak%" neq "0" goto error

REM ----------------------------------------------------------------------------
echo.
echo * Install Finance-Quote
echo.
perl -x -S ppm install Finance-Quote
if %errorlevel% neq 0 goto error

REM ----------------------------------------------------------------------------
:fqchk
echo.
echo * Run gnc-fq-check
echo.
perl -w gnc-fq-check
if %errorlevel% neq 0 goto error

REM ----------------------------------------------------------------------------
echo.
echo * Run gnc-fq-helper
echo.
echo (yahoo "AMZN") | perl -w gnc-fq-helper
if %errorlevel% neq 0 goto error

REM ----------------------------------------------------------------------------
:success
echo.
echo * Installation succeeded
echo.
goto end

REM ----------------------------------------------------------------------------
:error:
echo.
echo An error occurred, see above.
echo.

REM ----------------------------------------------------------------------------
:end
pause

