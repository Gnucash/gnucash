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
echo   no perl executable found
echo. 
echo Now finding and installing perl.....
REM ----------------------------------------------------------------------------
echo.
echo   * download Strawberry perl
echo.
call cscript//nologo getperl.vbs %TEMP%\Perl.msi
if %errorlevel% equ 0 goto donegetperl
   echo   Return Value: "%errorlevel%"
   echo.
   echo   failed to download perl install file
   echo.
   goto error
:donegetperl
REM ----------------------------------------------------------------------------
echo.
echo   * automated Perl install
echo.
msiexec /qb /l* %TEMP%\perl-log.txt /i %TEMP%\Perl.msi PERL_PATH=Yes PERL_EXT=Yes
if %errorlevel% equ 0 goto doneperlinst
   echo   Return Value: "%errorlevel%"
   echo.
   echo   failed to install perl (%TEMP%\Perl.msi)
   echo.
   goto error
:doneperlinst
perl -v
del  %TEMP%\Perl.msi
REM ----------------------------------------------------------------------------
echo.
echo   * updating PATH variable to include Perl 
echo.
:: delims is a TAB followed by a space
FOR /F "tokens=2* delims=	 " %%A IN ('REG QUERY "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v Path') DO SET NewPath=%%B
ECHO NewPath = %NewPath%
set Path="%NewPath%"
REM ----------------------------------------------------------------------------
echo.
echo * Run gnc-fq-update
echo.
perl -w gnc-fq-update
if %errorlevel% neq 0 goto error
REM ----------------------------------------------------------------------------
:chkver
echo.
echo * Check Perl Version
echo.
perl -e "exit(int($]));"
set _perlmajor=%errorlevel%
perl -e "$ver=1000*sprintf(\"%%.3f\", $]); exit(int($ver)-5000);"
set _perlminor=%errorlevel%
if %_perlmajor% equ 5 (
  if %_perlminor% geq 10 (
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
echo Please install ActivePerl 5.8, or above (http://www.activestate.com/store/activeperl)
echo and add the bin directory to your Path environment variable.
goto error
REM ----------------------------------------------------------------------------
:ccp
echo.
echo * Test for ActivePerl
echo.
perl -e "use Win32;if(defined &Win32::BuildNumber){exit 2;}else{exit 3;};"
REM echo status = %errorlevel%
if %errorlevel% equ 2 (
set bld=AS
goto pchk
)else if %errorlevel% equ 3 (
set bld=Other
goto pchk
)
if %errorlevel% equ 0 goto pchk
goto error
REM ----------------------------------------------------------------------------
:pchk
REM echo.
REM echo * Run gnc-path-check
REM echo.
REM perl -w gnc-path-check
REM if %errorlevel% neq 0 goto error
REM ----------------------------------------------------------------------------
echo * Install DateManip
echo.
if "%bld%" equ "AS" (
perl -x -S ppm install Date-Manip
if %errorlevel% neq 0 (
  perl -x -S ppm install DateManip
  if %errorlevel% neq 0 goto error
)
)else if "%bld%" equ "Other" (
perl -e "use strict;use CPAN;CPAN::Shell->install('Date::Manip');"
if %errorlevel% neq 0 (
echo   failed to install Date::Manip module
goto error
)
)
REM ----------------------------------------------------------------------------
echo.
echo * Install Crypt-SSLeay
echo.


if "%bld%" equ "AS" (
set OLDPATH="%PATH%"
set PATH="%CD%;%PATH%"
if %_perlversion% == 5.6 (
  perl -x -S ppm install http://theoryx5.uwinnipeg.ca/ppmpackages/Crypt-SSLeay.ppd
) else if %_perlversion% == 5.8 (
  echo anything | perl -x -S ppm install http://theoryx5.uwinnipeg.ca/ppms/Crypt-SSLeay.ppd
) else (
  perl -x -S ppm install Crypt-SSLeay
)
set errlvlbak=%errorlevel%
set PATH="%OLDPATH%"
if "%errlvlbak%" neq "0" goto error
)else if "%bld%" equ "Other" (
echo using CPAN install for Crypt::SSLeay
perl -e "use strict;use CPAN;CPAN::Shell->install('Crypt::SSLeay');"
if %errorlevel% neq 0 (
echo   failed to install Crypt::SSLeay module
goto error
)
)
REM ----------------------------------------------------------------------------
REM for some reason a CPAN install of Finance::Quote does not install prequisite
REM package HTML::Treebuilder
echo.
echo * Install HTML-TreeBuilder
echo.
if "%bld%" equ "Other" (
perl -e "use strict;use CPAN;CPAN::Shell->install('HTML::TreeBuilder');"
if %errorlevel% neq 0 (
echo   failed to install HTML::TreeBuilder module
goto error
)
)
REM ----------------------------------------------------------------------------
echo.
echo * Install Finance-Quote
echo.
if "%bld%" equ "AS" (
perl -x -S ppm install Finance-Quote
if %errorlevel% neq 0 goto error
)
if "%bld%" equ "Other" (
perl -e "use strict;use CPAN;CPAN::Shell->install('Finance::Quote');"
if %errorlevel% neq 0 (
echo   failed to install Finance::Quote module
goto error
)
)
REM ----------------------------------------------------------------------------
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

