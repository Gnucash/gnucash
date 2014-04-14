rem This is the Windows Batch Script for the daily builds.
rem It simply calls the actual MSYS Shell script to perform
rem the daily build and then the tag builds.

cd c:\soft\gnucash\repos\packaging\win32\

rem Development build (daily)
c:\msys\1.0\bin\sh.exe --login c:\soft\gnucash\repos\packaging\win32\daily_build.sh
rem Tags build for 2.5 and newer (daily -- only tags that weren't built yet)
c:\msys\1.0\bin\sh.exe --login c:\soft\gnucash\repos\packaging\win32\build_tags.sh
rem maintenance branch build (weekly)
rem There's no 2.6 branch yet. Hence the line below is commented out
rem c:\msys\1.0\bin\sh.exe --login c:\soft-maint\gnucash\repos\packaging\weekly_build.sh