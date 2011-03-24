rem This is the Windows Batch Script for the daily builds.
rem It simply calls the actual MSYS Shell script to perform
rem the daily build and then the tag builds.

cd c:\soft-2.4\packaging
c:\msys\1.0\bin\sh.exe --login c:\soft-2.4\packaging\daily_build.sh
