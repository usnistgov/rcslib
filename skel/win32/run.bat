setlocal

cd debug

@echo off

set nmlfile=..\..\etc\skel.nml

start skelNmlSvr -n %nmlfile%
rem believe it or not, this is the recommended batch file 'wait' command
ping -n 2 127.0.0.1 > nul

start subMain -n %nmlfile% -b 1 -d 0xFF
start subMain -n %nmlfile% -b 2 -d 0xFF
ping -n 1 127.0.0.1 > nul

start supMain -n %nmlfile% -d 0xFF

rem run this in the foreground, to pause this batch file
echo enter "?" for help, "shutdown" to stop the other processes, "quit"
supShell -n %nmlfile%

rem this one doesn't stop upon shutdown, so kill it manually
taskkill /IM skelNmlSvr.exe > nul

endlocal
