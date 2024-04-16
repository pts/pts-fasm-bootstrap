@echo off
@rem Runs Windows Scripting Host: cscript /nologo folink2c.js %*
set m=%~dpn0.cmd 
for %%f in ("%~n0.cmd") do if not exist "%~dpn0.cmd" set m=%%~dpn$PATH:f.cmd 
if "%m%"==".cmd" echo fatal: program not found: %~n0.cmd 1>&2 & exit /b 1 
for %%f in ("%m%") do set c=%%~dpnf.js
if not exist "%c%" echo fatal: script not found: %c% 1>&2 & exit /b 1 
cscript /nologo "%c%" %*
