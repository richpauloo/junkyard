
@echo off
rem -----------
rem Use following two lines to input value on command line
rem if "%1"=="" goto NoRunners
rem set N=%1
rem -----------
set N=2

if %N%==0 goto NoRunners

cd ..\runner1\HO1\HO1\HO1\HOmodel_test1\
Echo Starting Runner 1
Start "Runner 1" /min runner

if %N%==1 goto GoHome

cd ..\..\..\..\..\runner2\HO1\HO1\HO1\HOmodel_test1\
Echo Starting Runner 2
Start "Runner 2" /min runner

if %N%==2 goto GoHome

cd ..\..\..\..\..\runner3\HO1\HOmodel_test1\
Echo Starting Runner 3
Start "Runner 3" /min runner


:GoHome
cd ..\..\..\..\..\
goto End

:NoRunners
Echo No Runners started

:End
pause







