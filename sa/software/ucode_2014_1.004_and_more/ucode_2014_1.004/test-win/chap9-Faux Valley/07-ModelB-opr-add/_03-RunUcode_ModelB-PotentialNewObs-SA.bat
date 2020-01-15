@echo off

REM  Run UCODE sensitivity analysis run for conditions under which potential 
REM   new observations will be collected, with optimal parameter estimates, and 
REM   all parameters active:

cd ModelB-PotentialNewObs
"..\..\..\..\bin\UCODE_2014.exe" 01-ModelB-PotentialNewObs-SA_ucode_main.in ModelB-PotentialNewObs-SA_ucode /wait
cd ..\

REM  Copy output file from that run that is needed as input file to OPR-PPR 

copy ModelB-PotentialNewObs\ModelB-PotentialNewObs-SA_ucode._su .\

pause
