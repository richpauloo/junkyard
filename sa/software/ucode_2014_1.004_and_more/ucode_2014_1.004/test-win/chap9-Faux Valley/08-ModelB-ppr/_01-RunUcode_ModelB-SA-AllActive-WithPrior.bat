@echo off

REM  Run UCODE sensitivity analysis run with optimal parameter estimates, and 
REM   all parameters active, and all parameters supplied with prior information:

cd ..\02-ModelB-Prediction
"..\..\..\bin\UCODE_2014.exe" 01-ModelB-SA-AllActive-WithPrior_ucode_main.in ModelB-SA-AllActive-WithPrior_ucode /wait
cd ..\08-ModelB-ppr

REM  Copy output files from that run that are needed as input files to OPR-PPR 

copy ..\02-ModelB-Prediction\ModelB-SA-AllActive-WithPrior_ucode._dm .\
copy ..\02-ModelB-Prediction\ModelB-SA-AllActive-WithPrior_ucode._su .\
copy ..\02-ModelB-Prediction\ModelB-SA-AllActive-WithPrior_ucode._wt .\
copy ..\02-ModelB-Prediction\ModelB-SA-AllActive-WithPrior_ucode._supri .\
copy ..\02-ModelB-Prediction\ModelB-SA-AllActive-WithPrior_ucode._wtpri .\

pause
