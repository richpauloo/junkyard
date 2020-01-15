
REM  Run UCODE prediction sensitivity run with optimal parameter estimates, and 
REM   all parameters active:

cd ..\02-ModelB-Prediction
"..\..\..\bin\UCODE_2014.exe" 02-ModelB-Prediction_ucode_main.in ModelB-SA-AllActive-WithPrior_ucode ModelB-Prediction_ucode /wait
cd ..\07-ModelB-opr-add

REM  Copy output files from that run that are needed as input files to OPR-PPR 

copy ..\02-ModelB-Prediction\ModelB-Prediction_ucode._dmp .\
copy ..\02-ModelB-Prediction\ModelB-Prediction_ucode._spu .\
pause
