rem run to obtain predictions and their sensitivites
"..\..\bin\UCODE_2014.exe" 05-ex8-Prediction_ucode_main.in ex5.2_ucode ex3-pred_ucode /wait
pause

rem run to obtain confidence intervals on predictions 
"..\..\bin\linear_uncertainty.exe" ex5.2_ucode ex3-pred_ucode /wait
pause
