REM ******* Run Test Case II *********

 
REM --- Run MCMC to generate parameter samples

 ..\..\..\..\bin\UCODE_2014.exe HO2_MCMC-forward.in HO2

REM ***********************************************


REM --- Run MCMC_PREDICTION to evaluate predictions for generated parameter samples

 ..\..\..\..\bin\UCODE_2014.exe HO2_MCMC-pred.in HO2



PAUSE
