REM ******* Run Test Case I *********

 
REM --- Run MCMC to generate parameter samples

 ..\..\..\..\bin\UCODE_2014.exe HO1_MCMC-forward.in HO1

REM ***********************************************


REM --- Run MCMC_PREDICTION to evaluate predictions for generated parameter samples

 ..\..\..\..\bin\UCODE_2014.exe HO1_MCMC-pred.in HO1



PAUSE
