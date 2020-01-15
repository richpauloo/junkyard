REM ******* Run Test Case I *********

 
REM --- do model calibration to get optimal parameters HO1._paopt and the covariance matrix HO1._mv  
REM --- used in the MCMC run when the command in the MCMC_CONTROLS input block is UseRegResult=yes

 ..\..\..\..\bin\UCODE_2014.exe HO1_calibration.in HO1 


PAUSE
