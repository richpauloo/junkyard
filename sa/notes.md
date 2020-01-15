when usin priors, add priors to parameters that are LESS sensitive. 1998 paper in slides shows that a 5% change in a less sensitive paramater (Transmisivity) leads to less error in the solution than a 5% change in the more sensitive parameter (recharge)

concerns:

- model calibrated under steady state, but now introducing pumping or other transience
- needed to inject priors for paramaters, not enough data to constrain calibration of model

answer: sensitivity analysis

need to calculate prediciton uncertainy caused by paramater uncertainty

***

when calibrating to streamflow, optimization will be baised to high flows since large deviations are more penalized. Thus, need to weight the low flows equally otherwise model can do wonky stuff.

Map residuals to see if there is spatial bias in your model. 