# Norm Matloff

## Problem

300,000 observations of credit card fraud:

* 299,500 no-fraud (0)
* 500 fraud        (1)

Apply logistic regression, SVM, NN, etc.  

Model predicts all 0s.


## Common Solution

Artifically re-balance the classes: down sample or upsample data

* Downsample: 500 fraud, 500 non-fraud  
* Upsample: bootstrap. Replicate 500 fraud cases  

R Packages:  

* caret
* DMwR2
* imbalance
* mlr3 (machine learning in R: Next Generation, previously mlr)
* ROSE (random oversampling examples)
* etc.  


## How were the data generated?

3 cases:

* A: sample from overall pop: class sizes approx. reflect pop values
* B: sample evenly from each class: known class priors (not subjective Bayesian)
* C: sample evenly from each class, unknown priors


### For Sample Setting A

A simpler, more direct appraoch:

For sampling setting A, don't need to do formal classification. Just flag cases of interest, i.e. those for which P(Y = 1|X) > threshold of interest. 

E.g. credit card fraud, instead of flagging those for which prob > 0.50, may set threshoold at 0.20. 

Could set up formal loss function, etc. -- but no point to it. 

Most packages will output those estimated conditional probabilities as an option. Use them to threshold, like I did in the GEO200CN final class project with RF, GBM, etc.

### For Sample Setting B

Adjust using densities. See `regtools` package.

### For Sample Setting C

Not much we can do. 







