# Lecture 1

## Bayesian data analysis

Probabilty to describe uncertainty  

* extends ordinary logic (true/false) to continuous plausibility  
* Computationally difficult: MCMC to the rescue  
* Bayesian data analysis summed up: *Count all the ways data can happen according to assumptions. Assumptions with more ways that are consistent with data are more plausible.*  

Counts -> plausibility (probabilities - normalized counts)  
* things that can happen more ways are more plausible  
* plausibilit is *probability*: set of non-negative real number that sum to one  
* probability theory is just a ser of shortcuts for counting probabilities  


## Multi-level models

Models with multiple levels of uncertainty  

* replace parameters with models  

Common uses:  

* repeat and imbalanced sampling  
* study variation  
* avoid averaging  


## Model comparison

Instead of falsifying a **null** model, compare **meaningful** models  

Basic problems: 

* overfitting  
* causal inference  

Occam's razor is silly, information theory less silly (AIC, WIC, cross-validation)  

Must distingush prediction from inference  

## Bayesian updating

* update posterior probability with multiplication for new count data  
* can update with a prior probability


## Model building: 

1. Design the model (data story)  
2. Condition on the data (update)  
3. Evaluate the model (critique)  

Iterate.  












