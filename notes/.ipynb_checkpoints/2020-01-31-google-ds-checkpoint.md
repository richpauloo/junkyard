Google DS interview prep
================

  - [Collecting Data](#collecting-data)
      - [Types of Bias](#types-of-bias)
      - [Data collection strategies](#data-collection-strategies)
      - [Types of Studies](#types-of-studies)
      - [Simpson’s paradox](#simpsons-paradox)
      - [Variables](#variables)
      - [Principles of Experimental
        Design](#principles-of-experimental-design)
  - [Probability](#probability)
      - [Set notation](#set-notation)
          - [Set operations](#set-operations)
          - [Conditional probability](#conditional-probability)
          - [Independent events](#independent-events)
          - [Review](#review)
          - [Bayes Theorem](#bayes-theorem)
  - [Probability Distributions](#probability-distributions)
      - [Expected value and variance of a discrete random
        variable](#expected-value-and-variance-of-a-discrete-random-variable)
      - [Binomial random variables](#binomial-random-variables)
      - [A/B testing](#ab-testing)
      - [Confidence intervals](#confidence-intervals)
  - [Statistical Learning](#statistical-learning)
      - [Supervised v Unsupervised
        learning](#supervised-v-unsupervised-learning)
      - [Bias-Variance tradeoff](#bias-variance-tradeoff)
  - [Linear Regression](#linear-regression)
      - [Simple linear regression](#simple-linear-regression)
      - [Multiple linear regression](#multiple-linear-regression)
      - [Regression considerations](#regression-considerations)
  - [Classification](#classification)
      - [Logistic regression](#logistic-regression)
      - [Discriminant Analysis](#discriminant-analysis)
      - [KNN](#knn)
  - [Resampling methods](#resampling-methods)
      - [Cross-validation](#cross-validation)
      - [Bootstrap](#bootstrap)
  - [Linear model selection and
    Regularization](#linear-model-selection-and-regularization)
      - [Subset selection](#subset-selection)
      - [Shrinkage methods](#shrinkage-methods)
      - [Dimension reduction methods](#dimension-reduction-methods)
      - [Considerations in higher
        dimensions](#considerations-in-higher-dimensions)
  - [Beyond linearity](#beyond-linearity)
      - [Polynomial regression](#polynomial-regression)
      - [Step functions](#step-functions)
      - [Basis functions](#basis-functions)
      - [Regression Splines](#regression-splines)
      - [Smoothing splines](#smoothing-splines)
      - [Local regression](#local-regression)
      - [Generalized additive models
        (GAMs)](#generalized-additive-models-gams)
  - [Tree based methods](#tree-based-methods)
      - [Classification and regression trees
        (CART)](#classification-and-regression-trees-cart)
      - [Bagging, Random Forests,
        Boosting](#bagging-random-forests-boosting)
  - [Support Vector Machines](#support-vector-machines)
      - [Maximal Margin Classifier](#maximal-margin-classifier)
      - [Support Vector Classifier](#support-vector-classifier)
      - [Support Vector Machines (SVMs)](#support-vector-machines-svms)
      - [SVMs with more than 2 classes](#svms-with-more-than-2-classes)
  - [Unsupervised Learning](#unsupervised-learning)
      - [Principal Components Analysis
        (PCA)](#principal-components-analysis-pca)
      - [Clustering methods](#clustering-methods)
  - [Misc topics](#misc-topics)
      - [Spatial data analysis](#spatial-data-analysis)
      - [Interpolation](#interpolation)
      - [MCMC](#mcmc)
      - [Time series analysis](#time-series-analysis)
      - [bash](#bash)
      - [Git](#git)
      - [R](#r)
  - [Additional resources](#additional-resources)

``` r
library(tidyverse)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

    ## Registered S3 method overwritten by 'rvest':
    ##   method            from
    ##   read_xml.response xml2

    ## ── Attaching packages ──────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.1     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ─────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

Topic hierarchy for x:

1.  What is x in both technical and non-technical terms?  
2.  Why is x relevant to data science, or at Google specifically?
3.  What are the assumptions of x?  
4.  What is the math behind x?  
5.  Can you implement x in code?

# Collecting Data

## Types of Bias

Three types of bias in data collection:

1.  **non-response bias**: a large proportion of those sampled do not
    participate/respond (e.g., you get a 20% completion rate on a
    survey)
2.  **response bias**: when those that respond do not respond truthfully
    (e.g., they say what they think the researcher wants to hear)  
3.  **selection bias**: when the sample population doesn’t reflect the
    true population (e.g., you’re studying domestic well properties, but
    your sample includes both domestic wells and agricultural supply
    wells). Another example of selection bias that relates to 1. is that
    the small proportion that responds to a survey may be a
    self-selecting group, and thus may reflect not reflect the true
    population.

All forms of bias may or may not be observable.

## Data collection strategies

Furthermore, we can sample based on probability or not:

Probability-based sampling:

1.  simple random sample: randomly sample from a population  
2.  stratified random sample: sample equally from strata (e.g., sex,
    age, geographic region)
3.  cluster sample: randomly sample clusters, (e.g., to find salary of
    professors at a school, randomly sample departments, and analyze
    salaries in those clusters)

<!-- end list -->

``` r
# table of people giving age and height
d <- tibble(age    = sample(c(20,30,40),   1000, replace = TRUE),
            height = sample(c(50,60),      1000, replace = TRUE),
            gender = sample(c("M","F"),    1000, replace = TRUE),
            group  = sample(LETTERS[1:10], 1000, replace = TRUE))

# simple random sample of 100 individuals (rows) from d
d[sample(1:1000, 100, replace = FALSE), ]
```

    ## # A tibble: 100 x 4
    ##      age height gender group
    ##    <dbl>  <dbl> <chr>  <chr>
    ##  1    30     60 F      D    
    ##  2    30     50 M      D    
    ##  3    30     50 M      F    
    ##  4    40     50 F      A    
    ##  5    20     60 M      B    
    ##  6    30     60 F      B    
    ##  7    30     50 M      D    
    ##  8    40     60 M      I    
    ##  9    40     60 M      E    
    ## 10    30     50 M      G    
    ## # … with 90 more rows

``` r
# stratified random sample: gender is the strata.
# take 50 random samples from each strata.
split(d, d$gender) %>% 
  lapply(., function(x) x[ sample(1:nrow(x), 50 , replace=FALSE) , ] )
```

    ## $F
    ## # A tibble: 50 x 4
    ##      age height gender group
    ##    <dbl>  <dbl> <chr>  <chr>
    ##  1    30     60 F      H    
    ##  2    20     50 F      F    
    ##  3    20     60 F      J    
    ##  4    20     50 F      D    
    ##  5    20     50 F      D    
    ##  6    40     50 F      C    
    ##  7    40     50 F      J    
    ##  8    20     60 F      H    
    ##  9    40     50 F      C    
    ## 10    20     60 F      J    
    ## # … with 40 more rows
    ## 
    ## $M
    ## # A tibble: 50 x 4
    ##      age height gender group
    ##    <dbl>  <dbl> <chr>  <chr>
    ##  1    40     60 M      B    
    ##  2    20     60 M      H    
    ##  3    30     50 M      C    
    ##  4    30     50 M      C    
    ##  5    30     50 M      A    
    ##  6    20     50 M      H    
    ##  7    20     60 M      B    
    ##  8    20     50 M      J    
    ##  9    40     50 M      F    
    ## 10    40     50 M      B    
    ## # … with 40 more rows

``` r
# cluster sample: groups A:E are the cluster. 
filter(d, group %in% LETTERS[1:5]) 
```

    ## # A tibble: 507 x 4
    ##      age height gender group
    ##    <dbl>  <dbl> <chr>  <chr>
    ##  1    40     50 F      E    
    ##  2    30     50 M      D    
    ##  3    20     50 F      A    
    ##  4    30     50 F      A    
    ##  5    40     60 F      E    
    ##  6    20     60 M      B    
    ##  7    20     60 M      B    
    ##  8    40     50 M      C    
    ##  9    40     50 M      B    
    ## 10    40     50 M      A    
    ## # … with 497 more rows

By contrast, non-probability based methods (convenience sampling and
gathering volunteers) can be easier and cheaper, but is not
representative of the population and is suspect to **selection bias**.

Probability based methods can be generalized to the population via
inference.

## Types of Studies

**Observational**: think EDA. You can draw correlations and speculate
about a relationship, but can’t draw cause and effect.

**Experimental**: an experimental study involves random assignment of a
treatment, and can draw cause and effect.

For example, in an **observational study** we might observe a
relationship between a customer being given a 20% off coupon, and how
much they spend at a grocery store. In an **experimental** study, we
would randomly assign coupons to customers and test for significant
differences in spending between the two groups (e.g., does receiving a
coupon make a customer more likely to purchase more?).

## Simpson’s paradox

Relationship between variables within subgroups can be **reversed** when
the subgroups are combined.

## Variables

  - **response** (dependent): what’s of interest  
  - **predictor** (independent): what’s being used to predict  
  - **lurking variable**: a predictor that’s not in the model but that
    influences the response  
  - **confounding variable**: a predictor in the model which relates to
    other predictors, thus impacting the relationship between variables
      - precisely, in the association of **A** and **B**, **X** is a
        confounder if it is associated with **BOTH** **A** and **B**.

A lurking variable, when included in the study may be discovered to have
a confounding effect. Then we’d call it a confounding variable.

## Principles of Experimental Design

The purpose of experimental design is to ensure that effects observed in
an experiment are likely the result the treatment, rather than by caused
by chance.

  - **control**: control for effects due to factors other than the ones
    of primary interest  
  - **randomization**: random distribution of subjects into groups
    prevents selection bias
      - randomization ensures that the distribution of subjects into
        groups is not biased  
  - **replication**: ensure a sufficient number of subjects per group to
    ensure that differences between groups are detectable
      - replication allows for estimating the uncertainty associated
        with the experiment due to uncontrolled variation
      - increases precision  
  - **stratification**: also called “blocking” means taking measurements
    at different times (e.g., morning and night) if it’s anticipated
    that there might be differences among these periods  
  - **representativeness**: are the subjects representative of the
    population you want to study? I.e., is the study free of bias
    (response, non-response, selection)?

If you can, fix a variable. If you can’t, consider stratifying it. If
you can’t fix or stratify the variable, randomize it.

-----

# Probability

Probability of observing event A is \(P(A)\), and is bounded by 0 and 1:
\(0 \le P(A) \le 1\).

Probability of a not observing A is the compliment, \(P(A')\), also
called \(P(A^c) = 1 - P(A)\).

## Set notation

Used to define the sample space \(S\), the set of all possible outcomes
that may occur. For example, consider the \(S\) for tossing two die:

``` r
expand.grid(1:6, 1:6)
```

    ##    Var1 Var2
    ## 1     1    1
    ## 2     2    1
    ## 3     3    1
    ## 4     4    1
    ## 5     5    1
    ## 6     6    1
    ## 7     1    2
    ## 8     2    2
    ## 9     3    2
    ## 10    4    2
    ## 11    5    2
    ## 12    6    2
    ## 13    1    3
    ## 14    2    3
    ## 15    3    3
    ## 16    4    3
    ## 17    5    3
    ## 18    6    3
    ## 19    1    4
    ## 20    2    4
    ## 21    3    4
    ## 22    4    4
    ## 23    5    4
    ## 24    6    4
    ## 25    1    5
    ## 26    2    5
    ## 27    3    5
    ## 28    4    5
    ## 29    5    5
    ## 30    6    5
    ## 31    1    6
    ## 32    2    6
    ## 33    3    6
    ## 34    4    6
    ## 35    5    6
    ## 36    6    6

``` r
nrow(expand.grid(1:6, 1:6)) # number of outcomes in S
```

    ## [1] 36

The set for all possible combinations of two coin flips is
\(S = \{ HH, HT, TH, TT \}\)

### Set operations

[union, intersection, compliment,
disjoint](https://online.stat.psu.edu/stat500/lesson/2/2.2)

> **Tip**: read the intersection, \(\cap\) as *“AND”*.

For the disjoint set \(A \cap B = \emptyset\), \(P(A \cap B) = 0\).

For the union of events that are not mutually exclusive,
\(P(A \cup B) = P(A) + P(B) - P(A \cap B)\).

### Conditional probability

Dependent events:

Probability of A given B, \(P(A | B) = P(A \cap B) / P(B)\).

Probability of B given A, \(P(B | A) = P(B \cap A) / P(A)\).

Note that usually, \(P(A|B) \ne P(B|A)\).

### Independent events

[Two events are independent if either of the following is
true](https://online.stat.psu.edu/stat500/lesson/2/2.6):

\(P(A \cap B) = P(A) \cdot P(B)\)  
\(P(A|B) = P(A)\) and \(P(B|A) = P(B)\)

### Review

\(P(A \cup B)\) = union = probability of \(A\) or \(B\)  
\(P(A \cap B)\) = intersection = probability of \(A\) and \(B\)  
\(P(A')\) = compliment = probability of NOT \(A\), i.e.,
\(P(B) + P(C) + ... P(N)\)  
\(P(A | B)\) = conditional = probability of \(A\) given \(B\)

And another rule. Can re-write conditional probability (above) as:

\(P(A \cap B) = P(A|B) \cdot P(B)\)

Another helpful rule:

\(P(A \: or \: B) = P(A) + P(B) - P(A \cap B)\)

### Bayes Theorem

\(P(A|B) = \frac{P(B|A) P(A)} {P(B|A) P(A) + P(B|A')P(A')}\)

-----

# Probability Distributions

Binomial (discrete) and normal (continuous) distributions.

Probability **mass** functions \(\longrightarrow\) **discrete**
variables, and \(f(x) = P(X=x)\)

probability **density** functions \(\longrightarrow\) **continuous**
variables, and \(f(x) \ne P(X=x)\)

## Expected value and variance of a discrete random variable

Expected value (mean) is

\(\bar{x} = E(X) = \sum x_i f(x_i)\) where \(f(x_i)\) is the probability
of event \(x_i\), \(P(X=x_i)\).

Variance (\(\sigma^2\)) is
\(\sigma^2 = Var(X) = \sum (x_i - \bar{x})^2 f(x_i)\), also written as
\(\sum x_i^2 f(x_i) - \bar{x}^2\).

Standard deviation \(\sigma\) is simply the root of the variance.

``` r
# x_i = prior convictions, n = number of prisoners
d <- tibble(xi = 0:4, n = c(80,265,100,40,15))

# probability of event x_i, f(x_i)
d$p <- d$n / sum(d$n)

# expected value = sum(x_i * f(x_i))
ev <- sum(d$xi * d$p) 
ev # 1.29 prior convictions (E doesn't need to be a count)
```

    ## [1] 1.29

``` r
# variance = sum(x_i - EV) * f(x_i)
var <- sum((d$xi - ev)^2 * d$p)
var
```

    ## [1] 0.8659

``` r
# standard deviation 
sd <- sqrt(var)
sd
```

    ## [1] 0.9305375

## Binomial random variables

Special case where there are two possible outcomes of a discrete random
variable.

The number of possible samples of size \(k\) from a population of size
\(n\).

For a binomial random variable:

\(P(X=k) = \left( \frac{n}{k} \right) p^k (1-p)^{(n-k)}\)

where

\(\left( \frac{n}{k} \right) = \frac{n!}{k!(n-k)!}\)

## A/B testing

1.  
2.  
3.  
4.  
5.  
## Confidence intervals

1.  
2.  
3.  
4.  
5.  
# Statistical Learning

## Supervised v Unsupervised learning

## Bias-Variance tradeoff

# Linear Regression

## Simple linear regression

## Multiple linear regression

## Regression considerations

# Classification

## Logistic regression

## Discriminant Analysis

## KNN

# Resampling methods

## Cross-validation

## Bootstrap

# Linear model selection and Regularization

## Subset selection

## Shrinkage methods

## Dimension reduction methods

## Considerations in higher dimensions

# Beyond linearity

## Polynomial regression

## Step functions

## Basis functions

## Regression Splines

## Smoothing splines

## Local regression

## Generalized additive models (GAMs)

# Tree based methods

## Classification and regression trees (CART)

## Bagging, Random Forests, Boosting

# Support Vector Machines

## Maximal Margin Classifier

## Support Vector Classifier

## Support Vector Machines (SVMs)

## SVMs with more than 2 classes

# Unsupervised Learning

## Principal Components Analysis (PCA)

## Clustering methods

-----

# Misc topics

## Spatial data analysis

1.  
2.  
3.  
4.  
5.  
## Interpolation

1.  
2.  
3.  
4.  
5.  
## MCMC

1.  
2.  
3.  
4.  
5.  
## Time series analysis

1.  
2.  
3.  
4.  
5.  
Topic hierarchy for frameworks:

1.  What is x?  
2.  Why is x relevant to data science, or at Google specifically?
3.  What are some key features of x?

## bash

1.  
2.  
3.  
## Git

1.  
2.  
3.  
## R

1.  
2.  
3.  
# Additional resources

[Penn State Stat 500](https://online.stat.psu.edu/stat500) [Karl
Browman’s courses](https://kbroman.org/pages/teaching.html) [Karl
Browman’s
Stat 371](https://www.biostat.wisc.edu/~kbroman/teaching/stat371/syllabus.html)
