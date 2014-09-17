setwd('/Users/ivan/Work_directory/Data-Science-Regression-Models')
Consider the space shuttle data ?shuttle in the MASS library. 
Consider modeling the use of the autolander as the outcome (variable name use). 
Fit a logistic regression model with autoloader (variable auto) use (labeled as "auto" 1) 
versus not (0) as predicted by wind sign (variable wind). 
Give the estimated odds ratio for autoloader use comparing head winds, 
labeled as "head" in the variable headwind (numerator) to tail winds (denominator).

require(MASS)
data(shuttle)
str(shuttle)

