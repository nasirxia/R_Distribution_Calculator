
# binomial distribution
# dbinom(x,size,prob.)
dbinom(6, 10, .697)
dbinom(4, 10, .303)
pbinom(2,5,.697)
pbinom(2,5,.697,lower.tail = FALSE)
pbinom(1,5,.697,lower.tail = FALSE)

pbinom(3, 62, 0.1)


dbinom(2,7,0.3)+dbinom(1,7,0.3)+dbinom(0,7,0.3) # (1)
pbinom(2,7,0.3) # (2)
# (1) is equivalent to (2)
# which indicates prob. density of a point(a single event) in the 
# binomial distribution of a binary variable(Bernoulli trial), is equivalent 
# to the probability at the given point(the single event).

# negative binomial distribution
# https://rpubs.com/mpfoley73/458738
# : R function dnbinom(x, size, prob) is the probability of x failures prior to 
# the rth success (note the difference from dbinom) when the probability of success is prob. 
# size = the number of success; x + size= the number of trials

dnbinom(2, 4,.8)

## In the 2 cases above, is the density equivalent to probability 
## (in (negative) binomial distribution)?
## Seems so, check # (1) and # (2)

# Normal Distribution
# In this case, density is conceptually diff. from probability, by design.
# dnorm(x, mean, sd)
# pnorm(x, mean, sd)

1-pnorm(83, 77, 5)
1-pnorm(28, 25, 2.78)
qnorm(0.75, 25, 2.78) - qnorm(0.25, 25, 2.78)
#p-> probability
#q->quantile:
# here this is not z score, just the sample statistic
# it is z score in pt(q,df,lower.tail=TRUE)
# because the mean of t distribution is mean
# like standard normal distribution

# Geometric Distribution
# https://rpubs.com/mpfoley73/458721#:~:text=R%20function%20rgeom(n%2C%20size,tail%20%3D%20TRUE%20).
# R function dgeom(x, prob) is the probability of x failures prior to
# the first success (note the difference) when the probability of success 
# is prob. R function pgeom(q, prob, lower.tail) is the cumulative probability 
# (lower.tail = TRUE for left tail, lower.tail = FALSE for right tail) of 
# less than or equal to q failures prior to success.

dgeom(2,0.75)
0.25^2 * 0.75
1/0.75
sqrt(0.25/(0.75^2))

# Poisson Distribution
dpois(60,75)
dpois(70,75)


# T distribution (parameter:dof)
# pt(q,df,lower.tail = TRUE)

pt(5.3/sqrt(14^2/30+20^2/27),26,lower.tail = FALSE)*2

# Chi-square test
pchisq(15.08,6,lower.tail = FALSE)



# 2-proportion test

# The function below shall be used when the success-failure and independence conditions are met
# also, it is a 2-side test
proportion_diff <- function(n1, n2, ns1, ns2){ 
  # n1, n1 represents the number of success cases
  # ns1, ns2 represents the number of all the cases in each sample
  p_pooled <- (n1+n2)/(ns1+ns2)
  se <- sqrt(p_pooled*(1-p_pooled)*(1/ns1+1/ns2))
  # The pooled proportion is calculated assuming h0 holds
  # (difference of the 2 proportions equals to 0)
  # se is the standard error of the null distribution based on the current 2 samples
  p1 <- n1/ns1
  p2 <- n2/ns2
  ifelse(p1-p2>0, lower.tail <- FALSE, lower.tail <- TRUE)
  # define the parameter in pnorm()
  p_value <- pnorm(p1-p2,0,se,lower.tail = lower.tail)*2
  return(p_value)
}

print(proportion_diff(n1 = 35, n2 = 35 , ns1 = 292, ns2 = 203))

