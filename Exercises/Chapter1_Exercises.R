##Chapter 1 Exercises

##Exercise 1.1
#When computing tail probabilities such as p(X>a)
#you can use pnorm and pbinom

##Helpful scripts for downloading R files for Modern Stats for Modern Biol Text Book
#Run on CMD
##wget -r -np -nH --cut-dirs=3 web.stanford.edu/class/bios221/book/Rfiles/

##Helpful Script for renaming directories on Git
#git mv oldfilename newfilename


##Exercise 1.2
##dbinom(X, n, p)
library(epitools)
#Calcualte the probability mass distribution of X=2, B(10,0.3)
prob <- dbinom(0:10, prob = 0.3, size = 10)
round(prob, 2)

#Calculate the cumulative distribution at the value 2, corresponding to P(X<=2)
dbinom(0, size = 10, prob=0.3) +
dbinom(1, size =10, prob =0.3) +
dbinom(2, size =10, prob = 0.3)

#Alternatively, we can use the cumulative probability function for the binomial distribution pbinom
pbinom(2, size=10, prob=0.3)

#Answer: The probability of 2 or less by random in a sample size of 10 is ~38%

##Exercise 1.3
#Poisson=(x, lamda)
##Difficulties with this one...

##Exercise 1.4
#And this one due to above statement

##Exercise 1.5
#Use a simulation to find the probability of having a maximum of 9 or larger in 100 trials
maxes <- replicate(10000000, {
  max(rpois(100,0.5))
})
table(maxes)
#Approximation for P[Xmax >=9]
mean(maxes >=9)

#P(X<= x) is the cumulative distribution function
?ppois
#Calculates the probability P[X>x]
ppois(9, 0.5, lower.tail = FALSE)

##Exercise 1.6
##Look at distributions in R
?Distributions
#dbeta: Beta Distribution; Continuous
?dbeta
x_beta <- seq(0, 1, by = 0.02)
y_beta <- dbeta(x_beta, shape1 = 2, shape2 = 5)
plot(y_beta)
#dbinomial:Binomial Distribution; Discrete
x <- dbinom(0:10, 10, 0.5)
barplot(x)
#dcauchy: Normal Distribution; Continuous
x_dcauchy <- seq(0, 1, by = 0.02)
y_dcauchy <- dcauchy(x_dcauchy, scale = 5)
plot(y_dcauchy)
#dchisq: Chi Square Distribution; Continuous
?dchisq
x_dchisq <- seq(0, 20, by = 0.5)
y_dchisq <- dchisq(x_dchisq, df = 5)
plot(y_dchisq)
#dexp: Exponential; Continuous 
#dgamma:Tukey Distribution; Continuous
#dgeom: Geometric Distribution; Discrete
#dhyper: Hypergeometric Distribution; Discrete
#dlnorm: Log Normal Distribution; Continuous
#dmultinom: Multinomial Distribution; Discrete
#dnbinom: Negative Bionomial Distribution;
#dnorm: Continuous
#dpois: Discrete
#dt: Student's t Distribution; Continuous
#dunif: Uniform Distribution; Continuous
#dweibull: Weibull Distribution; Continuous

##Exercise 1.7
x <- rpois(100, 0.3)
mean(x)
var(x)

##Exercise 1.8
#if (!requireNamespace("BiocManager", quietly = TRUE))
  + install.packages("BiocManager")

#Load C. elegans Genome
library(BSgenome.Celegans.UCSC.ce2)
library(Biostrings)

#View C. elegans genome for sequence names
Celegans

#Explore nucleotide frequencies of chrM
LF <- letterFrequency(Celegans$chrM, letters = "ACGT", OR=0)
LF
lengthC <- sum(LF)
LF / lengthC


#Test whether C. elegans data is consistent with the uniform model
#Under Null
prob = rep(0.25, 4)
obsunder0 <- rmultinom(1, lengthC, p = prob)
dim(obsunder0)
t(obsunder0)

#Expected
##Expected value = 13794/4; 3448.5
expected0 = prob * 13794
expected0

#Difficulties with this one...had to look at answers for guidance
oenull = replicate(10000, oestat(e = expected0, o = rmultinom(1, n, p = prob)))
oenull

#Histogram of the null distribution
hist(oenull, breaks = 100, col = "lightblue")
?hist
