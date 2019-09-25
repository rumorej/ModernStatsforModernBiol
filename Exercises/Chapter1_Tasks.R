#Chapter 0-1 Tasks

#Work through Tasks/ Questions listed in Chapter 1

##Note 
#dbinom provides the probability of getting a result for a specific point on the binomial distribution
#pbinom calculates the cumulative probability of getting a result equal to or below that point on the distribution


#Look at the factor function
?factor

#Bernoulli Trials
#rbinom(n= #of trials, prob = probability, size = 1[always the case for bernoulli])

#Question 1.2
rbinom(15, 1, 0.5)
#Successes and failures can have unequal probabilities in a Bernoulli trial, as long as the probabilities sum to 1

rbinom(12, 1, 2/3)
# 1 indicates successes (i.e., landed in right-hand box) and 0 zero denotes left-hand box

#Binomial Succes Counts
#Only care about how many balls go into right-hand box
#Order doesn't matter

rbinom(1, prob = 2/3, size = 12)

#Number of successes in a Bernoulli trial with a probability of success of 0.3 is called a binomial random variable
#B(15, 0.3)

set.seed(235569515)
rbinom(1, prob = 0.3, size = 15)
# Question 1.3: Repeat Function 10 times and report most common outcome
#Most common outcome was 5

#Probability Mass Distribution
pvec <- dbinom(0:15, prob = 0.3, size = 15)
round(pvec,2)

#Plot the prob dist
barplot(pvec, names.arg = 0:15, col = "purple")

#Question 1.4
dbinom(4, size = 4, prob = 2/3)

#Poisson Distribution
#Use when the probability of succes p is small and the number of trials n is large; can approx. the binomal dist

#Question 1.5
dbinom(0:12, size = 10000, prob = 0.0005)
Mu <- 10000 * 0.0005
dpois(0:12, Mu)

#Simulate a mutation process along 10,000 positions with a mutations rate of 5x10^-4
rbinom(1, size = 10000, prob = 0.0005)
simulate <- rbinom(n = 300000, prob = 0.0005, size = 10000)
barplot(table(simulate), col = "green")

#Examine a collection of 50 patients
rbinom(0:100, size = 1, prob = 0.01)
rbinom(0:100, size = 50, prob = 0.01)

#Task
p <- 0.01
n <- 50

dbinom(n, 1, p)
dpois(n, 0.5)

#Task
#ppois(6, 0.5, lower.tail = FALSE)
#if lower.tail is TRUE (default), probabilities are P[X ≤ x], otherwise, P[X > x] if FALSE


##Multinomial
#More than two outcomes

##Generate a random number variable with four levels
?runif
pvec <- c(1/8, 3/8, 3/8, 1/8)
svec <- sample(4, prob = pvec)
runif(n = svec, min = 0, max = 1)

dmultinom(c(4, 2, 0, 0), prob = rep(0.25, 4))
pvec <- rep(1/4, 4)
pvec
t(rmultinom (1, prob = pvec, size = 8))

#Question 1.8
#t above means transpose
(rmultinom(1, prob = pvec, size = 8))

#Question 1.9
#Turns into a Bernoulli Trial
rmultinom(n = 8, prob = pvec, size = 1)

#Determing the Power of a Test
#Generate a 1000 simulated instances
pvecA <- c(3/8, 1/4, 3/12, 1/8)
Obs <- rmultinom(1000, prob = pvecA, size = 20)
dim(Obs)
#Returns the number of columns and rows of a data frame or matrix
#First number reflects the number of columns and the second number reflects the number of rows
?dim

Obs[, 1:7]
?apply
#Perfroms redunant functions on a matrix or data frame
#apply(x, margin, function)
#Margin1 = rows, Margin2 = columns
apply(Obs, 1, mean)

Exp <- pvecA * 20
Exp

stat <- function(obsvd, exptd = 20 * pvec) {
  sum((obsvd - exptd)^2 / exptd)
}

stat(Obs[, 1])
S1 <- apply(Obs, 2, stat)
q95
