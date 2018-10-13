# t-Distribution Exercises

# Exercise 1 - Using the t-Distribution
# We know that, with a normal distribution, only 5% of values are more than 2
#    standard deviations away from the mean.
#
# Calculate the probability of seeing t-distributed random variables being more
#    than 2 in absolute value when the degrees of freedom are 3.
#
# INSTRUCTIONS
# Use the pt function to calculate the probability of seeing a value less than
#    or equal to the argument.

1 - pt( 2, 3 ) + pt( -2, 3 )



# Exercise 2 - Plotting the t-distribution
# Now use sapply to compute the same probability for degrees of freedom from
#    3 to 50.
#
# Make a plot and notice when this probability converges to the normal
#    distribution's 5%.
#
# INSTRUCTIONS
# Make a vector called df that contains a sequence of numbers from 3 to 50.
# Using function, make a function called pt_func that recreates the calculation
#    for the probability that a value is greater than 2 as an absolute value for
#    any given degrees of freedom.
# Use sapply to apply the pt_func function across all values contained in df.
#    Call these probabilities probs.
# Use the plot function to plot df on the x-axis and probs on the y-axis.

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq( 3, 50 )

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom
pt_func <- function( x ) {
    1 - pt( 2, x ) + pt( -2, x )
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply( df, pt_func )

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot( df, probs )



# Exercise 3 - Sampling From the Normal Distribution
# In a previous section, we repeatedly took random samples of 50 heights from a
#    distribution of heights. We noticed that about 95% of the samples had confidence
#    intervals spanning the true population mean.
#
# Re-do this Monte Carlo simulation, but now instead of N=50, use N=15. Notice
#    what happens to the proportion of hits.
#
# INSTRUCTIONS
# Use the replicate function to carry out the simulation. Specify the number of
#    times you want the code to run and, within brackets, the three lines of code
#    that should run.
# First use the sample function to randomly sample N values from x.
# Second, create a vector called interval that calculates the 95% confidence
#    interval for the sample. You will use the qnorm function.
# Third, use the between function to determine if the population mean mu is
#    contained between the confidence intervals.
# Save the results of the Monte Carlo function to a vector called res.
# Use the mean function to determine the proportion of hits in res

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
    .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate( B, {
    X <- sample( x, N, replace = TRUE )
    interval <- mean( X ) + c( -1, 1 ) * qnorm( 0.975 ) * sd( X ) / sqrt( N )
    between( mu, interval[ 1 ], interval[ 2 ] )
} )

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean( res )




# Exercise 4 - Sampling from the t-Distribution
# N=15 is not that big. We know that heights are normally distributed, so the
#    t-distribution should apply. Repeat the previous Monte Carlo simulation
#    using the t-distribution instead of using the normal distribution to construct
#    the confidence intervals.
#
# What are the proportion of 95% confidence intervals that span the actual mean
#    height now?
#
# INSTRUCTIONS
# Use the replicate function to carry out the simulation. Specify the number of
#    times you want the code to run and, within brackets, the three lines of code
#    that should run.
# First use the sample function to randomly sample N values from x.
# Second, create a vector called interval that calculates the 95% confidence
#    interval for the sample. Remember to use the qt function this time to generate
#    the confidence interval.
# Third, use the between function to determine if the population mean mu is
#    contained between the confidence intervals.
# Save the results of the Monte Carlo function to a vector called res.
# Use the mean function to determine the proportion of hits in res.

# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate( B, {
    X <- sample( x, N, replace = TRUE )
    interval <- mean( X ) + c( -1, 1 ) * qt( 0.975, N - 1 ) * sd( X ) / sqrt( N )
    between( mu, interval[ 1 ], interval[ 2 ])
} )

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean( res )




# Exercise 5 - Why the t-Distribution?
# Why did the t-distribution confidence intervals work so much better?
#
# INSTRUCTIONS
# Possible Answers
# The t-distribution takes the variability into account and generates larger
#    confidence intervals.   <-*
# Because the t-distribution shifts the intervals in the direction towards the
#    actual mean.
# This was just a chance occurrence. If we run it again, the CLT will work better.
# The t-distribution is always a better approximation than the normal distribution.

