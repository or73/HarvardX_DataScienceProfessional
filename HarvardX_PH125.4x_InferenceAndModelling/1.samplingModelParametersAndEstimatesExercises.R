
# Exercise 1. Polling - expected value of S
# Suppose you poll a population in which a proportion p of voters are Democrats
#    and 1???p are Republicans. Your sample size is N=25. Consider the random
#    variable S, which is the total number of Democrats in your sample.
#
# What is the expected value of this random variable S?
#
# INSTRUCTIONS

# Possible Answers
# E(S) = 25 ( 1???p )
# E(S) = 25p                 <- *
# E(S) = sqrt( 25p ( 1???p ) )
# E(S) = p



# Exercise 1. Polling - expected value of S
# Suppose you poll a population in which a proportion p of voters are Democrats
#    and 1???p are Republicans. Your sample size is N=25. Consider the random
#    variable S, which is the total number of Democrats in your sample.
#
# What is the expected value of this random variable S?
#
# INSTRUCTIONS

# Possible Answers
# E(S) = 25 ( 1???p )
# E(S) = 25p
# E(S) = sqrt( 25p ( 1???p ) )   <- *
# E(S) = p



# Exercise 3. Polling - expected value of X-bar
# Consider the random variable S/N, which is equivalent to the sample average
#    that we have been denoting as X??. The variable N represents the sample size
#    and p is the proportion of Democrats in the population.
#
# What is the expected value of X???
#
# INSTRUCTIONS
# Possible Answers
# E(X??) = p        <- *
# E(X??) = Np
# E(X??) = N ( 1???p )
# E(X??) = 1???p


# Exercise 4. Polling - standard error of X-bar
# What is the standard error of the sample average, X???
# The variable N represents the sample size and p is the proportion of Democrats in the population.
#
# INSTRUCTIONS
# Possible Answers
# SE(X??) = sqrt( Np (1???p) )
# SE(X??) = sqrt( p (1???p) / N )   <- *
# SE(X??)= sqrt( p ( 1???p ) )
# SE(X??) = sqrt( N )



# Exercise 5. se versus p
# Write a line of code that calculates the standard error se of a sample average
#    when you poll 25 people in the population. Generate a sequence of 100
#    proportions of Democrats p that vary from 0 (no Democrats) to 1 (all Democrats).
#
# Plot se versus p for the 100 different proportions.
#
# INSTRUCTIONS
# Use the seq function to generate a vector of 100 values of p that range from 0 to 1.
# Use the sqrt function to generate a vector of standard errors for all values of p.
# Use the plot function to generate a plot with p on the x-axis and se on the y-axis.

# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq( from = 0, to = 1, length.out = 100 )
p

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt( p * ( 1 -p ) / N )
se
# Plot `p` on the x-axis and `se` on the y-axis
plot( p, se )



# Exercise 6. Multiple plots of se versus p
# Using the same code as in the previous exercise, create a for-loop that
#    generates three plots of p versus se when the sample sizes equal
#    N=25
#    N=100
#    N=1000.
#
# INSTRUCTIONS
# Your for-loop should contain two lines of code to be repeated for three
#    different values of N.
# The first line within the for-loop should use the sqrt function to generate a
#    vector of standard errors se for all values of p.
# The second line within the for-loop should use the plot function to generate
#    a plot with p on the x-axis and se on the y-axis.
# Use the ylim argument to keep the y-axis limits constant across all three
#    plots. The lower limit should be equal to 0 and the upper limit should
#    equal the highest calculated standard error across all values of p and N.

# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for ( data in p ) {
    for ( N in sample_sizes ) {
        se <- sqrt( p * ( 1 -p ) / N )
        plot( p, se, ylim = c(0,0.9 ) )
    }
}



# Exercise 7. Expected value of d
# Our estimate for the difference in proportions of Democrats and Republicans
#    is d=X?????(1???X??).
#
# Which derivation correctly uses the rules we learned about sums of random
#    variables and scaled random variables to derive the expected value of d?
#
# INSTRUCTIONS
# Possible Answers
# E[X?????(1???X??)]=E[2X?????1] =2E[X??]???1 =N(2p???1) =Np???N(1???p)
# E[X?????(1???X??)]=E[X?????1] =E[X??]???1 =p???1
# E[X?????(1???X??)]=E[2X?????1] =2E[X??]???1 = 2 sqrt( p ( 1???p ) ) ??? 1 = p ??? ( 1???p )
# E[X?????(1???X??)]=E[2X?????1] =2E[X??]???1 = 2p???1 = p ??? ( 1???p )    <- *



# Exercise 8. Standard error of d
# Our estimate for the difference in proportions of Democrats and Republicans
#    is d=X?????(1???X??).
#
# Which derivation correctly uses the rules we learned about sums of random
#    variables and scaled random variables to derive the standard error of d?
#
# INSTRUCTIONS
# Possible Answers
# SE[X?????(1???X??)]=SE[2X?????1] = 2 SE[X??] = 2 sqrt( p/N )
# SE[X?????(1???X??)]=SE[2X?????1] = 2 SE[X?????1] = 2 sqrt( p ( 1???p ) / N )???1
# SE[X?????(1???X??)]=SE[2X?????1] =2SE[X??] =2 sqrt( p ( 1???p ) / N )     <- *
# SE[X?????(1???X??)]=SE[X?????1] =SE[X??] = sqrt( p ( 1???p ) / N )



# Exercise 9. Standard error of the spread
# Say the actual proportion of Democratic voters is p=0.45. In this case, the
#    Republican party is winning by a relatively large margin of d=???0.1, or a
#    10% margin of victory. What is the standard error of the spread 2X?????1 in
#    this case?
#
# INSTRUCTIONS
# Use the sqrt function to calculate the standard error of the spread 2X?????1.
# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.

2 * sqrt( p * ( 1- p ) / N )


# Exercise 10. Sample size
# So far we have said that the difference between the proportion of Democratic
#    voters and Republican voters is about 10% and that the standard error of
#    this spread is about 0.2 when N=25. Select the statement that explains why
#    this sample size is sufficient or not.
#
# INSTRUCTIONS
# Possible Answers
# This sample size is sufficient because the expected value of our estimate
#    2X?????1 is d so our prediction will be right on.
# This sample size is too small because the standard error is larger than
#    the spread.      <- *
# This sample size is sufficient because the standard error of about 0.2 is
#    much smaller than the spread of 10%.
# Without knowing p, we have no way of knowing that increasing our sample size
#    would actually improve our standard error.





