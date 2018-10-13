# Poll Aggregators Exercises



# Exercise 1 - Heights Revisited
# We have been using urn models to motivate the use of probability models. However,
#    most data science applications are not related to data obtained from urns.
#    More common are data that come from individuals. Probability plays a role
#    because the data come from a random sample. The random sample is taken from
#    a population and the urn serves as an analogy for the population.
#
# Let's revisit the heights dataset. For now, consider x to be the heights of all
#    males in the data set. Mathematically speaking, x is our population. Using
#    the urn analogy, we have an urn with the values of x in it.
#
# What are the population average and standard deviation of our population?
#
# INSTRUCTIONS
# Execute the lines of code that create a vector x that contains heights for all
#    males in the population.
# Calculate the average of x.
# Calculate the standard deviation of x.

# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
    .$height

# Calculate the population average. Print this value to the console.
mean( x )

# Calculate the population standard deviation. Print this value to the console.
sd( x )



# Exercise 2 - Sample the population of heights
# Call the population average computed above μ and the standard deviation σ. Now
#    take a sample of size 50, with replacement, and construct an estimate
#    for μ and σ.
#
# INSTRUCTIONS
# Use the sample function to sample N values from x.
# Calculate the mean of the sampled heights.
# Calculate the standard deviation of the sampled heights.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample( x, size = N, replace = TRUE  )

# Calculate the sample average. Print this value to the console.
mean( X )

# Calculate the sample standard deviation. Print this value to the console.
sd( X )





# Exercise 3 - Sample and Population Averages
# What does the central limit theory tell us about the sample average and how it is related to μ, the population average?
#
# INSTRUCTIONS
# Possible Answers
# It is identical to μ.
# It is a random variable with expected value μ and standard error σ/ sqrt( N )   <- *
# It is a random variable with expected value μ and standard error σ.
# It underestimates μ.





# Exercise 4 - Confidence Interval Calculation
# We will use X¯ as our estimate of the heights in the population from our sample
#    size N. We know from previous exercises that the standard estimate of our
#    error X¯−μ is σ/N‾‾√.
#
# Construct a 95% confidence interval for μ.
#
# INSTRUCTIONS
# Use the sd and sqrt functions to define the standard error se
# Calculate the 95% confidence intervals using the qnorm function. Save the lower
#    then the upper confidence interval to a variable called ci.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)
X
# Define `se` as the standard error of the estimate. Print this value to the console.
X_hat <- mean( X )
se <- sd( X ) / sqrt( N )
se

# Construct a 95% confidence interval for the population average based on our sample.
#    Save the lower and then the upper confidence interval to a variable called `ci`.
Q <- qnorm( 0.975 )
ci <- c( X_hat - Q * se, X_hat + Q * se )
ci




# Exercise 5 - Monte Carlo Simulation for Heights
# Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals
#    as you have just done. What proportion of these intervals include μ?
#
# INSTRUCTIONS
# Use the replicate function to replicate the sample code for B <- 10000 simulations.
#    Save the results of the replicated code to a variable called res. The replicated
#    code should complete the following steps: -1. Use the sample function to sample
#    N values from x. Save the sampled heights as a vector called X. -2. Create
#    an object called interval that contains the 95% confidence interval for each
#    of the samples. Use the same formula you used in the previous exercise to
#    calculate this interval. -3. Use the between function to determine if μ is
#    contained within the confidence interval of that simulation.
# Finally, use the mean function to determine the proportion of results in res that
#    contain mu.

# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result
#    after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals
#    that contain mu
res <- replicate( B, {
    X <- sample( x, size = N, replace = TRUE )
    se <- sd( X ) / sqrt( N )
    X_hat <- mean( X )
    Q <- qnorm( 0.975 )
    interval <- c( X_hat - Q * se, X_hat + Q * se )

    between( mu, interval[ 1 ], interval[ 2 ] )
} )

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean( res )




# Exercise 6 - Visualizing Polling Bias
# In this section, we used visualization to motivate the presence of pollster bias
#    in election polls. Here we will examine that bias more rigorously. Lets consider
#    two pollsters that conducted daily polls and look at national polls for the
#    month before the election.
#
# Is there a poll bias? Make a plot of the spreads for each poll.
#
# INSTRUCTIONS
# Use ggplot to plot the spread for each of the two pollsters.
# Define the x- and y-axes usingusing aes() within the ggplot function.
# Use geom_boxplot to make a boxplot of the data.
# Use geom_point to add data points to the plot.

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>%
    filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
               enddate >= "2016-10-15" &
               state == "U.S.") %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head( polls, 1 )
# Make a boxplot with points of the spread for each pollster
polls %>% ggplot( aes( pollster, spread ) ) +
    geom_boxplot() +
    geom_point()




# Exercise 7 - Defining Pollster Bias
# The data do seem to suggest there is a difference between the pollsters. However,
#    these data are subject to variability. Perhaps the differences we observe are
#    due to chance. Under the urn model, both pollsters should have the same expected
#    value: the election day difference, d.
#
# We will model the observed data Yij in the following way:
#
#     Yij=d+bi+εij
# with i=1,2 indexing the two pollsters, bi the bias for pollster i, and εij poll
#    to poll chance variability. We assume the ε are independent from each other,
#    have expected value 0 and standard deviation σi regardless of j.
#
# Which of the following statements best reflects what we need to know to determine
# if our data fit the urn model?
#
# INSTRUCTIONS
# Possible Answers
# Is εij=0?
# How close are Yij to d?
# Is b1≠b2?    <- *
# Are b1=0 and b2=0?





# Exercise 8 - Derive Expected Value
# We modelled the observed data Yij as:
#
#     Yij=d+bi+εij
# On the right side of this model, only εij is a random variable. The other two
#    values are constants.
#
# What is the expected value of Y1j?
#
# INSTRUCTIONS
# Possible Answers
# d+b1   <- *
# b1+εij
# d
# d+b1+εij




# Exercise 9 - Expected Value and Standard Error of Poll 1
# Suppose we define Y¯1 as the average of poll results from the first poll and
#    σ1 as the standard deviation of the first poll.
#
# What is the expected value and standard error of Y¯1?
#
# INSTRUCTIONS
# Possible Answers
# The expected value is d+b1 and the standard error is σ1
# The expected value is d and the standard error is σ1/sqrt( N1 )
# # The expected value is d+b1 and the standard error is σ1/sqrt( N1 )   <-*
# The expected value is d and the standard error is σ1+sqrt( N1 )




# Exercise 10 - Expected Value and Standard Error of Poll 2
# Now we define Y¯2 as the average of poll results from the second poll.
#
# What is the expected value and standard error of Y¯2?
#
# INSTRUCTIONS
# Possible Answers
# The expected value is d+b2 and the standard error is σ2
# The expected value is d and the standard error is σ2/sqrt( N2 )
# The expected value is d+b2 and the standard error is σ2/sqrt( N2 )   <- *
# The expected value is d and the standard error is σ2+sqrt( N2 )




# Exercise 11 - Difference in Expected Values Between Polls
# Using what we learned by answering the previous questions, what is the expected
#    value of Y¯2−Y¯1?
#
# INSTRUCTIONS
# Possible Answers
# (b2−b1)2
# b2−b1/(√N)
# b2+b1
# b2−b1   <-*




# Exercise 12 - Standard Error of the Difference Between Polls
# Using what we learned by answering the questions above, what is the standard
#    error of Y¯2−Y¯1?
#
# INSTRUCTIONS
# Possible Answers
# sqrt( σ22/N2+σ21/N1 )   <-*
# sqrt( σ2/N2+σ1/N1 )
# (σ22/N2+σ21/N1)^2
# σ2^2/N2+σ1^2/N1




# Exercise 13 - Compute the Estimates
# The answer to the previous question depends on σ1 and σ2, which we don't know.
#    We learned that we can estimate these values using the sample standard deviation.
#
# Compute the estimates of σ1 and σ2.
#
# INSTRUCTIONS
# Group the data by pollster.
# Summarize the standard deviation of the spreads for each of the two pollsters.
# Store the pollster names and standard deviations of the spreads (σ) in an object
#    called sigma

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls, 1)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- data.frame( polls %>% select( pollster, spread ) %>%
                         mutate( s = sd( spread ) ) %>%
                         group_by( pollster ) %>%
                         summarize( sd( spread ) ) )


# Print the contents of sigma to the console
sigma




# Exercise 14 - Probability Distribution of the Spread
# What does the central limit theorem tell us about the distribution of the differences
#    between the pollster averages, Y¯2−Y¯1?
#
# INSTRUCTIONS
# Possible Answers
# The central limit theorem cannot tell us anything because this difference is
#    not the average of a sample.
# Because Yij are approximately normal, the averages are normal too.
# If we assume N2 and N1 are large enough, Y¯2 and Y¯1, and their difference,
#    are approximately normal.   <-*
# These data do not contain vectors of 0 and 1, so the central limit theorem does
#    not apply.




# Exercise 15 - Calculate the 95% Confidence Interval of the Spreads
# We have constructed a random variable that has expected value b2−b1, the pollster
#    bias difference. If our model holds, then this random variable has an approximately
#    normal distribution. The standard error of this random variable depends on
#    σ1 and σ2, but we can use the sample standard deviations we computed earlier.
#    We have everything we need to answer our initial question: is b2−b1 different
#    from 0?
#
# Construct a 95% confidence interval for the difference b2 and b1. Does this interval
#    contain zero?
#
# INSTRUCTIONS
# Use pipes %>% to pass the data polls on to functions that will group by pollster
#    and summarize the average spread, standard deviation, and number of polls per
#    pollster.
# Calculate the estimate by subtracting the average spreads.
# Calculate the standard error using the standard deviations of the spreads and
#    the sample size.
# Calculate the 95% confidence intervals using the qnorm function. Save the lower
#    and then the upper confidence interval to a variable called ci.

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by( pollster ) %>% summarize( avg = mean( spread ), s = sd( spread ), N = n() )
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- max( res$avg ) - min( res$avg )
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
res1_sd <- res$s[ 1 ]
res2_sd <- res$s[ 2 ]
N1 <- res$N[ 1 ]
N2 <- res$N[ 2 ]

se_hat <- sqrt( res1_sd^2 / N1 + res2_sd^2 / N2 )
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c( estimate - qnorm( .975 ) * se_hat, estimate + qnorm( .975 ) * se_hat )
ci




# Exercise 16 - Calculate the P-value
# The confidence interval tells us there is relatively strong pollster effect
#    resulting in a difference of about 5%. Random variability does not seem to
#    explain it.
#
# Compute a p-value to relay the fact that chance does not explain the observed
#    pollster effect.
#
# INSTRUCTIONS
# Use the pnorm function to calculate the probability that a random value is larger
#    than the observed ratio of the estimate to the standard error.
# Multiply the probability by 2, because this is the two-tailed test.

# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>%
    summarize(avg = mean(spread), s = sd(spread), N = n())

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
p <- 1 - ( pnorm( estimate / se_hat ) - pnorm( -estimate / se_hat ) )
p




# Exercise 17 - Comparing Within-Poll and Between-Poll Variability
# We compute statistic called the t-statistic by dividing our estimate of b2−b1
#    by its estimated standard error:
#
#     ( Y¯2−Y¯1) / sqrt( s2^2 / N2 + s2^2 / N1 )
# Later we learn will learn of another approximation for the distribution of this
#    statistic for values of N2 and N1 that aren't large enough for the CLT.
#
# Note that our data has more than two pollsters. We can also test for pollster
#    effect using all pollsters, not just two. The idea is to compare the variability
#    across polls to variability within polls. We can construct statistics to test
#    for effects and approximate their distribution. The area of statistics that
#    does this is called Analysis of Variance or ANOVA. We do not cover it here,
#    but ANOVA provides a very useful set of tools to answer questions such as:
#    is there a pollster effect?
#
# Compute the average and standard deviation for each pollster and examine the
#    variability across the averages and how it compares to the variability within
#    the pollsters, summarized by the standard deviation.
#
# INSTRUCTIONS
# Group the polls data by pollster.
# Summarize the average and standard deviation of the spreads for each pollster.
# Create an object called var that contains three columns: pollster, mean spread,
#    and standard deviation.
# Be sure to name the column for mean avg and the column for standard deviation s.

# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>%
    filter(enddate >= "2016-10-15" &
               state == "U.S.") %>%
    group_by(pollster) %>%
    filter(n() >= 5) %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
    ungroup()
polls
# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <- polls %>%
    group_by( pollster ) %>%
    summarize( avg = mean( spread ), s = sd( spread ) )
var
