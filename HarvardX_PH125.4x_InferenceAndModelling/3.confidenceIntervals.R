# Confidence Intervals

# We are ready to learn about confidence intervals.  Confidence intervals are a
#    very useful concept that are widely used by data scientists.
# A version of these that are very commonly seen come from the ggplot geometry
#    geom_smooth().
# Here's an example using some weather data.

library( ggsn )

data( 'nhtemp' )
data.frame( year = as.numeric( time( nhtemp ) ), temperature = as.numeric( nhtemp ) ) %>%
    ggplot( aes( year, temperature ) ) +
    geom_point() +
    geom_smooth() +
    ggtitle( 'Average Yearly Temperatures in New Haven' )

# We write the code like this and we get a picture that looks like this.
# We will later learn how this curve is formed, but note the shaded area around
#    the curve.
# This shaded area is created using the concept of confidence intervals.
# In our competition, we were asked to give an interval.
# If the interval you submit includes the actual proportion p, you get half the
#    money you spent on your poll back and pass to the next stage of the competition.
# One way to pass to the second round of the competition to report a very large
#    interval-- for example, the interval 0 to 1.
# This is guaranteed to include p.
# However, with an interval this big, we have no chance of winning the competition.
# Similarly, if you are an election forecaster and predict the spread will
#    be between negative 100 and 100, you'll be ridiculed for stating the obvious.
# Even a small interval such as saying that the spread will be between minus 10%
#    and 10% will not be considered serious.
# On the other hand, the smaller the interval we report, the smaller our chance
#    of passing to the second round.
# Similarly, a bold pollster that reports very small intervals and misses the
#    mark most of the time will not be considered a good pollster.
# We want to be somewhere in between. Confidence intervals will help us get there.
# We can use the statistical theory we have learned to compute, for any given
#    interval, the probability that it includes p.
# Similarly if we are asked to create an interval with, say, a 95% chance of
#    including p, we can do that as well.
# These are called 95% confidence intervals.
# Note that when pollsters report an estimate and a margin of error, they are,
#    in a way, reporting a 95% confidence interval.
# Let's show how this works mathematically. We want to know the probability that
#    the interval X-bar minus 2 times the estimated standard error to X-bar plus
#    2 times its estimated standard error contains the actual proportion p.
# [ X_ - 2SE( X_ ), X_ + 2SE( X_ )]
# First, note that the start and end of this interval are random variables.
# Every time that we take a sample, they change.
# To illustrate this, we're going to run a Monte Carlo simulation.
# We're going to do it just twice first.
# So we're going to use these parameters.
# We're going to make p 0.45 and N 1,000.

p <- 0.45
N <- 1000

# Note that the interval we get when we write this code is different from what
#    we get if we run that same code again.

X <- sample( c( 0, 1 ), size = N, replace = TRUE, prob = c( 1 - p, p ) )
X_hat <- mean( X )
SE_hat <- sqrt( X_hat * ( 1 - X_hat ) / N )
c( X_hat - 2 * SE_hat, X_hat + 2 * SE_hat )

# If we keep sampling and creating intervals, we will see that this is due to
#    random variation.
# To determine the probability that the interval includes p, we need to compute
#    this probability.
# By subtracting and dividing the same quantities in all parts of the equation,
#    we get that the equation is equivalent to this.
# The term in the middle is an approximately normal random variable with expected
#    value 0 and standard error 1, which we have been denoting with capital Z.
# Pr( X_ - 2SE( X_ ) <= p <= X_ + 2SE( X_ ) )
# Pr( -2 <= ( X_ - p ) / ( SE( X_ )) <= 2 )
# Pr( -2 <= Z <= 2 )

# So what we have is, what is the probability of a standard normal variable being
#    between minus 2 and 2?
# And this is, we know, about 95%.
# So we have a 95% confidence interval.
# Note that if we want to have a larger probability, say 99%, a 99% confidence
#    interval, we need to multiply by whatever z satisfies the following equation.
# Note that by using the quantity that we get by typing this code, which is about
#    2.576, will do it, because by definition the pnorm of what we get when we
#    type qnorm(0.995) is by definition 0.995.

z <- qnorm( 0.995 )
z
pnorm( qnorm( 0.995 ) )

# And by symmetry, pnorm of 1 minus qnorm(0.995) is 1 minus 0.995.
pnorm( 1 - qnorm( 0.995 ) )

# So now we compute pnorm minus pnorm minus z, we get 99%.
pnorm( z ) - pnorm( -z )

# This is what we wanted.
# We can use this approach for any percentile q.
# We use 1 minus (1 minus q) divided by 2.

# 1 - ( 1 - q ) / 2

# Why this number-- because of what we just saw, 1 minus (1 minus q) divided by
#    2 plus (1 minus q) divided by 2 equals q.
# And we get what we want.
# Also note that to get exactly 0.95, we actually use a slightly smaller number
#    than 2.
# How do we know?
# We type qnorm of 0.975 and we see that the value that we should be using to
#    get exactly a 95% confidence interval is 1.96.
qnorm( 0.975 )



# A Monte Carlo simulation for confidence intervals
# We can run a Monte Carlo simulation to confirm that, in fact, a 95% confidence
#    interval includes p 95% of the time.
# We write the simulation like this.
# We're going to write the simulation we've been writing.
# But now, we're going to actually construct the confidence interval inside the
#    call to replicate. And in the very final line, we're going to ask, is p
#    included in the interval.
# We're going to return either true or false.  To compute how often it happened,
#    we compute the mean of that vector of true and false.
# We run a simulation and we get 0.9522. This plot shows you the first few confidence
#    intervals that were generated in our Monte Carlo simulation.  In this case,
#    we created simulations so we know what p is.  In the plot, it's represented
#    with a vertical black line.  Notice that you can see the confidence intervals
#    varying. Each time, they fall in slightly different places. This is because
#    they're random variables.
# We also know that most of the times, p is included inside the confidence interval.
#    p is not moving, of course, because p is not a random variable.
# We also see that, every once in a while, we actually miss p.  These confidence
#    intervals are shown in red.  We should only see about 5% of the intervals
#    in red because they're 95% confidence intervals.
# This plot should help you understand what confidence intervals are and what
#    they mean.

B <- 10000
inside <- replicate( B, {
    X <- sample( c( 0, 1), size = N, replace = TRUE, prob = c( 1 - p, p ) )
    X_hat <- mean( X )
    SE_hat <- sqrt( X_hat * ( 1 - X_hat ) / N )
    between( p, X_hat - 2 * SE_hat, X_hat + 2 * SE_hat )
} )
mean( inside )



# The Correct Language
# When using the theory we just described, it is important to remember that it
#    is the intervals that are at random, not p.
# We showed a plot where we could see the random intervals that were moving around.
#   And we also saw p. p was not moving around.  It was fixed, and it was represented
#   with a vertical line.  It was staying in the same place.  So the 95% relates
#   to the probability that the random interval falls on top of p.  Saying that
#   p has a 95% chance of being between this and that is technically an incorrect
#   statement-- again, because p is not random.



# Power
#  Pollsters do not become successful for providing correct confidence intervals,
#     but rather for predicting who will win.  When we took a sample of size 25,
#     the confidence interval for the spread was-- and we can reconstruct it here--
#     from negative 0.93 to 0.85.
# This includes 0.
N <- 25
X_hat <- 0.48
( 2 * X_hat - 1 ) + c( -2, 2 ) * 2 * sqrt( X_hat * ( 1- X_hat ) / sqrt( N ) )
# If we were pollsters and we were forced to make a declaration about the election,
#    we would have no choice but to say it's a tossup.  A problem with our poll
#    results is that given the sample size and the value of p, we would have to
#    sacrifice on the probability of an incorrect call to create an interval that
#    does not include 0, an interval that makes a call of who's going to win.
# The fact that our interval includes 0, it does not mean that this election is
#    close.  It only means that we have a small sample size. In statistical
#    textbooks, this is called lack of power.  In the context of polls, power can
#    be thought of as the probability of detecting a spread different from 0. By
#    increasing our sample size, we lower our standard error, and therefore have
#    a much better chance of detecting the direction of the spread.




# p-values
# p-values are very common in, for example, the scientific literature. They are
#    related to confidence intervals, so we introduce the concept here.  Let's
#    consider the blue and red bead example again.  Suppose that rather than
#    wanting to estimate the spread or the proportion of blue, I'm interested
#    only in the question, are there more blue beads than red beads?
#        Another way to ask that is, is 2p minus 1 bigger than 0?  2p - 1 > 0?
#        Is the spread bigger than 0?
# So suppose we take a random sample of, say, 100 beads, and we observe 52 blue
#    beads.  This gives us a spread of 4%.  This seems to be pointing to there
#    being more blue beads than red beads, because 4% is larger than 0.
# 52% is larger than 48%.
# However, as data scientists, we need to be skeptical. We know there is chance
#    involved in this process, and we can get a 52 even when the actual spread
#    is 0.  The null hypothesis is the skeptic's hypothesis.  In this case, it
#    would be the spread is 0.
# We have observed a random variable 2 times X-bar minus 1, which in this case
#    is 4%, and the p-value is the answer to the question, how likely is it to
#    see a value this large when the null hypothesis is true?
# 2*X_ - 1 = 0.04
# Pr( | X_ - 0.5 | > 0.02 )
# So we write, what's the probability of X-bar minus 0.5 being bigger than 2%?
#    That's the same as asking, what's the chance that the spread is 4 or more?
#    The null hypothesis is that the spread is 0 or that p is a half.
# Under the null hypothesis, we know that this quantity here, the square root
#    of n times X-bar minus 0.5 divided by the square root of 0.5 times 1 minus 0.5,
#    is a standard normal.
# sqrt( N ) = ( X_ - 0.5 ) / sqrt( 0.5 * ( 1 - 0.5  ) )
# We've taken a random variable and divided it by its standard error after
#    subtracting its expected value.  So we can compute the probability, which is
#    a p-value, using this equation, which reduces to this equation, where z is
#    a standard normal.
# Pr( sqrt( N ) * | X_ - 0.5 | / sqrt( 0.5 * ( 1 -  0.5 ) ) ) > sqrt( N ) * 0.02 / sqrt( 0.5 * ( 1 - 0.5 ) )
# Pr( sqrt( N ) * | ( X_ - 0.5 ) / 0.5 | / 0.5 > Z )
# And now we can use code to compute this.  We compute the probability, which is
#    equal to 69% in this case.

N = 100
z <- sqrt( N ) * 0.02 / 0.5
1 - ( pnorm( z ) - pnorm( -z ) )

# This is the p-value.  In this case, there's actually a large chance of seeing
#    52 blue beads or more under the null hypothesis that there is the same amount
#    of blue beads as red beads.  So the 52 blue beads are not very strong evidence,
#    are not very convincing, if we want to make the case that there's more blue
#    beads than red beads.
# Note that there's a close connection between p-values and confidence intervals.
# If a 95% confidence interval of the spread does not include 0, we can do a little
#    bit of math to see that this implies that the p-value must be smaller than
#    1 minus 95%, or 0.05.
# To learn more about p-values, you can consult any statistics textbook.
# However, in general, we prefer reporting confidence intervals over p-values,
#    since it gives us an idea of the size of the estimate.  The p-value simply
#    reports a probability and says nothing about the significance of the finding
#    in the context of the problem.

