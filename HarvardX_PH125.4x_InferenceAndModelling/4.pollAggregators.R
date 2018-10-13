# Poll Aggregators

# In the 2012 presidential election, Barack Obama won the electoral college and
#    he won the popular vote by a margin of 3.9%.
# Let's go back to the week before the election before we knew this outcome.
# Nate Silver was giving Obama a 90% chance of winning.
# Yet, none of the individual polls were nearly that sure.
# In fact, political commentator Joe Scarborough said during his show,
# "Anybody that thinks that this race is anything but a tossup right now is such
#    an ideologue-- they're jokes."
# To which Nate Silver responded, "If you think it's a tossup, let's bet.
# If Obama wins, you donate $1,000 to the American Red Cross.
# If Romney wins, I do.
# Deal?"
# How is Mr. Silver so confident?
# We'll illustrate what Nate Silver saw that Joe Scarborough and other pundits
# did not. We're going to use a Monte Carlo simulation. We're going to generate
#    results for 12 polls taken the week before the election. We're going to
#    mimic the sample sizes from actual polls. We're going to construct and report
#    95% confidence intervals for each of these 12 polls. Here's the code we're
#    going to use.

d <- 0.039   # spread
Ns <- c( 1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516 )   # Sample sizes
p <- ( d + 1 ) / 2   # Proportion of people voting for Obama

confidence_intervals <- sapply( Ns, function( N ) {
    X <- sample( c(0, 1 ), size = N, replace = TRUE, prob = c( 1 - p, p ) )
    X_hat <- mean( X )   # proportion of people voting for Obama, in the sample
    SE_hat <- sqrt( X_hat * ( 1- X_hat ) / N )   # Standard Error
    2 * c( X_hat, X_hat - 2 * SE_hat, X_hat + 2 * SE_hat ) - 1
} )
confidence_intervals

# We're going to generate the data using the actual outcome, 3.9%.
# So d, the difference, the spread, is 0.039.
# The sample sizes were selected to mimic regular polls.
# So we see that the first one is 1,298, the second one is 533, etcetera.
# We're also going to define p, the proportion of Democrats-- or actually, the
#    proportion of people voting for Obama, as the spread plus 1 divided by 2.
# That's the formula we've seen before.  Then we're going to use the sapply function
#    to construct the confidence intervals.
# For each sample size for each poll, we're going to generate a sample.
# We're going to take a sample of size N. Then we're going to compute the proportion
#    of people voting for Obama in that sample-- that's X_hat-- construct a standard
#    error, and then return the estimate X_hat as well as the beginning and end
#    of the confidence interval.
# We're going to do this and then we're going to generate a data frame that has
#    all the results.
polls <- data.frame( poll = 1:ncol( confidence_intervals ),
                     t( confidence_intervals ),
                     sample_size = Ns )
names( polls ) <- c( 'poll', 'estimate', 'low', 'high', 'sample_size' )
polls

# Here are the results of the 12 polls that we generated with the Monte Carlo
#    simulation.
# Here's a visual of what the intervals of these pollsters
# would have reported for the difference between Obama and Romney.
# Not surprisingly, all 12 polls report confidence intervals
# that include the election night result, which is shown with the dashed line.
# This is the case because these are 95% confidence intervals.
# However, all 12 poll intervals include 0, which is shown with a solid black line.
# Therefore, individually if we asked for a prediction from the pollsters, from
#    each individual pollster, they would have to agree with Scarborough.
# It's a tossup.
# Now we're going to describe how pundits are missing a key insight.
# Poll aggregators, such as Nate Silver, realize that by combining the results
#    of different polls, you could greatly improve precision.
# By doing this, effectively we're conducting a poll with a huge sample size.
# As a result, we can report a smaller 95% confidence interval, and therefore a
#    more precise prediction.
# Although as aggregators we do not have access to the raw poll data, we can use
#    mathematics to reconstruct what we would have obtained had we made one large
#    poll with, in this case, 11,269 people, participants.

sum( polls$sample_size )

# Basically we construct an estimate of the spread-- let's call it d-- with a
#    weighted average in the following way.  We basically multiply each individual
#    spread by the sample size.  That's going to give us a total spread.  And then
#    we're going to divide by the total number of participants in our aggregated
#    poll.

d_hat <- polls %>% summarize( avg = sum( estimate * sample_size ) / sum( sample_size ) ) %>% .$avg
d_hat

# This gives us d_hat, which is an estimate of d.  Once we have an estimate of d,
#    we can construct an estimate for the proportion voting for Obama, which we
#    can then use to estimate the standard error.
# Once we do this, we see that our margin of error of the aggregated poll
#    is 0.018.

p_hat <- ( 1 + d_hat ) / 2   # proportion voting for Obama
moe <- 2 * 1.96 * sqrt( p_hat * ( 1 - p_hat ) / sum( polls$sample_size ) )   # margin of error
p_hat
moe

# Thus, using the weighted average, we can predict that the spread will be 3.1%
#    plus or minus 1.8%, which not only includes the actual result but is quite
#    far from including 0.

round( d_hat * 100, 1 )
round( moe * 100, 1 )

# Once we combine the 12 polls, we become quite certain that Obama will win the
#    popular vote.
# In this figure, you can see, in red, the interval that was created using the
#    combined polls.
# Nate Silver and other aggregators use the same approach to predict the electoral
#    college.  And they did very well in 2008 and 2012. However, note that this
#    was just a simulation to illustrate the idea.  The actual data science
#    exercise of forecasting elections is much more complicated and it involves
#    statistical modeling.




# Pollsters and Multilevel Models
# Now we're going to get ready to explain how pollsters fit multilevel models to
#    public poll data and use this to forecast election results.
# In the 2008 and 2012 US presidential elections, Nate Silver used this approach
#    to make an almost perfect prediction and silenced the pundits.
# Since the 2008 election, other organizations have started their own election
#    forecasting groups that, like Nate Silver, aggregate polling data and use
#    statistical models to make predictions.
# In 2016, forecasters greatly underestimated Trump's chances of winning the election.
# For example, the Princeton Election Consortium gave Trump less than 1% chance
#    of winning the election, while the Huffington Post gave him a 2% chance.
# In contrast, FiveThirtyEight had Trump's chances of winning at 29%.
# Although they didn't correctly predict him to have a higher probability, note
#    that 29% is a higher probability than the probability of tossing two coins
#    and getting two heads.
# It's also much, much bigger than what the other pollsters had predicted.
# By understanding statistical models and how these forecasters use them, we will
#    start to understand how this happened.
# Although not nearly as interesting as predicting the electoral college, the actual
#    outcome of the election, for illustrative purposes we will start by looking
#    at the predictions for the popular vote.
# FiveThirtyEight predicted a 3.6% advantage for Clinton.
# Their interval, their prediction interval, included the actual result of 2.1%,
#    48.2% for Clinton compared to 46.1% for Trump.
# They were much more confident about Clinton winning this, the popular vote,
#    giving her a 81.4% chance of winning.
# Next, we're going to look at actual public polling data from the 2016 US
#    presidential election to show how models are motivated and built to produce
#    these predictions.





# Poll Data and Pollster Bias
#  In this video we use public polling data organized by FiveThirtyEight for the
#     2016 presidential election.  The data is included as part of the dslabs
#     package.  You can get the data by typing this code.

data( "polls_us_election_2016" )
names( polls_us_election_2016 )

# Here, we show you the column names of the table.  The table includes results
#    for national polls, as well as state polls, taken in the year before the
#    election.
# For this first illustrative example, we will filter the data to include national
#    polls that happened during the week before the election.
# We also remove polls that FiveThirtyEight has determined not to be reliable,
#    and they have graded them with a B or less.
# Some polls have not been graded.  And we're going to leave these in.
# Here's the code we used to filter as we just described.

polls <- polls_us_election_2016 %>%
    filter( state == 'U.S.' & enddate >= '2016-10-31' & ( grade %in% c( 'A+', 'A', 'A-', 'B+' ) | is.na( grade ) ) )
polls

# We also add a spread estimate.

polls <- polls %>%
    mutate( spread = rawpoll_clinton / 100 - rawpoll_trump / 100 )
polls

# Remember, the spread is what we're really interested in estimating.
# So, we type this code to get the spread in proportions. For illustrative purpose,
#    we will assume that there are only two parties, and call:
#    p           the proportion voting for Clinton, and
#    1 - p       the proportion voting for Trump.
#    d = 2p -1   spread
# We're interested in the spread, which we've shown is 2p minus 1.
# Let's call this spread d. d is for difference.
# Note that we have several estimates of this spread coming from the different polls.
# The theory we learned tells us that these estimates are a random variable with
#    probability distribution that is approximately normal.
# The expected value is the election night spread, d.  And the standard error is
#    2 times the square root of p times 1 minus p divided by the sample size N.
#
#    d   expected value
#    2 * sqrt( p * ( 1 - p ) / N )   Standard Error
#
#    Assuming the urn model we described earlier are useful models, we can use
#    this information to construct a confidence interval based on the aggregated
#    data.  The estimated spread is now computed like this because now the sample
#    size is the sum of all the sample sizes.  And if we use this, we get a standard
#    error, typing this code, that then leads us to a margin of error of .0066,
# a very small margin of error.

d_hat <- polls %>%
    summarize( d_hat = sum( spread * samplesize ) / sum( samplesize ) ) %>%
    .$d_hat
d_hat   # estimated spread

# So, if we were going to use this data, we would report a spread of 1.43% with
#    a margin of error of 0.66%.  On election night, we find out that the actual
#    percentage is 2.1%, which is outside of the 95% confidence interval.

p_hat <- (d_hat + 1 ) / 2
moe <- 1.95 * 2 * sqrt( p_hat * ( 1 - p_hat ) / sum( polls$samplesize ) )

p_hat # standard error
moe   # margin of error

# So, what happened?
#     Was this just bad luck?
#     A histogram of the reported spreads shows another problem.
# With this code, we can quickly make a histogram of the spreads that we're looking at.
library( 'dplyr' )
polls %>%
    ggplot( aes( spread ) ) +
    geom_histogram( color = 'black', binwidth = .01 )

# The data does not appear to be normally distributed, and the standard error appears
#    to be larger than 0.0066.
# The theory is not quite working here.  To see why, notice that various pollsters
#    are involved and some are taking several polls a week.
# Here's a table showing you how many polls each pollster took that last week.

polls %>% group_by( pollster ) %>% summarize( n() )
# if an error appears, 'plyr' library is not required only 'dplyr' library

# Let's visualize the data for the pollsters that are regularly polling.
# We write this piece of code that first filters for only pollsters that polled
#    more than 6 times.  And then we simply plot the spreads estimated by each
#    pollster.  Each one has between five and six.  This plot reveals an unexpected
#    result.

polls %>% group_by( pollster ) %>%
    filter( n() >= 6 ) %>%
    ggplot( aes( pollster, spread ) ) +
    geom_point() +
    theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# First note that the standard error, predicted by theory for each poll-- now,
#    we're going to do this poll by poll-- gives us values between 0.018 and 0.033.
# This appears to be right.

polls %>% group_by( pollster ) %>%
    filter( n() >= 6 ) %>%
    summarize( se = 2 * sqrt( p_hat * ( 1 - p_hat ) / median( samplesize ) ) )

# This appears to be consistent with what we see in the plot. However, there
#    appears to be differences across the polls.  This is not explained by the
#    theory.
# Note for example, how the USC Dornsife/LA Times pollster is predicting a 4% win
#    for Trump while Ipsos is predicting a win larger than 5% for Clinton.
# The theory of learned says nothing about different pollsters producing polls
#    with different expected values.
# All the polls should have the same expected value, the actual spread, the spread
#    we will see on election night.
# FiveThirtyEight refers to these differences as "house effects."
# We can also call them pollster bias.
# Rather than use urn model theory, we're instead going to develop a data-driven
#    model to produce a better estimate and a better confidence interval.





# Data-Driven Models
#  For each pollster, let's collect their last-reported result before the election
#     using this simple piece of code.

one_poll_per_pollster <- polls %>%
    group_by( pollster ) %>%
    filter( enddate == max( enddate ) ) %>%
    ungroup()

# Here's a histogram of the data for these 15 pollsters.

one_poll_per_pollster %>%
    ggplot( aes( spread ) ) +
    geom_histogram( binwidth = 0.01 )

# In the previous video, we saw that using the urn model theory to combine these
#    results might not be appropriate due to the pollster effect.  Instead, we will
#    model this spread data directly.  The new model can also be thought of as an
#    urn model, although the connection to the urn idea is not as direct.
# Rather than having beads with zeros and ones inside the urn, now the urn contains
#    poll results from all possible pollsters.  We assume that the expected value
#    of our urn is the actual spread, which we have been calling d, which is equal
#    to 2p minus 1.

# d <- 2p - 1

# Now, because rather than zeros and ones our urn contains continuous numbers between
#    minus 1 and 1, the standard deviation of the urn is no longer the square root
#    of p times 1 minus p.
# Rather than just the sampling variability we get from taking different samples
#    of zeros and ones, the standard error for our average now includes the
#    pollster-to-pollster variability.
# Our new urn also includes the sample variability from the polling.  Regardless,
#    this standard deviation is now an unknown parameter.  In statistics textbooks,
#    the Greek symbol sigma is used to represent this parameter.
# Now in summary, we have two unknown parameters now, the expected value d, what
#    we want to estimate, and the standard deviation, sigma.
# Our task is to estimate d and provide inference for it.  Because we model the
#    observed values, let's call them X1 through XN, as a random sample from the
#    urn, the central limit theorem still works for the average of these values
#    because it's an average of independent random variables.
# For a large enough sample size N, the probability distribution of the sample
#    average, which we'll call X-bar, is approximately normal with expected value
#    d and standard deviation sigma divided by the square root of N. If we are
#    willing to consider N equals to 15 large enough, we can use this to construct
#    a confidence interval.
# A problem, though, is that we don't know sigma.  But the theory tells us that
#    we can estimate the urn model sigma, the unobserved sigma, with the sample
#    standard deviation, which is defined like this with this mathematical formula.
# Now note in the mathematical formula that unlike the population standard deviation,
#    we now divide by N minus 1.  This makes s a better estimate of sigma than if
#    we just divided by N.  And there's a mathematical explanation for this,
#    which is explained in most statistics textbooks, but we don't cover it here.
# Now the sd function in R computes the sample standard deviation. So we can compute
#    it for our data here with this simple line.  And we get that it's 0.024.

sd( one_poll_per_pollster$spread )

# We are now ready to form a confidence interval based on our new data-driven model.
# We simply use the central limit theorem and create a confidence interval using
#    this simple code.

results <- one_poll_per_pollster %>%
    summarize( avg = mean( spread ), se = sd( spread ) / sqrt( length( spread ) ) ) %>%
    mutate( start = avg - 1.96 * se, end = avg + 1.96 * se )
round( results * 100, 1 )

# We get an average, a standard error, and then a start of 1.7% and an end of 4.1%.
# That's our 95% confidence interval using now our data-driven model.
# Note that our new confidence interval is wider, and it now incorporates the
#    pollster variability.  It does include the election-night result of 2.1%,
#    and also it's small enough not to include 0.
# Which means that in this particular case, we would have been quite confident
#    that Clinton would win the popular vote.
# Now, are we now ready to declare a probability of Clinton winning as the
#    pollsters do?
#     Not yet.
# In our model, d is a fixed parameter, so we can't talk about probabilities.
# To provide probabilities, we'll need to learn something new.
# We're going to have to learn about Bayesian statistics.
# And we do that next.

