# Election Forecasting

# Pollsters tend to make probabilistic statements about the result of the election.
# For example, the chance that Obama wins the electoral college is 91%.
# That is a probabilistic statement about the parameter d.
# We show that for the 2016 election, FiveThirtyEight gave Clinton 81.4% chance
#    of winning the popular vote, and that happened.  To do this, they used the
#    Bayesian approach we describe here.
# We assume a hierarchical model similar to the one we used to predict the
#    performance of a baseball player.  In this case, we write it like this.

# d~N( μ, τ )   describes our best guess had we not seen any polling data
# X_ | d~N( d, σ )   describres randomnes due to sampling and the pollster effect

#    d, the spread, is going to be assumed to come from a normal distribution
#    with expected value mu and standard error tau.  And this describes our best
#    guess before we see any polling data.  Then, once we collect data for a given
#    spread and compute an average, we have that this is going to be normally
#    distributed with expected value d and standard error sigma.
# This probability distribution describes the randomness due to sampling and the
#    pollster effect.  For our best guess, we note that before any poll data is
#    available we can use data sources other than polling data.
# A popular approach is to use what are called fundamentals, which are based on
#    properties about the current economy and other factors that historically
#    appear to have an effect in favor or against an incumbent party.
# We don't use those here.  Instead we'll simply set mu to 0 ( μ=0 ) which is
#    interpreted as a model that simply does not provide any information on who
#    will win.
# So before we see a poll we say we have no idea who is going to win.  For the
#    standard deviation, we will use recent historical data that shows the winner
#    of the popular vote has an average spread of about 3.5%.  So we set tau to
#    0.035 ( τ = 0.035 ).
# Now we can use the formulas for the posterior distribution for the parameter d
#    that we previously learned to report the probability of d being bigger
#    than 0 given the observed poll data.
# With this code, we compute the posterior mean, the posterior standard error.

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / ( sigma^2 + tau^2 )

posterior_mean <- B * mu + ( 1 - B ) * Y
posterior_se <- sqrt( 1 / ( 1 / sigma^2 + 1 / tau^2  ) )

posterior_mean
posterior_se

# To make a probability statement, we use the fact that the posterior distribution
#    is also normal.  So one thing we can do is report what is called a credible
#    interval.

posterior_mean + c( -1.96, 1.96 ) * posterior_se

# The posterior mean plus minus 1.96 times the posterior standard error to the
#    posterior mean plus 1.96 times the posterior standard error give us interval
#    that has a 95% chance of occurring.
# The interval is now random.  Here is that interval and this goes from 1.6% to 4%.
# Now more interesting is to report the probability that d is bigger than 0.
# That we can compute using pnorm.  1 minus pnorm of 0, that means the probably
#    being bigger than 0, for a mean of posterior_mean and a standard deviation
#    of posterior_se.

1 - pnorm( 0, posterior_mean, posterior_se )

# This gives us a probability of almost 1, 99.9999%.
# That's the probability that we're going to report for Clinton winning the popular
#    vote.  We're saying it's almost 100%.  This seems to be a little bit too
#    overconfident.  Also it is not in agreement with FiveThirtyEight's 81.4%.
# What explains this difference?
# To understand why this happens we can look at what happens after elections.
# After elections are over, one can look at the difference between the pollsters'
#    predictions and the actual results.  An important observation that our model
#    does not account for is that it is common to see what is called a general bias
#    that affects many pollsters in the same way.  There is no good explanation
#    for this.  But we do observe it in historical data.  One election the average
#    of polls favors Democrats by 2%.
# The next election they favor Republicans with 1%.  Than next there's no bias.
#    Then the next Republicans are favored are 3%, and so on.  In 2016, the polls
#    were biased in favor of the Democrats by about 1% or 2%.
# Although we now know this bias, before the election we had no way of knowing it.
# So we can't correct our polls accordingly.  What we can do is including a term
#    in our model that accounts for this variability.




# Mathematical Representations of Models
# Suppose we are collecting data from one pollster, and we assume there's no
#    general bias.  The pollster collects several polls with a sample size of N.
# So we observe several measurements of the spread.  Let's call it X1 through XJ.
# The theory tells us that these random variables have expected value d and
#    standard error 2 times the square root of p times 1 minus p divided by N.

# 2 * sqrt( p * ( 1 - p ) / N )

# For reasons that will become clear soon, we can represent this model mathematically
#    like this.  We type Xj equals d plus what is called an error term.

# Xj = d + Ej

# We use the Greek letter epsilon, that is the Greek letter for e, which is the
#    first letter of error.
# We use the index j to represent the different polls.  And we define epsilon j
#    to be the random variable that explains the poll to poll variability introduced
#    by sampling error. To do this, we assume it has expected value 0 and standard
#    error 2 times square root of p times 1 minus p divided by n.  If d is 2.1
#    and the sample size for these polls was, say, 2,000, we could simulate six
#    data points from a model using this simple code.

J <- 6
N <- 2000
d <- 0.21
p <- ( d + 1 ) / 2
X <- d + rnorm( J, 0, 2 * sqrt( p * ( 1 - p ) / N ) )
X

# Now, suppose we have six data points from five different pollsters.  To represent
#    this, we now need two indices-- one for pollster and one for the polls each
#    pollster takes.  We're going to use Xi,j-- with i representing the pollster
#    and j representing the jth poll from a given pollster.  If we apply the same
#    model, we would then write Xij equals d plus the sampling error.
# To simulate data, we now have to use a loop.  We're going to use the sapply
#    function like this.  This creates data for five different pollsters.  Here
#    is the simulated data.

# Xi,j = d + Ei,j

I <- 5
J <- 6
N <- 2000
X <- sapply( 1>I, function( i ) {
    d + rnorm( J, 9, 2 * sqrt( p * ( 1 - p ) / N ))
} )
X

# The simulated data does not really seem to capture an important feature of the
#    actual data, which we can see here.  The model does not account for pollster
#    to pollster variability.  To fix this, we add a new term for the pollster
#    effect.  We're going to use hi to represent the house effect for the ith
#    pollster.  So now, the model looks like this.

# Xi,j = d + hi + Ei,j

#    To simulate data for a specific pollster, we now need to draw an hi for each
#    pollster and then add the epsilons. We can do this using this simulation.
#    We're going to assume that the standard error for pollster to pollster
#    variability is 0.025.  So here's the code.

I <- 5
J <- 6
N <- 2000
d <- 0.21
p <- ( d + 1 ) / 2
h <- rnorm( I, 0, 0.025 )
X <- sapply( 1:I, function( i ) {
    d + h[ i ] + rnorm( J, 0, 2 * sqrt( p * ( 1 - p ) / N ) )
} )
X

# The simulated data now looks much more like the actual data, where each pollster
#    has its own center.  Note that hi is common to all the observed spreads
#    from a specific pollster.  Different pollsters have a different hi, which
#    explains why we can see the groups of points shift up and down from pollster
#    to pollster.
# Now, in the model, we assume the average house effect is 0.  We think that for
#    every pollster that's biased in favor of one party, there's another that is
#    favored in favor of the other, so it all averages out.  But historically,
#    we see that every election has a general bias affecting all polls, as we
#    said earlier.
# We can't observe this with just the 2016 data.  But if we were to collect
#    historical data, we will see that the average of polls misses by more than
#    models like the one we showed would predict.  To see this, we would take--
#    for each election year-- the average of polls and compare it to the actual
#    value.
# If we did this, we would see differences with standard deviations of between
#    2% and 3%.
# To incorporate this into the model, we can add yet another term that accounts
#    for this general bias variability.  So now the model looks like this.  We've
#    added a b into it.

# Xi,j = d + b + hi + Ei,j

# The b is modeled to have expected value 0.  And again, based on historical data,
#    we assume that the standard error is about 0.025. Note that the variability
#    of b is not observed in one year, because every single poll we observe that
#    year has this general bias.  So we don't see that variability.  Every single
#    poll has the same value.
# An implication of adding this term to the model, though, is that the standard
#    deviation of Xi,j is actually higher than what we earlier called sigma--
#    which combines the pollster variability and the sample n variability.
#    We have to add the general bias variability.  Since we add this, now we note
#    that the sample average, which is shown here, implies that the standard
#    deviation of X-bar includes this term sigma b.  Because the same b is in
#    every measurement, the average does not reduce its variance.
# This is an important point.  It does not matter how many polls you take.
# This bias does not get reduced by taking averages.  If we redo the Bayesian
#    calculation taking this variability into account,we get a result much closer
#    to FiveThirtyEight.
# We write the code again.

mu <- 0
tau <- 0.035
sigma <- sqrt( results$se^2 + 0.25^2 )
Y <- results$avg
B <- sigma^2 / ( sigma^2 + tau^2 )

sigma
Y
B

posterior_mean <- B * mu + ( 1 - B ) * Y
posterior_se <- sqrt( 1 / ( 1 / sigma^2 + 1 / tau^2 ) )
posterior_mean
posterior_se
1 - pnorm( 0, posterior_mean, posterior_se )

# But now notice that the sigma includes the 0.025 squared.
# That's the general bias variability.  Once we do this, we get a probability of
#    Clinton winning the popular vote of 81.7%-- much lower than the 99.999--
#    again, because we're including the general bias variability.




# Predicting the Electoral College
# Up to now we have focused on the popular vote.
# But in the United States, elections are not decided by the popular vote, but
#    rather by what is called the electoral college.  Each state and DC get a
#    number of electoral votes that depend in a somewhat complex way on the population
#    size of the state.
# Here are the top five states ranked by electoral votes.

library( dplyr )
library( magrittr )
results_us_election_2016 %>% arrange( desc( electoral_votes ) ) %>% top_n( 5, electoral_votes )

# We can see California has 55 votes, Texas has 38 votes, etcetera.
# With some minor exceptions that we don't discuss, the electoral votes are won
#    all or nothing. So for example, if you win California by just one vote,
#    you still get all of its 55 electoral votes. This means that by winning a few
#    big states by a large margin, but losing many small states by a small margin,
#    you can win the popular vote and lose the electoral college.
# This happened in 1876, 1888, 2000, and 2016.  The idea behind this is to avoid
#    a few large states having too much power and dominate the presidential election.
# Although many in the US consider the electoral college unfair and would like to
#    see it changed, this is how the elections are decided.
# We are now ready to predict the electoral college result for 2016.
# We start by aggregating results from polls taken during the last week before
#    the election.  We write this code to filter out the polls we don't want,
#    compute the spread, and then compute the average and the standard deviation
#    for each state.

results <- polls_us_election_2016 %>%
    filter( state != 'U.S.' &
                !grepl( 'CD', state ) &
                enddate >= '2016-10-31' &
                ( grade %in% c( 'A+', 'A', 'A-', 'B+' ) | is.na( grade ) ) ) %>%
    mutate( spread = rawpoll_clinton / 100 - rawpoll_trump / 100 ) %>%
    group_by( state ) %>%
    summarize( avg = mean( spread ), sd = sd( spread ), n = n() ) %>%
    mutate( state = as.character( state ) )
results

# Here are the 10 closest races according to the polls already summarized.

results %>% arrange( abs( avg ) )

# We see Florida, North Carolina, Ohio, et cetera.
# These are called the battleground states.
# We now introduce a command that we will learn more about in later videos called
#    left_join(), and it will let us easily add the number of electoral votes
#    for each state.

results <- left_join( results, results_us_election_2016, by = 'state')
results

# Note that some states have no polls.
# This is because a winner is pretty much known.

results_us_election_2016 %>% filter( !state %in% results$state )

# No polls were conducted in DC, Rhode Island, Alaska, and Wyoming because the
#    first two are sure to be Democrats and the last two Republicans.

results <- results %>%
    mutate( sd = ifelse( is.na( sd ), median( results$sd, ra.rm = TRUE ), sd ) )
results

# This code assigns a standard deviation to the states that had just one poll by
#    substituting the missing value by the median of the standard deviations of
#    all the other states.
# We're going to use a Monte Carlo simulation to generate outcomes from simulated
#    elections. Then we're going to use this to make probability statements.
#    For each state, we apply the Bayesian approach we learned to generate an Election
#    Day d for each state.  We could construct the priors for each state based on
#    recent history. However, to keep it simple, we assign the same prior to each
#    state.  This prior is going to assume that we know nothing about what will
#    happen.  So the expected value will be 0.  Because from election year to election
#    year the results from a specific state don't change that much, we will assign
#    a standard deviation of 2%.
# So tau is going to be now 0.02.
# The Bayesian calculation looks like this.

mu <- 0
tau <- 0.02
results %>% mutate( sigma = sd / sqrt( n ),
                    B = sigma^2 / ( sd^2 + tau^2 ),
                    posterior_mean = B * mu + ( 1 - B ) * avg,
                    posterior_se = sqrt( 1 / ( 1 / sigma^2 + 1 / tau^2 ) ) ) %>%
    arrange( abs( posterior_mean ) )

# This is the code that generates the posterior mean and the posterior standard
#    error. Note that estimates based on posteriors move the estimates towards 0,
#    although the states with many polls are influenced less.
# You can see that in this plot.
# This is expected, as the more poll data we collect, the more we trust those results.
#    Now we repeat this 10,000 times and generate an outcome from the posterior.
#    So we're generating 10,000 election night results.
head( results, 1 )
mu <- 0
tau <- 0.02

clinton_EV <- replicate( 1000, {
    results %>% mutate( sigma = ifelse( is.na( sd ), 0.011, sd / sqrt( n ) ),
                        B = sigma^2 / ( sigma^2 + tau^2 ),

                        posterior_mean = B * mu + ( 1 - B ) * avg,
                        posterior_se = sqrt( 1 / ( 1 / sigma^2 + 1 / tau^2 ) ),

                        simulated_result = rnorm( length( posterior_mean ), posterior_mean, posterior_se ),
                        clinton = ifelse( simulated_result > 0, electoral_votes, 0 ) ) %>%
        summarize( clinton = sum( clinton ) ) %>%
        .$clinton + 7## 7 for Rhode Island and D.C.
} )
clinton_EV
mean( clinton_EV > 269)

#    In this code, we only keep the total number of electoral votes for Clinton.
#    We add 7 because we have no polls for Rhode Island and DC, but we're sure that
#    the Democrats will win those states.
# This model gives Clinton over 99% chance of winning, as we can see by writing
#    this code.  Here's a histogram of the outcomes.

data.frame( clinton_EV ) %>%
    ggplot( aes( clinton_EV ) ) +
    geom_histogram( binwidth = 1 ) +
    geom_vline( xintercept = 269 )

# Note that a similar prediction was made by the Princeton Election Consortium.
# We now know that that was quite off.
# So what happened?
#     Note that the model that we just showed ignores the general bias.
# The general bias in 2016 was not that big compared to other years, but it was
#    between 1% and 2%.  But because the election was close in several big states
#    and the large number of polls made the estimates of standard error small,
#    by ignoring the variability introduced by the general bias, pollsters were
#    overconfident of the polling data.
# FiveThirtyEight, on the other hand, models the general bias in a rather sophisticated
#    way and reported closer results.  We can simulate the results now using this
#    bias term.  For the state level, we're going to assume the general bias is
#    larger.  So we're going to set sigma b to be 0.03.  The code here recomputes
#    the Monte Carlo simulation, but accounting for the general bias.

tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate( 1000, {
    results %>% mutate( sigma = ifelse( is.na( sd ), 0.011, sqrt( sd^2 / n + bias_sd^2 ) ),
                        B = sigma^2 / ( sigma^2 + tau^2 ),

                        posterior_mean = B * mu + ( 1 - B ) * avg,
                        posterior_se = sqrt( 1 / ( 1 / sigma^2 + 1 / tau^2 ) ),

                        simulated_result = rnorm( length( posterior_mean ), posterior_mean, posterior_se ),
                        clinton = ifelse( simulated_result > 0, electoral_votes, 0 ) ) %>%
        summarize( clinton = sum( clinton ) + 7 ) %>%
        .$clinton   ## 7 for Rhode Island and D.C.
} )
clinton_EV_2
mean( clinton_EV_2 > 269 )

# When we do this, the probability of Clinton winning goes way down to 83%.

data.frame( clinton_EV_2 ) %>%
    ggplot( aes( clinton_EV_2 ) ) +
    geom_histogram( binwidth = 1 ) +
    geom_vline( xintercept = 269 )

# Looking at the outcomes of the simulation for these two approaches, we see how
#    the bias term adds variability to the final results. You can see this in this
#    plot.  FiveThirtyEight includes many other features we did not include here.
#    One is that they model variability with distributions that have higher probabilities
#    for extreme events compared to what the normal distribution give us.
# They ended up predicting a probability of 71%.




# Forecasting
# Forecasters like to make predictions well before the election.
# The predictions are adapted as new polls come out.  However, an important
#    question forecasters must ask is, how informative are polls taken several
#    weeks before the election?
# Here we study the variability of poll results across time.
# In our example, to make sure that the variability we observe is not due to
#    pollster effects, we're going to stick to just one pollster.
# Using this code, we're going to look at Ipsos data.

one_pollster <- polls_us_election_2016 %>%
    filter( pollster == 'Ipsos' & state == 'U.S.' ) %>%
    mutate( spread = rawpoll_clinton / 100 - rawpoll_trump / 100 )
head( one_pollster )

# Since there's no pollster effect, perhaps the theoretical standard error will
#    match the data-derived standard deviation.
# We can compute both of them using this code.

se <- one_pollster %>%
    summarize( empirical = sd( spread ),
               theoretical = 2 * sqrt( mean( spread ) * ( 1 - mean( spread ) ) / min( samplesize ) ) )
se

# And we see that the empirical standard deviation is a little bit higher than
#    the theoretical one.  Furthermore, the distribution of the data does not
#    look normal as the theory would predict, as we can see in this figure.

one_pollster %>% ggplot( aes( spread ) ) +
    geom_histogram( binwidth = 0.01, color = 'black' )

# Where is this extra variability coming from?
# This plot makes a strong case that the extra variability comes from time
#    variation not accounted for by the theory that assumes that p is fixed
#    across time.
# Some of the peaks and valleys we see coincide with events such as the party
#    conventions, which tend to give the candidates a boost.  We can see them
#    consistently across several pollsters, not just one.
# With this code we show the trend across time for several pollsters.

polls_us_election_2016 %>%
    filter( state == 'U.S.' & enddate >= '2016-07-01' ) %>%
    group_by( pollster ) %>%
    filter( n() >= 10 ) %>%
    ungroup() %>%
    mutate( spread = rawpoll_clinton / 100 - rawpoll_trump / 100 ) %>%
    ggplot( aes( enddate, spread ) ) +
    geom_smooth( method = 'loess', span = 0.1 ) +
    geom_point( aes( color = pollster ), show.legend = FALSE, alpha = 0.6 )

# We can see the peaks and valleys just the same.
# This implies that if we're going to forecast, our model must include a term to
#    model the time effect.  We could write a model that includes a bias term for
#    time, like this.  We just add the b_t term that will account for the variability
#    of changes through time.

# Yijt = d +b + hj +bt + Eijt

# The standard deviation of bt would depend on time, since the closer we get to
#    the election day, the smaller this variability should become.
# Pollsters also try to estimate trends.  Let's call them f(t) here, the function
#    f of t.  They try to estimate them from the data and incorporate them into
#    the predictions.
# The blue lines in the plot that we just showed are estimates of this f(t).
# The model would then look like this.

# Yijt = d +b + hj +bt + f(t) + Eijt

# In many pollsters' websites, we actually see the estimated f(t), not for the
#    difference but for the actual percentages for the two main candidates.
# The following code will let you make a plot like that, which looks like this.

polls_us_election_2016 %>%
    filter( state == 'U.S.' & enddate >= '2016-07-01') %>%
    select( enddate, pollster, rawpoll_clinton, rawpoll_trump ) %>%
    rename( Clinton = rawpoll_clinton, Trump = rawpoll_trump ) %>%
    gather( candidate, percentage, -enddate, -pollster ) %>%
    mutate( candidate = factor( candidate, levels = c( 'Trump', 'Clinton' ) ) ) %>%
    group_by( pollster ) %>%
    filter( n() >= 10 ) %>%
    ungroup() %>%
    ggplot( aes( enddate, percentage, color = candidate ) ) +
    geom_point( show.legend = FALSE, alpha = 0.4 ) +
    geom_smooth( method = 'loess', span = 0.15 ) +
    scale_y_continuous( limits = c( 30, 50 ) )

# Once we decide on a model, like the one we just showed, we can use historical
#    data and this year's data to estimate all the necessary parameters to make
#    predictions.
# There are a variety of methods for fitting models, which we don't discuss here.
# But later, we'll discuss some of these methods.
