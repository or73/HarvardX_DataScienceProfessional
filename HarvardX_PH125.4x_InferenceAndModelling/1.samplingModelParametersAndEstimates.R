# Sampling Model Parameters and Estimates

# We will use an urn, instead of voters, and because polisters are competing with
#    other polsters for media attention, we will imitate taht by having our
#    competition with a $25 prize.
# The challenge is to guess the spread between thye proportion of blue an red
#    balls in this urn.  Before making a prediction, you can take a sample, with
#    replacement, from the urn.
# To mimic the fact that running polls is expensive, it will cost you $0.10 per
#    bead you sample.  So, if your sample size is 250 and you win, you'll break
#    even, as you'll have to pay me $25 to collect your $25.
# Your entry into the competition can be an interval.  If the interval you submit
#    contains the true proportion, you get half what you pard and pass to the
#    second phase of the competition, the entry with the smallest interval is
#    selected as the winner.

library( tidyverse )
library( dslabs )
ds_theme_set()
take_poll( 25 )


# How you would construct your interval?  How many beads would you sample? etc...
# Notice that we have just described a simple sampling model for opinion polls.
# The beads inside the urn represent the individuals that will vote on election
#    day.  Those that will vote Republican are represented with red beads and the
#    Democrats with blue beads.  For simplicity, assume there are no other colors,
#    that there are just two parties.
# We want to predict the proportion of blue beads in the urn.  Let's call this
#    quantity 'p', which in turn tells us the proportion of red beads, 1-p, and
#    the spread, p - ( 1-p ) = 2p -1
# In statistical textbools, the beads in the urn are called the population.
# The proportion of blue beads in the population 'p' is called a parameter.
# The 25 beads that we saw in an earlier plot after we sampled,
#    that'a called sample.
# The task of statistical inference is to predict the parameter 'p', using the
#    observed data in the sample.  Now, can we do this with just the 25
#    observations we showed you?
# Well, they are certainly informative, for example, giben that we see 13 red
#    and 12 blue, it is unlikely that 'p' is bigger than 0.9 or smaller than 0.1
# Because if they were, it would be unprobable to see 13 red and 12 blue.  But,
#    are we ready to predict with certainty that there are more red beads than
#    blue?  Ok, what we want to do is construct an estimate of 'p' using only
#    the information we observe.
# estimate: summary of the observed data that we think is informative about the
#           parameter of interest.
# It seems intuitive to think that the proportion of blue beads in the sample,
#    which in this case is 0.48 must be at least related to the actual proportion
#    'p'.  But, do we simply predict to be 0.48?
# First, note that the sample proportion is a random variable.  If we run the
#    command take_poll( 25 ), say four times, we get four different answers.
take_poll( 25 )
take_poll( 25 )
take_poll( 25 )
take_poll( 25 )

# Each time the sample is different and the sample proportion is different.
#    The sample proportion is a random variable.  note that in the four random
#    samples we show, the sample proportion ranges from 0.44 to 0.6.  By describing
#    the distribution of this random variable, we'll be able to gain insights into
#    how good this estimate is and how we can make it better.



# The Sample Average
# Taking an opinion poll is being modeled as taking a random sample from an urn.
# We are proposing the use of the proportion of blue beads in our sample as an
#    estimate of the parameter p.
# Once we have this estimate, we can easily report an estimate of the spread, 2p-1.
# But for simplicity, we will illustrate the concept of statistical inference
# for estimating p.
# We will use our knowledge of probability to defend our use of the sample
# proportion, and quantify how close we think it is from the population
#    proportion p.  We start by defining the random variable X. X is going to be
#    1 if we pick a blue bead at random, and 0 if it's red.
# This implies that we're assuming that the population, the beads in the urn,
# are a list of 0s and 1s.
# If we sample N beads, then the average of the draws X_1 through X_N is equivalent
#    to the proportion of blue beads in our sample.
# This is because adding the Xs is equivalent to counting the blue beads, and
#    dividing by the total N turns this into a proportion.  We use the symbol
#    X-bar to represent this average.
# In general, in statistics textbooks, a bar on top of a symbol means the average.
# The theory we just learned about the sum of draws becomes useful, because we
#    know the distribution of the sum N times X-bar.  We know the distribution
#    of the average X-bar, because N is a non random constant.
# For simplicity, let's assume that the draws are independent.
# After we see each sample bead, we return it to the urn.
# It's a sample with replacement.
# In this case, what do we know about the distribution of the sum of draws?
#     First, we know that the expected value of the sum of draws is N times the
#     average of the values in the urn.
# We know that the average of the 0s and 1s in the urn must be the proportion p,
#    the value we want to estimate.
# Here, we encounter an important difference with what we did in the probability
#    module.  We don't know what is in the urn.  We know there are blue and red
#    beads, but we don't know how many of each.  This is what we're trying to
#    find out.  We're trying to estimate p.
# Just like we use variables to define unknowns in systems of equations, in
#   statistical inference, we define parameters to define unknown parts of our
#   models.  In the urn model we are using to mimic an opinion poll, we do not
#   know the proportion of blue beads in the urn.  We define the parameter p
#   to represent this quantity.  We are going to estimate this parameter.
# Note that the ideas presented here, on how we estimate parameters and provide
#   insights into how good these estimates are, extrapolate to many data science
#   tasks.  For example, we may ask, what is the difference in health improvement
#   between patients receiving treatment and a control group?
#     We may ask, what are the health effects of smoking on a population?
#     What are the differences in racial groups of fatal shootings by police?
#     What is the rate of change in life expectancy in the US during the last 10
# years?
#     All these questions can be framed as a task of estimating a parameter from
#     a sample.



# Polling Vesus Forecasting
# Before we continue, let's make an important clarification related to the
#    practical problem of forecasting the election.   If a poll is conducted
#    4 months before the election, it is estimating the p for that moment,
#    not for election day.
# But, note that the p for election night might be different since people's
#    opinions fluctuate through time.  The polls provided the night before the
#    election tend to be the most accurate since opinions don't change that much
#    in a couple of days.   However, forecasters try to build tools that model
#    how opinions vary across time and try to predict the election day result,
#    taking into consideration the fact that opinions fluctuate.  We'll describe
#    some approaches for doing this in a later section.


# Properties of our stimate
# To understand how good our estimate is, we'll describe the statistical
#    properties of the random variable we just defined, the sample proportion.
# Note that, if we multiply by N, N times X-bar is a sum of independent draws,
#    so the rules we covered in the probability module apply.  Using what we
#    have learned, the expected value of the sum N times X-bar is N times the
#    average of the urn, p.  So, dividing by the nonrandom constant N gives us
#    that the expected value of the average X-bar is p.
# We can write it using our mathematical notation like this. We also can use
#    what we learned to figure out the standard error.  We know that the standard
#    error of the sum is square root of N times the standard deviation of the
#    values in the urn.
# Can we compute the standard error of the urn?
# We learned a formula that tells us that it's:
#    1 minus 0 times the square of p times 1 minus p,
#    ( 1 - 0 ) sqrt( p ( 1-p ) )
# which is the square root of p times 1 minus p.
# Because we are dividing by the sum, N, we arrive at the following formula for
#    the standard error of the average.  The standard error of the average is
#    square root of p times 1 minus p divided by the square root of N.
#    SE( X_ ) = sqrt( p ( 1-p ) / N )
# This result reveals the power of polls. The expected value of the sample
#    proportion, X-bar, is the parameter of interest, p.
#    E( X_ ) = p
# And we can make the standard error as small as we want by increasing the sample
#    size, N.
#    SE( X_ ) = sqrt( p ( 1-p ) / N )
# The law of large numbers tells us that, with a large enough poll, our estimate
#    converges to p.  If we take a large enough poll to make our standard error,
#    say, about 0.01, we'll be quite certain about who will win. But how large
#    does a pool have to be for the standard error to be this small?
# One problem is that we do not know p, so we can't actually compute the
#    standard error.   For illustrative purposes, let's assume that p is 0.51
#    and make a plot of the standard error versus the sample size N. Here it is.
#    You can see that obviously it's dropping. From the plot, we also see that
#    we would need a poll of over 10,000 people to get the standard error as low
#    as we want it to be. We rarely see polls of this size due, in part, to costs.
# We'll give other reasons later. From the RealClearPolitics table we saw earlier,
#    we learned that the sample sizes in opinion polls range from 500 to 3,500.
#    For a sample size = 1,000
#      if we set p = 0.51
#      the standard error is about 0.15, or 1.5 percentage points.
# So even with large polls, for close elections, X-bar can lead us astray if we
#    don't realize it's a random variable.
# But, we can actually say more about how close we can get to the parameter p.








