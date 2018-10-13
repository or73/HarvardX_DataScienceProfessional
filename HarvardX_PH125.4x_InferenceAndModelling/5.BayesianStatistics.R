# Bayesian Statistics

# What does it mean when an election forecaster tells us that a given candidate
#   has a 90% chance of winning?
# In the context of the urn model this would be equivalent to stating that the
#    probability that the proportion p of people voting for this candidate being
#    bigger than 0.5, than 50%, is 90%.  But as we discussed, in the urn model,
#    p is a fixed parameter, and it does not make sense to talk about the
#    probability of p being this or that.  With Bayesian statistics, we assume it
#    is in fact random.  And then it makes sense to talk about probability.
#    Forecasters also use models to describe variability at different levels.
#    For example: sampling variability, pollster to pollster variability, day to
#    day variability, and election to election variability.  One of the most
#    successful approaches used to describe these different levels of variability
#    are called hierarchical models.  And hierarchical models are best explained
#    in the context of Bayesian statistics.  Bayesian statistics is the topic of
#    the following videos.




# Baye's Theorem
# We start by reviewing Bayes' theorem.
# We do this using a hypothetical cystic-fibrosis test as an example.  So let's
# start.
# Suppose a test for cystic fibrosis has an accuracy of 99%.
# We will use the following notation to represent this.
# We're going to write that the probability of a positive test given that you have
#    the disease, D equals 1, is 0.99.  Also, the probability of a negative test
#    given that you don't have the disease, D equals 0, is 0.99. Here in this
#    formula plus means a positive test, and D represents if you actually have
#    the disease, 1 or 0.

# Prob( + | D = 1 ) = 0.99, Prob( - | D = 0 ) = 0.99

# Suppose we select a random person and they test positive.  What is the probability
#    that they have the disease?  We write this as the probability of D equals 1
#    given that the test was positive.

# Prob( D = 1 | + )

#    The cystic fibrosis rate is 1 in 3,900, which implies that the probability
#    that D equals 1 is 0.00025.

#    Prob( D - 1 ) = 0.00025

# To answer this question, we'll use Bayes' theorem, which in general tells us
#    that the probability of event A happening given that event B happening is
#    equal to the probability of them both happening divided by the probability
#    of B happening.

# Pr( A | B )  =  Pr( A and B ) / Pr( B )  =  Pr( B | A ) Pr( A ) / Pr( B )

# The numerator is split using the multiplication rule into the probability of B
#    happening given A happening times the probability of A happening.
# This is going to be useful because sometimes we know the probability of A given
#    B and not the probability of B given A, as is the case in the cystic fibrosis
#    example.
# Here is the Bayes' theorem equation applied to our cystic-fibrosis example.

# Pr( D = 1 | + ) = Pr( + | D = 1 ) Pr( D = 1 ) / Pr( + )
#  = Pr( + | D = 1 ) Pr( D = 1 ) / [ Pr( + | D = 1 ) Pr( D = 1) + Pr( + | D = 0 ) Pr( D = 0 ) ]

# The probability of D equals 1 given a positive test is what we want to know.
# We don't know that.  We want to find out.  What we do know is the probability
#    of a positive test given that D equals 1.
# We also know the probability of a positive test given D equals 0.
# So now using Bayes' formula, we write out the equation.
# And we end up with a larger fraction that includes quantities that we know.
# If you look at each one of those quantities up there, we know what they are.
# And now we're going to plug in the values.

# Pr( D = 1 | + ) = 0.99 Pr( D = 1 ) / [ 0.99 Pr( D = 1 ) + Pr( + | D = 0 ) Pr( D = 0 ) ]
#  = ( 0.99 * 0.00025) / [ 0.99 * 0.00025 + Pr( + | D = 0 ) Pr( D = 0 ) ]
#  = ( 0.99 * 0.00025 ) / [ ( 0.99 * 0.00025 ) + ( 0.01 * 0.99975 ) ]
#  = 0.02

# We do that here.
# And now we have that that probability is 0.02, only a 2% chance.
# This says that despite the test having 99% accuracy, the probability of having
#    the disease given a positive test is only 2%.
# This may appear counterintuitive to some.  But we're going to see how it makes
#    sense.  The reason this is the case is because we have to factor in the very
#    rare possibility that a person chosen at random has the disease.
# This is the Bayesian way of thinking. To illustrate this, we can use a Monte Carlo
#    simulation.  The following simulation is meant to help you visualize Bayes'
#    theorem.  We start by randomly selecting 100,000 people from a population in
#    which the disease in question has a 1 in 3,900 prevalence.
# So we set the prevalence to be 0.00025.  We set N to be 100,000.
# And now we sample 100,000 people using the code that we've learned to use.
# Here it is.

prev <- 0.00025
N <- 100000
outcome <- sample( c ( 'Disease', 'Healthy' ), N, replace = TRUE, prob = c( prev, 1- prev ) )
head( outcome )

# Note that because prevalence is so low, once we take the sample the number of
#    people with the disease is low.  It's only 23.

N_D <- sum( outcome == 'Disease' )
N_D

# Here's the code you use to get that.  And of course there's a lot of healthy
#    people, 99,977.

N_H <- sum( outcome == 'Healthy' )
N_H

# This makes the probability that we see some false positives quite high.
# There are so many people without the disease that are getting the test that,
#    although it's rare, we were going to get a few people getting a positive test
#    despite them being healthy.    Here's the code that shows this.

accuracy <- 0.99
test <- vector( 'character', N )
test[ outcome == 'Disease' ] <- sample( c( '+', '-' ), N_D,
                                        replace = TRUE,
                                        prob = c( accuracy, 1 - accuracy ) )
test[ outcome == 'Healthy' ] <- sample( c( '-', '+' ), N_H,
                                        replace = TRUE,
                                        prob = c( accuracy, 1 - accuracy ) )
head( test )


# Each person has a 99% chance of getting the test giving them the right answer.
# So we write the code like this. For each of the diseased and healthy people,
#    we're going to sample either a correct or incorrect test with the appropriate
#    probabilities, a very high probability of the correct test.
# If you examine this code, you will see that that's what we're doing in the sample
#    call.  So we have two variables here, the outcome, which is disease or healthy,
#    and test, which is positive or negative.
# We can make a table that shows us the number of people in each one of these four
#    combinations.  We do that using the table command.  Here it is.

table( outcome, test )

# We can see that there are a lot of people they are healthy that got a positive
#    outcome.  That's because there are so many more healthy people.
# From this table, we can also see that the proportion of positive tests that
#    have the disease is 23.  And this is out of a total of 23 plus 965, which is
#    988.  If you divide 23 by 988, you get about 2%, which is exactly what Bayes'
#    theorem told us it should be.
# We can run this simulation over and over again, and we'll see that that probability
#    will converge to the probability that Bayes' theorem told us it would be, which
#    is again, about 2%.




# Bayes in Practice
# To demonstrate the usefulness of hierarchical models, Bayesian models, in practice,
#    we're going to show you a baseball example.  In sports, we use Bayesian thinking
#    all the time, even if we don't realize it.  Let's go to the example.
# Jose Iglesias is a professional baseball player.
# In April 2013, when he was starting his career, he was performing rather well.
# He had been to bat 20 times and he had nine hits, which is an average of 0.450.

# | Date | At Bats | H  | Avg |
# |--    | --      | -- | --  |
# | April| 20      | 9  | .450|

# This average of 0.450 means Jose had been successful 45% of the times he had
#    batted, which is rather high historically speaking.
# Note, for example, that no one has finished a season with an average of 0.400
#    or more since Ted Williams did it in 1941.
# To illustrate the way hierarchical models are powerful, we will try to predict
#    Jose's batting average at the end of the season.
# In a typical season, players have about 500 or bats.  With the techniques we
#    have learned up to now, referred to as frequentist statistics, the best we
#    can do is provide a confidence interval.
# We can think of outcomes for hitting as a binomial with a success rate of p.
# So if the success rate is indeed 0.450, the standard error of just 20 at bats
#    can be computed like this.   And it's 0.111.

sqrt( .450 * ( 1 - .450 ) / 20 )

# We can use this to construct a 95% confidence interval, which will be
#    from 0.228 to 0.672.
# This prediction has two problems.
# First, it's very large, so it's not very useful.
# Second, it's centered at 0.450, which implies that our best guess is that this
#    relatively unknown player will break Ted Williams' longstanding record.
# If you follow baseball, this last statement will seem wrong.  And this is because
#    you're implicitly using the hierarchical model that factors in information
#    from years of following baseball.
# Here we show how we can quantify this intuition.
#    First, let's explore the distribution of batting averages for all players
#    with more than 500 at bats during the seasons 2010, 2011, and 2012.
# Here are the histograms.
# We note that the average player had an average of 0.275.  And the standard
#    deviation of the population of all these players was 0.027.  So we can see
#    already that 0.450 would be quite an anomaly, since it is over six standard
#    deviations away from the average.
# So is Jose lucky or the best batter seen in the last 50 years?
# Perhaps it's a combination of both.
# But how do we decide how much of this is luck and how much of this is talent?
#    If we become convinced that this is just luck, we should trade him to a team
#    that trusts the 0.450 observation and is maybe overestimating his potential.




# The Hierarchichal Model
# The hierarchical model provides a mathematical description of how we come to
#    see the observation of 0.450.
# First, we pick a player at random with an intrinsic ability summarized by,
#    for example, p-- the proportion of times they will actually be successful.
# Then we see 20 random outcomes with success probability p.
# We use a model to represent two levels of variability in our data.
# First, each player is assigned a natural ability to hit at birth.
# You can think of it that way.
# We will use a symbol, p, to represent this ability.
# You can think of p as a batting average you would have converged to if this
#    particular player batted over and over and over and over again.
# Based on the plots, we assume that p has a normal distribution.
# If we just pick a player at random, the random variable p will have a normal
#    distribution.
# We also know that the expected value is about 0.270 and a standard error
# of 0.027.
# Now, the second level of variability has to do with luck.
# Regardless of how good or bad a player is, sometimes you have bad luck, and
#    sometimes you have good luck when you're batting.
# At each at bat, this player has a probability of success, p.
# If we add up these successes and failures as 0's and 1's, then the CLT tells
#    us that the observed average, let's call it Y, has a normal distribution
#    with expected value p and standard error square root of p times 1 minus p,
#    divided by N. N is the number of at bats.

# SE = sqrt( p ( 1 - p ) / N )

# Statistical textbooks will write the model like this.
# We are going to use a tilde to denote the distribution of something.

# p ~ N ( mu, tau )

# So p tilde N(mu, tau) is telling us that p, which is now a random variable,
#    has a distribution that is normal with respected value mu and standard error
#    tau.
# This describes the randomness in picking a player.
# Now we describe the distribution at the next level.

# Y | p ~ N( p, sigma )

# So the distribution of the observed batting average Y, given that this player
#    has a talent, p, is also normally distributed with expected value p and a
#    standard error sigma.
# This describes the randomness in the performance of this particular player.
# In our case, mu is 0.270.
# Tau is 0.027.
# And sigma squared is p times 1 minus p divided by n.

# miu = 0.270
# tau = 0.027
# SE = sigma^2 = p ( 1 - p ) / N = 0.111

# Because there are two levels, we call these hierarchical models.
#    The first one is the player to player variability.
#    The second is the variability due to luck when batting.
# In a Bayesian framework, the first level is called prior distribution, and the
#    second the sampling distribution.
# Now, let's use this model for Jose's data.   Suppose we want to predict his
#    innate ability in the form of his true batting average, p.

# p ~ N( 0.275, 0.027 )
# Y | p ~ N( p, 0.111 )

# This would be the hierarchical model for our data.  p is normal with expected
#    value 0.275, standard error 0.027.
# And Y, given p, is normal with expected value p-- we don't know what p is;
#    we're trying to estimate it-- and standard error 0.111.
# We now are ready to compute what is called a posterior distribution to summarize
#    our prediction of p.
# What Bayesian statistics lets us do is compute the probability distribution
#    of p given that we have observed data.
# This is called a posterior distribution.
# Again, the probability distribution of p conditioned that we have observed data
# Y. There is a continuous version of Bayes' rule that lets us compute the posterior
#    distribution in cases like this, where the distributions are continuous.
# The normal distribution is a continuous distribution.
# We can use this continuous version of Bayes' rule to derive a posterior probability
#    function for p assuming that we have observed Y equals, for example, little y.
# In our case, we can show that this posterior distribution follows a normal
#    distribution with expected value given by this formula.

# E( p | y ) = B miu + ( 1 - B ) Y
#            = miu + ( 1 - B ) ( Y - miu )
# B = sigma^2 / ( sigma^2 + tau^2 )

# miu -> is the average for all baseball players
# Y ->  what we have observed for Jose.

# Now, let's study this formula closely, because it is very informative, and it
#    actually explains our intuition. Note that this is a weighted average between
#    miu-- miu is the average for all baseball players-- and Y, what we have observed
#    for Jose.
# So if B were to be 1, this would mean that we're just saying Jose is just an
#    average player, so we're going to predict mu.
# If B is 0, we would be saying forget the past, we're going to predict that Jose
#    is what he is, what we've observed. His average is 0.450.
# Now, look at how B is constructed.  B is the standard error sigma squared divided
#    by the sum of the standard error sigma squared, plus the standard error tau
#    squared.
# So B, the weight, is going to be closer to 1 when sigma is large.
#    When is sigma large?  Sigma is large when the variance, when the standard
#    error, of our observed data is large.
# When we don't trust our observed data too much, sigma is large. So we make B = 1.
# In this case, we would predict that Jose Iglesias is an average player.
# We would predict mu.  On the other hand, if the sigma is very, very small,
# this means that we really do trust our data Y, and we're actually going to say,
#    no, we trust our data, and we are going to actually ignore the past and
#    predict Y.
# Of course, B is somewhere in the middle, so we get something in the middle.
# This weighted average is sometimes referred to as shrinking, because it shrinks
#    the observed Y towards a prior mean, which in this case is mu.
# We shrink the observed data towards what the average player is, mu.
# In the case of Jose Iglesias, we can fill in those numbers and get that the
#    expected value for the posterior distribution is 0.285.

# E( p | Y = .450 ) = B * .275 + ( 1 - B ) * .450
#                   = .275 + ( 1 - B ) * ( .450 - .275 )
# B = .111^2 / ( .111^2  + .027^2 )  = 0.944
# E( p | Y = .450 ) = .285

# It's a number between the 0.450 that we saw and the 0.270 that we've seen
#    historically for the average player.
# The standard error can also be computed.  We use mathematics to do this.
# We're not showing it here, but we can do it.  And we get a formula that we show
#    here.   This is the formula for the standard error of the posterior
#    distribution.  And in this case, we get that the standard deviation is 0.026.

# SE( p | y )^2 = 1 / ( 1/sigma^2 + 1/tau^2 )
#               = 1 / ( 1/.111^2 + 1/.27^2)
#               = 0.00069

# So we started with a frequentist 95% confidence interval that ignored data
#    from other players from the past and simply summarized Jose's data as 0.450
#    plus or minus 0.220.
# Then we used a Bayesian approach that incorporated data from the past, from
#    other players, and obtained a posterior probability.
# We should point out that this is actually referred to as an empirical Bayesian
#    approach.
# In a traditional Bayesian approach, we simply state the prior.
# In an empirical Bayesian approach, we use data to construct the prior, and that's
#    what we did here.
# Using the posterior distribution, we can report what is called a 95% credible
#    interval.  This is a region centered at the expected value with a 95% chance
#    of occurring.
# Remember that p is now random, so we can talk about the chances of p happening,
#    falling here or falling there.
# In our case, we can construct this by adding twice the standard error to the
#    expected value of the posterior distribution.
# And we get 0.285 plus or minus 0.052.
# Note that the Bayesian approach is giving us a prediction that is much lower
#    than the 0.450.   It's also giving us a much more precise interval.
# The Bayesian credible interval suggests that if another team that is ignoring
#    past data is impressed by the 450, the 0.450 observation, we should consider
#    trading Jose as they probably overvalue-- if we trust our new prediction that
#    is predicting that he will be just slightly above average.
# Interestingly, the Red Sox traded Jose Iglesias to the Detroit Tigers in July 2013.
# Let's look at his batting average for the next five months.
# Notice that if we take April out, his batting average for the rest of the season
#    was 0.293.

#   | Month           | At Bat | Hits | AVG  |
#   |--               |--      |--    |--    |
#   | April           | 20     | 9    | .450 |
#   | May             | 26     | 11   | .423 |
#   | June            | 86     | 34   | .395 |
#   | July            | 83     | 17   | .205 |
#   | August          | 85     | 25   | .294 |
#   | September       | 50     | 10   | .200 |
#   |--               |--      |--    |--    |
#   | Total w/o April | 330    | 97   | .293 |
#

# Although both intervals, the frequentist confidence interval and the Bayesian
#    credible intervals, included the final batting average of 0.293, the Bayesian
#    credible interval provided a much more precise prediction.
# In particular, it predicted that he would not be as good for the remainder of
#    the season.
# So trading him was perhaps the right decision.

