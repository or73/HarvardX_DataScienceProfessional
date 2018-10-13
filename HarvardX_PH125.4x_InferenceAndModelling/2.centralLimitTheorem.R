# Central Limit Theorem

# The central limit theorem tells us that the distribution function for a sum of
#    draws is approximately normal.  We also learned that when dividing a normally
#    distributed random variable by a nonrandom constant, the resulting random
#    variable is also normally distributed.
# This implies that the distribution of X-bar is approximately normal.
# So in summary, we have that X-bar has an approximately normal distribution.
# And in a previous video, we determined that the expected value is p, and the
#    standard error is the square root of p times 1 minus p divided by the sample
#    size N.
# Now, how does this help us?
#     Let's ask an example question.
# Suppose we want to know what is the probability that we are within one percentage
#    point from p-- that we made a very, very good estimate?
# So we're basically asking, what's the probability that the distance between
#    X-bar and p, the absolute value of X-bar minus p, is less than 0.01,
#    1 percentage point.
# We can use what we've learned to see that this is the same as asking, what
#    is the probability of X-bar being less than or equal to p plus 0.01
#    minus the probability of X-bar being less than or equal to p minus 0.01.
# Now, can we answer the question now?
#    Can we compute that probability?
#    Note that we can use the mathematical trick that we learned in the previous
#    module.
# What was that trick?
#    We subtract the expected value and divide by the standard error on both
#    sides of the equation.
# What this does is it gives us a standard normal variable, which we have been
#    calling capital Z, on the left side.
# And we know how to make calculations for that.
# Since p is the expected value, and the standard error of X-bar is the square
#    root of p times 1 minus p divided by N, we get that the probability that
#    we were just calculating is equivalent to probability of Z, our standard
#    normal variable, being less than 0.01 divided by the standard error of X-bar
#    minus the probability of Z being less than negative 0.01 divided by that
#    standard error of X-bar.
# OK, now can we compute this probability?
#     Not yet.
# Our problem is that we don't know p.
# So we can't actually compute the standard error of X-bar using just the data.
# But it turns out-- and this is something new we're showing you- that the CLT
#    still works if we use an estimate of the standard error that, instead of p,
#    uses X-bar in its place.
# We say this is a plug-in estimate.
# We call this a plug-in estimate.
# Our estimate of the standard error is therefore the square root of X-bar times
#    1 minus X-bar divided by N.
#    SE( X_ ) = sqrt( p ( 1-p ) / N )
#
# Notice, we changed the p for the X-bar.
# In the mathematical formula we're showing you, you can see a hat on top of the SE.
# In statistics textbooks, we use a little hat like this to denote estimates.
# This is an estimate of the standard error, not the actual standard error.
# But like we said, the central limit theorem still works. Note that, importantly,
#    that this estimate can actually be constructed using the observed data.
# Now, let's continue our calculations. But now instead of dividing by the
#    standard error, we're going to divide by this estimate of the standard error.
# Let's compute this estimate of the standard error for the first sample that we
#    took, in which we had 12 blue beads and 13 red beads.
# In that case, X-bar was 0.48.  So to compute the standard error, we simply
#    write this code.
X_hat <- 0.48
se <- sqrt( X_hat * ( 1 - X_hat ) / 25 )
se
# And we get that it's about 0.1.
# So now, we can answer the question.
# Now, we can compute the probability of being as close to p as we wanted.
# We wanted to be 1 percentage point away.
# The answer is simply pnorm of 0.01--
#     that's 1 percentage point--
# divided by this estimated se minus pnorm of negative 0.01
# divided by the estimated se.
pnorm( 0.01 / se ) - pnorm( -0.01 / se )
# We plug that into R, and we get the answer.
# The answer is that the probability of this happening is about 8%.
# So there is a very small chance that we'll be as close as this to the actual
#    proportion.
# Now, that wasn't very useful, but what it's going to do, what we're going to be
#    able to do with the central limit theorem is determine what sample sizes are
#    better.  And once we have those larger sample sizes, we'll be able to provide
#    a very good estimate and some very informative probabilities.



# Margin of Error
#  So a poll of only 25 people is not really very useful, at least for a close
#     election.
# Earlier we mentioned the margin of error.
# Now we can define it because it is simply 2 times the standard error, which
#    we can now estimate.
# In our case it was 2 times se, which is about 0.2.
2 * se
# Why do we multiply by 2?
#     This is because if you ask what is the probability that we're within 2
#     standard errors from p, using the same previous equations, we end up with
#     an equation like this one.
pnorm( 2 ) - pnorm( -2 )
# This one simplifies out, and we're simply asking what is the probability of
#     the standard normal distribution that has the expected value 0 and
#     standard error one is within two values from 0, and we know that this is
#     about 95%.
# So there's a 95% chance that X-bar will be within 2 standard errors.
# That's the margin of error, in our case, to p.
# Now why do we use 95%?
#     This is somewhat arbitrary.
# But traditionally, that's what's been used.
# It's the most common value that's used to define margins of errors.
# In summary, the central limit theorem tells us that our poll based on a sample
#    of just 25 is not very useful.
# We don't really learn much when the margin of error is this large.
# All we can really say is that the popular vote will not be won by a large margin.
# This is why pollsters tend to use larger sample sizes.
# From the table that we showed earlier from RealClearPolitics, we saw that a
#    typical sample size was between 700 and 3,500.
# To see how this gives us a much more practical result, note that if we had
#    obtained an X-bar of 0.48, but with a sample size of 2,000, the estimated
#    standard error would have been about 0.01.
# So our result is an estimate of 48% blue beads with a margin of error of 2%.
# In this case, the result is much more informative and would make us think that
#    there are more red beads than blue beads.
# But keep in mind, this is just hypothetical.
# We did not take a poll of 2,000 beads since we don't want to ruin the competition.



# A Monte Carlo Simulation for the CLT
# Suppose we want to use a Monte Carlo simulation to corroborate that the tools
#    that we've been using to build estimates and margins of errors using
#    probability theory actually work.  To create the simulation, we would need
#    to write code like this.

B <- 10000
N <- 1000
X_hat <- replicate( B, {
    X <- sample( c( 0, 1), size = N, replace = TRUE, prob = c( 1 - p, p ) )
    mean( X )
} )

B
N
X_hat

# We would simply write the urn model, use replicate to construct a Monte Carlo
#    simulation.
# The problem is, of course, that we don't know p.  We can't run the code we just
#    showed you because we don't know what p is.
# However, we could construct an urn like the one we showed in a previous video
#    and actually run an analog simulation.  It would take a long time because
#    you would be picking beads and counting them, but you could take 10,000
#    samples, count the beads each time, and keep track of the proportions that
#    you see.
# We can use the function take poll with n of 1,000 instead of actually drawing
#    from an urn, but it would still take time because you would have to count
#    the beads and enter the results into R.  So one thing we can do to corroborate
#    theoretical results is to pick a value of p or several values of p and then
#    run simulations using those.
# As an example, let's set p to 0.45.

p <- 0.45
B <- 10000
N <- 1000
X_hat <- replicate( B, {
    X <- sample( c( 0, 1), size = N, replace = TRUE, prob = c( 1 - p, p ) )
    mean( X )
} )
X_hat

# We can simulate one poll of 1,000 beads or people using this simple code.
# Now we can take that into a Monte Carlo simulation.
# Do it 10,000 times, each time returning the proportion of blue beads that we
#    get in our sample.  To review, the theory tells us that X-bar has
#    approximately normal distribution with expected value 0.45 and a standard
#    error of about 1.5%.
# The simulation confirms this.  If we take the mean of the X-hats that we created,
#    we indeed get a value of about 0.45.  And if we compute the sd of the values
#    that we just created, we get a value of about 1.5%.

mean( X_hat )
sd( X_hat )

# A histogram and a qq plot of this X-hat data confirms that the normal
#    approximation is accurate as well.

library( gridExtra )

p1 <- data.frame( X_hat = X_hat ) %>% ggplot( aes( X_hat ) ) +
    geom_histogram( binwidth = 0.005, color = 'black' )

p2 <- data.frame( X_hat = X_hat ) %>% ggplot( aes( sample = X_hat ) ) +
    stat_qq( dparams = list( mean = mean( X_hat ), sd = sd( X_hat ) ) ) +
    geom_abline() +
    ylab( 'X_hat' ) +
    xlab( 'Theoretical normal')
grid.arrange( p1, p2, nrow = 1 )

# Again, note that in real life, we would never be able to run such an experiment
#    because we don't know p.
# But we could run it for various values of p and sample sizes N and see that
#    the theory does indeed work well for most values.
# You can easily do this yourself by rerunning the code we showed you after
#    changing p and N.



# The Spread
# The competition is to predict the spread, not the proportion p.
# However, because we are assuming there are only two parties, we know that the
#    spread is just p minus (1 minus p),which is equal to 2p minus 1.
# p - ( 1-p ) = 2p - 1
# So everything we have done can easily be adapted to estimate to p minus 1.
# Once we have our estimate, X-bar, and our estimate of our standard error of
#    X-bar, we estimate the spread by 2 times X-bar minus 1, just plugging in
#    the X-bar where you should have a p.
# 2X_ - 1
# And, since we're multiplying a random variable by 2, we know that the standard
#    error goes up by 2.
# 2SE( X_ )
# So the standard error of this new random variable is 2 times the standard error
#    of X-bar.
# Note that subtracting the 1 does not add any variability, so it does not affect
#    the standard error.
# So, for our first example, with just the 25 beads, our estimate of p was 0.48
#    with a margin of error of 0.2.
# This means that our estimate of the spread is 4 percentage points, 0.04, with
#    a margin of error of 40%, 0.4.
# Again, not a very useful sample size.  But the point is that once we have an
#    estimate and standard error for p, we have it for the spread 2p minus 1.



# Bias: Why not run a very large poll?
#  Note that for realistic values of p, say between 0.35 and 0.65 for the popular
#     vote, if we run a very large poll with say 100,000 people, theory would
#     tell us that we would predict the election almost perfectly, since the
#     largest possible margin of error is about 0.3%.  Here are the calculations
#     that were used to determine that.

N <- 100000
p <- seq( 0.35, 0.65, length = 100 )
SE <- sapply( p, function( x ) 2 * sqrt( x * ( 1 - x ) / N ) )
data.frame( p = p, SE = SE ) %>% ggplot( aes( p, SE ) ) +
    geom_line()

# We can see a graph showing us the standard error for several values of p if we
#    fix N to be 100,000.
# So why are there no pollsters that are conducting polls this large?
#    One reason is that running polls with a sample size of 100,000 is very
#    expensive.  But perhaps a more important reason is that theory has its
#    limitations.
# Polling is much more complicated than picking beads from an urn.  For example,
#    while the beads are either red or blue, and you can see it with your eyes,
#    people, when you ask them, might lie to you.  Also, because you're conducting
#    these polls usually by phone, you might miss people that don't have phones.
#    And they might vote differently than those that do. But perhaps the most
#    different way an actual poll is from our urn model is that we actually don't
#    know for sure who is in our population and who is not.
# How do we know who is going to vote?
# Are we reaching all possible voters?
# So, even if our margin of error is very small, it may not be exactly right that
#    our expected value is p.  We call this bias.  Historically, we observe that
#    polls are, indeed, biased, although not by that much.  The typical bias
#    appears to be between 1% and 2%.
# This makes election forecasting a bit more interesting.
