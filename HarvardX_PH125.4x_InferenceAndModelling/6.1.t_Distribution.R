# The t-Distribution
# Previously, we made use of the Central Limit Theorem with sample sizes as small
#    as 15.  Because we're also estimating a second parameter, sigma, further
#    variability is introduced into our confidence interval.  And this results
#    in a confidence interval that is overconfident because it doesn't account
#    for that variability.
# For very large sample sizes, this extra variability is negligible.  But in general,
#    for values smaller than 30, we need to be cautious about using the Central
#    Limit Theorem.
# However, if the data in the urn is known to follow a normal distribution, in
#   other words, if the population data is known to follow a normal distribution,
#   then we actually have a mathematical theory that tells us how much bigger we
#   need to make the intervals to account for the estimation of sigma.
# Using this theory, we can construct confidence intervals for any urn but again,
#   only if the data in the urn is known to follow a normal distribution.
# So for the 0, 1 data of previous urn models, this theory definitely does not
#    apply.
# The statistic on which confidence intervals for d are based is this one.
# We've seen it earlier.
# We call it Z. The CLT tells us that Z is approximately normally distributed with
#    expected value 0 and standard error 1.  But in practice, we don't know sigma,
#    so we use s instead.  We substitute s where we have the sigma.  But by doing
#    this, we introduce some variability.  The s, as variability, is estimated from
#    data.  This theory that we mentioned tells us that Z follows what is called
#    a t-distribution with what is called N minus 1 degrees of freedom.
# The degrees of freedom is a parameter that controls the variability via what
#    are called fatter tails.  You can see that in this figure, where we have
#    t-distributions with degrees of freedom 3, 5, and 15.  And you can see how
#    the tails, the ends, go higher and higher, meaning that large values have
#    larger probabilities for smaller values of the degrees of freedom.
# In our case of pollster data, if we are willing to assume that the pollster
#    effect data is normally distributed, then we can use this theory.
# Based on the sample, we can corroborate if, in fact, the data is normally
#    distributed.  Here's a q-q plot showing us our sample data versus a normal
#    distribution.  It's not a perfect match, but is relatively close.  And this
#    particular theory is quite robust to deviations from normality.  So once we
#    make that decision, then, perhaps a better confidence interval for d is
#    constructed using the t-distribution instead of the normal distribution.
# So all we change is the 1.96.  We now change to the quantile coming from a
#    t-distribution with 14 degrees of freedom.  The new confidence interval
#    goes from 1.5% to 4.2%.

z <- qt( 0.975, nrow( one_poll_per_pollster ) -1 )
one_poll_per_pollster %>%
    summarize( avg = mean( spread ), moe = z * sd( spread ) / sqrt( length( spread ) ) ) %>%
    mutate( start = avg - moe, end = avg + moe )


# So it is a little bit bigger than the one we made using the normal distribution.
# This is, of course, expected because the quantile from the t-distribution is
#    larger than the quantile from the normal distribution, as we can see here.

qt( 0.975, 14 )
qnorm( 0.975 )

# FiveThirtyEight uses the t-distribution to generate errors that better model
#    the deviation we see in election data.
# Again, because they have fatter tails.
# So, for example, the deviation we saw in Wisconsin between the polls and the
#    actual result, the actual result was that Trump won by 0.7%, is more in line
#    with distributed data than normal distributed data.
