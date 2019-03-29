# Stratification and Variance Explained


# Anscombe's Quarted/Stratification

# To help us understand when it is that correlation is meaningful as a summary
#    statistic, we'll try to predict the son's height using the father's height.
#    This will help motivate and define linear regression.
# We start by demonstrating how correlation can be useful for prediction.
# Suppose we are asked to guess the height of a randomly selected son.
# Because of the distribution of the son height is approximately normal, we know
#    that the average height of 70.5 inches is a value with the highest proportion
#    and would be the prediction with the chances of minimizing the error.
# But what if we are told that the father is 72 inches?
#    Do we still guess 70.5 inches for the son?
#    The father is taller than average, specifically he is 1.14 standard deviations
#    taller than the average father.  So shall we predict that the son is also
#    1.14 standard deviations taller than the average son?
# It turns out that this would be an overestimate.  To see this, we look at all
#    the sons with fathers who are about 72 inches.  We do this by stratifying
#    the father's side.  We call this a conditional average, since we are computing
#    the average son height conditioned on the father being 72 inches tall.
# A challenge when using this approach in practice is that we don't have many
#    fathers that are exactly 72.  In our data set, we only have eight.  If we
#    change the number to 72.5, we would only have one father who is that height.
#    This would result in averages with large standard errors, and they won't be
#    useful for prediction for this reason.
# But for now, what we'll do is we'll take an approach of creating strata of
#    fathers with very similar heights.  Specifically, we will round fathers'
#    heights to the nearest inch.  This gives us the following prediction for the
#    son of a father that is approximately 72 inches tall.

conditional_avg <- galton_heights %>%
    filter( round( father ) == 72 ) %>%
    summarize( avg = mean( son ) ) %>%
    .$avg
conditional_avg

# We can use this code and get our answer, which is 71.84.
# This is 0.54 standard deviations larger than the average son, a smaller number
#    than the 1.14 standard deviations taller that the father was above the average
#    father.
# Stratification followed by box plots lets us see the distribution of each group.
#   Here is that plot.

galton_heights %>%
    mutate( father_strata = factor( round( father ) ) ) %>%
    ggplot( aes( father_strata, son ) ) +
    geom_boxplot() +
    geom_point()

# We can see that the centers of these groups are increasing with height, not
#    surprisingly.  The means of each group appear to follow a linear relationship.
#    We can make that plot like this, with this code.

galton_heights %>%
    mutate( father = round( father ) ) %>%
    group_by( father ) %>%
    summarize( son_conditional_avg = mean( son ) ) %>%
    ggplot( aes( father, son_conditional_avg ) ) +
    geom_point()

# See the plot and notice that this appears to follow a line.  The slope of this
#    line appears to be about 0.5, which happens to be the correlation between
#    father and son heights.  This is not a coincidence.  To see this connection,
#    let's plot the standardized heights against each other, son versus father,
#    with a line that has a slope equal to the correlation. Here's the code.
#    Here's a plot.

r <- galton_heights %>%
    summarize( r = cor( father, son ) ) %>%
    .$r

galton_heights %>%
    mutate( father = round( father ) ) %>%
    group_by( father ) %>%
    summarize( son = mean( son ) ) %>%
    mutate( z_father = scale( father ), z_son = scale( son ) ) %>%
    ggplot( aes( z_father, z_son ) ) +
    geom_point() +
    geom_abline( intercept = 0, slope = r )

# This line is what we call the regression line.
# Here, we define it and compute it for the data at hand.  The regression line
#    for two variables, x and y, tells us that for every standard deviation sigma
#    x increase above the average mu x.  For x, y grows rho standard deviations
#    sigma y above the average mu y.  The formula fot he relation line is therefore
#    this one.

# ( ( Yi - muy) / sigmay ) = ro( ( Xi - mux ) / sigmax )

# If there's perfect correlation, we predict an increase that is the same number
#    of SDs.  If there's zero correlation, then we don't use x at all for the
#    prediction of y.  For values between 0 and 1, the prediction is somewhere
#    in between.  If the correlation is negative, we predict a reduction, instead
#    of an increase.  It is because when the correlation is positive but lower
#    than the one, that we predict something closer to the mean, that we call this
#    regression.  The son regresses to the average height.  In fact, the title of
#    Galton's paper was "Regression Towards Mediocrity in Hereditary Stature."
# Note that if we write this in the standard form of a line, y equals b plus mx,
#    where b is the intercept and m is the slope, the regression line has slope
#    rho times sigma y, divided by sigma x, and intercept mu y, minus mu x, times
#    the slope.

# Fo regression line:   y = b + mx
#    slope m = ro( sigmay / sigma x)
#    intercept b = muy -m mux

# So if we standardize the variable so they have average 0 and standard deviation 1.
# Then the regression line has intercept 0 and slope equal to the correlation rho.
# Let's look at the original data, father son data, and add the regression line.
# We can compute the intercept and the slope using the formulas we just derived.
# Here's a code to make the plot with the regression line.

mu_x <- mean( galton_heights$father )
mu_y <- mean( galton_heights$son )
s_x <- sd( galton_heights$father )
s_y <- sd( galton_heights$son )
r <- cor( galton_heights$father, galton_heights$son )
m <- r * s_y / s_x
b <- mu_y - m * mu_x

# If we plot the data in standard units, then, as we discussed, the regression
#    line as intercept 0 and slope rho.  Here's the code to make that plot.

galton_heights %>%
    ggplot( aes( father, son ) ) +
    geom_point( alpha = 0.5 ) +
    geom_abline( intercept = b, slope = m )

# If we plot the data in standard units, then, as we discussed, the regression
#    line as intercept 0 and slope rho.  Here's the code to make that plot.

galton_heights %>%
    ggplot( aes( scale( father ), scale( son ) ) ) +
    geom_point( alpha = 0.5 ) +
    geom_abline( intercept = 0, slope =  r )

# We started this discussion by saying that we wanted to use the conditional means
#    to predict the heights of the sons.  But then we realized that there were
#    very few data points in each strata.  When we did this approximation of rounding
#    off the height of the fathers, we found that these conditional means appear
#    to follow a line.  And we ended up with the regression line.  So the regression
#    line gives us the prediction.  An advantage of using the regression line is
#    that we used all the data to estimate just two parameters, the slope and the
#    intercept.  This makes it much more stable.  When we do conditional means,
#    we had fewer data points, which made the estimates have a large standard error,
#    and therefore be unstable.  So this is going to give us a much more stable
#    prediction using the regression line.  However, are we justified in using
#    the regression line to predict?  Galton gives us the answer.

# -------       -------       -------       -------       -------       -------

# Question 1
# Look at the figure below. The slope of the regression line in this figure is equal to what, in words?
#
# scatter plot of son and father heights with son heights on the y-axis and father heights on the x-axis
#
# Slope = (correlation coefficient of son and father heights) * (standard deviation of sons’ heights / standard deviation of fathers’ heights)    <-*
# Slope = (correlation coefficient of son and father heights) * (standard deviation of fathers’ heights / standard deviation of sons’ heights)
# Slope = (correlation coefficient of son and father heights) / (standard deviation of sons’ heights * standard deviation of fathers’ heights)
# Slope = (mean height of fathers) - (correlation coefficient of son and father heights * mean height of sons).



# Question 2
# Why does the regression line simplify to a line with intercept zero and slope when we standardize our x and y variables? Try the simplification on your own first!
# When we standardize variables, both x and y will have a mean of one and a standard deviation of zero. When you substitute this into the formula for the regression line, the terms cancel out until we have the following equation: .
# When we standardize variables, both x and y will have a mean of zero and a standard deviation of one. When you substitute this into the formula for the regression line, the terms cancel out until we have the following equation: .    <-*
# When we standardize variables, both x and y will have a mean of zero and a standard deviation of one. When you substitute this into the formula for the regression line, the terms cancel out until we have the following equation: .



# Question 3
# What is a limitation of calculating conditional means?
# Select ALL that apply.
#
# Each stratum we condition on (e.g., a specific father’s height) may not have many data points.  <-*
# Because there are limited data points for each stratum, our average values have large standard errors.  <-*
# Conditional means are less stable than a regression line.   <-*
# Conditional means are a useful theoretical tool but cannot be calculated.

# -------       -------       -------       -------       -------       -------

# Bivariate Normal Distribution

# Correlation and the regression line are widely used summary statistics. But
#    it is often misused or misinterpreted.  As come is example provided two example
#    data sets in which summarizing with a correlation would be a mistake.  But
#    we also see it in the media and in scientific literature as well.  The main
#    way we motivate the use of correlation involve what is called the bivariate
#    normal distribution.  When a pair of random variables is approximated by a
#    bivariate normal distribution, the scatterplot looks like ovals, like American
#    footballs.  They can be thin.  That's when they have high correlation.  All
#    the way up to a circle shape when they have no correlation. We saw some
#    examples previously.
# A more technical way to define the bivariate normal distribution is the following.
#    First, this distribution is defined for pairs.  So we have two variables,
#    x and y.  And they have paired values.  They are going to be bivariate normally
#    distributed if the following happens.
# If x is a normally distributed random variable, and y is also a normally distributed
#    random variable

# X = x

#    -- and for any grouping of x that we can define, say, with x
#    being equal to some predetermined value, which we call here in this formula
#    little x-- then the y's in that group are approximately normal as well.
#    If this happens, then the pair is approximately bivariate normal.
# When we fix x in this way, we then refer to the resulting distribution of the
#    y's in the group-- defined by setting x in this way-- as the conditional
#    distribution of y given x. We write the notation like this for the conditional
#    distribution and the conditional expectation.

# fY | X = x = is the conditional distribution
# E( Y | X = x ) is the conditional expected value

# If we think the height data is well-approximated by the bivariate normal distribution,
#    then we should see the normal approximation hold for each grouping.  Here,
#    we stratify the son height by the standardized father heights and see that
#    the assumption appears to hold.  Here's the code that gives us the desired plot.

galton_heights %>%
    mutate( z_father = round( ( father - mean( father ) )  / sd( father ) ) ) %>%
    filter( z_father %in% -2:2 ) %>%
    ggplot() +
    stat_qq( aes( sample = son ) ) +
    facet_wrap( ~z_father )

# Now, we come back to defining correlation.

# E( Y | X = x ) = muy + roh ( ( X - mux ) / sigmax) sigmay

# Galton showed-- using mathematical statistics-- that when two variables follow
#   a bivariate normal distribution, then for any given x the expected value of
#   the y in pairs for which x is set at that value is mu y plus rho x minus mu
#   x divided by sigma x times sigma y.
# Note that this is a line with slope rho times sigma y divided by sigma x

# slope = roh( sigmay  / sigmax )

# and intercept mu y minus n times mu x.

# intercept = muy - m mux

# And therefore, this is the same as the regression line we saw in a previous video.
# That can be written like this.

# ( E( Y | X = x) - muy ) / sigmay = roh( x - mux ) / sigmax

# So in summary, if our data is approximately
#   bivariate, then the conditional expectation-- which is the best prediction
#   for y given that we know the value of x-- is given by the regression line.

# -------       -------       -------       -------       -------       -------

# Question 1
# A regression line is the best prediction of Y given we know the value of X when:
# X and Y follow a bivariate normal distribution.    <-*
# Both X and Y are normally distributed.
# Both X and Y have been standardized.
# There are at least 25 X-Y pairs.


# Question 2
# Which one of the following scatterplots depicts an x and y distribution that is NOT well-approximated by the bivariate normal distribution?
# scatter plot with v-shaped distribution of points centered on zero   <-*
# scatter plot with rising slope and a roughly oval shaped distribution
# scatter plot with slope of approximately zero and a very round oval shaped distribution
# scatter plot with negative slope and a very long, tight oval shaped distribution of points

# -------       -------       -------       -------       -------       -------

# Variance Explained
# The theory we've been describing also tells us that the standard deviation of
#   the conditional distribution that we described in a previous video is Var of
#   Y given X equals sigma y times the square root of 1 minus rho squared.

# Var( Y | X = x ) = sigmay sqrt( 1 - roh^2 )

# This is where statements like x explains such and such percent of the variation
#    in y comes from.  Note that the variance of y is sigma squared.

# Variance of Y is sigma^2

# That's where we start.  If we condition on x, then the variance goes down to 1
#    minus rho squared times sigma squared y.

# condition on X: variance goes down to:  ( 1 - rho^2 ) sigmay^2

# So from there, we can compute how much the variance has gone down.  It has gone
#    down by rho squared times 100%.  So the correlation and the amount of variance
#    explained are related to each other.  But it is important to remember that
#    the variance explained statement only makes sense when the data is approximated
#    by a bivariate normal distribution.

# -------       -------       -------       -------       -------       -------

# Question 1
# We previously calculated that the correlation coefficient rho between fathers’ and sons’ heights is 0.5.
#
# Given this, what percent of the variation in sons’ heights is explained by fathers’ heights?
# 0%
# 25%   <-* rho^2
# 50%
# 75%

# -------       -------       -------       -------       -------       -------

# There are two regression lines
# We computed a regression line to predict the son's height from the father's
#   height.  We used these calculations-- here's the code-- to get the slope and
#   the intercept.

mu_x <- mean( galton_heights$father )
mu_y <- mean( galton_heights$son )
s_x <- sd( galton_heights$father )
s_y <- sd( galton_heights$son )
r <- cor( galton_heights$father, galton_heights$son )
m <- r * s_y / s_x
b <- mu_y - m * mu_x

# This gives us the function that the conditional expectation of y given
#    x is 35.7 plus 0.5 times x.

# E( Y | X = x ) = 35.7 + 0.5x

# So, what if we wanted to predict the father's height based on the son's?
#    It is important to know that this is not determined by computing the inverse
#    function of what we just saw, which would be this equation here.

# x = { E( Y | X = x) - b } / 0.5

# We need to compute the expected value of x given y.

# E( X | Y = y )

# This gives us another regression function altogether, with slope and intercept
#    computed like this.

m <- r * s_x / s_y
b <- mu_x - m * mu_y


# So now we get that the expected value of x given y, or the expected value of the
#    father's height given the son's height, is equal to 34 plus 0.5 y, a different
#    regression line.

# E( X | Y = y ) = 34 + 0.5y
