# Linear Models

# Introduction to Linear Models
# Confounding: Are BBs More Predictive?
# In a previous video, we found that the slope of the regression line for predicting
#    runs from bases on balls was 0.735.  So, does this mean that if we go and
#    higher low salary players with many bases on balls that increases the number
#    of walks per game by 2 for our team?
# Our team will score 1.47 more runs per game?  We are again reminded that association
#    is not causation.
# The data does provide strong evidence that a team with 2 more bases on balls
#    per game than the average team scores 1.47 more runs per game, but this does
#    not mean that bases on balls are the cause.  If we do compute the regression
#    line slope for singles, we get 0.449, a lower value.  Note that a single gets
#    you to first base just like a base on ball.  Those that know a little bit more
#    about baseball will tell you that with a single, runners that are on base have
#    a better chance of scoring than with a base on balls.  So, how can base on
#    balls be more predictive of runs?
# The reason this happens is because of confounding.  Note the correlation between
#    homeruns, bases on balls, and singles.

Teams %>%
    filter( yearID %in% 1961:2001 ) %>%
    mutate( Singles = ( H - HR - X2B - X3B ) / G, BB = BB / G, HR = HR / G ) %>%
    summarize( cor( BB, HR ), cor( Singles, HR ), cor( BB, Singles ) )

# We see that the correlation between bases on balls and homeruns is quite high
#    compared to the other two pairs.  It turns out that pitchers, afraid of homeruns,
#    will sometimes avoid throwing strikes to homerun hitters.  As a result, homerun
#    hitters tend to have more bases on balls.  Thus, a team with many homeruns
#    will also have more bases on balls than average, and as a result, it may appear
#    that bases on balls cause runs.  But it is actually the homeruns that caused
#    the runs.  In this case, we say that bases on balls are confounded with homeruns.
#    But could it be that bases on balls still help?  To find out, we somehow have
#    to adjust for the homerun effect.  Regression can help with this.

# -------       -------       -------       -------       -------       -------

# Question 1
# Why is the number of home runs considered a confounder of the relationship between bases on balls and runs per game?
# Home runs is not a confounder of this relationship.
# Home runs are the primary cause of runs per game.
# The correlation between home runs and runs per game is stronger than the correlation between bases on balls and runs per game.
# Players who get more bases on balls also tend to have more home runs; in addition, home runs increase the points per game.   <-*

# -------       -------       -------       -------       -------       -------

# Stratification and Multivariate Regression
#  To try to determine if bases on balls is still useful for creating runs, a first
#     approach is to keep home runs fixed at a certain value and then examine the
#     relationship between runs and bases on balls.  As we did when we stratified
#     fathers by rounding to the closest inch, here, we can stratify home runs per
#     game to the closest 10th.  We filtered our strata with few points.
# We use this code to generate an informative data set.

dat <- Teams %>%
    filter( yearID %in% 1961:2001 ) %>%
    mutate( HR_strata = round( HR / G, 1),
            BB_per_game = BB / G,
            R_per_game = R / G ) %>%
    filter( HR_strata >= 0.4 & HR_strata <= 1.2 )
head( dat )

# And then, we can make a scatter plot for each strata.  A scatterplot of runs
#    versus bases on balls.  This is what it looks like.

dat %>%
    ggplot( aes( BB_per_game, R_per_game ) ) +
    geom_point( alpha = 0.5 ) +
    geom_smooth( method = 'lm' ) +
    facet_wrap( ~HR_strata )

# Remember that the regression slope for predicting runs with bases on balls when
#    we ignore home runs was 0.735.  But once we stratify by home runs, these slopes
#    are substantially reduced.  We can actually see what the slopes are by using
#    this code.

dat %>%
    group_by( HR_strata ) %>%
    summarize( slope = cor( BB_per_game, R_per_game ) * sd( R_per_game ) / sd( BB_per_game ) )

# We stratify by home run and then compute the slope using the formula that we
#    showed you previously.
# These values are closer to the slope we obtained from singles, which is 0.449.
#    Which is more consistent with our intuition.  Since both singles and bases on
#    ball get us to first base, they should have about the same predictive power.
# Now, although our understanding of the application-- our understanding of baseball
#    -- tells us that home runs cause bases on balls and not the other way around,
#    we can still check if, after stratifying by base on balls, we still see a
#    home run effect or if it goes down.  We use the same code that we just used
#    for bases on balls.  But now, we swap home runs for bases on balls to get
#    this plot.

dat <- Teams %>%
    filter( yearID %in% 1961:2001 ) %>%
    mutate( BB_strata = round( BB / G, 1 ),
            HR_per_game = HR / G,
            R_per_game = R / G ) %>%
    filter( BB_strata >= 2.8 & BB_strata <= 3.9 )
dat %>%
    ggplot( aes( HR_per_game, R_per_game ) ) +
    geom_point( alpha = 0.5 ) +
    geom_smooth( method = 'lm' ) +
    facet_wrap( ~BB_strata )

# In this case, the slopes are the following.
 dat %>%
     group_by( BB_strata ) %>%
     summarize( slope = cor( HR_per_game, R_per_game ) * sd( R_per_game ) / sd( HR_per_game ) )

# You can see they are all around 1.5, 1.6, 1.7.
# So they do not change that much from the original slope estimate, which was 1.84.
#    Regardless, it seems that if we stratify by home runs, we have an approximately
#    bivariate normal distribution for runs versus bases on balls.
# Similarly, if we stratify by bases on balls, we have an approximately normal
#    bivariate distribution for runs versus home runs.
# So what do we do?
#     It is somewhat complex to be computing regression lines for each strata.
#    We're essentially fitting this model that you can see in this equation with
#    the slopes for x1 changing for different values of x2 and vice versa.

# E[ R | BB = x1, HR = x2 ] = Beta0 + Beta1 ( x2 ) x1 + Beta2 ( x1 ) x2
# Here, x1 is bases on balls.   And x2 are home runs.

# Is there an easier approach?
# Note that if we take random variability into account, the estimated slopes by
#    strata don't appear to change that much.  If these slopes are in fact the
#    same, this implies that this function beta 1 of x2 and the other function
#    beta 2 of x1 are actually constant.

# Beta1 ( x2 )
# Beta2 ( x1 )

# Which, in turn, implies that the expectation of runs condition on home runs
#    and bases on balls can be written in this simpler model.

# E[ R | BB = x1, HR = x2 ] = Beta0 + Beta1 x1 + Beta2 x2

# Which, in turn, implies that the expectation of runs condition on home runs
#    and bases on balls can be written in this simpler model.  This model implies
#    that if the number of home runs is fixed, we observe a linear relationship
#    between runs and bases on balls.  And that the slope of that relationship
#    does not depend on the number of home runs.  Only the slope changes as the
#    home runs increase.
# The same is true if we swap home runs and bases on balls.  In this analysis,
#    referred to as multivariate regression, we say that the bases on balls slope
#    beta 1 is adjusted for the home run effect.  If this model is correct, then
#    confounding has been accounted for.
# But how do we estimate beta 1 and beta 2 from the data?
#     For this, we'll learn about linear models and least squares estimates.

 # -------       -------       -------       -------       -------       -------

# Question 1
# As described in the video, when we stratified our regression lines for runs per game vs. bases on balls by the number of home runs, what happened?
# The slope of runs per game vs. bases on balls within each stratum was reduced because we removed confounding by home runs.   <-*
# The slope of runs per game vs. bases on balls within each stratum was reduced because there were fewer data points.
# The slope of runs per game vs. bases on balls within each stratum increased after we removed confounding by home runs.
# The slope of runs per game vs. bases on balls within each stratum stayed about the same as the original slope.



# Linear Models
# Since Galton's original development, regression has become one of the most widely
#     used tools in data science.  One reason for this has to do with the fact that
#     regression permits us to find relationships between two variables while adjusting
#     for others, as we have just shown for bases on balls and home runs.
# This has been particularly popular in fields where randomized experiments are
#    hard to run, such as economics and epidemiology.  When we're not able to
#    randomly assign each individual to a treatment or control group, confounding
#    is particularly prevalent.
# For example, consider estimating the effect of any fast foods on life expectancy
#    using data collected from a random sample of people in some jurisdiction.
# Fast food consumers are more likely to be smokers, drinkers, and have lower
#    incomes.  Therefore, a naive regression model may lead to an overestimate
#    of a negative health effect of fast foods.  So how do we adjust for confounding
#    in practice?
# We can use regression.  We have described how, if data is by very normal, then
#    the conditional expectation follow a regression line, that the conditional
#    expectation as a line is not an extra assumption, but rather a result derived
#    from the assumption, that they are approximately by very normal.
# However, in practice it is common to explicitly write down a model that describes
#    the relationship between two or more variables using what is called a linear
#    model.
# We know that linear here does not refer to lines exclusively, but rather to
#    the fact that the conditional expectation is a linear combination of known
#    quantities.  Any combination that multiplies them by a constant and then
#    adds them up with, perhaps, a shift.
#    For example,  2 + 3x - 4y + 5z
# So Beta0 + Beta1 x1 + Beta2 x2 is a linear combination of x1 and x2.
#    The simplest linear model is a constant Beta0.
#    The second simplest is a line, Beta0 + Beta1 x.
# For Galton's data, we would denote and observe fathers' heights with x1 through xn.
#    Then we model end son heights we are trying to predict with the following model.

# Yi = Beta0 + Beta1 xi + Epsiloni, i = 1, ..., N
# xi = fathers's height
# Yi = random son's height that we want to predict
# Epsilon = errors, independent from each other, and have exprected value 0

# Here, the little xi's are the father's heights, which are fixed not random, due
#    to the conditioning.  We've conditioned on these values.  And then Yi big Yi
#    is the random son's height that we want to predict.  We further assume that
#    the errors that are denoted with the Greek letter for E, epsilon, epsilon i,
#    are independent from each other, have expected value 0, and the standard
#    deviation, which is usually called sigma, does not depend on i.
# It's the same for every individual.  We know the xi, but to have a useful model
#    for prediction, we need beta 0 and beta 1.  We estimate these from the data.
#    Once we do, we can predict the sons' heights from any father's height, x.
#    Note that if we further assume that the epsilons are normally distributed,
#    then this model is exactly the same one we derived earlier for the bivariate
#    normal distribution.
# A somewhat nuanced difference is that in the first approach, we assumed the data
#    was a bivariate normal, and the linear model was derived, not assumed.
# In practice, linear models are just assumed without necessarily assuming normality.
#    The distribution of the epsilons is not specified. But nevertheless, if your
#    data is bivariate normal, the linear model that we just showed holds.  If your
#    data is not bivariate normal, then you will need to have other ways of justifying
#    the model.
# One reason linear models are popular is that they are interpretable.
# In the case of Galton's data, we can interpret the data like this.
# Due to inherited genes, the son's height prediction grows by beta 1 for each
#    inch we increase the father's height x.  Because not all sons with fathers
#    of height x are of equal height, we need the term epsilon, which explains the
#    remaining variability.
# This remaining variability includes the mother's genetic effect, environmental
#    factors, and other biological randomness.  Note that given how we wrote the
#    model, the intercept beta 0 is not very interpretable, as it is the predicted
#    height of a son with a father with no height.  Due to regression to the mean,
#    the prediction will usually be a bit larger than 0, which is really not very
#    interpretable.  To make the intercept parameter more interpretable, we can
#    rewrite the model slightly in the following way. Here, we have changed xi to
#    xi minus the average height x bar.

# Yi = Beta0 + Beta1 ( xi - x_ ) + Epsiloni, i = 1, ..., N

# We have centered our covariate xi.  In this case, beta 0, the intercept, would
#    be the predicted height for the average father for the case where xi equals
#    x bar.

 # -------       -------       -------       -------       -------       -------

# Question 1
# We run a linear model for sons’ heights vs. fathers’ heights using the Galton height data, and get the following results:
#
# > lm(son ~ father, data = galton_heights)
#
# Call:
#     lm(formula = son ~ father, data = galton_heights)
#
# Coefficients:
#     (Intercept)    father
#        35.71       0.50
# Interpret the numeric coefficient for "father."
# For every inch we increase the son’s height, the predicted father’s height increases by 0.5 inches.
# For every inch we increase the father’s height, the predicted son’s height grows by 0.5 inches.   <-*
# For every inch we increase the father’s height, the predicted son’s height is 0.5 times greater.


# Question 2
# We want the intercept term for our model to be more interpretable, so we run the same model as before but now we subtract the mean of fathers’ heights from each individual father’s height to create a new variable centered at zero.
#
# galton_heights <- galton_heights %>%
#     mutate(father_centered=father - mean(father))
# We run a linear model using this centered fathers’ height variable.
#
# > lm(son ~ father_centered, data = galton_heights)
#
# Call:
#     lm(formula = son ~ father_centered, data = galton_heights)
plot( lm(formula = son ~ father_centered, data = galton_heights) )
#
# Coefficients:
#     (Intercept)    father_centered
# 70.45          0.50
# Interpret the numeric coefficient for the intercept.
# The height of a son of a father of average height is 70.45 inches.   <-*
# The height of a son when a father’s height is zero is 70.45 inches.
# The height of an average father is 70.45 inches.

 # -------       -------       -------       -------       -------       -------

# Least Squares Estimates
# LEast Squares Estimates( LSE )
# For linear models to be useful, we have to estimate the unknown parameters,
#    the betas.  The standard approach in science is to find the values that
#    minimize the distance of the fitted model to the data.  To quantify, this
#    we use the least squares equation.
# This quantity is called the Residual Sum of Squares, RSS.

# RSS = sum{ Yi - ( Beta0 - Beta1 xi ) }^2

# Once we find the values that minimize the RSS,
# we call the values the Least Squares Estimate, LSE, and denote them, in this
#    case, with beta 0 hat and beta 1 hat.  Let's write the function that computes
#    the RSS for any pair of values, beta 0 and beta 1, for our heights data.
#    It would look like this.

rss <- function( beta0, beta1, data ) {
    resid <- galton_heights$son - ( beta0 + beta1 * galton_heights$father )
    return( sum( resid^2 ) )
}

# So for any pair of values, we get an RSS.  So this is a three-dimensional plot
#    with beta 1 and beta 2, and x and y and the RSS as a z.  To find the minimum,
#    you would have to look at this three-dimensional plot.  Here, we're just
#    going to make a two-dimensional version by keeping beta 0 fixed at 25.
# So it will be a function of the RSS as a function of beta 1.  We can use this
#    code to produce this plot.

beta1 = seq( 0, 1, len = nrow( galton_heights ) )
results <- data.frame( beta1 = beta1,
                       rss = sapply( beta1, rss, beta0 = 36 ) )
head( results, 1 )

rss_min <- min( results$rss )
subset( results, rss >= rss_min-1 & rss < rss_min+1 )


results %>%
    ggplot( aes( beta1, rss ) ) +
    geom_line( aes( beta1, rss ), col = 2 )

# We can see a clear minimum for beta 1 at around 0.65.  So you could see how we
#    would pick the least squares estimates.  However, this minimum is for
#    beta 1 when beta 0 is fixed at 25.
# But we don't know if that's the minimum for beta 0.  We don't know if ( 25, 0.65 )
#    minimizes the equation across all pairs.  We could use trial and error,
#    but it's really not going to work here.  Instead we will use calculus.
# We'll take the partial derivatives, set them equal to 0, and solve for beta 1
#    and beta 0.  Of course, if we have many parameters, these equations can get
#    rather complex.  But there are functions in R that do these calculations for
#    us.  We will learn these soon.  To learn the mathematics behind this, you
#    can consult the book on linear models.

# -------       -------       -------       -------       -------       -------

# Question 1
# The following code was used in the video to plot RSS with .
#
# beta1 = seq(0, 1, len=nrow(galton_heights))
# results <- data.frame(beta1 = beta1,
#                       rss = sapply(beta1, rss, beta0 = 25))
# results %>% ggplot(aes(beta1, rss)) + geom_line() +
#     geom_line(aes(beta1, rss), col=2)
# In a model for sons’ heights vs fathers’ heights, what is the least squares estimate (LSE) for  if we assume  is 36?
# Hint: modify the code above to do your analysis.
#
# 0.65
# 0.5   <-*
# 0.2
# 12


# Question 2
# The least squares estimates for the parameters Beta0, Beta1, ..., Betan,
#    minimize the residual sum of squares.

# -------       -------       -------       -------       -------       -------

# The lm function
# In r, we can obtain the least squares estimates using the lm function.  To fit
#    the following model where Yi is the son's height and Xi is the father height,
#    we would write the following piece of code.

# Yi = Beta0 + Beta1 xi + Epsiloni
# xi: is the father's height

fit <- lm( son ~ father, data = galton_heights )
fit

# This gives us the least squares estimates, which we can see in the output of r.

# The general way we use lm is by using the tilde character to let lm know which
#    is:
#        the value we're predicting that's on the left side of the tilde,
#        and which variables we're using to predict-- those will be on the right
#        side of the tilde.

# The intercept is added automatically to the model.  So you don't have to include
#    it when you write it.  The object fit that we just computed includes more
#    information about the least squares fit.  We can use the function summary
#    to extract more of this information, like this.

summary( fit )

# To understand some of the information included in this summary, we need to remember
#    that the LSE are random variables.  Mathematical statistics gives us some ideas
#    of the distribution of these random variables. And we'll learn some of that next.

# -------       -------       -------       -------       -------       -------

# Question 1
# Run a linear model in R predicting the number of runs per game based on the number of bases on balls and the number of home runs. Remember to first limit your data to 1961-2001.
#
# What is the coefficient for bases on balls?
# 0.39   <-*
# 1.56
# 1.74
# 0.027

# The coefficient for bases on balls is 0.39;
# The coefficient for home runs is 1.56;
# The intercept is 1.74;
# The standard error for the BB coefficient is 0.027.


dat <- Teams %>%
    filter( yearID %in% 1961:2001 ) %>%
    mutate( HR_strata = round( HR / G, 1),
            BB_per_game = BB / G,
            R_per_game = R / G )
head( dat )
R_per_game <- dat$R_per_game
BB_per_game <- dat$BB_per_game
HR_strata <- dat$HR_strata
BB_fit <- lm( R_per_game ~ BB_per_game + HR_strata, data = dat )
BB_fit

# -------       -------       -------       -------       -------       -------

# LSE are Random Variables
# The LSE are derived from the data, Y1 through Yn, which are random.
# This implies that our estimates are random variables.  To see this, we can run
#    a Monte Carlo simulation in which we assume that the son and father height
#    data that we have defines an entire population.  And we're going to take
#    random samples of size 50 and compute the regression slope coefficient for
#    each one.  We write this code, which gives us several estimates of the regression
#    slope.

B <- 1000
N <- 50
lse <- replicate( B , {
    sample_n( galton_heights, N, replace = TRUE ) %>%
        lm( son ~ father, data = . ) %>%
        .$coef
} )
head( lse, 1 )
lse <- data.frame( beta_0 = lse[ 1, ],
                   beta_1 = lse[ 2, ] )
head( lse )

# We can see the variability of the estimates by plotting their distribution.

library( gridExtra )
p1 <- lse %>%
    ggplot( aes( beta_0 ) ) +
    geom_histogram( binwidth = 5, color =  'black' )
p2 <- lse %>%
    ggplot( aes( beta_1 ) ) +
    geom_histogram( binwidth = 0.1, color = 'black' )
grid.arrange( p1, p2, ncol = 2 )

# Here you can see the histograms of the estimated beta0's and the estimated
#    beta1's.  The reason these look normal is because the central limit theorem
#    applies here as well.
#             For large enough N, the least squares estimates
#             will be approximately normal with expected value
#             Beta0 and Beta1 respectively.
# The standard errors are a bit complicated to compute, but mathematical theory
#    does allow us to compute them, and they are included in the summary provided
#    by the lm function.

sample_n( galton_heights, N, replace = TRUE ) %>%
    lm( son ~ father, data = . ) %>%
    summary

# Here are the estimated standard errors for one of our simulated data sets.
# You could see them at the second column in the coefficients table.
lse %>%
    summarize( se_0 = sd( beta_0 ),
               se_1 = sd( beta_1 ) )

# You can see that the standard errors estimates reported by the summary function
#    are closed, so the standard errors that we obtain from our Monte Carlo simulation.

# The summary function also reports t-statistics-- this is the t value column--
#    and p-values.  This is the Pr bigger than absolute value of t column.
# The t-statistic is not actually based on the central limit theorem, but rather
#    on the assumption that the epsilons follow a normal distribution.  Under
#    this assumption, mathematical theory tells us that the LSE divided by their
#    standard error, which we can see here and here, follow a t distribution with
#    N minus p degrees of freedom, with p the number of parameters in our model,
#    which in this case is 2.  The 2p values are testing the null hypothesis that
#    beta 0 is 0 and beta 1 is 0 respectively.
# Note that as we described previously, for large enough N, the central limit
#    works, and the t distribution becomes almost the same as a normal distribution.
# So if either you assume the errors are normal and use the t distribution or if
#    you assume that N is large enough to use the central limit theorem, you can
#    construct confidence intervals for your parameters.  We know here that although
#    we will not show examples in this video, hypothesis testing for regression
#    models is very commonly used in, for example, epidemiology and economics, to
#    make statements such as the effect of A and B was statistically significant
#    after adjusting for X, Y, and Z.  But it's very important to note that several
#    assumptions-- we just described some of them-- have to hold for these statements
#    to hold.

# -------       -------       -------       -------       -------       -------

# Question 1
# We run a Monte Carlo simulation where we repeatedly take samples of N = 100 from the Galton heights data and compute the regression slope coefficients for each sample:

B <- 1000
N <- 100
lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
        lm(son ~ father, data = .) %>% .$coef
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
lse

# What does the central limit theorem tell us about the variables beta_0 and beta_1?
#     Select ALL that apply.
#
# They are approximately normally distributed.   <-*
# The expected value of each is the true value of Beta0 and Beta1  (assuming the Galton heights data is a complete population).   <-*
# The central limit theorem does not apply in this situation.
# It allows us to test the hypothesis that Beta0 = 0 and Beta1 = 0.


# Question 2
# In an earlier video, we ran the following linear model and looked at a summary of the results.
#
mod <- lm(son ~ father, data = galton_heights)
summary(mod)
#
# Call:
#     lm(formula = son ~ father, data = galton_heights)
#
# Residuals:
#     Min     1Q  Median     3Q    Max
# -5.902  -1.405  0.092    1.342  8.092
#
# Coefficients:
#     Estimate  Std. Error    t value     Pr(>|t|)
# (Intercept)     35.7125     4.5174       7.91    2.8e-13 ***
#     father       0.5028     0.0653       7.70    9.5e-13 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# What null hypothesis is the second p-value (the one in the father row) testing?
# Beta1 = 1, where Beta1 is the coefficient for the variable "father."
# Beta1 = 0.503, where Beta1  is the coefficient for the variable "father."
# Beta1 = 0, where Beta1 is the coefficient for the variable "father."   <-*

# -------       -------       -------       -------       -------       -------

# Advanced Note on LSE
# Although interpretation is not straight-forward, it is also useful to know that the LSE can be strongly correlated, which can be seen using this code:

lse %>% summarize(cor(beta_0, beta_1))
#
# However, the correlation depends on how the predictors are defined or transformed.
#
# Here we standardize the father heights, which changes  to .
#
B <- 1000
N <- 50
lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
        mutate(father = father - mean(father)) %>%
        lm(son ~ father, data = .) %>% .$coef
    })

# Observe what happens to the correlation in this case:

cor(lse[1,], lse[2,])

# -------       -------       -------       -------       -------       -------

# Predicted Variables are Random Variables
# Once we fit our model, we can obtain predictions of y by plugging the estimates
#    into the regression model.  For example, if the father's height is x, then
#    our prediction for y-- which we'll denote with a hat on top of the y-- for
#    the son's height will be the following.  We're just plugging in beta-- the
#    estimated betas into the equation.  If we plot y hat versus x, we'll see the
#    regression line.
# Note that the prediction y hat is also a random variable, and mathematical theory
#    tells us what the standard errors are.  If we assume the errors are normal
#    or have a large enough sample size to use the Central Limit Theorem, we can
#    construct confidence intervals for our predictions, as well.
# In fact, the ggplot layer geom underscore smooth, when we set method equals to
#    lm-- we've previously shown this for several plots-- plots confidence intervals
#    around the predicted y hat.  Let's look at an example with this code.

galton_heights %>%
    ggplot( aes( son, father ) ) +
    geom_point() +
    geom_smooth( method = 'lm' )

# You can see the regression line.
# Those are the predictions (the line that appears on the graph)
# , and you see a band around them.  Those are the confidence intervals.
# The R function predict takes an lm object as input and returns these predictions.
# We can see it here in this code which produces this plot

galton_heights %>%
    mutate( Y_hat = predict( lm( son ~ father, data = . ) ) ) %>%
    ggplot( aes( father, Y_hat ) ) +
    geom_line()

# and if requested the standard errors and other information from which we can
#    construct confidence intervals can be obtained from the predict function.
# You can see it by running this code.

fit <- galton_heights %>%
    lm( son ~ father, data = . )

Y_hat <- predict( fit, se.fit = TRUE )

names( Y_hat )

# -------       -------       -------       -------       -------       -------

# Question 1
# Which R code(s) below would properly plot the predictions and confidence intervals for our linear model of sons’ heights?
#     Select ALL that apply.
#
# galton_heights %>% ggplot(aes(father, son)) +
#     geom_point() +
#     geom_smooth()
#
# galton_heights %>% ggplot(aes(father, son)) +     <-*
#     geom_point() +
#     geom_smooth(method = "lm")
#
# model <- lm(son ~ father, data = galton_heights)      <-*
# predictions <- predict(model, interval = c("confidence"), level = 0.95)
# data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
# ggplot(data, aes(x = father, y = fit)) +
#     geom_line(color = "blue", size = 1) +
#     geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
#     geom_point(data = galton_heights, aes(x = father, y = son))
#
# model <- lm(son ~ father, data = galton_heights)
# predictions <- predict(model)
# data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
# ggplot(data, aes(x = father, y = fit)) +
#     geom_line(color = "blue", size = 1) +
#     geom_point(data = galton_heights, aes(x = father, y = son))


# If using the geom_smooth command, you need to specify that method = “lm” in your geom_smooth command, otherwise the smooth line is a loess smooth and not a linear model.

# If using the predict command, you need to include the confidence intervals on your figure by first specifying that you want confidence intervals in the predict command, and then adding them to your figure as a geom_ribbon.

# -------       -------       -------       -------       -------       -------

# Tibbles, do, and broom
# Advanced dplyr: Tibbles
# Let's go back to baseball.  In a previous example, we estimated the regression
#    linesto predict runs from bases and balls in different home run strata.  We
#    first constructed a data frame similar to this.

dat <- Teams %>%
    filter( yearID %in% 1961:2001 ) %>%
    mutate( HR = round( HR / G, 1 ),
            BB = BB / G,
            R = R / G ) %>%
    select( HR, BB, R ) %>%
    filter( HR >= 0.4 & HR <= 1.2 )
head( dat )

# Then, to compute the regression line in each strata, since we didn't know the
#    lm function back then,we used the formula directly like this.
 dat %>%
     group_by( HR ) %>%
     summarize( slope = cor( BB, R ) * sd( R ) / sd( BB ) )

# We argued that the slopes are similar and that the differences were perhaps
#    due to random variation.  To provide a more rigorous defense of the slopes
#    being the same, which is what led to our multivariate regression model,
#    we could compute confidence intervals for each slope.
# We have not learned the formula for this, but the lm function provides enough
#    information to construct them.  First, note that if we try to use the lm
#    function to get the estimated slope like this, we don't get what we want.

dat %>%
    group_by( HR ) %>%
    lm( R ~ BB, data = . ) %>%
    .$coef

# The lm function ignored the group_by.  This is expected, because lm is not part
#    of the tidyverse and does not know how to handle the outcome of group_by which
#    is a group tibble.  We're going to describe tibbles in some details now.
# When summarize receives the output of group_by, it somehow knows which rows of
#    the table go with which groups.  But where is this information stored in the
#    data frame? Let's write some code to see the output of a group_by call.

dat %>%
    group_by( HR ) %>%
    head()

# Note that there are no columns with the information needed to define the groups.
# But if you look closely at the output, you notice the line "A tibble, 6 by 3."
# We can learn the class of the return object using this line of code, and we see
#    that the class is a "tbl."

dat %>%
    group_by( HR ) %>%
    class()

# This is pronounced "tibble."  It is also a tbl_df.  This is equivalent to tibble.
# The tibble is a special kind of data frame.  We have seen them before, because
#    tidyverse functions such as group_by and also summarize always return this
#    type of data frame.  The group_by function returns a special kind of tibble,
#    the grouped tibble.  We will say more about the grouped tibbles later.
# Note that the manipulation verbs, select, filter, mutate, and arrange, don't
#    necessarily return tibbles.  They preserve the class of the input.  If they
#    receive a regular data frame, they return a regular data frame.  If they
#    receive a tibble, they return a tibble.
# But tibbles are the default data frame for the tidyverse.  Tibbles are very
#    similar to data frames.  You can think of them as modern versions of data
#    frames.  Next, we're going to describe briefly three important differences.

# -------       -------       -------       -------       -------       -------

# Question 1
# What problem do we encounter when we try to run a linear model on our baseball data, grouping by home runs?
# There is not enough data in some levels to run the model.
# The lm function does not know how to handle grouped tibbles.   <-*
# The results of the lm function cannot be put into a tidy format.


# Question 2
# Tibbles are similar to what other class in R?
# Vectors
# Matrices
# Data frames    <-*
# Lists

# -------       -------       -------       -------       -------       -------

# Tibbles: Differences from Data Frames
#  1. The print method for tibble is much more readable than that of a data frame.
#     To see this, type teams on your console after loading the Baseball Lahman
#     Database.

Teams

#     And you will see a very, very long list of columns and rows. It's barely
#     readable.  Teams is a data frame with many rows and columns.  That's why
#     you see that.  Nevertheless, the output just shows everything wraps around
#     and is hard to read.  It is so bad that we don't even print it here.  We'll
#     let you print it on your own screen.  Now if you convert this data frame to
#     a tibble data frame, the output is much more readable.  Here's an example.

as_tibble( Teams )

# 2. Is that if you subset the columns of a data frame, you may get back an object
#    that is not a data frame. Here's an example.

class( Teams[ , 20 ] )

#    If we subset the 20th column, we get back an integer.  That's not a data frame.
#    With tibbles, this is not the case. Here's an example.

class( as_tibble( Teams )[ ,20 ] )

#    If we subset a table, we get back a table.  This is useful in the tidyverse
#    since functions require data frames as input.  Now whenever you want to access
#    the original vector that defines a column in a table, for this, you actually
#    have to use the accessor dollar sign.  Here's an example.

class( as_tibble( Teams )$HR )

# 3. A related feature to this is that tibbles will give you a warning if you try
#    to access a column that does not exist.  That's not the case for data frames.
#    For example, if we accidentally write hr in lowercase instead of uppercase,
#    with a data frame, all we get is a null.  No warning.

Teams$hr

#    This can make it quite hard to debug code. In contrast, if it's a tibble,
#    and you try to access the lowercase hr column, which doesn't exist, you
#    actually get a warning.

as_tibble( Teams )$hr

# 4. A third difference is that while columns of a data frame need to be a vector
#    of number strings or Boolean, tibbles can have more complex objects, such as
#    lists or functions.  Also note that we can create tibbles with the tibble
#    function.  So, look at this line of code.

tibble( id = c( 1, 2, 3 ),
        func = c( mean, median, sd ) )

# 5. Tibbles can be grouped.
#    The function group by returns a special kind of tibble, a grouped tibble.
#    This class stores information that lets you know which rows are in which
#    groups.  The tidyverse functions, in particular the summarize functions,
#    are aware of the group information. In the example we showed, we saw that
#    the ln function, which is not part of the tidyverse, does not know how to
#    deal with group tibbles.  The object is basically converted to a regular
#    data frame, and then the function runs ignoring the groups.  This is why we
#    only get one pair of estimates, as we see here.  To make these non-tidyverse
#    function better integrate with a tidyverse, we will learn a new function,
#    the function do.

dat %>%
    group_by( HR ) %>%
    lm( R ~ BB, data = . )

# -------       -------       -------       -------       -------       -------

# Question 1
# What are some advantages of tibbles compared to data frames?
# Select ALL that apply.
#
# Tibbles display better.   <-*
# If you subset a tibble, you always get back a tibble.   <-*
# Tibbles can have complex entries.   <-*
# Tibbles can be grouped.   <-*

# -------       -------       -------       -------       -------       -------

# do
#  In this video, we'll describe the very useful do( ) function.  The tidyverse
#     functions know how to interpret group tibbles.  Furthermore, to facilitate
#     stringing commands through the pipe, tidyverse function consistently return
#     data frames.  Since this assures that the output of 1 is accepted as the
#     input of another.  But most our functions do not recognize group tibbles,
#     nor do they return data frames.  The lm( ) function is an example.
# The do( ) function:
#     - Serves as a bridge between our functions, such as lm( ) and the tidyverse.
#     - Understands group tibbles and always returns a data frame.
# So let's try to use the do( ) function to fit a regression line to each home
#    run strata.  We would do it like this.

dat %>%
    group_by( HR ) %>%
    do( fit = lm( R ~ BB, data = . ) )

# Notice that we did in fact fit a regression line to each strata.  But the do( )
#    function would create a data frame with the first column being the strata value.
#    And a column named fit.  We chose that name. It could be any name.  And that
#    column will contain the result of the lm( ) call.  Therefore, the return table
#    has a column with lm( ) objects in the cells, which is not very useful.
# Also note that if we do not name a column, then do( ) will return the actual
#    output of lm( ), not a data frame.  And this will result in an error since
#    do( ) is expecting a data frame as output.  If you write this, you will get
#    the following error.

dat %>%
    group_by( HR ) %>%
    do( lm( R ~ BB, data = . ) )

# For a useful data frame to be constructed, the output of the function, inside
#    do( ), must be a data frame as well.  We could build a function that returns
#    only what you want in the form of a data frame.  We could write for example,
#    this function.

get_slope <- function( data ) {
    fit <- lm( R ~ BB, data = data )
    data.frame( slope = fit$coefficients[ 2 ],
                se = summary( fit )$coefficient[ 2, 2 ] )
}

# And then we use to do( ) without naming the output, since we are already getting
#    a data frame.  We can write this very simple piece of code and now we get the
#    expected result. We get the slope for each strata and the standard error for
#    that slope.

dat %>%
    group_by( HR ) %>%
    do( get_slope( . ) )

# If we name the output, then we get a column containing the data frame.  So if
#    we write this piece of code, now once again, we get one of these complex
#    tibbles with a column having a data frame in each cell.  Which is again, not
#    very useful.

dat %>%
    group_by( HR ) %>%
    do( slope = get_slope( . ) )

# Now we're going to cover one last feature of do( ).  If the data frame being
#    returned has more than one row, these will be concatenated appropriately.
#    Here's an example in which return both estimated parameters.
 get_lse <- function( data ) {
     fit <- lm( R ~ BB, data = data )
     data.frame( term = names( fit$coefficients ),
                 slope = fit$coefficients,
                 se = summary( fit )$coefficient[ ,2 ] )
 }
dat %>%
    group_by( HR ) %>%
    do( get_lse( . ) )

# The slope and intercept.  We write this piece of code.  And now we use the do( )
#    function as we used it before, and get a very useful tibble, giving us the
#    estimates of the slope and intercept, as well as the standard errors.
# Now, if you think this is all a bit too complicated, you're not alone. To simplify
#    things, we're going to introduce the broom package, which was designed to
#    facilitate the use of model fitting functions such as lm( ) with the tidyverse.

# -------       -------       -------       -------       -------       -------

# Question 1
# What are two advantages of the do command, when applied to the tidyverse?
# Select ALL that apply.
#
# It is faster than normal functions.
# It returns useful error messages.
# It understands grouped tibbles.   <-*
# It always returns a data.frame.   <-*



# Question 2
# You want to take the tibble dat, which we’ve been using in this video, and run the linear model R ~ BB for each strata of HR. Then you want to add three new columns to your grouped tibble: the coefficient, standard error, and p-value for the BB term in the model.
#
# You’ve already written the function get_slope, shown below.
#
get_slope <- function(data) {
    fit <- lm(R ~ BB, data = data)
    sum.fit <- summary(fit)

    data.frame(slope = sum.fit$coefficients[2, "Estimate"],
               se = sum.fit$coefficients[2, "Std. Error"],
               pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}
# What additional code could you write to accomplish your goal?
#
# dat %>%
#     group_by(HR) %>%
#     do(get_slope)
#
# dat %>%    <-*
#     group_by(HR) %>%
#     do(get_slope(.))
#
# dat %>%
#     group_by(HR) %>%
#     do(slope = get_slope(.))
#
# dat %>%
#     do(get_slope(.))

# -------       -------       -------       -------       -------       -------

# broom
#  The original task we ask for in a previous video was to provide an estimate
#    and a confidence interval for the slope estimates of each strata.  The broom
#    packs will make this quite easy.  Broom has three main functions all of which
#    extract information from the object returned by the function LM, and return
#    it in a tidy verse friendly data frame.  These functions are tidy, glance
#    and augment.
# The tidy function returns estimates and related information as a data frame.
#    Here's an example.

library( broom )
fit <- lm( R ~ BB, data =dat )
tidy( fit )

# We can add other important summaries, such as confidence intervals, using
#    arguments like this.

tidy( fit, conf.int = TRUE )

# Because the outcome is a data frame, we can immediately use it with do to string
#    together the commands that produce the table we are after.

dat %>%
    group_by( HR ) %>%
    do( tidy( lm( R ~ BB, data = . ),
              conf.int = TRUE ) )

# So this piece of code will generate what we wanted to see.  Because a data frame
#    is returned, we can filter and select the rows and columns we want.  So this
#    simple piece of code gives us exactly the table we asked for.

dat %>%
    group_by( HR ) %>%
    do( tidy( lm( R ~ BB, data = . ),
              conf.int = TRUE ) ) %>%
    filter( term == 'BB' ) %>%
    select( HR, estimate, conf.low, conf.high )

# We have filtered away the intercept rows, and only show the columns we care about,
#    the estimate and the confidence intervals.  Furthermore, a table like this
#    makes visualization with GE plot quite easy.  So this piece of code produces
#    this nice plot, which provides very useful information.

dat %>%
    group_by( HR ) %>%
    do( tidy( lm( R ~ BB, data =  . ),
              conf.int = TRUE ) ) %>%
    filter( term == 'BB' ) %>%
    select( HR, estimate, conf.low, conf.high ) %>%
    ggplot( aes( HR, y = estimate, ymin = conf.low, ymax = conf.high ) ) +
    geom_errorbar() +
    geom_point()

# Now we return to discussing our original task of determining if slopes change.
#    The plot we just made using do and broom shows that the confidence intervals
#    overlap, which provides a nice visual confirmation that our assumption that
#    the slopes do not change with home run strata, is relatively safe.
# Earlier we mentioned two other functions from the broom package, glance and
#    augment Glance and augment relate to model specific and observation specific
#    outcomes, respectively.  Here we can see the model fit summary the glance
#    returns.  You can learn more about these summaries in any regression textbook.

glance( fit )

# -------       -------       -------       -------       -------       -------

# Question 1
# The output of a broom function is always what?
# A data.frame   <-*
# A list
# A vector



# Question 2
# You want to know whether the relationship between home runs and runs per game varies by baseball league. You create the following dataset:
#
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR = HR/G,
           R = R/G) %>%
    select(lgID, HR, BB, R)
# What code would help you quickly answer this question?
#
# dat %>%    <-*
#    group_by(lgID) %>%
#    do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>%
#    filter(term == "HR")

# dat %>%
#    group_by(lgID) %>%
#    do(glance(lm(R ~ HR, data = .)))

# dat %>%
#    do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>%
#    filter(term == "HR")

# dat %>%
#    group_by(lgID) %>%
#    do(mod = lm(R ~ HR, data = .))

# -------       -------       -------       -------       -------       -------

# Regression and Baseball
# Building a Better Offensive Metric for Baseball
# In trying to answer how well bases on balls predict runs, data exploration let us to
#    this model.

# E[ R | BB = x1, HR = x2 ] = Beta0 + Beta1 x1 + Beta2 x2

# Here, the data is approximately normal.  And conditional distributions were also
#    normal.  Thus, we're justified to pose a linear model like this.

# Yi = Beta0 + Beta1 x1 + Beta2 x2 + Epsiloni

# Yi : runs per game
# x1 : BB per game (walks per game)
# x2 : HR per game (home runs per game)

# To use lm here, we need to let it know that we have two predictive variables.
# So we use the plus symbol as follows. Here's a code that fits that multiple
#    regression model.

fit <- Teams %>%
    filter( yearID %in% 1961:2001 ) %>%
    mutate( BB = BB / G, HR = HR / G, R = R / G ) %>%
    lm( R ~ BB + HR, data = . )

# Now, we can use the tidy function to see the nice summary.

tidy( fit, conf.int = TRUE )

# When we fit the model with only one variable without the adjustment, the estimated
#    slopes were 0.735 and 1.844 for bases on ball and home runs, respectively.
# But note that when we fit the multivariate model, both these slopes go down with
#    the bases on ball effect decreasing much more.  Now, if we want to construct
#    a metric to pick players, we need to consider single, doubles, and triples
#    as well.  Can we build a model that predicts runs based on all these outcomes?
#    Now, we're going to take somewhat of a leap of faith and assume that these
#    five variables are jointly normal.
# This means that if we pick any one of them and hold the other for fixed, the
#    relationship with the outcome-- in this case, runs per game-- is linear.
#    And the slopes for this relationship do not depend on the other four values
#    that were held constant.  If this is true, if this model holds true, then a
#    linear model for our data is the following.

# Yi = Beta0 + Beta1 xi,1 + Beta2 xi,2 + Beta3 xi,3 + Beta4 xi,4 + Beta5 xi,5 + Epsiloni

# xi,1 : bases on balls per game
# xi,2 : singles per game
# xi,3 : doubles per game
# xi,4 : triples per game
# xi,5 : home runs per game

# Using lm, we can quickly find the least square errors for the parameters using
#    this relatively simple piece of code.

fit <- Teams %>%
    filter( yearID %in% 1961:2001 ) %>%
    mutate( BB = BB / G,
            singles = ( H - X2B - X3B - HR ) / G,
            doubles = X2B / G,
            triples = X3B / G,
            HR = HR / G,
            R = R / G ) %>%
    lm( R ~ BB + singles + doubles + triples + HR, data = . )
fit

# We can again use the tidy function to see the coefficients, the standard errors,
#    and confidence intervals.

coefs <- tidy( fit, conf.int = TRUE )
coefs

# To see how well our metric actually predicts runs, we can predict the number
#    of runs for each team in 2002 using the function predict to make the plot.
# Note that we did not use the 2002 year to create this metric.  We used data from
#    years previous to 2002.  And here is the plot.

Teams %>%
    filter( yearID %in% 2002 ) %>%
    mutate( BB = BB / G,
            singles = ( H - X2B - X3B - HR ) / G,
            doubles = X2B / G,
            triples = X3B / G,
            HR = HR / G,
            R = R / G ) %>%
    mutate( R_hat = predict( fit, newdata = . ) ) %>%
    ggplot( aes( R_hat, R ), label = teamID ) +
    geom_point( ) +
    geom_text( aes( label = teamID ), hjust = 0, vjust = 0 ) +
    geom_smooth( method = 'lm', se = FALSE )

# Our model does quite a good job, as demonstrated by the fact that points from
#    the observed versus predicted plot fall close to the identity line.
# So instead of using batting average or just the number of home runs as a measure
#    for picking players, we can use our fitted model to form a more informative
#    metric that relates more directly to run production.
# Specifically, to define a metric for player A, we imagine a team made up of
#    players just like player A and use our fitted a regression model to predict
#    how many runs this team would produce.  The formula would look like this.

# -2.769 + 0.371 x BB + 0.519 x singles + 0.771 x doubles + 1.240 x triples + 1.443 x HR

# We're basically sticking in the estimated coefficients into the regression formula.
# However, to define a player's specific metric, we have a bit more work to do.
# Our challenge here is that we have derived the metrics for teams based on
#    team-level summary statistics.
# For example, the home run value that is entered into the equation is home runs
#    per game for the entire team.  If you compute the home runs per game for a
#    player, it will be much lower.  As the total is accumulated by nine batters,
#    not just one.  Furthermore, if a player only plays part of the game and gets
#    less opportunity than average, it's still considered a game play.
# So this means that their rates will be lower than they should be.  For players,
#    a rate that takes into account opportunities is a per-plate-appearance rate.
#    To make the per-game team rate comparable to the per-plate-appearance player
#    rate, we compute the average number of team plate appearances per game using
#    this simple piece of code.

pa_per_game <- Batting %>%
    filter( yearID == 2002 ) %>%
    group_by( teamID ) %>%
    summarize( pa_per_game = sum( AB +BB ) / max( G ) ) %>%
    .$pa_per_game %>%
    mean
pa_per_game

# Now, we're ready to use our metric.  We're going to compute the per-plate-appearance
#    rates for players available in 2002.  But we're going to use data from 1999 2001.
# Because remember, we are picking players in 2002.  We don't know what has happened
#    yet.   To avoid small sample artifacts, we're going to filter players with
#    few plate interferences.  Here is the calculation of what we want to do in
#    one long line of code using tidyverse.

players <- Batting %>%
    filter( yearID %in% 1999:2001 ) %>%
    group_by( playerID ) %>%
    mutate( PA = BB + AB ) %>%
    summarize( G = sum( PA ) / pa_per_game,
               BB = sum( BB ) / G,
               singles = sum( H - X2B - X3B - HR ) / G,
               doubles = sum( X2B ) / G,
               triples = sum( X3B ) / G,
               HR = sum( HR ) / G,
               AVG = sum( H ) / sum( AB ),
               PA = sum( PA ) ) %>%
    filter( PA >= 300 ) %>%
    select( -G ) %>%
    mutate( R_hat = predict( fit, newdata = . ) )
players

# So we fit our model.  And we have player-specific metrics.  The player-specific
#    predicted runs computer here can be interpreted as a number of runs we
#    would predict a team to score if this team was made up of just that player,
#    if that player batted every single time.  The distribution shows that there's
#    y variability across players, as we can see here.

players %>%
    ggplot( aes( R_hat ) ) +
    geom_histogram( binwidth = 0.5, color =  'black' )

# To actually build the teams, we will need to know the players' salaries, since
#    we have a limited budget.  Remember, we are pretending to be the Oakland A's
#    in 2002 with only a $40 million budget.  We also need to know the players'
#    position.  Because we're going to need one shortstop, one second baseman,
#    one third baseman, et cetera.  For this, we're going to have to do a little
#    bit of data wrangling to combine information that is contained in different
#    tables from the lm library.
# OK, so here we go.  We start by adding the 2002 salaries for each player using
#    this code.

players <- Salaries %>%
    filter( yearID == 2002 ) %>%
    select( playerID, salary ) %>%
    right_join( players, by = 'playerID' )
players

# Next, we're going to add the defensive position.  This is a little bit complicated,
#    because players play more than one position each year.  So here, we're going
#    to pick the one position most played by each player using the top_n function.
# And to make sure that we only pick one position in the case of ties, we're going
#    to take the first row if there is a tie.   We also remove the OF position.
#    Because this stands for outfielder, which is a generalization of three
#    positions-- left field, center field, right field.  We also remove pitchers,
#    as they don't bat in the league that the Athletics play.
# Here is the code that does that.

players <- Fielding %>%
    filter( yearID == 2002 ) %>%
    filter( !POS %in% c( 'OF', 'P' ) ) %>%
    group_by( playerID ) %>%
    top_n( 1, G ) %>%
    filter( row_number( G ) == 1 ) %>%
    ungroup() %>%
    select( playerID, POS ) %>%
    right_join( players, by = 'playerID' ) %>%
    filter( !is.na( POS ) & !is.na( salary ) )
players

# Finally, we add their names and last names so we know who we're talking about.
#   And here's a code that does that.

players <- Master %>%
    select( playerID, nameFirst, nameLast, debut ) %>%
    right_join( players, by = 'playerID' )
players

# So now, we have a table with our predicted run statistic, some other statistic,
#    the player's name, their position, and their salary.

players %>%
    select( nameFirst, nameLast, POS, salary, R_hat ) %>%
    arrange( desc( R_hat ) ) %>%
    top_n( 10 )

# If we look at the top 10 players based on our run production statistic, you're
#    going to recognize some names if you're a baseball fan.  Note the very high
#    salaries of these players in the top 10.  In fact, we see that players with
#    high metrics have high salaries.  We can see that by making a plot we do see
#    some low-cost players with very high metrics.

players %>%
    ggplot( aes( salary, R_hat, color = POS ) ) +
    geom_point() +
    scale_x_log10()

# These would be great for our team.  Unfortunately, these are likely young players
#    that have not yet been able to negotiate a salary and are not going to be
#    available in 2002.  For example, the lowest earner on our top 10 list is
#    Albert Pujols, who was a rookie in 2001. Here's a plot with players that
#    debuted before 1997.

players %>%
    filter( debut < 1998 ) %>%
    ggplot( aes( salary, R_hat, color = POS ) ) +
    geom_point() +
    scale_x_log10()

# This removes all the young players.  We can now search for good deals by looking
#    at players that produce many more runs and others with similar salaries.
#    We can use this table to decide what players to pick and keep our total
#    salary below the $40 million Billy Beane had to work with.

# -------       -------       -------       -------       -------       -------

# Question 1
# What is the final linear model we use to predict runs scored per game?
# lm(R ~ BB + HR)
# lm(HR ~ BB + singles + doubles + triples)
# lm(R ~ BB + singles + doubles + triples + HR)    <-*
# lm(R ~ singles + doubles + triples + HR)



# Question 2
# We want to estimate runs per game scored by individual players, not just by teams. What summary metric do we calculate to help estimate this?
#
#     Look at the code from the video for a hint:
#
pa_per_game <- Batting %>%
    filter(yearID == 2002) %>%
    group_by(teamID) %>%
    summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
    .$pa_per_game %>%
    mean

# pa_per_game: the mean number of plate appearances per team per game for each team
# pa_per_game: the mean number of plate appearances per game for each player
# pa_per_game: the number of plate appearances per team per game, averaged across all teams   <-*



# Question 3
# Imagine you have two teams. Team A is comprised of batters who, on average, get two bases on balls, four singles, one double, and one home run. Team B is comprised of batters who, on average, get one base on balls, six singles, two doubles, and one triple.
#
# Which team scores more runs, as predicted by our model?
# Team A
# Team     <-*
# Tie
# Impossible to know

# -------       -------       -------       -------       -------       -------

# Building a Better Offensive Metric for Baseball: Linear Programming

# A way to actually pick the players for the team can be done using what computer scientists call linear programming. Although we don't go into this topic in detail in this course, we include the code anyway:

library( reshape2 )
library( lpSolve )
library( magrittr )

players <- players %>% filter(debut <= 1997 & debut > 1988)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE)

# This algorithm chooses these 9 players:

our_team <- players %>%
    filter(lp_solution$solution == 1) %>%
    arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

# nameFirst    nameLast POS   salary R_hat
# 1     Jason      Giambi  1B 10428571  7.99
# 2     Nomar Garciaparra  SS  9000000  7.51
# 3      Mike      Piazza   C 10571429  7.16
# 4      Phil       Nevin  3B  2600000  6.75
# 5      Jeff        Kent  2B  6000000  6.68
#
# We note that these players all have above average BB and HR rates while the same is not true for singles.

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB),
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
    filter(playerID %in% our_team$playerID) %>%
    select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
    arrange(desc(R_hat))

# nameFirst    nameLast    BB singles doubles triples    HR  AVG R_hat
# 1     Jason      Giambi 3.317 -0.5315   0.754  -0.675 2.067 2.63  3.54
# 2     Nomar Garciaparra 0.284  1.7330   2.651   0.471 1.003 3.95  2.97
# 3      Mike      Piazza 0.596 -0.0499  -0.177  -1.335 2.682 1.70  2.56
# 4      Phil       Nevin 0.790 -0.6751   0.670  -1.137 2.103 1.09  2.07
# 5      Jeff        Kent 0.875 -0.2717   1.833   1.210 0.967 1.66  2.00

# -------       -------       -------       -------       -------       -------

# On Base Plus Slugging (OPS)

# Since the 1980s sabermetricians have used a summary statistic different from
#    batting average to evaluate players.  They realized walks were important,
#    and that doubles, triples, and home runs should be weighted much more than
#    singles, and proposed the following metric.

# ( BB / PA ) + ( Singles + 2Doubles + 3Triples + 4HR ) / AB

# They call this on-base-percentage plus slugging percentage, or OPS.  Today,
#    this statistic has caught on, and you see it in ESPN and other sports networks.
#    Although the sabermetricians are probably not using regression, this metric
#    is impressively close to what one gets with regression to the summary statistic
#    that we created.  Here is the plot.  They're very correlated.

# -------       -------       -------       -------       -------       -------

# Question 1
# The on-base-percentage plus slugging percentage (OPS) metric gives the most weight to:
# Singles
# Doubles
# Triples
# Home Runs   <-*

# -------       -------       -------       -------       -------       -------

# Regression Fallacy

#  Wikipedia defines the sophomore slump in the following way.  A sophomore slump
#     or sophomore jinx or sophomore jitters refers to an instance in which a second,
#     or sophomore, effort fails to live up to the standard of the first effort.
# It is commonly used to refer to the apathy of students-- second year of high
#     school, college, university-- the performance of athletes-- second season
#     of play-- singers/bands-- second album-- television shows-- second season--
#     and movies-- sequels or prequels.  We hear about the sophomore slump often
#     in Major League Baseball.  This is because in Major League Baseball, the
#     Rookie of the Year-- this is an award that's given to the first year player
#     that is judged to have performed the best-- usually does not perform as well
#     during their second year. Therefore they call this the sophomore slump.
# Know, for example, that in a recent Fox Sports article they asked, will MLB's
#    tremendous rookie class of 2015 suffer a sophomore slump.  Now does the data
#    confirm the existence of a sophomore slump?  Let's take a look and examine
#    the data for batting averages to see if the observation holds true.  The data
#    is available in the [? Lehman ?] [? Library, ?] but we have to do some work
#    to create a table with the statistics for all the rookies of the year.
# Let's go through them. First, we create a table with player ID, their names and
#    their most played position, using this code.

library( Lahman )
playerInfo <- Fielding %>%
    group_by( playerID ) %>%
    arrange( desc( G ) ) %>%
    slice( 1 ) %>%
    ungroup %>%
    left_join( Master, by = 'playerID' ) %>%
    select( playerID, nameFirst, nameLast, POS )
playerInfo

# Now we will create a table with only the Rookie of the Year Award winners and
#    add their batting statistics.  We're going to filter out pitchers since
#    pitchers are not given awards for batting.  And we're going to focus on
#    offense.  Specifically, we'll focus on batting average since it is the summary
#    that most pundits talk about when discussing the sophomore slump.  So we
#    write this piece of code to do this.

ROY <- AwardsPlayers %>%
    filter( awardID == 'Rookie of the Year' ) %>%
    left_join( playerInfo, by = 'playerID' ) %>%
    rename( rookie_year = yearID ) %>%
    right_join( Batting, by = 'playerID' ) %>%
    mutate( AVG = H / AB ) %>%
    filter( POS != 'P' )
ROY

# Now we'll keep only the rookie and sophomore seasons and remove players that did
#    not play a sophomore season.  And remember, now we're only looking at players
#    that won the Rookie of the Year Award.  This code achieves what we want.

ROY <- ROY %>%
    filter( yearID == rookie_year | yearID == rookie_year + 1 ) %>%
    group_by( playerID ) %>%
    mutate( rookie = ifelse( yearID == min( yearID ), 'rookie', 'sophomore' ) ) %>%
    filter( n() == 2 ) %>%
    ungroup %>%
    select( playerID, rookie_year, rookie, nameFirst, nameLast, AVG )
ROY

# Finally, we will use the spread function to have one column for the rookie and
#    another column for the sophomore years' batting averages.  For that we use
#    this simple line of  code.

ROY <- ROY %>%
    spread( rookie, AVG ) %>%
    arrange( desc( rookie ) )
ROY

# Now we can see the top performers in their first year.  These are the Rookie
#    of the Year Award winners. And we're showing their rookie season batting
#    average and their sophomore season batting average.  Look closely and you
#    will see the sophomore slump.  It definitely appears to be real.
# In fact, the proportion of players that have a lower batting average their
#    sophomore years is 68%.  So is it jitters?  Is it a jinx?
#    To answer this question, let's turn our attention to all players.  We're
#    going to look at the 2013 season and 2014 season. And we're going to look
#    at players that batted at least 130 times.  This is a minimum needed to win
#    the Rookie of the Year.
# We're going to perform a similar operation as we did before to construct this
#    data set.  Here is the code.

two_years <- Batting %>%
    filter( yearID %in% 2013:2014 ) %>%
    group_by( playerID, yearID  ) %>%
    filter( sum( AB ) >= 130 ) %>%
    summarize( AVG = sum( H ) / sum( AB ) ) %>%
    ungroup %>%
    spread( yearID, AVG ) %>%
    filter( !is.na( `2013` ) & !is.na( `2014` ) ) %>%
    left_join( playerInfo, by = 'playerID' ) %>%
    filter( POS != 'P' ) %>%
    select( -POS ) %>%
    arrange( desc( `2013` ) ) %>%
    select( -playerID )
two_years

# Now let's look at the top performers of 2013 and then look at their performance
#    in 2014.  Note that the same pattern arises when we look at the top performers.
# Batting averages go down for the top performers.  But these are not rookies.
#    So this can't be explained with a sophomore slump.  Also know what happens
#    to the worst performers of 2013.  Here they are.

arrange( two_years, `2013` )

# Their batting averages go up in their second season in 2014.  Is this some sort
#    of reverse sophomore slump?
#    It is not.  There is no such thing as a sophomore slump. This is all explained
#    with a simple statistical fact.  The correlation of performance in two separate
#    years is high but not perfect.

two_years %>%
    ggplot( aes( `2013`, `2014` ) ) +
    geom_point()

# Here is the data for 2013 performance and 2014 performance.  You can see it's
#    correlated.  But it's not perfectly correlated.  The correlation is 0.46.

summarize( two_years, cor( `2013`, `2014` ) )

# The data look very much like a bivariate normal distribution, which means that
#    if we were to predict the 2014 batting average, let's call it y, for any
#    given player that had a 2013 batting average of x, we would use the regression
#    equation, which would be this.

# ( Y - .255 ) / 0.32  =  0.46 ( X - .261 ) / .023

# Because a correlation is not perfect, regression tells us that on average, we
#    expect high performers from 2013 to do a little bit worse in 2014.
# This regression to the mean. It's not a jinx. It's just due to chance.
# The rookies of the year are selected from the top values of x
# So it is expected that their y will regress to the mean.

# -------       -------       -------       -------       -------       -------

# Question 1
# What statistical concept properly explains the “sophomore slump”?
# Regression to the mean   <-*
# Law of averages
# Normal distribution

# -------       -------       -------       -------       -------       -------

# Measurement Error Models
#  Up until now, all our linear regression examples have been applied to two or
#     more random variables.  We assume the pairs are bivariate normal and use
#     this to motivate a linear model.  This approach covers most of real life
#     examples where linear regression is used.
# The other major application comes from measurement error models.  In these
#    applications, it is common to have a nonrandom covariates, such as time.
#    And randomness is introduced from measurement error, rather than sampling
#    or natural variability.  To understand these models, we're going to use a
#    motivation example related to physics.  Imagine you are Galileo in the 16th
#    century trying to describe the velocity of a falling object.  An assistant
#    climbs the Tower of Pisa and drops a ball.  While several other assistants
#    record the position at different times.
# The falling object data set contains an example of what that data would look
#    like.  The assistant hands the data to Galileo and this is what he sees.
#    He uses ggplot to make a plot.

library( dslabs )
falling_object <- rfalling_object()

falling_object %>%
    ggplot( aes( time, observed_distance ) ) +
    geom_point() +
    ylab( 'Distance in meters' ) +
    xlab( 'Time in seconds' )

# Here we see the distance in meters that has dropped on the y-axis and time on
#    the x-axis.  Galileo does not know the exact equation, but from data exploration,
#    by looking at the plot, he deduces that the position should follow a parabola,
#    which we can write like this.

# f( x ) = Beta0 + Beta1 x + Beta2 x^2

# The data does not fall exactly on a parabola, but Galileo knows that this is due
#    to measurement error.  His helpers make mistakes when measuring the distance
#    the ball has fallen.  To account for this, we write this model.

# Yi = Beta0 + Beta1 xi + Betax xi^2 + Epsiloni
# i : 1, ..., n
# Yi : Distance in meters
# xi : Time in seconds
# Epsilon : Measurement error

# The measurement error is assumed to be random, independent from each other
#    and having the same distribution from each eye.  We also assume that there
#    is no bias, which means that the expected value of epsilon is 0.

# E[ Epsilon ] = 0

# Note that this is a linear model because it is a linear combination of known
#    quantities.  X and x squared are known and unknown parameters, the betas.
#    Unlike our previous example, the x's are fixed quantities.  This is just time.
#    We're not conditioning.  Now to pose a new physical theory and start making
#    predictions about other falling objects,  Galileo needs actual numbers, rather
#    than the unknown parameters.
# The LSE squares estimates seem like a reasonable approach.
# So how do we find the LSE squares estimates?
#     Note that the LSE calculations do not require the errors to be approximately
#     normal.  The lm( ) function will find the betas that minimize the residual
#     sum of squares, which is what we want.  So we use this code to obtain our
#     estimated parameters.

head( falling_object, 1 )

fit <- falling_object %>%
    mutate( time_sq = time^2 ) %>%
    lm( y ~ time + time_sq, data = . )
tidy( fit )

# To check if the estimated parabola fits the data, the broom function augment( )
#    lets us do this easily.  Using this code, we can make the following plot.

augment( fit ) %>%
    ggplot() +
    geom_point( aes( time, y ) ) +
    geom_line( aes( time, .fitted ) )

# Note that the predicted values go right through the points.  Now, thanks to my
#    high school physics teacher, I know that the equation for the trajectory of
#    a falling object is the following.

# d = h0 + v0t - 0.5 x 9.8t^2
# h0 : starting height
# v0 : starting velocity

# The data we use follow this equation and added measurement error to simulate an
#    observations.  Dropping the ball, that means the starting velocity is 0
#    because we start just by dropping it from the Tower of Pisa, which has a height
#    of about 56.67 meters.
# These known quantities are consistent with the parameters that we estimated,
#    which we can see using the tidy function.  Here they are.

tidy( fit, conf.int = TRUE )

# The Tower of Pisa height is within the confidence interval for beta 0.
# The initial velocity of 0 is in the confidence interval for beta 1.
# Note that the p value is larger than 0.05, which means we wouldn't reject the
#    hypothesis that the starting velocity is 0.  And finally, the acceleration
#    constant is in the confidence intervals for negative 2 times beta 2.

# -------       -------       -------       -------       -------       -------

# Question 1
# In our model of time vs. observed_distance, the randomness of our data was due to:
# sampling
# natural variability
# measurement error   <-*


# Question 2
# Which of the following are important assumptions about the measurement errors in this experiment?
# Select ALL that apply.
#
# The measurement error is random    <-*
# The measurement error is independent    <-*
# The measurement error has the same distribution for each time i   <-*


# Question 3
# Which of the following scenarios would violate an assumption of our measurement error model?
# The experiment was conducted on the moon.
# There was one position where it was particularly difficult to see the dropped ball.   <-*
# The experiment was only repeated 10 times, not 100 times.

# -------       -------       -------       -------       -------       -------

# Confounding
# Correlation is Not Causation: Spurious Correlation
# Correlation is not causation is perhaps the most important lesson one learns in
#    a statistics class.  In this course, we have described tools useful for
#    quantifying associations between variables, but we must be careful not
#    to over interpret these associations.
# There are many reasons that a variable x can correlate with a variable y, without
#    either being a cause for the other.  Here we examine common ways that can
#    lead to misinterpreting associations.  The first example of how we can
#    misinterpret associations are spurious correlations.  The following comical
#    example underscores that correlation is not causation.
# The example shows a very strong correlation between divorce rates and margarine
#    consumption.  The correlation is 0.93.  Does this mean that margarine causes
#    divorces, or do divorces cause people to eat more margarine?
#    Of course, the answer to both these questions is no.  This is just an example
#    of what we call spurious correlations.  You can see many, many more observed
#    examples in this website (http://tylervigen.com/spurious-correlations)
#    completely dedicated to spurious correlations.
# In fact, that's the title of the website.  The cases presented in the spurious
#    correlation site are all examples of what is generally called data dredging,
#    or data phishing, or data snooping.  It's basically a form of what in the
#    United States, they call cherry picking.  An example of data dredging would
#    be if you look through many results produced by a random process, and pick
#    the one that shows a relationship that supports the theory you want to defend.
# A Monte Carlo simulation can be used to show how data dredging can result in
#    finding high correlations among variables that are theoretically uncorrelated.
#    We'll save the results of a simulation into a tibble like this.

N <- 25
G <- 1000000
sim_data <- tibble( group = rep( 1:G, each = N ),
                    X = rnorm( N * G ),
                    Y = rnorm( N * G ) )
head( sim_data )

# The first column denotes group and we simulated one million groups, each with
#    25 observations.  For each group, we generate 25 observations which are
#    stored in the second and third column.  These are just random, independent
#    normally distributed data.  So we know, because we constructed the simulation,
#    that x and y are not correlated.
# Next, we compute the correlation between x and y for each group, and look for
#    the maximum.  Here are the top correlations.

res <- sim_data %>%
    group_by( group ) %>%
    summarize( r = cor( X, Y ) ) %>%
    arrange( desc( r ) )
res

# If we just plot the data from this particular group, it shows a convincing plot
#    that x and y are, in fact, correlated.

sim_data %>%
    filter( group == res$group[ which.max( res$r ) ] ) %>%
    ggplot( aes( X, Y ) ) +
    geom_point() +
    geom_smooth( method = 'lm' )

# But remember that the correlations number is a random variable.  Here's the
#    distribution we just generated with our Monte Carlo simulation.

res %>%
    ggplot( aes( x = r ) ) +
    geom_histogram( binwidth = 0.1, color = 'black' )

# It is just a mathematical fact that if we observe random correlations that are
#    expected to be 0, but have a standard error of about 0.2, the largest one
#    will be close to 1 if we pick from among one million.  Note that if we
#    performed regression on this group and interpreted the p-value, we would
#    incorrectly claim this was a statistically significant relation.
# Here's the code.

sim_data %>%
    filter( group == res$group[ which.max( res$r ) ] ) %>%
    do( tidy( lm( Y ~ X, data = . ) ) )

# Look how small the p-value is.  This particular form of data dredging is referred
#    to as p-hacking.  P-hacking is a topic of much discussion because it is a
#    problem in scientific publications.  Because publishers tend to reward statistically
#    significant results over negative results, there's an incentive to report
#    significant results.
# In epidemiology in the social sciences for example, researchers may look for
#    associations between an average outcome and several exposures, and report
#    only the one exposure that resulted in a small p-value.  Furthermore, they
#    might try fitting several different models to adjust for confounding and pick
#    the one model that yields the smallest p-value.
# In experimental disciplines, an experiment might be repeated more than once,
#    and only the one that results in a small p-value are reported.  This does
#    not necessarily happen due to unethical behavior, but rather to statistical
#    ignorance or wishful thinking.
# In advanced statistics courses, you'll learn methods to adjust for what is called
#    the multiple comparison problem.

# -------       -------       -------       -------       -------       -------

# Question 1
# In the video, we ran one million tests of correlation for two random variables, X and Y.
#
# How many of these correlations would you expect to have a significant p-value (p>0.05), just by chance?
# 5,000
# 50,000    <-*
# 100,000
# It’s impossible to know


# Question 2
# Which of the following are examples of p-hacking?
# Select ALL that apply.
#
# Looking for associations between an outcome and several exposures and only reporting the one that is significant.   <-*
# Trying several different models and selecting the one that yields the smallest p-value.   <-*
# Repeating an experiment multiple times and only reporting the one with the smallest p-value.   <-*
# Using a Monte Carlo simulations in an analysis.

# -------       -------       -------       -------       -------       -------

# Correlation is Not Causation: Outliers
# Another way that we can see high correlations when there's no causation is when
#    we have outliers.  Suppose we take measurements from two independent outcomes,
#    x and y, and we standardize the measurements.  However, imagine we made a
#    mistake and forgot to standardize entry 23.  We can simulate such data using
#    the following code.

set.seed( 1 )
x <- rnorm( 100, 100, 1 )
y <- rnorm( 100, 84, 1 )
x[ -23 ] <- scale( x[ -23 ] )
y[ -23 ] <- scale( y[ -23 ] )

# The data looks like this.

tibble( x, y ) %>%
    ggplot( aes( x, y ) ) +
    geom_point( alpha = 0.5 )

# Not surprisingly, the correlation is very high.

cor( x, y )

# That one point, that one outlier, is making the correlation be as high as 0.99.
#    But again, this is driven by that one outlier.  If we remove this outlier,
#    the correlation is greatly reduced to almost 0, which is what it should be.
# Here's what we get if we remove entry 23.

cor( x[ -23 ], y[ -23 ] )

# So one way to deal with outliers is to try to detect them and remove them.  But
#    there is an alternative way to the sample correlation for estimating the
#    population correlation that is robust to outliers.  It is called Spearman
#    correlation.  The idea is simple.  Compute the correlation on the ranks of
#    the values, rather than the values themselves.  Here's a plot of the ranks
#    plotted against each other for that data set that includes the outlier.

tibble( x, y ) %>%
    ggplot( aes( rank( x ), rank( y ) ) ) +
    geom_point( alpha = 0.5 )

# Note that the one point that's very large is just at the 100th location.  It is
#    no longer really out there and pulling the correlation towards 1.  So if we
#    compute the correlation of the ranks, we get something much closer to 0, as
#    we see here.

cor( rank( x ), rank( y ) )

# Spearman correlation can also be calculated with the correlation function, but
#    using the method argument to tell cor which correlation to compute.

cor( x, y, method = 'spearman' )

# There are also methods for robust fitting of linear models, which you can learn
#    about in, for example, this book.
#    Robust Statistics: Edition 2
#    Peter J. Huber, Elvezio M. Rochetti

# -------       -------       -------       -------       -------       -------

# Question 1
# The Spearman correlation coefficient is robust to outliers because:
# It drops outliers before calculating correlation.
# It is the correlation of standardized values.
# It calculates correlation between ranks, not values.   <- *

# -------       -------       -------       -------       -------       -------

# Correlation is Not Causation: Reversing Cause and Effect
# Another way associations are confounded with causation is when the cause and
#    effect are reversed.  An example of this is claiming that tutoring makes
#    students perform worse because they test lower than peers that are not tutored.
# Here, the tutoring is not causing the low test, but the other way around.
#    A form of this claim was actually made in an op ed in the New York Times,
#    titled "Parental Involvement is Overrated".  Consider this quote from the
#    article.  "When we examine whether regular help with homework had a positive
#    impact on children's academic performance, we were quite startled by what we
#    found.
# Regardless of family social class, racial, or ethnic background, or child's
#    grade level, consistent homework help almost never improved test scores or
#    grades.
# Even more surprising to us was that when parents regularly helped with homework,
#    kids usually performed worse.
# A very likely possibility is that children needing regular parental help get
#    this help because they don't perform well in school.  To see another example,
#    we're going to use one of the data sets that we've seen in this course.
# Specifically, we can easily construct an example of cause and effect reversal
#    using the father and son height data.  Note that if we fit the following
#    model to the father and son height data, with x representing the father
#    height, and y representing the son height, we do get a statistically
#    significant result.

# Xi = Beta0 + Beta1 yi + Epsiloni
# i = 1, ..., N
# Xi : father height
# yi : son height

# You can see that with this simple code.

library( HistData )
data( "GaltonFamilies" )
GaltonFamilies %>%
    filter( childNum == 1 & gender == 'male' ) %>%
    select( father, childHeight ) %>%
    rename( son = childHeight ) %>%
    do( tidy( lm( father ~ son, data = . ) ) )

# This model fits the data very well.  However, if we look at the mathematical
#    formulation of the model, it could easily be incorrectly interpreted as to
#    suggest that the son being tall caused the father to be tall.
# But given what we know about genetics and biology, we know it's the other way
#    around.  The model is technically correct.  The estimates and p-value were
#    obtained correctly as well.  What is wrong here is simply the interpretation.

# -------       -------       -------       -------       -------       -------

# Question 1
# Which of the following may be examples of reversed cause and effect?
# Select ALL that apply.
#
# Past smokers who have quit smoking may be more likely to die from lung cancer.   <-*
# Tall fathers are more likely to have tall sons.
# People with high blood pressure tend to have a healthier diet.   <-*
# Individuals in a low social status have a higher risk of schizophrenia.   <-*

# -------       -------       -------       -------       -------       -------

# Correlation is Not Causation: Confounders
# Confounders are perhaps the most common reason that leads to associations being
#    misinterpreted.  If X and Y are correlated, we call Z a confounder if changes
#    in Z cause changes in both X and Y. Earlier, when studying baseball data,
#    we saw how home runs was a confounder that resulted in higher correlation
#    than expected when studying the relationship between bases and balls and runs.
# In some cases, we can use linear models to account for confounders, as we did
#    in the baseball example.  But it is not always possible.  Incorrect interpretation
#    due to confounders are ubiquitous in the lay press.  They are sometimes hard
#    to detect.
# We examined admission data from UC Berkeley majors from 1973 that showed that
#    more men were being admitted than women. 44% men were admitted compared to
#    30% women.  Here's the data.

data( "admissions" )
admissions

# The percent of men and women that got accepted can be computed from this data
#    using this simple piece of code.

admissions %>%
    group_by( gender ) %>%
    summarize( percentage = round( sum( admitted * applicants ) / sum( applicants ), 1 ) )

# A statistical test, the chi-squared test that we learned about in a previous
#    course, clearly rejects the hypothesis that gender and admissions are independent.

admissions %>%
    group_by( gender ) %>%
    summarize( total_admitted = round( sum( admitted / 100 * applicants ) ),
               not_admitted = sum( applicants ) - sum( total_admitted ) ) %>%
    select( -gender ) %>%
    do( tidy( chisq.test( . ) ) )

# The p value is very small.  But closer inspection shows a paradoxical result.
#    Here are the percent of admissions by major.

admissions %>%
    select( major, gender, admitted ) %>%
    spread( gender, admitted ) %>%
    mutate( women_minus_men = women - men )

# Four out of the six majors favor women.  But more importantly, all the differences
#    are much smaller than the 14% difference that we see when examining the totals.
#    The paradox is that analyzing the totals suggest a dependence between admissions
#    and gender.  But when the data is grouped by major, this dependence seems to
#    disappear.  What's going on?
#    This actually can happen if an uncounted confounder is driving most of the
#    variability. So let's define three variables.  X is 1 for men and 0 for women,
#    Y is 1 for those admitted and 0 otherwise, and Z quantifies how selective
#    the major is.

# X : 1 for men
# X : 0 for women
# Y : 1 for admitted
# Y : 0 for not admitted
# Z : selectivity of major

# A gender bias claim would be based on the fact that this probability is higher
#    when X is 1 than when X is 0.  But Z is an important confounder.

# Pr( Y = 1 | X =x )

# Clearly, Z is associated with Y, because the more selective a major, the lower
#    the probability that someone enters that major.  But is major selectivity,
#    which we call Z, associated with gender?
#    One way to see this is to plot the total percent admitted to a major versus
#    the percent of women that make up the applicants.
# We can see that in this plot.

admissions %>%
    group_by( major ) %>%
    summarize( major_selectivity = sum( admitted * applicants ) / sum( applicants ),
               percent_women_applicants = sum( applicants * ( gender == 'women' ) / sum( applicants ) ) *100 ) %>%
    ggplot( aes( major_selectivity, percent_women_applicants, label = major ) ) +
    geom_text()

# There seems to be an association.  The plot suggests that women were much more
#    likely to apply to the two hard majors.  Gender and major selectivity are
#    confounded.  Compare, for example, major B and major E. Major E is much
#    harder to enter than major B. And over 60% of applicants to major E were
#    women, while less than 30% of the applicants of major B were women.
# The following plot shows the percentage of applicants that were accepted by gender.

admissions %>%
    mutate( percent_admitted = admitted * applicants / sum( applicants ) ) %>%
    ggplot( aes( gender, y = percent_admitted, fill = major ) ) +
    geom_bar( stat = 'identity', position = 'stack' )

# The color here represents major.  It also breaks down the acceptance rates by
#    major.  The size of the colored bar represent the percent of each major
#    students that were admitted to.  This breakdown lets us see that the majority
#    of accepted men came from two majors, A and B. It also lets us see that few
#    women apply to these two easy majors.
# What the plot does not show us is what's the percent admitted by major.  In this
#    plot, we can see that if we condition or stratify by major, and then look at
#    differences, we control for the confounder, and the effect goes away.

admissions %>%
    ggplot( aes( major, admitted, col = gender, size = applicants ) ) +
    geom_point()

# Now we see that major by major, there's not much difference.  The size of the
#    dot represents the number of applicants, and explains the paradox.  We see
#    large red dots and small blue dots for the easiest majors, A and B.  If we
#    first stratify by major, compute the difference, and then average, we find
#    that the percent difference is actually quite small.  This is actually an
#    example of something that is called Simpson's paradox, which we will describe
#    in the next video.

admissions %>%
    group_by( gender ) %>%
    summarize( average = mean( admitted ) )

# -------       -------       -------       -------       -------       -------

# Question 1
# What can you do to determine if you are misinterpreting results because of a confounder?
#
# Nothing, if the p-value says the result is significant, then it is.
# More closely examine the results by stratifying and plotting the data.  <-*
# Always assume that you are misinterpreting the results.
# Use linear models to tease out a confounder.



# Question 2
# Look again at the admissions data using ?admissions. What important characteristic of the table variables do you need to know to understand the calculations used in this video? Select the best answer.
#
# The data is from 1973.
# The columns “major” and “gender” are of class character, while “admitted” and “applicants” are numeric.
# The data is from the “dslabs” package.
# The column “admitted” is the percent of student admitted, while the column “applicants” is the total number of applicants.   <-*



# Question 3
# In the example in the video, major selectivity confounds the relationship between UC Berkley admission rates and gender because:
#
#
# It was harder for women to be admitted to UC Berkeley.
# Major selectivity is associated with both admission rates and with gender, as women tended to apply to more selective majors.    <-*
# Some majors are more selective than others
# Major selectivity is not a confounder.

# -------       -------       -------       -------       -------       -------

# Simpson's Paradox
# We have just seen an example of Simpson's paradox.  It is called a paradox
#    because we see the sign of the correlation flip when we computed on the
#    entire population and when we computed on specific strata.
# Now, we're going to use a very illustrative simulated example to show you how
#    this can happen.  Suppose you have three variables-- x, y, and z.
# Here's a scatter plot of y versus x.  You can see that x and y are negatively
#    correlated.
# However, once we stratify by z--the confounder which we haven't looked at yet--
#    we see that another pattern emerges.  This plot, the different strata defined
#    by disease, are shown in different colors.
# If you compute the correlation in each strata, you see that the correlations
#    are now positive.  So it's really z that is negatively correlated with x.
# If we stratify by z, the x and y are actually positively correlated.  This is
#    an example of Simpson's paradox.

# -------       -------       -------       -------       -------       -------

# Question 1
# Admission rates at UC Berkeley are an example of Simpson’s Paradox because:
#
# It appears that men have higher a higher admission rate than women, however, after we stratify by major, we see that on average women have a higher admission rate than men.   <-*
# It was a paradox that women were being admitted at a lower rate than men.
# The relationship between admissions and gender is confounded by major selectivity.

