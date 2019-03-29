# Correlation

# Galto'n family data
# We'll create a data set with the heights of fathers and the first sons.
# The actual data Galton used to discover and define regression.

library( HistData )
data( "GaltonFamilies" )

galton_heights <- GaltonFamilies %>%
    filter( childNum == 1 & gender == 'male' ) %>%
    select( father, childHeight ) %>%
    rename( son = childHeight )
head( galton_heights )

# So we have the father and son height data.
# Suppose we were to summarize these data.
# Since both distributions are well approximated by normal distributions,
# we can use the two averages and two standard deviations as summaries.
galton_heights %>%
    summarize( mean( father ), sd( father ), mean( son ), sd( son ) )

# However, this summary fails to describe a very important characteristic
#    of the data that you can see in this figure
galton_heights %>%
    ggplot( aes( father, son ) ) +
    geom_point( alpha = 0.5 )

# The trend that the taller the father, the taller the son, is not described
#    by the summary statistics of the average and the standard deviation.

# -------       -------       -------       -------       -------       -------

#Question 1
# While studying heredity, Francis Galton developed what important statistical concept?
# Standard deviation
# Normal distribution
# Correlation   <-*
# Probability



# Question 2
# The correlation coefficient is a summary of what?
# The trend between two variables   <-*
# The dispersion of a variable
# The central tendency of a variable
# The distribution of a variable

# -------       -------       -------       -------       -------       -------

# Correlation Coefficient
#
# The correlation coefficient is defined for a list of pairs-- x1, y1 through
#    xn, yn-- with the following formula.
# Here, mu x and mu y are the averages of x and y, respectively.
#    And sigma x and sigma y are the standard deviations.
# p = ( 1 / n ) sum( ( ( Xi - mux) / sigmax) ( ( Yi - muy ) / sigmay ) )

# To understand why this equation does, in fact, summarize how two variables
#    move together, consider the i-th entry of x is xi minus mu x divided
#    by sigma x SDs away from the average.

Xi = ( X - mux ) / deltax

# Similarly, the yi-- which is paired with the xi-- is yi minus mu y divided
#    by sigma y SDs away from the average y.

Yi = ( Yi - muy ) / deltay

# If x and y are unrelated, then the product of these two quantities will be
#    positive.

# ( ( Xi - mux ) / sigmax ) ( ( Yi / muy ) / sigmay )

# That happens when they are both positive or when they are both negative as
#    often as they will be negative.  That happens when one is positive and the
#    other is negative, or the other way   One is negative and the other one is
#    positive.  This will average to about 0.  The correlation is this average.
#    And therefore, unrelated variables will have a correlation of about 0.

# If instead the quantities vary together, then we are averaging mostly positive
#    products.  Because they're going to be either positive times positive or
#    negative times negative.  And we get a positive correlation.  If they vary
#    in opposite directions, we get a negative correlation.

# The correlation between father and sons' height is about 0.5.
library( HistData )
data( 'GaltonFamilies' )
galton_heights <- GaltonFamilies %>%
    filter( childNum == 1 & gender == 'male' ) %>%
    select( father, childHeight ) %>%
    rename( son  = childHeight )

galton_heights %>% summarize( cor( father, son ) )

# When the correlation is negative, we see that they go in opposite direction.
#    As x increases, y decreases.
# When the correlation gets either closer to 1 or negative 1, we see the clot
#    of points getting thinner and thinner.
# When the correlation is 0, we just see a big circle of points.

# -------       -------       -------       -------       -------       -------

# Question 1
# Below is a scatter plot showing the relationship between two variables, x and y.
#
# Scatter plot of relationship between x (plotted on the x-axis) and y (plotted on the y-axis). y-axis values range from -3 to 3; x-axis values range from -3 to 3. Points are fairly well distributed in a tight band with a range from approximately (-2, 2) to (3, -3).
# From this figure, the correlation between x and y appears to be about:
# -0.9    <-*
# -0.2
# 0.9
# 2

# -------       -------       -------       -------       -------       -------

# Sample Correlation is a Random Variable

# Random sample of 25 pairs
set.seed( 0 )
R <- sample_n( galton_heights, 25, replace = TRUE ) %>%
    summarize( cor( father, son ) )
R  # Is the random variable


# Monte Carlo simmulation to see the distribution of this random variable
B <- 1000
N <- 50
R <- replicate( B, {
    sample_n( galton_heights, N, replace = TRUE ) %>%
        summarize( r = cor( father, son ) ) %>%
        .$r
} )
data.frame( R ) %>% ggplot( aes( R ) ) +
    geom_histogram( binwidth = 0.05, color = 'black' )

# We see that the expected value is the population correlation, the mean of
#    these Rs is 0.5, and that it has a relatively high standard error relative
#    to its size, SD 0.147.
mean( R )
sd( R )

#  Note that because the sample correlation is an average of independent draws,
#     the Central Limit Theorem actually applies.

# -------       -------       -------       -------       -------       -------

# Question 1
# Instead of running a Monte Carlo simulation with a sample size of 25 from our 179 father-son pairs, we now run our simulation with a sample size of 50.
#
# Would you expect the mean of our sample correlation to increase, decrease, or stay approximately the same?
# Increase
# Decrease
# Stay approximately the same   <-*
# unanswered



# Question 2
# Instead of running a Monte Carlo simulation with a sample size of 25 from our 179 father-son pairs, we now run our simulation with a sample size of 50.
#
# Would you expect the standard deviation of our sample correlation to increase, decrease, or stay approximately the same?
# Increase
# Decrease    <-*
# Stay approximately the same
