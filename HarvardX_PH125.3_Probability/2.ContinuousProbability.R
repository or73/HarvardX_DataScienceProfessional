# Continuous Probability

library( tidyverse )
library( dslabs )

data( "heights" )

x <- heights %>% filter( sex == 'Male' ) %>% .$height
x

# Empirical distribution function
# For every value of a, this gives a proportion of values in thelist x that are
#    smaller or equal to a
F <- function( a ) mean( x <= a )
F

# If I pick one of the male students at random, what is the chance that he is
#    taller than 70.5 inches?

# Because every student hs the same chance of being picked, the answer to this
#    question is the proportion of students that are taller than 70.5
1 - F( 70 )

# The probability of a student being between the height 'a' and the height 'b'
#    is simpli F( b ) - F( a )


# Theoretical Distribution
# We say that a random quantity is normally distributed with average 'avg', and
#    standard deviation 's', if it's probability distribution is defined by:
#    F( a ) = pnorm( a, avg, s )

# What is the probability that a randomly selected student is taller than 70.5
#    inches.
1 - pnorm( 70.5, mean( x ), sd( x ) )


# For practicing data scientists, pretty much everything we do involves data,
#    which is technically speaking discrete.
# For example, we could consider our adult data categorical with each specific
#     height a unique category.  The probability distribution would then be
#     defined by the proportion of students reporting each of those unique
#     lengths.
plot( prop.table( table( x ) ), xlab = "a = Hegiht in inches", ylab = "Pr( X = a )" )

# A student reported his height to be 69.6850393700787, what is that about?
# What's that very, very precise number?
# That's 177 centimeters
# The probability assigned to this height is about 0.001, it's 1 in 708
# However, the probability for 70 inches is 0.12

# The normal distribution is useful for appoximating the proportion of students
#    reporting between 69.5 and 70.5
mean( x <= 68.5 ) - mean( x <= 67.5 )
pnorm( 68.5, mean( x ), sd( x ) ) - pnorm( 67.5, mean( x ), sd( x ) )   # approximation

mean( x <= 69.5 ) - mean( x <= 68.5 )
pnorm( 69.5, mean( x ), sd( x ) ) - pnorm( 68.5, mean( x ), sd( x ) )   # approximation

mean( x <= 70.5 ) - mean( x <= 69.5 )
pnorm( 70.5, mean( x ), sd( x ) ) - pnorm( 69.5, mean( x ), sd( x ) )   # approximation


# Approximation is not always useful - Discretization
mean( x <= 70.9 ) - mean( x <= 70.1 )
pnorm( 70.9, mean( x ), sd( x ) ) - pnorm( 70.1, mean( x ), sd( x ) )


# Probability Density
# For categorical data, we can define the probability of a category
# For example: roll of a die, let's call it x, can be: 1, 2, 3, 4, 5, 6
# Pr( X = 4 ) = 1/6
# F( 4 ) = Pr( X <= 4 ) = Pr( X = 4 ) + Pr( X = 3 ) + Pr( X = 2 ) + Pr( X = 1 )

# Probability of someone being taller than 76 inches
avg <- mean( x )
s <- sd( x )
1 - pnorm( 76, avg, s )

# dnorm -> density normal
# pnorm -> probability normal
# rnorm -> random normal
# qnorm -> quantiles


# Monte Carlo simulations
x <- heights %>% filter( sex == 'Male' ) %>% .$height
n <- length( x )
avg <- mean( x )
s <- sd( x )
simulated_heights <- rnorm( n, avg, s )
simulated_heights
ds_theme_set()
data.frame( simulated_heights = simulated_heights ) %>% ggplot( aes( simulated_heights ) ) + geom_histogram( color = 'black', binwidth = 2 )


# If we pick 800 males at random, what is the distribution of the tallest person?
B <- 10000
tallest <- replicate( B, {
    simulated_data <- rnorm( 800, avg, s )
    max( simulated_data )
} )
tallest
mean( tallest >= 7*12 )


x <- seq( -4, 4, length.out = 100 )
data.frame( x, f = dnorm( x ) ) %>% ggplot( aes( x, f ) ) + geom_line()







