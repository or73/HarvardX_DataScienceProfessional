# Random Variables
# Are numeric outcomes resulting from a random process.

# Define x to be 1 if a bead is blue, and red otherwise
beads <- rep( c( 'red', 'blue' ), times = c( 2, 3 ) )
beads
X <- ifelse( sample( beads, 1 ) == 'blue', 1, 0 )
X
ifelse( sample( beads, 1 ) == 'blue', 1, 0 )
ifelse( sample( beads, 1 ) == 'blue', 1, 0 )
ifelse( sample( beads, 1 ) == 'blue', 1, 0 )

# Sampling models
# Example: we can model the process of polling likey voters as drawing
#    0's = Republicans
#    1's = Democrats
#    from an urn containing the 0 and 1 code for all likely voters


# Example: a very small casino hires you to consult on whether they should set up
#    a reoulette wheel.  They want to know if they can make money off it,
#    or if it's too risy and they might lose.
#    Assume that 1000 people will play and that the only game you can play is
#    to bet on red or black.
#    The casino wants to predict how much money they will make or lose.
#    They wan a range of values that are possible, and in particular, they want
#     to know, what is the chance of losing money?
#     If this probability is too high, they will pass on installing reoulette
#     wheels, since they can't take the risk, giben that they need to pay their
#     employess and keep the lights on.

# S -> casino total winnings
# A roulette wheel has 18 red pockets, and 18 black pockets, and 2 green ones.
# Construct the urn
color <- rep( c( 'Black', 'Red', 'Green' ), c( 18, 18, 2 ) )
color
# The 1000 outcomes from 1000 people playing are independent draws from this urn
# If red comed up, the gambler wins, and the casino loses $1, so we draw a
#    negative 1.  Otherwise, the casino wins $1, and we draw a 1.
n <- 1000
X <- sample( ifelse( color == 'Red', -1, 1 ), n, replace = TRUE )
X[ 1:10 ]

# Because, we know the proportions with one line of code, without defining color.
X <- sample( c( -1, 1 ), n, replace = TRUE, prob = c( 9/19, 10/19 ) )   # Sampling model
X
S <- sum( X )
S

# The probability distribution of a random variables> probability of the
#    observed value falling in any given interval
# Example: if we want to know the probability that we lose money, we're asking
#    waht is the probability that S is in the interval S less than 0.
# F( a ) = Pr( S <= a )   <- Random Distribution Function
n <- 1000
B <- 10000
S <- replicate( B, {
    X <- sample( c( -1, 1 ), n, replace = TRUE, prob = c( 9/19, 10/19 ) )
    sum( X )
} )

mean( S < 0 )    # Probability of lose money
hist( S )
avg <- mean( S )
sd <- sd( S )
s <- seq( min( S ), max( S ), length = 100 )
normal_density <- data.frame( s = s, f = dnorm( s, avg, sd ) )
data.frame( S = S ) %>% ggplot( aes( S, ..density.. ) ) +
    geom_histogram( color = 'black', binwidth =  10 ) +
    ylab( 'Probability' ) +
    geom_line( data = normal_density, mapping = aes( s, f ), color = 'blue ')




# Distributions versus Probability Distributions
# Distribution of a list of numbers -> F( a ) = what proportion or the list is
#                                               less than or equal to a?
avg <- sum( x ) / length( x )
s <- sqrt( sum( x - avg )^2 ) / length( x )
s

# A random variable 'x' has a distribution function.  To define this, we do not
#    need a list of numbers.  It's a theoretical concept.
#    F( a ) = what is the probability that 'x' is less than or equal to 'a'
#             there is not a list of numbers.
# Howerver, if 'x' is defined by drawing from an urn with numbers in it,
#    then there is a list, the list of numbers inside the urn.  In this case,
#    the distribution of that list is the probability distribution of 'x' and
#    the average and standard deviation of that list are the expected value and
#    standard errors of the random variable.  Another way to think about it that
#    does no involve an urn is to run a Monte Carlo simulation and generate a
#    very large list of outcomes of 'x'.  These outcomes are a list of numbers.
#    The distribution of this list will be a very good approximation of the
#    probability distribution of 'x'.  The longer the list, the better the
#    approximation, the average and standard deviation of this list will
#    approximate the expected value and standard error of the random variable.

# Notation for random variables
# Capital letters are used to denote random variables
# Lower case letters are used for observed values


# Central limit theorem
# When the number of draws is large, the sum of the independent draws is
#    approximately normal.
#    average -> expected value   E[ X ] = u
#    standard deviation -> standard error   SE[ X ]
#              If draws are independent
#                   Then the standard of the sum is =
#                     sqrt( number of draws ) x standard deviation of the numbers
#                                                  in the urn
#                     Standard Deviation = | b - a | sqrt( p( 1 - p ) )
#                     | 1 - ( -1 ) | sqrt( 10/19 x 9/19 )
B <- 10^6
X <- sample( c( -1, 1 ), B, replace = TRUE, prob = c( 9/19, 10/19 ) )
X
mean( X )

# The first useful fact is that the expected value of the sum of draws is the
#    number in the urn.  So if 1000 people play roulette, the Casino expects
#    to win, on average, 1000 times $ 0.5, which is $50.  But this is an expected
#    value.
# number of draws x average of the numbers in the urn

# How different can one observation be from the expected value?


# The sum of 1000 people playing has standard error of about $32
n <- 1000
sqrt( n ) * 2 * sqrt( 90 ) / 19


n * ( 20 - 18 ) / 38
sqrt( n ) * 2 * sqrt( 90 ) / 19


mean( S )
sd( S )
