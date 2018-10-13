# Big Short: Interest Rates Explained

# The sampling models we've been talking about are also used by banks to decide
#    interest rate.  Let's see how this could be.  Suppose you run a small bank
#    that has a history of identifying potential homeowners that can ve trusted
#    to make payments.  In fact, historically in a given year only 2% of your
#    customers default, meaning that they don't pay the money you lent them.
#    However, note that if you simply loan money to everybody without interest
#    you'll end up losing money due to this 2%.  Althoug you know 2% of your
#    clients will probably default, you don't know which ones. However, by
#    charging everybody just a bit extra you can make up for the losses incurred
#    due to the 2% and also pay the employees at work to make these loans happen.
#    You can also make a profit, but if you ser the interest rate too high your
#    clients will go to another bank.

#    Suppose that your bank will give out 1.000 loans for 180.000 this year. Also,
#    suppose that your bank loses, after adding up all the costs, $200.000 per
#    foreclosure.  For simplicity, we assume that includes all operational costs.

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c( 0, 1 ), n, prob = c( 1 - p, p ), replace = TRUE )
sum( defaults * loss_per_foreclosure )

# We can construct a Monte Carlo simulation to get an idea of the distribution
#    of this random variable

B <- 10000
losses <- replicate( B, {
    defaults <- sample( c( 0, 1 ), n, prob = c( 1 - p, p ), replace = TRUE )
    sum( defaults * loss_per_foreclosure )
} )
losses

data.frame( losses_in_millions = losses / 10^6 ) %>%
    ggplot( aes( losses_in_millions ) ) +
    geom_histogram( binwidth = 0.6, col = 'black' )

# Using Central Limit Theorem

n * ( p * loss_per_foreclosure + ( 1 - p ) * 0 )
sqrt( n ) * abs( loss_per_foreclosure ) * sqrt( p * ( 1 - p ) )

# Set an interest rate to guarantee that on average, we break even
# We need to add a quantity 'x' to each loan, which in this case are represented
#    by draws, so taht the expected values is zero
# That mean breaken even
# lp + x( 1- p ) = 0
# lp -> loss per foreclosure
# x <- loss_per_foreclosure * p / ( 1 - p )
-loss_per_foreclosure * p / ( 1 - p )   # This is an interest rate of about 2%


# We want our chances of losing money to be one in 100.
# What does 'x' have to be now?
# Pr( S < 0 ) = 0.01
# S -> sum
# n -> number of loans/draws
# Expected value ofS  E[S]-> { lp + x( 1-p ) } n
# Standard Error      SE[S]-> | x - l | sqrt( np( 1 - p ) )
# Pr( S < 0 ) = 0.01  =  Pr ( (S - E[S] ) / SE[S] < -E[S] / SE[S] )
# Z -> ( S - E[S] ) / SE[S] )
#
# Pr( S < 0 = 0.01 )  = Pr( Z < -E[S] / SE[S] )
#                     = Pr( Z < -{ lp + x( 1-p ) }n / ( x-l ) sqrt( np( 1- p ) ) )
# The term on the left side is a normal random variable with
#    expected value of zero
#    standard error one
# It means that the quatinty on the right must be qnorm( 0.01 ) = -2.326348
qnorm( 0.01 )   # Pr( Z <= z ) = 0.01


l <- loss_per_foreclosure
z <- qnorm( 0.01 )
x <- -l * ( n * p - z * sqrt( n * p * ( 1 - p ) ) ) / ( n * ( 1 - p ) + z * sqrt( n * p * ( 1 - p )) )
x   # interest rate of about 3%


# we now have an expected profit per loan of about $2.124, whic is a total
#    expected profit of about $2 million
# We can run a Monte Carlo simulation and check our theoretical approximation.
loss_per_foreclosure * p + x * ( 1 - p )

B <- 10000
profit <- replicate( B, {
    draws <- sample( c( x, loss_per_foreclosure ), n,
                     prob = c( 1 - p, p ), replace = TRUE )
    sum( draws )
} )
profit
mean( profit )
mean( profit < 0 )



# The Big Short
# One of our employees points out that since the bank is making about $2000 per
#    loan, that you should give out more loans.  Why just n?  You explain that
#    finding those end clients was hard.  You need a group that is predictable,
#    and that keeps the chances of defaults low.
#    He then points our that even if the probability of default is higher, as
#    long as your expected value is positive, you can minimize your chances of
#    losing money by increading 'n', the number of loans, and relying on the law
#    of large numbers.
#    He claims that even if the default rate is twice as high, say 4%, if we set
#     the rate just a bit higher so taht this happens, you will get a postive
#     expected value.
#    So if we set the interest rate at 4%, we are guaranteed
#     a positive expected value of $640 per loan.

r <- 0.05
x <- r * 180000
p <- 0.04
loss_per_foreclosure * p + x * ( 1 - p )

# And we can minimize our chances of losingmoney by simply increasing the number
#    of loans, since the probability of S being less than 0 is equal to the
#    probability of Z being less than negative expected value of S divided by
#    standard erro S, with Z a standard normal random variable.
#    Pr( S < 0 ) = Pr( Z < -E[S] / SE[S] )
#    E[S] = nu
#    SE[S] = sqrt( nsigma )
#    -( nu / sqrt( nsigma ) ) = -sqrt( nu ) / sigma = z
#    if n >= Z^2 * sigma^2 / u^2    then we guranteed to have a probability of
#                                      less than 0.01 of losing money

# When 'n' is large our average earning per loan vonverges to the expected
#    earning 'u'
# With 'x' fixed, now we can ask what 'n' do we need for the probability to be 0.01?
z <- qnorm( 0.01 )
n <- ceiling( (z^2 * ( x - l )^2 * p * ( 1 - p ) ) / ( l * p + x * ( 1 - p ) )^2 )
n

# we are expected to earn a total of about
n * ( loss_per_foreclosure * p + x * ( 1 - p ) )


p <- 0.04
x <- 0.05 * 180000
profit <- replicate( B, {
    draws <- sample( c( x, loss_per_foreclosure ), n,
                     prob = c( 1 - p, p ), replace = TRUE )
    sum( draws )
} )
profit
mean( profit < 0 )




p <- 0.04
x <- 0.05 * 180000
profit <- replicate( B, {
    new_p <- 0.04 + sample( seq( -0.01, 0.01, length = 100 ), 1 )
    draws <- sample( c( x, loss_per_foreclosure ), n,
                     prob = c( 1 - new_p, new_p ), replace = TRUE )
    sum( draws )
} )
profit
mean( profit )
mean( profit < 0 )
mean( profit < -10000000 )
