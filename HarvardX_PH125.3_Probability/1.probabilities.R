# Pick a bead from a bag or a urn with 3 bluer and 2 read ones.
beads <- rep( c( 'red', 'blue'), times = c( 2, 3 ) )
beads

sample( beads, 1 )   # pick one bead randomly

# Montecarlos simulation
# Repeat is used to repeat the same experiment several times
B <- 10000
events <- replicate( B, sample( beads, 1 ) )
tab <- table( events )
tab

# Same previous exercicse but with replacement
events_replacement <- sample( beads, B, replace = TRUE )
prop.table( table( events_replacement ) )


# Exercise 1. Probability of cyan - generalized
# In the edX exercises for this section, we calculated some probabilities by hand.
# Now we'll calculate those probabilities using R.

# One ball will be drawn at random from a box containing: 3 cyan balls,
#    5 magenta balls, and 7 yellow balls.

#  What is the probability that the ball will be cyan?

# INSTRUCTIONS
#  Define a variable p as the probability of choosing a cyan ball from the box.
#  Print the value of p.
cyan <- 3
magenta <- 5
yellow <- 7

# Assign a variable `p` as the probability of choosing a cyan ball from the box
p <- cyan / (cyan + magenta + yellow )

# Print the variable `p` to the console
p


# Exercise 2. Probability of not cyan - generalized
# We defined the variable p as the probability of choosing a cyan ball from a
#    box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.

# What is the probability that the ball you draw from the box will NOT be cyan?

# INSTRUCTIONS
# Using the probability of choosing a cyan ball, p, calculate the probability
#    of choosing any other ball.

# `p` is defined as the probability of choosing a cyan ball from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.
# Using variable `p`, calculate the probability of choosing any ball that is not cyan from the box
1 - p


# Exercise 3. Sampling without replacement - generalized
# Instead of taking just one draw, consider taking two draws. You take the
#    second draw without returning the first draw to the box. We call this
#    sampling without replacement.

# What is the probability that the first draw is cyan and that the second
#    draw is not cyan?

# INSTRUCTIONS
#  Calculate the probability of choosing a ball that is not cyan after
#    one cyan ball has been removed from the box.
# Calculate probability of two sequential actions occuring: choosing a
#    cyan ball on the first draw and a ball that is not cyan on the second draw.
cyan <- 3
magenta <- 5
yellow <- 7

# The variable `p_1` is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p_2` as the probability of not choosing a cyan ball on the second draw without replacement.
p_2 <- ( magenta + yellow ) / ( cyan - 1 + magenta + yellow )

# Calculate the probability that the first draw is cyan and the second draw is not cyan.
p_1 * p_2



# Exercise 4. Sampling with replacement - generalized
# Now repeat the experiment, but this time, after taking the first draw and
#    recording the color, return it back to the box and shake the box. We call
#    this sampling with replacement.

# What is the probability that the first draw is cyan and that the second
#    draw is not cyan?

# INSTRUCTIONS
# Calculate probability of two independent actions occuring: choosing a cyan
#    ball on the first draw and a ball that is not cyan on the second draw,
#    after replacing the first ball.
cyan <- 3
magenta <- 5
yellow <- 7

# The variable 'p_1' is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable 'p_2' as the probability of not choosing a cyan ball on the second draw with replacement.
p_2 <- 1 - p_1

# Calculate the probability that the first draw is cyan and the second draw is not cyan.
p_1 * p_2













