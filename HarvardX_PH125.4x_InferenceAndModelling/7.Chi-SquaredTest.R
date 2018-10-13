#Association Tests

# The statistical tests we have covered up to now leave out a substantial portion
#    of data types.  Specifically, we have not discussed inference for binary,
#    categorical, or ordinal data.  To give a very specific example, consider
#    the following case study.
# A 2014 PNAS paper analyzed success rates from funding agencies in the Netherlands
#    and concluded that their "results reveal gender bias favoring male applicants
#    over female applicants in the prioritization of their quality of research."
# The main evidence for this conclusion comes down to a comparison of the percentages.
# The first table in the supplement of the paper includes the information we need.
# We have included it in the dslabs package.  And here it is.

data( "research_funding_rates" )
research_funding_rates

# We can compute the differences in percentages for men and women. To do this,
#    we'll compute the totals that were successful and the totals that were not
#    using this code.

totals <- research_funding_rates %>%
    select( -discipline ) %>%
    summarise_all( funs( sum ) ) %>%
    summarize( yes_men = awards_men,
               no_men = applications_men - awards_men,
               yes_women = awards_women,
               no_women = applications_women - awards_women )
totals

totals %>% summarize( percent_men = yes_men / ( yes_men + no_men ),
                     percent_women = yes_women / ( yes_women + no_women ) )

# So we see that a larger percent of men received awards than women.
# The percent for men was about 18%.
# The percent for women was about 15%.
# But could this be due just to random variability?
#    Here in this video, we learn how to perform inference for this type of data.
# R.A. Fisher was one of the first to formalize hypothesis testing.
# The Lady Tasting Tea is one of the most famous examples.
# The story goes like this.  Muriel Bristol, a colleague of Fisher's, claimed
#     that she could tell if milk was added before or after tea was poured.
# Fisher was skeptical.  He designed an experiment to test this claim.  He gave
#    her 4 pairs of cups of tea, 1 with milk poured first, the other after.  The
#    order of the two was randomized.  The null hypothesis here is that she was
#    just guessing.
# Fisher derived the distribution of the number of correct picks on the assumption
#    that the choices were random and independent.  As an example, suppose she
#    picked 3 out of 4 correctly.
# Do we believe she has a special ability based on this?
#     The basic question we ask is, if the tester is actually guessing,
# what are the chances that she gets 3 or more correct?
#     Just as we have done before, we can compute a probability under the null
#     hypothesis that she's guessing four of each.
# Under this null hypothesis, we can think of this particular example as picking
#    4 beads out of an urn where 4 are blue.
# Those are the correct answers.
# And 4 are red.
# Those are the incorrect answers.
# Remember that she knows that there are 4 before tea and 4 after.
# Under the null hypothesis that she's simply guessing, each bead has the same
#    chance of being picked. We can then use combinatorics to figure out each
#    probability. The probability of picking 3 can be derived using this mathematical
#    formula that tells you that it's 16/70.  The probability of picking 4 correct
#    is given by this formula, and the answer is 1/70.
# Thus the chance of observing 3 correct answers or more under the null hypothesis
#    is approximately 0.24.  This is the p-value.  The procedure that produces
#    p-value is called Fisher's exact test.  And it uses the hypergeometric
#    distribution to compute the probabilities. One quick note.  In the real story,
#    it turns out Muriel could tell if the milk was poured before or after every
#    single time.  So the p-value was 1/70.
# The data from this type of experiment is usually summarized by a table like this.

tab <- matrix( c( 3, 1, 1, 3 ), 2, 2 )
rownames( tab ) <- c( 'Poured Before', 'Poured After' )
colnames( tab ) <- c( 'Guessed Before', 'Gessed After' )
tab

# These are referred to as two-by-two tables.
# They show, for each of the 4 combinations one can get with a pair of binary
#    variables, the observed counts for each of these pairs. The function
#    fisher.test performs the inference calculations and can be applied to the
#    two-by-two table using this simple piece of code.

fisher.test( tab, alternative = 'greater' )

# Here we can see that the p-value is what we previously calculated.





# Chi-Squared Tests
# Note that, in a way, our funding rates case study is similar to the lady tasting
#    tea example.  However, in the tasting tea example, the number of blue and
#    red beads is experimentally fixed.  And the number of answers given for each
#    category is also fixed.
# This is because Fischer made sure there were 4 before tea and 4 after tea.
#    And the lady knew this so the answer is would also have 4 and 4.  If this
#    is the case, the sums of the rows and the sum of the columns of the 2 by 2
#    table are fixed.  This defines a constraint on the possible ways we can fill
#    the 2 by 2 table and also permits us to use the hypergeometric distribution.
# In general, this is not the case.  Nonetheless, there's another approach that's
#    very similar, the chi-squared test, which we will now describe.  Imagine we
#    have 2,823 individuals.  Some are men, and some are women.  Some get funded;
#    others don't.  There you have two binary variables. We saw that the success
#    rate for men and women were the following: about 18% for men, about 15% for
#    women.  We can compute the overall funding rate using the following code.

funding_rate <- totals %>%
    summarize( percent_total =
                   ( yes_men + yes_women ) / ( yes_men + no_men + yes_women + no_women ) ) %>%
    .$percent_total
funding_rate

#    It's between 16% and 17%.
# So now the question is will we see a difference between men and women as big as
#    the one we see if funding was assigned at random using this rate?
#     The chi-squared test answers this question.
# The first step is to create a 2 by 2 table just like before.
# In our case, we can use this code and construct the following 2 by 2 table for
#    the research funding data.

two_by_two <- tibble( awarded = c( 'no', 'yes' ),
                      men = c( totals$no_men, totals$yes_men ),
                      women = c( totals$no_women, totals$yes_women ) )
two_by_two

# The general idea of a chi-squared test is to compare this 2 by 2 table, the
#    observed 2 by 2 table, to what you expect to see at the overall funding rate,
#    which we can compute using this code.   And here is the table.

tibble( awarded = c( 'no', 'yes' ),
        men = ( totals$no_men + totals$yes_men ) * c( 1 - funding_rate, funding_rate ),
        women = ( totals$no_women + totals$yes_women ) * c( 1 - funding_rate, funding_rate ) )

# We can see that more men than expected and less women than expected received
#    funding.  However, under the null hypothesis, this observation is a random
#    variable.  The chi-squared test tells us how likely it is to see a deviation
#    like this, or larger, by chance.
# This test uses an asymptotic result, similar to the central limit theorem,
#    related to the sums of independent binary outcomes in a context like this.
# The R function chisq.test takes a 2 by 2 table and returns the results from
#    this test.  Here's the simple code.

two_by_two %>%
    select( -awarded ) %>%
    chisq.test()

# We see that the p-value is 0.051. this means that the probability of seeing a
#    deviation like the one we see or bigger under the null that funding is assigned
#    at random is 0.051.  All right.  So we described how to obtain p-values.
#    But now let's talk about summary statistics.  An informative summary statistic
#    associated with 2 by 2 tables is the odds ratio.
# Define the two variables X=1 if you are male or 0 otherwise and Y=1  if you're
#    funded and 0 otherwise.  The odds of getting funded if you're a man is defined
#    as follows and can be computed using the simple code.

# Pr( Y = 1 | X = 1 ) / Pr( Y = 0 | X = 1 )
odds_men <- ( two_by_two$men[ 2 ] / sum( two_by_two$men ) ) / ( two_by_two$men[ 1 ] / sum( two_by_two$men ))
odds_men

#    The odds of being funded if you're a woman is given by this simple formula
#    and can be computed like this.

# Pr( Y = 1 | X = 0 ) / Pr( Y = 0 | X = 0 )
odds_women <- ( two_by_two$women[ 2 ] / sum( two_by_two$women ) ) / ( two_by_two$women[ 1 ] / sum( two_by_two$women ))
odds_women

# The odds ratio is the ratio of these two odds.
# How many times larger are the odds for men than for women?
#    In this case, we get that is 1.23.
# A quick note of caution regarding p-values from 2 by 2 tables.
# As mentioned earlier, reporting only p-values is not an appropriate way to report
#    the results of data analysis.  In scientific journals, for example, some
#    studies seem to overemphasize p-values.  Some of these studies have large
#    sample size and report impressively small p-values.
# Yet when one looks closely at the results, we realize that the odds ratios are
#    quite modest, barely bigger than 1.
# In this case, the difference may not be practically significant or scientifically
#    significant.
# Note that the relationship between odds ratios and p-values is not one to one.
#    It depends on the sample size.  So a very small p-value does not necessarily
#    mean a very large odds ratio.  Look at what happens to the p-value if we
#    multiply our 2 by 2 table by 10.

two_by_two %>%
    select( -awarded ) %>%
    mutate( men = men * 10, women = women * 10 ) %>%
    chisq.test()

# We multiply each cell by 10; the odds ratio remains the same.  But look at how
#    small the p-value becomes.  Earlier we mention that instead of p-values, it's
#    more appropriate to report confidence intervals.  However, computing confidence
#    intervals for odds ratio is not mathematically straightforward.
# Unlike other statistics for which you can derive useful approximations for the
#    distribution, the odds ratio is not only a ratio, but a ratio of ratios.
# Therefore, there's no simple way of using, for example, the central limit
#    theorem.  One approach is to use the theory of generalized linear models,
#    which is too advanced for this course.
# But you can learn more about it in this book: McCullagh and Nelder - 1989.





