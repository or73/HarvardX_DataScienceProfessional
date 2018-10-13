# Chi-Squated Test Exercises


# Exercise 1 - Comparing Proportions of Hits
# In a previous exercise, we determined whether or not each poll predicted the
#    correct winner for their state in the 2016 U.S. presidential election.
#    Each poll was also assigned a grade by the poll aggregator. Now we're going
#    to determine if polls rated A- made better predictions than polls rated C-.
#
# In this exercise, filter the errors data for just polls with grades A- and C-.
#    Calculate the proportion of times each grade of poll predicted the correct
#    winner.
#
# INSTRUCTIONS
# Filter errors for grades A- and C-.
# Group the data by grade and hit.
# Summarize the number of hits for each grade.
# Generate a two-by-two table containing the number of hits and misses for each
#    grade.
# Calculate the proportion of times each grade was correct.

# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors, 1)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors%>%
    filter( grade %in% c( 'A-', 'C-' ) ) %>%
    group_by( grade, hit ) %>%
    summarize( num = n() ) %>%
    spread( grade, num )
totals

# Print the proportion of hits for grade A- polls to the console
totals[[ 2, 3 ]] / sum( totals[ 3 ] )

# Print the proportion of hits for grade C- polls to the console
totals[[ 2, 2,]] / sum( totals[ 2 ] )





# Exercise 2 - Chi-squared Test
# We found that the A- polls predicted the correct winner about 86% of the time
#    in their states and C- polls predicted the correct winner about 80% of the
#    time.
#
# Use a chi-squared test to determine if these proportions are different.
#
# INSTRUCTIONS
# 100 XP
# Use the chisq.test function to perform the chi-squared test. Save the results
#    to an object called chisq_test.
# Print the p-value of the test to the console.
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% select( -hit ) %>% chisq.test()

# Print the p-value of the chi-squared test to the console
chisq_test$p.value




# Exercise 3 - Odds Ratio Calculation
# It doesn't look like the grade A- polls performed significantly differently
#    than the grade C- polls in their states.
#
# Calculate the odds ratio to determine the magnitude of the difference in
#    performance between these two grades of polls.
#
# INSTRUCTIONS
# Calculate the odds that a grade C- poll predicts the correct winner. Save this
#    result to a variable called odds_C.
# Calculate the odds that a grade A- poll predicts the correct winner. Save this
#    result to a variable called odds_A.
# Calculate the odds ratio that tells us how many times larger the odds of a grade
#    A- poll is at predicting the winner than a grade C- poll.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals, 1)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- totals$'C-'[[ 2 ]] / totals$'C-'[[ 1 ]]
odds_C

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- totals$'A-'[[ 2 ]] / totals$'A-'[[ 1 ]]
odds_A
# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A / odds_C




# Exercise 4 - Significance
# We did not find meaningful differences between the poll results from grade A-
#    and grade C- polls in this subset of the data, which only contains polls for
#    about a week before the election. Imagine we expanded our analysis to include
#    all election polls and we repeat our analysis. In this hypothetical scenario,
#    we get that the p-value for the difference in prediction success if 0.0015
#    and the odds ratio describing the effect size of the performance of grade A-
#    over grade B- polls is 1.07.
#
# Based on what we learned in the last section, which statement reflects the best
#    interpretation of this result?
#

# INSTRUCTIONS
# Possible Answers
# The p-value is below 0.05, so there is a significant difference. Grade A- polls
#    are significantly better at predicting winners.
# The p-value is too close to 0.05 to call this a significant difference. We do
#    not observe a difference in performance.
# The p-value is below 0.05, but the odds ratio is very close to 1. There is not
#    a scientifically significant difference in performance.   <-*
# The p-value is below 0.05 and the odds ratio indicates that grade A- polls
#    perform significantly better than grade C- polls.





