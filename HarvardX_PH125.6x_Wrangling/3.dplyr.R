library( tidyverse )
library( dslabs )
data("heights")
data("murders")
names( heights )
names(murders)

# /----------------------/
# Sorting Data Tables

# Order the states by their population size
murders %>% arrange( population ) %>% head()

# Order the states by their murder rate
murders %>% arrange( murder_rate ) %>% head()

# Sort murder rate in descending order
murders %>% arrange( desc( murder_rate ) ) %>% head()

# Order by region
murders %>% arrange( region, murder_rate ) %>% head()

# show the 10 states with the highest murder rate
murders %>% top_n( 10, murder_rate )

# order and show the 10 states with the highest murder rate
murders %>% arrange( desc( murder_rate ) ) %>% top_n( 10 )

# /----------------------/
# Group By
heights %>% group_by( sex )

heights %>% group_by( sex ) %>%
    summarize( average = mean( height ), standard_deviation = sd( height ) )


murders %>% group_by( region ) %>%
    summarize( median_rate = median( murder_rate ) )

# /----------------------/
# Return a vector as opposed to data frames
murders <- murders %>% mutate( murder_rate = total/population*100000 )
summarize( murders, mean( murder_rate ) )


# /----------------------/
#
# Data table include total murders and oppulation, size for each state
#   and add a murder rate column
#   Note that the US murder rate is not the average of the state murder rates
murders <- murders %>% mutate( murder_rate = total/population*100000)
summarize( murders, mean( murder_rate ) )
# The wrong result is because in this computation we're counting the small
#    states just the same as the large states, and when we compute the
#    average US murder rate, it needs to take into account bigger states
#    more than samller states

# Correct computation
us_murder_rate <- murders %>% summarize( rate = sum( total ) / sum( population ) * 100000 ) %>% .$rate
us_murder_rate

# to extract numeric value from a data set
us_murder_rate %>% .$rate

us_murder_rate <- murders %>%
    summarize( rate = sum( total ) / sum( population ) * 100000 ) %>%
    us

# /----------------------/

# quantiles - WRONG - ERROR
heights %>% filter( sex == 'Male' ) %>%
    summarize( range = quantile( height, c( 0, 0.5, 1 ) ) )

# /----------------------/

# Computes the median, the mean, the min, and the max
heights %>% filter( sex == 'Male' ) %>%
    summarize( median  = median( height ),
               minimum = min( height ),
               maximum = max( height) )

# /----------------------/

s <- heights %>% filter( sex == 'Male' ) %>%
    summarize( average = mean( height ), standard_deviation = sd( height ) )
s
