# 1. Is it fair to say the world is divided into rich and poor?
#    Is it a fair characterization of today's world to say that is divided
#      into a Western rich nations and the developing world in Agrica, Asia,
#      and Latin America?
# 2. Has income inequality across countries sorsened during the last 40 years?

# GapMinder data
library( dslabs )
data( "gapminder" )
head( gapminder )

# For each of the pairs of countries here, which country do you think had the
#    highest child mortality in 2015?  And also, whic pairs do you think are most
#    simmilar?
#    1. Sri Lanka or Turkey
#    2. Poland or South Korea
#    3. Malaysia or Russia
#    4. Pakistan or Vietnam
#    5. Thailand or South Africa

gapminder %>% filter( year == 2015 & country %in% c( 'Sri Lanka', 'Turkey' ) ) %>%
    select( country, infant_mortality )
# Sri Lanka   8.4
# Turkey     11.6

gapminder %>% filter( year == 2015 & country %in% c( 'Poland', 'South Korea' ) ) %>%
    select( country, infant_mortality )
# South Korea   2.9
# Poland        4.5

gapminder %>% filter( year == 2015 & country %in% c( 'Malaysia', 'Russia' ) ) %>%
    select( country, infant_mortality )
# Malaysia   6.0
# Russia     8.2

gapminder %>% filter( year == 2015 & country %in% c( 'Pakistan', 'Vietnam' ) ) %>%
    select( country, infant_mortality )
# Pakistan   65.8
# Vietnam    17.3

gapminder %>% filter( year == 2015 & country %in% c( 'Thailand', 'South Africa' ) ) %>%
    select( country, infant_mortality )
# South Africa   33.6
# Thailand       10.5


# 1. Is it fair to say the world is divided into rich and poor?
#    Is it a fair characterization of today's world to say that is divided
#      into a Western rich nations and the developing world in Agrica, Asia,
#      and Latin America?

# Scatterplot of life expectancy versus fertility rates
#   Fertility rates are defined as the average number of children per woman
#   We will start by looking at data from about 50 years ago when, perhaps,
#     this worldview was cemented in our minds
ds_theme_set()
filter( gapminder, year == 1962 ) %>%
    ggplot( aes( fertility, life_expectancy, color = continent ) ) +
    geom_point()

# Comparisson between data of 1962 and 2012
# We stratify the data by some variable (continent and year) and make the same
# plot for each strata
filter( gapminder, year %in% c( 1962, 2012 ) ) %>%
    ggplot( aes( fertility, life_expectancy, col = continent ) ) +
    geom_point() +
    facet_grid( continent~year )

filter( gapminder, year %in% c( 1962, 2012 ) ) %>%
    ggplot( aes( fertility, life_expectancy, col = continent ) ) +
    geom_point() +
    facet_grid( .~year )

# Plot data for 1962, 1980, 1990, 2000, 2012
years <- c( 1962, 1980, 1990, 2000, 2012 )
continents <- c( 'Europe', 'Asia' )

gapminder %>%
    filter( year %in% years & continent %in% continents ) %>%
    ggplot( aes( fertility, life_expectancy, col = continent ) ) +
    geom_point() +
    facet_wrap( ~year )

# Which countries are improving more? which countries are improving less?
# Was the improvement constant during the last 50 years or was there more of an
#    acceleraion during a specific certain period?

# Time Series Plot
#    Have time in the x-axis, and an outcome, or measurement of interest,
#       on the y-axis
countries <- c( 'Colombia', 'Germany', 'South Korea', 'United States', 'Venezuela')
gapminder %>%
    filter( country %in% countries ) %>%
    ggplot( aes( year, fertility, col = country ) ) +
    geom_point() +
    geom_line()



countries = c( 'Germany', 'South Korea' )
labels <- data.frame( country = countries, x = c( 1975, 1965 ), y = c( 60, 72 ) )

gapminder %>%
    filter( country %in% countries ) %>%
    ggplot( aes( year, life_expectancy, col = country ) ) +
    geom_line() +
    geom_text( data = labels, aes( x, y, label = country ), size = 5 ) +
    theme( legend.position = 'none' )


# Transformations
# GDP - Gross Domestic Product
# GDP measures the market value of goods and services produced by a country per year
# The GDP per person is often used as a rough summary of how rich a country is
# Dividing GDP by 365 we can obtain a measure of dollars per day, a person surviving
#    on an income of less than $2 a day, is defined to be living in absolute poverty
gapminder <- gapminder %>%
    mutate( dollars_per_day = gdp/population/365 )

head( gapminder )

past_year <- 1970
gapminder %>%
    filter( year == past_year & !is.na( gdp ) ) %>%
    ggplot( aes( dollars_per_day ) ) +
    geom_histogram( binwidth = 1, color = 'black' )

head( gapminder )

# log transformation
gapminder %>%
    filter( year == past_year & !is.na( gdp ) ) %>%
    ggplot( aes( log2( dollars_per_day ) ) ) +
    geom_histogram( binwidth = 1, color = 'black' )

head(gapminder)
gapminder %>%
    filter( year == past_year & !is.na( gdp ) ) %>%
    ggplot( aes( dollars_per_day ) ) +
    geom_histogram( binwidth = 1, color = 'black' ) +
    scale_x_continuous( trans = 'log2' )
head(gapminder)
# Stratify and Boxplot
# See distribution by geographical region
length( levels( gapminder$region ) )   # Number of regions

# boxplot
p <- gapminder %>%
    filter( year == past_year & !is.na( gdp ) ) %>%
    ggplot( aes( region, dollars_per_day ) )

p + geom_boxplot() +
    theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )
head( p )
# reorder regions by their median income level
p <- gapminder %>%
    filter( year == past_year & !is.na( gdp ) ) %>%
    mutate( region = reorder( region, dollars_per_day, FUN = median ) ) %>%   # reorder regions by their median income level
    ggplot( aes( region, dollars_per_day, fill = continent ) ) +
    geom_boxplot() +
    theme( axis.text.x = element_text( angle = 90, hjust = 1 ) ) +  # rotate labels 90 degrees
    xlab( '' ) +
    scale_y_continuous( trans = 'log2' ) +   # Change the scale to the log scale
    geom_point( show.legend = FALSE )   # Show points
p

head( p )

# example about how to use reorder function
fac <- factor( c( 'Asia', 'Asia', 'West', 'West', 'West' ) )
levels( fac )   # Default order function of R

# To order fac according to an associated number
value <- c( 10, 11, 12, 6, 4 )
fac <- reorder( fac, value, FUN = mean )
levels( fac )

# Comparing distributions
# Define a vector that defines the regions in the West
west <- c( 'Western Europe', 'Northern Europe', 'Southern Europe', 'Northern America', 'Australia and New Zealand' )
# Compare the differences in distribution across time
gapminder %>%
    filter( year == past_year & !is.na( gdp ) ) %>%
    mutate( group = ifelse( region %in% west, 'West', 'Developing' ) ) %>% # create two groups 'West' and 'Developing' to make a comparisson between these groups to identify the modals showed in 1970 graph
    ggplot( aes( dollars_per_day ) ) +
    geom_histogram( binwidth = 1, color = 'black' ) +
    scale_x_continuous( trans = 'log2' ) +
    facet_grid( .~group )

head( gapminder )
head( p )
# faceting by region and year
# To see if separation is worse today than it was 40 years ago
past_year <- 1970
present_year <- 2010
gapminder %>%
    filter( year %in% c( past_year, present_year ) & !is.na( gdp ) ) %>%
    mutate( group = ifelse( region %in% west, 'West', 'Developing' ) ) %>%
    ggplot( aes( dollars_per_day ) ) +
    geom_histogram( binwidth = 1, color = 'black' ) +
    scale_x_continuous( trans = 'log2' ) +
    facet_grid( year ~ group )

head( gapminder )
head( p )
# remake the plot but using only countries with data available for both years
country_list_1 <- gapminder %>%
    filter( year == past_year & !is.na( dollars_per_day ) ) %>%
    .$country

country_list_2 <- gapminder %>%
    filter( year == present_year & !is.na( dollars_per_day ) ) %>%
    .$country
country_list <- intersect( country_list_1, country_list_2 )

head( country_list_1 )
head( country_list_2 )
head( country_list )
gapminder %>%
    filter( year %in% c( past_year, present_year ) & country %in% country_list ) %>%
    mutate( group = ifelse( region %in% west, 'West', 'Developing' ) ) %>%
    ggplot( aes( dollars_per_day ) ) +
    geom_histogram( binwidth = 1, color = 'black' ) +
    scale_x_continuous( trans = 'log2' ) +
    facet_grid( year ~ group )

head( gapminder )
head( p )
# Remake the box plots, but adding 2010
p <- gapminder %>%
    filter( year %in% c( past_year, present_year ) & country %in% country_list ) %>%
    mutate( region = reorder( region, dollars_per_day, FUN = median ) )  %>%
    ggplot() +
    theme( axis.text.x = element_text( angle = 90, hjust = 1 ) ) +
    xlab( '' ) +
    scale_y_continuous( trans = 'log2' )
p + geom_boxplot( aes( region, dollars_per_day, fill = continent ) ) +
    facet_grid( year~. )
head( p )
# put each together
p + geom_boxplot( aes( region, dollars_per_day, fill = factor( year ) ) )



# Density plots
# Groups and size
gapminder %>%
    filter( year == past_year & country %in% country_list ) %>%
    mutate( group = ifelse( region %in% west, 'West', 'Developing' ) ) %>%
    group_by( group ) %>%
    summarize( n = n() ) %>%
    knitr::kable()
head( gapminder )
# To have the areas of the desities be proportional to the size of the groups,
#    we multiply the y-axis values by the size of the group
p <- gapminder %>%
    filter( year %in% c( past_year, present_year ) & country %in% country_list ) %>%
    mutate( group = ifelse( region %in% west, 'West', 'Developing' ) ) %>%
    ggplot( aes( dollars_per_day, y = ..count.., fill = group ) ) +
    scale_x_continuous( trans = 'log2' )
p + geom_density( alpha = 0.2, bw = 0.75 ) +
    facet_grid( year ~. )


head( gapminder )
head( p )
# GapMinder data
library( dslabs )
data( "gapminder" )
head( gapminder )
# Define a vector that defines the regions in the West
west <- c( 'Western Europe', 'Northern Europe', 'Southern Europe', 'Northern America', 'Australia and New Zealand' )
# show key regions separately
gapminder <- gapminder %>%
    mutate( group = case_when(
        .$region %in% west ~ 'West',
        .$region %in% c( 'Eastern Asia', 'South-Eastern Asia' ) ~ 'East Asia',
        .$region %in% c( 'Caribbean', 'Central America', 'South America' ) ~ 'Latin America',
        .$continent == 'Africa' & .$region != 'Northern Africa' ~ 'Sub-Saharan Africa', TRUE ~ 'Others'
    ) )
# turn previous variable into a factor, to control the order of the levels
gapminder <- gapminder %>%
    mutate( group = factor( group, levels = c( 'Others', 'Latin America', 'East Asia', 'Sub-Saharan Africa', 'West' ) ) )

gapminder <- gapminder %>%
    mutate( dollars_per_day = gdp/population/365 )

head( gapminder )

# Plot the density for each one, use color and size to clearly see the top
p <- gapminder %>%
    filter( year %in% c( past_year, present_year ) & country %in% country_list ) %>%
    ggplot( aes( dollars_per_day, y = ..count.., fill = group ) ) +
    scale_x_continuous( trans = 'log2' )

p + geom_density( alpha = 0.2, bw = 0.75, position = 'stack' ) +
    facet_grid( year ~ . )


p <- gapminder %>%
    filter( year %in% c( past_year, present_year ) & country %in% country_list ) %>%
    group_by( year ) %>%
    mutate( weight = population/sum( population ) * 2 ) %>%
    ungroup() %>%
    ggplot( aes( dollars_per_day, fill = group, weight = weight ) ) +
    scale_x_continuous( trans = 'log2' ) +
    geom_density( alpha = 0.2, bw = 0.75, position = 'stack' ) +
    facet_grid( year ~ . )
p


# Focus on the relationship between contry child survival rates Vs average income
# Compare quatities across regions
gapminder <- gapminder %>%
    mutate( group = case_when(
        .$region %in% west ~ 'The West',
        .$region %in% 'Northern Africa' ~ 'Northern Africa',
        .$region %in% c( 'Eastern Asia', 'South-Eastern Asia' ) ~ 'East Asia',
        .$region == 'Southern Asia' ~ 'Southern Asia',
        .$region %in% c( 'Central America', 'South America', 'Caribbean' ) ~ 'Latin America',
        .$continent == 'Africa' & .$region != 'Northern Africa' ~ 'Sub-Saharan Africa',
        .$region %in% c( 'Melanesia', 'Micronesia', 'Polynesia' ) ~ 'Pacific Islands'
        ) )
# Compute the quantites the we're interested in form each region
surv_income <- gapminder %>%
    filter( year %in% present_year & !is.na( gdp ) & !is.na( infant_mortality ) & !is.na( group ) ) %>%
    group_by( group ) %>%
    summarize( income = sum( gdp )/sum( population )/365, infant_survival_rate = 1 - sum( infant_mortality/1000 * population )/sum( population ) )

surv_income %>% arrange( income )


surv_income %>%
    ggplot( aes( income, infant_survival_rate, label = group, color = group ) ) +
    scale_x_continuous( trans = 'log2', limit = c( 0.25, 150 ) ) +
    scale_y_continuous( trans = 'logit', limit = c( 0.875, .9981 ),
                        breaks = c( .85, .90, .95, .99, .995, .998 ) ) +
    geom_label( size = 3, show.legend = FALSE)



