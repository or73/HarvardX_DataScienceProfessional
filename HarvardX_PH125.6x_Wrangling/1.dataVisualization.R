west <- c( 'Western Europe', 'Northern Europe', 'Southern Europe', 'Northern America', 'Australia and New Zealand' )
countries2 <- c( 'United Kingdom', 'Portugal' )

dat <- gapminder %>%
    filter( year %in% c( 2010, 2015 ) & region %in% west & !is.na( life_expectancy ) & population > 10^7 )

head( dat )

dat %>%
    mutate( location = ifelse( year == 2010, 1, 2 ),
            location = ifelse( year == 2015 & country %in% countries2, location + 0.22, location ),
            hjust = ifelse( year == 2010, 1, 0 ) ) %>%
    mutate( year = as.factor( year ) ) %>%
    ggplot( aes( year, life_expectancy, group = country ) ) +
    geom_line( aes( color = country ), show.legend = FALSE ) +
    geom_text( aes( x = location, label = country, hjust = hjust, check_overlap = TRUE, color = country ),
               show.legend = FALSE ) +
    xlab( '' ) +
    ylab( 'Life Expectancy' )

# Vaccines case study
# Data related to the impact of vaccines
data( "us_contagious_diseases" )
str( us_contagious_diseases )
states3 <- c( 'Hawaii', 'Alaska' )
# data object contains all measles data
#    it includes a per 100.000 rate, orders states by average value of disease, and removes Alaska and Hawaii, since they only vecame states in the late 50s
the_disease <- 'Measles'
dat <- us_contagious_diseases %>%
    filter( !state %in% states3 & disease == the_disease ) %>%
    mutate( rate = count / population * 10000 ) %>%
    mutate( state = reorder( state, rate ) )
# plot disease rates for per year
dat %>% filter( state == 'California' ) %>%
    ggplot( aes( year, rate ) ) +
    geom_line() +
    ylab( 'Cases per 10.000' ) +
    geom_vline( xintercept = 1963, col = 'blue' )
class( dat )
head( dat )
# show all states in one graph
library( RColorBrewer )

#display.brewer.all( type = 'seq' )   # sequential colors
#display.brewer.all( type = 'div' )   # divergent colors
dat %>% ggplot( aes( year, state, fill = rate ) ) +
    geom_tile( color = 'grey50') +
    scale_x_continuous( expand = c( 0,0 ) ) +
    scale_fill_gradientn( colors = brewer.pal( 9, 'Reds' ), trans = 'sqrt') +
    geom_vline( xintercept = 1963, col = 'blue' ) +
    theme_minimal() +
    theme( panel.grid = element_blank() ) +
    ggtitle( the_disease ) +
    ylab( '' ) +
    xlab( '' )

head( us_contagious_diseases )

avg <- us_contagious_diseases %>%
    filter( !state %in% states3 & disease == the_disease ) %>%
    #filter( disease == the_disease ) %>%
    group_by( year ) %>%
    summarize( us_rate = sum( count, na.rm = TRUE ) / sum( population, na.rm =  TRUE ) * 10000 )

avg %>% ggplot( aes( year, us_rate ) ) +
    geom_line( color = 'black' )

dat3 <-  us_contagious_diseases %>%
    filter( !state %in% states3 & disease == the_disease ) %>%
    mutate( rate = count / population * 10000 ) %>%
    mutate( state = reorder( state, rate ) )



# libraries
library( tidyverse )
library( dslabs )
data( "gapminder" )
args( ggplot )
library( RColorBrewer )
data( "us_contagious_diseases" )
states3 <- c( 'Hawaii', 'Alaska' )
the_disease <- 'Measles'
greyColors <- c( '#dcdcdc', '#d3d3d3', '#c0c0c0', '#a9a9a9', '#808080', '#696969', '#778899', '#708090', '#2f4f4f' )



head(us_contagious_diseases)
avg <- us_contagious_diseases %>%
    filter( !state %in% states3 & disease == the_disease ) %>%
    mutate( us_rate = sum( count, na.rm = TRUE ) / sum( population, na.rm =  TRUE ) * 10000 ) %>%
    group_by( year ) %>%
    summarize( us_rate = sum( count, na.rm = TRUE ) / sum( population, na.rm =  TRUE ) * 10000 )
avg

dat3 <-  us_contagious_diseases %>%
    filter( !state %in% states3 & disease == the_disease ) %>%
    mutate( rate = count / population * 10000 ) %>%
    mutate( state = reorder( state, rate ) )

head(avg)
head(dat3)
class( dat3 )
class( avg )
ceiling( runif( 1, 1, 9 ) )
###
###   Linel graph
###
dat4 <- ggplot() +
    # dat3 plot
    geom_line( data = dat3, aes( year, rate, group = state ), color = 'darkgray', alpha = 0.75 ) +
    # avg plot
    geom_line( data = avg, aes( year, us_rate ), color = 'black', size = 1.15 ) +
    scale_y_continuous( trans = 'sqrt', limits = c( 0, 300), breaks = c( 5, 25, 125, 300 ) ) +
    geom_vline( xintercept = 1963, col = 'blue' ) +
    ylab( '' ) +
    xlab( '' )

dat4

###
### SMOTTH GRAPH
###
dat5 <- ggplot() +
    # dat3 plot
    geom_smooth( data = dat3, aes( year, rate, group = state ), color = 'gray' ) +
    # avg plot
    geom_smooth( data = avg, aes( year, us_rate ), color = 'black' ) +
    scale_y_continuous( breaks = c( 5, 25, 125, 300 ) ) +
    geom_vline( xintercept = 1963, col = 'blue' ) +
    ylab( '' ) +
    xlab( '' )
dat5


# plot disease rates for per year
dat3 %>% ggplot( aes( year, rate, group = state ) ) +
    #scale_y_continuous( trans = 'logit', limit = c( 0.1, 300 ),
    #                    breaks = c( 5, 25, 125, 300 ) ) +
    scale_y_continuous( breaks = c( 5, 25, 125, 300 ) ) +
    geom_line( color = 'grey' ) +
    geom_vline( xintercept = 1963, col = 'blue' ) +
    ylab( '' ) +
    xlab( '' )
