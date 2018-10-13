as <- heights %>% filter( sex == 'Male' ) %>% ggplot( aes( x =  height ) )
as1 <- as +
    geom_histogram( binwidth = 1, fill = 'blue', col = 'black' )
as2 <- as +
    geom_histogram( binwidth = 2, fill = 'blue', col = 'black' )
as3 <- as +
    geom_histogram( binwidth = 3, fill = 'blue', col = 'black' )

grid.arrange( as1, as2, as3, ncol = 3 )

# /----------------------/

# To make a histrogram
# Filter thehegihts to only include the males
q <- heights %>% filter( sex == 'Male' ) %>% ggplot( aes ( x = height ) ) # Decide what geometry we need

q + geom_histogram( binwidth = 1, fill = 'blue', col = 'black' ) +
    xlab( 'Make heights in inches' ) +
    ggtitle( 'Histogram' )
q + geom_density( fill = 'blue' )


y <- heights %>% filter( sex == 'Male' ) %>% ggplot( aes( sample = scale( height ) ) ) +
    geom_qq() +
    geom_abline()
y


params <- heights %>% filter( sex == 'Male' ) %>% summarize( mean = mean( height ), sd = sd( height ) )

y <- heights %>% filter( sex == 'Male' ) %>% ggplot( aes( sample = height ) ) +
    geom_qq( dparams = params ) +
    geom_abline()

# /----------------------/

# Define the slope of the line
r <- murders %>% summarize( rate = sum( total ) / sum( population ) * 10^ 6 ) %>% .$rate

# Make the plot
args( ggplot )
p <- murders %>% ggplot( aes( population/10^6, total, label = abb ) )
p <- p +
    geom_abline( intercept = log10( r ), lty = 2, color = 'darkgrey' ) +
    geom_point( aes( col = region ), size = 3 ) +
    geom_text_repel( ) +
    scale_x_log10() +
    scale_y_log10() +
    xlab( 'Populations in millions (log scale)' ) +
    ylab( 'Total number of murders (log scale)' ) +
    ggtitle( 'US Gun Murders in US 2010' ) +
    scale_color_discrete( name = 'Region' )

p + theme_economist()

# /----------------------/

r <- murders %>% summarize( rate = sum( total ) / sum( population ) * 10^ 6 ) %>% .$rate

args( ggplot )
p <- murders %>% ggplot( aes( population/10^6, total, label = abb ) )
p <- p +
    scale_color_discrete( name = 'Region' ) +
    geom_abline( intercept = log10( r ), lty = 2, color = 'darkgrey' ) +
    geom_point( aes( col = region ), size = 3 ) +
    geom_text( nudge_x = 0.075 ) +
    scale_x_log10() +
    scale_y_log10() +
    xlab( 'Populations in millions (log scale)' ) +
    ylab( 'Total number of murders (log scale)' ) +
    ggtitle( 'US Gun Murders in US 2010' )

p + theme_economist()

# /----------------------/

r <- murders %>% summarize( rate = sum( total ) / sum( population ) * 10^ 6 ) %>% .$rate

args( ggplot )
p <- murders %>% ggplot( aes( population/10^6, total, label = abb ) )
p <- p +
    scale_color_discrete( name = 'Region' ) +
    geom_abline( intercept = log10( r ), lty = 2, color = 'darkgrey' ) +
    geom_point( aes( col = region ), size = 3 ) +
    geom_text( nudge_x = 0.075 ) +
    scale_x_log10() +
    scale_y_log10() +
    xlab( 'Populations in millions (log scale)' ) +
    ylab( 'Total number of murders (log scale)' ) +
    ggtitle( 'US Gun Murders in US 2010' )
p


# /----------------------/

args( ggplot )
p <- murders %>% ggplot( aes( population/10^6, total, label = abb ) )
p + geom_point( size = 3 ) +
    geom_text( nudge_x = 0.075 ) +
    scale_x_log10() +
    scale_y_log10() +
    xlab( 'Populations in millions (log scale)' ) +
    ylab( 'Total number of murders (log scale)' ) +
    ggtitle( 'US Gun Murders in US 2010' )

# /----------------------/

args( ggplot )
p <- murders %>% ggplot( aes( population/10^6, total, label = abb ) )
p + geom_point( size = 3 ) +
    geom_text( nudge_x = 0.075 ) +
    scale_x_continuous( trans = 'log10' ) +
    scale_y_continuous( trans = 'log10' )

# /----------------------/

q <- murders %>% ggplot()
q   + geom_point( aes( x =  population / 10^6, y = total ), size = 3 ) +  # geometry
    geom_text( aes( population/10^6, total, label = abb ), nudge_x = 1 )   # details

# /----------------------/

p <- ggplot( data = murders )

