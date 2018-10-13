heights %>% ggplot( aes( sex, height ) ) + geom_point()

# Add jitter && Alpha blending
heights %>% ggplot( aes( sex, height ) ) + geom_jitter( alpha = 0.25 )

heights %>% ggplot( aes( sex, height ) ) + geom_boxplot( color = 'darkorange' ) + geom_jitter( alpha = 0.25, color= 'darkblue' )


color_blind_friendly_cols <- c( '#999999', '#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7' )
p1 <- data.frame( x = 1:8, y = 1:8, col = as.character( 1:8 ) ) %>% ggplot( aes( x, y, color = col ) ) + geom_point( size = 5 )
p1 + scale_color_manual( values = color_blind_friendly_cols )
