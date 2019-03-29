setwd("~/Dropbox/training_courses/R/HarvardX_PH125.5x_ProductivityTools/murder")
library( tidyverse )

murders %>% mutate( abb = reorder( abb, rate ) ) %>%
    ggplot( aes( abb, rate ) ) +
    geom_bar( width = 0.5, stat = 'identity', color = 'black ' ) +
    coord_flip()
ggsave( 'figs/barplot.png' )
