# ****************
# FUNCTIONS
# ****************

# -------------------------------------------------- GRAPHS
# graphs
graph.geom.bar <- function(data.set, title = NULL, x.title = NULL, y.title = NULL, target.distribution) {
    data.set %>%
        ggplot(aes(x = target)) +
        geom_bar(position = 'stack', alpha = 0.5) +
        geom_text(data = target.distribution, aes(x = Var1, y = '', label = Freq),
                  vjust = -25,
                  color = 'darkblue') +
        ggtitle(title) +
        xlab(x.title) +
        ylab(y.title) +
        theme(axis.line = element_line(color = 'darkblue',
                                       size = 1,
                                       linetype = 'solid'),
              axis.title = element_text(color = 'steelblue4',
                                        size = 10),
              axis.text = element_text(color = 'magenta',
                                       size = 9),
              plot.title = element_text(hjust = 0.5,
                                        size = 12,
                                        face = 'bold'))
}


graph.target.geom.bar <- function(data.set = NULL, var1 = NULL, title = NULL, sub.title = NULL, x.title = NULL, y.title = NULL, variable.type = NULL, x.range = NULL, y.range = NULL) {
    if (variable.type == 'categorical') {
        return(data.set %>%
                   ggplot(aes_string(x = var1,
                                     y = 'n',
                                     fill = 'target')) +
                   geom_bar(position = 'stack', stat = 'identity') +
                   ggtitle(title,
                           subtitle = sub.title) +
                   xlab(x.title) +
                   ylab(y.title) +
                   theme( axis.line = element_line(color = 'darkblue',
                                                   size = 1,
                                                   linetype = 'solid'),
                          axis.title = element_text(color = 'steelblue4',
                                                    size = 10),
                          axis.text = element_text(color = 'magenta',
                                                   size = 9),
                          plot.title = element_text(hjust = 0.5,
                                                    size = 12,
                                                    face = 'bold'),
                          plot.subtitle = element_text(hjust = 0.5,
                                                       size = 11,
                                                       face = 'bold')))
    } else {
        x.min.break <- x.range[1] # min(data.set$numeric)
        x.max.break <- x.range[2] #max(data.set$numeric)
        x.by.break <- round(x.max.break/10)
        x.breaks <- seq(x.min.break, x.max.break, by = x.by.break)
        y.min.break <- y.range[1] # min(data.set$n)
        y.max.break <- y.range[2] # max(data.set$n)
        y.by.break <- round(y.max.break/10)
        y.breaks <- seq(y.min.break, y.max.break, by = y.by.break)
        return(data.set %>%
                   ggplot(aes(x = numeric,
                              y = n,
                              fill = target)) +
                   geom_bar(stat = 'identity',
                            color = 'steelblue4',
                            size = 0.25) +
                   scale_x_continuous(limits = c(x.min.break, x.max.break),
                                       breaks = x.breaks) +
                   scale_y_continuous(limits = c(y.min.break, y.max.break),
                                      breaks = y.breaks) +
                   ggtitle(title,
                           subtitle = sub.title) +
                   xlab(x.title) +
                   ylab(y.title) +
                   theme( axis.line = element_line(color = 'darkblue',
                                                   size = 1,
                                                   linetype = 'solid'),
                          axis.title = element_text(color = 'steelblue4',
                                                    size = 10),
                          axis.text = element_text(color = 'magenta',
                                                   size = 9),
                          plot.title = element_text(hjust = 0.5,
                                                    size = 12,
                                                    face = 'bold'),
                          plot.subtitle = element_text(hjust = 0.5,
                                                       size = 11,
                                                       face = 'bold')))
    }
}

# -------------------------------------------------- LOAD DATA
load.data <- function() {
    zip.fileName <- 'heart-disease-uci-data.zip'
    cat('Validating if file exists...\n')
    # if (!file.exists('data', 'heart.csv') & file.exists(zip.fileName)) {
    cat('unzipping file...\n')
    unzip(zip.fileName,
          list = TRUE)
    # } else {cat('File already exist...\n')}


    hd.set <- read.csv(file.path('data', 'heart.csv'),
                       header = TRUE)
    cat('Creating training & test sets...\n')
    set.seed(123)
    test.index <- caret::createDataPartition(y = hd.set$age,
                                             times = 1,
                                             p = 0.1,
                                             list = FALSE)
    training.set <- hd.set[-test.index,]
    test.set <- hd.set[test.index,]
    cat('\n\nTraining & Test sets are ready...\n')
}

# Load the Packages
# Note: this process could take a couple of minutes, and load all required libraries for this project
load.packages.list <- function(){

    # Required packages
    packages.list <-  c('anytime',
                        'Boruta',
                        'caret', 'caretEnsemble', 'cluster',
                        'data.table',
                        'earth',
                        'factoextra', 'fit.models', 'fpc', 'funModeling',
                        'ggfortify', 'glmnet', 'gsubfn', 'ggvis',
                        'kableExtra', 'knitr',
                        'magrittr', 'MASS', 'matrixStats',
                        'nnet',
                        'pacman', 'party', 'plyr', 'png',
                        'randomForest', 'rapport', 'rapportools',
                        'RColorBrewer', 'RCurl', 'recosystem',
                        'relaimpo', 'rlang', 'rmarkdown', 'ROCR', 'rpart',
                        'skimr',
                        'tidyverse',
                        'utils')
    # Load required packages/libraries
    new.pkg <- packages.list[!(packages.list %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg,
                         dependencies = TRUE,
                         repos = 'http://cran.us.r-project.org')
    sapply(packages.list,
           require,
           character.only = TRUE)
    cat('All packages have been loaded')
}

# -------------------------------------------------- TABLE
table.basic <- function(data.set = NULL, caption = '', table.size = 'big') {
    if (table.size == 'big') {
        return(data.set %>%
                   knitr::kable(caption = caption,
                                booktabs = T) %>%
                   kable_styling(latex_options = c('striped', 'hold_position', 'scale_down'),
                                 full_width = FALSE,
                                 position = 'center'))
    } else {
        return(data.set %>%
                   knitr::kable(caption = caption,
                                booktabs = T) %>%
                   kable_styling(latex_options = c('striped', 'hold_position'),
                                 full_width = FALSE,
                                 position = 'center'))
    }
}

table.count <- function(value = FALSE, caption = NULL, desc = FALSE, amount = 0, data.set = NULL, data.group1 = NULL, data.group2 = NULL) {
    if (value) {
        grouped.data <- data.set %>%
            dplyr::group_by(!!!rlang::syms(data.group1)) %>%
            dplyr::summarize(count = dplyr::n())
    } else {
        grouped.data <- data.set %>%
            dplyr::group_by(!!!rlang::syms(data.group1),
                            !!!rlang::syms(data.group2)) %>%
            dplyr::summarize(count = dplyr::n())
    }

    if (desc) {
        cat('Descendent order\n')
        return(grouped.data %>%
                   dplyr::arrange(desc(count)) %>%
                   head(n = amount) %>%
                   knitr::kable(caption = caption,
                                booktabs = T) %>%
                   kable_styling(latex_options = c('striped', 'hold_position'),
                                 full_width = FALSE,
                                 position = 'center'))
    }
    cat('Ascendent order\n')
    return(grouped.data %>%
               dplyr::arrange(count) %>%
               head(n = amount) %>%
               knitr::kable(caption = caption,
                            booktabs = T) %>%
               kable_styling(latex_options = c('striped', 'hold_position'),
                             full_width = FALSE,
                             position = 'center'))
}

# -------------------------------------------------- VARIOUS
new.tibble <- function(col.name1 = '', col.name2 = '', rows1 = NULL, rows2 = NULL) {
    # col.name1 <- quo_name(col.name1) #(enquo(col.name1))
    # col.name2 <- quo_name(col.name2) #(enquo(col.name2))
    new.tibble <- tibble(col1 = rows1,
                         col2 = rows2)
    names(new.tibble) <- c(col.name1, col.name2)
    return(new.tibble)
}

# Percent function
percent <- function(x = 0, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# The RMSE function that will be used in this project is:
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

