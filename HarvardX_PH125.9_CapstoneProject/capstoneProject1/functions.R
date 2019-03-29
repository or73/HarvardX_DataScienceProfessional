# ****************
# FUNCTIONS
# ****************

# 2.1.2. Load the MovieLens 10M Dataset

#############################################################
# Create edx set, validation set, and submission file
#############################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# -------------------------------------------------- GRAPHS
# graphs
graph <- function(data.set = NULL, count.data = NULL, title = '', sub.title = '', x.title = '', y.title = '', point.breaks = NULL) {
    # point.breaks <- c(1,10,100,1000, 10000)
    if (length(point.breaks) > 0) {
        return(data.set %>%
                   dplyr::count(!!!rlang::syms(count.data)) %>%
                   ggplot(aes(n))  +
                   geom_histogram(binwidth = 0.15,
                                  aes(fill = ..count..),
                                  color = 'steelblue4',
                                  alpha = 0.85) +
                   geom_freqpoly(binwidth = 0.15,
                                 color = 'red',
                                 size = 1.5,
                                 alpha = 0.85) +
                   scale_x_log10(breaks = point.breaks) +
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
        {
            return(data.set %>%
                       dplyr::count(!!!rlang::syms(count.data)) %>%
                       ggplot(aes(n))  +
                       geom_histogram(binwidth = 0.15,
                                      aes(fill = ..count..),
                                      color = 'steelblue4',
                                      alpha = 0.85) +
                       geom_freqpoly(binwidth = 0.15,
                                     color = 'red',
                                     size = 1.5,
                                     alpha = 0.85) +
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
}

graph.bar.col <- function(data.set = NULL, data1 = NULL, data2 = NULL, graph.title = '', graph.subtitle = '', x.title = '', y.title = '', graph.type = 'bar') {
    if (graph.type == 'bar') {
        cat('Bar Graph')
        names(data.set)
        graph.date.year.ratings <- data.set %>%
            ggplot(aes(x = data1,
                       y = data2)) +
                       # group = 1,
                       # fill = ratings)) +
                   geom_bar(stat = 'identity',
                            width = 0.75,
                            color = 'steelblue4',
                            alpha = 0.5) +
                   geom_point(shape = 16,
                              size = 3,
                              alpha = 0.75) +
                   geom_line(color = 'steelblue4',
                             alpha = 1) +
                   ggtitle(graph.title,
                           subtitle = graph.subtitle) +
                   xlab(x.title) +
                   ylab(y.title) +
                   theme(axis.line = element_line(color = 'darkblue',
                                                   size = 1,
                                                   linetype = 'solid'),
                         axis.title = element_text(color = 'steelblue4',
                                                   size = 10),
                         axis.text = element_text(color = 'magenta'),
                         plot.title = element_text(hjust = 0.5,
                                                   size = 12,
                                                   face = 'bold'),
                         plot.subtitle = element_text(hjust = 0.5,
                                                      size = 11,
                                                      face = 'bold'))
        return(graph.date.year.ratings)
    } else {
        cat('Col Graph')
        return(data.set %>%
                   ggplot(aes(x = data1,
                              y = data2)) +
                   geom_col(aes(fill = genres),
                            position = 'dodge') +
            ggtitle(graph.title,
                    subtitle = graph.subtitle) +
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

graph.bar.col.color <- function(data.set = NULL, data1 = NULL, data2 = NULL, graph.title = '', graph.subtitle = '', x.title = '', y.title = '', graph.type = 'bar', bar1 = NULL, bar2 = NULL, array.size = 0) {

cat('Bar Graph Color')
    color.array <- rep('steelblue4', array.size)
    color.array[bar1] <- '#E74C3C' # orange
    color.array[bar2] <- '#CB4335' # red
graph.date.year.ratings <- data.set %>%
    ggplot(aes(x = data1,
               y = data2,
               group = 1,
               color = color.array,
               fill = color.array)) + #factor(data1))) +
    # scale_color_manual(values = color.array.point) +
    geom_bar(stat = 'identity',
             width = 0.75,
             alpha = 0.5) +
    geom_point(shape = 16,
               size = 3,
               alpha = 0.75) +
    geom_line(color = 'steelblue4',
              alpha = 1) +
    ggtitle(graph.title,
            subtitle = graph.subtitle) +
    xlab(x.title) +
    ylab(y.title) +
    theme(axis.line = element_line(color = 'darkblue',
                                   size = 1,
                                   linetype = 'solid'),
          axis.title = element_text(color = 'steelblue4',
                                    size = 8),
          axis.text = element_text(color = 'magenta',
                                   angle = 90,
                                   hjust = 1),
          plot.title = element_text(hjust = 0.5,
                                    size = 12,
                                    face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5,
                                       size = 11,
                                       face = 'bold'),
          legend.position = 'none')
return(graph.date.year.ratings)
}

graph.mean <- function(data.set = NULL, data = NULL, title = '', sub.title = '', x.title = '', y.title = '') {
    return(data.set %>%
               ggplot(aes(data)) +
               geom_histogram(binwidth = 0.15,
                              aes(fill = ..count..),
                              color = 'steelblue4') +
               geom_freqpoly(binwidth = 0.15,
                             color = 'red',
                             size = 1.5,
                             alpha = 0.85) +
               ggtitle(title,
                       subtitle = sub.title) +
               xlab(x.title) +
               ylab(y.title) +
               theme( axis.line = element_line(color = 'darkblue',
                                               size = 1,
                                               linetype = 'solid'),
                      axis.title = element_text(color = 'steelblue4',
                                                size = 9),
                      axis.text = element_text(color = 'magenta',
                                               size = 10),
                      plot.title = element_text(hjust = 0.5,
                                                size = 12,
                                                face = 'bold'),
                      plot.subtitle = element_text(hjust = 0.5,
                                                   size = 11,
                                                   face = 'bold')))
}

# -------------------------------------------------- LOAD DATA
load.datasets <- function() {
    dl <- tempfile()
    # movies <- matrix()
    # ratings <- data.frame()

    if (file_test('-f', file.path('ml-10M100K', 'movies.dat')) & file_test('-f', file.path('ml-10M100K', 'ratings.dat'))) {
     cat('ml-10M100K directory exists...\n')
     cat('loading movies data...\n')
     movies <- stringr::str_split_fixed(readLines(file.path('ml-10M100K', 'movies.dat')),
                                        '\\::',
                                        3)
     cat('loading ratings data...\n')
     ratings.load.file <- data.table::fread(file.path('ml-10M100K', 'ratings.dat'))
     names(ratings.load.file) <- c('temp')
     ratings <- setnames(splitstackshape::cSplit(ratings.load.file,
                                                 splitCols = 'temp',
                                                 sep = '::'),
                         c('userId',
                           'movieId',
                           'rating',
                           'timestamp'))
     cat('data loaded\n')
    } else {
        cat('downloading file from URL\n')
        # Download file
        download.file('https://files.grouplens.org/datasets/movielens/ml-10m.zip',
                      dl)
        cat('unzipping file...\n')
        # Unzipping file and loading movies dataset data, with colnames = c('movieId', 'title', 'genres')
        movies <- stringr::str_split_fixed(readLines(unzip(dl,
                                                           'ml-10M100K/movies.dat')),
                                           '\\::',
                                           3)
        # Unzipping file and loading ratings dataset data, with colnames = c('userId', 'movieId', 'rating', 'timestamp')
        ratings <- read.table(text = gsub('::',
                                          '\t',
                                          readLines(unzip(dl,
                                                          'ml-10M100K/ratings.dat'))),
                              col.names = c('userId',
                                            'movieId',
                                            'rating',
                                            'timestamp'),
                              allowEscapes = FALSE)
    }
    colnames(movies) <- c('movieId', 'title', 'genres')

    cat('movies -> creating movie data.frame\n')
    # Creating a data frame with movies matrix
    movies <- as.data.frame(movies) %>%
        dplyr::mutate(movieId = as.numeric(levels(movieId))[movieId],
                      title = as.character(title),
                      genres = as.character(genres))

    cat('movies & ratings -> joining datasets\n')
    # Joining Datasets in movielens, by 'movieId'  colnames = 'userId' 'movideId' 'rating' 'timestamp' 'title' 'genres'
    movielens <- dplyr::left_join(ratings,
                                  movies,
                                  by = 'movieId')
    movielens <- na.omit(movielens)
    # 2.1.2. Dataset Structure and General Information
    # Structure of Dataframe
    # structure
    str(movielens)

    # 2.2. Process
    # As mentioned before, the whole data has 10.000.054 ratings, which is split into a 90% training set and 10% tet set (validation).
    cat('Creating edx & temp datasets...\n')
    set.seed(1)
    test_index <- caret::createDataPartition(y = movielens$rating,
                                             times = 1,
                                             p = 0.1,
                                             list = FALSE)
    edx <- movielens[-test_index,]
    temp <- movielens[test_index,]

    cat('Validating validation set...\n')
    # Make sure userId and movieId in validation set are also in edx set
    validation <- temp %>%
        semi_join(edx, by = 'movieId') %>%
        semi_join(edx, by = 'userId')

    cat('Add rows removed from validation set back into edx set...\n')
    # Add rows removed from validation set back into edx set
    removed <- anti_join(temp, validation)
    edx <- rbind(edx, removed)


    # rm(dl, movies, movielens, ratings, removed, temp, test_index)
    rm(dl, movies, ratings, removed, temp, test_index)
    cat('Data Set has been loaded successfully\n')

    # ******************** EDX DATES & GENRES SPLIT - START ************************
    edx <- edx %>%
        dplyr::mutate(dates = as.POSIXct(timestamp,
                                         origin = '1970-01-01'))
    edx <- edx %>%
        dplyr::mutate(date.year = format(as.Date(dates), '%Y'),
                      date.year.month = format(as.Date(dates), '%Y-%m'),
                      date.year.month.day = format(as.Date(dates), '%Y-%m-%d'))

    cat('Splitting genres...\n')
    edx.genres.split <- split.genres(edx)
    # **************************** EDX DATES - END *********************************

    # ************** VALIDATION DATES & GENRES SPLIT - START ***********************
    validation <- validation %>%
        dplyr::mutate(dates = as.POSIXct(timestamp,
                                         origin = '1970-01-01'))
    validation <- validation %>%
        dplyr::mutate(date.year = format(as.Date(dates), '%Y'),
                      date.year.month = format(as.Date(dates), '%Y-%m'),
                      date.year.month.day = format(as.Date(dates), '%Y-%m-%d'))

    cat('Splitting genres...\n')
    validation.genres.split <- split.genres(validation)
    # **************************** EDX DATES - END *********************************


    # Create dir folder if it doesn't exist
    dir.create(file.path('data'),
               showWarnings = FALSE)
    cat('storing edx data...\n')
    # store edx & validation data sets
    write.csv(edx,
              file.path('data', 'edx.csv'),
              na = '',
              row.names = FALSE)
    cat('storing validation data...\n')
    write.csv(validation,
              file.path('data', 'validation.csv'),
              na = '',
              row.names = FALSE)
    cat('storing edx.test data...\n')
    write.csv(edx.genres.split,
              file.path('data', 'edx_test.csv'),
              na = '',
              row.names = FALSE)
    cat('storing validation.test data\n')
    write.csv(validation.genres.split,
              file.path('data', 'validation_test.csv'),
              na = '',
              row.names = FALSE)
    cat('edx & validation data sets have been stored in \'data\' folder\n')
}

# Load the Packages
# Note: this process could take a couple of minutes, and load all required libraries for this project
load.packages.list <- function(pkg = NULL){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg,
                         dependencies = TRUE,
                         repos = 'http://cran.us.r-project.org')
    sapply(pkg, require, character.only = TRUE)
    cat('All packages have been loaded')
}

# -------------------------------------------------- TABLE
table <- function(data.set = NULL, caption = '', table.size = 'big') {
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

# This function discretize ratings values to nearest .5 or integer value
rating.round <- function(rating, whole = FALSE) {
    purrr::map2_dbl(rating,
                    whole,
                    function(a, b) {
                        if (a <= 0.5) a <- 0.51 else if (a > 5) a <- 5
                        if (b) round(a + 0.01) else round(a * 2) / 2
                    })
}

split.genres <- function(data.set = NULL) {
    str(data.set)
    data <- data.set %>%
        separate_rows(genres,
                      sep = '\\|')
    Encoding(data$title) <- 'latin1'

    data <- data %>%
        dplyr::mutate(dates = as.POSIXct(timestamp,
                                         origin = '1970-01-01'))
    data <- data %>%
        dplyr::mutate(date.year = format(as.Date(dates), '%Y'),
                      date.year.month = format(as.Date(dates), '%Y-%m'),
                      date.year.month.day = format(as.Date(dates), '%Y-%m-%d'))
    return(data)
}


