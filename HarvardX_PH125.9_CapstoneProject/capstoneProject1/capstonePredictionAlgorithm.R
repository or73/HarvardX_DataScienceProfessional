# -----------------------------------------------------------------------
# ---------------------------------------------- CLEAN WORKSPACE
# -----------------------------------------------------------------------
rm(list = ls())
gc()

# -----------------------------------------------------------------------
# ---------------------------------------------- SETTING WORK DIRECTORY
# -----------------------------------------------------------------------
setwd('~/OR73 Dropbox/Oscar Reyes/training_courses/R/HarvardX_PH125.9_CapstoneProject/capstoneProject')

# -----------------------------------------------------------------------
# ---------------------------------------------- LOAD FILES
# -----------------------------------------------------------------------
source('./functions.R')

# -----------------------------------------------------------------------
# ---------------------------------------------- LOAD PACKAGES/LIBRARIES
# -----------------------------------------------------------------------
# Required packages
packages.list <-  c('anytime',
                    'caret', 'caretEnsemble', 'cluster',
                    'data.table',
                    'factoextra', 'fpc',
                    'gsubfn', 'ggvis',
                    'kableExtra', 'knitr',
                    'magrittr', 'matrixStats',
                    'pacman', 'plyr', 'png',
                    'RColorBrewer', 'recosystem', 'rlang', 'rmarkdown',
                    'skimr',
                    'tidyverse')
# Load required packages/libraries
load.packages.list(packages.list)

# Validate if files have been loaded previously
if (!file_test('-f', file.path('data', 'edx.csv')) | !file_test('-f', file.path('data', 'validation.csv')) | !file_test('-f', file.path('data', 'edx_test.csv')) | !file_test('-f', file.path('data', 'validation_test.csv'))) {
    cat('Loading MovieLens Data Set...\n')
    # Loading MovieLens 10M Dataset
    load.datasets()
} else {
    cat('edx & validation data sets are loaded')
}

# -----------------------------------------------------------------------
# ---------------------------------------------- LOADING DATASETS FILES
# -----------------------------------------------------------------------
# Loading data sets data
edx <- read.csv(file.path('data', 'edx.csv'),
                header = TRUE)
edx.test <- read.csv(file.path('data','edx_test.csv'),
                header = TRUE)
validation <- read.csv(file.path('data','validation.csv'),
                       header = TRUE)
validation.test <- read.csv(file.path('data', 'validation_test.csv'),
                            header = TRUE)
# -----------------------------------------------------------------------
# ---------------------------------------------- GROUPS CREATION
# -----------------------------------------------------------------------
# Create goups
group.date.year <- edx.test %>% dplyr::group_by(date.year)
group.date.year.month <- edx.test %>% dplyr::group_by(date.year.month)
group.genres <- edx.test %>% dplyr::group_by(genres)
group.movieId <- edx.test %>% dplyr::group_by(movieId)
group.movieId.title <- edx.test %>% dplyr::group_by(movieId, title)
group.rating <- edx.test %>% dplyr::group_by(rating)
group.userId <- edx.test %>% dplyr::group_by(userId)
group.edx.date.year <- edx %>% dplyr::group_by(date.year)
group.edx.genre <- edx %>% dplyr::group_by(genres)
group.edx.rating <- edx %>% dplyr::group_by(rating)
group.edx.title <- edx %>% dplyr::group_by(title)
group.edx.title.id <- edx %>% dplyr::group_by(movieId, title)
group.edx.title.year <- edx %>% dplyr::group_by(date.year, title)
group.edx.user <- edx %>% dplyr::group_by(userId)

# -----------------------------------------------------------------------
# ---------------------------------------------- DATASETS EXPLORATION
# -----------------------------------------------------------------------

# ---------------------------------------------- Total of new fields by splitted genres
new.fields.amount <- length(edx.test$movieId) - length(edx$movieId)
new.fields.amount

# Remove unrequired variables
rm(packages.list)

# ---------------------------------------------- Amount of different users & movies
users.movies <- edx.test %>%
    dplyr::summarize(Users = n_distinct(userId),
                     Movies = n_distinct(movieId))
table(users.movies,
      'Amount of Users & Movies')
# ---------------------------------------------- Structure al edx.test dataset, when genres are more than one
str(edx.test)

# Dimension of edx.test data set
dim(edx.test)

# Columns names
colnames(edx.test)

# ---------------------------------------------- Histogram of year vs ratings
# Min year data
min.year <- min(edx.test$date.year)
# Max year data
max.year <- max(edx.test$date.year) + 1

# ---------------------------------------------- Total ratings per years
max.year - min.year
table.edx.test.year <- group.date.year %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::arrange(date.year)

graph.year.ratings <- graph.bar.col(table.edx.test.year,
                                    table.edx.test.year$date.year,
                                    table.edx.test.year$ratings,
                                    'Ratings per Year',
                                    'Amount of Ratings',
                                    'Year',
                                    'Ratings',
                                    'bar')
graph.year.ratings

# Total ratings per genre, over years
table.edx.test.year.genre <- edx.test %>%
    dplyr::group_by(date.year, genres) %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::arrange(desc(date.year))

graph.year.ratings.genre <- graph.bar.col(table.edx.test.year.genre,
                                          table.edx.test.year.genre$date.year,
                                          table.edx.test.year.genre$ratings,
                                          'Genres Over Time',
                                          'Genres by Year',
                                          'year',
                                          'ratings',
                                          'col')
graph.year.ratings.genre

# Users by year
table.edx.year.user <- group.date.year %>%
    dplyr::summarize(users = n_distinct(userId)) %>%
    dplyr::arrange(date.year)
# Graphic of users per year
graph.year.users <- graph.bar.col(table.edx.year.user,
                                  table.edx.year.user$date.year,
                                  table.edx.year.user$users,
                                  'Users Over Time',
                                  'Users by Year',
                                  'Year',
                                  'Users',
                                  'bar')
graph.year.users

# Histogram on log scale with number of times movies have been rated
graph.movies.ratings <- graph(group.movieId,
                              'movieId',
                              'Times Movies have been Rated',
                              'Movies Vs Rating',
                              'Movies',
                              'Rating',
                              c(1,10,100,1000, 10000))
graph.movies.ratings

# Histogram on log scale of time a user have rated movies
graph.users.movies.ratings <- graph(group.userId,
                                    'userId',
                                    'Times Users have been Rated Movies',
                                    'Users Vs Rating',
                                    'Users',
                                    'Rating',
                                    c(1,10,100,1000, 10000))
graph.users.movies.ratings

# Top 10 genres for movies
table.top.10.genres.movies <- table.count(TRUE,
                                          'Top 10 Genres',
                                          TRUE,
                                          10,
                                          edx.test,
                                          'genres',
                                          '')
table.top.10.genres.movies

# Top 10 of most rated movies
table.top.10.movies.rating <- table.count(FALSE,
                                          'Top 10 Rated Movies',
                                          TRUE,
                                          10,
                                          edx.test,
                                          'genres',
                                          'title')
table.top.10.movies.rating

# ---------------------------------------------- Analysis by Rating
# <!-- Most & Less Rated years -->
table.rating.year <- group.edx.date.year %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::arrange(date.year)

table(table.rating.year,
      'Rating Per Year',
      'small')

most.rated.year <- table.rating.year[which.max(table.rating.year$ratings),]
less.rated.year <- table.rating.year[which.min(table.rating.year$ratings),]
graph.rating.year <- graph.bar.col.color(table.rating.year,
                                         table.rating.year$date.year,
                                         table.rating.year$ratings,
                                         'Ratings of Years Over Time',
                                         'Ratings per Year',
                                         'Year',
                                         'Rating',
                                         'bar',
                                         which.max(table.rating.year$ratings),
                                         which.min(table.rating.year$ratings),
                                         length(table.rating.year$ratings))
graph.rating.year

# <!-- Most Rated Movie -->
table.rating.movie <- group.edx.title %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::arrange(desc(ratings))

# Most rated movie
table.rating.movie[which.max(table.rating.movie$ratings),]
# Less rated movie
table.rating.movie[which.min(table.rating.movie$ratings),]

table.rating.movie <- table.rating.movie %>%
    dplyr::slice(1:10)
table.rating.movie

graph.rating.movie <- graph.bar.col.color(table.rating.movie,
                                          table.rating.movie$title,
                                          table.rating.movie$ratings,
                                          'Ratings of Movies Over Time',
                                          'Ratings per Movie',
                                          'Movie',
                                          'Rating',
                                          'bar',
                                          which.max(table.rating.movie$ratings),
                                          which.min(table.rating.movie$ratings),
                                          length(table.rating.movie$ratings))
graph.rating.movie

table.rating.movie <- table.rating.movie
table.rating.movie
table(table.rating.movie,
      'Ratings per Movie',
      'small')


# <!-- Most Rated Genre -->
table.rating.genre <- group.genres %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::arrange(desc(ratings))

table.rating.genre

# Most rated genre
most.rated.genre <- which.max(table.rating.genre$ratings)
# Less rated movie
less.rated.genre <- which.min(table.rating.genre$ratings)

table(table.rating.genre %>% arrange(desc(ratings)),
      'Ratings per Genre',
      'small')

graph.rating.genre <- graph.bar.col.color(table.rating.genre,
                                          table.rating.genre$genres,
                                          table.rating.genre$ratings,
                                          'Ratings of Genres Over Time',
                                          'Ratings per Genre',
                                          'Genre',
                                          'Rating',
                                          'bar',
                                          which.max(table.rating.genre$ratings),
                                          which.min(table.rating.genre$ratings),
                                          length(table.rating.genre$ratings))
graph.rating.genre

# ---------------------------------------------- Analysis by Title
# <!-- Movie/Title with more Ratings -->
table.rating.title <- group.edx.title %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::arrange(desc(ratings))

# Most rated title
most.rated.title <- table.rating.title[which.max(table.rating.title$ratings),]
# Less rated title
less.rated.title <- table.rating.title[which.min(table.rating.title$ratings),]

table.rating.title <- table.rating.title %>%
    dplyr::slice(1:10)

table1.rating.title <- table(table.rating.title,
      'Rating per Title',
      'small')
table1.rating.title

graph.rating.title <- graph.bar.col.color(table.rating.title,
                                          table.rating.title$title,
                                          table.rating.title$ratings,
                                          'Ratings of Title Over Time',
                                          'Ratings per Title',
                                          'Title',
                                          'Rating',
                                          'bar',
                                          which.max(table.rating.title$ratings),
                                          which.min(table.rating.title$ratings),
                                          length(table.rating.title$ratings))
graph.rating.title

# <!-- Most Success Movie/Title per Year -->
table.edx.title.year <- group.edx.title.year %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::filter(ratings == max(ratings)) %>%
    dplyr::arrange(date.year, desc(ratings))

# Most rated title per year
table(table.edx.title.year,
      'Most Rated Title per Year',
      'small')

graph.rating.title.year <- graph.bar.col.color(table.edx.title.year,
                                               table.edx.title.year$date.year,
                                               table.edx.title.year$ratings,
                                               'Most Rated Title per Year',
                                               'Title per Year',
                                               'Title',
                                               'Rating',
                                               'bar',
                                               which.max(table.edx.title.year$ratings),
                                               which.min(table.edx.title.year$ratings),
                                               length(table.edx.title.year$ratings))
graph.rating.title.year <- graph.rating.title.year +
    geom_text(aes(x = table.edx.title.year$date.year,
                  label = table.edx.title.year$title),
              color = 'black',
              size = 2.5,
              angle = 70,
              check_overlap = TRUE,
              hjust = 0,
              nudge_x = 0,
              vjust = 0,
              nudge_y = 0.5)
graph.rating.title.year

# ---------------------------------------------- Analysis by User
#     <!-- User's Ratings -->
table.rating.user <- group.rating %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::mutate(percent = ratings / nrow(edx.test) * 100) %>%
    dplyr::arrange(desc(ratings)) %>%
    dplyr::slice(1:20)

table(table.rating.user,
      'Ratings per User',
      'small')

graph.rating.user <- graph.bar.col.color(table.rating.user,
                                         table.rating.user$rating,
                                         table.rating.user$ratings,
                                         'Ratings of Users Over Time',
                                         'Ratings per User',
                                         'User ID',
                                         'Rating',
                                         'bar',
                                         which.max(table.rating.user$ratings),
                                         which.min(table.rating.user$ratings),
                                         length(table.rating.user$ratings))
graph.rating.user

# Users rated movies with 4.0 over 28%, more than quarter of time

#     <!-- User with more Ratings -->
table.rating.user1 <- group.userId %>%
    dplyr::summarize(ratings = n()) %>%
    dplyr::arrange(desc(ratings))

# Most rated user
most.rated.user <- which.max(table.rating.user1$ratings)
# Less rated user
less.rated.user <- which.min(table.rating.user1$ratings)

table.rating.user <- table.rating.user1 %>% slice(1:10)

table(table.rating.user,
      'Ratings per User',
      'small')
table.rating.user1$userId <- as.character(table.rating.user1$userId)
graph.rating.user <- graph.bar.col.color(table.rating.user1,
                                         table.rating.user1$userId,
                                         table.rating.user1$ratings,
                                         'Ratings of Users Over Time',
                                         'Ratings per User',
                                         'User ID',
                                         'Rating',
                                         'bar',
                                         which.max(table.rating.user1$ratings),
                                         which.min(table.rating.user1$ratings),
                                         length(table.rating.user1$ratings))
graph.rating.user

# -----------------------------------------------------------------------
# ---------------------------------------------- Recommendation Algorithm
# -----------------------------------------------------------------------

# mu calculation
mu.hat <- mean(edx.test$rating)
mu.hat

# RMSE Baseline model
RMSE.baseline <- RMSE(edx.test$rating, mu.hat)
RMSE.baseline

# RMSEs comparisson table
table.RMSE.comparisson <- tibble(method = 'Baseline',
                                 RMSE = RMSE.baseline)
table(table.RMSE.comparisson, 'RMSEs Comparisson')

# Test to evidence that other values give a worse result
# prediction <- rep(4, nrow(edx.test))
RMSE(edx.test$rating, rep(4, nrow(edx.test)))

# -----------------------------------------------------------------------
# ---------------------------------------------- DATASETS ANALYSIS
# -----------------------------------------------------------------------

# ---------------------------------------------- Movies Bias RMSE
# Movies Bias
mu <- mean(edx.test$rating)
movies.avg <- group.movieId %>%
    dplyr::summarize(b.i = mean(rating - mu))

str(movies.avg)
graph.movies.avg <- graph.mean(movies.avg, movies.avg$b.i, 'Movies Bias', 'Movies Impact', 'b.i', '')
graph.movies.avg
names(movies.avg)
# Predictions
predict.rating <- mu.hat + edx.test %>%
    left_join(movies.avg,
              by = 'movieId') %>%
    .$b.i

RMSE.movies.bias <- RMSE(predict.rating, edx.test$rating)
table.RMSE.comparisson <- bind_rows(table.RMSE.comparisson,
                                    tibble(method = 'Movies Bias',
                                           RMSE = RMSE.movies.bias))
table(table.RMSE.comparisson, 'RMSEs Comparisson')

# ---------------------------------------------- Movies & User Bias
movies.users.avg <- group.userId %>%
    dplyr::summarize(b.u = mean(rating))

graph.movies.users.bias <- graph.mean(movies.users.avg, movies.users.avg$b.u, 'Movies & Users Bias', 'Movies & Users Impact', 'b.u', '')
graph.movies.users.bias

# ---------------------------------------------- User Bias
users.avg <- edx.test %>%
    dplyr::left_join(movies.avg,
                     by = 'movieId') %>%
    dplyr::group_by(userId) %>%
    dplyr::summarize(b.u = mean(rating - mu.hat - b.i))
users.avg

# ---------------------------------------------- Calculate predictions with user & movies Bias
# predict.rating
predict.rating <- edx.test %>%
    left_join(movies.avg, by = 'movieId') %>%
    left_join(users.avg, by = 'userId') %>%
    mutate(pred = mu.hat + b.i + b.u) %>%
    .$pred
predict.rating

# ---------------------------------------------- RMSE movies & users Bias
RMSE.movies.users.bias <- RMSE(predict.rating,
                               edx.test$rating)
table.RMSE.comparisson <- bind_rows(table.RMSE.comparisson,
                                    tibble(method = 'Movies & Users Bias',
                                           RMSE = RMSE.movies.users.bias))
table(table.RMSE.comparisson, 'RMSEs Comparisson')

# ---------------------------------------------- Regularization
# ---------------------------------------------- Prediction - largest errors in edx.test data frame
errors.edx.test <- edx.test %>%
    dplyr::left_join(movies.avg, by = 'movieId') %>%
    dplyr::mutate(residual = rating - (mu.hat + b.i)) %>%
    dplyr::arrange(residual) %>%
    dplyr::select(title, residual) %>%
    dplyr::slice(1:10)
table(errors.edx.test, 'Largest Errors')

# ---------------------------------------------- Join all movies replications
movies.joined <- edx.test %>%
    dplyr::select(movieId, title) %>%
    distinct()
movies.joined

# 10 best movies
movies.10.best <- movies.avg %>%
    dplyr::left_join(movies.joined, by = 'movieId') %>%
    dplyr::arrange(desc(b.i)) %>%
    dplyr::select(title, b.i) %>%
    dplyr::slice(1:10)
table(movies.10.best, '10 Better Movies')

# 10 worst movies
movies.10.worst <- movies.avg %>%
    dplyr::left_join(movies.joined, by = 'movieId') %>%
    dplyr::arrange(b.i) %>%
    dplyr::select(title, b.i) %>%
    dplyr::slice(1:10)
table(movies.10.worst, '10 Worst Movies')

# How many times the 10 best movies have been rated?
names(edx.test)
movies.10.best.rated <- edx.test %>%
    dplyr::count(movieId) %>%
    dplyr::left_join(movies.avg) %>%
    dplyr::left_join(movies.joined, by = 'movieId') %>%
    dplyr::arrange(desc(b.i)) %>%
    dplyr::select(title, b.i, n) %>%
    dplyr::slice(1:10)
table(movies.10.best.rated, '10 Best Movies Rating')

# How many times the 10 top worst movies have been rated in edx dataset
movies.10.worst.rated.movies <- edx.test %>%
    count(movieId) %>%
    left_join(movies.10.worst) %>%
    left_join(movies.joined,
              by = 'movieId') %>%
    arrange(b.i) %>%
    select(title,
           b.i,
           n) %>%
    slice(1:10)


# Add lamda for movies that have been rated few times
lambda <- 2.25
movies.reg.avg <- group.movieId %>%
    dplyr::summarize(b.i = sum(rating - mu) / (n() + lambda),
                     n.i = n())

# ---------------------------------------------- Graph of stimates shrink with lambda
movies.lambda <- tibble(original = movies.avg$b.i,
                        regularized = movies.reg.avg$b.i,
                        n = movies.reg.avg$n.i)
movies.lambda %>%
    ggplot(aes(original,
               regularized,
               size = sqrt(n))) +
    geom_point(shape = 1,
               alpha = 0.5)

# Top 10 of best movies after regularization
movies.top.10.best.regularized <- edx.test %>%
    dplyr::count(movieId) %>%
    dplyr::left_join(movies.reg.avg) %>%
    dplyr::left_join(movies.joined,
                     by = 'movieId') %>%
    dplyr::arrange(desc(b.i)) %>%
    dplyr::select(title, b.i, n) %>%
    slice(1:10)
table(movies.top.10.best.regularized, 'Top 10 of Best Regularized Movies')

# Top 10 of worst movies after regularization
movies.top.10.worst.regularized <- edx.test %>%
    dplyr::count(movieId) %>%
    dplyr::left_join(movies.reg.avg) %>%
    dplyr::left_join(movies.joined, by = 'movieId') %>%
    dplyr::arrange(b.i) %>%
    dplyr::select(title, b.i, n) %>%
    slice(1:10)
table(movies.top.10.worst.regularized, 'Top 10 of Worst Regularized Movies')

# ---------------------------------------------- Prediction for Movies Bias after regularization
predict.rating <- edx.test %>%
    left_join(movies.reg.avg, by = 'movieId') %>%
    mutate(pred = mu.hat + b.i) %>%
    .$pred

# ---------------------------------------------- RMSE movies Bias after Regularization
RMSE.movies.regularization.bias <- RMSE(predict.rating, edx.test$rating)
table.RMSE.comparisson <- bind_rows(table.RMSE.comparisson,
                                    tibble(method = 'Regularized Movies Bias',
                                           RMSE = RMSE.movies.regularization.bias))
table(table.RMSE.comparisson, 'RMSEs Comparisson')

# ---------------------------------------------- Cross validation to choose a lambda
# Identify the best lambda *from 0 to 7 at sequence of 0.2), over the movies bias, and the lowest value will be choosed

# ---------------------------------------------- Regularization movies Bias
lambda.values <- seq(0, 7, .25)
mu <- mean(edx.test$rating)
movies.sum <- group.movieId %>%
    dplyr::summarize(summ = sum(rating - mu),
                     n.i = n())

rmses <- function(l){
    predict.rating <- edx.test %>%
        dplyr::left_join(movies.sum,
                         by = "movieId") %>%
        dplyr::mutate(b.i = summ / (n.i + l)) %>%
        dplyr::mutate(pred = mu + b.i) %>%
        .$pred

    return(RMSE(predict.rating,
                edx.test$rating))
}

RMSE.function.reg <- sapply(lambda.values,
                            rmses)

qplot(lambda.values,
      RMSE.function.reg,
      main = "Regularization",
      xlab = "RMSE",
      ylab = "Lambda") # lambda vs RMSE

best.lambda <- lambda.values[which.min(RMSE.function.reg)]
best.lambda

# ---------------------------------------------- Regularization users & movies Bias
lambda.function <- function(l){
    m.i <- group.movieId %>%
        dplyr::summarize(m.i = sum(rating - mu)/(n() + l),
                         count = n())
    u.i <- edx.test %>%
        dplyr::left_join(m.i,
                         by = "movieId") %>%
        dplyr::group_by(userId) %>%
        dplyr::summarize(u.i = sum(rating - m.i - mu)/(n() + l))

    predicted_ratings <- edx.test %>%
        dplyr::left_join(m.i, by = "movieId") %>%
        dplyr::left_join(u.i, by = "userId") %>%
        dplyr::mutate(pred = mu + m.i + u.i) %>%
        .$pred

    return(RMSE(predicted_ratings,
                edx.test$rating))
}

RMSE.function.reg <- sapply(lambda.values,
                            lambda.function)

qplot(lambda.values,
      RMSE.function.reg,
      main = "Regularization",
      xlab = "RMSE",
      ylab = "Lambda") # lambda vs RMSE

best.lambda <- lambda.values[which.min(RMSE.function.reg)]
best.lambda

# ---------------------------------------------- Best RMSE
table.RMSE.comparisson <- bind_rows(table.RMSE.comparisson,
                                    tibble(method = 'Regularized Moves & User',
                                           RMSE = min(RMSE.function.reg)))
table(table.RMSE.comparisson, 'RMSEs Comparisson')

# Command to create .pdf file
# knitr::knit("./capstonePredictionAlgorithm.Rmd")
