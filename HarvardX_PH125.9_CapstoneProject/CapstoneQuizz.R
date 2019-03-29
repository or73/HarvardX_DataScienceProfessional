# -------       -------       -------       -------       -------       -------

# Note: this process could take a couple of minutes
packages.list <- c('caret', 'caretEnsemble',
                   'dplyr',
                   'stringr',
                   'tidyr', 'tidyverse')

lapply(packages.list, require, character.only = TRUE)

load.packages.list <- function(pkgs.list) {
    # Validate if some package, of the list, is not installed
    packages.to.load <- packages.list[!(packages.list %in% installed.packages()[, 'Package'])]
    if (length(packages.to.load)) {
        cat('Packages to load: ', packages.to.load)
        install.packages(packages.to.load,
                         dependencies = TRUE,
                         repos = 'http://cran.us.r-project.org')
        sapply(packages.to.load, character.only = TRUE)
    }
    cat('All packages have been loaded')
}
load.packages.list(packages.list)


#############################################################
# Create edx set, validation set, and submission file
#############################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Load Dataset
dl <- tempfile()

# Download file
download.file('http://files.grouplens.org/datasets/movielens/ml-10m.zip', dl)

# Unzipping file and loading ratings dataset data, with colnames = c('userId', 'movieId', 'rating', 'timestamp')
ratings <- read.table(text = gsub('::', '\t',
                                  readLines(unzip(dl, 'ml-10M100K/ratings.dat'))),
                      col.names = c('userId',
                                    'movieId',
                                    'rating',
                                    'timestamp'))

# Unzipping file and loading movies dataset data, with colnames = c('movieId', 'title', 'genres')
movies <- str_split_fixed(readLines(unzip(dl, 'ml-10M100K/movies.dat')), '\\::', 3)
colnames(movies) <- c('movieId', 'title', 'genres')

# Creating a data frame with movies matrix
movies <- as.data.frame(movies) %>%
    mutate(movieId = as.numeric(levels(movieId))[movieId],
           title = as.character(title),
           genres = as.character(genres))

# Joining Datasets in movielens, by 'movieId'  colnames = 'userId' 'movideId' 'rating' 'timestamp' 'title' 'genres'
movielens <- left_join(ratings,
                       movies,
                       by = 'movieId')

# Structure of Dataframe
# structure
str(movielens)
# colnames
names(movielens)
# number of columns
ncol(movielens)
# number of rows
nrow(movielens)

# See top 7 rows and all columns
head(movielens, 7)

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- caret::createDataPartition(y = movielens$rating,
                                         times = 1,
                                         p = 0.1,
                                         list = FALSE)
nrow(test_index)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
nrow(edx)
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
    semi_join(edx, by = 'movieId') %>%
    semi_join(edx, by = 'userId')

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

nrow(validation)
nrow(edx)

# -------       -------       -------       -------       -------       -------

# Quiz: MovieLens Dataset

# All of the questions in this quiz involve working with the datasets you've created using the code we provided.
#
# Note: answers to Q5 and Q6 were updated 12/24/2018 after edits to the dataset code were implemented. (Attempts reset for all learners.)
#
# Q1
# How many rows and columns are there in the edx dataset?
#
# Number of rows:
nrow(edx)  # 9000055

# Number of columns:
ncol(edx) # 6

# Q2
# How many zeros were given as ratings in the edx dataset?
edx %>% filter(rating == 0) %>% tally() #  0

# How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% tally() # 2121240

# Q3
# How many different movies are in the edx dataset?

n_distinct(edx$movieId) # 10677

# Q4
# How many different users are in the edx dataset?
n_distinct(edx$userId) # 69878

# Q5
# How many movie ratings are in each of the following genres in the edx dataset?

edx %>% separate_rows(genres, sep = '\\|') %>%
    group_by(genres) %>%
    summarize(count = n()) %>%
    arrange(desc(count))

# Drama:
# 3910127

# Comedy:
# 3540930

# Thriller:
# 2325899

# Romance:
# 1712100

# Q6
# Which movie has the greatest number of ratings?
# Forrest Gump
# Jurassic Park
# Pulp Fiction   <-*
# The Shawshank Redemption
# Speed 2: Cruise Control

# Q7
# What are the five most given ratings in order from most to least?
# 4, 3, 5, 3.5, 2    <-*
# 3.5, 4.5, 1, 3, 5
# 0.5, 5, 1, 4.5, 1.5
# 5, 3, 1, 4.5, 3.5
# 2, 3.5, 5, 3, 4

# Q8
# 1 point possible (graded)
# True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
# True   <-*
# False

# -------       -------       -------       -------       -------       -------



# -------       -------       -------       -------       -------       -------



