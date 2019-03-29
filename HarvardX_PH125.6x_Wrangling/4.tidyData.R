# tidyData
library( dslabs )
data( 'gapminder' )
tidy_data <- gapminder %>%
    filter( country %in% c( 'South Korea', 'Germany' ) ) %>%
    select( country, year, fertility )
head( tidy_data )

tidy_data %>%
    ggplot( aes( year, fertility, color = country ) ) +
    geom_point()

# tidy Data:  Each row represents one observation and the columns represent the different variables that we have data on for those observations.

# This is an example of non tidy data:
path <- system.file( 'extdata', package = 'dslabs' )
filename <- file.path( path, 'fertility-two-countries-example.csv' )
wide_data <- read_csv( filename )
head( wide_data )

# Differences between wide format and tidy format:
#    Wide format:
#                 1. Each row includes several observations
#                 2. One of thhe variables is stored in the header

# Question 1
# A collaborator sends you a file containing data for three years of average race finish times.
#
# age_group,2015,2016,2017
# 20,3:46,3:22,3:50
# 30,3:50,3:43,4:43
# 40,4:39,3:49,4:51
# 50,4:48,4:59,5:01
# Are these data considered “tidy” in R? Why or why not?
# Yes. These data are considered “tidy” because each row contains unique observations.
# Yes. These data are considered “tidy” because there are no missing data in the data frame.
# No. These data are not considered “tidy” because the variable “year” is stored in the header.   <-*
# No. These data are not considered “tidy” because there are not an equal number of columns and rows.




# Question 2
# 1 point possible (graded)
# Below are four versions of the same dataset. Which one is in a tidy format?

# state abb region population total    <-*
# Alabama	AL	South	4779736	135
# Alaska  AK   West 	710231	19
# Arizona  AZ   West	6392017   232
# Arkansas  AR  South	2915918	93
# California  CA   West   37253956  1257
# Colorado  CO   West	5029196	65

# state abb region    	var  people
# Alabama  AL  South population 4779736
# Alabama  AL  South      total 	135
# Alaska  AK   West population  710231
# Alaska  AK   West  	total  	19
# Arizona  AZ   West population 6392017
# Arizona  AZ   West      total 	232

# state abb Northeast   South North Central 	West
# Alabama  AL        NA 4779736        	NA   	NA
# Alaska  AK        NA  	NA        	NA   710231
# Arizona  AZ        NA  	NA        	NA  6392017
# Arkansas  AR        NA 2915918        	NA   	NA
# California  CA    	NA      NA        	NA 37253956
# Colorado  CO        NA  	NA        	NA  5029196

# state abb region 	rate
# Alabama  AL  South 2.82e-05
# Alaska  AK   West 2.68e-05
# Arizona  AZ   West 3.63e-05
# Arkansas  AR  South 3.19e-05
# California  CA   West 3.37e-05
# Colorado  CO   West 1.29e-05





# reshaping Data
# gather() : Converts wide format data into tidy data
#            - First argument: Sets the name of the column that will hold the
#                                   variable that is currently kept in the wide
#                                   data column name.
#            - Second argument: Sets the column name for the column that will
#                                   hold the values in the column cells.
#            - Third argument: specifies the columns that will be gathered.

new_tidy_data <- wide_data %>%
    gather( year, fertility, `1960`:`2015` )
head( new_tidy_data )

new_tidy_data <- wide_data %>%
    gather( year, fertility, -country )
head( new_tidy_data )

class( tidy_data$year )
class( new_tidy_data$year )

# The gather function assumes column names are characters

new_tidy_data <- wide_data %>%
    gather( year, fertility, -country, convert = TRUE )
class( new_tidy_data$year )

new_tidy_data %>%
    ggplot( aes( year, fertility, color = country ) ) +
    geom_point()


# Convert tidy data into wide data
# spread() function is the inverse of gather() function
#    - The first argument: tells spread which variables.
#    - The second argument: specifies which variables to use to fill out the cells.

new_wide_data <- new_tidy_data %>%
    spread( year, fertility )
select( new_wide_data, country, `1960`:`1967` )



# Question 1
# Your file called “times.csv” has age groups and average race finish times for three years of marathons.
#
# age_group,2015,2016,2017
# 20,3:46,3:22,3:50
# 30,3:50,3:43,4:43
# 40,4:39,3:49,4:51
# 50,4:48,4:59,5:01
# You read in the data file using the following command.
#
# d <- read_csv("times.csv")
# Which commands will help you “tidy” the data?
#
# tidy_data <- d %>%
#     gather(year, time, `2015`:`2017`)    <-*
#
# tidy_data <- d %>%
#     spread(year, time, `2015`:`2017`)
#
# tidy_data <- d %>%
#     gather(age_group, year, time, `2015`:`2017`)
#
# tidy_data <- d %>%
#     gather(time, `2015`:`2017`)




# Question 2
# You have a dataset on U.S. contagious diseases, but it is in the following wide format:
#
#     > head(dat_wide)
# state year population Hepatitis A Mumps Polio Rubella
# Alabama 1990    4040587      	86	19    76   	1
# Alabama 1991    4066003      	39	14    65   	0
# Alabama 1992    4097169      	35	12    24   	0
# Alabama 1993    4133242      	40	22    67   	0
# Alabama 1994    4173361      	72	12    39   	0
# Alabama 1995    4216645      	75 	2     38            0
# Which of the following would transform this into a tidy dataset, with each row representing an observation of the incidence of each specific disease (as shown below)?
# > head(dat_tidy)
# state year population 	disease count
# Alabama 1990	4040587 Hepatitis A	86
# Alabama 1991	4066003 Hepatitis A	39
# Alabama 1992	4097169 Hepatitis A	35
# Alabama 1993	4133242 Hepatitis A	40
# Alabama 1994	4173361 Hepatitis A	72
# Alabama 1995	4216645 Hepatitis A	75
#
# dat_tidy <- dat_wide %>%
#     gather (key = count, value = disease, `Hepatitis A`, `Rubella`)
#
# dat_tidy <- dat_wide %>%
#     gather(key - count, value = disease, -state, -year, -population)
#
# dat_tidy <- dat_wide %>%
#     gather(key = disease, value = count, -state)
#
# dat_tidy <- dat_wide %>%
#     gather(key = disease, value = count, “Hepatitis A”: “Rubella”)   <-*




# Question 3
# You have successfully formatted marathon finish times into a tidy object called tidy_data. The first few lines are shown below.
#
# age_group,year,time
# 20,2015,03:46
# 30,2015,03:50
# 40,2015,04:39
# 50,2015,04:48
# 20,2016,03:22
# Select the code that converts these data back to the wide format, where each year has a separate column.
# tidy_data %>% spread(time, year)
# tidy_data %>% spread(year, time)    <-*
# tidy_data %>% spread(year, age_group)
# tidy_data %>% spread(time, year, `2015`:`2017`)





# Question 4
# > head(dat)
# state abb region    	var  people
# Alabama  AL  South population 4779736
# Alabama  AL  South  	total 	135
# Alaska  AK   West population  710231
# Alaska  AK   West  	total  	19
# Arizona  AZ   West population 6392017
# Arizona  AZ   West  	total 	232
# You would like to transform it into a dataset where population and total are each their own column (shown below). Which code would best accomplish this?
#     state abb region population total
# Alabama  AL  South	4779736   135
# Alaska  AK   West 	710231	19
# Arizona  AZ   West	6392017   232
# Arkansas  AR  South	2915918	93
# California  CA   West   37253956  1257
# Colorado  CO   West	5029196	65
#
# dat_tidy <- dat %>% spread(key = var, value = people)     <-*
# dat_tidy <- dat %>% spread(key = state:region, value = people)
# dat_tidy <- dat %>% spread(key = people, value = var)
# dat_tidy <- dat %>% spread(key = region, value = people)





# Separate and Unite

# Next file includes two variables: life expectancy and fertility

path <- system.file( 'extdata', package = 'dslabs' )
filename <- file.path( path, 'life-expectancy-and-fertility-two-countries-example.csv' )

raw_dat <- read_csv( filename )
select( raw_dat, 1:5 )


dat <- raw_dat %>%
    gather( key, value, -country )
head( dat )

# We must separate the key column values, and we will use the separate() function:
# This function receives:
#    1. Name of the column to be separated.
#    2. Name to be used for the new columns.
#    3. Character that separates the variables.

dat %>%
    separate( key, c( 'year', 'variable_name' ), '_' )

# A mistake is identified, because the '_' is used into 'life_expectancy' name, then
#    we can add a thid column to catch this and let the separate function know which column to fill in with missing values - NAs, in this case -- when there is not a third value

dat %>%
    separate( key,
              c( 'year', 'first_variable_name', 'second_variable_name' ),
              fill = 'right')

# But, to avoid the use of two columns, we can use an aditional field called 'extra':
dat %>%
    separate( key,
              c( 'year', 'variable_name' ),
              sep = '_',
              extra = 'merge' )

# We need to create a column for each variable
dat %>%
    separate( key,
              c( 'year', 'variable_name' ),
              sep = '_',
              extra = 'merge' ) %>%
    spread( variable_name, value )

# We can obtain the same result with this code:
dat %>%
    separate( key,
              c( 'year', 'first_variable_name', 'second_variable_name' ),
              fill = 'right' ) %>%
    unite( variable_name, first_variable_name, second_variable_name, sep = '_' ) %>%
    spread( variable_name, value ) %>%
    rename( fertility = fertility_NA )




# Question 1
# A collaborator sends you a file containing data for two years of average race finish times.
#
# age_group,2015_time,2015_participants,2016_time,2016_participants
# 20,3:46,54,3:22,62
# 30,3:50,60,3:43,58
# 40,4:39,29,3:49,33
# 50,4:48,10,4:59,14
# You read in the data file
#
# d <- read_csv("times.csv")
# Which of the answers below best tidys the data?
#
# tidy_data <- d %>%
#     gather(key = “key”, value = “value”, -age_group) %>%
#     separate(col = key, into = c(“year”, “variable_name”), sep = “.”) %>%
#     spread(key = variable_name, value = value)
#
#
# tidy_data <- d %>%           <-*
#     gather(key = “key”, value = “value”, -age_group) %>%
#     separate(col = key, into = c(“year”, “variable_name”), sep = “_”) %>%
#     spread(key = variable_name, value = value)
#
#
# tidy_data <- d %>%
#     gather(key = “key”, value = “value”) %>%
#     separate(col = key, into = c(“year”, “variable_name”), sep = “_”) %>%
#     spread(key = variable_name, value = value)
#
#
# tidy_data <- d %>%
#     gather(key = “key”, value = “value”, -age_group) %>%
#     separate(col = key, into = “year”, sep = “_”) %>%
#     spread(key = year, value = value)




# Question 2
# You are in the process of tidying some data on heights, hand length, and wingspan for basketball players in the draft. Currently, you have the following:
#
#     > head(stats)
# key		value
# allen_height		75
# allen_hand_length	8.25
# allen_wingspan	79.25
# bamba_height	83.25
# bamba_hand_length 9.75
# bamba_wingspan	94
# Select all of the correct commands below that would turn this data into a “tidy” format.
#
# tidy_data <- stats %>%       <-*
#     separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>%
#     spread(key = variable_name, value = value)
#
# tidy_data <- stats %>%
#     separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", fill = "right") %>%
#     unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>%
#     spread(key = variable_name, value = value)
#
# tidy_data <- stats %>%
#     separate(col = key, into = c("player", "variable_name"), sep = "_") %>%
#     spread(key = variable_name, value = value)



# Combining Tables

# Explore thre relationship between population size for US states, which we have
#    in this table
data( murders )
head( murders )

# and electoral votes, which we have in this one
data( "polls_us_election_2016" )
head( results_us_election_2016 )

#Notice that just joining these two tables together like this will not work since
#    the order of the states is not quite the same

identical( results_us_election_2016, murders$state )

tab <- left_join( murders, results_us_election_2016, by = 'state' )
head( tab )

library( ggrepel )
tab %>%
    ggplot( aes( population / 10^6, electoral_votes, label = abb ) ) +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous( trans = 'log2' ) +
    scale_y_continuous( trans = 'log2' ) +
    geom_smooth( method = 'lm', se = FALSE )


# A subset of our first table
tab1 <- slice( murders, 1:6 ) %>% select( state, population )
tab1

# A subset of our second table
tab2 <- slice( results_us_election_2016, c( 1:3, 5, 7:8 ) ) %>%
    select( state, electoral_votes )
tab2


# Using different joins options with tab1 and tab2
# left join
# We want a table like tab one, but adding electoral votes to whatever states
#    we have available in tab one
left_join( tab1, tab2 )
tab1 %>% left_join( tab2 )

# If instead of a table like tab one we want one like tab two
tab1 %>% right_join( tab2 )


# If we want to keep only the rows that have information in both tables
inner_join( tab1, tab2 )


# If we want to keep all the rows and fill in the missing parts with NAs
full_join( tab1, tab2 )


# The semi join function lets us keep the part of ther first table for which
#    we have information in the second.  It does not add the columns of the
#    second.
semi_join( tab1, tab2 )

# Anti join is the opposite of semi join.  It keeps the elements of the first
#    table for which there is no information in the second
anti_join( tab1, tab2 )



# Question 1
# You have created a tab1 and tab2 of state population and election data, similar to our module videos:
#
# > tab1
# state 	population
# Alabama	4779736
# Alaska     	710231
# Arizona    	6392017
# Delaware     	897934
# District of Columbia     601723
#
# > tab2
# state 	electoral_votes
# Alabama      9
# Alaska         3
# Arizona        11
# California     55
# Colorado      9
# Connecticut  7
#
# > dim(tab1)
# [1] 5 2
#
# > dim(tab2)
# [1] 6 2
# What are the dimensions of the table dat, created by the following command?
#
#     dat <- left_join(tab1, tab2, by = “state”)
#
# 3 rows by 3 columns
# 5 rows by 2 columns
# 5 rows by 3 columns   <-*
# 6 rows by 3 columns



# Question 2
# 1 point possible (graded)
# We are still using the tab1 and tab2 tables shown in question 1. What join command would create a new table “dat” with three rows and two columns?
#
# dat <- right_join(tab1, tab2, by = “state”)
# dat <- full_join(tab1, tab2, by = “state”)
# dat <- inner_join(tab1, tab2, by = “state”)
# dat <- semi_join(tab1, tab2, by = “state”)     <-*




# Binding
# The binding functions do not try to march by a variable, but rather just combine
#    the data
# If the data sets don't match by the appropiate dimension we will obtain an error

# bind_cols
# binds two objects by putting the columns of each together in a tibble
bind_cols( a = 1:3, b = 4:6 )

tab1 <- tab[ , 1:3 ]
tab2 <- tab[ , 4:6 ]
tab3 <- tab[ , 7:9 ]
new_tab <- bind_cols( tab1, tab2, tab3 )
head( new_tab )


# cbind
# performs the same function as bind_cols, but creates objects other than tibbles,
#    either matrices or data frames, something else


# bind_rows
# is the same as bind_columns, but binds rows instead of columns
tab1 <- tab[ 1:2, ]
tab2 <- tab[ 3:4, ]
bind_rows( tab1, tab2 )


# Question 1
# Which of the following are real differences between the join and bind functions?
#     Please select all correct answers.
#
# Binding functions combine by position, while join functions match by variables.    <-*
# Joining functions can join datasets of different dimensions, but the bind functions must match on the appropriate dimension (either same row or column numbers).   <-*
# Bind functions can combine both vectors and dataframes, while join functions work for only for dataframes.    <-*
# The join functions are a part of the dplyr package and have been optimized for speed, while the bind functions are inefficient base functions.



# Set Operators

# intersect
# intersection of numeric or character vectors
intersect( 1:10, 6:15 )
intersect( c( 'a', 'b', 'c' ), c( 'b', 'c', 'd' ) )

tab1 <- tab[ 1:5, ]
tab2 <- tab[ 3:7, ]
intersect( tab1, tab2 )


# union
union( 1:10, 6:15 )
union( c( 'a', 'b', 'c' ), c( 'b', 'c', 'd' ) )

tab1 <- tab[ 1:5, ]
tab2 <- tab[ 3:7, ]
union( tab1, tab2 )


# setdiff
# Take set differences, this function is not symmetric
setdiff( 1:10, 6:15 )
setdiff( 6:15, 1:10 )

tab1 <- tab[ 1:5, ]
tab2 <- tab[ 3:7, ]
setdiff( tab1, tab2 )



# setequal
# Tells us if two sets are the same regardless of order
setequal( 1:5, 1:6 )
setequal( 1:5, 5:1 )

setequal( tab1, tab2 )


# Question 1
# 1 point possible (graded)
# We have two simple tables, shown below:
#
#     > df1
# x     y
# a     a
# b     a
#
# > df2
# x     y
# a     a
# a     b
#
# Which command would result in the following table?
#
#     > final
# x     y
# b     a
#
# final <- union(df1, df2)
# final <- setdiff(df1, df2)    <-*
# final <- setdiff(df2, df1)
# final <- intersect(df1, df2)
