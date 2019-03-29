# String Processing
# In the String Processing section, we use case studies that help demonstrate how string processing is a powerful tool useful for overcoming many data wrangling challenges. You will see how the original raw data was processed to create the data frames we have used in courses throughout this series.
#
# This section is divided into three parts.
#
# After completing the String Processing section, you will be able to:
#
#     Remove unwanted characters from text.
# Extract  numeric values from text.
# Find and replace characters.
# Extract  specific parts of strings.
# Convert free form text into more uniform formats.
# Split strings into multiple values.
# Use regular expressions (regex) to process strings.
# There are comprehension checks that follow most videos.
#
# We encourage you to use R to interactively test out your answers and further your own learning. If you get stuck, we encourage you to search the discussion boards for the answer to your issue or ask us for help!

# One of the most common data wrangling challenges involves converting or extracting
#    numeric data contained in character strings into numeric representations
#    required to make plots, summarize data, or fit models in r.  Also common is
#    processing unorganized text into meaningful variable names of categorical
#    variables.
url <- 'https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state'
murders_raw <- read_html( url ) %>%
    html_nodes( 'table' ) %>%
    html_table()
murders_raw <- murders_raw[[ 2 ]] %>%
    setNames( c( 'state', 'population', 'total', 'murders', 'gun_murders', 'gun_ownership', 'total_rate', 'murder_rate', 'gun_murder_rate' ) )
head( murders_raw )

# We realize that two of the columns taht we expected to contain numbers actually
#    contain characters.
class( murders_raw$population )
class( murders_raw$total )


# Question 1
# Which of the following is NOT an application of string parsing?
##
# Removing unwanted characters from text.
# Extracting numeric values from text.
# Formatting numbers and characters so they can easily be displayed in deliverables like papers and presentations.   <-*
# Splitting strings into multiple values.



# Defining strings> Single and Double Quoes and How to Escape

# Question 1
# Which of the following commands would not give you an error in R?
#
# cat(" LeBron James is 6’8\" ")    <-*
# cat(' LeBron James is 6'8" ')
# cat(` LeBron James is 6'8" `)
# cat(" LeBron James is 6\’8" ")




# stringr Package
# We will work with the object called 'murders'
# The population column has a character vector
murders_raw$population[ 1:3 ]

# Question 1
# Which of the following are advantages of the stringr package over string processing functions in base R? Select all that apply.
#
# Base R functions are rarely used for string processing by data scientists so it’s not worth learning them.
# Functions in stringr all start with “str_”, which makes them easy to look up
#    using autocomplete.   <-*
# Stringr functions work better with pipes.   <-*
# The order of arguments is more consistent in stringr functions than in base R.  <-*



# Case Study> US Murders Data
# We will work with the object called 'murders'
# The population column has a character vector
murders_raw$population[ 1:3 ]

commas <- function( x ) any( str_detect( x, ',' ) )
murders_raw %>% summarize_all( funs( commas ) )

test_1 <- str_replace_all( murders_raw$population, ',', '' )
test_1
test_1 <- as.numeric( test_1 )
test_1

test_2 <- parse_number( murders_raw$population )
identical( test_1, test_2 )

murders_new <- murders_raw %>% mutate_at( 2:3, parse_number )
murders_new %>% head


# Question 1
# You have a dataframe of monthly sales and profits in R
#
# > head(dat)
# # A tibble: 5 x 3
# Month    Sales    Profit
# <chr>    <chr>   	<chr>
#     January  	$128,568 	$16,234
# February 	$109,523 	$12,876
# March    	$115,468 	$17,920
# April    	$122,274 	$15,825
# May      	$117,921 	$15,437
# Which of the following commands could convert the sales and profits columns to numeric? Select all that apply.
#
#
#
# dat %>% mutate_at(2:3, parse_number)    <-*
# dat %>% mutate_at(2:3, as.numeric)
# dat %>% mutate_all(parse_number)
# dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>%
#     mutate_at(2:3, as.numeric)    <-*



# Case Study 2: Reported Heights
library( dslabs )
data( "reported_heights" )

class( reported_heights$height )
x <- as.numeric( reported_heights$height )
# How many NAs contain?
sum( is.na( x ) )

# We can see some entries that are not cussessfully converted by using the filter
#    function to keep only the entries that reslted in NAs
reported_heights %>% mutate( new_height = as.numeric( height ) ) %>%
    filter( is.na( new_height ) ) %>%
    head( n = 10 )

not_inches <- function( x, smallest = 50, tallest = 84 ) {
    inches <- suppressWarnings( as.numeric( x ) )
    ind <- is.na( inches ) | inches < smallest | inches > tallest
    ind
}

problems <- reported_heights %>%
    filter( not_inches( height ) ) %>%
    .$height
length( problems )

# Three patterns have been identified:
# Pattern 1: x'y  or x'y"  or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset( problems, pattern ) %>% head( n= 10 ) %>% cat

# Pattern 2: x.y   or   x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset( problems, pattern ) %>% head( n=10 ) %>% cat

# Pattern 3> reported in centimeters
ind <- which( between( suppressWarnings( as.numeric( problems ) ) / 2.54, 54, 81 ) )
ind <- ind[ !is.na( ind ) ]
problems[ ind ] %>% head( n = 10 ) %>% cat




# Question 1
# In the video, we use the function not_inches to identify heights that were incorrectly entered
#
# not_inches <- function(x, smallest = 50, tallest = 84) {
#     inches <- suppressWarnings(as.numeric(x))
#     ind <- is.na(inches) | inches < smallest | inches > tallest
#     ind
# }
# In this function, what TWO types of values are identified as not being correctly formatted in inches?
#
# Values that specifically contain apostrophes (‘), periods (.) or quotations (“).
# Values that result in NA’s when converted to numeric   <-*
# Values less than 50 inches or greater than 84 inches   <-*
# Values that are stored as a character class, because most are already classed as numeric.


# Question 2
# Which of the following arguments, when passed to the function not_inches, would return the vector c(FALSE)?
#
# c(175)
# c(“5’8\””)
# c   <-*
# c(85) (the height of Shaquille O'Neal in inches)


# Question 3
# Our function not_inches returns the object ind. Which answer correctly describes ind?
#
# ind is a logical vector of TRUE and FALSE, equal in length to the vector x (in the arguments list). TRUE indicates that a height entry is incorrectly formatted.    <-*
# ind is a logical vector of TRUE and FALSE, equal in length to the vector x (in the arguments list). TRUE indicates that a height entry is correctly formatted.
# ind is a data frame like our reported_heights table but with an extra column of TRUE or FALSE. TRUE indicates that a height entry is incorrectly formatted.
# ind is a numeric vector equal to reported_heights$heights but with incorrectly formatted heights replaced with NAs.



# Regex
# Show all the entries that use 'cm'
str_subset( reported_heights$height, 'cm' )

# Show all entries that use 'cm' or 'inches'
yes <- c( '180 cm', '70 inches' )
no <- c( '180', "70''" )
s <- c( yes, no )
str_detect( s, 'cm' ) | str_detect( s, 'inches' )

str_detect( s, 'cm|inches' )

# \d identifies any digit, we use a previous '\' to allow '\d' works
yes <- c( '5', '6', "5'10", '5 feet', "4'11" )
no <- c( '', '.', 'Five', 'six' )
s <- c( yes, no )
pattern <- '\\d'
str_detect( s, pattern )


# str_view
# shows the first match for each string
library( htmlwidgets )
str_view( s, pattern )
str_view_all( s, pattern )


# Question 1
# Given the following code
#
# > s
# [1] "70"       "5 ft"     "4'11"     ""         "."        "Six feet"
# What pattern vector yields the following result?
#
#     str_view_all(s, pattern)
# 70
# 5 ft
# 4’11
# .
# Six feet
#
# pattern <- "\\d|ft   <-*
# pattern <- "\d|ft"
# pattern <- "\\d\\d|ft"
# pattern <- "\\d|feet"




# Character Classes, Anchors, and Qualifiers
# We define character clases with the square brackets '[]'
# If we want the parent to match only if we have a 5 or a 6
str_view( s, '[56]' )
# If we want to match values between 4 and 7, we use ranges
str_view( s, '[0-9]' )   # Equivalent to use the \\d

yes <- as.character( 4:7 )
no <- as.character( 1:3 )
s <- c( yes, no )
str_detect( s, '[4-7]' )

# in regex everything is a character, there are no numbers

# The pattern to match when we have exactly one digit
# We must use 'anchors' which let us define patterns that must start or end
#    at specific places.  The two most common are:
#    '^'   -> beginning of string
#    '$'   -> end of string
# Example:  '^\\d$'   -> start of the string followed by 1 digit folloed by the
#                           end of the string
pattern <- '^\\d$'
yes <- c( '1', '5', '9' )
no <- c( '12', '13', ' 1', 'a4', 'b' )
s <- c( yes, no )
str_view( s, pattern )


# For the inches part, we can have one or two digits.  This can be specified in
#    regex with quantifiers.  This is done by following the pattern by curly
#    brackets with the possible number of times the previuous entry repeats
#    Ex: \\d{1,2}

pattern <- '^\\d{1,2}'
yes <- c( '1', '5', '9' )
no <- c( '12', '13', ' 1', 'a4', 'b' )
s <- c( yes, no )
str_view( s, pattern )


# To look for one feet and inches pattern, we can add the symbol for feet "'" and
#    the symbol for inches '"'.
pattern <- '^[4-7]\'\\d{1,2}"$'   # starts this string, the any number between
                                  #    4 and 7, the other feet, then the symbol
                                  #    for feet, then eigher one or two digits,
                                  #    and then the end of the string

yes <- c( '5\'7"', '6\'2"', '5\'12"' )
no <- c( '6,2"', '6.2"', 'I am 5\'11"', '3\'2"', '64' )
str_detect( yes, pattern )
str_detect( no, pattern )



# Question 1
# You enter the following set of commands into your R console. What is your printed result?
#
# > animals <- c("cat", "puppy", "Moose", "MONKEY")
# > pattern <- "[a-z]"
# > str_detect(animals, pattern)
#
# TRUE
# TRUE TRUE TRUE TRUE
# TRUE TRUE TRUE FALSE   <-*
# TRUE TRUE FALSE FALSE



# Question 2
# You enter the following set of commands into your R console. What is your printed result?
#
#     > animals <- c("cat", "puppy", "Moose", "MONKEY")
# > pattern <- "[A-Z]$"
# > str_detect(animals, pattern)
#
# FALSE FALSE FALSE FALSE
# FALSE FALSE TRUE TRUE
# FALSE FALSE FALSE TRUE   <-*
# TRUE TRUE TRUE FALSE



# Question 3
# You enter the following set of commands into your R console. What is your printed result?
#
# > animals <- c("cat", "puppy", "Moose", "MONKEY")
# > pattern <- "[a-z]{4,5}"
# > str_detect(animals, pattern)
#
# FALSE TRUE TRUE FALSE   <-*
# TRUE TRUE FALSE FALSE
# FALSE FALSE FALSE TRUE
# TRUE TRUE TRUE FALSE



# Search and Replace with Regex

pattern <- '^[4-7]\'\\d{1,2}"$'
sum( str_detect( problems, pattern ) )

problems[ c( 2, 10, 11, 12, 15 ) ] %>% str_view( pattern )

# We see that only two of them match.
# A first problem we see inmediately is that som students wrote out the words
#    feet and inches
str_subset( problems, 'inches' )
str_subset( problems, "''" )   # Entries that use single quotes twice
                               #    to represent inches, instead of double quotes

# First step:
# To replace the different ways of representing inches and feet with a uniform
#    symbol.  We'oll use a single quiote for feet, and for inches, we'll simply
#    not use anything
# If we no longer use the inches symbol at the end, we can change our pattern
#    accordingly by taking it out of the pattern
pattern <- '^[4-7]\'\\d{1,2}$'
problems %>%
    str_replace( 'feet|ft|foot', "'" ) %>%
    str_replace( 'inches|in|\'\'|"', '' ) %>%
    str_detect( pattern ) %>%
    sum

# We must eliminate all spaces, and in regex we can represent space with \s

# Quantifiers:
#     - For zero or more instances   -> *
#     - For none or once             -> ?
#     - For one or more              -> +

yes <- c( 'AB', 'A1B', 'A11B', 'A111B', 'A1111B' )
no <- c( 'A2B', 'A21B' )
str_detect( yes, 'A1*B' )
str_detect( no, 'A1-B' )


data.frame( string = c( 'AB', 'A1B', 'A11B', 'A111B', 'A1111B' ),
            none_or_more = str_detect( yes, 'A1*B' ),
            none_or_once = str_detect( yes, 'A1?B' ),
            once_or_more = str_detect( yes, 'A1+B' ) )


pattern <- '^[4-7]\\s*\'\\s*\\d{1,2}$'
problems %>%
    str_replace( 'feet|ft|foot', "'" ) %>%      # replace feet, ft, foot with '
    str_replace( 'inches|in|\'\'|"', '' ) %>%   # remove all inches symbols
    str_detect( pattern ) %>%
    sum

# Question 1
# Given the following code
#
# animals <- c("moose", "monkey", "meerkat", "mountain lion")
# Which TWO “pattern” vectors would yield the following result?
#
#     > str_detect(animals, pattern)
# [1] TRUE TRUE TRUE TRUE
#
# pattern <- “mo*”   <-*
# pattern <- “mo?”   <-*
# pattern <- “mo+”
# pattern <- “moo*”



# Question 2
# You are working on some data from different universities. You have the following vector
#
# > schools
# [1] "U. Kentucky"                 "Univ New Hampshire"          "Univ. of Massachusetts"      "University Georgia"
# [5] "U California"                "California State University"
# You want to clean this data to match the full names of each university
#
# > final
# [1] "University of Kentucky"      "University of New Hampshire" "University of Massachusetts" "University of Georgia"
# [5] "University of California"    "California State University"
# What of the following commands could accomplish this?
#
# schools %>%
#     str_replace("Univ\\.?|U\\.?", "University ") %>%
#     str_replace("^University of |^University ", "University of ")

# schools %>%         <-*
#     str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>%
#     str_replace("^University of |^University ", "University of ")

# schools %>%
#     str_replace("^Univ\\.\\s|^U\\.\\s", "University") %>%
#     str_replace("^University of |^University ", "University of ")

# schools %>%
#     str_replace("^Univ\\.?\\s|^U\\.?\\s", "University") %>%
#     str_replace("University ", "University of ")



# Groups with Regex
# The second large group of problematic entries were of the form:
#       x.y
#       x,y
#       x y
# We want to change all these to our common format:
#       x'y
# But we can't just do the search and replace, because we would change values
#    such as 70.5 into 70'5
# Groups are a powerful aspect of regex that permits the extraction of values.
# Groups are defined using parentheses
pattern_without_groups <- '^[4-7],\\d*$'
pattern_with_groups <- '^([4-7]),(\\d*)$'
yes <- c( '5,9', '5,11', '6,', '6,1' )
no <- c( '5\'9', ',', '2,8', '6.1.1' )
s <- c( yes, no )
str_detect( s, pattern_without_groups )
str_detect( s, pattern_with_groups )

# We can use a function str_match to extract the values these groups define
str_match( s, pattern_with_groups )
str_extract( s, pattern_with_groups )

# str_extract   -> extracts only strings that march a pattern, not the values
#                     defined by the groups

# The special character tor the i-th group is \\i  -> 'i' is the group number
# Example: Replace a comma by a period, but only if it is betwen two digits
pattern_with_groups <- '^([4-7]),(\\d*)$'
yes <- c( '5,9', '5,11', '6,', '6,1' )
no <- c( '5\'9', ',', '2,8', '6.1.1' )
s <- c( yes, no )
str_replace( s, pattern_with_groups, '\\1\'\\2' )

# Pattern to convert all:
#       x.y
#       x,y
#       x y
# into> x y' format
pattern_with_groups <- '^([4-7])\\s*[,\\.\\s+]\\s*(\\d)$'
# ^       -> start of the string
# [4-7]   -> one digit, either 4, 5, 6 or 7
# \\s*    -> none or more white space
# [,\\.\\s+]   -> feet symbol is either ',', '.' or at least one space
# \\s*    -> none or more withe space
# (\\d)   -> none or more digits
# $       -> end of the string
str_subset( problems, pattern_with_groups ) %>% head

str_subset( problems, pattern_with_groups ) %>%
    str_replace( pattern_with_groups, '\\1\'\\2\'' ) %>%
    head


# Question 1
# Rather than using the pattern_with_groups vector from the video, you accidentally write in the following code
#
# problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
# pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
# str_replace(problems, pattern_with_groups, "\\1'\\2")
# What is your result?
#
#
# [1] "5'3" "5'5" "6 1" "5 .11" "5, 12"   <-*
# [1] "5.3" "5,5" "6 1" "5 .11" "5, 12"
# [1] "5'3" "5'5" "6'1" "5 .11" "5, 12"
# [1] "5'3" "5'5" "6'1" "5’11" "5’12"



# Question 2
# You notice your mistake and correct your pattern regex to the following
#
# problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
# pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
# str_replace(problems, pattern_with_groups, "\\1'\\2")
# What is your result?
#
# [1] "5'3" "5'5" "6 1" "5 .11" "5, 12"
# [1] "5.3" "5,5" "6 1" "5 .11" "5, 12"
# [1] "5'3" "5'5" "6'1" "5 .11" "5, 12"   <-*
# [1] "5'3" "5'5" "6'1" "5’11" "5’12"



# Testing and Improving
# Write a function that captures all the entries that can't be converted into numbers,
#   remembering that some are in centimeters
not_inches_or_cm <- function( x, smallest = 50, tallest = 84 ) {
    inches <- suppressWarnings( as.numeric( x ) )
    ind <- !is.na( inches ) &
        ( ( inches >= smallest & inches <= tallest ) |
              (inches / 2.54 >= smallest & inches / 2.54 <= tallest ) )
    !ind
}
head( reported_heights, 1 )
problems <- reported_heights %>%
    filter( not_inches_or_cm( height ) ) %>%
    .$height
length( problems )

# How many of these we can make fit our pattern after the several processing
#    steps we have developed
converted <- problems %>%
    str_replace( 'feet|foot|ft', "'" ) %>%      # convert feet symbols to '
    str_replace( "inches|in|''|\"", '' ) %>%   # remove inches symbols
    str_replace( '^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$', "\\1'\\2" ) # change format
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect( converted, pattern )
mean( index )

# We are amtching more than half now.
# Let's examine the remaining cases
converted[ !index ]

# What are the problems?
#    1. Many strudents measuring exactly 5 or 6 feet did not enter any inches
#    2. Some strudents measuring exactly 5 or 6 feet, entered just that number
#    3. Some of the inches were entered with decimal points
#    4. Some entries have spaces at the end
#    5. Some entries are in meters and some use European decimals
#    6. Two strudents added 'cm'
#    7. One student spelled out the numbers



# Question 1
# In our example, we use the following code to detect height entries that do not match our pattern of x’y”.
#
# converted <- problems %>%
#     str_replace("feet|foot|ft", "'") %>%
#     str_replace("inches|in|''|\"", "") %>%
#     str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
#
# pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
# index <- str_detect(converted, pattern)
# converted[!index]
# Which answer best describes the differences between the regex string we use as an argument in str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
#
# And the regex string in pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"?
#
#
# The regex used in str_replace looks for either a comma, period or space between the feet and inches digits, while the pattern regex just looks for an apostrophe; the regex in str_replace allows for one or more digits to be entered as inches, while the pattern regex only allows for one or two digits.
# The regex used in str_replace allows for additional spaces between the feet and inches digits, but the pattern regex does not.
# The regex used in str_replace looks for either a comma, period or space between the feet and inches digits, while the pattern regex just looks for an apostrophe; the regex in str_replace allows none or more digits to be entered as inches, while the pattern regex only allows for the number 1 or 2 to be used.
# The regex used in str_replace looks for either a comma, period or space between the feet and inches digits, while the pattern regex just looks for an apostrophe; the regex in str_replace allows for none or more digits to be entered as inches, while the pattern regex only allows for one or two digits.     <-*




# Question 2
# 1 point possible (graded)
# You notice a few entries that are not being properly converted using your str_replace and str_detect code
#
# yes <- c("5 feet 7inches", “5 7”)
# no <- c("5ft 9 inches", "5 ft 9 inches")
# s <- c(yes, no)
#
# converted <- s %>%      <-*
#     str_replace("feet|foot|ft", "'") %>%
#     str_replace("inches|in|''|\"", "") %>%
#     str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
#
# pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
# str_detect(converted, pattern)
# [1]  TRUE FALSE FALSE
# It seems like the problem may be due to spaces around the words feet|foot|ft and inches|in. What is another way you could fix this problem?
#
#
#
#     converted <- s %>%
#     str_replace("\\s*(feet|foot|ft)\\s*", "'") %>%
#     str_replace("\\s*(inches|in|''|\")\\s*", "") %>%
#     str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
#
# converted <- s %>%
#     str_replace("\\s+feet|foot|ft\\s+”, "'") %>%
#                 str_replace("\\s+inches|in|''|\"\\s+", "") %>%
#                 str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
#
#                 converted <- s %>%
#                 str_replace("\\s*|feet|foot|ft", "'") %>%
#                 str_replace("\\s*|inches|in|''|\"", "") %>%
#                 str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
#
#                 converted <- s %>%
#                 str_replace_all(“\\s”, “”) %>%
#                 str_replace("\\s|feet|foot|ft", "'") %>%
#                 str_replace("\\s|inches|in|''|\"", "") %>%
#                 str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")





# Using Groups and Quantifiers
# Four clear patterns of entries have arisen along with some other minor problems:
#
# Many students measuring exactly 5 or 6 feet did not enter any inches. For example, 6' - our pattern requires that inches be included.
# Some students measuring exactly 5 or 6 feet entered just that number.
# Some of the inches were entered with decimal points. For example 5'7.5''. Our pattern only looks for two digits.
# Some entires have spaces at the end, for example 5 ' 9.
# Some entries are in meters and some of these use European decimals: 1.6, 1,7.
# Two students added cm.
# One student spelled out the numbers: Five foot eight inches.
# It is not necessarily clear that it is worth writing code to handle all these cases since they might be rare enough. However, some give us an opportunity to learn some more regex techniques so we will build a fix.
#
# Case 1
# For case 1, if we add a '0 to, for example, convert all 6 to 6'0, then our pattern will match. This can be done using groups using the following code:
#
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")
# The pattern says it has to start (^), be followed with a digit between 4 and 7, and then end there ($). The parenthesis defines the group that we pass as \\1 to the replace regex.
#
# Cases 2 and 4
# We can adapt this code slightly to handle case 2 as well which covers the entry 5'. Note that the 5' is left untouched by the code above. This is because the extra ' makes the pattern not match since we have to end with a 5 or 6. To handle case 2, we want to permit the 5 or 6 to be followed by no or one symbol for feet. So we can simply add '{0,1} after the ' to do this. We can also use the none or once special character ?. As we saw previously, this is different from * which is none or more. We now see that this code also handles the fourth case as well:
#
str_replace(s, "^([56])'?$", "\\1'0")
# Note that here we only permit 5 and 6 but not 4 and 7. This is because heights of exactly 5 and exactly 6 feet tall are quite common, so we assume those that typed 5 or 6 really meant either 60 or 72 inches. However, heights of exactly 4 or exactly 7 feet tall are so rare that, although we accept 84 as a valid entry, we assume that a 7 was entered in error.
#
# Case 3
# We can use quantifiers to deal with case 3. These entries are not matched because the inches include decimals and our pattern does not permit this. We need allow the second group to include decimals and not just digits. This means we must permit zero or one period . followed by zero or more digits. So we will use both ? and *. Also remember that for this particular case, the period needs to be escaped since it is a special character (it means any character except a line break).
#
# So we can adapt our pattern, currently ^[4-7]\\s*'\\s*\\d{1,2}$ to permit a decimal at the end:
#
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"
# Case 5
# Case 5, meters using commas, we can approach similarly to how we converted the x.y to x'y. A difference is that we require that the first digit is 1 or 2:
#
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
# We will later check if the entries are meters using their numeric values.
#
# Trimming
# In general, spaces at the start or end of the string are uninformative. These can be particularly deceptive because sometimes they can be hard to see:
#
s <- "Hi "
cat(s)
identical(s, "Hi")
# This is a general enough problem that there is a function dedicated to removing them: str_trim.
#
str_trim("5 ' 9 ")
# To upper and to lower case
# One of the entries writes out numbers as words: Five foot eight inches. Although not efficient, we could add 12 extra str_replace to convert zero to 0, one to 1, and so on. To avoid having to write two separate operations for Zero and zero, One and one, etc., we can use the str_to_lower function to make all words lower case first:
#
s <- c("Five feet eight inches")
str_to_lower(s)
# Putting it into a function
# We are now ready to define a procedure that handles converting all the problematic cases.
#
# We can now put all this together into a function that takes a string vector and tries to convert as many strings as possible to a single format. Below is a function that puts together the previous code replacements:
#
convert_format <- function(s){
    s %>%
        str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
        str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
        str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
        str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
        str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
        str_trim() #remove extra space
}
# We can also write a function that converts words to numbers:
#
words_to_numbers <- function(s){
    str_to_lower(s) %>%
        str_replace_all("zero", "0") %>%
        str_replace_all("one", "1") %>%
        str_replace_all("two", "2") %>%
        str_replace_all("three", "3") %>%
        str_replace_all("four", "4") %>%
        str_replace_all("five", "5") %>%
        str_replace_all("six", "6") %>%
        str_replace_all("seven", "7") %>%
        str_replace_all("eight", "8") %>%
        str_replace_all("nine", "9") %>%
        str_replace_all("ten", "10") %>%
        str_replace_all("eleven", "11")
}
# Now we can see which problematic entries remain:
#
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

# Question 1

# s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
# tab <- data.frame(x = s)
# If you use the extract code from our video, the decimal point is dropped. What modification of the code would allow you to put the decimals in a third column called “decimal”?
#
# extract(data = tab, col = x, into = c(“feet”, “inches”, “decimal”),
#        regex = "(\\d)'(\\d{1,2})(\\.)?"
#
# extract(data = tab, col = x, into = c("feet", "inches", "decimal"),
#        regex = "(\\d)'(\\d{1,2})(\\.\\d+)"
#
# extract(data = tab, col = x, into = c("feet", "inches", "decimal"),
#        regex = "(\\d)'(\\d{1,2})\\.\\d+?"
#
# extract(data = tab, col = x, into = c("feet", "inches", "decimal"),
#        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")     <-*



# Separate with Regex
# Extract and save the feet and number value, so taht we can convert them to inches
#    when appropiate
s <- c( "5'10", "6'1" )
tab <- data.frame( x = s )
tab

# We can use this code to separate out the feet part and the inches part
tab %>% separate( x, c( 'feet', 'inches' ), sep = "'" )

tab %>% extract( x, c( 'feet', 'inches' ), regex = '(\\d)\'(\\d{1,2})')

# So, why do we even need the new function extract?
# The groups in regesx give us much flexibility
s <- c( "5'10", "6'1\"", "5'8inches" )
tab <- data.frame( x = s )
tab

tab %>% separate( x, c( 'feet', 'inches' ), sep = '\'', fill = 'right' )

tab %>% extract( x, c( 'feet', 'inches' ), regex = '(\\d)\'(\\d{1,2})')

# Putting it All Together
# We are now ready to put everything we've done so far together and wrangle our reported heights data as we try to recover as many heights as possible. The code is complex but we will break it down into parts.
#
# We start by cleaning up the height column so that the heights are closer to a feet'inches format. We added an original heights column so we can compare before and after.
#
# Let's start by writing a function that cleans up strings so that all the feet and inches formats use the same x'y format when appropriate.
#
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>%
    mutate(original = height,
           height = words_to_numbers(height) %>% convert_format()) %>%
    extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>%
    mutate_at(c("height", "feet", "inches"), as.numeric) %>%
    mutate(guess = 12*feet + inches) %>%
    mutate(height = case_when(
        !is.na(height) & between(height, smallest, tallest) ~ height, #inches
        !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
        !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
        !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
        TRUE ~ as.numeric(NA))) %>%
    select(-guess)
# We can check all the entries we converted using the following code:
#
new_heights %>%
filter(not_inches(original)) %>%
select(original, height) %>%
arrange(height) %>%
View()
# Let's take a look at the shortest students in our dataset using the following code:
#
# new_heights %>% arrange(height) %>% head(n=7)
# We see heights of 53, 54, and 55. In the original heights column, we also have 51 and 52. These short heights are very rare and it is likely that the students actually meant 5'1, 5'2, 5'3, 5'4, and 5'5. But because we are not completely sure, we will leave them as reported.





# String Splitting
# Suppose we did not have the function read underscore CSV available to use.
# Suppose that we instead have to read a CSV file using the base R
# function read lines like this.

filename <- system.file( 'extdata/murders.csv', package = 'dslabs' )
lines <- readLines( filename )
lines %>% head()


# We want to extract the values that are separated by commas for each string in
#    the vector.
x <- str_split( lines, ',' )
x %>% head()


# Note that the first entry has a column name.  So we can separate that out
col_names <- x[[ 1 ]]
col_names
x <- x[ -1 ]
x %>% head()

# To convert our list into a data frame, we can use a shortcut provided by the
#    map function in the purrr package.
# The map function applies the same function to each element in a list.
# extract the first entry of each element in x,
library( purrr )
map( x, function( y ) y[ 1 ] ) %>% head


# The map function applies the same function to each element in a list.
# So if we want to extract the first entry of each element in x, we can write
#    the following code using the map function.
# However, because this is such a common task, purrr provides a shortcut.
# If the second argument, instead of a function, receives an integer,
# it assumes that we want that entry.
map( x, 1 ) %>% head()


# To force map to return a character vector instead of a list, we can use
#    map underscore chr.
# Similarly, map underscore int returns integers.

dat <- data.frame( map_chr( x, 1 ),
                   map_chr( x, 2 ),
                   map_chr( x, 3 ),
                   map_chr( x, 4 ),
                   map_chr( x, 5 ) ) %>%
    mutate_all( parse_guess ) %>%
    setNames( col_names )
dat %>% head()

dat <- x %>%
    transpose() %>%
    map( ~ parse_guess( unlist( . ) ) ) %>%
    setNames( col_names ) %>%
    as.data.frame()
dat %>% head()


x <- str_split( lines, ',', simplify = TRUE )
col_names <- x[ 1, ]
x <- x[ -1, ]
x %>% as_data_frame() %>%
    setNames( col_names ) %>%
    mutate_all( parse_guess )


# Question 1
# You have the following table
#
# >schedule
# day		staff
# Monday		Mandy, Chris and Laura
# Tuesday		Steve, Ruth and Frank
# You want to turn this into a more useful data frame.
#
# Which two commands would properly split the text in the “staff” column into each individual name? Select ALL that apply.
#
# str_split(schedule$staff, ",|and")
# str_split(schedule$staff, ", | and ")          <-*
# str_split(schedule$staff, ",\\s|\\sand\\s")    <-*
# str_split(schedule$staff, "\\s?(,|and)\\s?")



# Question 2
# 1 point possible (graded)
# You have the following table
#
# > schedule
# day           staff
# Monday 	Mandy, Chris and Laura
# Tuesday 	Steve, Ruth and Frank
# What code would successfully turn your “Schedule” table into the following tidy table
#
# < tidy
# day     staff
# <chr>   <chr>
#     Monday  Mandy
# Monday  Chris
# Monday  Laura
# Tuesday Steve
# Tuesday Ruth
# Tuesday Frank
#
#
# tidy <- schedule %>%      <-*
#     mutate(staff = str_split(staff, ", | and ")) %>%
#     unnest()
#
#
# tidy <- separate(schedule, staff, into = c("s1","s2","s3"), sep = “,”) %>%
#     gather(key = s, value = staff, s1:s3)
#
#
# tidy <- schedule %>%
#     mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>%
#     unnest()




# Case Study: Extracting a Table from a PDF
# One of the datasets provided in dslabs shows scientific funding rates by gender in the Netherlands:
#
library(dslabs)
data("research_funding_rates")
research_funding_rates
# The data come from a paper published in the prestigious journal PNAS. However, the data are not provided in a spreadsheet; they are in a table in a PDF document. We could extract the numbers by hand, but this could lead to human error. Instead we can try to wrangle the data using R.
#
# Downloading the data
# We start by downloading the PDF document then importing it into R using the following code:
#
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
txt
file.remove(temp_file)
# If we examine the object text we notice that it is a character vector with an entry for each page. So we keep the page we want using the following code:
#
raw_data_research_funding_rates <- txt[2]
raw_data_research_funding_rates

# The steps above can actually be skipped because we include the raw data in the dslabs package as well:
#
data("raw_data_research_funding_rates")

# Looking at the download
# Examining this object,
#
raw_data_research_funding_rates %>% head
# we see that it is a long string. Each line on the page, including the table rows, is separated by the symbol for newline: \n.
#
# We can therefore can create a list with the lines of the text as elements:
#
tab <- str_split(raw_data_research_funding_rates, "\n")
tab
# Because we start off with just one element in the string, we end up with a list with just one entry:
#
tab <- tab[[1]]
# By examining this object,
#
tab %>% head
# we see that the information for the column names is the third and forth entires:
#
the_names_1 <- tab[3]
the_names_2 <- tab[4]
# In the table, the column information is spread across two lines. We want to create one vector with one name for each column. We can do this using some of the functions we have just learned.
#
# Extracting the table data
# Let's start with the first line:
#
# the_names_1
# We want to remove the leading space and everything following the comma. We can use regex for the latter. Then we can obtain the elements by splitting using the space. We want to split only when there are 2 or more spaces to avoid splitting success rate. So we use the regex \\s{2,} as follows:
#
the_names_1 <- the_names_1 %>%
str_trim() %>%
str_replace_all(",\\s.", "") %>%
str_split("\\s{2,}", simplify = TRUE)
the_names_1
# Now let's look at the second line:
#
#     the_names_2
# Here we want to trim the leading space and then split by space as we did for the first line:
#
the_names_2 <- the_names_2 %>%
str_trim() %>%
str_split("\\s+", simplify = TRUE)
the_names_2
# Now we can join these to generate one name for each column:
#
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
str_to_lower() %>%
str_replace_all("\\s", "_")
the_names
# Now we are ready to get the actual data. By examining the tab object, we notice that the information is in lines 6 through 14. We can use str_split again to achieve our goal:
#
new_research_funding_rates <- tab[6:14] %>%
str_trim %>%
str_split("\\s{2,}", simplify = TRUE) %>%
data.frame(stringsAsFactors = FALSE) %>%
setNames(the_names) %>%
mutate_at(-1, parse_number)
new_research_funding_rates %>% head()
# We can see that the objects are identical:
#
identical(research_funding_rates, new_research_funding_rates)




# Recoding
# if you have a really long name for your levels, and you will be displaying them
#    in plots, you might want to use shorter versions of the names.

library( dslabs )
data( "gapminder" )

# Suppose we want to show the life expectancy time series for countries in
#    the Caribbean.
gapminder %>%
    filter( region == 'Caribbean' ) %>%
    ggplot( aes( year, life_expectancy, color = country ) ) +
    geom_line()


# Countries with long names
gapminder %>%
    filter( region == 'Caribbean' ) %>%
    filter( str_length( country ) >= 12 ) %>%
    distinct( country )

# Change long names to short names
gapminder %>%
    filter( region == 'Caribbean' ) %>%
    mutate( country = recode( country,
                              `Antigua and Barbuda` = 'Barbuda',
                              `Dominican Republic` = 'DR',
                              `St. Vincent and the Grenadines` = 'St. Vincent',
                              `Trinidad and Tobago` = 'Trinidad' ) ) %>%
    ggplot( aes( year, life_expectancy, color = country ) ) +
    geom_line()



# Question 1
# Using the gapminder data, you want to recode countries longer than 12 letters in the region “Middle Africa” to their abbreviations in a new column, “country_short”. Which code would accomplish this?
#
#
#
# dat <- gapminder %>% filter(region == "Middle Africa") %>%
#     mutate(recode(country,
#                   "Central African Republic" = "CAR",
#                   "Congo, Dem. Rep." = "DRC",
#                   "Equatorial Guinea" = "Eq. Guinea"))
#
#
# dat <- gapminder %>% filter(region == "Middle Africa") %>%
#     mutate(country_short = recode(country,
#                                   c("Central African Republic", "Congo, Dem. Rep.", "Equatorial Guinea"),
#                                   c("CAR", "DRC", "Eq. Guinea")))
#
#
# dat <- gapminder %>% filter(region == "Middle Africa") %>%
#     mutate(country = recode(country,
#                             "Central African Republic" = "CAR",
#                             "Congo, Dem. Rep." = "DRC",
#                             "Equatorial Guinea" = "Eq. Guinea"))
#
#
# dat <- gapminder %>% filter(region == "Middle Africa") %>%     <-*
#     mutate(country_short = recode(country,
#                                   "Central African Republic" = "CAR",
#                                   "Congo, Dem. Rep." = "DRC",
#                                   "Equatorial Guinea" = "Eq. Guinea"))





# Date, Times and Text Mining
# In the Dates, Times, and Text Mining section, you will learn how to deal with dates and times in R and also how to generate numerical summaries from text data.

# After completing this section, you will be able to:
#
#     Handle dates and times in R.
# Use the lubridate package to parse dates and times in different formats.
# Generate numerical summaries from text data and apply data visualization and analysis techniques to those data.
# There are comprehension checks that follow most videos.
#
# We encourage you to use R to interactively test out your answers and further your own learning. If you get stuck, we encourage you to search the discussion boards for the answer to your issue or ask us for help!


library( dslabs )
data( "polls_us_election_2016" )
polls_us_election_2016$startdate %>% head()
class( polls_us_election_2016$startdate )

# Convert date to a number
as.numeric( polls_us_election_2016$startdate ) %>% head()

polls_us_election_2016 %>%
    filter( pollster == 'Ipsos' & state == 'U.S.' ) %>%
    ggplot( aes( startdate, rawpoll_trump ) ) +
    geom_line()

library( lubridate )
# Take a random sample of dates
set.seed( 2 )
dates <- sample( polls_us_election_2016$startdate, 10 ) %>% sort
dates

# Extract dates values
data.frame( date = days( dates ),
            month = month( dates ),
            day = day( dates ),
            year = year( dates ) )

# Extract month labels
month( dates, label = TRUE )

# Parser   -> Convert string to date
x <- c( 20090101, '2009-01-02', '2009 01 03', '2009-1-4', '2009-1, 5', 'Created on 2009 1 6', '200901 !!! 07')
ymd( x )


x <- '09/01/02'
ymd( x )
mdy( x )
dmy( x )
dym( x )

# Get current time
Sys.time()

# Find time zone
now()
now( 'GMT' )
OlsonNames()

# Extract hours, minutes and seconds from time
now() %>% hour()
now() %>% minute()
now() %>% second()

# Parse strings into time
x <- c( '12:34:56' )
hms( x )

x <- 'Nov/2/2012 12:34:56'
mdy_hms( x )

# Question 1
# Which of the following is the standard ISO 8601 format for dates?
#
# MM-DD-YY
# YYYY-MM-DD    <-*
# YYYYMMDD
# YY-MM-DD
# unanswered



# Question 2
# Which of the following commands could convert this string into the correct date format?
#
#     dates <- c("09-01-02", "01-12-07", "02-03-04")
#
# ymd(dates)
# mdy(dates)
# dmy(dates)
# It is impossible to know which format is correct without additional information.   <-*




# Text Mining
# With the exception of labels used to represent categorical data, we have focused on numerical data, but in many applications data starts as text. Well known examples are spam filtering, cyber-crime prevention, counter-terrorism and sentiment analysis.
#
# In all these examples, the raw data is composed of free form texts. Our task is to extract insights from these data. In this section, we learn how to generate useful numerical summaries from text data to which we can apply some of the powerful data visualization and analysis techniques we have learned.
#
# Case study: Trump Tweets
# During he 2016 US presidential election then candidate Donald J. Trump used his tweeter account as a way to communicate with potential voters. On August 6, 2016 Todd Vaziri tweeted about Trump that "Every non-hyperbolic tweet is from iPhone (his staff). Every hyperbolic tweet is from Android (from him)." Data scientist David Robison conducted an analysis to determine if data supported this assertion. Here we go through David's analysis to learn some of the basics of text mining. To learn more about text mining in R we recommend this book.
#
# We will use the following libraries

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

# In general, we can extract data directly from twitter using the \emph{rtweet} package. However, in this case, a group has already compiled data for us and made it available at http://www.trumptwitterarchive.com. We
#

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
filter(!is_retweet & !str_detect(text, '^"')) %>%
mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST"))

# For convenience we include the result of the code above in the dslabs package:

library(dslabs)
data("trump_tweets")

# This is data frame with information about the tweet:

head(trump_tweets)

# The variables that are included are

names(trump_tweets)

# The help file ?trump_tweets provides details on what each variable represents. The tweets are represented by the textvariable:

trump_tweets %>% select(text) %>% head

# and the source variable tells us the device that was used to compose and upload each tweet:

trump_tweets %>% count(source) %>% arrange(desc(n))

# We can use extract to remove the Twitter for part of the source and filter out retweets.

trump_tweets %>%
extract(source, "source", "Twitter for (.*)") %>%
count(source)

# We are interested in what happened during the campaign, so for the analysis here we will focus on what was tweeted between the day Trump announced his campaign and election day. So we define the following table:

campaign_tweets <- trump_tweets %>%
extract(source, "source", "Twitter for (.*)") %>%
filter(source %in% c("Android", "iPhone") &
created_at >= ymd("2015-06-17") &
created_at < ymd("2016-11-08")) %>%
filter(!is_retweet) %>%
arrange(created_at)

# We can now use data visualization to explore the possibility that two different groups were tweeting from these devices. For each tweet, we will extract the hour, in the east coast (EST), it was tweeted then compute the proportion of tweets tweeted at each hour for each device.

ds_theme_set()
campaign_tweets %>%
    mutate(hour = hour(with_tz(created_at, "EST"))) %>%
    count(source, hour) %>%
    group_by(source) %>%
    mutate(percent = n / sum(n)) %>%
    ungroup %>%
    ggplot(aes(hour, percent, color = source)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "Hour of day (EST)",
         y = "% of tweets",
         color = "")
# We notice a big peak for the Android in early hours of the morning, between 6 and 8 AM. There seems to be a clear different in these patterns. We will therefore assume that two different entities are using these two devices. Now we will study how their tweets differ. To do this we introduce the tidytext package.
#
# Text as data
# The tidytext package helps us convert free from text into a tidy table. Having the data in this format greatly facilitates data visualization and applying statistical techniques.

library(tidytext)

# The main function needed to achieve this is unnest_tokens. A token refers to the units that we are considering to be a data point. The most common token will be words, but they can also be single characters, ngrams, sentences, lines or a pattern defined by a regex. The functions will take a vector of strings and extract the tokens so that each one gets a row in the new table. Here is a simple example:

example <- data_frame(line = c(1, 2, 3, 4),
text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

# Now let's look at a quick example with a tweet number 3008

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>%
unnest_tokens(word, text) %>%
select(word)

# Note that the function tries to convert tokens into words and strips characters important to twitter such as # and @. A token in twitter is not the same as in regular english. For this reason instead of using the default, words, we define a regex that captures twitter character. The pattern appears complex but all we are defining is a patter that starts with @, # or neither and is followed by any combination of letter or digits:

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# We can now use the unnest_tokens function with the regex option and appropriately extract the hashtags and mentions:

campaign_tweets[i,] %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    select(word)

# Another minor adjustment we want to make is remove the links to pictures:

campaign_tweets[i,] %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    select(word)

# Now we are now read to extract the words for all our tweets.

tweet_words <- campaign_tweets %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern)

# And we can now answer questions such as "what are the most commonly used words?"

tweet_words %>%
    count(word) %>%
    arrange(desc(n))

# It is not surprising that these are the top words. The top words are not informative. The tidytext package has database of these commonly used words, referred to as stop words, in text mining:

stop_words

# If we filter out rows representing stop words with filter(!word %in% stop_words$word)

tweet_words <- campaign_tweets %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    filter(!word %in% stop_words$word )

# we end up with a much more informative set of top 10 tweeted words

tweet_words %>%
    count(word) %>%
    top_n(10, n) %>%
    mutate(word = reorder(word, n)) %>%
    arrange(desc(n))

# Some exploration of the resulting words (not show here) reveals a couple of unwanted characteristics in our tokens. First, some of our tokens are just numbers (years for example). We want to remove these and we can find them using the regex ^\d+$. Second, some of our tokens come from a quote and they start with '. We want to remove the ' when it's at the start of a word so we will use str_replace. We add these two lines to the code above to generate are final table:

tweet_words <- campaign_tweets %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    filter(!word %in% stop_words$word &
               !str_detect(word, "^\\d+$")) %>%
    mutate(word = str_replace(word, "^'", ""))

# Now that we have all our words in a table, along with information about what device was used to compose the tweet they came from, we can start exploring which words are more common when comparing Android to iPhone.
# For each word we want to know if it is more likely to come from an Android tweet or an iPhone tweet. We previously introduced the odds ratio a summary statistic useful for quantifying these differences. We each device and a given word, let's call it y, we compute the odds or the ratio between the proportion of words that are y and not y and compute the ratio of those odds. Here we will have many proportions that are 0 so we use the 0.5 correction.

android_iphone_or <- tweet_words %>%
    count(word, source) %>%
    spread(source, n, fill = 0) %>%
    mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))

android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#               Given that several of these words are overall low frequency words we can impose a filter based on the total frequency like this:

android_iphone_or %>% filter(Android+iPhone > 100) %>%
    arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
    arrange(or)

# We already see somewhat of a pattern in the types of words that are being tweeted more in one device versus the other. However, we are not interested in specific words but rather in the tone. Vaziri's assertion is that the Android tweets are more hyperbolic. So how can we check this with data? Hyperbolic is a hard sentiment to extract from words as it relies on interpreting phrases. However, words can be associated to more basic sentiment such as as anger, fear, joy and surprise. In the next section we demonstrate basic sentiment analysis.

# Sentiment Analysis
# In sentiment analysis we assign a word to one or more "sentiment". Although this approach will miss context dependent sentiments, such as sarcasm, when performed on large numbers of words, summaries can provide insights.

# The first step in sentiment analysis is to assign a sentiment to each word. The tidytext package includes several maps or lexicons in the object sentiments:

table(sentiments$lexicon)

# The bing lexicon divides words into positive and negative. We can see this using the tidytext function get_sentiments:

get_sentiments("bing")

# The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive.

get_sentiments("afinn")

# The loughran and nrc lexicons provide several different sentiments:

get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

# To start learning about how these lexicons were developed read this help file ?sentiments.

# For the analysis here we are interested in exploring the different sentiments of each tweet so we will use the nrc lexicon:

nrc <- sentiments %>%
    filter(lexicon == "nrc") %>%
    select(word, sentiment)

# We can combine the words and sentiments using inner_join, which will only keep words associated with a sentiment. Here are 10 random words extracted from the tweets:

tweet_words %>% inner_join(nrc, by = "word") %>%
    select(source, word, sentiment) %>% sample_n(10)

# Now we are ready to perform a quantitative analysis comparing Android and iPhone by comparing the sentiments of the tweets posted from each device. Here we could perform a tweet by tweet analysis, assigning a sentiment to each tweet. However, this somewhat complex since each tweet will have several sentiments attached to it, one for each word appearing in the lexicon. For illustrative purposes we will perform a much simpler analysis: we will count and compare the frequencies of each sentiment appears for each device.

sentiment_counts <- tweet_words %>%
    left_join(nrc, by = "word") %>%
    count(source, sentiment) %>%
    spread(source, n) %>%
    mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

# Because more words were used on the Android than on the phone:

tweet_words %>% group_by(source) %>% summarize(n = n())

# For each sentiment we can compute the odds of being in the device: proportion of words with sentiment versus proportion of words without and then compute the odds ratio comparing the two devices

sentiment_counts %>%
    mutate(Android = Android / (sum(Android) - Android), iPhone = iPhone / (sum(iPhone) - iPhone), or = Android/iPhone) %>%
    arrange(desc(or))

# So we do see some difference and the order is interesting: the largest three sentiments are disgust, anger, and negative! But are they statistically significant? How does this compare if we are just assigning sentiments at random?

# To answer that question we can compute, for each sentiment, an odds ratio and confidence interval. We will add the two values we need to form a two-by-two table and the odd rat

library(broom)
log_or <- sentiment_counts %>%
    mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
            se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
            conf.low = log_or - qnorm(0.975)*se,
            conf.high = log_or + qnorm(0.975)*se) %>%
    arrange(desc(log_or))

log_or

# A graphical visualization shows some sentiments that are clearly overrepresented:

log_or %>%
    mutate(sentiment = reorder(sentiment, log_or),) %>%
    ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
    geom_errorbar() +
    geom_point(aes(sentiment, log_or)) +
    ylab("Log odds ratio for association between Android and sentiment") +
    coord_flip()

# We see that the disgust, anger, negative sadness and fear sentiments are associated with the Android in a way that is hard to explain by chance alone. Words not associated to a sentiment were strongly associated with the iPhone source, whic is in agreement with the original claim about hyperbolic tweets.

# If we are interested in exploring which specific words are driving these differences, we can back to our android_iphone_or object:

android_iphone_or %>% inner_join(nrc) %>%
    filter(sentiment == "disgust" & Android + iPhone > 10) %>%
    arrange(desc(or))

# We can make a graph

android_iphone_or %>% inner_join(nrc, by = "word") %>%
    mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
    mutate(log_or = log(or)) %>%
    filter(Android + iPhone > 10 & abs(log_or)>1) %>%
    mutate(word = reorder(word, log_or)) %>%
    ggplot(aes(word, log_or, fill = log_or < 0)) +
    facet_wrap(~sentiment, scales = "free_x", nrow = 2) +
    geom_bar(stat="identity", show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
