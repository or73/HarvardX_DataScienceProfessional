# Grades analysis in R
# Suppose you're a student in a prestigious academy that only allows the
#    country's best 200 students every year. Just for fun, you decide to
#    analyze the grades of this academy. The highest possible grade is 100,
#    the lowest possible grade is 0.

# You have access to your own grades, to anonymized grades for all of your 199
#    classmates, and for the 200 pupils in the previous 4 years. This data is
#    available in your workspace as me, other_199, and previous_4, respectively.
#    Have a look at these variables in the console.

# To format our data so that it'd be easier to work with and analyze later on,
#    let's merge the three datasets to one called last_5, with dimensions 200
#    by 5. That is, last_5 contains all 200 scores from the last 5 classes.

# INSTRUCTIONS
# Use c() to combine the data in me and other_199 (in this order). Store the
#    result in my_class.
# Build a 200-by-5 matrix named last_5: use cbind() to combine the vector my_class
#    side-by-side with the 200-by-4 matrix previous_4. my_class should be on the
#    left, and previous_4 should be on the right.
# Use colnames() on last_5 to give the columns the names year_1, up to year_5,
#    in this order. To get you started, a character vector nms that you can use
#    is shown in the script.

# me, other_199, and previous_4 are available in your workspace
me   # [1] 89
other_199
previous_4

# Merge me and other_199: my_class
my_class <- c( me, other_199 )

# cbind() my_class and previous_4: last_5
last_5 <- cbind( my_class, previous_4 )

# Name last_5 appropriately
nms <- paste0("year_", 1:5)
nms
colnames( last_5 ) <- nms

# /--------------/

# Explore your data
# To get a good feel for your data, it's a good idea to make some visualizations
#    and make some summaries.

# The object me is still preloaded, as are the objects my_class and last_5 that
#    you built yourself. Up to you to write some exploration code!

# INSTRUCTIONS
# Use hist() to create a histogram of my_class. Can you tell in which bin your
#    grade (the variable me) is?
# Generate a summary of last_5. What do these numbers tell you?
# Maybe a boxplot is more informative here; use boxplot() on last_5 and try
#    to read the resulting graph.

# me, my_class and last_5 are available in your workspace

# Build histogram of my_class
hist( my_class )

# Generate summary of last_5
summary( last_5 )

# Build boxplot of last_5
boxplot( last_5 )

# /--------------/

# Understand your data
# Have another look at a colored version of the histogram you created in the
#    previous exercise. What's the color of the bin in which your grade (me) lies?

# INSTRUCTIONS
# Possible Answers
# red
# blue
# green
# yellow   <-

# /--------------/

# Basic queries
# Remember relational operators? Here's a short list to freshen your memory:

# == - Equality
# != - Inequality
# > - Greater than
# < - Less than
# >= - Greater than or equal to
# <= - Less than or equal to
# In this exercise, you'll be writing queries to answer questions about me,
#    my_class and last_5. These variables are available in your workspace. The result of your queries #    should be a vector or a matrix of logicals.

# INSTRUCTIONS
# Is your grade equal to 72?
# Which grades in your class are higher than 75?
# Which grades in the last 5 years are below or equal to 64?

# me, my_class and last_5 are preloaded

# Is your grade equal to 72?
me == 72

# Which grades in your class are higher than 75?
my_class > 75
my_class[ my_class > 75 ]

# Which grades in the last 5 years are below or equal to 64?
last_5 <= 64
last_5[ last_5 <= 64 ]

# /--------------/

# Build aggregates
# Answering the question which grades in your class are higher than 75? with a
#    vector of logicals is not very insightful. It's much better to ask the
#    question how many grades in your class are higher than 75? instead.

# You can answer such questions with the sum() function. Each TRUE you pass
#    count as 1, each FALSE as 0. Just in the same way, you can use mean();
#    this will give you the proportion of TRUE values in the data structure
#    you pass it.

# INSTRUCTIONS
# How many grades in your class are higher than 75?
# How many students in your class scored strictly higher than you?
# What's the proportion of grades below or equal to 64 in the last five years?

# me, my_class and last_5 are preloaded

# How many grades in your class are higher than 75?
length( my_class[ my_class > 75 ] )

# How many students in your class scored strictly higher than you?
length( my_class[ my_class > me ] )

# What's the proportion of grades below or equal to 64 in the last 5 years?
length( last_5[ last_5 <= 64 ] ) / length( last_5 )

# /--------------/

# Logical operator
# Next to relational operators, there are also logical operators, to combine logicals:

# & - AND
# | - OR
# ! - NOT
# Try to answer the following questions such that the output of your code is logical:

# INSTRUCTIONS
# Is your grade greater than 87 and smaller than or equal to 89?
# Which grades in your class are below 60 or above 90?

# me, my_class and last_5 are preloaded

# Is your grade greater than 87 and smaller than or equal to 89?
me > 87  & me <= 89

# Which grades in your class are below 60 or above 90?
my_class < 60 | my_class > 90

# /--------------/

# Build aggregates (2)
# This exercise sums it all up: use relational operators, logical operators,
#    and functions such as sum() and mean() to come up with the solution!

# INSTRUCTIONS
# What's the proportion of grades in your class that is average,
#    i.e. greater than or equal to 70 and lower than or equal to 85?
# How many students in the last 5 years had a grade of exactly 80 or 90?

# me, my_class and last_5 are preloaded

# What's the proportion of grades in your class that is average?
avg <- my_class >= 70 & my_class <= 85
length( my_class[ avg ] ) / length( my_class )

# How many students in the last 5 years had a grade of 80 or 90?

length( last_5[ last_5 == 80 | last_5 == 90 ] )

# /--------------/

# if, else
# As a refresher of the syntax, have a look at this example:

#     if (me > 80) {
#         print("Good student!")
#     } else {
#         print("Better luck next year!")
#     }
# Your grade, me, equals 89, so the condition in the if statement evaluates to
#    TRUE, and print("Good student!") is executed.

# Note that the else part should come on the same line as the closing bracket
#    of the if statement! If you don't do this, R will not understand your code!
#
# What is the output if this control structure is run in case me equals 80?
#
# INSTRUCTIONS
# Possible Answers
# [1] "Good student!"
# [1] "Better luck next year!"   <-
# It gives an error, as me is exactly 80.

# /--------------/

# if, else: DIY
# With the insight acquired from the previous exercise, coding your own if-else
#    statement will be a walk in the park.

# The variables you have been working with all along - me, my_class, and last_5
#    - are available in your workspace.
#
# Note that the else part should come on the same line as the closing bracket
#    of the if statement! If you don't do this, R will not understand your code!
#
# INSTRUCTIONS
# Assign to n_smart the number of grades in my_class that are greater than or equal to 80.
# Write an if statement that prints out "smart class" if n_smart is greater than 50.
# Extend the if statement with an else clause that prints out "rather average".

# me, my_class and last_5 are preloaded

# Define n_smart
n_smart <- length( my_class[ my_class >= 80 ] )
n_smart

# Code the if-else construct
if ( n_smart > 50 ) {
    print( 'smart class' )
} else {
    print( 'rather average' )
}

# /--------------/

# else if
# You can further customize your if-else constructs with an else if statement:
#
#     if (condition) {
#         expr
#     } else if (condition) {
#         expr
#     } else {
#         expr
#     }
# Remember here that as soon as R encounters a condition that evaluates to TRUE,
#    the corresponding expr is executed and the control structure is abandoned.
#
# Note that the else if and else parts should come on the same line as the
#    closing bracket of the previous statement! If you don't do this, R will
#    not understand your code!
#
# INSTRUCTIONS
# Assign to prop_less the proportion of students whose grade - stored in
#    my_class - was lower than yours.
# Write a control construct with the following properties:
# if prop_less exceeds 0.9, print out "you're among the best 10 percent".
# if the above is not the case, but prop_less exceeds 0.8, print out "you're
#    among the best 20 percent".
# if the above two don't hold, print out "need more analysis".

# me, my_class and last_5 are preloaded

# Define prop_less
prop_less <- length( my_class[ my_class < me ] ) / length( my_class )
prop_less
# Code the control construct
if ( prop_less > 0.9 ) {
    print( "you're among the best 10 percent" )
} else if ( prop_less > 0.8 ) {
    print( "you're among the best 20 percent" )
} else {
    print( "need more analysis" )
}

# /--------------/

# Embed if-else clauses
# An example of embedded control structures is included in the sample code.
# There's a top-level if-else construct and there are other if-else constructs
#    inside the statements. However, there's still something wrong here.
#
# INSTRUCTIONS
# Fix the control structure such that:
#
# The body of the top-level if condition is executed if the average score of
#    your class is strictly below 75.
# There are no syntax errors.

# me, my_class and last_5 are preloaded

# Embedded control structure: fix the error
if ( mean( my_class ) < 75 ) {
    if ( mean(my_class) > me ) {
        print("average year, but still smarter than me")
    } else {
        print("average year, but I'm not that bad")
    }
} else {
    if (mean(my_class) > me) {
        print("smart year, even smarter than me")
    } else {
        print("smart year, but I am smarter")
    }
}

# /--------------/

# Operations and controls expertise
# Thinking about your time at school, you remember the wide range of students
#    in your class. In this exercise, let's try blending everything together
#    to see whether there were more high achievers than low achievers in your
#    class. You will not receive too much help from the feedback messages...
#    you're pretty much on your own here!
#
# INSTRUCTIONS
# Create a sub-vector of my_class that only contains the grades that are greater
#    than or equal to 85. Call this vector top_grades.
# Create a similar sub-vector, but this time with the grades of my_class that
#    are strictly under 65. Call this vector worst_grades.
# Create a conditional statement that prints out "top grades prevail" if the
#    length of top_grades exceeds that of worst_grades. Don't include an else statement.

# me, my_class and last_5 are preloaded

# Create top_grades
top_grades <- my_class[ my_class >= 85 ]
top_grades
# Create worst_grades
worst_grades <- my_class[ my_class < 65 ]

# Write conditional statement
if ( length( top_grades ) > length( worst_grades ) ) {
    print( 'top grades prevail')
}

# /--------------/

# Scanning Logs in R
# Imagine you're a data scientist in a huge chemical company. Business is going
#    well, but in the last couple of days, some unexpected errors occurred at
#    your plant. You decide to dig through the log files from the last 4 days.
#    You find that approximately every hour, the plant's monitoring system has
#    produced a control message. It is up to you to analyze it in R.
#
# The data is stored as a list logs, which is loaded in your workspace.
# Let's do some exploring.
#
# INSTRUCTIONS
# Display the structure of logs. It appears to be a list of lists, interesting.
# Use list subsetting to print out the details component of the 11th element
#    of logs.
# Use class() to print out the class of the timestamp component of the first
#    entry of logs.

# logs is already available in your workspace
logs
# Print the structure of logs
str( logs )

# Use list subsetting to print the details part of 11th logs entry
str( logs[ 11 ] )
log11 <- logs[ 11 ]
log11[[1]]$details

# Print the class of the timestamp component of the first entry
class( logs[ 1 ][[ 1 ]]$timestamp )

# /--------------/

# Understand the logs
# The previous exercise already gave you a good feeling for the data in logs.
#    Which of the statements concerning this data is NOT correct? logs is
#    available in your workspace so you can play around with it in the console.
#
# INSTRUCTIONS
# Possible Answers
# The classes of the timestamp elements of each list entry are POSIXct and POSIXt.
# When success is TRUE for a single log entry, the details element is a list
#    of length one. When success is FALSE, the details list contains two elements.
# There are 100 log entries in the logs list.  <--
# You can use logs[[c(2, 2, 1)]], which is equivalent to logs[[2]][[2]][[1]]
#    to select the message of the second log entry.

# /--------------/

# While: start easy
# As a refresher, have a look at this while loop recipe:
#
#     while (condition) {
#         expr
#     }
# The expr part gets executed over and over again, as long as condition
#    evaluates to TRUE. Remember that condition should become FALSE at
#    one point, otherwise your loop will go on indefinitely!
#
# As a data scientist, suppose you want to know how many entries there were
#    before the first failure message. One way to go about this is to write a
#    simple while loop that goes through each log entry in logs, prints out the
#    entry number, and stops when it sees the first failure message, that is,
#    when the success element is FALSE. The last number printed to the console
#    is the number of entries before the first failure message was logged.
#
# INSTRUCTIONS
# Start with defining an iterator, i, equal to 1, outside of the while loop.
# Inside the while loop's condition, check if the success element of logs[[i]] is TRUE.
# Inside the while loop's condition:
#     First print out i and
# Next, increase i by 1. This is important!

# logs is available in your workspace

# Initialize the iterator i to be 1
i <- 1

# Code the while loop
while ( logs[[i]]$success ) {
    print( i )
    i = i + 1
}

# /--------------/

# Adapt the while loop
# In the previous exercise, you iterated through the logs until you found a log
#    that indicates a failure, i.e. the success component is FALSE. Here, you'll
#    further extend this while loop so that you can read the message for every
#    log entry before the first failure; maybe there were warning messages that
#    we've missed! Instead of simply printing i, print out the message, available
#    inside the details element of each log entry.
#
# The sample code already contains the solution to the previous exercise. It's
#    up to you to make the appropriate changes. You can always use str(logs) to
#    have a look at the structure of logs.
#
# INSTRUCTIONS
# Do not change how i is initialized or updated.
# Change the print(i) call with a call that prints the message element of the details element of each log entry.

# logs is available in your workspace
str( logs )
logs[1][[1]]$details
# Adapt the while loop
i <- 1
while (logs[[i]]$success) {
    print( logs[ i ][[ 1 ]]$details$message )
    i <- i + 1
}

# /--------------/

# While: different approach
# To answer more complicated questions, you'll have to work with additional
#    temporary variables that indicate whether you found what you're looking for
#    while going through the logs. This temporary variable comes in addition to
#    the iterator, i, that you use to access subsequent log entries.
#
# Suppose you have a meeting with your colleagues from other sectors of the
#    company and find there may be problems in the waste department.
#    To investigate this, you need to search through the while loop with the
#    iterator i again, but this time you need to keep looking until you find a
#    failure that occurred at the waste department, that is, where location
#    inside the details element of the log equals "waste". Follow the instructions
#    step-by-step to get there.
#
# The ___ parts in the sample code should be replaced with valid R code.
#
# INSTRUCTIONS
# Initialize two variables: i, to 1 as before, and found, to FALSE.
# Write a while loop that:
#     keeps running as long as found is FALSE.
# Checks if logs[[i]]$success is FALSE and logs[[i]]$details$location equals
#    "waste". You'll need to use the && sign here. This causes the evaluation
#    of the condition to halt as soon as the result of the condition is known;
#    if logs[[i]]$success is TRUE, it's certain that the condition will be FALSE,
#    so the next comparison is not evaluated anymore.
# prints out "found" and sets found to TRUE if the above condition holds.
# prints out "still looking" and increment i if the above condition does not hold.

# Initialize i and found
i <- 1
found <- FALSE

# Code the while loop
while ( !found ) {
    if ( logs[[ i ]]$success == FALSE && logs[[ i ]]$details$location == 'waste' ) {
        print( 'found' )
        found <- TRUE
    } else {
        print( 'still looking' )
        i = i + 1
    }

# /--------------/

# The for loop
# Let's do a quick review. The while loop keeps executing code until its
#    condition evaluates to FALSE. The for loop, on the other hand, iterates
#    over a sequence, where a looping variable changes for each iteration,
#    according to the sequence. Have a look at the following that prints the
#    value of each element in vec in two different ways:
#
# vec <- c(2, 3, 5, 7, 11, 13)
#
# # Option 1
# for (el in vec) {
# print(el)
# }
#
# # Option 2
# for (i in 1:length(vec)) {
# print(vec[i])
# }
# In this exercise and in the following exercises, you'll again be working
#    with the chemical plant logs that's loaded in your workspace as logs.
#    Here, you'll write a for loop that prints when each log entry was logged.
#
# INSTRUCTIONS
# Build a for loop that separately prints the timestamp element of each log
#    entry in the logs list. You can do this in several ways:
#
# By looping over logs: for(log in logs). At each iteration, log will be
#    an entry of the logs list.
# By using a looping index: for(i in 1:length(logs)). You can then use
#    logs[[i]] to access the log entry.

# logs is available in your workspace

# Code a for loop that prints the timestamp of each log
for ( log in logs ) {
    print( log$timestamp )
}

# /--------------/

# Going through the list
# You can use an if-else construct inside a for loop. This allows you to loop
#    through the log entries and perform more specific tasks.
#
# In the sample code on the right, a for loop that solved the previous exercise
#    is included. It's up to you to extend it so that you know when each
#    successful log entry was logged.
#
# INSTRUCTIONS
# Add an if statement such that the for loop only prints the timestamp if the
#    log in question represents a success, i.e. if its success element is TRUE.

# logs is available in your workspace

# Make the printout conditional: only if success
for (log in logs) {
    if ( log$success == TRUE ) {
        print(log$timestamp)
    }
}

# /--------------/

# Adapt the logs list
# Apart from doing simple printouts in a for loop, remember that it's possible
#    to adapt data structures inside a for loop.
#
# As a data scientist, you know it's always helpful to convert any time-related
#    data to a Date object. So, in this case, we want to convert the timestamp
#    to a Date object. How could we go about this? Using the for (log in logs)
#    approach here won't work, because log is a local copy of an element in logs.
#    To actually access and change the elements in the logs list, you will need
#    to use the looping index.
#
# The sample code already includes a for loop without a body, can you complete it?
#
# INSTRUCTIONS
# Finish the for loop in the sample code; after running it, each entry in logs
#    should contain a date element. This should be a Date object that you can
#    build from the timestamp element with as.Date().
# Print the first 6 elements in logs by calling the head() function on logs.
# Is the date information in there now?

# logs is available in your workspace

# Finish the for loop: add date element for each entry
for ( i in 1:length( logs ) ) {
    # print( logs[[ i ]]$timestamp )
    logs[[i]]$date <- as.Date( logs[[ i ]]$timestamp )
}

# Print first 6 elements in logs
head( logs, 6 )

# /--------------/

# Collect all failures
# In the previous exercise, you adapted the data structure you were looping over.
# Remember, it's also possible to build a new data structure altogether inside
#    your for loop.
#
# Your plant manager approaches you and asks for a report on all failures that
#    are available in the logs list. Instead of the entire list, she is only
#    interested in the failures. Get to work to generate what she asks for!
#
# Just a tip before you get to it: If you have a list of lists a and want to add
#    a list b to it, you can use c(a, list(b)).
#
# INSTRUCTIONS
# 100 XP
# Initialize an empty list, failures using the list() function without arguments.
# Finish the for loop such that each log entry that indicates a failure is added
#    to failures.
# Display the structure of the failures list that results.

# logs is available in your workspace
head( logs, 3 )
# Intialize empty list: failures
failures <- list()

# Finish the for loop: add each failure to failures
for (log in logs) {
    print( log$success )
    if ( log$success == FALSE ) {
        failures <- c( failures, list( log ) )
    }
}

# Display the structure of failures
str(failures)

# /--------------/

# Using functions
# Recall that when you call a function, R matches your input parameters with
#    its function arguments, either by value or by position, then executes the
#    function body. Function arguments can have default values: if you do not
#    specify these arguments, R will take the default value.
#
# Remember the chemical plant logs stored as logs? logs is a list of log entries;
#    each entry itself is also a list containing a bunch of information regarding
#    measurements in the plant. logs is still available in your workspace so feel
#    free to refresh your memory.
#
# You wonder when the most recent failure was logged. The timestamps vector,
#    which contains the timestamps of logs indicating a failure, is already
#    available in your workspace. It's up to you to use the max() function to
#    find the most recent timestamp. You can check the function's documentation
#    by typing ?max in the console.
#
# INSTRUCTIONS
# Call max() on timestamps to find the latest timestamp.
# Write a one-liner that converts the latest timestamp to a Date object. You can use as.Date() for this.

# logs is available in your workspace
head( timestamps )

# Call max() on timestamps
max( timestamps )

# What is the date of the latest timestamp?
as.Date( max( timestamps ) )

# /--------------/

# Optional Arguments
# Due to some irregularities in the logging system, some of the timestamps for
#    failures are missing. The vector timestamps that was gathered from all
#    failures is available in your workspace. You will see that some timestamps
#    are NA, which is R's way of denoting a missing value.
#
# max() has a default way of handling NA values in a vector: it simply returns NA.
# You can have max() ignore NA values by controlling the na.rm argument.
#
# INSTRUCTIONS
# Inspect the contents of timestamps by simply printing them.
# Call max() on timestamps without specifying additional arguments.
# Again call max() on timestamps but set the na.rm argument appropriately.
# What is the difference?

# A faulty version of timestamps is available in your workspace

# Print out timestamps
timestamps

# Call max() on timestamps, no additional arguments
max( timestamps )

# Call max() on timestamps, specify na.rm
max( timestamps, na.rm = TRUE )

# /--------------/

# Which call is valid?
# Because of the way positional and named arguments work in R, you can call
#    functions in several ways.
#
# Take the nchar() function, for example. You can use this function to count
#    the number of characters in a character vector, such as in vec that is
#    available in your workspace.
#
# Only one of the calls of nchar() below is valid; can you guess which one?
# Have a look at the function's documentation first with
#
# ?nchar
# before trying each command in the console.
#
# INSTRUCTIONS
# Possible Answers
# nchar(keepNA = TRUE, allowNAs = FALSE, x = vec)
# nchar(vec, FALSE)
# nchar(keepNA = TRUE, vec)   <-
# nchar(x)

# /--------------/

# Extract log information (1)
# By now you've seen quite some examples of how data is extracted from the logs
#    list with a for loop. As the plant's data scientist, you'll often want to
#    extract information from the list, be it the messages, the timestamp,
#    whether or not it was a success, etc. You could write a dedicated for loop
#    for each one of them, but this is rather tedious. Why not wrap it in a
#    function? Let's start simple and make your function more awesome step-by-step.
#
# Remember you can define your own function as follows:
#
#     my_fun <- function(arg1, arg2) {
#         body
#     }
# The for loop that extracts the timestamp information for each log is included
#    on the right. This time, the logs list doesn't contain any missing
#    information like it did in the previous exercises.
#
# INSTRUCTIONS
# Skeleton code for extract_info() is already provided. This function should
#    take a list of logs as the input (the x argument), and return() a vector
#    with all the logs' timestamps. Place the for loop that has already been
#    coded in the function body. You'll also have to rename variables to make
#    it work. Good luck!
# Call extract_info() on logs; simply print out the result, no need to store it
#    in a variable.

# logs is available in your workspace

# for loop to extract timestamp; put this inside function body below
info <- c()
for (log in logs) {
    info <- c(info, log$timestamp)
}

# Build a function extract_info(): use for loop, add return statement
extract_info <- function(x) {
    info <- c()
    # body
    for ( log in x ) {
        info <- c( info, log$timestamp )
    }
    return( info )
}

# /--------------/

# Extract log information (2)
# In the previous exercise, you wrote a function extract_info(), which is
#    available on the right. To make this function really powerful, you'll want
#    to add an additional argument, property, so that you can select any
#    property (ie. success, details, or timestamp) from the log entries.
#
# Next, you can use this argument to subset the list accordingly. You cannot
#    use the $ notation if the element you want to select is a variable and not
#    the actual name of a list:
#
# log$property # won't work
# log[[property]] # will work
# INSTRUCTIONS
# 100 XP
# Add an additional argument to the function, called property.
# Adapt the function body such that property is used to select the correct
#    information from each log.
# Call extract_info() on logs and set the property argument to "timestamp".
# Call extract_info() on logs and set the property argument to "success".

# logs is available in your workspace

# Adapt the extract_info() function.
extract_info <- function( x, property ) {
    info <- c()
    for ( log in x ) {
        info <- c( info, log[[ property ]] )
    }
    return( info )
}

# Call extract_info() on logs, set property to "timestamp"
extract_info( logs, 'timestamp' )
extract_info( logs, 'success' )

# /--------------/

# Extract log information (3)
# The property argument in the previous exercises did not have a default value.
# This causes R to throw an error if you call extract_info() without specifying
#    the property argument. Time to add this default value and see how your
#    function behaves.
#
# INSTRUCTIONS
# In the definition of extract_info(), set the default value of the property
#    argument to "success".
# Call extract_info() on logs without specifying the property argument.
# Call extract_info() on logs, and set the property argument to "timestamp".
# logs is available in your workspace

# Add default value for property argument
extract_info <- function(x, property = 'success') {
    info <- c()
    for (log in x) {
        info <- c(info, log[[property]])
    }
    return(info)
}

# Call extract_info() on logs, don't specify property
extract_info( logs )
extract_info( logs, 'timestamp' )

# /--------------/

# Extract log information (4)
# In the for loop exercises, you wrote code that extracts information on log
#    entries that indicate a failure. This is something your extract_info()
#    function can not yet do. You can already guess what the purpose of this
#    exercise is, right?
#
# INSTRUCTIONS
# Add an argument to your function extract_info(): call it include_all, and make
#    it TRUE by default. That is, the default is to extract all log entries,
#    whether it indicates a failure or a success.
# Change the body of your function: inside the for loop, add an if test: if
#    include_all or if !log$success, you want to add the log[[property]] to the
#    info vector. In all other cases, you're not adding anything to info.
#    Use the || operator in your condition.
# Call your new extract_info() function on logs, first without any additional
#    arguments. The default value for include_all, which is TRUE, will be used.
# Call extract_info() on logs again; this time set include_all to FALSE.

# logs is available in your workspace

# Adapt extract_info():
# - add argument with default value
# - change function body
extract_info <- function( x, property = "success", include_all = TRUE ) {
    info <- c()
    for ( log in x ) {
        # add if construct around the line below
        if ( include_all || !log$success ) {
            info <- c(info, log[[property]])
        }
    }
    return( info )
}

# Call extract_info() on logs, no additional arguments
extract_info( logs )

# Call extract_info() on logs, set include_all to FALSE
extract_info( logs, include_all = FALSE )

# /--------------/

# Extract log information (5)
# Have another look at logs, that is still available in your workspace. Have you
#    noticed that the details element of each log entry differs between logs
#    indicating success and failure? For successes, it's a list with a message
#    element. For failures, it's a list with two elements: message and location.
#    We've printed out str(logs) for you below. See the differences in
#    structures between a successful log and an unsuccessful log:
#
# $ :List of 3
# ..$ success  : logi TRUE
# ..$ details  :List of 1
# .. ..$ message: chr "all good"
# ..$ timestamp: POSIXct[1:1], format: "2015-09-18 13:45:27"
# $ :List of 3
# ..$ success  : logi FALSE
# ..$ details  :List of 2
# .. ..$ message : chr "human error"
# .. ..$ location: chr "waste"
# ..$ timestamp: POSIXct[1:1], format: "2015-09-17 23:37:18"
# At first sight, our function only allows the selection of log entry
#    information on the first level, such as success and details. To get
#    information that's deeper inside the log entries, such as message, we'll
#    need another function, right? Nope, your function will work just fine.
#    To select elements from embedded lists, you can use chained selection.
#    The following code chunk uses chained selection to return the value 2:
#
# x <- list(a = 1, b = list(r = 2, s = 3))
# x[[c("b", "r")]]
#
# INSTRUCTIONS
# Use extract_info() to build a vector containing the message elements of all
# log entries, irrespective of whether they indicate a failure or not.
# Use extract_info() to build a vector containing the location information for
# log entries indicating a failure. This means you have to set
# include_all = FALSE now!

# logs is available in your workspace
head( logs, 3 )
# Definition of the extract_info() function
extract_info <- function(x, property = "success", include_all = TRUE) {
    info <- c()
    for (log in x) {
        if (include_all || !log$success) {
            info <- c(info, log[[property]])
        }
    }
    return(info)
}

# Generate vector of messages
extract_info(logs, property = c('details', 'message' ) )

# Generate vector of locations for failed log entries
extract_info(logs, property = c('details', 'location' ), include_all = FALSE )

# /--------------/

# Over to you
# Now that you've played around with building up a function, making it more
#    powerful step-by-step, you're ready to write your own function from the
#    bottom with minimal help.
#
# As usual, logs, a list of lists, is available in your workspace to test
#    your function.
#
# INSTRUCTIONS
# Write a function, compute_fail_pct(), that calculates the percentage of log
#    entries that indicate failures.
# It should have one input argument: a list of log entries.
# It should loop over each log entry, counting the number of failures.
#    (Not the number of successes!)
# It should return a single number between 0 and 100: the percentage of failures.
#    For example, if the list of logs has length 50 and contains 5 failures,
#    compute_fail_pct(x) should return 100???5/50=10.
# Call compute_fail_pct() on logs.

# logs is available in your workspace

# Write the function compute_fail_pct

compute_fail_pct <- function( x ) {
    counter_total <- 0
    counter_failures <- 0
    for ( log in x ) {
        if ( !log$success ) {
            counter_failures <- counter_failures + 1
        }
        counter_total <- counter_total + 1
    }
    return( 100 * counter_failures / counter_total )
}

# Call compute_fail_pct() on logs
compute_fail_pct( logs )














#                               --- me ---
# [1] 89

#                               --- other_199 ---
# [1]  66  81  77  90  95  54  78  90  80  71  96  70  81  71  60  91  62  54
# [19]  70  96  90  82  78 100  79  84  79  73  66  63  97  91  82  81  52  73
# [37]  89  70  69  60  63  78  77  73  64  62  61  72  64  87  54  80  82  61
# [55]  70  69  61  88  90  74  80  59  75  65  83  70  83  83  81  80  88  77
# [73]  84  50  73  70  74  75  71  60  62  80  78  81  60  80  99  90  90  66
# [91]  61  79  71  80  69  67  80  59  72  76  74  70  74  96  73  90  92  75
# [109]  77  63  94  67  56  95  85  63  80  96  72  77  79  69  68  70  73  99
# [127]  64  59  63  82  76  90  80  86  77  80  84  80  99  80  68  77  51  67
# [145]  86  60  61  57  62  86  86  75  75  62  60  75  71  70  70  70  75  72
# [163]  79  74  72  78  87  70  77  64  77  67  88  87  80  76  73  78  89  72
# [181]  86  68  84  64  73  73  64  70  92  91  65  69  99  76  94  72  77  80
# [199]  64

#                               --- previous_4 ---
# [,1] [,2] [,3] [,4]
# [1,]   82   89   53   70
# [2,]   66   89   66   76
# [3,]   66   65   85   73
# [4,]   86   79   65   64
# [5,]   76   83   75   56
# [6,]   74   73   69   71
# [7,]   56   79   98   96
# [8,]   80   87   66   78
# [9,]   71   59  100   77
# [10,]   69   77   76   99
# [11,]   69   84   70   89
# [12,]   72   75   89   56
# [13,]   68   70   60   92
# [14,]   63   63   52   73
# [15,]   83   74   80   85
# [16,]   55   70   84   71
# [17,]   87   61   95   67
# [18,]   79   85   68   75
# [19,]   75   75   92   65
# [20,]  100   72   78   90
# [21,]   61   53   55   87
# [22,]   70   80   69   91
# [23,]   66   80   64   77
# [24,]   90   85   95   81
# [25,]   85   75   71   99
# [26,]   56   77   55   98
# [27,]   73   89   80   59
# [28,]   72   73   70   73
# [29,]   81   72   63   56
# [30,]   86   68   80   70
# [31,]   60   74   73   97
# [32,]   88   85   93   50
# [33,]   77   81   62   71
# [34,]   80   76   80   75
# [35,]   57   90   90   95
# [36,]   69   66   61   89
# [37,]   73   87   64   88
# [38,]   61   61   75   93
# [39,]   50   73   92   88
# [40,]   67   60  100   74
# [41,]   98   68   76   61
# [42,]   80   70   85   96
# [43,]   82   84   97   97
# [44,]   69   90   67   82
# [45,]   89   68   72   64
# [46,]   74   74   60   85
# [47,]   89   88   89   71
# [48,]   73   93   91   87
# [49,]   67   66   60   51
# [50,]   67   79   73   68
# [51,]   90   83   87   65
# [52,]   68   77   92   79
# [53,]   64   78   71   96
# [54,]   85   66   72   99
# [55,]   70   56   82   94
# [56,]   78   66   90   80
# [57,]   96   77   63   71
# [58,]   69   99   70   94
# [59,]   74   91   56   64
# [60,]   71   67   75   59
# [61,]   55   63   79   86
# [62,]   62   87   62   70
# [63,]   56   78   98   55
# [64,]   83   70   63   87
# [65,]   65   68   88   75
# [66,]   76   84   66   83
# [67,]   83   70   58   72
# [68,]   77   65   73   69
# [69,]   78   85   67   63
# [70,]   52   70   80   80
# [71,]   75   99   69   85
# [72,]   55   60   52   67
# [73,]   50  100   77   60
# [74,]   58   77   60   70
# [75,]   68   92   70   82
# [76,]   94   82   76   82
# [77,]   90   79   84   90
# [78,]   67   89   98   51
# [79,]   70   50   62   92
# [80,]   58   64   70   86
# [81,]   80   87   79   85
# [82,]   93   63   90   62
# [83,]   65   99   88   90
# [84,]   73   73   60   90
# [85,]   64   85   67   68
# [86,]   56   82   75   98
# [87,]   73   62   80   93
# [88,]   57   64   77   80
# [89,]   65   84   69   75
# [90,]   56   77   72   70
# [91,]   83   71   68   66
# [92,]   72   87   54   87
# [93,]   61   58   90  100
# [94,]   58   70   79   90
# [95,]   56   87   65   85
# [96,]   92   60   71   76
# [97,]   99   90   83  100
# [98,]   68   68   68   52
# [99,]   61   55   60   75
# [100,]   60   85   77   56
# [101,]   97   96   79   74
# [102,]   96   79   62   91
# [103,]   51   80   69   84
# [104,]   95   71   52   81
# [105,]   90   59   64   89
# [106,]   89   80   77   89
# [107,]   72   70   89   80
# [108,]   67   79   80   70
# [109,]   50   75   71   88
# [110,]   80   84   60   77
# [111,]   74   77   77   59
# [112,]   72   70   89   50
# [113,]   70   59   77   56
# [114,]   88   74   82   83
# [115,]   56   73   90   88
# [116,]   79   90   73   77
# [117,]   98   69   72   95
# [118,]   51   80   50   90
# [119,]   66   94   50   83
# [120,]   79   57   80   75
# [121,]   53   99   61   78
# [122,]   53   73   76   76
# [123,]   72   80   89   55
# [124,]   66   74   91   73
# [125,]   87   54   65   82
# [126,]   61   77   86   92
# [127,]   54   84   92   90
# [128,]   70   79   62   71
# [129,]   54   53   87   86
# [130,]   77   89   97   66
# [131,]   60   73   52   73
# [132,]   88   83   76   86
# [133,]   71   85   65   64
# [134,]   90   79   80   71
# [135,]   51   99   71   80
# [136,]   74   54   69   95
# [137,]   74   90   74   69
# [138,]   86   78   98   82
# [139,]   90   70   79   84
# [140,]   80   79   75   84
# [141,]   68   70   50   53
# [142,]   75   98   80   97
# [143,]   63   60   66   78
# [144,]   78   70   55   76
# [145,]   65   70   85   80
# [146,]   73   92   75   61
# [147,]   70   86   78   70
# [148,]   69   59   97   78
# [149,]   72   73   58   70
# [150,]   65   98   86   79
# [151,]   88   72   74   76
# [152,]   65   76   70   86
# [153,]   67   62   67   69
# [154,]   76   82   68   95
# [155,]   68   77   58   55
# [156,]   88   78   80   74
# [157,]   69   84   73   96
# [158,]   83   82   74   86
# [159,]   76   78   67   57
# [160,]   75   76   54   79
# [161,]   51   51   62   70
# [162,]   66   55   76   80
# [163,]   61   82   66   73
# [164,]   58   94   83   84
# [165,]   73   62   72   67
# [166,]   80   81   82   89
# [167,]   96   81   66   88
# [168,]   61   73   66   85
# [169,]   52   99   75   64
# [170,]   74   80   84   70
# [171,]   72   77   81   77
# [172,]   76   80   70   72
# [173,]   78   79   99   55
# [174,]   54   84   84   77
# [175,]   80   76   98   72
# [176,]   79   84   64   73
# [177,]   66   88   68   86
# [178,]   94   75   76   94
# [179,]   63   52   74   50
# [180,]   74   82   90   71
# [181,]   67   73   99   80
# [182,]   84   76   69   74
# [183,]   93   99  100   94
# [184,]   70   73   51   90
# [185,]   94   70   81   84
# [186,]   79   75   99   88
# [187,]   60   67   70  100
# [188,]   60   70   62   66
# [189,]   70   83   52   68
# [190,]   60   88   70   62
# [191,]   63   83   70   71
# [192,]   68   78   73   60
# [193,]   72   57   80   67
# [194,]   90   87   64   88
# [195,]   96   63   73   78
# [196,]   61   60   98   87
# [197,]   66   76   74   78
# [198,]   70   76   61   86
# [199,]   61   77   70   76
# [200,]   63   78   80   94

#                               --- logs ---
# [[1]]
# [[1]]$success
# [1] TRUE
#
# [[1]]$details
# [[1]]$details$message
# [1] "check"
#
# [[1]]$timestamp
# [1] "2015-09-14 23:01:07 UTC"
#
# [[2]]
# [[2]]$success
# [1] TRUE
#
# [[2]]$details
# [[2]]$details$message
# [1] "all good"
#
# [[2]]$timestamp
# [1] "2015-09-15 00:00:13 UTC"
#
# [[3]]
# [[3]]$success
# [1] TRUE
#
# [[3]]$details
# [[3]]$details$message
# [1] "check"
#
# [[3]]$timestamp
# [1] "2015-09-15 01:00:43 UTC"
#
# [[4]]
# [[4]]$success
# [1] TRUE
#
# [[4]]$details
# [[4]]$details$message
# [1] "check"
#
# [[4]]$timestamp
# [1] "2015-09-15 02:01:18 UTC"
#
# [[5]]
# [[5]]$success
# [1] TRUE
#
# [[5]]$details
# [[5]]$details$message
# [1] "ok"
#
# [[5]]$timestamp
# [1] "2015-09-15 02:59:59 UTC"
#
# [[6]]
# [[6]]$success
# [1] TRUE
#
# [[6]]$details
# [[6]]$details$message
# [1] "all good"
#
# [[6]]$timestamp
# [1] "2015-09-15 04:01:08 UTC"
#
# [[7]]
# [[7]]$success
# [1] TRUE
#
# [[7]]$details
# [[7]]$details$message
# [1] "check"
#
# [[7]]$timestamp
# [1] "2015-09-15 05:03:20 UTC"
#
# [[8]]
# [[8]]$success
# [1] TRUE
#
# [[8]]$details
# [[8]]$details$message
# [1] "all good"
#
# [[8]]$timestamp
# [1] "2015-09-15 05:59:25 UTC"
#
# [[9]]
# [[9]]$success
# [1] TRUE
#
# [[9]]$details
# [[9]]$details$message
# [1] "all good"
#
# [[9]]$timestamp
# [1] "2015-09-15 06:59:29 UTC"
#
# [[10]]
# [[10]]$success
# [1] FALSE
#
# [[10]]$details
# [[10]]$details$message
# [1] "stack overflow"
#
# [[10]]$details$location
# [1] "control room"
#
# [[10]]$timestamp
# [1] "2015-09-15 08:00:53 UTC"
#
# [[11]]
# [[11]]$success
# [1] TRUE
#
# [[11]]$details
# [[11]]$details$message
# [1] "ok"
#
# [[11]]$timestamp
# [1] "2015-09-15 08:59:54 UTC"
#
# [[12]]
# [[12]]$success
# [1] TRUE
#
# [[12]]$details
# [[12]]$details$message
# [1] "ok"
#
# [[12]]$timestamp
# [1] "2015-09-15 10:00:39 UTC"
#
# [[13]]
# [[13]]$success
# [1] TRUE
#
# [[13]]$details
# [[13]]$details$message
# [1] "all good"
#
# [[13]]$timestamp
# [1] "2015-09-15 11:03:18 UTC"
#
# [[14]]
# [[14]]$success
# [1] TRUE
#
# [[14]]$details
# [[14]]$details$message
# [1] "check"
#
# [[14]]$timestamp
# [1] "2015-09-15 12:01:49 UTC"
#
# [[15]]
# [[15]]$success
# [1] TRUE
#
# [[15]]$details
# [[15]]$details$message
# [1] "check"
#
# [[15]]$timestamp
# [1] "2015-09-15 13:01:54 UTC"
#
# [[16]]
# [[16]]$success
# [1] TRUE
#
# [[16]]$details
# [[16]]$details$message
# [1] "ok"
#
# [[16]]$timestamp
# [1] "2015-09-15 14:02:27 UTC"
#
# [[17]]
# [[17]]$success
# [1] TRUE
#
# [[17]]$details
# [[17]]$details$message
# [1] "check"
#
# [[17]]$timestamp
# [1] "2015-09-15 15:06:20 UTC"
#
# [[18]]
# [[18]]$success
# [1] TRUE
#
# [[18]]$details
# [[18]]$details$message
# [1] "check"
#
# [[18]]$timestamp
# [1] "2015-09-15 16:05:48 UTC"
#
# [[19]]
# [[19]]$success
# [1] TRUE
#
# [[19]]$details
# [[19]]$details$message
# [1] "all good"
#
# [[19]]$timestamp
# [1] "2015-09-15 17:07:10 UTC"
#
# [[20]]
# [[20]]$success
# [1] FALSE
#
# [[20]]$details
# [[20]]$details$message
# [1] "segmentation fault"
#
# [[20]]$details$location
# [1] "waste"
#
# [[20]]$timestamp
# [1] "2015-09-15 18:07:58 UTC"
#
# [[21]]
# [[21]]$success
# [1] TRUE
#
# [[21]]$details
# [[21]]$details$message
# [1] "check"
#
# [[21]]$timestamp
# [1] "2015-09-15 19:08:24 UTC"
#
# [[22]]
# [[22]]$success
# [1] TRUE
#
# [[22]]$details
# [[22]]$details$message
# [1] "all good"
#
# [[22]]$timestamp
# [1] "2015-09-15 20:10:08 UTC"
#
# [[23]]
# [[23]]$success
# [1] FALSE
#
# [[23]]$details
# [[23]]$details$message
# [1] "human error"
#
# [[23]]$details$location
# [1] "reactor"
#
# [[23]]$timestamp
# [1] "2015-09-15 21:14:16 UTC"
#
# [[24]]
# [[24]]$success
# [1] TRUE
#
# [[24]]$details
# [[24]]$details$message
# [1] "check"
#
# [[24]]$timestamp
# [1] "2015-09-15 22:13:49 UTC"
#
# [[25]]
# [[25]]$success
# [1] TRUE
#
# [[25]]$details
# [[25]]$details$message
# [1] "ok"
#
# [[25]]$timestamp
# [1] "2015-09-15 23:14:57 UTC"
#
# [[26]]
# [[26]]$success
# [1] TRUE
#
# [[26]]$details
# [[26]]$details$message
# [1] "all good"
#
# [[26]]$timestamp
# [1] "2015-09-16 00:16:04 UTC"
#
# [[27]]
# [[27]]$success
# [1] TRUE
#
# [[27]]$details
# [[27]]$details$message
# [1] "check"
#
# [[27]]$timestamp
# [1] "2015-09-16 01:17:27 UTC"
#
# [[28]]
# [[28]]$success
# [1] TRUE
#
# [[28]]$details
# [[28]]$details$message
# [1] "ok"
#
# [[28]]$timestamp
# [1] "2015-09-16 02:17:04 UTC"
#
# [[29]]
# [[29]]$success
# [1] TRUE
#
# [[29]]$details
# [[29]]$details$message
# [1] "ok"
#
# [[29]]$timestamp
# [1] "2015-09-16 03:20:13 UTC"
#
# [[30]]
# [[30]]$success
# [1] TRUE
#
# [[30]]$details
# [[30]]$details$message
# [1] "all good"
#
# [[30]]$timestamp
# [1] "2015-09-16 04:22:47 UTC"
#
# [[31]]
# [[31]]$success
# [1] TRUE
#
# [[31]]$details
# [[31]]$details$message
# [1] "ok"
#
# [[31]]$timestamp
# [1] "2015-09-16 05:23:56 UTC"
#
# [[32]]
# [[32]]$success
# [1] TRUE
#
# [[32]]$details
# [[32]]$details$message
# [1] "ok"
#
# [[32]]$timestamp
# [1] "2015-09-16 06:23:06 UTC"
#
# [[33]]
# [[33]]$success
# [1] TRUE
#
# [[33]]$details
# [[33]]$details$message
# [1] "ok"
#
# [[33]]$timestamp
# [1] "2015-09-16 07:22:06 UTC"
#
# [[34]]
# [[34]]$success
# [1] TRUE
#
# [[34]]$details
# [[34]]$details$message
# [1] "ok"
#
# [[34]]$timestamp
# [1] "2015-09-16 08:21:56 UTC"
#
# [[35]]
# [[35]]$success
# [1] TRUE
#
# [[35]]$details
# [[35]]$details$message
# [1] "check"
#
# [[35]]$timestamp
# [1] "2015-09-16 09:20:00 UTC"
#
# [[36]]
# [[36]]$success
# [1] TRUE
#
# [[36]]$details
# [[36]]$details$message
# [1] "all good"
#
# [[36]]$timestamp
# [1] "2015-09-16 10:21:13 UTC"
#
# [[37]]
# [[37]]$success
# [1] TRUE
#
# [[37]]$details
# [[37]]$details$message
# [1] "ok"
#
# [[37]]$timestamp
# [1] "2015-09-16 11:21:27 UTC"
#
# [[38]]
# [[38]]$success
# [1] TRUE
#
# [[38]]$details
# [[38]]$details$message
# [1] "ok"
#
# [[38]]$timestamp
# [1] "2015-09-16 12:22:18 UTC"
#
# [[39]]
# [[39]]$success
# [1] TRUE
#
# [[39]]$details
# [[39]]$details$message
# [1] "ok"
#
# [[39]]$timestamp
# [1] "2015-09-16 13:24:54 UTC"
#
# [[40]]
# [[40]]$success
# [1] TRUE
#
# [[40]]$details
# [[40]]$details$message
# [1] "ok"
#
# [[40]]$timestamp
# [1] "2015-09-16 14:25:02 UTC"
#
# [[41]]
# [[41]]$success
# [1] TRUE
#
# [[41]]$details
# [[41]]$details$message
# [1] "ok"
#
# [[41]]$timestamp
# [1] "2015-09-16 15:22:59 UTC"
#
# [[42]]
# [[42]]$success
# [1] TRUE
#
# [[42]]$details
# [[42]]$details$message
# [1] "ok"
#
# [[42]]$timestamp
# [1] "2015-09-16 16:23:08 UTC"
#
# [[43]]
# [[43]]$success
# [1] TRUE
#
# [[43]]$details
# [[43]]$details$message
# [1] "check"
#
# [[43]]$timestamp
# [1] "2015-09-16 17:25:20 UTC"
#
# [[44]]
# [[44]]$success
# [1] TRUE
#
# [[44]]$details
# [[44]]$details$message
# [1] "all good"
#
# [[44]]$timestamp
# [1] "2015-09-16 18:25:45 UTC"
#
# [[45]]
# [[45]]$success
# [1] TRUE
#
# [[45]]$details
# [[45]]$details$message
# [1] "all good"
#
# [[45]]$timestamp
# [1] "2015-09-16 19:25:39 UTC"
#
# [[46]]
# [[46]]$success
# [1] TRUE
#
# [[46]]$details
# [[46]]$details$message
# [1] "check"
#
# [[46]]$timestamp
# [1] "2015-09-16 20:25:44 UTC"
#
# [[47]]
# [[47]]$success
# [1] TRUE
#
# [[47]]$details
# [[47]]$details$message
# [1] "check"
#
# [[47]]$timestamp
# [1] "2015-09-16 21:25:37 UTC"
#
# [[48]]
# [[48]]$success
# [1] TRUE
#
# [[48]]$details
# [[48]]$details$message
# [1] "check"
#
# [[48]]$timestamp
# [1] "2015-09-16 22:28:16 UTC"
#
# [[49]]
# [[49]]$success
# [1] TRUE
#
# [[49]]$details
# [[49]]$details$message
# [1] "check"
#
# [[49]]$timestamp
# [1] "2015-09-16 23:28:48 UTC"
#
# [[50]]
# [[50]]$success
# [1] TRUE
#
# [[50]]$details
# [[50]]$details$message
# [1] "check"
#
# [[50]]$timestamp
# [1] "2015-09-17 00:28:57 UTC"
#
# [[51]]
# [[51]]$success
# [1] FALSE
#
# [[51]]$details
# [[51]]$details$message
# [1] "stack overflow"
#
# [[51]]$details$location
# [1] "tubes"
#
# [[51]]$timestamp
# [1] "2015-09-17 01:31:51 UTC"
#
# [[52]]
# [[52]]$success
# [1] TRUE
#
# [[52]]$details
# [[52]]$details$message
# [1] "ok"
#
# [[52]]$timestamp
# [1] "2015-09-17 02:33:47 UTC"
#
# [[53]]
# [[53]]$success
# [1] TRUE
#
# [[53]]$details
# [[53]]$details$message
# [1] "ok"
#
# [[53]]$timestamp
# [1] "2015-09-17 03:32:17 UTC"
#
# [[54]]
# [[54]]$success
# [1] TRUE
#
# [[54]]$details
# [[54]]$details$message
# [1] "all good"
#
# [[54]]$timestamp
# [1] "2015-09-17 04:35:01 UTC"
#
# [[55]]
# [[55]]$success
# [1] TRUE
#
# [[55]]$details
# [[55]]$details$message
# [1] "check"
#
# [[55]]$timestamp
# [1] "2015-09-17 05:37:19 UTC"
#
# [[56]]
# [[56]]$success
# [1] TRUE
#
# [[56]]$details
# [[56]]$details$message
# [1] "ok"
#
# [[56]]$timestamp
# [1] "2015-09-17 06:35:20 UTC"
#
# [[57]]
# [[57]]$success
# [1] TRUE
#
# [[57]]$details
# [[57]]$details$message
# [1] "check"
#
# [[57]]$timestamp
# [1] "2015-09-17 07:36:10 UTC"
#
# [[58]]
# [[58]]$success
# [1] TRUE
#
# [[58]]$details
# [[58]]$details$message
# [1] "ok"
#
# [[58]]$timestamp
# [1] "2015-09-17 08:34:29 UTC"
#
# [[59]]
# [[59]]$success
# [1] TRUE
#
# [[59]]$details
# [[59]]$details$message
# [1] "check"
#
# [[59]]$timestamp
# [1] "2015-09-17 09:34:02 UTC"
#
# [[60]]
# [[60]]$success
# [1] TRUE
#
# [[60]]$details
# [[60]]$details$message
# [1] "ok"
#
# [[60]]$timestamp
# [1] "2015-09-17 10:33:19 UTC"
#
# [[61]]
# [[61]]$success
# [1] TRUE
#
# [[61]]$details
# [[61]]$details$message
# [1] "ok"
#
# [[61]]$timestamp
# [1] "2015-09-17 11:34:41 UTC"
#
# [[62]]
# [[62]]$success
# [1] TRUE
#
# [[62]]$details
# [[62]]$details$message
# [1] "ok"
#
# [[62]]$timestamp
# [1] "2015-09-17 12:33:41 UTC"
#
# [[63]]
# [[63]]$success
# [1] FALSE
#
# [[63]]$details
# [[63]]$details$message
# [1] "segmentation fault"
#
# [[63]]$details$location
# [1] "waste"
#
# [[63]]$timestamp
# [1] "2015-09-17 13:33:43 UTC"
#
# [[64]]
# [[64]]$success
# [1] TRUE
#
# [[64]]$details
# [[64]]$details$message
# [1] "check"
#
# [[64]]$timestamp
# [1] "2015-09-17 14:35:19 UTC"
#
# [[65]]
# [[65]]$success
# [1] FALSE
#
# [[65]]$details
# [[65]]$details$message
# [1] "human error"
#
# [[65]]$details$location
# [1] "waste"
#
# [[65]]$timestamp
# [1] "2015-09-17 15:34:22 UTC"
#
# [[66]]
# [[66]]$success
# [1] TRUE
#
# [[66]]$details
# [[66]]$details$message
# [1] "ok"
#
# [[66]]$timestamp
# [1] "2015-09-17 16:34:35 UTC"
#
# [[67]]
# [[67]]$success
# [1] TRUE
#
# [[67]]$details
# [[67]]$details$message
# [1] "ok"
#
# [[67]]$timestamp
# [1] "2015-09-17 17:35:00 UTC"
#
# [[68]]
# [[68]]$success
# [1] TRUE
#
# [[68]]$details
# [[68]]$details$message
# [1] "all good"
#
# [[68]]$timestamp
# [1] "2015-09-17 18:35:20 UTC"
#
# [[69]]
# [[69]]$success
# [1] TRUE
#
# [[69]]$details
# [[69]]$details$message
# [1] "ok"
#
# [[69]]$timestamp
# [1] "2015-09-17 19:34:08 UTC"
#
# [[70]]
# [[70]]$success
# [1] TRUE
#
# [[70]]$details
# [[70]]$details$message
# [1] "all good"
#
# [[70]]$timestamp
# [1] "2015-09-17 20:33:43 UTC"
#
# [[71]]
# [[71]]$success
# [1] TRUE
#
# [[71]]$details
# [[71]]$details$message
# [1] "check"
#
# [[71]]$timestamp
# [1] "2015-09-17 21:32:27 UTC"
#
# [[72]]
# [[72]]$success
# [1] TRUE
#
# [[72]]$details
# [[72]]$details$message
# [1] "check"
#
# [[72]]$timestamp
# [1] "2015-09-17 22:32:34 UTC"
#
# [[73]]
# [[73]]$success
# [1] TRUE
#
# [[73]]$details
# [[73]]$details$message
# [1] "ok"
#
# [[73]]$timestamp
# [1] "2015-09-17 23:34:26 UTC"
#
# [[74]]
# [[74]]$success
# [1] TRUE
#
# [[74]]$details
# [[74]]$details$message
# [1] "check"
#
# [[74]]$timestamp
# [1] "2015-09-18 00:34:56 UTC"
#
# [[75]]
# [[75]]$success
# [1] TRUE
#
# [[75]]$details
# [[75]]$details$message
# [1] "all good"
#
# [[75]]$timestamp
# [1] "2015-09-18 01:37:54 UTC"
#
# [[76]]
# [[76]]$success
# [1] TRUE
#
# [[76]]$details
# [[76]]$details$message
# [1] "ok"
#
# [[76]]$timestamp
# [1] "2015-09-18 02:36:15 UTC"
#
# [[77]]
# [[77]]$success
# [1] FALSE
#
# [[77]]$details
# [[77]]$details$message
# [1] "human error"
#
# [[77]]$details$location
# [1] "waste"
#
# [[77]]$timestamp
# [1] "2015-09-18 03:37:18 UTC"
#
# [[78]]
# [[78]]$success
# [1] TRUE
#
# [[78]]$details
# [[78]]$details$message
# [1] "ok"
#
# [[78]]$timestamp
# [1] "2015-09-18 04:34:42 UTC"
#
# [[79]]
# [[79]]$success
# [1] TRUE
#
# [[79]]$details
# [[79]]$details$message
# [1] "check"
#
# [[79]]$timestamp
# [1] "2015-09-18 05:34:40 UTC"
#
# [[80]]
# [[80]]$success
# [1] TRUE
#
# [[80]]$details
# [[80]]$details$message
# [1] "all good"
#
# [[80]]$timestamp
# [1] "2015-09-18 06:35:30 UTC"
#
# [[81]]
# [[81]]$success
# [1] TRUE
#
# [[81]]$details
# [[81]]$details$message
# [1] "check"
#
# [[81]]$timestamp
# [1] "2015-09-18 07:35:34 UTC"
#
# [[82]]
# [[82]]$success
# [1] TRUE
#
# [[82]]$details
# [[82]]$details$message
# [1] "check"
#
# [[82]]$timestamp
# [1] "2015-09-18 08:38:13 UTC"
#
# [[83]]
# [[83]]$success
# [1] TRUE
#
# [[83]]$details
# [[83]]$details$message
# [1] "check"
#
# [[83]]$timestamp
# [1] "2015-09-18 09:39:01 UTC"
#
# [[84]]
# [[84]]$success
# [1] TRUE
#
# [[84]]$details
# [[84]]$details$message
# [1] "check"
#
# [[84]]$timestamp
# [1] "2015-09-18 10:39:36 UTC"
#
# [[85]]
# [[85]]$success
# [1] TRUE
#
# [[85]]$details
# [[85]]$details$message
# [1] "check"
#
# [[85]]$timestamp
# [1] "2015-09-18 11:39:57 UTC"
#
# [[86]]
# [[86]]$success
# [1] TRUE
#
# [[86]]$details
# [[86]]$details$message
# [1] "ok"
#
# [[86]]$timestamp
# [1] "2015-09-18 12:41:17 UTC"
#
# [[87]]
# [[87]]$success
# [1] TRUE
#
# [[87]]$details
# [[87]]$details$message
# [1] "ok"
#
# [[87]]$timestamp
# [1] "2015-09-18 13:41:12 UTC"
#
# [[88]]
# [[88]]$success
# [1] TRUE
#
# [[88]]$details
# [[88]]$details$message
# [1] "all good"
#
# [[88]]$timestamp
# [1] "2015-09-18 14:42:09 UTC"
#
# [[89]]
# [[89]]$success
# [1] TRUE
#
# [[89]]$details
# [[89]]$details$message
# [1] "ok"
#
# [[89]]$timestamp
# [1] "2015-09-18 15:43:31 UTC"
#
# [[90]]
# [[90]]$success
# [1] TRUE
#
# [[90]]$details
# [[90]]$details$message
# [1] "all good"
#
# [[90]]$timestamp
# [1] "2015-09-18 16:44:50 UTC"
#
# [[91]]
# [[91]]$success
# [1] TRUE
#
# [[91]]$details
# [[91]]$details$message
# [1] "all good"
#
# [[91]]$timestamp
# [1] "2015-09-18 17:45:27 UTC"
#
# [[92]]
# [[92]]$success
# [1] TRUE
#
# [[92]]$details
# [[92]]$details$message
# [1] "all good"
#
# [[92]]$timestamp
# [1] "2015-09-18 18:48:43 UTC"
#
# [[93]]
# [[93]]$success
# [1] TRUE
#
# [[93]]$details
# [[93]]$details$message
# [1] "all good"
#
# [[93]]$timestamp
# [1] "2015-09-18 19:49:38 UTC"
#
# [[94]]
# [[94]]$success
# [1] TRUE
#
# [[94]]$details
# [[94]]$details$message
# [1] "ok"
#
# [[94]]$timestamp
# [1] "2015-09-18 20:50:24 UTC"
#
# [[95]]
# [[95]]$success
# [1] TRUE
#
# [[95]]$details
# [[95]]$details$message
# [1] "check"
#
# [[95]]$timestamp
# [1] "2015-09-18 21:50:54 UTC"
#
# [[96]]
# [[96]]$success
# [1] TRUE
#
# [[96]]$details
# [[96]]$details$message
# [1] "check"
#
# [[96]]$timestamp
# [1] "2015-09-18 22:47:49 UTC"
