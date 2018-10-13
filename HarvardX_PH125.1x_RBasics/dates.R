# Exercise 1: Tile plot - measles and smallpox
# The sample code given creates a tile plot showing the rate of measles cases
#    per population. We are #going to modify the tile plot to look at smallpox cases instead.
# INSTRUCTIONS
# Modify the tile plot to show the rate of smallpox cases instead of measles cases.
# Exclude years in which cases were reported in fewer than 10 weeks from the plot.
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)


the_disease = "Measles"
dat <- us_contagious_diseases %>%
    filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
    mutate(rate = count / population * 10000) %>%
    mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) +
    geom_tile(color = "grey50") +
    scale_x_continuous(expand=c(0,0)) +
    scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    ggtitle(the_disease) +
    ylab("") +
    xlab("")

# Modify the tile plot to show the rate of smallpox cases instead of measles cases.
# Exclude years in which cases were reported in fewer than 10 weeks from the plot.
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

head( us_contagious_diseases )
the_disease = 'Smallpox'
dat <- us_contagious_diseases %>%
    filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10 ) %>%
    mutate(rate = count / population * 10000) %>%
    mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) +
    geom_tile(color = "grey50") +
    scale_x_continuous(expand=c(0,0)) +
    scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    ggtitle(the_disease) +
    ylab("") +
    xlab("")

# Exercise 2. Time series plot - measles and smallpox
# The sample code given creates a time series plot showing the rate of measles cases
#   per population by state. We are going to again modify this plot to look at smallpox cases instead.
# INSTRUCTIONS
# Modify the sample code for the time series plot to plot data for smallpox instead of for measles.
# Once again, restrict the plot to years in which cases were reported in at least 10 weeks.
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

the_disease = "Measles"
dat <- us_contagious_diseases %>%
    filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
    mutate(rate = count / population * 10000) %>%
    mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
    filter(disease==the_disease) %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
    geom_line(aes(year, rate, group = state),  color = "grey50",
              show.legend = FALSE, alpha = 0.2, size = 1) +
    geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
    scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) +
    ggtitle("Cases per 10,000 by state") +
    xlab("") +
    ylab("") +
    geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") +
    geom_vline(xintercept=1963, col = "blue")

# Modify the sample code for the time series plot to plot data for smallpox instead of for measles.
# Once again, restrict the plot to years in which cases were reported in at least 10 weeks.
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
    filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10 ) %>%
    mutate(rate = count / population * 10000) %>%
    mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
    filter(disease==the_disease) %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
    geom_line(aes(year, rate, group = state),  color = "grey50",
              show.legend = FALSE, alpha = 0.2, size = 1) +
    geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
    scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) +
    ggtitle("Cases per 10,000 by state") +
    xlab("") +
    ylab("") +
    geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") +
    geom_vline(xintercept=1963, col = "blue")


# Exercise 3: Time series plot - all diseases in California
# Now we are going to look at the rates of all diseases in one state.
#    Again, you will be modifying the sample code to produce the desired plot.
# INSTRUCTIONS
# For the state of California, make a time series plot showing rates for all diseases.
# Include only years with 10 or more weeks reporting.
# Use a different color for each disease.

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(state=="California") %>%
    group_by(year, disease) %>%
    summarize(rate = sum(count)/sum(population)*10000) %>%
    ggplot(aes(year, rate)) +
    geom_line()

# For the state of California, make a time series plot showing rates for all diseases.
# Include only years with 10 or more weeks reporting.
# Use a different color for each disease.

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>%
    filter( state == "California" & weeks_reporting >= 10 ) %>%
    group_by(year, disease) %>%
    summarize(rate = sum(count)/sum(population)*10000) %>%
    ggplot(aes(year, rate, color = disease )) +
    geom_line()

# Exercise 4: Time series plot - all diseases in the United States
# Now we are going to make a time series plot for the rates of all diseases in
#    the United States. For this exercise, we have provided less sample code
#    - you can take a look at the previous exercise to get you started.
# INSTRUCTIONS
# Compute the US rate by using summarize to sum over states.
# The US rate for each disease will be the total number of cases divided by the total population
# Remember to convert to cases per 10,000.
# You will need to filter for !is.na(population) to get all the data.
# Plot each disease in a different color.

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

# Compute the US rate by using summarize to sum over states.
# The US rate for each disease will be the total number of cases divided by the total population
# Remember to convert to cases per 10,000.
# You will need to filter for !is.na(population) to get all the data.
# Plot each disease in a different color.

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>%
    filter( !is.na( population ) ) %>%
    group_by( year, disease ) %>%
    summarize( rate = sum( count ) / sum( population ) *10000 ) %>%
    ggplot( aes( year, rate, color = disease ) ) + geom_line()


sapply(list(runif (10), runif (10)),
       function(x) c(min = min(x), mean = mean(x), max = max(x)))


# .*: A usual suspect! It can be read as "any character that is matched zero or
#     more times".
# \\s: Match a space. The "s" is normally a character, escaping it (\\) makes it
#      a metacharacter.
# [0-9]+: Match the numbers 0 to 9, at least once (+).
# ([0-9]+): The parentheses are used to make parts of the matching string
#              available to define the replacement.
#              The \\1 in the replacement argument of sub() gets set to the
#              string that is captured by the regular expression [0-9]+.

awards <- c("Won 1 Oscar.",
            "Won 1 Oscar. Another 9 wins & 24 nominations.",
            "1 win and 2 nominations.",
            "2 wins & 3 nominations.",
            "Nominated for 2 Golden Globes. 1 more win & 2 nominations.",
            "4 wins & 1 nomination.")

sub(".*\\s([0-9]+)\\snomination.*$", "\\1", awards)


# Today
today <- Sys.Date()
today
class( today )

now <- Sys.time()
now
class( now )

# Create Date Objects - Defaul format yyyy-mm-dd
# Data my 14 1971
my_date <- as.Date( '1971-05-14' )
my_date

# specify format
my_date1 <- as.Date( '1971-14-05', format = '%Y-%d-%m' )
my_date1


# Create POSIXct objects
my_time <- as.POSIXct( '1971-05-14 11:25:15' )
my_time

# Date Arithmetic
my_date
my_date + 1
my_date2 <- as.Date( '1998-09-29' )
my_date2 - my_date

my_time
my_time + 1   # increments 1 second
my_time2 <- as.POSIXct( '1974-07-14 21:11:55 CET' )
my_time2 - my_time

# Convert date to other format
my_date3 <- as.Date( '1971-05-14' )
unclass( my_date3 )   # Shows the days elapsed from January 1, 1970

my_time3 <- as.POSIXct( '1971-05-14 11:25:15 CET' )
unclass( my_time3 )   # Shows time elapsed from Janyary 1, 1970 at midnight

# Dedicated R Packages
# lubridate   zoo   xts

# Create and format dates
# To create a Date object from a simple character string in R, you can use the as
# .Date() function. The character string has to obey a format that can be defined
# using a set of symbols (the examples correspond to 13 January, 1982):

#    %Y: 4-digit year (1982)
# %y: 2-digit year (82)
# %m: 2-digit month (01)
# %d: 2-digit day of the month (13)
# %A: weekday (Wednesday)
# %a: abbreviated weekday (Wed)
# %B: month (January)
# %b: abbreviated month (Jan)
# The following R commands will all create the same Date object for the 13th day in # January of 1982:

#    as.Date("1982-01-13")
# as.Date("Jan-13-82", format = "%b-%d-%y")
# as.Date("13 January, 1982", format = "%d %B, %Y")
# Notice that the first line here did not need a format argument, because by default # R matches your character string to the formats "%Y-%m-%d" or "%Y/%m/%d".

# In addition to creating dates, you can also convert dates to character strings
# that use a different date notation. For this, you use the format() function. Try
# the following lines of code:

#     today <- Sys.Date()
# format(Sys.Date(), format = "%d %B, %Y")
# format(Sys.Date(), format = "Today is a %A!")

# In the editor on the right, three character strings representing dates have been
#   created. Convert them to dates using as.Date(), and assign them to date1, date2, #   and date3 respectively. The code for date1 is already included.
# Extract useful information from the dates as character strings using format().
#   From the first date, select the weekday. From the second date, select the day of #   the month. From the third date, you should select the abbreviated month and the
#   4-digit year, separated by a space.

# Definition of character strings representing dates
str1 <- "May 23, '96"
str2 <- "2012-03-15"
str3 <- "30/January/2006"

# Convert the strings to dates: date1, date2, date3
date1 <- as.Date(str1, format = "%b %d, '%y")
date2 <- as.Date( str2 )
date3 <- as.Date( str3, format = '%d/%B/%Y' )
date1
date2
date3
# Convert dates to formatted strings
format( date1, "%A" )
format( date2, '%d' )
format( date3, '%b %Y' )


# Create and format times
# Similar to working with dates, you can use as.POSIXct() to convert from a
#   character string to a POSIXct object, and format() to convert from a POSIXct
#   object to a character string. Again, you have a wide variety of symbols:

# %H: hours as a decimal number (00-23)
# %I: hours as a decimal number (01-12)
# %M: minutes as a decimal number
# %S: seconds as a decimal number
# %T: shorthand notation for the typical format %H:%M:%S
# %p: AM/PM indicator
# For a full list of conversion symbols, consult the strptime documentation in the
#   console:

# ?strptime
# Again,as.POSIXct() uses a default format to match character strings. In this case, #   it's %Y-%m-%d %H:%M:%S. In this exercise, abstraction is made of different time
#   zones.

# INSTRUCTIONS
# Convert two strings that represent timestamps, str1 and str2, to POSIXct objects
# called time1 and time2.
# Using format(), create a string from time1 containing only the minutes.
# From time2, extract the hours and minutes as "hours:minutes AM/PM". Refer to the
#   assignment text above to find the correct conversion symbols!

# Definition of character strings representing times
str1 <- "May 23, '96 hours:23 minutes:01 seconds:45"
str2 <- "2012-3-12 14:23:08"

# Convert the strings to POSIXct objects: time1, time2
time1 <- as.POSIXct(str1, format = "%B %d, '%y hours:%H minutes:%M seconds:%S")
time2 <- as.POSIXct( str2 )
time1
time2
# Convert times to formatted strings
format( time1, '%M' )
format( time2, '%I:%M %p')

# Calculations with Dates
# Both Date and POSIXct R objects are represented by simple numerical values under
#  the hood. This makes calculation with time and date objects very straightforward: #  R performs the calculations using the underlying numerical values, and then
#  converts the result back to human-readable time information again.

# You can increment and decrement Date objects, or do actual calculations with them
#    (try it out in the console!):

# today <- Sys.Date()
# today + 1
# today - 1

# as.Date("2015-03-12") - as.Date("2015-02-27")
# To control your eating habits, you decided to write down the dates of the last
#  five days that you ate pizza. In the workspace, these dates are defined as five
#  Date objects, day1 to day5. The code on the right also contains a vector pizza
#  with these 5 Date objects.

# INSTRUCTIONS
# Calculate the number of days that passed between the last and the first day you
#    ate pizza. Print the result.
# Use the function diff() on pizza to calculate the differences between consecutive
#    pizza days. Store the result in a new variable day_diff.
# Calculate the average period between two consecutive pizza days. Print the result.


# day1, day2, day3, day4 and day5 are already available in the workspace
day1   # "2018-08-21"
day2   # "2018-08-23"
day3   # "2018-08-28"
day4   #"2018-09-03"
day5   # "2018-09-08"
# Difference between last and first pizza day
day5 - day1

# Create vector pizza
pizza <- c(day1, day2, day3, day4, day5)

# Create differences between consecutive pizza days: day_diff
day_diff <- diff( pizza, differences = 1 )
day_diff
class( day_diff )
# Average period between two consecutive pizza days
mean( day_diff )


# Calculations with Times
# Calculations using POSIXct objects are completely analogous to those using Date
#    objects. Try to experiment with this code to increase or decrease POSIXct
#    objects:

# now <- Sys.time()
# now + 3600          # add an hour
# now - 3600 * 24     # subtract a day
# Adding or substracting time objects is also straightforward:

# birth <- as.POSIXct("1879-03-14 14:37:23")
# death <- as.POSIXct("1955-04-18 03:47:12")
# einstein <- death - birth
# einstein
# You're developing a website that requires users to log in and out. You want
#    to know what is the total and average amount of time a particular user
#    spends on your website. This user has logged in 5 times and logged out
#    5 times as well. These times are gathered in the vectors login and logout,
#    which are already defined in the workspace.

# INSTRUCTIONS
# Calculate the difference between the two vectors logout and login, i.e. the
#    time the user was online in each independent session. Store the result in
#    a variable time_online.
# Inspect the variable time_online by printing it.
# Calculate the total time that the user was online. Print the result.
# Calculate the average time the user was online. Print the result.

# login and logout are already defined in the workspace
login
# [1] "2018-08-25 10:18:04 UTC" "2018-08-30 09:14:18 UTC"
# [3] "2018-08-30 12:21:51 UTC" "2018-08-30 12:37:24 UTC"
# [5] "2018-09-01 21:37:55 UTC"
logout
# [1] "2018-08-25 10:56:29 UTC" "2018-08-30 09:14:52 UTC"
# [3] "2018-08-30 12:35:48 UTC" "2018-08-30 13:17:22 UTC"
# [5] "2018-09-01 22:08:47 UTC"

# Calculate the difference between login and logout: time_online
time_online <- logout - login
time_online

# Inspect the variable time_online
unclass( time_online )

# Calculate the total time online
sum( time_online )

# Calculate the average time online
mean( time_online )





# Time is of the essence
# The dates when a season begins and ends can vary depending on who you ask.
# People in Australia will tell you that spring starts on September 1st.
# The Irish people in the Northern hemisphere will swear that spring starts
#    on February 1st, with the celebration of St. Brigid's Day. Then there's
#    also the difference between astronomical and meteorological seasons:
#    while astronomers are used to equinoxes and solstices, meteorologists
#    divide the year into 4 fixed seasons that are each three months long.
#    (source: www.timeanddate.com)

# A vector astro, which contains character strings representing the dates on
#    which the 4 astronomical seasons start, has been defined on your workspace.
# Similarly, a vector meteo has already been created for you, with the
#    meteorological beginnings of a season.

# INSTRUCTIONS
# Use as.Date() to convert the astro vector to a vector containing Date objects.
# You will need the %d, %b and %Y symbols to specify the format. Store the
#     resulting vector as astro_dates.
# Use as.Date() to convert the meteo vector to a vector with Date objects.
# This time, you will need the %B, %d and %y symbols for the format argument.
# Store the resulting vector as meteo_dates.
# With a combination of max(), abs() and -, calculate the maximum absolute
# difference between the astronomical and the meteorological beginnings of a
# season, i.e. astro_dates and meteo_dates. Simply print this maximum difference
# to the console output.

astro
# spring        summer          fall        winter
# "20-Mar-2015" "25-Jun-2015" "23-Sep-2015" "22-Dec-2015"

meteo
# spring            summer              fall            winter
# "March 1, 15"      "June 1, 15" "September 1, 15"  "December 1, 15"

# Convert astro to vector of Date objects: astro_dates
astro_dates <-  as.Date( astro, '%d-%B-%Y' )
astro_dates
# Convert meteo to vector of Date objects: meteo_dates
meteo_dates <- as.Date( meteo, '%B %d, %y')
meteo_dates
# Calculate the maximum absolute difference between astro_dates and meteo_dates
max( abs ( astro_dates - meteo_dates ) )





