# Introduction to Regression

# Introduction to Regression Overview

# Motivating Example: Moneyball
# Question 1
# What is the application of statistics and data science to baseball called?
# Moneyball
# Sabermetrics   <-*
# The “Oakland A’s Approach”
# There is no specific name for this; it’s just data science.

# Baseball basics
# Question 1
# Which of the following outcomes is not included in the batting average?
# A home run    <-*
# A base on balls
# An out
# A single


# Question 2
# Why do we consider team statistics as well as individual player statistics?
# The success of any individual player also depends on the strength of their team.  <-*
# Team statistics can be easier to calculate.
# The ultimate goal of sabermetrics is to rank teams, not players.


# Bases on Balls or Stolen Bases?
library(rvest)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)
library(dslabs)
library( magrittr )
library( ggplot2 )
library( Lahman )
ds_theme_set()
year_ID <- 1973

# Relation between home runs and runs
Teams %>% filter( yearID %in% 1961:2001 ) %>%
    mutate( HR_per_game = HR / G, R_per_game = R / G ) %>%
    ggplot( aes( HR_per_game, R_per_game ) ) +
    geom_point( alpha = 0.5 )

# Relation between stolen bases and wins
Teams %>% filter( yearID %in% 1961:2001 ) %>%
    mutate( SB_per_game = SB / G, R_per_game = R /G ) %>%
    ggplot( aes( SB_per_game, R_per_game ) ) +
    geom_point( alpha = 0.5 )

# Relation between bases on balls and runs
Teams %>% filter( yearID %in% 1961:2001 ) %>%
    mutate( BB_per_game = BB / G, R_per_game = R / G ) %>%
    ggplot( aes( BB_per_game, R_per_game ) ) +
    geom_point( alpha = 0.5 )

# Question 1
# You want to know whether teams with more at-bats per game have more runs per game. What R code below correctly makes a scatter plot for this relationship?
#
# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#     ggplot(aes(AB, R)) +
#     geom_point(alpha = 0.5)
#
# Teams %>% filter(yearID %in% 1961:2001 ) %>%    <-*
#     mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#     ggplot(aes(AB_per_game, R_per_game)) +
#     geom_point(alpha = 0.5)
#
# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#     mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#     ggplot(aes(AB_per_game, R_per_game)) +
#     geom_line()
#
# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#     mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#     ggplot(aes(R_per_game, AB_per_game)) +
#     geom_point()


# Question 2
# What does the variable “SOA” stand for in the Teams table?
#     Hint: make sure to use the help file (?Teams).
#
# sacrifice out
# slides or attempts
# strikeouts by pitchers   ;'*
# accumulated singles
