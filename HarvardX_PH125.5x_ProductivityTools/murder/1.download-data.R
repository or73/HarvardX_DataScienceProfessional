setwd("~/Dropbox/training_courses/R/HarvardX_PH125.5x_ProductivityTools/murder")
url <- 'https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv'
dest_file <- 'data/murders.csv'
download.file( url, destfile = dest_file )
