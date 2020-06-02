### Title:    Ideas and globals
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-02-24 20:13:33

##########################################################-
# IDEAS ####
##########################################################-

# (1) idea: take trended data (i.e. has a date column) that has more than one non-date variable
# and create a "lightbulb" function that does some quick analysis:
#  - correlatins between the variables
#  - min, max, etc. of each column
#  - a forecast of each
#  - maybe add a few takeaways similar to the causal impact package


##########################################################-
# LIBRARIES ####
##########################################################-
if(!("pacman" %in% installed.packages())) install.packages("pacman")
pacman::p_load(tidyverse, reshape2, gmailr, purrr, rlang, formattable, kableExtra)
