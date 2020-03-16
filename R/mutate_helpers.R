### Title:    Functions
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
pacman::p_load(tidyverse, reshape2, gmailr, purrr, rlang)


##########################################################-
# FUNCTIONS ####
##########################################################-
# > Gmail functions ----


# > Mutate functions ----
nuke <- function(data, nuke_value = NA, ash = 0, exact = TRUE){
  nuked <- mutate_all(data, function(x){
    if(is.na(nuke_value)){
      replace_na(x, ash)
    } else if(exact){
      replace(x, x == nuke_value, ash)
    } else {
      replace(x, x %~% nuke_value, ash)
    }
  })
}





# Utility Functions ----
# x contains regex string
`%~%` <- function(x, reg){
  grepl(reg, x, ignore.case = TRUE, perl = TRUE)
}

# x does not contain regex string
`%!~%` <- function(x, reg){
  !grepl(reg, x, ignore.case = TRUE, perl = TRUE)
}

# concatenation of two strings
`%%` <- function(x, y){
  paste0(
    ifelse(is.na(x),"",x),
    ifelse(is.na(y),"",y)
  )
}

# x is not a value of y (list or vector)
`!in` <- function(x, y){
  !(x %in% y)
}



