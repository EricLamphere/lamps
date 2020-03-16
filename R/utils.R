### Title:    Utility Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-02-24 20:13:33

##########################################################-
# FUNCTIONS ####
##########################################################-
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



