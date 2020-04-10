### Title:    Utility Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-02-24 20:13:33

##########################################################-
# FUNCTIONS ####
##########################################################-
# Utility Functions ----
see <- function(x){
  View(x, title = deparse(substitute(x)))
}

# Pipe operators ----
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
`%!in%` <- function(x, y){
  !(x %in% y)
}

# save environment
saveEnv <- function(environment = .GlobalEnv){
  .GlobalCopy <<- as.environment(as.list(environment, all.names = TRUE))
}


# restore saved environment
# restoreEnv <- function(){
#   # Remove difference between the saved copy and the current .GlobalEnv
#   #set_env(.GlobalEnv, .GlobalCopy)
#   rm(list = setdiff(ls(envir = .GlobalEnv, all.names = TRUE), '.GlobalCopy'))
#   # assign the values from the saved copy back to the .GlobalEnv
#   for(n in ls(.GlobalCopy, all.names = TRUE)) assign(n, get(n, .GlobalCopy), envir = .GlobalEnv)
# }
