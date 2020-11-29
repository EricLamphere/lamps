### Title:    Utility Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-02-24 20:13:33

##########################################################-
# FUNCTIONS ####
##########################################################-
# Utility Functions ----

#' Wrapper for \code{View} - Invoke Data Viewer
#' @description see(x) = View(x). See \code{View} documentation with \code{?View} for more information.
#' @param x an R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @return Invisible NULL. The functions puts up a window and returns immediately: the window can be closed via its controls or menus.
#' @export
see <- function(x){
  View(x, title = deparse(substitute(x)))
}

# Pipe operators ----

#' Pattern Matching
#' @description Search for matches to an argument pattern within each element of a character vector. Uses perl style regex and ignores case. See \code{grepl} documentation for more information.
#' @param x Character vector
#' @param reg Character string containing a regular expression to be matched in the given character vector.
#' @return See \code{grepl} documentation for more information
#' @export
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
save_env <- function(environment = .GlobalEnv){
  .GlobalCopy <<- as.environment(as.list(environment, all.names = TRUE))
}


# remove pattern from strings
`%remove%` <- function(x, pattern){
  gsub(pattern, "", x, ignore.case = TRUE, perl = TRUE, fixed = FALSE)
}

# restore saved environment
# restoreEnv <- function(){
#   # Remove difference between the saved copy and the current .GlobalEnv
#   #set_env(.GlobalEnv, .GlobalCopy)
#   rm(list = setdiff(ls(envir = .GlobalEnv, all.names = TRUE), '.GlobalCopy'))
#   # assign the values from the saved copy back to the .GlobalEnv
#   for(n in ls(.GlobalCopy, all.names = TRUE)) assign(n, get(n, .GlobalCopy), envir = .GlobalEnv)
# }
