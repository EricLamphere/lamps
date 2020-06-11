### Title:    Mutate Helper Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-02-24 20:13:33

##########################################################-
# FUNCTIONS ####
##########################################################-
#' Replace all occurrences of a value in a data frame.
#' @param df The data frame to which you're replacing values.
#' @param nuke_value The value you want to replace with \code{ash}.
#' @param ash The value you want to replace \code{nuke_value} with.
#' @param regex Logical indicating whether or not you want to use \code{grepl(ignore.case = TRUE, perl = TRUE)} style regex.
#' @return A data frame with the all occurrences of \code{nuke_value} replaced with \code{ash}.
#' @examples
#' nuke(band_members, "Mick", "IT WORKED")
#' nuke(mtcars, 6, "IT WORKED AGAIN")
#' nuke(mtcars, 6)
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate_all
#' @export
nuke <- function(df, nuke_value = NA, ash = 0, regex = FALSE){
  nuked <- mutate_all(data, function(x){
    if(is.na(nuke_value)){
      replace_na(x, ash)
    } else if(exact){
      replace(x, x == nuke_value, ash)
    } else {
      replace(x, x %~% nuke_value, ash)
    }
  })
  return(nuked)
}


#' Calculate new columns with lists of labels and formulas
#' @description Create new columns using a list of column names (labels) and formulas. \code{labels} and \code{formulas} should both be character vectors of the same length
#' @param df The data frame you're manipulating. The \code{formulas} should be based off of the column names of df.
#' @param labels A character vector used as the names for the newly created columns.
#' @param formulas A character vector of formulas (e.g. \code{col1 / col2}) used to generate the new columns.
#' @param prefix A prefix to append to the beginning of all of the column names in \code{labels}.
#' @return A data frame with the newly calculated columns.
#' @examples
#' calc(mpg, "average mpg", "mean(c(cty, hwy), na.rm = TRUE)")
#' @importFrom dplyr "%>%" mutate_
#' @export
calc <- function(df, labels, formulas, prefix = ""){
  calculated <- df %>%
    mutate_(.dots = setNames(
      formulas,
      prefix %% labels
    ))
  return(calculated)
}


