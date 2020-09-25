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
#' @param exact Logical indicating whether or not you want to use \code{grepl(ignore.case = TRUE, perl = TRUE)} style regex.
#' @param where Filter applied to the data indicating the conditions required for the values to be replaced. Requires a string input, but functions the same way \code{dplyr::filter} does.
#' @param which_cols The columns across which the replacements will be applied. Use this if some columns that don't contain \code{nuke_value} are returning as a different class than they were in the original \code{data} input
#' @return A data frame with the all occurrences of \code{nuke_value} replaced with \code{ash}.
#' @examples
#' nuke(band_members, "Mick", "IT WORKED")
#' nuke(mtcars, 6, "IT WORKED AGAIN", where = "mpg == 21.0")
#' nuke(mtcars, 6)
#' @importFrom tidyr replace_na
#' @import dplyr
#' @export
nuke <- function(data, nuke_value = NA, ash = 0, exact = TRUE, where = NULL, which_cols = colnames(data)){

  column_classes <- tibble(colName = colnames(data),
                           class = sapply(data, class)) %>%
    mutate(convertFun = paste0("as.", class))

  data.1 <- data %>%
    mutate(..ID_COL.. = 1:n())

  danger_zone <- data.1 %>%
    (function(x){
      if(is.null(where)){
        x
      } else {
        # apply the filter
        x %>% filter(eval(rlang::parse_expr(where)))
      }
    }) %>%
    (function(x){
      if(nrow(x) > 0){
        x %>%
          mutate_at(vars(all_of(which_cols)),
                    function(x){
                      if(is.na(nuke_value)){
                        replace_na(x, ash)
                      } else if(exact){
                        replace(x, x == nuke_value, ash)
                      } else {
                        replace(x, x %=~% nuke_value, ash)
                      }
                    })
      } else {
        x
      }
    })

  safe_zone <- data.1 %>%
    (function(x){
      if(is.null(where)){
        x %>% filter(..ID_COL.. == 0)
      } else {
        # apply the filter
        x %>% filter(!eval(rlang::parse_expr(where)))
      }
    })


  aftermath <- data.1 %>%
    select(..ID_COL..) %>%
    left_join(bind_rows(
      safe_zone,
      danger_zone
    ), by = "..ID_COL..") %>%
    select(-..ID_COL..)

  return(aftermath)
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


