### Title:    Table Formatting Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-06-02 12:48:07

##########################################################-
# FUNCTIONS ####
##########################################################-

#' Get Colors
#' @description Create a matrix from vector to represent colors in gradient.
#' @param x A numeric vector.
#' @param pos Color of the value farthest above \code{zero_val}.
#' @param neg Color of the value farthest below \code{zero_val}.
#' @param zero Color assigned to \code{zero_val}.
#' @param zero_val The value you wish to represent 0 (the default, or median value in \code{x}).
#' @return A data frame with the newly calculated columns. See \code{gradient} documentation for relevant information.
#' @importFrom dplyr "%>%" left_join
#' @importFrom formattable csscolor gradient
#' @export
get_colors <- function(x, pos = "#8af3a3", neg = "#f49c9c", zero = "#ffffff", zero_val = 0){
  xsort <- sort(as.numeric(x))
  xpos <- x[x>zero_val] %>% sort()
  xneg <- x[x<zero_val] %>% sort()
  x0 <- x[x==zero_val]

  colors_pos <- csscolor(gradient(c(zero_val, xpos), zero, pos)) %>% tail(-1)
  colors_neg <- csscolor(gradient(c(xneg, zero_val), neg, zero)) %>% head(-1)
  x0 <- rep(zero, length(x0))

  colors <- tibble(vals = x) %>%
    left_join(tibble(
      vals = xsort,
      colors = c(colors_neg, x0, colors_pos)
    ), by = "vals") %>%
    pull(colors)

  return(colors)
}


#' Formattable Gradient Tile
#' @description More flexible alternative to \code{formattable::color_tile}.
#' @param format.fun Function to format the values within the selected cells.
#' @param format.digits Number of digits to which the values within the selected cells will be rounded.
#' @param font.family Font family of values within the selected cells.
#' @param font.weight Font size of values within the selected cells.
#' @param border.radius Radius of the border.
#' @param color Color assigned to text.
#' @param ... Parameters passed to \code{get_colors}.
#' @return Formatted table.
#' @import formattable
#' @export
gradient_tile <- function(format.fun = "percent", format.digits = 2,
                        font.family = "Arial", font.weight = "normal", border.radius = NULL, color = NULL,
                        ...){ # ... = inputs to get_colors function

  format.fun <- match.fun(format.fun)

  formatter("span",
            x ~ format.fun(x, digits = format.digits),
            style = function(x){
              style(
                display = "block", padding = "4px", `border-radius` = border.radius,
                color = color, `font-family` = font.family, `font-weight` = font.weight,
                `background-color` = get_colors(as.numeric(x), ...)
              )
            })
}


#' Formattable Cell Editor
#' @description Easily edit cells in a table using \code{formattable}.
#' @param font.family Font family of values within the selected cells.
#' @param font.weight Font size of values within the selected cells.
#' @param format.fun Function to format the values within the selected cells.
#' @param digits Number of digits to which the values within the selected cells will be rounded.
#' @param ... Parameters passed to \code{style}.
#' @return Formatted table.
#' @import formattable
#' @export
cell_edit <- function(font.family = "", font.weight = "normal", format.fun = "comma", digits = 2,
                      ...){ # ... = style inputs
  format.fun <- match.fun(format.fun)
  formatter(
    "span",
    x ~ format.fun(x, digits = digits),
    style = function(x){
      style(`font-family` = font.family, font.weight = font.weight, ...)
    }
  )
}
