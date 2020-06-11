### Title:    Table Formatting Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-06-02 12:48:07

##########################################################-
# FUNCTIONS ####
##########################################################-


get_colors <- function(x, pos = "#8af3a3", neg = "#f49c9c", zero = "#ffffff", zero_val = 0){
  xsort <- sort(as.numeric(x))
  xpos <- x[x>zero_val] %>% sort()
  xneg <- x[x<zero_val] %>% sort()
  x0 <- x[x==zero_val]
  colors_pos <- csscolor(gradient(c(zero_val, xpos), zero, pos)) %>% tail(-1)
  colors_neg <- csscolor(gradient(c(xneg, zero_val), neg, zero)) %>% head(-1)
  x0 <- rep(zero, length(x0))
  colors <- tibble(vals = xsort,
                   colors = c(colors_neg, x0, colors_pos)) %>%
    right_join(tibble(vals = x), by = "vals") %>%
    pull(colors)
  return(colors)
}


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
