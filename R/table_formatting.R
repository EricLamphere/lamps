### Title:    Table Formatting Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-06-02 12:48:07

##########################################################-
# FUNCTIONS ####
##########################################################-

# > get_colors ----
get_colors <- function(x, pos = RELDIFF_GREEN_DARK, neg = RELDIFF_RED_DARK, zero = WHITE){
  xsort <- sort(as.numeric(x))
  xpos <- x[x>0] %>% sort()
  xneg <- x[x<0] %>% sort()
  x0 <- x[x==0]
  colors_pos <- csscolor(gradient(c(0, xpos), zero, pos)) %>% tail(-1)
  colors_neg <- csscolor(gradient(c(xneg, 0), neg, zero)) %>% head(-1)
  x0 <- rep(zero, length(x0))
  colors <- tibble(vals = xsort,
                   colors = c(colors_neg, x0, colors_pos)) %>%
    right_join(tibble(vals = x), by = "vals") %>%
    pull(colors)
  return(colors)
}

# > color_tile2 ----
color_tile2 <- function(format.fun = "percent", digits = 2, font.family = "Audi Type Normal",
                        ...){ # ... = inputs to get_colors function
  format <- match.fun(format.fun)
  formatter("span",
            x ~ format(x, digits = digits),
            style = function(x){
              style(
                display = "block", padding = "4px", #`border-radius` = "4px",
                #color = "black",
                `font-family` = font.family,
                `background-color` = get_colors(as.numeric(x), ...)
              )
            })
}

# > cell_edit ----
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
