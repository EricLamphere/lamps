### Title:    On attach
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-09-25 14:53:23

.onAttach <- function(libname, pkgname){
  packageStartupMessage(
    "lamps v", utils::packageVersion("lamps"),
    " loaded ",
    praise::praise("${adverb_manner}. ${Exclamation}!")
  )

  if(!("pacman" %in% installed.packages())) install.packages("pacman")
  pacman::p_load(tidyverse, reshape2, gmailr, purrr, rlang, formattable, kableExtra)
}
