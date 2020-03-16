### Title:    Mutate Helper Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-02-24 20:13:33

##########################################################-
# FUNCTIONS ####
##########################################################-
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
