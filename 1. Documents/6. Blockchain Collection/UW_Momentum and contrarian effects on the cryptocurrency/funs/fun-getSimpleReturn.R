# function to get simple returs
getSimpleReturn <- function(x) {
  
  xts::diff.xts(x)/xts::lag.xts(x)
}
