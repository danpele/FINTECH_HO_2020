findFirstNonNAValue <- function(x) {
  NonNAindex <- which(!is.na(x))
  firstNonNA <- min(NonNAindex)
  return(x[firstNonNA])
}
