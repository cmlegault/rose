#' Compute catch in short term deterministic projections.
#'
#' @param naa vector of starting numbers at age
#' @param maa vector of natural mortality at age
#' @param fmult double fishing mortality multiplier
#' @param selx vector of fishery selectivity
#' @param cwaa vector of catch weight at age
#' @param nprojyears integer number of years in projection
#' @param recruits double recruitment in projection year 2 and onwards
#'
#' @return vector of catch in weight for nprojyears
#' @export

project_short_term <- function(naa, maa, fmult, selx, cwaa, nprojyears, recruits){
  res <- rep(NA, nprojyears)
  na <- length(naa)
  faa <- fmult * selx
  zaa <- maa + faa
  for (i in 1:nprojyears){
    res[i] <- sum(naa * cwaa * faa * (1 - exp(-zaa)) / zaa)
    naa[1] <- recruits
    survs <- rep(NA, na)
    survs[2:na] <- naa[1:(na-1)] * exp(-zaa[1:(na-1)])
    survs[na] <- survs[na] + naa[na] * exp(-zaa[na])
    naa[2:na] <- survs[2:na]
  }
  
  return(res)
}
