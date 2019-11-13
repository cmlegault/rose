#' Compute numbers at age in terminal year plus one and fill in recruitment using median of input years.
#'
#' @param asap list from reading asap.rdat file using dget()
#' @param recruityears vector of years to include when computing median recruitment
#'
#' @return vector of numbers at age
#' @export

get_starting_naa <- function(asap, recruityears){
  
  nAge <- asap$parms$nages
  
  ny <- asap$parms$nyears
  yearindex <- seq(1, ny)
  years <- seq(asap$parms$styr, asap$parms$endyr)
  yearR <- yearindex[years %in% recruityears]
  
  naa <- rep(NA, nAge)
  naa[1] <- stats::median(asap$N.age[yearR, 1])

  zaa <- asap$M.age[ny, ] + asap$F.age[ny, ]
  naa[2:nAge] <- asap$N.age[ny, 1:(nAge-1)] * exp(-zaa[1:(nAge-1)])
  naa[nAge] <- naa[nAge] + asap$N.age[ny, nAge] * exp(-zaa[nAge])
  
  return(naa)
}


