#' Calculate spawning stock biomass per recruit
#'
#' @param nAge integer number of ages
#' @param ssbwaa vector of spawning stock biomass weights at age
#' @param maturity vector of maturity at age
#' @param maa vector of natural mortality at age
#' @param fmult double fishing mortality multiplier
#' @param selx vectory of fishery selectivity at age
#' @param spawntime double fraction of year before spawning occurs (should be within [0, 1])
#'
#' @return spr double value of spawners per recruit
#' @export

calc_ssb_per_r <- function(nAge, ssbwaa, maturity, maa, fmult, selx, spawntime){
  
  spr <- 0.0
  cumsurvival <- 1.0
  
  for (i in 1:(nAge - 1)){
    z <- maa[i] + fmult * selx[i]
    spr <- spr + ssbwaa[i] * maturity[i] * cumsurvival * exp(-(z * spawntime))
    cumsurvival <- cumsurvival * exp(-z)
  }
  
  z <- maa[nAge] + fmult * selx[nAge]
  spr <- spr + ssbwaa[i] * maturity[i] * cumsurvival * exp(-(z * spawntime)) / (1 - exp(-z))
  
  return(spr)
}
