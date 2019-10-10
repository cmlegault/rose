#' Calculates Mohn's rho for SSB from ASAP retrospective
#'
#' Computes relative change in spawning stock biomass from tip of previous n.peels 
#' relative to terminal estimates for that year. Returns the average value.
#'
#' @param fname character path and name of ASAP input file (including .dat extension)
#' @param n.peels integer number of peels used in calculating mean Mohn's rho value (must be <10)
#'
#' @return double mean Mohn's rho for SSB
#' @export
#'

calcSSBrho <- function(fname, n.peels){
  n.char <- nchar(fname)
  fname.base <- substr(fname,1, n.char-4)
  ssb <- list()
  for (i in 0:n.peels){
    asap.name <- paste0(fname.base,"_00",i,".rdat")
    asap <- dget(asap.name)
    ssb[[i+1]] <- asap$SSB
  }

  ssb.base <- ssb[[1]]
  rho <- rep(NA, n.peels)
  for (i in 1:n.peels){
    ssb.use <- ssb[[i+1]]
    n.y <- length(ssb.use)
    rho[i] <- (ssb.use[n.y] - ssb.base[n.y]) / ssb.base[n.y]
  }
  ssb.rho <- mean(rho, na.rm=T)
  return(ssb.rho)
}
