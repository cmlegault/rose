#' Extract recent averages of ASAP estimates for per recruit and projection calculations.
#'
#' @param asap list from reading asap.rdat file using dget()
#' @param nrecentyears integer number of recent years to include in averages
#'
#' @return list of values
#' @export

get_asap_recent_vals <- function(asap, nrecentyears){
  res <- list()
  res$nAge <- asap$parms$nages
  ny <- asap$parms$nyears
  nr <- ny-nrecentyears+1
  res$ssbwaa <- apply(asap$WAA.mats$WAA.ssb[nr:ny, ], 2, mean)
  res$maturity <- apply(asap$maturity[nr:ny, ], 2, mean)
  res$maa <- apply(asap$M.age[nr:ny, ], 2, mean)
  sel.mat <- asap$F.age[nr:ny, ] / apply(asap$F.age[nr:ny, ], 1, max)
  sel.age1 <- apply(sel.mat, 2, mean)
  res$selx <- sel.age1 / max(sel.age1)
  res$spawntime <- asap$options$frac.yr.spawn
  return(res)
}
