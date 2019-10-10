#' Adjust ASAP input file by catch or M multiplier using ramp and M selectivity
#' 
#' Modifies an exisiting ASAP input data by applying catch and M multipliers after specified year. 
#' The ramp variable allows for sudden change to multipled values or a linear increase with 
#' ramp number of years having intermediate value between original and full catch or M multiplier. 
#' The M selectivity allows for change in M to happen equally across all ages (all values = 1), 
#' or to focus on specific ages (values of 1, with other ages less than one and greater than or 
#' equal to 0).
#'
#' @param asap.dat list of asap data created by ReadASAP3DatFile 
#' @param ramp integer nubmer of years with intermediate values between original and full multipler 
#' @param change.year integer first year of full multiplier, all subsequent years have full multiplier
#' @param cmult double the multiplier to be applied to total catch of all fleets for years > year
#' @param mmult double the multiplier to be applied to M for years > year
#' @param mselx vector of length equal to number of ages, modified mmult at age
#'
#' @return list of asap data that can be written out using WriteASAP3DatFile
#' @export
#'
#' @examples
#' adjustASAP(asap.dat, 9, 2005, 1, 2.5, seq(0.1, 1, 0.1))

adjustASAP <- function(asap.dat, ramp, change.year, cmult, mmult, mselx){
  asap.dat.adj <- asap.dat
  year1 <- as.numeric(asap.dat$dat[names(asap.dat$dat) == "year1"])
  nyears <- as.numeric(asap.dat$dat[names(asap.dat$dat) == "n_years"])
  year.count <- seq(1, nyears)
  years <- seq(year1, (year1 + nyears - 1) , 1)
  tsmult <- rep(0, nyears) # determines how multpliers work over time
  change.year.count <- year.count[years == change.year]
  tsmult[change.year.count:nyears] <- 1
  if (ramp > 0){
    start.year <- change.year.count - ramp
    for (i in 1:ramp){
      tsmult[start.year + i - 1] <- i / (ramp + 1)
    }
  }
  # adjust catch
  catch.mat <- asap.dat$dat[names(asap.dat$dat) == "CAA_mats"]
  nfleets <- length(catch.mat[[1]])
  for (ifleet in 1:nfleets){
    catch.mat.adj <- catch.mat[[1]][[ifleet]]
    nc <- length(catch.mat.adj[1,])
    catch.mat.adj[, nc] <- catch.mat.adj[, nc] * (1 + tsmult * cmult)
    asap.dat.adj$dat[names(asap.dat$dat) == "CAA_mats"][[1]][[ifleet]] <- catch.mat.adj
  }
  # adjust natural mortality
  m.mat <- asap.dat$dat[names(asap.dat$dat) == "M"]
  m.mat.adj <- m.mat[[1]]
  mmult.mat <- 1 + (outer(tsmult, mselx) * mmult)
  m.mat.adj <- m.mat.adj * mmult.mat
  asap.dat.adj$dat[names(asap.dat$dat) == "M"][[1]] <- m.mat.adj
  
  return(asap.dat.adj)
}

