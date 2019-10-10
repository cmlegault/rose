#' Wrapper to run ASAP retrospective across range of multipliers for catch or M
#' 
#' Scenarios defined by ramp (number of years included in ramp of multipliers), change year, 
#' catch multipliers, natural mortality multipliers, natural mortality selectivty 
#' (maximum value should be 1, minimum value allowed is zero). Uses Mohn's rho for 
#' spawning stock biomass as the metric of removing retrospective pattern (when ssbrho = 0).
#'
#' @param scenario.name character describes the runs in words to distinguish from other sets of runs
#' @param asap.fname character name of asap dat file (including extension)
#' @param n.peels integer number of peels used to compute Mohn's rho
#' @param ramp integer number of years in ramp (0 means immediate jump to multiplied value)
#' @param year.vals vector of change years
#' @param cmult.vals vector of catch multipliers
#' @param mmult.vals vector of natural mortality (M) multipliers
#' @param mselx vector of selectivity at age to mutiply mmult by (allows additional mortality to be for all ages equally, focused on young ages, focused on old ages, etc.)
#' @param mselxlab character describes natural mortality selectivity used
#' @param save.files logical TRUE means keep all retrospective files, default FALSE removes all files
#'
#' @return data frame of scenario, change year, ramp, catch multiplier, M multiplier, M selectivity label, and Mohn's rho for SSB
#' @export
#'
# @examples
# runRetroMults("Sudden Catch Mults", "simple.dat", 5, 0, c(2000,2005), seq(1.5,5,0.5), 1, rep(1,6), "All Ages", FALSE)

runRetroMults <- function(scenario.name,asap.fname,n.peels,ramp,year.vals,cmult.vals,mmult.vals,mselx, mselxlab,save.files=FALSE){
  res <- data.frame(scenario = character(),
                    change.year = integer(),
                    ramp = integer(),
                    cmult = double(),
                    mmult = double(),
                    mselxlab = character(),
                    ssbrho = double())
  
  n.years <- length(year.vals)
  n.cmults <- length(cmult.vals)
  n.mmults <- length(mmult.vals)
  
  asap.dat <- ASAPplots::ReadASAP3DatFile(asap.fname)
  terminal.year <- as.numeric(asap.dat$dat[1]) + as.numeric(asap.dat$dat[2]) - 1
  retro.first.year <- terminal.year - n.peels
  
  # loop through change years, cmults, mmults
  for (iy in 1:n.years){
    change.year <- year.vals[iy]
    for (ic in 1:n.cmults){
      cmult <- cmult.vals[ic]
      for (im in 1:n.mmults){
        mmult <- mmult.vals[im]
        
        asap.dat.adj <- adjustASAP(asap.dat, ramp, change.year, cmult, mmult, mselx)
        fname <- paste0("y", change.year, "c", cmult, "m", mmult, ".dat")
        header.text <- paste0("year=", change.year, ", catch mult=", cmult, ", m mult=", mmult)
        print(header.text)
        ASAPplots::WriteASAP3DatFile(fname, asap.dat.adj, header.text)
        
        shell(paste("ASAPRETRO.exe", fname, retro.first.year), intern=TRUE)
        ssbrho <- calcSSBrho(fname, n.peels)
        thisdf <- data.frame(scenario = scenario.name,
                             change.year = change.year,
                             ramp = ramp,
                             cmult = cmult,
                             mmult = mmult,
                             mselxlab = mselxlab,
                             ssbrho = ssbrho)
        res <- rbind(res, thisdf)
        if (save.files == FALSE) cleanUpFiles(fname)
      }
    }
  }
  return(res)
} 
