#' Remove files created during runRetroMults
#' 
#' Deletes files in current directory associated with runRetroMults analyses. Removes both 
#' named files as well as default files created when ASAP runs.
#'
#' @param fname character base file name used in runRetroMults
#'
#' @return NULL
#' @export
#'
# @examples
# cleanUpFiles("simple.dat")

cleanUpFiles <- function(fname){
  n.char <- nchar(fname)
  fname.base <- substr(fname, 1, n.char-4)
  shell(paste0("del ", fname.base, ".dat"))
  shell(paste0("del ", fname.base, ".rts"))
  shell(paste0("del ",fname.base,"_00*.*"))
  shell("del admodel.*")
  shell("del admb2r.log")
  shell("del asap3.b*")
  shell("del asap3.cor")
  shell("del asap3.dat")
  shell("del asap3.eva")
  shell("del asap3.log")
  shell("del asap3.p*")
  shell("del asap3.r*")
  shell("del asap3.std")
  shell("del asap3input.log")
  shell("del asap3MCMC.dat")
  shell("del eigv.rpt")
  shell("del fmin.log")
  shell("del variance")
  return()
}
