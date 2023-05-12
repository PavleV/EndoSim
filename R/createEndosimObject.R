# create EndoSim class

setClass("EndoSim", slots=list(name="character", id="numeric", contact="character"))


#' Create an EndoSim simulation object
#'
#' This function initialises an EndoSim object with initial settings. It requires age range and
#' number of iterations.
#'
#' @param iter_num Number of iterations of the simulation
#' @param age_range Range of ages
#' @return An EndoSim object
#' @export
createEndoSim <- function(iter_num=1000,age_range=20:45){

  print("Created EndoSim simulation object")
  new("EndoSim",name="Steven", id=1002, contact="West Avenue")

}


