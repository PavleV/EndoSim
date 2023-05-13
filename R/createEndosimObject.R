# create EndoSim class

setClass("EndoSim", slots=list(iter_num="numeric",age_range="numeric",result="matrix"), validity = check_endosim)

check_endosim <- function(object){

  errors <- character()
  age_range.length <- length(object@age_range)

  if (age_range.length != 2){
    msg <- paste0("Number of ages supplied is ",age_range.length,". Should be 2.")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors

}





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

  new("EndoSim",iter_num=iter_num,age_range=age_range,result=matrix(0,2,2))

}

# create generic method

setMethod("show",
          "EndoSim",
          function(object) {
            cat("Number of iterations:",object@iter_num, "\n")
            cat("Age range:",object@age_range, "\n")
            #cat("Contact:", object@contact, "\n")
          }
)
