# create EndoSim class

# validity method for EndoSim class
check_endosim <- function(object){

  errors <- character()
  age_range.length <- length(object@age_range)

  if (age_range.length != 2){
    msg <- paste0("Number of ages supplied is ",age_range.length,". Should be 2.")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors

}


setClass("EndoSim", slots=list(iter_num="numeric",age_range="numeric",simulation="list"), validity = check_endosim)



#' Create an EndoSim simulation object
#'
#' This function initialises an EndoSim object with initial settings. It requires age range and
#' number of iterations.
#'
#' @param iter_num Number of iterations of the simulation. Default value = 1000.
#' @param age_range Range of ages. Default value = c(20,45).
#' @param space_SI Number of cycles between successful implantations. Default value = 12.
#' @return An EndoSim object
#' @export
createEndoSim <- function(iter_num=1000,age_range=c(20,45), space_SI = 12){

  suppressWarnings({

  simulation <- FUN.simulation4(ages = age_range[1]:age_range[2],
                                number.of.cases=iter_num,
                                aneuploid.table = Aneuploid_table_fitted_3day_210212,
                                space.UI = 3, space.SI = space_SI, LB.adjust = F,
                                LB.adjust.risk= 0.02, PL.adjust = T, PL.adjust.risk= 0.04,
                                reset.endo = F,PL.adjust.factor=10000,adjust.history = F)

  #simulation <- FUN.simulation(ages = age_range[1]:age_range[2], number.of.cases=iter_num, LB.adjust = F, LB.adjust.risk= 0.02, PL.adjust = T, PL.adjust.risk= 0.04, reset.endo = F,PL.adjust.factor=10000,adjust.history = F)
  #simulation <- FUN.simulation(number.of.cases=10000,LB.adjust = F, LB.adjust.risk= 0.2, PL.adjust = T, PL.adjust.risk= 0.02, reset.endo = T,PL.adjust.factor=10000,adjust.history = T)

  })

  future_outcome <- futureEndoSim(sim = simulation[[1]])
  cycles_to_outcome <- cycleToOutcome(obj = simulation[[1]])

  summary_table <- summaryEndoSim(outcome=simulation[[1]],losses=simulation[[2]],Abembryo=simulation[[3]],Abendometrium=simulation[[4]],futureOutcome=future_outcome,cyclesToOutcome=cycles_to_outcome)

  new("EndoSim",iter_num=iter_num,age_range=age_range,simulation=list(outcome=simulation[[1]],losses=simulation[[2]],Abembryo=simulation[[3]],Abendometrium=simulation[[4]],futureOutcome=future_outcome, cyclesToOutcome=cycles_to_outcome, summary=summary_table))

}

# create generic method

setMethod("show",
          "EndoSim",
          function(object) {
            cat("EndoSim simulation object \n")
            cat("Number of iterations:",object@iter_num, "\n")
            cat("Age range:",object@age_range, "\n")
          }
)
