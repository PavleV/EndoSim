# set of functions for sampling the simulated population


#' Create an EndoSim simulation object
#'
#' This function initialises an EndoSim object with initial settings. It requires age range and
#' number of iterations.
#'
#' @param obj A simulation object (required).
#' @param sample_size Number of biopsies to retrieve from simulation.
#' @param ages Age range for sampling.
#' @param num_prev_losses Number of previous losses for sampling.
#' @param seed Set seed. Default NULL.
#' @return Table of biopsies
#' @export
sampleEndoSim <- function(obj, sample_size = NULL, ages=c(30,40), num_prev_losses = 1:5, seed = NULL){

  # sample rows and columns

  if(!is.null(sample_size)){
    if(obj@iter_num < sample_size){
      return(cat("Error. Sample size cannot be larger than number of iterations."))
    }
    observation_row <- sample(1:obj@iter_num, size=sample_size)
    data_sampled <- obj@simulation$outcome[observation_row,]
  }else{
    data_sampled <- obj@simulation$outcome
    sample_size <- nrow(data_sampled)
  }

  cycle_col <- sample(1:(length(obj@age_range[1]:obj@age_range[2])*12),size=sample_size,replace=T)

  cycles_to_outcome <- matrix(NA,nrow=nrow(data_sampled),ncol=312)

  for(i in (312-1):1){

    cycles_to_outcome[data_sampled[,i+1] != "NP",i] <- 1
    cycles_to_outcome[data_sampled[,i+1] == "NP",i] <- cycles_to_outcome[data_sampled[,i+1] == "NP",i+1] + 1

  }

  return(cycles_to_outcome)

}

cycleToOutcome <- function(obj){

  # sample rows and columns

  cycles_to_outcome <- matrix(NA,nrow=nrow(obj),ncol=ncol(obj))

  for(i in (ncol(obj)-1):1){

    cycles_to_outcome[obj[,i+1] != "NP",i] <- 1
    cycles_to_outcome[obj[,i+1] == "NP",i] <- cycles_to_outcome[obj[,i+1] == "NP",i+1] + 1

  }

  return(cycles_to_outcome)

}



futureEndoSim <- function(sim){

  result <- matrix(NA,nrow(sim),ncol(sim))
  result[,ncol(sim)] <- sim[,ncol(sim)]

  for(i in (ncol(sim)-1):1){

    result[sim[,i] != "NP", i] <- sim[sim[,i] != "NP" ,i]
    result[sim[,i] == "NP", i] <- result[sim[,i] == "NP", i+1]

  }

  return(result)

}
