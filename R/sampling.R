# set of functions for sampling the simulated population


#' Sampling an EndoSim simulation object
#'
#' This function samples an EndoSim object. It requires age range and
#' number of iterations.
#'
#' @param summary_data A simulation summary table (required).
#' @param sample_size Number of biopsies to retrieve from simulation. Default NULL.
#' @param ages Age range for sampling.
#' @param num_prev_losses Number of previous losses for sampling.
#' @param seed Set seed. Default NULL.
#' @return Table of biopsies
#' @export
sampleEndoSim <- function(summary_data, sample_size = NULL, ages=c(30,40), num_prev_losses = 1:5, seed = NULL){

  if(!is.null(seed)){
    set.seed(1984)
  }

  subdata <- subset(summary_data, Col.num >= ages[1] & Col.num <= ages[2] & History <= 5 & cyclesToOutcome <= 24)

  observation_order <- sample(unique(subdata$Row.num))

  result <- data.frame(Row.num=numeric(),Col.num=numeric(),Outcome=character(),History=numeric(),AbEmbryo=logical(),AbEndometrium=logical(),futureOutcome=character(),cyclesToOutcome=numeric())

  for(i in observation_order){

    this.subdata <- subdata[subdata$Row.num == i,]
    this.subdata <- this.subdata[sample(1:nrow(this.subdata),1),]

    result <- rbind(result,this.subdata)

  }

  return(result)

}



#' Tabulate number of cycles to future outcome in an EndoSim simulation object
#'
#' This function tabulates the number of cycles to future outcome for any given cycle in an EndoSim simulation object.
#'
#' @param obj An outcome slot from simulation object (required).
#' @return Table of number of cycles to next outcome
#' @export
cycleToOutcome <- function(obj){

  cycles_to_outcome <- matrix(NA,nrow=nrow(obj),ncol=ncol(obj))

  for(i in (ncol(obj)-1):1){

    cycles_to_outcome[obj[,i+1] != "NP",i] <- 1
    cycles_to_outcome[obj[,i+1] == "NP",i] <- cycles_to_outcome[obj[,i+1] == "NP",i+1] + 1

  }

  return(cycles_to_outcome)

}


#' Tabulate future outcome in an EndoSim simulation object
#'
#' This function tabulates the future outcome for any given cycle in an EndoSim simulation object.
#'
#' @param sim An outcome slot from simulation object (required).
#' @return Table of future outcomes
#' @export
futureEndoSim <- function(sim){

  result <- matrix(NA,nrow(sim),ncol(sim))
  result[,ncol(sim)] <- sim[,ncol(sim)]

  for(i in (ncol(sim)-1):1){

    result[sim[,i] != "NP", i] <- sim[sim[,i] != "NP" ,i]
    result[sim[,i] == "NP", i] <- result[sim[,i] == "NP", i+1]

  }

  return(result)

}


summaryEndoSim <- function(outcome,losses,Abembryo,Abendometrium,futureOutcome,cyclesToOutcome){

  melted_outcome <- reshape2::melt(outcome)
  melted_losses <- reshape2::melt(losses)
  melted_Abembryo <- reshape2::melt(Abembryo)
  melted_Abendometrium <- reshape2::melt(Abendometrium)
  melted_futureOutcome <- reshape2::melt(futureOutcome)
  melted_cyclesToOutcome <- reshape2::melt(cyclesToOutcome)

  combined_table <- data.frame(Row.num=melted_outcome$Var1,Col.num=melted_outcome$Var2,Outcome=melted_outcome$value,History=melted_losses$value,AbEmbryo=melted_Abembryo$value,AbEndometrium=melted_Abendometrium$value,futureOutcome=melted_futureOutcome$value,cyclesToOutcome=melted_cyclesToOutcome$value)

  return(combined_table)

}
