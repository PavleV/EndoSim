# load data

data("Aneuploid_table_fitted_3day_210212")

# required packages

#require(ggplot2)
#require(reshape2)
#library(ggpubr)
#library(plyr)
#library(tidyr)
#library(dplyr)

# Function to determine openness to conception by maternal age.
# Given a vector of maternal ages and a conversion table, FUN.open.byAge() maps the ages to probabilities.
# Probabilities are then used to sample a binomial distribution and produce a vector of logical values.
# Default conversion table does not currently come from empirical data, but is meant to peak around 30 years of age.
# Visualize conversion table with following code: plot(20:46,(dnorm(20:46,30,8)/max(dnorm(20:46,30,8))))


FUN.open.byAge <- function(ages=c(26,34,32,42,23), conversion.table = data.frame(Age = 20:46,
                                                                                 prob = dnorm(20:46,30,8)/max(dnorm(20:46,30,8)))){

  probabilities <- plyr::mapvalues(ages, from=conversion.table$Age, to=conversion.table$prob, warn_missing = F)

  outcome <- rbinom(length(ages),1,probabilities)
  as.logical(outcome)
}


# Function to determine openness to conception by previous-live-birth.
# Given a vector of number of previous live births and a conversion table, FUN.open.byPrevLB() maps the live births to probabilities.
# Probabilities are then used to sample a binomial distribution and produce a vector of logical values.
# Default conversion table does not currently come from empirical data, but is meant to convey decrease with number of previous births.
# Visualize conversion table with following code: plot(0:325,c(1,0.5,0.25,0.15,seq(from=0.10, to=0, length.out = 322)))


FUN.open.byPrevLB <- function(prevLBs=c(0,1,1,2,5), conversion.table = data.frame(prevLB = 0:325, prob = c(1,0.5,0.25,0.15,seq(from=0.10, to=0, length.out = 322)))){

  probabilities <- plyr::mapvalues(prevLBs, from=conversion.table$prevLB, to=conversion.table$prob,warn_missing = F)

  outcome <- rbinom(length(prevLBs),1,probabilities)
  as.logical(outcome)

}

# Function to determine openness to conception given spacing from previous events.
# Given a table of outcomes, a column number, the specific event and desired spacing, FUN.spacing() returns a logical where True indicates enough spacing.

FUN.spacing <- function(column=14, mydata=result, event="SI", spacing=12){

  if(column < 1+spacing){outcome <- apply(as.matrix(mydata[,1:column]), 1, function(r) any(r %in% event))}
  else{outcome <- apply(mydata[,(column-spacing):(column-1)], 1, function(r) any(r %in% event))}

  !outcome
}

# Function to determine embryonic aneuploidy by maternal age.
# Given a vector of maternal ages and a risk of aneuploidy table, FUN.aneuploidy() returns a logical based on sampling of the binomial distribution.
# Maternal ages beyond scope of conversion table are converted to either max or min age values before mapping.

FUN.aneuploidy <- function(maternal.age = c(18,26,49), aneuploidytable = Aneuploid_table_fitted_3day_210212){

  maternal.age[maternal.age < min(aneuploidytable$age)] <- min(aneuploidytable$age)
  maternal.age[maternal.age > max(aneuploidytable$age)] <- max(aneuploidytable$age)

  probabilities <- plyr::mapvalues(maternal.age, from=aneuploidytable$age, to=aneuploidytable$prob,warn_missing = F)

  outcome <- rbinom(length(maternal.age),1,probabilities)
  as.logical(outcome)

}

# Function to determine abnormal endometrium given a vector or probabilities.
# Logical output sampled from binomial distribution.

FUN.endoerror <- function(endoerror.prob){

  outcome <- rbinom(length(endoerror.prob),1,endoerror.prob)
  as.logical(outcome)

}


# Function to adjust endometrial abnormality risk by previous outcome.
# If PL.adjust and LB.adjust options are set to TRUE, original risk is adjusted by adjust.risk number given.
# Outcome must be between 0 and 1.


FUN.adjust <- function(original.risk=c(0,0,0), prevOutcome = c("SI","UI","NP"), PL.adjust.risk= 0, PL.adjust=T,  LB.adjust.risk= 0, LB.adjust=T){

  adjusted.risk <- original.risk

  if(PL.adjust){
    adjusted.risk[prevOutcome == "UI"] <- adjusted.risk[prevOutcome == "UI"] + PL.adjust.risk
  }

  if(LB.adjust){
    adjusted.risk[prevOutcome == "SI"] <- adjusted.risk[prevOutcome == "SI"] - LB.adjust.risk
  }

  adjusted.risk[adjusted.risk > 1 ] <- 1
  adjusted.risk[adjusted.risk < 0 ] <- 0
  adjusted.risk

}

# Function to adjust abnormal endometrium probability based on previous number of losses.

FUN.adjust.history <- function(original.risk=c(0,0,0), prevLosses=c(2,1,0), PL.adjust.risk= 0, PL.adjust=T,  LB.adjust.risk= 0, LB.adjust=T, PL.adjust.factor=1000){

  adjusted.risk <- original.risk

  if(PL.adjust){

    adjusted.risk[prevLosses > 0] <- adjusted.risk[prevLosses > 0] + ((PL.adjust.risk * prevLosses[prevLosses > 0]) + 2^prevLosses[prevLosses > 0]/PL.adjust.factor)
  }

  if(LB.adjust){
    #adjusted.risk[prevOutcome == "SI"] <- adjusted.risk[prevOutcome == "SI"] - LB.adjust.risk
    #adjusted.risk[prevLosses > 0] <- (adjust.risk * prevLosses) + 2^prevLosses/100

  }

  adjusted.risk[adjusted.risk > 1 ] <- 1
  adjusted.risk[adjusted.risk < 0 ] <- 0
  adjusted.risk

}

# Function to combine information on abnormal endometrium, aneuploid embryo and openness to conception.

FUN.outcome <- function(is.endoerror, is.embryoerror, is.open){

  outcome <- rep("NP",length(is.open))

  outcome[is.endoerror & is.open] <- "UI"

  outcome[!is.endoerror & !is.embryoerror & is.open] <- "SI"

  outcome

}

# Simulation function.
# Beta distribution is used to model the abnormal endometrial risk based on shape1 and shape2 parameters.
# Plot risk distribution with hist(rbeta(100000,shape1=1,shape2=30),breaks=50).
# Spacing (in cycles) after UI or SI events before open to conception.
# reset.endo argument determines whether risk of abnormal endometrium is reset from beta distribution after every cycle.
# LB.adjust and PL.adjust options determines if risk is abnormal endometrium risk is adjusted after each SI or UI event.
# PL.adjust.risk and LB.adjust.risk is the risk to be added or subtracted based on UI or SI events.
# adjust.history option determines if abnormal endometrium risk should be adjusted by total number of previous losses using PL.adjust.factor.

FUN.simulation <- function(ages = 20:45, number.of.cases = 1000, beta.shape1 = 1, beta.shape2 = 30,
                           aneuploid.table= Aneuploid_table_fitted_3day_210212, space.UI = 3, space.SI = 12,
                           LB.adjust = T, LB.adjust.risk= 0, PL.adjust = T, PL.adjust.risk= 0,
                           reset.endo = F, adjust.history = F, PL.adjust.factor=1000){

  # set up result matrices.
  # Main result matrix keeps track of UI and SI events with each cycle.
  # PrevLosses matrix keeps track of number of previous losses.

  outcome <- matrix(NA,nrow=number.of.cases,ncol = length(ages)*12)
  result.embryo <- matrix(NA,nrow=number.of.cases,ncol = length(ages)*12)
  result.endometrium <- matrix(NA,nrow=number.of.cases,ncol = length(ages)*12)
  result.prevLosses <- matrix(0,nrow=number.of.cases,ncol = length(ages)*12)

  # set baseline number of previous births and losses (0)
  # set previous outcome ("NP") and openness based on spacing to previous UI/SI.
  prevLB.count <- rep(0,number.of.cases)
  prevPL.count <- rep(0,number.of.cases)
  spacing.SI <- rep(T,number.of.cases)
  spacing.UI <- rep(T,number.of.cases)

  # set baseline endometrium error probability based on beta distribution parameters
  endoerror.prob <- rbeta(number.of.cases,beta.shape1,beta.shape2)

  # loop through the cycles assuming 12 cycles per year
  for(i in 1:ncol(outcome)){

    column.age <- min(ages)+floor(i/12) # round current cycle number to obtain maternal age

    is.open <- FUN.open.byAge(rep(column.age,number.of.cases)) & FUN.open.byPrevLB(prevLB.count) & spacing.SI & spacing.UI # determine openness

    is.embryoerror <- FUN.aneuploidy(rep(column.age,number.of.cases),aneuploidytable =aneuploid.table) # determine abnormal embryo based on maternal age

    if(reset.endo){endoerror.prob <- rbeta(number.of.cases,beta.shape1,beta.shape2)}# optional reset of abnormal endometrium probability to random value from binomial dist

    if(adjust.history & i > 1){
      endoerror.prob <- FUN.adjust.history(original.risk = endoerror.prob, prevLosses = prevPL.count, PL.adjust.factor = PL.adjust.factor, PL.adjust.risk = PL.adjust.risk)
    } # optional adjustment of abnormal endometrium probability based on number of previous losses

    is.endoerror <- FUN.endoerror(endoerror.prob) # determine abnormal endometrium based on probability

    outcome[,i] <- FUN.outcome(is.endoerror, is.embryoerror, is.open) # combine information on abnormal endometrium, embryo and openness
    result.embryo[,i] <- is.embryoerror
    result.endometrium[,i] <- is.endoerror

    # increase previous live birth and loss counts depending on outcome to be used in next cycle

    prevLB.count[outcome[,i] == "SI"] <- prevLB.count[outcome[,i] == "SI"] + 1

    prevPL.count[outcome[,i] == "UI"] <- prevPL.count[outcome[,i] == "UI"] + 1

    # update matrix of previous losses
    if(i < ncol(outcome)){result.prevLosses[,i+1] <- prevPL.count}

    # adjust abnormal endometrium risk to be used in next cycle based on previous outcome.

    endoerror.prob <- FUN.adjust(original.risk = endoerror.prob, prevOutcome = outcome[,i], PL.adjust.risk = PL.adjust.risk, PL.adjust = PL.adjust,  LB.adjust.risk = LB.adjust.risk, LB.adjust = LB.adjust )

    spacing.SI <- FUN.spacing(column=i,mydata=outcome,event="SI",spacing=space.SI)# check spacing of SI

    spacing.UI <- FUN.spacing(column=i,mydata=outcome,event="UI",spacing=space.UI)# check spacing of UI

  }

  list(outcome,result.prevLosses,result.embryo,result.endometrium)

}

# Function to combine information on abnormal endometrium, aneuploid embryo and openness to conception.

FUN.outcome2 <- function(is.endoerror, is.embryoerror, is.open){

  outcome <- rep("NP",length(is.open))

  outcome[is.open] <- "SI"

  outcome[(is.endoerror | is.embryoerror) & is.open] <- "UI"


  outcome

}

## simple simulation 2


FUN.simulation2 <- function(ages = 20:45, number.of.cases=10000, aneuploid.table = Aneuploid_table_fitted_3day_210212, beta.shape1 = 1, beta.shape2 = 30, space.UI = 3, space.SI = 12, reset.endo=F){

  result <- matrix(NA,nrow=number.of.cases,ncol = length(ages)*12)
  result.prevLosses <- matrix(0,nrow=number.of.cases,ncol = length(ages)*12)
  # set baseline endometrium error probability based on beta distribution parameters
  endoerror.prob <- rbeta(number.of.cases,beta.shape1,beta.shape2)

  prevLB.count <- rep(0,number.of.cases)
  prevPL.count <- rep(0,number.of.cases)
  spacing.SI <- rep(T,number.of.cases)
  spacing.UI <- rep(T,number.of.cases)

  # loop through the cycles assuming 12 cycles per year
  for(i in 1:ncol(result)){

    column.age <- min(ages)+floor(i/12) # round current cycle number to obtain maternal age

    if(reset.endo){endoerror.prob <- rbeta(number.of.cases,beta.shape1,beta.shape2)}# optional reset of abnormal endometrium probability to random value from binomial dist

    is.open <- FUN.open.byAge(rep(column.age,number.of.cases)) & FUN.open.byPrevLB(prevLB.count) & spacing.SI & spacing.UI # determine openness

    is.embryoerror <- FUN.aneuploidy(rep(column.age,number.of.cases))
    is.endoerror <- FUN.endoerror(endoerror.prob)

    result[,i] <- FUN.outcome2(is.endoerror, is.embryoerror, is.open)

    # increase previous live birth and loss counts depending on outcome to be used in next cycle

    prevLB.count[result[,i] == "SI"] <- prevLB.count[result[,i] == "SI"] + 1

    prevPL.count[result[,i] == "UI"] <- prevPL.count[result[,i] == "UI"] + 1

    # update matrix of previous losses
    if(i < ncol(result)){result.prevLosses[,i+1] <- prevPL.count}

    spacing.SI <- FUN.spacing(column=i,mydata=result,event="SI",spacing=space.SI)# check spacing of SI

    spacing.UI <- FUN.spacing(column=i,mydata=result,event="UI",spacing=space.UI)# check spacing of UI

  }

  list(result,result.prevLosses)

}



# Simulation function.
# Beta distribution is used to model the abnormal endometrial risk based on shape1 and shape2 parameters.
# Plot risk distribution with hist(rbeta(100000,shape1=1,shape2=30),breaks=50).
# Spacing (in cycles) after UI or SI events before open to conception.
# reset.endo argument determines whether risk of abnormal endometrium is reset from beta distribution after every cycle.
# LB.adjust and PL.adjust options determines if risk is abnormal endometrium risk is adjusted after each SI or UI event.
# PL.adjust.risk and LB.adjust.risk is the risk to be added or subtracted based on UI or SI events.
# adjust.history option determines if abnormal endometrium risk should be adjusted by total number of previous losses using PL.adjust.factor.

FUN.simulation3 <- function(ages = 20:45, number.of.cases = 1000, beta.shape1 = 1, beta.shape2 = 30,
                            aneuploid.table= Aneuploid_table_fitted_3day_210212, space.UI = 3, space.SI = 12,
                            LB.adjust = T, LB.adjust.risk= 0, PL.adjust = T, PL.adjust.risk= 0,
                            reset.endo = F, reset.prob = 0.1, adjust.history = F, PL.adjust.factor=1000){

  # set up result matrices.
  # Main result matrix keeps track of UI and SI events with each cycle.
  # PrevLosses matrix keeps track of number of previous losses.

  result <- matrix(NA,nrow=number.of.cases,ncol = length(ages)*12)
  result.prevLosses <- matrix(0,nrow=number.of.cases,ncol = length(ages)*12)

  # set baseline number of previous births and losses (0)
  # set previous outcome ("NP") and openness based on spacing to previous UI/SI.
  prevLB.count <- rep(0,number.of.cases)
  prevPL.count <- rep(0,number.of.cases)
  spacing.SI <- rep(T,number.of.cases)
  spacing.UI <- rep(T,number.of.cases)

  # set baseline endometrium error probability representing background risk factors

  endoerror.prob.base <- rbeta(number.of.cases,beta.shape1,beta.shape2)

  # set baseline endometrium error probability based on history

  endoerror.prob.history <- rep(0,number.of.cases)

  # loop through the cycles assuming 12 cycles per year
  for(i in 1:ncol(result)){

    column.age <- min(ages)+floor(i/12) # round current cycle number to obtain maternal age

    is.open <- FUN.open.byAge(rep(column.age,number.of.cases)) & FUN.open.byPrevLB(prevLB.count) & spacing.SI & spacing.UI # determine openness

    is.embryoerror <- FUN.aneuploidy(rep(column.age,number.of.cases)) # determine abnormal embryo based on maternal age

    #if(reset.endo){endoerror.prob <- rbeta(number.of.cases,beta.shape1,beta.shape2)}# optional reset of abnormal endometrium probability to random value from binomial dist

    if(adjust.history & i > 1){
      endoerror.prob.history <- FUN.adjust.history(original.risk = endoerror.prob.history, prevLosses = prevPL.count, PL.adjust.factor = PL.adjust.factor)
    } # optional adjustment of abnormal endometrium probability based on number of previous losses

    endoerror.prob <- endoerror.prob.base + endoerror.prob.history

    is.endoerror <- FUN.endoerror(endoerror.prob) # determine abnormal endometrium based on probability

    result[,i] <- FUN.outcome(is.endoerror, is.embryoerror, is.open) # combine information on abnormal endometrium, embryo and openness

    # increase previous live birth and loss counts depending on outcome to be used in next cycle

    prevLB.count[result[,i] == "SI"] <- prevLB.count[result[,i] == "SI"] + 1

    prevPL.count[result[,i] == "UI"] <- prevPL.count[result[,i] == "UI"] + 1

    # update matrix of previous losses
    if(i < ncol(result)){result.prevLosses[,i+1] <- prevPL.count}

    # adjust abnormal endometrium risk to be used in next cycle based on previous outcome.

    endoerror.prob.history <- FUN.adjust(original.risk = endoerror.prob.history, prevOutcome = result[,i], PL.adjust.risk = PL.adjust.risk, PL.adjust = PL.adjust,  LB.adjust.risk = LB.adjust.risk, LB.adjust = LB.adjust )

    spacing.SI <- FUN.spacing(column=i,mydata=result,event="SI",spacing=space.SI)# check spacing of SI

    spacing.UI <- FUN.spacing(column=i,mydata=result,event="UI",spacing=space.UI)# check spacing of UI

    #
    if(reset.endo){
      is.reset <- rbinom(number.of.cases, 1, reset.prob)
      endoerror.prob.base[as.logical(is.reset)] <- rbeta(sum(is.reset),beta.shape1,beta.shape2)
    }

  }

  list(result,result.prevLosses)

}


## Simulation function tracking all variables as list items.

FUN.simulation4 <- function(ages = 20:45, number.of.cases = 1000, beta.shape1 = 1, beta.shape2 = 30,
                            aneuploid.table= Aneuploid_table_fitted_3day_210212, space.UI = 3, space.SI = 12,
                            LB.adjust = T, LB.adjust.risk= 0, PL.adjust = T, PL.adjust.risk= 0,
                            reset.endo = F, reset.prob = 0.1, adjust.history = F, PL.adjust.factor=1000, seed=NULL){

  if(!is.null(seed)){
    set.seed(seed)
  }

  # set up result matrices.
  # Main outcome matrix keeps track of UI and SI events with each cycle.
  # PrevLosses matrix keeps track of number of previous losses.

  outcome <- matrix(NA,nrow=number.of.cases,ncol = length(ages)*12)
  result.embryo <- matrix(NA,nrow=number.of.cases,ncol = length(ages)*12)
  result.endometrium <- matrix(NA,nrow=number.of.cases,ncol = length(ages)*12)
  result.prevLosses <- matrix(0,nrow=number.of.cases,ncol = length(ages)*12)

  # set baseline number of previous births and losses (0)
  # set previous outcome ("NP") and openness based on spacing to previous UI/SI.
  prevLB.count <- rep(0,number.of.cases)
  prevPL.count <- rep(0,number.of.cases)
  spacing.SI <- rep(T,number.of.cases)
  spacing.UI <- rep(T,number.of.cases)

  # set baseline endometrium error probability representing background risk factors

  endoerror.prob.base <- rbeta(number.of.cases,beta.shape1,beta.shape2)

  # set baseline endometrium error probability based on history

  endoerror.prob.history <- rep(0,number.of.cases)

  # loop through the cycles assuming 12 cycles per year
  for(i in 1:ncol(outcome)){

    column.age <- min(ages)+floor(i/12) # round current cycle number to obtain maternal age

    is.open <- FUN.open.byAge(rep(column.age,number.of.cases)) & FUN.open.byPrevLB(prevLB.count) & spacing.SI & spacing.UI # determine openness

    is.embryoerror <- FUN.aneuploidy(rep(column.age,number.of.cases)) # determine abnormal embryo based on maternal age

    #if(reset.endo){endoerror.prob <- rbeta(number.of.cases,beta.shape1,beta.shape2)}# optional reset of abnormal endometrium probability to random value from binomial dist

    if(adjust.history & i > 1){
      endoerror.prob.history <- FUN.adjust.history(original.risk = endoerror.prob.history, prevLosses = prevPL.count, PL.adjust.factor = PL.adjust.factor)
    } # optional adjustment of abnormal endometrium probability based on number of previous losses

    endoerror.prob <- endoerror.prob.base + endoerror.prob.history

    is.endoerror <- FUN.endoerror(endoerror.prob) # determine abnormal endometrium based on probability

    outcome[,i] <- FUN.outcome(is.endoerror, is.embryoerror, is.open) # combine information on abnormal endometrium, embryo and openness
    result.embryo[,i] <- is.embryoerror
    result.endometrium[,i] <- is.endoerror

    # increase previous live birth and loss counts depending on outcome to be used in next cycle

    prevLB.count[outcome[,i] == "SI"] <- prevLB.count[outcome[,i] == "SI"] + 1

    prevPL.count[outcome[,i] == "UI"] <- prevPL.count[outcome[,i] == "UI"] + 1

    # update matrix of previous losses
    if(i < ncol(outcome)){result.prevLosses[,i+1] <- prevPL.count}

    # adjust abnormal endometrium risk to be used in next cycle based on previous outcome.

    endoerror.prob.history <- FUN.adjust(original.risk = endoerror.prob.history, prevOutcome = outcome[,i], PL.adjust.risk = PL.adjust.risk, PL.adjust = PL.adjust,  LB.adjust.risk = LB.adjust.risk, LB.adjust = LB.adjust )

    spacing.SI <- FUN.spacing(column=i,mydata=outcome,event="SI",spacing=space.SI)# check spacing of SI

    spacing.UI <- FUN.spacing(column=i,mydata=outcome,event="UI",spacing=space.UI)# check spacing of UI

    #
    if(reset.endo){
      is.reset <- rbinom(number.of.cases, 1, reset.prob)
      endoerror.prob.base[as.logical(is.reset)] <- rbeta(sum(is.reset),beta.shape1,beta.shape2)
    }

  }

  list(outcome,result.prevLosses,result.embryo,result.endometrium)

}

