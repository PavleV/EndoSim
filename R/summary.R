#

FUN.summary <- function(mysim=test.simulation, ages=20:45){

  result <- data.frame(PrevLosses=numeric(), Count=numeric(), Age=numeric(), Outcome=character())

  for(i in 1:(max(ages)-min(ages))){

    this.start <- i*12
    this.end <- this.start + 11

    mysim1 <- mysim[[1]][,this.start:this.end]
    mysim2 <- mysim[[2]][,this.start:this.end]

    this.UI.result <- reshape2::melt(table(mysim2[which(mysim1 == "UI")]))
    this.UI.result <- dplyr::mutate(this.UI.result,Age=ages[i],Outcome="UI")
    colnames(this.UI.result) <- c("PrevLosses","Count","Age","Outcome")

    this.SI.result <- reshape2::melt(table(mysim2[which(mysim1 == "SI")]))
    this.SI.result <- dplyr::mutate(this.SI.result,Age=ages[i],Outcome="SI")
    colnames(this.SI.result) <- c("PrevLosses","Count","Age","Outcome")

    result <- rbind(result,this.UI.result,this.SI.result)

  }

  result

}

#

FUN.summary.history <- function(mysim=test.simulation, ages=20:45){

  result.history <- matrix("NP", nrow=nrow(mysim[[1]]), ncol=ncol(mysim[[1]]))
  column.history <- rep("NP",nrow(result.history))

  for(i in 2:(ncol(result.history)-1)){

    column.history[mysim[[1]][,i-1] != "NP"] <- paste(column.history[mysim[[1]][,i-1] != "NP"],mysim[[1]][mysim[[1]][,i-1] != "NP",i-1],sep=",")
    result.history[mysim[[1]][,i] != "NP",i] <- column.history[mysim[[1]][,i] != "NP"]

  }

  result <- data.frame(History=numeric(), Count=numeric(), Age=numeric(), Outcome=character())

  for(i in 1:(max(ages)-min(ages))){

    this.start <- i*12
    this.end <- this.start + 11

    mysim1 <- mysim[[1]][,this.start:this.end]
    mysim3 <- stringr::str_sub(result.history[,this.start:this.end],-14)

    this.UI.result <- reshape2::melt(table(mysim3[which(mysim1 == "UI")]))
    this.UI.result <- dplyr::mutate(this.UI.result,Age=ages[i],Outcome="UI")
    colnames(this.UI.result) <- c("History","Count","Age","Outcome")

    this.SI.result <- reshape2::melt(table(mysim3[which(mysim1 == "SI")]))
    this.SI.result <- dplyr::mutate(this.SI.result,Age=ages[i],Outcome="SI")
    colnames(this.SI.result) <- c("History","Count","Age","Outcome")

    result <- rbind(result,this.UI.result,this.SI.result)

  }

  result

}


#

FUN.graphdata <- function(mydata=test.sum){

  mydata.total2 <- mydata %>% group_by(Age, PrevLosses) %>% summarise(Total = sum(Count))

  mydata.sum2.SI <- subset(mydata, Outcome == "SI")

  mydata.merged2 <- merge(mydata.total2,mydata.sum2.SI, by= c("Age","PrevLosses"), all = T)

  mydata.merged2$Count[is.na(mydata.merged2$Count)] <- 0

  mydata.merged2$LB.ratio <- mydata.merged2$Count / mydata.merged2$Total

  mydata.merged2 <- mydata.merged2[,c("Age","PrevLosses","LB.ratio")]

  mydata.merged2

}

#

FUN.graphdata.history <- function(mydata=test.sum){

  mydata.total2 <- mydata %>% group_by(Age, History) %>% summarise(Total = sum(Count))

  mydata.sum2.SI <- subset(mydata, Outcome == "SI")

  mydata.merged2 <- merge(mydata.total2,mydata.sum2.SI, by= c("Age","History"), all = T)

  mydata.merged2$Count[is.na(mydata.merged2$Count)] <- 0

  mydata.merged2$LB.ratio <- mydata.merged2$Count / mydata.merged2$Total

  mydata.merged2 <- mydata.merged2[,c("Age","History","LB.ratio")]

  mydata.merged2

}
