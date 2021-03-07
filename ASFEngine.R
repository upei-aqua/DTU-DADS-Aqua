#### ASfEngine library version 0.20
#### Includes the following functions:
####
#### ASFEngine (TH)
####

ASFEngine <- function() {

  if(verbose) cat("Started engine\n")

  for (iteration in 1:n) {
    iteration <<- iteration
    initializeASFvars()

    while (any(!(aHerd$status %in% c(1,5,6,7)) & gTime < maxTime)) {
      gTime                <<- gTime + 1
      outbreakDetectedLast <<- outbreakDetected
      infHerdNumsLast      <<- infHerdNums ## Vector of infectious IDs

      ## Update production time
      aHerd$FarmProdTime <<- aHerd$FarmProdTime + 1
      aHerd$FarmProdTime[aHerd$FarmProdTime == 731] <<- 1
      FarmInfStatus <- !sapply(aHerd$FarmID, function(x)any(aHerd$Diagnosed))
      aHerd$FarmActive[aHerd$FarmProdTime > 0 & aHerd$FarmProdTime < (20*30) & FarmInfStatus[aHerd$FarmID]] <<- TRUE

      if(verbose) cat("Time:",gTime,"\n")

      updateHerds()
      limitMovements()
      SurvHerds()

      ## Update relative effect of measures on DC, IMC and ILC
      if (outbreakDetected) {

        for (i in 1:length(controlMethods)) {
          controlMethods[[i]]$day()
        }

      }

      if (outbreakDetected) {

        for (i in 1:length(InterMethods)) {
          InterMethods[[i]]$day()
        }

      }

      ## Finding the indexes of infectious herds
      tmpTagged <- which(aHerd$status       == 6 &
                         aHerd$SusAgain     == 0 &
                         aHerd$timeInfected < Inf)

      infHerdNums <<- unique(c(tmpTagged,
                               (aInfHerd$getIDs())[aInfHerd$getstatus() %in% 3:4]))

      if (verbose) print(c("aInfHerd$getIDs: ",aInfHerd$getIDs()))

      ## Making newInfections if there still are some infected herds.
      ## newInfFunctions is given as an input to ASFoptions() and translated
      ## into newInfMethods
      if (length(infHerdNums) > 0 & length(aInfHerd$getIDs()) > 0) {

        for (i in 1:length(newInfMethods)){
          newInfMethods[[i]]$day()

        }
      }

      summaryFunction("day")

      #print(c(iteration,gTime))
      #print(table(aHerd$status))
    }

    summaryFunction("iter")

    ## Reseting the distance function and the infected herd function
    Dist$wipe()
    aInfHerd$wipe()

    for (i in 1:length(newInfMethods)) newInfMethods[[i]]$cleaniteration()
    for (i in 1:length(InterMethods))  InterMethods[[i]]$cleaniteration()

    gc(verbose = FALSE, reset = TRUE)

  }
}
