#### LIMIT_MOVEMENTS library version 0.20
#### Includes the following functions:
####
#### limitMovements
####

# ----Mortality-based outbreak detection----------------------------------------
limitMovements <- function() {

  # If the first case is not detected, then get all clinical herds and the time
  # they started to show clinical signs
  if (!outbreakDetected) {
    indexDiag  <- which(aHerd$status == 4)
    SickTime   <- aInfHerd$getTClic(indexDiag)
    # Calculate the expected mortality+sickness from the moment the animals got sick
    ExpectMort <- (gTime - SickTime) * aHerd$ExpMortality[indexDiag]
    tmp        <-
      (aHerd$Mortality[indexDiag] >= (ExpectMort * MortalityIncrease1)) &
      (aHerd$Mortality[indexDiag] >= (aHerd$CageSize[indexDiag] * FirstDetPara1))

    indexDiag  <- indexDiag[tmp]

      if (length(indexDiag) > 0 & gDaysUntilBaseline == 0) {
        gDaysUntilBaseline   <<- gTime
        outbreakDetected     <<- TRUE
      }
  }

  # Determine the herds that will be diagnosed today and set them to queue for
  # culling. this is for detection after first detection
  if (outbreakDetected & gTime > gDaysUntilBaseline) {
    indexDiag <- which(aHerd$status == 4)
    SickTime  <- aInfHerd$getTClic(indexDiag)
    # Calculate the expected proportion of mortality+sickness from the moment the
    # animals got sick
    ExpectMort <- (gTime - SickTime) * aHerd$ExpMortality[indexDiag]
    tmp        <-
      (aHerd$Mortality[indexDiag] >= (ExpectMort * MortalityIncrease2)) &
      (aHerd$Mortality[indexDiag] >= (aHerd$CageSize[indexDiag] * FirstDetPara2))

    indexDiag  <- indexDiag[tmp]

    if (length(indexDiag) > 0) {
      indexDiag <- indexDiag[runif(length(indexDiag)) <= ProbSelDiag]
    }
  }

  if (outbreakDetected) {

    if (length(indexDiag) > 0) {
      aHerd$Diagnosed[indexDiag]     <<- TRUE
      aHerd$diagnosisTime[indexDiag] <<- gTime
      aHerd$DiagMode[indexDiag]      <<- 2
      aHerd$FarmDiag[aHerd$FarmID %in% aHerd$FarmID[indexDiag]] <<- TRUE
      aInfHerd$setDiagnosed(indexDiag)
      # Adding newly diagnosed herds to the depopQueue and placing them after
      # those herds that were previously diagnosed and before non diagnosed
      # herds tagged by interventions.
      depopQueueDiag <- aHerd$Diagnosed[depopQueue[,1]]
      depopQueue    <<- rbind(
        depopQueue[depopQueueDiag,],
        cbind(indexDiag,gTime),
        depopQueue[!depopQueueDiag,])
      # print(depopQueue)
    }
  }

  # Check for active surveillance and update the times
  aHerd$timeToASV[aHerd$timeToASV < gTime] <<-
    aHerd$timeToASV[aHerd$timeToASV < gTime] + 90
} ##EndOf function
