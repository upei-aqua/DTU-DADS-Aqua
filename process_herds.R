#### PROCESS_HERDS library (V. 0.20)
####
#### Includes the following functions:
####
#### SurvHerds          -- Sets farms for surveillance
#### updateHerds        -- Keeps herd file updated
#### DBinf              -- Distance-based spread (between-farm)
#### BCSpread           -- Between-cage spread
#### constructAInfHerd  -- Keeps track of infected cages
####


##############################################################################
## Set herds for queue to surveillance and survay them based on the available
## resources for Surveillance. Surveillance is based on number of herds
## and is carried out for cages but based on farm level. (TH + DP)
##############################################################################

SurvHerds <- function() {

  ### Herds that should be included in Queuing for surveillance
  setInQueue <- unique(aHerd$FarmID[(aHerd$timeToFSV == gTime |
                                     aHerd$timeToSSV == gTime |
                                     aHerd$timeToASV == gTime) &
                                    !(aHerd$Diagnosed) &
                                    !(aHerd$status %in% 5:6)])

  ## Arrange the selected herds for visiting
  setQueue <- matrix(numeric(0), ncol = 2)

  # the categories in the last line of each matrix represent the type of visit
  # 1=PV2, 2=PV1, 3=SV1, 4=SV2, 5=IDC, 6=DC. these categories are used to
  # determine which test is used for the economic analysis of the alternative
  # scenarios.
  if (length(setInQueue) > 0) setQueue <- cbind(setInQueue,gTime)

  # Set herds that are in the zones or traced for queuing to surveillence and
  # remove duplicates here we priortise the ones that will have PCR and Serology
  # for sure.
  if (dim(setQueue)[1] > 0) zoneQueue <<- rbind(zoneQueue,setQueue)

  if (dim(zoneQueue)[1] > 0) {
    zoneQueue <<- zoneQueue[!duplicated(zoneQueue[,1]),, drop = FALSE]
  }


  # Move herds from queue to being Surveyed
  if (dim(zoneQueue)[1] > 0) {

    SurvMat <- zoneQueue
    survInd <- 1:dim(SurvMat)[1]

    if (length(survInd) <= (CapSurvay)) {
          toSurv <- survInd
    } else {
      toSurv <- survInd[1:sum(1:length(survInd) <= CapSurvay)]
    }

    SurvMat2 <- SurvMat[toSurv,, drop = FALSE]

    # Survey herds: following clinical surveillance, herds maybe tested
    # (serology + PCR) once the herd has doubled mortality (including sick
    # animals)

    aHerd$Visited[aHerd$FarmID %in% SurvMat2[,1]] <<-
      aHerd$Visited[aHerd$FarmID %in% SurvMat2[,1]] + 1

    toBeCulled <- which((aHerd$FarmID %in% SurvMat2[,1]) &
                        !(aHerd$Diagnosed) &
                        (aHerd$status %in% 3:4))

    if (length(toBeCulled) > 0) {
      TMP1     <- aInfHerd$getInfected(toBeCulled)
      FarmIDs  <- sort(unique(aHerd$FarmID[toBeCulled]))

      NumbSamples <- cbind(FarmIDs,
        tapply(aHerd$NumSamp[aHerd$FarmID %in% FarmIDs],
               aHerd$FarmID[aHerd$FarmID %in% FarmIDs],
               mean))

      herdSize    <- cbind(FarmIDs,
        tapply(aHerd$CageSizeVar[aHerd$FarmID %in% FarmIDs],
               aHerd$FarmID[aHerd$FarmID %in% FarmIDs],
               sum))

      indexNS1 <- match(aHerd$FarmID, NumbSamples[,1])
      indexNS  <- ifelse(is.na(indexNS1), FALSE, TRUE)
      indexNS1 <- indexNS1[!is.na(indexNS1)]
      indexHS1 <- match(aHerd$FarmID, herdSize[,1])
      indexHS  <- ifelse(is.na(indexHS1), FALSE, TRUE)
      indexHS1 <- indexHS1[!is.na(indexHS1)]

      aHerd$NumSamSurv[indexNS]    <<- NumbSamples[indexNS1, 2]
      aHerd$herdSizeSurv[indexHS]  <<- herdSize[indexHS1, 2]

      TMP2           <-  1 - (1 - (aHerd$NumSamSurv[toBeCulled] /
                                   (aHerd$herdSizeSurv[toBeCulled] -
                                    ((TMP1 - 1) / 2)))) ^ TMP1

      TMP2[TMP2 > 1] <- 1
      toBeCulled     <- toBeCulled[runif(length(toBeCulled)) <= TMP2]
      #toBeCulled     <- toBeCulled[rbinom(length(toBeCulled), 1, TMP2) >= 1]

      if (length(toBeCulled) > 0) {
        depopQueue <<-rbind(depopQueue,cbind(toBeCulled,gTime))
        aHerd$Diagnosed[toBeCulled]     <<- TRUE
        aHerd$DiagMode[toBeCulled]      <<- 1
        aHerd$diagnosisTime[toBeCulled] <<- gTime
        aInfHerd$setDiagnosed(toBeCulled)
      }
    }

    if (Detailed) {

      if(length(SurvMat2[,1]) > 0) {
        SurvMatOut  <<- rbind(SurvMatOut,cbind(iteration,gTime,SurvMat2[,1]))
      }

      if(dim(SurvMatOut)[1]>= DumpData){

      # NAME here will be exactly the same as that in the initialization file,
      # so no worries; no overwriting will happen ;-) (TH)
        NAMESH <- paste(runID,"SurvayedHerds.txt",sep="-")
        write.table(SurvMatOut,NAMESH,append=TRUE,col.names = F,row.names = F)
        SurvMatOut <<- matrix(numeric(0),ncol=3)
      }
    }

    aHerd$visitCount[SurvMat2[,1]]  <<- aHerd$visitCount[SurvMat2[,1]] + 1
    zoneQueue                       <<- zoneQueue[-toSurv,, drop = FALSE]

  }
}

##############################################################################
### updateHerds based on Danish project by TH and LEC
##############################################################################
updateHerds <- function () {
  ## Depopulating diagnosed and tagged herds

  if (dim(depopQueue)[1] > 0) {
    ## Remove duplets and those already being depopulated
    tmpIndex    <- !duplicated(depopQueue[,1])
    depopQueue <<- depopQueue[tmpIndex,, drop = FALSE]

    if (CULLALL) {
      getCHR    <- unique(aHerd$FarmID[depopQueue[1]])
      extraCull <- aHerd$ID[aHerd$FarmID %in% getCHR]
      extraCull <- extraCull[!extraCull %in% depopQueue[1]]

      if (length(extraCull) > 0) {
        depopQueue  <<- rbind(depopQueue, cbind(extraCull, gTime))
        InfEC        <- extraCull[aHerd$status[extraCull] %in% c(2,3,4,7)]
        if (length(InfEC) > 0) {
          aHerd$Diagnosed[InfEC]     <<- TRUE
          aHerd$diagnosisTime[InfEC] <<- gTime
          aInfHerd$setDiagnosed(InfEC)
        }
      }
    }

    ## Move herds from queue to being depoped
    cullMat <- matrix(numeric(0), ncol = 3)
    cullMat <- cbind(depopQueue, aHerd$CageSizeCull[depopQueue[,1]])

    if (sum(cullMat[,3]) <= Capacity) {
      beingDepoped <<- rbind(beingDepoped, cbind(cullMat[,1], 1))
      toCull        <- 1:dim(cullMat)[2]

    } else {

      cumNumAnimals <- cumsum(cullMat[,3])
      cullCat       <- which(cumNumAnimals <= Capacity)
      partial       <- which(!(cumNumAnimals <= Capacity))[1]

      if (!is.null(partial)) {
        cullMat[partial, 3] <- (cumNumAnimals[partial] - Capacity)
        aHerd$CageSizeCull[cullMat[partial, 1]] <<- cullMat[partial, 3]
      }

      if (length(cullCat) > 0){
        beingDepoped <<- rbind(beingDepoped, cbind(cullMat[cullCat,1], 1))
      }

      toCull <- cullCat

    }

    if (length(toCull) > 0) depopQueue <<- depopQueue[-toCull,, drop = FALSE]

    if (dim(beingDepoped)[1] > 0) {

      tmpID <- beingDepoped[,1]
      aHerd$timeToRemoveZone[tmpID] <<- gTime
      aHerd$timeCulled[tmpID]       <<- gTime
      aHerd$status[tmpID]           <<- 5
      aInfHerd$delInf(tmpID, ignoreNotInfected = TRUE)

      gDepopulationCount[iteration] <<-
        gDepopulationCount[iteration] + length(tmpID)

      beingDepoped <<- matrix(numeric(0), ncol = 2)

      if (Detailed) {

        DepopMatOut <<- rbind(DepopMatOut, cbind(iteration, gTime, tmpID))

        if (dim(DepopMatOut)[1] >= DumpData) {

          ### NAME here will be exactly the same as that in the initialization file,
          ### so no worries; no overwriting will happen ;-) (TH)
          NAMED <- paste(runID, "DepopHerds.txt", sep = "-")

          write.table(DepopMatOut,
                      NAMED,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE)

          DepopMatOut<<- matrix(numeric(0),ncol=3)
        }
      }
    }
  }

  ## Simulate one day of intra herd dynamics:
  aInfHerd$simDay()
}

##############################################################################
### Distance-based spread as changed by TH and DP
### Determine infection based on distance to infection famrs
##############################################################################

DBinf <- function(label = 1, tStart = 0, tEnd = Inf) {

  ## Functions: 'day' and 'inititeration'
  list(
    init = function() {},

    day  = function() {

      if (gTime > tStart & gTime < tEnd) {
        AllNewInfs    = NULL
        AllNewAnimals = NULL

        #### Distance-based spread from other farms
        IndexFarms    <- unique(aHerd$FarmID)
        ### check vaccine efficacy implementation
        FarmSus       <- tapply(aHerd$RelSusceptibility,aHerd$FarmID,mean)
        ### check vaccine efficacy implementation
        FarmInfect    <- as.logical(tapply(aHerd$Infectiousness,
                                           aHerd$FarmID,
                                           max))
        ## This needs to be check in relation to culling capacity.
        FarmInfect[is.na(FarmInfect)]    <- TRUE
        FarmActive    <- tapply(aHerd$FarmActive,
                                aHerd$FarmID,
                                any)

        ProbMat       <- sapply(IndexFarms, function(x) {
         (exp(-ScalingInf * DistMat[x,] / 1000) / 30)
        })

        diag(ProbMat) <- 0
        ## define infection herd based on status
        ProbMat <- apply(ProbMat, 2, function(x) {
         x * (FarmActive * FarmSus * FarmInfect)
        })

        ProbabInf  <- 1 - apply(ProbMat, 2, function(x) prod(1 - x))

        ### Would the farms be infected or not
        NewInfect  <- rbinom(length(IndexFarms), 1, ProbabInf)
        NewInfFarm <- IndexFarms[NewInfect > 0]

        ### Select a cage per newly infected farm
        if (length(NewInfFarm) > 0) {
          newInf  <- unlist(sapply(NewInfFarm, function(x) {
             tmp  <- aHerd$ID[aHerd$FarmID == x & aHerd$status == 1 & aHerd$FarmActive]
             if (length(tmp) > 1) sample(tmp, 1) else tmp
        }))

          if (length(newInf) > 0) {
            #print(paste("distance", newInf))
            newinfanimals <- round(rpert(length(newInf), 1, 10, 100)) # Derek, remember to check that
            AllNewInfs    = c(AllNewInfs, newInf)
            AllNewAnimals = c(AllNewAnimals,
                              newinfanimals[as.logical(newinfanimals)])
            aHerd$status[newInf]       <<- 2
            aHerd$timeInfected[newInf] <<- gTime
            aHerd$infMode[newInf]      <<- label
            aInfHerd$addInf(AllNewInfs, cbind(AllNewAnimals, 0, 0), 0)
          }
        }
      }
    },

    cleaniteration = function(){
      if (verbose) cat("Entered LASinf$inititeration()\n")
  })
}

##############################################################################
### Between cages spread within a farm as changed by TH and DP
##############################################################################

BCSpread <- function(label = 2, tStart = 0, tEnd = Inf) {
  ## Functions: 'day' and 'inititeration'
  list(
    init = function() {},
    day  = function() {
      if (gTime > tStart & gTime < tEnd) {
        AllNewInfs    = NULL
        AllNewAnimals = NULL

        # Distance-based spread from other farms
        InfectFarms <- unique(aHerd$FarmID[infHerdNums])

        for (i in InfectFarms) {
          SusCagesIF <- aHerd$ID[aHerd$FarmID==i & aHerd$status==1 & aHerd$FarmActive]

          if (length(SusCagesIF) > 0) {

            #NumbInf  <-  aInfHerd$getInfected(aHerd$ID[aHerd$FarmID == i])
            NumbInf  <-  sum(aInfHerd$getInfected(aHerd$ID[aHerd$FarmID == i]))

            # NumbFish <-  sapply(SusCagesIF, function(x) {
            #   sum(aHerd$CageSizeVar[aHerd$FarmID == i & aHerd$ID != x])
            # })
            NumbFish <- sum(aHerd$CageSizeVar[aHerd$FarmID == i & !(aHerd$ID %in% SusCagesIF)])
            
            # printing mismatch between NumbInf and NumbFish (should not occur)
            if (length(NumbInf) != length(NumbFish)) {
              print(paste0("NumbInf = ", length(NumbInf), "; NumbFish = ", length(NumbFish)))
            }

            probInf <- 1 - exp(-BetCageProb * NumbInf / NumbFish)

            ### Would the cage be infected or not
            newInfCage <- rbinom(length(SusCagesIF), 1, probInf)
            newInf     <- SusCagesIF[newInfCage > 0]

            if (length(newInf) > 0) {
              #print(paste("Cage", newInf))
              newinfanimals <- round(rpert(length(newInf), 1, 10, 100)) # Derek, remember to check that
              AllNewInfs    = c(AllNewInfs, newInf)
              AllNewAnimals = c(AllNewAnimals,
                                newinfanimals[as.logical(newinfanimals)])
              aHerd$status[newInf]       <<- 2
              aHerd$timeInfected[newInf] <<- gTime
              aHerd$infMode[newInf]      <<- label
              aInfHerd$addInf(AllNewInfs, cbind(AllNewAnimals, 0, 0), 0)
            }
          }
        }
      }
    },

    cleaniteration = function() {
      if (verbose) cat("Entered LASinf$inititeration()\n")
    }
  )
}

##############################################################################
### Call once to construct aInfHerd object.
##############################################################################

constructAInfHerd <- function() {
  ## Internal variables:
  ## VarDef herds    Matrix with info on infected herds
  ## VarDes Column names are in herdsCol. A matrix without names is faster
  nColHerds  <- 19
  herds      <- matrix(numeric(0), ncol = nColHerds)
  DeadMat    <- matrix(numeric(0), ncol = DaysDead)
  DeadMatSur <- matrix(numeric(0), ncol = DaysSurDead)

  ##Keeping deleted herds
  delHerds <- matrix(numeric(0), ncol = nColHerds)

  herdsCol <- c("Sus1",
                "latent2",
                "SubC3",
                "Clinic4",
                "Immune5",
                "Total6",
                "status7",
                "ID8",
                "p9",
                "NA.latDur10",
                "NA.SubDur11",
                "TClic12",
                "TDiag13",
                "Diagnosed14",
                "InfByDC15",
                "TInfected16",
                "herdType17",
                "Vaccinated18",
                "TLastAnCli19")

  dimnames(herds)[[2]] <- herdsCol

  ## VarDef latent,subclinical  Used to model transitions between stages
  ## VarDes Matrix with a row for each farm and columns for
  ## VarDes   each day latent or subclinical or clinical
  latent      <- matrix(numeric(0), ncol = ncol(herdtypes$latDurFreq))
  subclinical <- matrix(numeric(0), ncol = ncol(herdtypes$SubDurFreq))
  clinical    <- matrix(numeric(0), ncol = ncol(herdtypes$CliDurFreq))

  "rpoly2" <- function(np) {
    tabulate(sample(1:(length(np) - 1),
                    size    = np[1],
                    replace = TRUE,
                    prob    = np[-1]),
             nbins = (length(np) - 1))

  }

  list(
  # List holding all information to be access from outside
  #################################################################
    getIDs          = function() {return(herds[,8])},
    getstatus       = function() {return(herds[,7])},
    getTDiag        = function() {return(herds[,13])},
    getDelHerds     = function() {return(delHerds)},
    getHerds        = function() {return(herds)},
    setHerds        = function(newHerds) {herds <<- newHerds},
    getDiagnosed    = function() {return(herds[,14])},
    getDiagnosedIDs = function() {return(herds[herds[,14] == 1,8])},

    ### Time the herd showed clinical signs (TH)
    getTClic = function(IDs) {
      Herds <- herds
      index <- match(IDs, Herds[,8])

      if (any(is.na(index))) {
        warning("ConstructAInfHerd: Some IDs not in aInfHerd!")
        index <- index[!is.na(index)]
      }

      tmp <- Herds[index, 12]
      return(tmp)

    },

    setDiagnosed = function(IDs) {
      index <- match(IDs, herds[,8])

      if (any(is.na(index))) {
        # warning("ConstructAInfHerd: Some IDs not in aInfHerd!")
        # this warning is stopped, because some herds will be culled and hence
        # treated as diagnosed if they have the same chr as a detected herd.
        index <- index[!is.na(index)]
      }

      herds[index, 14] <<- 1
    },

    getDelIDs = function() {return(delHerds[,8])},
    getInfness = function(IDs = herds[,8]) {
      index <- match(IDs,herds[,8])

      if (any(is.na(index))) {
        warning("ConstructAInfHerd: Some IDs not in aInfHerd!")
        index <- index[!is.na(index)]
      }

      tmp <- (herds[index,3] + herds[index,4]) /
             (herds[index,6] - (herds[index,5] * PerDeadAnim))
      return(tmp)
    },

    anyInf = function() return(nrow(herds) > 0),

    ##numInfAnimals: three column matrix for latent, subC, and Clin
    addInf = function(IDs, numInfAnimals, DC) {
      ##Only those that were not already infected
      tmpID <- (IDs %in% herds[,8])

      if (any(tmpID)) {
        IDs <- IDs[!tmpID]
        numInfAnimals <- numInfAnimals[!tmpID,, drop = FALSE]
        DC <- DC[!tmpID]
      }

      if (length(IDs) > 0) {
        tmp <- cbind(aHerd$Sus[IDs], 0, 0, 0, 0, aHerd$Sus[IDs], 1,
                     IDs, aHerd$p[IDs], 0, 0, Inf, Inf, 0, DC, gTime,
                     aHerd$herdType[IDs], 0, 0)

        tmp[,1:4] <- tmp[,1:4] + cbind(-rowSums(numInfAnimals), numInfAnimals)
        tmp[,7]   <- apply(tmp[, 1:4, drop = FALSE], 1, function(x) max((1:4)[x >= 1]))

        ## Updating diagnosis if initial status is 4 (Clinical
        if (is.na(tmp[1,1])) browser()
        if (any(tmpIndex <- (tmp[,7] == 4))) {
          ## Time herd showed clinical signs
          tmp[tmpIndex,12] <- gTime
        }

        herds <<- rbind(herds, tmp)

        ## Distributing the animals on No. days being latent or subclincal or clinical
        latent <<- rbind(latent,
          t(apply(cbind(tmp[,2], herdtypes$latDurFreq[tmp[,17],, drop = FALSE]),
                  1, rpoly2)))

        subclinical <<- rbind(subclinical,
          t(apply(cbind(tmp[,3],herdtypes$SubDurFreq[tmp[,17],, drop = FALSE]),
                  1, rpoly2)))

        clinical <<- rbind(clinical,
          t(apply(cbind(tmp[,4],herdtypes$CliDurFreq[tmp[,17],, drop = FALSE]),
                  1, rpoly2)))

        DeadMat <<- rbind(DeadMat,
          t(sapply(IDs, function(x) rep(0, (DaysDead)))))

        DeadMatSur <<- rbind(DeadMatSur,
          t(sapply(IDs, function(x) rep(0, (DaysSurDead)))))


      }

    },

    delInf = function(IDs, ignoreNotInfected = FALSE) {
      index <- match(IDs, herds[,8])

      if (any(is.na(index))) {
        if (!ignoreNotInfected)
          warning("ConstructAInfHerd: Cannot remove IDs not in aInfHerd!",
                  IDs,index)
        index <- index[!is.na(index)]
      }

      if(length(index)>0){
        delHerds    <<- rbind(delHerds,herds[index,])
        herds       <<- herds[-index,,drop=FALSE]
        latent      <<- latent[-index,,drop=FALSE]
        subclinical <<- subclinical[-index,,drop=FALSE]
        clinical    <<- clinical[-index,,drop=FALSE]
        DeadMat     <<- DeadMat[-index,,drop=FALSE]
        DeadMatSur  <<- DeadMatSur[-index,,drop=FALSE]
      }
    },

    simDay = function() {
      if (nrow(herds) > 0) {
        # Supressing warning: NAs generated due to complete cage mortality (fixed on line 558)
        rS2L <- suppressWarnings(
                rbinom(nrow(herds), herds[,1],
                       1 - exp(-herds[,9] * ((herds[,3]) + (herds[,4])) /
                         (herds[,6] - herds[,5]))))
        
        ### Prevent model crash because of NAs due to complete cage mortality
        rS2L[is.na(rS2L)] <- 0
        
        ## Updating totals
        herds[,1:5] <<- herds[,1:5] + cbind(-rS2L,
                                            rS2L-latent[,1],
                                            latent[,1] - subclinical[,1],
                                            subclinical[,1] - clinical[,1],
                                            clinical[,1])

        ## Distributing new latent infected
        newLat <- apply(cbind(rS2L,
                              herdtypes$latDurFreq[herds[,17],, drop = FALSE]),
                              1, rpoly2)

        ## Distributing new subclinical infected
        newSubC <- apply(cbind(latent[,1],
                               herdtypes$SubDurFreq[herds[,17],, drop = FALSE]),
                               1, rpoly2)

        ## Distributing new clinical infected
        newCli <- apply(cbind(subclinical[,1],
                              herdtypes$CliDurFreq[herds[,17],, drop = FALSE]),
                              1, rpoly2)

        ## Update intra latent and subclinical and clinical waiting states
        latent      <<- cbind(latent[,-1, drop = FALSE], 0) + t(newLat)
        subclinical <<- cbind(subclinical[,-1, drop = FALSE], 0) + t(newSubC)
        DeadMat     <<- cbind(DeadMat, clinical[,1])
        DeadMatSur  <<- cbind(DeadMatSur, clinical[,1])
        DeadMat     <<- DeadMat[,-1, drop = FALSE]
        DeadMatSur  <<- DeadMatSur[,-1, drop = FALSE]
        clinical    <<- cbind(clinical[,-1, drop = FALSE], 0) + t(newCli)

        ## Checking for herds that changed status
        ## status 2 -> 3 : ## status latent to subclinical
        tmpIndex <- herds[,7] == 2 & herds[,3] >= 1

        if (sum(tmpIndex) > 0) {
          herds[tmpIndex, 7]               <<- 3
          aHerd$status[herds[tmpIndex, 8]] <<- 3
        }

        ## status 3 -> 4 : # status subclinical to clinical
        tmpIndex <- herds[,7] == 3 & herds[,4] >= 1

        if (sum(tmpIndex) > 0) {
          herds[tmpIndex, 7]               <<- 4
          herds[tmpIndex, 12]              <<- gTime
          aHerd$status[herds[tmpIndex, 8]] <<- 4
        }

#        ## status 4 -> 7 : status clinical to recovered
#        tmpIndex <- (herds[,7] == 4 & herds[,5] == herds[,6]) |
#                    (herds[,7] == 4 &
#                     herds[,2] == 0 &
#                     herds[,3] == 0 &
#                     herds[,4] == 0 &
#                     herds[,5] >  0 &
#                     rowSums(DeadMat) == 0)
#        if (sum(tmpIndex) > 0) {
#          herds[tmpIndex,7]                   <<- 7
#          aHerd$status[herds[tmpIndex,8]]     <<- 7
#          aHerd$immuneTime[herds[tmpIndex,8]] <<- gTime
#        }
#        ## status 7 -> 1 : status immune to susceptible
#        tmpIndex <- herds[,7] == 7 & herds[,5] < herds[,6]
#       if (sum(tmpIndex) > 0) {
#          herds[tmpIndex, 7]                <<- 1
#          aHerd$status[herds[tmpIndex,8]]   <<- 1
#          aHerd$SusAgain[herds[tmpIndex,8]] <<- gTime
#          aInfHerd$delInf(herds[tmpIndex,8])
#
#          ## Update the indexes of infectious herds
#          tmpTagged <- which(aHerd$status == 6 &
#                             aHerd$timeInfected < Inf &
#                             aHerd$SusAgain == 0)
#          infHerdNums <<- unique(c(tmpTagged,
#                                   (aInfHerd$getIDs())[aInfHerd$getstatus() %in% 3:4]))
#        }

        ## update the number sick and dead animals
        aHerd$Mortality[herds[,8]] <<- (herds[,4] + (herds[,5] * PerDeadAnim))
        aHerd$Survived[herds[,8]]  <<- (herds[,5] * (1 - PerDeadAnim))

        ### update herd size in the aHerd Matrix (TH + DP)
        aHerd$CageSizeVar[herds[,8]]  <<- herds[,6] - (herds[,5] * PerDeadAnim)

        ### Update Cage infectiousness (TH + DP)
        aHerd$Infectiousness[herds[,8]] <<- (herds[,4] + herds[,3]) /
                                            (herds[,6] - (herds[,5] * PerDeadAnim))
        
      #print(herds)

      }
    },

    # Get the dead animals animals during last week and sick animals today for the infected herds
    getDead = function(IDs) {
      index <- match(IDs, herds[,8])

      if (any(is.na(index))) {
        warning("ConstructAInfHerd: Some IDs in zones are not in aInfHerd")
        index <- index[!is.na(index)]
      }

      tmp <- round(rowSums(DeadMatSur)[index] * PerDeadAnim)
      return(tmp)
    },

    getInfected = function(IDs) {
      index<-match(IDs,herds[,8])

      if (any(is.na(index))) {
        #warning("ConstructAInfHerd: Some IDs in not in aInfHerd!")
        index <- index[!is.na(index)]
      }

      tmp <-round(herds[index,3] +
                  herds[index,4] +
                  ##Include infected animals that survived death
                  (herds[index,5] * (1 - PerDeadAnim)))

      return(tmp)
    },

    wipe = function() {
      delHerds             <<- matrix(numeric(0), ncol = nColHerds)
      herds                <<- matrix(numeric(0), ncol = nColHerds)
      dimnames(herds)[[2]] <<- herdsCol
      latent               <<- matrix(numeric(0), ncol = ncol(herdtypes$latDurFreq))
      subclinical          <<- matrix(numeric(0), ncol = ncol(herdtypes$SubDurFreq))
      clinical             <<- matrix(numeric(0), ncol = ncol(herdtypes$CliDurFreq))
      DeadMat              <<- matrix(numeric(0), ncol = DaysDead)
      DeadMatSur           <<- matrix(numeric(0), ncol = DaysSurDead)
    }
  )
}
