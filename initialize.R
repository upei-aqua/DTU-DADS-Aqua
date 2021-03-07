### INITIALIZE library V. 0.15.1
### Includes the following functions:
###
### selectIndexHerd
### createASFvars
### initializeASFvars

######################
### selectIndexHerd
######################
## The options are used to select rows in aHerd.
## This function is called in the beginning of each simulated outbreak
## See help file in package for further details.
selectIndexHerd <- function(args = NULL,...) {

  if (is.null(args)) args <- list(...)

  ## Allowing more than one index case in each outbreak:
  if (nTmp <- match("nIndex", names(args), nomatch = 0)) {
    nIndex <- args[[nTmp]]
    ## removing "nIndex" from the list of arguments
    args   <- args[-nTmp]
  } else {
    nIndex <- 1
  }

  ## Matching the remaining arguments:
  if (length(args) > 0) {
    indexAHerd <- match(names(args), names(aHerd))

    if (any(is.na(indexAHerd))) {
      stop(paste("The argument(s):",
                 paste(names(args)[is.na(indexAHerd)],
                       collapse = " and "),
                 "was/were not recognized.\n"))
    }
  }

  tmp <- args[[1]]
  tmp[iteration]
}


######################
### selectIndexHerd 2
######################
## Start the infection in more than 1 index herds
selectIndHerdMore <- function(args = NULL) {

#### args should be a data frame containing the index herds
#### Each row in args includes the multiple index herds (History herds)
#### that will be used to initiate the epidemic
    tmp <- args
    tmp <- tmp[iteration,]
    tmp <- unlist(unname(tmp))
    tmp <- tmp[!is.na(tmp)]
}


##################################################################
### createASFvars
###
### This funtion is called once to read data and initialize variables
##################################################################
createASFvars <- function() {

  ## Load Data
  if (verbose) cat("Loading Data\n")

  ## read in herd information from comma delimited infofile
  eval.parent(expression(aHerd <- NULL))
  aHerd <<- as.list(read.table(infofile,
                               sep = ";",
                               dec = ".",
                               header = TRUE))

  ## Making sure the ID column is named "ID"
  names(aHerd)[1] <<- "ID"

  ## Check aHerd for non-unique IDs
  if (length(unique(aHerd$ID)) < length(aHerd$ID)) {
    stop("Herd ID numbers are not unique. Simulations Fails",
         paste(unique(aHerd$ID[duplicated(aHerd$ID)]),collapse=", "))
  }

  ## read in herd types from comma delimited typesfile
  eval.parent(expression(herdtypes <- NULL))
  herdtypes <<- as.list(read.table(typesfile,sep=";",as.is=TRUE,header=TRUE))

  ## modify one column of herdtypes table if tornado plot option is selected
  if (!is.null(tornCol)) {

    if (is.numeric(herdtypes[[tornCol]])) {
      herdtypes[[tornCol]] <<- 0.9 * herdtypes[[tornCol]]
    } else {
      herdtypes[[tornCol]] <<- paste(as.character(tornMult),
                                     "*",herdtypes[[tornCol]])
    }
  }

  ## Parsing 'herdtypes' once and for all:
  for (i in 3:length(herdtypes)) {
    herdtypes[[i]] <<- parse(text = herdtypes[[i]])
  }

  ### Read the movement and category matrises for All and for weaners
  eval.parent(expression(DistMat <- NULL))
  DistMat <<- as.matrix(read.table(fileDistMat,
                                   sep = ";",
                                   dec = ","))


  ## Further transformation of latent and subclinical duration frequencies
  eval_x <- function(x) if (is.character(x)) eval(parse(text = x)) else eval(x)

  herdtypes$latDurFreq <<- t(sapply(herdtypes$latDurFreq, eval_x))
  herdtypes$SubDurFreq <<- t(sapply(herdtypes$SubDurFreq, eval_x))
  herdtypes$CliDurFreq <<- t(sapply(herdtypes$CliDurFreq, eval_x))

  ## Declaring some variables in the parent scope.(Accessible to all functions)
  eval.parent(expression(outbreakDetectedLast <- outbreakDetected <- FALSE))
  eval.parent(expression(depopQueue   <- NULL))
  eval.parent(expression(beingDepoped <- NULL))
  eval.parent(expression(indexHerd    <- NA))
  eval.parent(expression(iteration    <- NULL))
  eval.parent(expression(infHerdNums  <- NULL))
  eval.parent(expression(gTime        <- 0))
  eval.parent(expression(initHideMap  <- hideMap))

  if(is.null(initHideMap)) eval.parent(expression(hideMap <- T))

  eval.parent(expression(gMaxHerds <- length(aHerd$herdType)))
  aHerd$Sus <<- rep(NA, gMaxHerds)
  aHerd$K   <<- rep(NA, gMaxHerds)
  aHerd$cullEligible <<- rep(T, gMaxHerds)

  if (!identical(cullTypes,"all")) {
    aHerd$cullEligible <<- aHerd$herdType %in% cullTypes
  }

  ## choose functions
  eval.parent(expression(
      RFtrans <- switch(RFstoch + 1,
                        function(z,nn,pp){ nn*pp }, # if False
                        rbinom                      # if True
                        )
  ))

  eval.parent(expression(
      Tinfs <- switch(Tstoch + 1,
                      function(contactHerds,dProbInfection,Sus) {
                        rbinom(length(contactHerds),1,dProbInfection)
                      },
                      function(contactHerds,dProbInfection,Sus) {
                        rbinom(length(contactHerds),aHerd$Sus[contactHerds],
                               1 - (1 - dProbInfection) ^ (1 / Sus[contactHerds]))
                      })
  ))

  ## Set initial herd status to 1 for every herd unless values taken from file
  if (ignoreStatus) {
    eval.parent(expression(initStatus       <- rep(1, gMaxHerds)))
    eval.parent(expression(initTimeInfected <- rep(Inf, gMaxHerds)))
  } else {
    eval.parent(expression(initStatus       <- aHerd$status))
    eval.parent(expression(initTimeInfected <- aHerd$timeInfected))
  }


  eval.parent(expression(AllInfCages <- NULL))
  AllInfCages <<- matrix(numeric(0), ncol = 8)
  NAMEInf      <- paste(runID,"AllInfCages.txt", sep = "-")
  write.table(AllInfCages, NAMEInf, sep = " ")

  eval.parent(expression(SumResOut  <- NULL))
  SumResOut  <<- matrix(numeric(0), ncol = 10)
  NAME <- paste(runID, "ISA.txt", sep = "-")
  write.table(SumResOut, NAME, sep = " ")

  ######################################################################
  ### initiate the Matrix that includes surveyed herds (TH) Oct 2015 ###
  ######################################################################
  if(Detailed){

    eval.parent(expression(SurvMatOut   <- NULL))
    eval.parent(expression(DepopMatOut  <- NULL))
    eval.parent(expression(PreEmpMatOut <- NULL))

    SurvMatOut   <<- matrix(numeric(0), ncol = 3)
    DepopMatOut  <<- matrix(numeric(0), ncol = 3)
    PreEmpMatOut <<- matrix(numeric(0), ncol = 3)

    NAMESH <- paste(runID,"SurvayedHerds.txt", sep = "-")
    NAMED  <- paste(runID,"DepopHerds.txt",    sep = "-")
    NAMEPE <- paste(runID,"PreEmpHerds.txt",   sep = "-")

    write.table(SurvMatOut,  NAMESH, col.names = FALSE, row.names = FALSE)
    write.table(DepopMatOut, NAMED,  col.names = FALSE, row.names = FALSE)
    write.table(PreEmpMatOut,NAMEPE, col.names = FALSE, row.names = FALSE)
  }
}

##################################################################
### initializeASFvars
###
### This function is called at the beginning of each simulated epidemic
##################################################################
initializeASFvars <- function() {

  if (verbose) cat("iteration",iteration,"\n")

  ## If seed is negative the seed is set at the beginning of each iteration:
  if (!is.null(seed)) {
    if(seed < 0) set.seed(iteration + abs(seed))
  }

  ## Determine the day the first detection will happen
  gDaysUntilBaseline <<- 0

  ## output variable to calculate the number of herds queuing for surveillance per day. does not affect anything (TH)
  TotNumQueue <<- 0

  ## Initialize waiting periods and transmission probabilities
  aHerd$taggedDur <<-rep(0, gMaxHerds)

  ## New additions TH ###

  aHerd$DetTaggedDep   <<- rep(FALSE, gMaxHerds)
  aHerd$timeVisited    <<- rep(0, gMaxHerds)
  aHerd$immuneTime     <<- rep(0, gMaxHerds)
  aHerd$timeCulled     <<- rep(0, gMaxHerds)
  aHerd$ToSurvDead     <<- rep(0, gMaxHerds)
  aHerd$DeadSampTest   <<- rep(0, gMaxHerds)
  aHerd$SampledDead    <<- rep(0, gMaxHerds)
  aHerd$DiagSurvDead   <<- rep(FALSE, gMaxHerds)
  aHerd$SubDeadSamp    <<- rep(0, gMaxHerds)
  aHerd$SusAgain       <<- rep(0, gMaxHerds)
  aHerd$Survived       <<- rep(0, gMaxHerds)
  aHerd$inSurZone      <<- rep(FALSE, gMaxHerds)

  ## New Variables TA and DP
  aHerd$Infectiousness <<- rep(0, gMaxHerds)
  aHerd$infMode        <<- rep(0, gMaxHerds)
  aHerd$CageSizeVar    <<- aHerd$CageSize
  aHerd$CageSizeCull   <<- aHerd$CageSize
  aHerd$DiagMode       <<- rep(0, gMaxHerds)
  aHerd$timeToFSV      <<- rep(0, gMaxHerds)
  aHerd$timeToSSV      <<- rep(0, gMaxHerds)
  aHerd$timeToASV      <<- rep(0, gMaxHerds)
  aHerd$FarmDiag       <<- rep(FALSE, gMaxHerds)
  # number of samples for surveillance- to be checked by DP
  aHerd$NumSamp        <<- round(runif(gMaxHerds, 3, 30))
  aHerd$NumSamSurv     <<- rep(0, gMaxHerds)
  aHerd$herdSizeSurv   <<- rep(0, gMaxHerds)
  aHerd$visitCount     <<- rep(0, gMaxHerds)

  FarmASVisit           <- round(runif(unique(aHerd$FarmID),1,90))
  aHerd$timeToASV      <<- FarmASVisit[aHerd$FarmID]

  ## This part works like list; it evaluates the text entries from the input
  ## table, whether those entries are numeric or R functions (like random
  ## variate selection), in order to set the waiting periods and transmission
  ## probabilities by herd type.
  for (i in herdtypes$herdTypeID) {
    herdIndex <- aHerd$herdType == i
    nType     <- sum(herdIndex)
    typeIndex <- herdtypes$herdTypeID == i
    ## Intra-herd interaction rate
    aHerd$K[herdIndex] <<- eval(herdtypes$K[typeIndex],list(n = nType))
    aHerd$RelSusceptibility[herdIndex] <<-
      eval(herdtypes$RelSusceptibility[typeIndex], list(n = nType))

  }

  ## Making sure that both 'Sus' and 'CageSize' are in 'aHerd'
  ##LEC: This should be changed so that only one name is used!
  if ("CageSize" %in% names(aHerd)) {
    aHerd$Sus <<- aHerd$CageSize
  } else {
    aHerd$CageSize <<- aHerd$Sus
  }


  ## Making sure that the above distributed numbers make sense in the model:
  aHerd$Sus[aHerd$Sus < 2]             <<- 2
  aHerd$K[aHerd$K < 0]                 <<- 0
  aHerd$taggedDur[aHerd$taggedDur < 0] <<- 0

  aHerd$p                              <<- aHerd$K # Reed-Frost probability
  aHerd$p[aHerd$p > 1]                 <<- 1

  ## Initializing infection methods
  for (i in 1:length(newInfMethods)) newInfMethods[[i]]$init()

  ## Initializing control measures
  for (i in 1:length(controlMethods)) controlMethods[[i]]$init()

  #### Reset Values of other parameters
  gTime <<- 0

  aHerd$timeToTaggedForDepop <<- rep(Inf, gMaxHerds)
  aHerd$diagnosisTime        <<- rep(Inf, gMaxHerds)
  aHerd$Diagnosed            <<- rep(F, gMaxHerds)
  aHerd$DiagSurv             <<- rep(F, gMaxHerds)

  outbreakDetectedLast <<- outbreakDetected <<- FALSE

  depopQueue   <<- matrix(numeric(0), ncol = 2)
  beingDepoped <<- matrix(numeric(0), ncol = 2)
  zoneQueue    <<- matrix(numeric(0), ncol = 2)

  aHerd$timeToRemoveZone<<-rep(Inf,gMaxHerds)


  #### set initial values for index herd(s)

  ## SelectindexHerd(s) for next simulation
  aHerd$timeInfected <<- initTimeInfected
  aHerd$status       <<- initStatus
  

  if (ignoreStatus) {

    if(is.null(stepInFile)){

      indexHerd <<- indexHerdFunction(args = indexHerdSelect)
      aHerd$status[indexHerd]       <<- 2 + indexDirect
      aHerd$timeInfected[indexHerd] <<- 0

      if (indexDirect) {
        aInfHerd$addInf(indexHerd,
                        matrix(c(0,1,0),
                               byrow = TRUE,
                               ncol  = 3,
                               nrow  =length(indexHerd)),
                        1)
      } else {

        aInfHerd$addInf(indexHerd,
                        matrix(c(1,0,0),
                               byrow = TRUE,
                               ncol  = 3,
                               nrow  = length(indexHerd)),
                        0)
      }

    } else { ## Using stepInFile

      tmp <- read.table(stepInFile, sep = ",", header = TRUE)
      names(tmp)[1] <- "ID" #First column must be unique ID
      aHerdIndex    <- match(tmp$ID, aHerd$ID)

      assign("stepIn", cbind(aHerdIndex,tmp), envir=parent.env(environment()))

      if (any(is.na(aHerdIndex))) {
        stop("stepInFile: ",
             stepInFile,
             " contains IDs not in herdfile. \n Missing IDs are:",
             paste(stepIn$ID[is.na(aHerdIndex)],collapse=", "))
      }

      indexHerd <- aHerdIndex[stepIn$infectionDay == min(stepIn$infectionDay)]
      gDaysUntilBaseline <<- min(stepIn$diagnosedDay)

      if ("depopedDay" %in% names(stepIn)) {
        traced <- stepIn$diagnosedDay >= stepIn$depopedDay
        ## "diagnosing" the herds time to diagnosis before depop:
        stepIn$diagnosedDay <<- stepIn$depopedDay - aHerd$timeToDiagnosis[stepIn$aHerdIndex]
      }

      ##DK for (day in min(stepIn$infectionDay):(gDaysUntilBaseline-1)){
      for (day in min(stepIn$infectionDay):max(stepIn$infectionDay)) {
        gTime  <<- day
        newInf  <- which(stepIn$infectionDay == day)

        if(length(newInf) > 0){## Add these

          for (j in 1:length(newInf)){ ## One at a time

            if(stepIn$infType[newInf[j]] == 1){ ## Direct => subClin
              aInfHerd$addInf(aHerdIndex[newInf[j]], cbind(0,1,0), 1)
              aHerd$status[aHerdIndex[newInf[j]]] <<- 3      # subclin infection status
              ##DK aInfHerd$addInf(aHerdIndex[ newInf[j] ],cbind(1,0,0),1)
              ##DK Ups: the scenario dictates that the moved animal is latent

            } else {
              aInfHerd$addInf(aHerdIndex[ newInf[j] ],cbind(1,0,0),0)
              aHerd$status[aHerdIndex[newInf[j]]] <<- 2      # latent infection status
            }
          }
        }## Added newInfected herds

        aInfHerd$simDay()
      }

      aHerd$timeInfected[stepIn$aHerdIndex] <<- stepIn$infectionDay
      aHerd$infSource[stepIn$aHerdIndex]    <<- stepIn$infectionSource

      ### Time line!!!
      ### Set Time Diagnosed as to make sure depop happens as it should!!!
    }
  } else { ## What to do for infected herds given their time of diagnosis

    print("nothing was done given time of diagnosis")
  }

  vaccToday <<- 0
  Lock      <<- FALSE

  if(is.null(initHideMap))
    hideMap <<- TRUE

  BMAtmp <- round(runif(unique(aHerd$BMAID),120, 730))
  aHerd$BMATime       <<- rep(0, gMaxHerds)
  aHerd$FarmProdTime  <<- rep(0, gMaxHerds)
  aHerd$FarmActive    <<- rep(FALSE, gMaxHerds)
  aHerd$BMATime       <<- BMAtmp[aHerd$BMAID]
  aHerd$FarmProdTime  <<- aHerd$BMATime - round(runif(length(aHerd$BMATime),0, 120))
  ## force the farm of the index herd to be active
  IndexHerdFarmID <- aHerd$FarmID[indexHerd]
  aHerd$FarmProdTime[aHerd$FarmID%in%IndexHerdFarmID]  <<- round(runif(sum(aHerd$FarmID%in%IndexHerdFarmID),0, 120))
  aHerd$FarmProdTime[indexHerd] <<- 1

}

