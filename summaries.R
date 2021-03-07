#### summary library version 0.15.1
####
####
####
sumTh <- function(step) {
  switch(step,

    init = {if (verbose) cat("Initializing...\n")
      ## initialize distance calculations
      eval.parent(expression(aHerd$dDist        <- NULL))
      ## global count of herds depopulated
      eval.parent(expression(gDepopulationCount <- rep(0,n)))
      ## global count of vaccinated herds
      eval.parent(expression(gVaccineCount      <- rep(0,n)))
      ## number of times each herd was infected
      eval.parent(expression(gEpiDur            <- rep(NA,n)))
      eval.parent(expression(ObsEpiDur          <- rep(NA,n)))
      eval.parent(expression(NumInf             <- rep(0,n)))
      eval.parent(expression(NumDet             <- rep(0,n)))
      eval.parent(expression(NumCulled          <- rep(0,n)))
      eval.parent(expression(Recovered          <- rep(0,n)))
      eval.parent(expression(NumCullAnim        <- rep(0,n)))
      eval.parent(expression(VisEpi             <- rep(0,n)))
      eval.parent(expression(VisCount           <- rep(NA,n)))
      eval.parent(expression(ZoneDur            <- rep(0,n)))
      eval.parent(expression(FirstEpiDet        <- rep(0,n)))
      eval.parent(expression(diagHerdsSurv      <- rep(0,n)))
      eval.parent(expression(SurvCosDead        <- rep(0,n)))
      eval.parent(expression(NumSurvDeadAnim    <- rep(0,n)))
      eval.parent(expression(NumSurvDeadHerd    <- rep(0,n)))
      eval.parent(expression(DetFromSurvDead    <- rep(0,n)))
      eval.parent(expression(NumFarmInf         <- rep(0,n)))
      eval.parent(expression(LastInfCage        <- rep(0,n)))
    },

    day = {
    ## Nothing to see here
    },

    iter = {
      if(verbose) cat("\nRecording iteration Values\n")
      gEpiDur[iteration]     <<- gTime - gDaysUntilBaseline
      NumDet[iteration]      <<- sum(aHerd$Diagnosed) # detected on cage level
      NumInf[iteration]      <<- sum(aHerd$Diagnosed |
                                     aHerd$SusAgain > 0 |
                                     aHerd$infMode  > 0 |
                                     aHerd$status == 7)
      NumCulled[iteration]   <<- sum(aHerd$status %in% c(5,6))
      Recovered[iteration]   <<- sum(aHerd$SusAgain > 0 | aHerd$status == 7)
      NumCullAnim[iteration] <<- sum(aHerd$CageSize[aHerd$status %in% c(5,6)])
      NumFarmInf[iteration]  <<- length(unique(aHerd$FarmID[aHerd$status > 1]))
      LastInfCage[iteration] <<- max(aHerd$timeInfected[aHerd$timeInfected<Inf])


      ### Herds that are in the zones and must be visited after the model run is
      ### finished (last detected herd is culled). Notice that we do not
      ### include the possibility of testing after with PCR for herds assigned
      ### to serology because of suspecion,  as we do in the survHerds
      ### function during the model run, because at this stage we know that
      ### there are no clinical herds.  The outbreak is finished and hence no
      ### suspesion will happen.

      aHerd$visitCount[aHerd$timeToFPV >= gTime] <<- aHerd$visitCount[aHerd$timeToFPV >= gTime] + 1
      aHerd$visitCount[aHerd$timeToSPV >= gTime] <<- aHerd$visitCount[aHerd$timeToSPV >= gTime] + 1
      aHerd$visitCount[aHerd$timeToAPV >= gTime] <<- aHerd$visitCount[aHerd$timeToAPV >= gTime] + 1

      #Count of farm visits
      VisCount[iteration]      <<- sum(tapply(aHerd$visitCount, aHerd$FarmID, max))
      diagHerdsSurv[iteration] <<- sum(aHerd$DiagSurv)
      FirstEpiDet[iteration]   <<- gDaysUntilBaseline

      ### in case of detailed information is demanded, then check the herds that
      ### will still have to be surveyed and att then to the relevant
      ### surveillance output data file
      if (Detailed) {
        SurvVisits<- rep(FALSE,gMaxHerds)
        SurvVisits[aHerd$timeToFPV>=gTime|aHerd$timeToSPV>=gTime] <- TRUE

        if(sum(SurvVisits)>0){
          SurvMatOut  <<- rbind(SurvMatOut,
                                cbind(iteration,
                                      gTime,
                                      which(SurvVisits)))
          ### NAME here will be exactly the same as that in the initialization
          ### file, so no worries; no overwriting will happen ;-) (TH)
          NAMESH <- paste(runID, "SurvayedHerds.txt", sep = "-")
          write.table(SurvMatOut,
                      NAMESH,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE)
          SurvMatOut <<- matrix(numeric(0), ncol = 3)
        }
      }#end of if(Detalied){
      ### Exporting the summary results ###
      SumResOut <<- cbind(FirstEpiDet[iteration],
                          gEpiDur[iteration],
                          LastInfCage[iteration],
                          NumInf[iteration],
                          NumFarmInf[iteration],
                          NumDet[iteration],
                          NumCulled[iteration],
                          Recovered[iteration],
                          NumCullAnim[iteration],
                          VisCount[iteration])
      NAME <- paste(runID,"ISA.txt",sep="-")
      write.table(SumResOut,
                  NAME,
                  append = TRUE,
                  sep = " ",
                  col.names = FALSE,
                  row.names = FALSE)
      SumResOut  <<- matrix(numeric(0), ncol = 10)

      ### Exporting the Infected herds matrix ###
      IndexAIH <- which(aHerd$Diagnosed |
                        aHerd$SusAgain > 0 |
                        aHerd$infMode > 0 |
                        aHerd$status == 7)
      AllInfHerds <<- cbind(iteration,
                            aHerd$diagnosisTime[IndexAIH],
                            aHerd$immuneTime[IndexAIH],
                            IndexAIH,
                            aHerd$timeInfected[IndexAIH],
                            aHerd$infMode[IndexAIH],
                            aHerd$FarmID[IndexAIH],
                            aHerd$FarmProdTime[IndexAIH])
      NAMEInf <- paste(runID, "AllInfCages.txt", sep = "-")
      write.table(AllInfHerds,
                  NAMEInf,
                  append = TRUE,
                  sep = " ",
                  col.names = FALSE,
                  row.names = FALSE)
      AllInfHerds <<- matrix(numeric(0), ncol = 8)

      if (Detailed) {
        NAMED    <- paste(runID, "DepopHerds.txt", sep = "-")
        NAMEPE   <- paste(runID, "PreEmpHerds.txt", sep = "-")
        write.table(DepopMatOut,
                    NAMED,
                    append    = TRUE,
                    col.names = FALSE,
                    row.names = FALSE)
        write.table(PreEmpMatOut,
                    NAMEPE,
                    append    = TRUE,
                    col.names = FALSE,
                    row.names = FALSE)
        DepopMatOut  <<- matrix(numeric(0), ncol = 3)
        PreEmpMatOut <<- matrix(numeric(0), ncol = 3)
      }
      cat(iteration, " ")
    },

    final = {
      return(aHerd)
    },
    ## This will be executed if nothing above matches!
    cat("Step should be either init, day, iter, or final")
  )
} ## End of sumTH


