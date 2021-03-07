#### CONTROL MEASURES library Version 0.15.1
#### Implementation of control measures including
#### circular  zones, tracing, control of diagnosed herds,
#### national standstill and surveillance of herds (TH)
#############################################################
SurvZone <- function(size1 = 10, size2 = 5, label) {

  SZcenters <- NULL

  list(

    init = function() { ## Initialize next iteration
      SZcenters <<- rep(FALSE, gMaxHerds)
    },

    day = function() { ## Daily update of variables

      ## First: Check if inCirc should be updated
      if (ZoneDuration < maxTime) { ## If centers may be removed.
        tmpCenters <- (aHerd$Diagnosed &
                       !(gTime > (aHerd$timeToRemoveZone + ZoneDuration)))
      } else {
        tmpCenters <- aHerd$Diagnosed
      }

      if(sum(SZcenters != tmpCenters) > 0){ ## Update inCirc if changes
        SZcenters <<- tmpCenters
      }##EndOf if (sum ...)

      aHerd$inSurZone <<- rep(FALSE,gMaxHerds)

      ## Notice the centers are the farm ids. not the cage ids
      Centers <- unique(aHerd$FarmID[SZcenters])

      for (i in 1:length(Centers)) {
        CurCenter <- Centers[i]
        Distance  <- DistMat[CurCenter,]
        IDS       <- unique(aHerd$FarmID)
        indexZone <- switch(max(aHerd$DiagMode[aHerd$FarmID == CurCenter]),
                            IDS[(Distance/1000) <= size1],
                            IDS[(Distance/1000) <= size2])

        aHerd$inSurZone[aHerd$FarmID %in% indexZone] <<- TRUE
      }

      # here only herds within the surveillance zones are processed. herds will
      # get a visit, if they become included in a zone while they were never
      # visited, or when they have been visited earlier, but after inclusion in a
      # surveillance zone following a period they were outside a surveillance
      # zone. herds in overlapping zones will get assigned a new visit (SV2)
      # every SecSurVisitOLSZ number of days (by default 0) as long as they are
      # still in a SZ and have had not been visited within the last DelayVisit
      # number of days (by default 7 days) this means a new visit every
      # DelayVisit days

      surv <- which(aHerd$inSurZone &
                    !aHerd$Diagnosed &
                    ((aHerd$timeToFSV < gTime) & (aHerd$timeToSSV < gTime)))

      if (length(surv) > 0) {
        FarmFVisit <- round(runif(length(unique(aHerd$FarmID)),1,7))
        Index      <- FarmFVisit[aHerd$FarmID[surv]]
        aHerd$timeToFSV[surv] <<- gTime + Index
        aHerd$timeToSSV[surv] <<- aHerd$timeToFSV[surv] + ZSurVisit
      }
    },

    getIn = function() return(inCirc),

    getLabel = function() return(label)
  )
}



