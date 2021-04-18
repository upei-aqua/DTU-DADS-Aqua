#### Creating list of options for ASF version 0.20
#### Includes the following functions:
####
#### ASFoptions
####
#### The output is a list of parameters.

ASFoptions <- function(...) {

  args <- list(...)

  ## List of default values:
  defaults <- list(

    # ----Input File names------------------------------------------------------

    # File with herd locations and type
    infofile = "FarmFile.csv",
    # Definitions of type parameters
    typesfile = "typesfile.csv",
    # File used for additional output
    runfile = "",
    # Distance matrix in meters
    fileDistMat = "DistMat.csv",
    # File name or FALSE to use runID.
    chroniclefile = FALSE,

    # ----Aqua-specific options (TH + DP)---------------------------------------

    # Scaling value of distances between farms
    ScalingInf = 0.42,
    # Active BMA probability
    ProbActiveBMA = 0.8,
    # Between cages spread within a farm (transmission rate) - to be checked
    BetCageProb = 0.052,
    # Whether or not to call the whole farm if one cage is detected
    CULLALL = FALSE,
    # time between 2 surveillance visits for herds within a surveillance zone
    ZSurVisit = 30,
    # Number of farms that can be surveyed per day.
    CapSurvay = 2,

    # ----Iteration control-----------------------------------------------------

    # Number of simulated epidemics
    n = 10,
    # ID used for output and temp. files using a random number if NULL
    runID = "test",
    # File used to step into an outbreak and start all iterations from same
    # setup.
    stepInFile = NULL,
    # Maximum length of each outbreak
    maxTime = 365,
    # Seed for random number generator. If negative the seed is found as:
    # set.seed(iteration + abs(seed))
    seed = -10,
    # Logical for ingoring disease status information in input file (all reset
    # to 1 if TRUE)
    ignoreStatus = TRUE,
    # Make verbose output while running
    verbose = FALSE,
    # Name of function used for #  summaries. It is called with arguments:
    # "init", "day", "iter", and "final"
    summaryFunction = "sumTh",

    # ----Spread control--------------------------------------------------------

    # Vector of functions used to make new infections (including parameters).
    newInfFunctions = c("BCSpread()", "DBinf()"),
    # Number of infected animals in newly infected herd: stochastic (T) or
    # exactly 1 (F)
    Tstoch = FALSE,
    # Number of intraherd disease transmissions: binomial chain model (T) or
    # Reed-Frost model (F)
    RFstoch = FALSE,
    # Name of function used to select index herd for each simulated epidemic
    # (iteration)
    indexHerdFunction = "selectIndexHerd",
    # Argument to above function
    indexHerdSelect = list(herdType = 1:18),
    # Index herd infected by direct (T)  or indirect (F) contact
    indexDirect = FALSE,
    # proportion of sick and dead animals
    FirstDetPara1 = 0.00255,
    FirstDetPara2 = 0.00255,
    # risk of infection from subclinical animals before clinical signs appeared
    InfPropSub = 0.1,
    # percentage of animals that die following infection
    PerDeadAnim = 1,
    # number of past days to be used to determine infectiousness of leftovers of
    # dead animals
    DaysDead = 2,
    # parameter to address the impact of leftovers on disease spread
    DeadImpact = 1,
    # parameter to address uncertainty of survivability of virus in leftovers
    ImpDeadTime = 1,
    # cutoffs in km for local spread distance probabilities
    LocSpLim = c(0.1,0.5,1,2),
    # a list that includes the probability of infection (distProb) through local
    # spread given the distance in km (distCat) from the infectious herd these
    # probabilities are based on Boklund et al. (2009) for CSF after reduction
    # by 50% to include the lower infectivity of ASF as carried out by Nigsch et
    # al., 2013.
    DistList = list(distCat  = c(1,2,3,4,5),
                    distProb = c(0.1,0.006,0.002,0.000015,0)),
    # Default distributions of distance categories in km
    probList = list(DistCat = c(0,1,3,10,15,seq(20,250,10),300),
                    distcat = c(0,1,3,10,15,seq(20,250,10),300)),

    # ----Surveillance control--------------------------------------------------

    # Vector of functions used for movement controls, tracing and surveillance
    controlFunctions=c( "SurvZone(label='SZ')"),
    # should detailed surveillance output be printed
    Detailed = FALSE,
    # Vector of starting times for time-dependent diagnosis delays
    delaySteps = 1,
    # Vector of diagnosis delays for starting times in delaySteps. These should
    # be expressions or text that can be parsed to expressions
    delayTimes = expression(round(rpert(n,1,2,4))),
    # Tracing will go back to the defined number of days; default 30 days
    TracePeriod = 30,
    # How often the visit within the surveillance zone be repeated
    RepVisSurvZone = 14,
    # Should traceback information be saved
    traceBack = FALSE,
    traceBackDelay = 1,
    # Abattoir rate before detection first infected herd for swine when there is
    # a movement from swine herds to slaughter
    rateBefD = 1,
    # Abattoir rate after detection first infected herd when there is a movement
    # from the herds to slaughter (less frequent visits despite of a higher
    # value for rate because the Exp function will compress it more ;-))
    rateAftD = 2,
    # The probability of diagnosing a selected herd for diagnosis
    ProbSelDiag = 1,
    # Number of data lines that can be reached before data about survyed herds
    # can be dumpped in the output file
    DumpData = 1,
    # proportion of herds that will be tested (PCR) during first protection zone
    # visit
    ProbSelPV1 = 0,
    # proportion of herds that will be tested (PCR) during first surveillance
    # zone visit
    ProbSelSV1 = 0,
    # proportion of herds that will be tested during second surveillance zone
    # visit
    ProbSelSV2 = 0,
    # proportion of traced herds from indirect contacts that will be tested
    # (PCR) visit
    ProbSelTIDC = 0.1,
    # number of days. herds in overlapping surveillance zones will get a new
    # visit every SecSurVisitOLSZ days.
    SecSurVisitOLSZ = 0,
    # number of days. herds in overlapping protection zone will get a new visit
    # every DelayStartVisitOLPZ once they continue in the protection zone and
    # the time of second PZ visit has passed
    DelayStartVisitOLPZ = 0,
    # surveillance visit.called second here because the first is not mandatory.
    SecSurVisit = 40,
    # second visit in protection zone
    SecProtVisit = 45,
    # Allow first surveillance visit(T/F)
    firstSurvVisit = FALSE,
    # Number of days before the visiting of herds for surveillance would start
    DelayStartVisit = 2,
    # delay for the extra visits for herds in overlapping zones.
    DelayVisit = 30,
    # level of increase in mortality before potential detection.
    MortalityIncrease1 = 2,
    # level of increase in mortality before potential detection.
    MortalityIncrease2 = 2,
    # level of increase in mortality before potential detection.
    MortalityIncreaseZone = 1.5,
    # protection Zones duration should be 50 days at the start of each iteration
    ZoneDuration = 50,
    # The type of visit (1 = PV2, 2 = PV1, 3 = SV1, 4 = SV2, 5 = trace IDC,
    # 6 = Trace DC) where serology testing will be applied.
    SerologyTesting = c(1,5,6),
    # The type of visit (1 = PV2, 2 = PV1, 3 = SV1, 4 = SV2, 5 = trace IDC,
    # 6 = Trace DC) where PCR testing will be applied.
    PCRTesting = c(5,6),
    # Number of dead animals in the herd for first detection
    NumDeadAnimFirst = 5,
    # Number of dead animals in the herds for detection after first detection
    # occured
    NumDeadAnimAftFirst = 1,
    # Number of dead animals in the herd for detection through surveillance.
    NumDeadAnimSurv = 1,
    # Number of past days to be used to survay dead animals
    DaysSurDead = 7,
    # Number of dead animals tested
    numTestDead = 5,
    # Delay on the submited samples to arrive to the laboratory
    DelaySubDeadSamp = 1,
    ToTracedIDC = 2:4,
    # the probability that a movement will not be forgotten and it will be
    # traced and visited,
    probSelectTIDC = c(0,0.838,0.2,0.125),

    # ----Graphics options------------------------------------------------------

    # Time in seconds to pause between new graphs.
    pause = 0,
    # Hide map (T) or show map (F) while running
    hideMap = NULL,
    # Update period (number of iterations) for summary graphs
    itupdate = 10,
    # Typesfile column to reduce for tornado plot (default NULL)
    tornCol = NULL,
    # Multiplier for tornCol column of typesfile
    tornMult = 0.9,
    # Hide (T) or show (F) summary plots, including final risk map
    hidePlots = FALSE,

    # ----Intervention control--------------------------------------------------

    # Vector of functions used to make interventions (including parameters)
    # Format: strings or expressions.
    interventionFunctions = c("DummyInter()"),

    # ----Depopulation control--------------------------------------------------

    # Number of locations that can be depoped at a time. not used anymore
    depopTeams = Inf,
    # The culling capacity per day in number of individuals
    Capacity = c(20000),
    # herd types to be culled
    cullTypes = c(1:18),
    # which days culling should be considered
    CullDays = 1:3650
  )

  # ----Replace defaults with user-specified arguments--------------------------
  if (length(args) > 0) {
    ## Check which values to change
    changes <- match(names(args), names(defaults))

    ## Stop if trying to change nonexisting parameter
    if (any(is.na(changes))){
      stop(paste(paste(names(args)[is.na(changes)], collapse = ", ") ,
                 " is/are not valid parameter names. \n See help file."))
    }

    ## Change the requested values and return
    for (i in 1:length(changes)) {
      defaults[[changes[i]]] <- args[[i]]
    }
  }

  ## Converting strings to functions
  defaults$indexHerdFunction <- match.fun(defaults$indexHerdFunction)
  defaults$summaryFunction   <- match.fun(defaults$summaryFunction)

  if (defaults$verbose) cat("Leaving ASFoptions. ")

  return(defaults)
}
