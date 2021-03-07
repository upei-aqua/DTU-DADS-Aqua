# DTU-DADS-Aqua

An implementation of the DTU-DADS disease spread simulation framework for aquaculture. The current implementation models ISAv spread at three levels:

- within a cage
- between cages
- between farms

Table 1. Parameters used in the ISAv Model

| Name | Used | Value | Description | Source |
|------|------|-------|-------------|--------|
| ScalingInf            | Yes | 0.42     | Scaling paramenter for distance-based between-farm transmission | Aldrin et al, 2010 |
| BetaCageProb          | Yes | 0.052    | infectiousness for between-cage transmission | Mardones et al, 2013 |
| ProbActiveBMA         | Yes | 0.8      | the probability that a BMA is active | - |
| CULLALL               | Yes | FALSE    | cull the whole farm if a cage is detected | - |
| ZSurVisit             | Yes | 30       | time between surveillance visits | - |
| CapSurvay             | Yes | 2        | number of farms surveyed per day | - |
| n                     | Yes | 1        | number of iterations | - |
| maxTime               | Yes | 365      | max outbreak length | - |
| seed                  | Yes | NULL     | random seed for iteration? (varies each iteration) | - |
| ignoreStatus          | Yes | TRUE     | ignore disease status from input file | - |
| summaryFunction       | Yes | "sumTh"  | name of summary function? | - |
| newInfFunctions       | Yes | c("BCSpread()", "DBinf()") | Custom spread functions | - |
| Tstoch                | Yes | FALSE    | Number of infected stochastic? Otherwise, exactly one | - |
| RFstoch               | Yes | FALSE    | Intraherd spread using binomial chain model? Otherwise, use Reed-Frost | - |
| indexHerdFunction     | Yes | "selectIndexHerd" | Index herd selection function | - |
| indexHerdSelect       | Yes | list(herdType 1:18) | Aguments to preceding function | - |
| indexDirect           | Yes | FALSE    | Is index herd infected through direct contact? Otherwise, indirect | - |
| FirstDetPara1         | Yes | 0.00255  | Mortality threshold for first detection in a farm | - |
| FirstDetPara2         | Yes | 0.00255  | Mortality threshold for subsequent detections in a farm | - |
| PerDeadAnim           | Yes | 1        | Percentage of animals that die from infection | - |
| DaysDead              | Yes | 2        | How many days should dead fish stay infectious | - |
| Detailed              | Yes | FALSE    | Display additional output | - |
| ProbSelDiag           | Yes | 1        | Probability of detection on a cage selected for diagnosis | - |
| DumpData              | Yes | 1        | Number of lines held in memory only before dumping data in a file | - |
| MortalityIncrease1    | Yes | 2        | Times the normal mortality has to increase to trigger disease detection | - |
| MortalityIncrease2    | Yes | 2        | Times the normal mortality has to increase to trigger disease detection | - |
| ZoneDuration          | Yes | 50       | Duration of protection zones at the start of each iteration | - |
| DaysSurDead           | Yes | 7        | Number of past days to be used to survay dead animals | - |
| DelaySubDeadSamp      | Yes | 1        | Days from sampling to lab analysis | - |
| Capacity              | Yes | c(25000) | Number of animals that can be culled per day | - |
| cullTypes             | Yes | c(1:18)  | Herd types to be culled | - |
| delayTimes            | ?   | expression(round(rpert(n,1,2,4))) | Diagnosis delays | - |
| traceBack             | ?   | FALSE    | Save trace information | - |
| traceBackDelay        | ?   | 1        | Delay for trace back | - |
| pause                 | ?   | 0        | Time in seconds to pause between new graphs | - |
| hideMap               | ?   | NULL     | Hide map while running? | - |
| itupdate              | ?   | 10       | Number of iterations to update graphs | - |
| tornCol               | ?   | NULL     | Typesfile column to reduce for tornado plot | - |
| tornMult              | ?   | 0.9      | Multiplier for tornCol column | - |
| hidePlots             | ?   | FALSE    | Hide summary plots? | - |
| interventionFunctions | ?   | c("DummyInter()") | Intervention functions | - |
| delaySteps            | No  | 1        | Statrting times for time-dependent diagnosis delays | - |
| InfPropSub            | No  | 0.1      | Risk of infection from subclinical animals | - |
| DeadImpact            | No  | 1        | Impact of leftovers from dead fish | - |
| ImpDeadTime           | No  | 1        | Survivability of virus in carcass | - |
| LocSpLim              | No  | c(0.1,0.5,1,2) | cut-offs for local spread | - |
| DistList              | No  | -        | List of probabilitities for dicrete distance-based spread | - |
| probList              | No  | -        | Discrete distance categories | - |
| TracePeriod           | No  | 30       | Days to trace | - |
| RepVisSurvZone        | No  | 14       | Days between visits within surveillance zone | - |
| rateBefD              | No  | 1        | Abattoir rate before detection | - |
| rateAftD              | No  | 2        | Abattoir rate after detection | - |
| ProbSelPV1            | No  | 0        | Proportion of cages tested with PCR during first protection zone visit | - |
| ProbSelSV1            | No  | 0        | Proportion of cages tested with PCR during first surveillance visit | - |
| ProbSelSV2            | No  | 0        | Proportion of cages tested with PCR during second surveillance visit | - |
| ProbSelTIDC           | No  | 0.1      | Proportion of traced herd from indirect contacts tested with PCR | - |
| SecSurVisitOLSZ       | No  | 0        | Deal with overlapping surveillance zones | - |
| DelayStartVisitOLPZ   | No  | 0        | Deal with overlapping protection zones | - |
| SecSurVisit           | No  | 40       | Second surveillance visit | - |
| SecProtVisit          | No  | 45       | Second visit in protection zone | - |
| firstSurvVisit        | No  | FALSE    | Allow first surveillance visit? | - |
| DelayStartVisit       | No  | 2        | Number of days that will pass before visits start in surveillance zone| - |
| DelayVisit            | No  | 30       | Delay for extra visits of herds in surveillance zones | - |
| MortalityIncreaseZone | No  | 1.5      | Times the normal mortality has to increase to trigger disease detection after first detection | - |
| SerologyTesting       | No  | c(1,5,6) | Type(s) of visit where samples will be submitted for serology | - |
| PCRTesting            | No  | c(5,6)   | Type(s) of visit where samples will be submitted for PCR | - |
| NumDeadAnimFirst      | No  | 5        | Number of dead animals in the herd to trigger first detection | - |
| NumDeadAnimAftFirst   | No  | 1        | Number of dead animals in the herd to trigger subsequent detections | - |
| NumDeadAnimSurv       | No  | 1        | Number of dead animals in the herd to trigger detection through surveillance | - |
| numTestDead           | No  | 5        | Number of dead animals tested | - |
| ToTracedIDC           | No  | 2:4      | Trace indirect contacts | - |
| probSelectTIDC        | No  | c(0,0.838,0.2,0.125) | Probability that a movement will not be forgotten and will be queued for visit | - |
| depopTeams            | No  | Inf      | Number of locations that can be depopulated at the same time | - |
| CullDays              | No  | 1:3650   | Which days culling should be considered | - |

?: parsed or defined, but not used AFAIK