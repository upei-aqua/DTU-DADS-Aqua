# Parameters used in the ISAv Model

| Name | Available | Value | Description | Source |
|------|------|-------|-------------|--------|
| n                     | Yes | 1        | number of iterations | - |
| maxTime               | Yes | 365      | max outbreak length | - |
| seed                  | Yes | NULL     | random seed for iteration? (varies each iteration) | - |
| ScalingInf            | Yes | 0.42     | Scaling paramenter for distance-based between-farm transmission | Aldrin et al, 2010 |
| BetaCageProb          | Yes | 0.052    | infectiousness for between-cage transmission | Mardones et al, 2013 |
| Capacity              | Yes | c(25000) | Number of animals that can be culled per day | - |
| ProbActiveBMA         | No | 0.8      | the probability that a BMA is active | - |
| CULLALL               | No | FALSE    | cull the whole farm if a cage is detected | - |
| ZSurVisit             | No | 30       | time between surveillance visits | - |
| CapSurvay             | No | 2        | number of farms surveyed per day | - |
| ignoreStatus          | No | TRUE     | ignore disease status from input file | - |
| summaryFunction       | No | "sumTh"  | name of summary function? | - |
| newInfFunctions       | No | c("BCSpread()", "DBinf()") | Custom spread functions | - |
| Tstoch                | No | FALSE    | Number of infected stochastic? Otherwise, exactly one | - |
| RFstoch               | No | FALSE    | Intraherd spread using binomial chain model? Otherwise, use Reed-Frost | - |
| indexHerdFunction     | No | "selectIndexHerd" | Index herd selection function | - |
| indexHerdSelect       | No | list(herdType 1:18) | Aguments to preceding function | - |
| indexDirect           | No | FALSE    | Is index herd infected through direct contact? Otherwise, indirect | - |
| FirstDetPara1         | No | 0.00255  | Mortality threshold for first detection in a farm | - |
| FirstDetPara2         | No | 0.00255  | Mortality threshold for subsequent detections in a farm | - |
| PerDeadAnim           | No | 1        | Percentage of animals that die from infection | - |
| DaysDead              | No | 2        | How many days should dead fish stay infectious | - |
| Detailed              | No | FALSE    | Display additional output | - |
| ProbSelDiag           | No | 1        | Probability of detection on a cage selected for diagnosis | - |
| DumpData              | No | 1        | Number of lines held in memory only before dumping data in a file | - |
| MortalityIncrease1    | No | 2        | Times the normal mortality has to increase to trigger disease detection | - |
| MortalityIncrease2    | No | 2        | Times the normal mortality has to increase to trigger disease detection | - |
| ZoneDuration          | No | 50       | Duration of protection zones at the start of each iteration | - |
| DaysSurDead           | No | 7        | Number of past days to be used to survay dead animals | - |
| DelaySubDeadSamp      | No | 1        | Days from sampling to lab analysis | - |
| cullTypes             | No | c(1:18)  | Herd types to be culled | - |
