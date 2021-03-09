---
title: "The DTU-DADS-Aqua: Introduction to the framework and parameters"
author: "João F. Romero, Derek Price"
output: pdf_document
---
# DTU-DADS-Aqua

A simulation framework for modelling waterborne spread of pathogens in aquaculture. The current implementation models ISAv spread at three levels:

- Within a net-pen
- Between net-pens in a farm
- Between farms

Table 1. Parameters used in the ISAv model control processes.

| Name | Used | Value | Description | Source |
|------|------|-------|-------------|--------|
| ZSurVisit             | Yes | 30       | Time between surveillance visits | - |
| CapSurvay             | Yes | 2        | Number of marine farm sites surveyed per day | - |
| DaysSurDead           | Yes | 7        | Number of past days to be used to survey dead animals | Vike et al., 2014 |
| DelaySubDeadSamp      | Yes | 1        | Days from sampling to lab analysis | - |
| NumSamp               | Yes | U(3, 30) | Number of animals to be randomly sampled in each net-pen | - |
| FirstDetPara1         | Yes | 0.00255  | Mortality threshold for first detection in a net-pen | - |
| FirstDetPara2         | Yes | 0.00255  | Mortality threshold for subsequent detections in a net-pen | - |
| ProbSelDiag           | Yes | 1        | Probability of ISAv detection on a net-pen selected for diagnosis | - |
| MortalityIncrease1    | Yes | 2        | Mortality increase to trigger ISAv detection in net-pens | - |
| MortalityIncrease2    | Yes | 2        | Mortality increase to trigger ISAv detection in net-pens | - |
| CULLALL               | Yes | FALSE    | Cull every net-pen in the farm if one net-pen is detected | Chang et al., 2014 |
| Capacity              | Yes | 20000    | Number of animals that can be culled per day | - |


Table 2. Parameters used in the ISAv model transmission processes.

| Name | Used | Value | Description | Source |
|------|------|-------|-------------|--------|
| ScalingInf            | Yes | 0.42             | Scaling paramenter for distance-based between-farm transmission of pathogens | Aldrin et al., 2010 |
| BetCageProb           | Yes | 0.052            | Infectiousness for between net-pen transmission of pathogens within a farm | Mardones et al., 2013 |
| ProbActiveBMA         | Yes | 0.8              | The probability that a BMA is active | Chang et al., 2014 |
| newinfanimals         | Yes | PERT(1, 10, 100) | Number of infected animals in a newly infected net-pen | - |
| PerDeadAnim           | Yes | 1                | Percentage of animals that die from infection | - |
| DaysDead              | Yes | 2                | Number of days a dead animal in a net-pen remains infectious | Vike et al., 2014 |


#### References

Aldrin, M., Storvik, B., Frigessi, A., Viljugrein, H., & Jansen, P. A. (2010). A stochastic model for the assessment of the transmission pathways of heart and skeleton muscle inflammation, pancreas disease and infectious salmon anaemia in marine fish farms in Norway. Preventive Veterinary Medicine, 93(1), 51–61.  
Chang, B. D., Coombs, K. A., & Page, F. H. (2014). The Development of the Salmon Aquaculture Industry in Southwestern New Brunswick, Bay of Fundy, Including Steps Toward Integrated Coastal Zone Management. Aquaculture Economics & Management, 18(1), 1–27.  
Mardones, F. O., Jansen, P. A., Valdes-Donoso, P., Jarpa, M., Lyngstad, T. M., Jimenez, D., Carpenter, T.E., & Perez, A. M. (2013). Within-farm spread of infectious salmon anemia virus (ISAV) in Atlantic salmon Salmo salar farms in Chile. Diseases of Aquatic Organisms, 106(1), 7–16.  
Vike, S., Duesund, H., Andersen, L., & Nylund, A. (2014). Release and survival of infectious salmon anaemia (ISA) virus during decomposition of Atlantic salmon (Salmo salar L.). Aquaculture, 420–421, 119–125.