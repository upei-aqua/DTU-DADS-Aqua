---
Title: "DTU-DADS-Aqua: Introduction to the framework"
Authors: "João F. Romero, Derek Price"
---
# DTU-DADS-Aqua

A simulation framework for modelling waterborne spread of pathogens in aquaculture. The current implementation models ISAV spread at three levels:

- Within a net-pen
- Between net-pens in a farm
- Between farms

The model also simulates ISAV mitigation interventions. An example scenario of a simulated ISAV epidemic is provided, as defined by the parameter values in **Tables 1 and 2**.

## Collaborations and contributions

The authors welcome and invite collaborations and contributions from colleagues working in aquatic animal health, infectious disease modelling, and/or related research projects. If you wish to contribute to the development of the framework or collaborate in a research project, please review the [contribution guidelines for this project](/docs/CONTRIBUTING.md). Any valuable contributions to the framework will be acknowledged in each release version.

## Instructions for model use

The basic recommended use of the framework is described in this section; for further details please refer to the user manual. The DTU-DADS-Aqua framework can be used to directly create and run simulation models by following these steps:
1. [Fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo?platform=windows) and [clone](https://docs.github.com/en/get-started/quickstart/fork-a-repo#cloning-your-forked-repository) the DTU-DADS-Aqua GitHub repository
2. Run the [sourceASF](/sourceASF.R) script
3. Run the `sourceASF` function
4. Define parameter values within [ASFoptions](/ASFoptions.R) script
5. Run the `ASF` function

Alternatively, the framework can be run through a Shiny application:
1. [Fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo?platform=windows) and [clone](https://docs.github.com/en/get-started/quickstart/fork-a-repo#cloning-your-forked-repository) the DTU-DADS-Aqua GitHub repository
2. Run the [control-sim-app](/control-sim-app.R) script to deploy the Shiny app locally
   > [!IMPORTANT]
   > To be able to run the Shiny app the following packages must be installed: "shiny", "shinythemes", "tidyverse", "markdown".
   
3. Define parameter values within the `Example` tab within the app
4. Click `Run Simulation` in the `Simulation control` panel of the app

## Model inputs

**Table 1.** Parameters used in the simulated ISAV mitigation processes.

| Name | Used | Value | Description | Source |
|------|------|-------|-------------|--------|
| ZSurVisit             | Yes | 30       | Time between surveillance visits | - |
| CapSurvay             | Yes | 2        | Number of marine farm sites surveyed per day | - |
| DaysSurDead           | Yes | 7        | Number of past days to be used to survey dead animals | Vike et al., 2014 |
| DelaySubDeadSamp      | Yes | 1        | Days from sampling to lab analysis | - |
| NumSamp               | Yes | U(3, 30) | Number of animals to be randomly sampled in each net-pen | - |
| FirstDetPara1         | Yes | 0.00255  | Mortality threshold for first detection in a net-pen | - |
| FirstDetPara2         | Yes | 0.00255  | Mortality threshold for subsequent detections in a net-pen | - |
| ProbSelDiag           | Yes | 1        | Probability of ISAV detection on a net-pen selected for diagnosis | - |
| MortalityIncrease1    | Yes | 2        | Mortality increase to trigger ISAV detection in net-pens | - |
| MortalityIncrease2    | Yes | 2        | Mortality increase to trigger ISAV detection in net-pens | - |
| CULLALL               | Yes | FALSE    | Cull every net-pen in the farm if one net-pen is detected | Chang et al., 2014 |
| Capacity              | Yes | 20000    | Number of animals that can be culled per day | - |


**Table 2.** Parameters used in the simulated ISAV transmission processes.

| Name | Used | Value | Description | Source |
|------|------|-------|-------------|--------|
| ScalingInf            | Yes | 0.42             | Scaling paramenter for distance-based between-farm transmission of pathogens | Aldrin et al., 2010 |
| BetCageProb           | Yes | 0.052            | Infectiousness for between net-pen transmission of pathogens within a farm | Mardones et al., 2013 |
| ProbActiveBMA         | Yes | 0.8              | The probability that a BMA is active | Chang et al., 2014 |
| newinfanimals         | Yes | PERT(1, 10, 100) | Number of infected animals in a newly infected net-pen | - |
| PerDeadAnim           | Yes | 1                | Percentage of animals that die from infection | - |
| DaysDead              | Yes | 2                | Number of days a dead animal in a net-pen remains infectious | Vike et al., 2014 |

## Model outputs

Results from the simulated scenarios are summarized in 2 text (.txt) files. These are created after each model run. For further details on file naming and interpretation of variables included in the summary files, please refer to the user manual.



### References

Aldrin, M., Storvik, B., Frigessi, A., Viljugrein, H., & Jansen, P.A. (2010). A stochastic model for the assessment of the transmission pathways of heart and skeleton muscle inflammation, pancreas disease and infectious salmon anaemia in marine fish farms in Norway. *Prevent. Veter. Med.* 93 (1), 51–61. https://doi.org/10.1016/j. prevetmed.2009.09.010

Chang, B.D., Coombs, K.A., & Page, F.H. (2014). The Development of the Salmon aquaculture industry in southwestern New Brunswick, bay of Fundy, including steps toward integrated coastal zone management. *Aquac. Econ. Manag.* 18 (1), 1–27. https://doi.org/10.1080/13657305.2014.855952

Mardones, F. O., Jansen, P. A., Valdes-Donoso, P., Jarpa, M., Lyngstad, T. M., Jimenez, D., Carpenter, T. E., & Perez, A. M. (2013). Within-farm spread of infectious salmon anemia virus (ISAV) in Atlantic salmon *Salmo salar* farms in Chile. *Diseases of Aquatic Organisms*, 106(1), 7–16. https://doi.org/10.3354/dao02639

Vike, S., Duesund, H., Andersen, L., & Nylund, A. (2014). Release and survival of infectious salmon anaemia (ISA) virus during decomposition of Atlantic salmon (Salmo salar L.). Aquaculture, 420–421, 119–125.
