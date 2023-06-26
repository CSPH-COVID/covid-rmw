# Analyses for the Rocky Mountain Covid Data Project

Welcome! This repository serves as a landing page for the analyses performed for the Rocky Mountain Covid Data project, including partitioning the Rocky Mountain West into its sub-state regions, calibrating parameters to existing COVID-19 data, and more.

## Table of Contents

I. Introduction
II. Regional Partitioning
III. Estimating Transmission Rate
IV. Calibrating Hospital Length of Stay
V. Calibrating Mortality
VI. Estimating Therapeutic Use
VII. Age-Weighting Parameters

### I. Introduction

The Rocky Mountain Covid Data project aims to deliver data visualizations that address public health leaders’ current and future needs for COVID-19 in the Rocky Mountain West, comprising Colorado, Idaho, Montana, New Mexico, Utah, and Wyoming. The Rocky Mountain West includes sparsely populated rural expanses, dense urban areas, and destinations that attract tourists from around the country and the world. These characteristics, as well as heterogeneous population demographics and health behaviors, can impact the spread of SARS-CoV-2, leading to important differences in the timing and severity of COVID-19 across the Rocky Mountain West.

It was the diversity of populations and transmissions dynamics across the Rocky Mountain West that inspired us to create a spatially resolved mathematical model for COVID-19 in this part of the United States. Our model has its roots in a previously published model (Buchwald et al. 2021) and features parameters that are tailored specifically towards the population of the Rocky Mountain West and its sub-populations. Many of these parameters were estimated from the literature, but some, such as hospital length of stay and therapeutics use, were possible to estimate from region-specific data sources, available either publicly or via a data use agreement with our partnership organizations. De-identified data such as mobile device counts and therapeutic doses administered, which is available for anyone who visits this repository to use, can be found in the accompanying `Data` folder. Patient-level data is not housed in this repository, though we have posted the analysis code for transparency. 

### II. Regional Partitioning

Our data visualizations and model outputs are designed to help show differences in the timing and magnitude of severe COVID-19 (measured as COVID-19 hospitalizations), as well as vaccination coverage, across the Rocky Mountain West. To do this, we defined 21 sub-state regions that capture the unique demographics and population dynamics in each region. We produce data visualizations and model outputs for each of these 21 regions (3 or 4 to a state), allowing state, local, and tribal public health officials to access crucial information on the past, current, and future landscape of COVID-19.

We aimed to define regions that grouped together populations that are likely to mix with each other, taking into account boundaries for public health decision-making.  We also aimed to reflect some practical constraints when definine regions, such as the need for sufficient population size and the ability for our model to handle computationally intense tasks. We estimated the following criteria to meet these goals:

1. Regions would not cross state lines.
2. Regions would not subdivide counties.
3. Each state would have at least two, but up to four regions.
4. Each region would have a population of at least 100,000.
5. All counties in a region would be geographically adjacent.

Using these criteria and the algorithm, we were able to partition out sub-state regions, each with its own independent SEIR-type compartmental model for COVID-19. Each region is assumed to have its own unique set of input data including hospitalizations and vaccinations specific to the region, as well as its own unique set of variant seeds estimated by a separate optimizer.

Population mobility data were obtained from SafeGraph, a company that aggregates anonymized location data from numerous applications to provide information about places people visit. SafeGraph reports the number of mobile device visits between all census block groups (CBG) by day in the U.S.* We used data from the period from January 01, 2021 to April 16, 2021—a time when mobility was impacted by COVID-19, but to a lesser extent than earlier phases of the pandemic. 

We created mobility informed clusters for each state by taking the daily number of mobile device visits by CBG and aggregating this count up to the county level to determine a daily device count for every origin-destination county pair within each of the six Rocky Mountain West states. This count includes the number of origin county devices that were seen in the destination county, and the number of destination county devices that were seen in the origin county. The higher the device count, the more connected the two counties were deemed to be.We then created a dendrogram for each state, which is a depiction of the counties in a state and their connectedness to each other. We used this information to resolve regional configurations for each state that comprised at least two but no more than four regions, choosing the configuration with the maximum number of regions that conformed to our five criteria.

After generating mobility-informed clusters for each state, we compared these clusters to existing public health regions for each state. If any clusters did not align with these regions, we identified and assessed discordant counties and used a decision-making process to determine whether or not to reallocate those counties to different clusters to better match with public health planning regions.

To run this partitioning algorithm and generate mobility-informed clusters, run the `regional_partitioning_rmw.R` program. This program splits down the SafeGraph dataset into the six individual states, then runs the partitioning algorithm one state at a time.

*To enhance privacy, SafeGraph excludes census block group information if fewer than two devices visited an establishment in a month from a given CBG.

### III. Estimating Transmission Rate

Transmission rate (beta) is the product of transmission-relevant contact and the probability of infection given that contact. It is directly proportional to the basic reproduction number, R0 (R-naught), which is the average number of people that an infected person will infect in a fully susceptible population.

We estimate beta using the initial growth pattern of hospitalizations in the state of Colorado in the very early pandemic. We chose the state of Colorado because we had access to hospitalization data at the time scale necessary to obtain a reliable estimate. We used the first reported cases in Colorado combined with estimated symptom onset and doubling time to extrapolate the start date to January 24, 2020 (Buchwald et al. 2021). While Colorado is only a subset of the Rocky Mountain West, we believe that the estimate we obtained for beta is sufficiently representative of the Rocky Mountain West and its constituent regions.

To obtain a value for beta, we ran a smaller-scale version in R of our current compartmental model (which is housed in Python). The code for this can be found in the `CO model fitting BETA.R` program. The accompanying optimizer in the R model calculates the residual sum of squares (RSS) between observed and model-estimated hospitalizations and aims to minimize that RSS. The value for beta estimated from the model and applied to all regions of the Rocky Mountain West is 0.3329. This serves as the baseline value for the transmission rate for the wildtype variant; other variants deemed to be more transmissible apply a separate multiplier to this baseline value to represent increased transmission potential in the model.

### IV. Calibrating Hospital Length of Stay

Our model fits to total in-hospital patient count (i.e., hospital census). The decision to fit to hospitalizations was made early in the pandemic, as hospitalizations were (and still are) a more stable indicator of community transmission, as opposed to cases, which fluctuate with testing availability and test-seeking behaviors.

The number of model-estimated actively hospitalized individuals (i.e. those that show up in the `Ih` compartment) depends on several parameters, one of them being hospital length of stay (LOS). Therefore, it is important to calibrate the length of stay in the model to what we see in the data to ensure that we generate valid model outputs and projections.In this analysis, we investigated LOS across the three age groups in the model (0-17, 18-64, 65+) and characterized how LOS changed over time across the age groups throughout different phases of the pandemic. We used a large, patient-level dataset that contains all the historical COVID-19 hospitalizations in the state of Colorado through January 31, 2023. We chose the state of Colorado because it was the best data to which we had access, and we believe that LOS estimated for the state of Colorado is sufficiently representative of the Rocky Mountain West and its constituent regions.

Please note that the COVID-19 hospitalization dataset used for this analysis is NOT posted in this public repository.

### V. Calibrating Mortality

In our model, we assume that a small proportion of hospitalized individuals, as well as a small proportion of non-hospitalized, symptomatically infected individuals, pass away and enter the Deceased compartment, where they remain indefinitely and are not replaced. Because the `D` compartment has two points of entry (i.e., from the `I` and `Ih` compartments), we have two accompanying parameters: `dh` (death fraction among hospitalized) and `dnh` (death fraction among non-hospitalized), both split down by age group.

To estimate COVID-19 deaths to date, we used the National Center for Health Statistics (NCHS) mortality dataset, which reports COVID-19 and other deaths by state, month, year, age group, and setting (found here: https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Place-of-Death-and-/4va6-ph5s). If a death count is between 1 and 9, that count is censored (indicated by a blank space). To populate censored values, we used states with complete or near-complete data to obtain an age distribution for deaths. We used this distribution to infer values across censored age groups at the year timescale, then divided yearly deaths evenly among censored months for each age group.

To calculate the in-hospital death fraction, we divided the NCHS imputed deaths by HHS hospitalizations by state, which reports each previous day's COVID-19 hospital admissions by age group (found here: https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh), and obtained an estimate for each age group on the monthly timescale.

To calculate the out-of-hospital death fraction, we divided the NCHS imputed deaths by the CDC COVID-19 case surveillance data, which contained de-identified line list data with all reported COVID-19 cases by age group and state (found here: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4). In the model, the `dnh` parameter represents death fraction among all true symptomatic cases, which surveillance systems will never capture completely. However, the value of this parameter is quite small and fairly inconsequential for the two younger age groups. For 65+, more testing and screening was happening on a regular basis, especially in skilled nursing and long-term care facilities, which was where many of the out-of-hospital deaths in this age group were occurring. Therefore, we believe the CDC case surveillance data to be a sufficiently robust denominator for the purposes of estimating this parameter.

To ensure sufficient sample sizes in each age group (especially 0-17), we opted to calculate a single set of age-specific `dh` and `dnh` parameters for the whole Rocky Mountain West by summing up the numerators and denominators for all six states. Though we do acknowledge that mortality rates may vary slightly by state, we decided that combining counts of deaths, hospitalizations, and cases across states would stabilize our estimates and avoid unnecessary or uninterpretable death fraction values at the state level.

### VI. Estimating Therapeutic Use

Our model has two therapeutics: monoclonal antibodies, and nirmatrelvir-ritonavir (aka Paxlovid). These therapeutics reduce the risk of hospitalization among symptomatically infected in the model. However, if an individual in the model receives treatment and still ends up hospitalized, the risk of death stays the same. In our model, we assume that therapeutics are only given to the 65+ age group--we understand that this is not the case in reality.

To estimate Paxlovid use over time for the Rocky Mountain West, we used the HHS COVID-19 state-specific therapeutics allocation and administration data, which reports cumulative Paxlovid doses administered over time by state (found here: https://aspr.hhs.gov/COVID-19/Therapeutics/Orders/Documents/Forms/AllItems.aspx). HHS began reporting this information on September 04, 2022 and stopped reporting on March 05, 2023. To estimate the number of doses given prior to September 04, 2022, we divided the cumulative doses as of September 04, 2022 over the period spanning December 23, 2021 and September 04, 2022, assuming that 20% of cumulative doses were administered at a constant rate between December 23, 2021 and April 26, 2022 (when Paxlovid was newly authorized and obtaining it was difficult), and 80% of cumulative doses were administered at a constant rate between April 26, 2022 and September 04, 2022 (when Paxlovid was much easier to obtain). We converted the cumulative doses into a daily pattern and divided these doses by the number of model-estimated daily symptomatic infections to calculate the proportion of symptomatic infections that were to receive Paxlovid. 

### VII. Age-Weighting Parameters

Many of the parameters in our model are stratified by age. Where we are unable to analyze region-specific data to calibrate parameters, we obtain values from the literature. However, published literature often includes estimates that are different from the age groups we use in our model. To translate literature-reported values to age-specific values in our model, we population-weight the values from our literature to match the age distribution in the Rocky Mountain West.
