# COVID-19 Analysis for Rocky Mountain West Model

Welcome! This repository serves as a landing page for the methods we used to partition the Rocky Mountain West states (Colorado, Idaho, Montana, New Mexico, Utah, and Wyoming) into sub-state regions using mobility data. In addition, we also developed a method to estimate variant seeds, both historical and current, for the sub-state regions using mobility and visit data.

## Regional Partitioning

We used cell phone data from SafeGraph to construct mobility networks at the multi-county level in the Rocky Mountain West states, passing this data through a clustering algorithm that resolved regions that were as disconnected as possible (i.e., maximizing mobility within and minimizing mobility between). Through this method, we were able to partition out sub-state regions, each with its own independent SEIR-type compartmental model for COVID-19. Each region is assumed to have its own unique set of input data including hospitalizations and vaccinations specific to the region, as well as its own unique set of mobility-informed variant seeds (see below).

To run this partitioning algorithm, run the `regional_partitioning_rmw.R` program. This program splits down the SafeGraph dataset into the six individual states, then runs the partitioning algorithm one state at a time. The algorithm is structured this way to avoid creating regions that straddle state lines.

## Mobility-Informed Variant Seeding

In our model, we propagage outbreaks by "seeding" variants in the population system at a certain rate per day for a certain number of days. Variants with enough growth potential in the model can outcompete and discplace other variants in circulation. We base estimates of infectiousness, immune escape, and virulence on reviews of the emerging literature.

To estimate variant seeding in our model, we use Colorado Variant Sentinel Surveillance to estimate a seed for the whole state of Colorado. We then use the relative levels of outside visitation to all of the Rocky Mountain West regions to scale seeds according to the values calibrated for Colorado. Visit data originates from SafeGraph and includes, for each region, a daily count of mobile devices entering the region from anywhere in the United States (including Alaska, Hawaii, and Washington DC) except the region itself.

We average out the daily visitation over the year 2019 (a pre-pandemic year) to estimate baseline visitation levels for each of the regions and use the ratio of each region's visitation to a reference value (in this case, Colorado North, which receives more visitors on a daily basis than any other region) to allocate variant seeds for each region. We also conduct sensitivity analyses looking at the pandemic years of 2020 and 2021, when visitation was drastically down, as well as seasonal trends within the year 2019, to ensure these patterns were consistent.

To run this variant seeding algorithm, first run the `sampling_frac.R` program, which uses high-resolution mobility data for the state of Colorado to estimate what proportion of total devices are captured in the Safegraph dataset. If you don't want to run this program, the resulting value of the sampling fraction is hard-coded into the `mobility_informed_seeding.R` program. Run that program to generate the estimates for visitation, mobility-informed seeding multipliers, and sensitivity analyses.

