# Analysis of Alcohol Related Mortality

## Data Sources
* The U.S. Centers for Disease Control and Prevention (CDC) Division of Vital Statistics produces yearly [Multiple Cause Mortality Files](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple), which include basic demographic variables such as race<span id="a1">[[1]](#f1)</span> and sex. This analysis uses the latest dataset to date, 2016.

* In addition, [total population estimates](https://wonder.cdc.gov/bridged-race-population.html) are produced by the joint effort of the U.S. Census Bureau and the National Center for Health Statistics (NCHS). These can be used to calculate mortality rates by age group for different populations, and as a standard population for age adjusted comparisons<span id="a2">[[2]](#f2)</span> of the aggregate mortality rates for populations which may vary in their age distributions.

## Methodology
### Load Needed Libraries
~~~r
library(tidyverse)
library(RColorBrewer)
~~~



## Results


## Footnotes
1. <span id="f1"></span>The CDC uses bridged race categories for their mortality data. For more information on race-bridging, [see here](https://www.cdc.gov/nchs/nvss/bridged_race.htm).
2. <span id="f2"></span>Age-adjustment is crucial when tracking how a population's mortality rate changes over time, or when comparing two different populations, as in each case the age distribution of the population may be a significant contributing factor to mean differences. For more information on age-adjustment, [see here](https://www.cdc.gov/nchs/data/statnt/statnt06rv.pdf).