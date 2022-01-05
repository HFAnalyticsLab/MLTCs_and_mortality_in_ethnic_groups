# Examining multiple long-term conditions and mortality in diverse ethnic groups

#### Project Status: [Completed]

## Project Description

The prevalence of multiple long-term conditions is higher in some minoritised ethnic groups. But evidence on whether ethnicity affects survival once multiple conditions are established is scarce and based on US data. This study will quantify the association between multiple conditions and mortality risk and test ethnicity as an effect modifier. This information could be used to identify groups with multiple conditions that have higher mortality risk and may benefit from targeted care. The coronavirus pandemic has made this issue even more pressing. People with multiple conditions are high users of health care and some may have been especially impacted by disruptions to health care services during the pandemic.

Survival time based on deaths from all causes is the primary outcome for this study. Cox regression models will estimate hazard ratios for multiple condition status, ethnicity and their interaction. Sequential adjustment for deprivation and new conditions acquired during follow-up will be made to examine the contribution of these mediators to explaining differences in survival.

## Data source

We used data from the Clinical Practice Research Datalink (CPRD Aurum) linked to Hospital Episode Statistics, IMD mortality and Indices of Multiple Depriavtion (eRAP 20_000239).

## How does it work?

There are four R scripts that process the CPRD sample, create variables and then run models to test our hypotheses.

* 01_Process_data.R
*   This creates and analysis dataset from our CPRD sample and clinical codelists.

* 02_Descriptives_for_ethnicity_mortality_ppr.R
* 03_Survival_prepdata.R
* 04_Survival.R
### Requirements

Software or packages that needs to be installed and and how to install them.

For example:
These scripts were written in R version (to be added) and RStudio Version 1.1.383. 
The following R packages (available on CRAN) are needed: 
* [**tidyverse**](https://www.tidyverse.org/)

### Getting started

Describe the way in which the code can be used. 

## Useful references


## Authors

* contributor name - [Twitter] - [GitHub]

## License

This project is licensed under the [MIT License](link to license file).

## Acknowledgments

* Credit anyone whose code was used
* Inspiration
* etc
