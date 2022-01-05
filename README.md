# Examining multiple long-term conditions and mortality in diverse ethnic groups

#### Project Status: [Completed]

## Project Description

The prevalence of multiple long-term conditions is higher in some minoritised ethnic groups. But evidence on whether ethnicity affects survival once multiple conditions are established is scarce and based on US data. This study will quantify the association between multiple conditions and mortality risk and test ethnicity as an effect modifier. This information could be used to identify groups with multiple conditions that have higher mortality risk and may benefit from targeted care. The coronavirus pandemic has made this issue even more pressing. People with multiple conditions are high users of health care and some may have been especially impacted by disruptions to health care services during the pandemic.

Survival time based on deaths from all causes is the primary outcome for this study. Cox regression models will estimate hazard ratios for multiple condition status, ethnicity and their interaction. Sequential adjustment for deprivation and new conditions acquired during follow-up will be made to examine the contribution of these mediators to explaining differences in survival.

## Data source

We used data from the Clinical Practice Research Datalink (CPRD Aurum) linked to Hospital Episode Statistics, IMD mortality and Indices of Multiple Depriavtion (eRAP 20_000239).

The codelists we used to derive conditions for patients are from [Anna Head](https://github.com/annalhead) at the University of Liverpool and can be found [here](https://github.com/annalhead/CPRD_multimorbidity_codelists)

## How does it work?

There are four R scripts that process the CPRD sample, create variables and then run models to test our hypotheses.

* 01_Process_data.R - This creates an analysis dataset from our CPRD sample and clinical codelists.
* 02_Descriptives_for_ethnicity_mortality_ppr.R - Derives variables and creates summary statistics and tables
* 03_Survival_prepdata.R - Creates additional variables for modelling
* 04_Survival.R - Model creation and hypothesis testing

### Requirements

These scripts were written in R version 4.0.5 and RStudio Version 1.1.383.

The following R packages (available on CRAN) are needed: 
* [**tidyverse**](https://www.tidyverse.org/)
* [**data.table**](https://cran.r-project.org/web/packages/data.table)
* [**arrow**](https://arrow.apache.org/docs/r/)
* [**tableone**](https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html)
* [**nnet**](https://cran.r-project.org/web/packages/nnet/index.html)
* [**survival**](https://cran.r-project.org/web/packages/survival/index.html)
* [**survminer**](https://cran.r-project.org/web/packages/survminer/index.html)
* [**broom**](https://cran.r-project.org/web/packages/broom/vignettes/broom.html)

In addition these scripts make use of our in house package [**aurumpipeline**](https://github.com/HFAnalyticsLab/aurumpipeline) available here on GitHub.

### Getting started

If you have access to a CPRD Aurum sample then you can run the scripts in order to re-produce this work. You will need to download the codelists described above and find a suitable codelist to derive ethincity, such as the one created at [opensafely](https://www.opencodelists.org/codelist/opensafely/ethnicity/2020-04-27/).

## Authors

Add link to paper [here]

* Mai Stafford, PhD - [Twitter](https://twitter.com/stafford_xm)
* Anne Alarilla - [Twitter](https://twitter.com/alarillaanne)
* Jay Hughes - [GitHub](https://github.com/Jay-ops256)

## License

This project is licensed under the MIT License
