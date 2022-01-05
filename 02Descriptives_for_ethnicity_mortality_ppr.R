
#pth <- "R:/R_repository"

#install.packages(c("crayon"),
#                 repos = file.path("file://", pth),
#                 type = "win.binary", dependencies = TRUE)

library(crayon)
library(devtools)
library(plyr)
library(dplyr)
library(tidyverse)
library(haven)
library(arrow)
library(data.table)
library(tableone)


pats_flags1 <- readRDS(str_c(processed_RDS_path, 'Pats_update.rds'))

## derive variables, add labels
pats_flags1 <- pats_flags1 %>% 
  mutate(age_baseline=2015-yob) %>% ## derive age at baseline
  mutate(age_baseline_centredat50=2015-yob-50) %>%
  mutate(age_baseline_centredsq=age_baseline_centredat50*age_baseline_centredat50)
pats_flags1$age_baseline <- cut(pats_flags1$age_baseline, 
                                breaks=c(-Inf, 28, 38, 48, 58, 68, 78, Inf),
                                labels=c("18-29y", "30-39y", "40-49y", "50-59y", "60-69y", "70-79y", "80+y"))  

pats_flags1$gender <- factor(pats_flags1$gender, levels=c(1,2), labels=c("men", "women"))
pats_flags1$region <- factor(pats_flags1$region, labels=c("NE", "NW", "Yorks & Humber", "E Mids", "W Mids",
                                                          "East Engl","SW", "S Central", "London", "SE Coast")) 


#############################################
# Derive additional multimorbidity variables
pats_flags1 <- pats_flags1 %>% 
  mutate(bodysystem1=ifelse(cancer==1, 1, 0)) %>% #cancer
  mutate(bodysystem2=ifelse(atrial_fibrillation==1 | heart_disease==1 | heart_failure==1 | hypertension==1 | stroke==1 | vascular_disease==1, 1, 0)) %>% #circulatory system    mutate(bodysystem3=ifelse(liver_disease==1 | diverticulosis==1 | ibs==1, 1, 0)) %>%
  mutate(bodysystem4=ifelse(hearing_loss==1, 1, 0)) %>% #diseases of the ear 
  mutate(bodysystem5=ifelse(diabetes==1 | kidney_disease==1 | thyroid_disorders==1, 1, 0)) %>% #endocrine system
  mutate(bodysystem6=ifelse(blindness==1, 1, 0)) %>% #diseases of the eye 
  mutate(bodysystem7=ifelse(kidney_disease==1, 1, 0)) %>% #genitourinary system 
  mutate(bodysystem8=ifelse(asthma==1 | bronchiectasis==1 | copd==1, 1, 0)) %>% #respiratory system 
  mutate(bodysystem10=ifelse(viral_hepatitis==1, 1, 0)) %>% #infectious disease
  mutate(bodysystem11=ifelse(alcohol==1 | anxiety_depression==1 | schizophrenia==1 | dementia==1 | anorexia==1 | learning_disability==1 | substance_misuse==1, 1, 0)) %>% #mental health disorders
  mutate(bodysystem12=ifelse(arthritis==1, 1, 0)) %>% #musculoskeletal conditions
  mutate(bodysystem13=ifelse(epilepsy==1 | migraine==1 | multiple_sclerosis==1 | parkinsons==1, 1, 0)) %>% #neurological conditions
  mutate(bodysystem15=ifelse(psoriasis==1, 1, 0)) %>%  #skin conditions
  mutate(totbodysystems=bodysystem1 + bodysystem2 + bodysystem4 + bodysystem5 + bodysystem6 + bodysystem7 + bodysystem8 + bodysystem10 + bodysystem11 + bodysystem12 + bodysystem13 + bodysystem15) %>%
  mutate(complexMM=ifelse(totbodysystems>2, 1, 0))
pats_flags1$complexMM <- factor(pats_flags1$complexMM, labels=c("Not MM", "complex MM"))

pats_flags2 <- pats_flags1 %>% 
  mutate(ethnic16label=na_if(ethnic16label, "No data")) %>%
  mutate(ethnic16label=na_if(ethnic16label, "Insufficient information")) %>%
  mutate(gen_ethnicity=na_if(gen_ethnicity, "Unknown")) %>%
  mutate(eth11=ifelse(!is.na(ethnic16label), ethnic16label, gen_ethnicity))
pats_flags2$eth11[pats_flags2$eth11=="African"] <- "Bl_Afric"
pats_flags2$eth11[pats_flags2$eth11=="Bangladesi"] <- "Bangladeshi"
pats_flags2$eth11[pats_flags2$eth11=="Caribbean"] <- "Bl_Carib"
pats_flags2$eth11[pats_flags2$eth11=="British"] <- "White"
pats_flags2$eth11[pats_flags2$eth11=="Irish"] <- "White"
pats_flags2$eth11[pats_flags2$eth11=="Other Asian"] <- "Oth_Asian"
pats_flags2$eth11[pats_flags2$eth11=="Other Black"] <- "Bl_Other"
pats_flags2$eth11[pats_flags2$eth11=="Other white"] <- "White"
pats_flags2$eth11[pats_flags2$eth11=="Other mixed"] <- "Mixed"
pats_flags2$eth11[pats_flags2$eth11=="White & Asian"] <- "Mixed"
pats_flags2$eth11[pats_flags2$eth11=="White & Black African"] <- "Mixed"
pats_flags2$eth11[pats_flags2$eth11=="White & Black Caribbean"] <- "Mixed"
  
pats_flags2 <- pats_flags2 %>% # Include Irish and other white with Other ethnicity
  mutate(eth11Alt=ifelse(!is.na(ethnic16label), ethnic16label, gen_ethnicity))
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="African"] <- "Bl_Afric"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="Bangladesi"] <- "Bangladeshi"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="Caribbean"] <- "Bl_Carib"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="British"] <- "White"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="Irish"] <- "Other"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="Other Asian"] <- "Oth_Asian"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="Other Black"] <- "Bl_Other"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="Other white"] <- "Other"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="Other mixed"] <- "Mixed"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="White & Asian"] <- "Mixed"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="White & Black African"] <- "Mixed"
pats_flags2$eth11Alt[pats_flags2$eth11Alt=="White & Black Caribbean"] <- "Mixed"


#############################################
# select analytical sample with complete data
pats_flags3 <- pats_flags2 %>%
  filter(!is.na(age_baseline) & !is.na(gender) & !is.na(eth11Alt) & !is.na(imd2015_10))

catvars <- c("age_baseline", "gender", "eth11", "eth11Alt", "imd2015_10", "complexMM") # NB IMD1 is least deprived. IMD10 is most deprived.
catTableOverall <- CreateCatTable(vars=catvars, data=pats_flags3)
catTableOverall
summary(catTableOverall)
outputcatTableOverall <- print(catTableOverall, quote=FALSE, noSpaces=TRUE)
write.csv(outputcatTableOverall, file=(str_c(outputs_path, 'mai_table1.csv')))


vars <- c("total")
TableStrat <- CreateTableOne(vars=vars, data=pats_flags3)
TableStrat
outputTable <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'mai_table1meantotal.csv')))
outputTable <- print(TableStrat, nonnormal=vars, quote=FALSE, noSpaces=TRUE) ## or medians
write.csv(outputTable, file=(str_c(outputs_path, 'mai_table1mediantotal.csv')))


## compare with UK population 2019 from populationpyramid.net: 15-29yr 22%, 30-39yr 17%, 40-49yr 16%, 50-59yr 16%, 60-69yr 13%, 70-79yr 10%, 80+yr 6%
## compare with England and Wales pop (2011 census data) ethnicity-facts-figures.service.gov.uk/uk-population-by-ethnicity/national-and-regional-populations/population-of-england-and-wales/latest

## Compare analytical sample and excluded patients
pats_flags2 <- pats_flags2 %>%
  mutate(excluded=ifelse(is.na(age_baseline) | is.na(gender) | is.na(eth11) | is.na(imd2015_10), 1, 0))

vars <- c("age_baseline", "gender", "imd2015_10", "complexMM", "total")  # NB most patients excl because of missing eth11 so can't include that in the table due to small n
catvars <- c("age_baseline", "gender", "imd2015_10", "complexMM")
TableStrat <- CreateTableOne(vars=vars, strata=c("excluded"), data=pats_flags2, factorVars=catvars)
TableStrat
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'mai_suppltable2.csv')))


saveRDS(pats_flags3, file=(str_c(outputs_path, 'pats_flags3.rds')))

