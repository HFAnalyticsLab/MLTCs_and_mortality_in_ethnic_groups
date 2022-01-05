library(tidyverse)
library(haven)
library(arrow)
library(data.table)
library(tableone)
library(nnet)
library(survival)
library(survminer)
library(lubridate)


pats_flags3 <- readRDS(str_c(processed_RDS_path, 'pats_flags3.rds'))


#############################################
# Derive survival time and compare for those with complete and missing ethnicity

pats_flags3 <- pats_flags3 %>%
  group_by(patid) %>%
  mutate(study_end=c("31/12/2019")) %>%
  mutate(study_end=as.Date(study_end,"%d/%m/%Y")) %>%
  mutate(regenddate=as.Date(regenddate,"%Y-%m-%d")) %>%
  mutate(ddate=as.Date(ddate,"%Y-%m-%d")) %>%
  mutate(lcd=as.Date(lcd,"%d/%m/%Y")) %>%
  mutate(censoring_date_cprd=min(regenddate, lcd, study_end, na.rm=TRUE)) %>%
  mutate(days_in_study_cprd=(as.numeric(censoring_date_cprd - as.Date('2015-01-01')))) %>%
  mutate(years_in_study_cprd=round(as.numeric(censoring_date_cprd - as.Date('2015-01-01'))/365, 2)) %>%
  mutate(years_in_study_cprd=as.numeric(years_in_study_cprd)) %>%
  mutate(event=ifelse(years_in_study_cprd<5 & !is.na(ddate), 2, 1)) %>% ## event is 2 if died and 1 otherwise
  ungroup()

pats_flags3 <- pats_flags3 %>%
  mutate(yearsMM=(as.numeric(difftime(as.Date('2015-01-01'), oldest_cond, units="weeks")))/52.25) %>%
  mutate(yearsMM=ifelse(total<2, 0, yearsMM))

min(pats_flags3$years_in_study_cprd)

catvars <- c("event")
catTableOverall <- CreateCatTable(vars=catvars, data=pats_flags3)
catTableOverall
outputTable <- print(catTableOverall, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'mai_table1died.csv')))



## Characteristics by ethnicity
pats_flags3 <- pats_flags3 %>%
mutate(eth11=factor(eth11, levels=c("Bangladeshi", "Pakistani", "Indian", "Oth_Asian" , "Bl_Afric", "Bl_Carib", "Bl_Other", "Chinese", "Mixed", "Other", "White")))  ## reorder categories for tables
vars <- c("age_baseline", "gender", "imd2015_10", "complexMM", "event", "years_in_study_cprd", "yearsMM", "total", "EndMLTC_count")
catvars <- c("age_baseline", "gender", "imd2015_10", "complexMM", "event")
TableStrat <- CreateTableOne(vars=vars, strata=c("eth11"), data=pats_flags3, factorVars=catvars)
TableStrat
outputTable <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
# outputTable <- print(TableStrat, nonnormal=contvars, quote=FALSE, noSpaces=TRUE) ## or medians
write.csv(outputTable, file=(str_c(outputs_path, 'mai_suppltable4.csv')))


pats_flags3cox <- pats_flags3 %>%
  mutate(eth11Alt=factor(eth11Alt, levels=c("White", "Bangladeshi", "Pakistani", "Indian", "Oth_Asian" , "Bl_Afric", "Bl_Carib", "Bl_Other", "Chinese", "Mixed", "Other")))  ## reorder categories so white is reference group

saveRDS(pats_flags3cox, file=(str_c(processed_RDS_path, 'pats_flags3cox.rds')))



#################### Test model assumptions ##################################
pats_flags3cox <- pats_flags3cox %>%
  mutate(startage=2015-yob-50)
pats_flags3cox1 <- pats_flags3cox %>% filter(years_in_study_cprd>0)

coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + eth11Alt*total, data=pats_flags3cox1)
coxoutput
testph <- cox.zph(coxoutput) # test proportional hazards assumption
testph

splitdata <- survSplit(Surv(years_in_study_cprd, event)  ~ gender + eth11Alt*startage + eth11Alt*total,
                       data=pats_flags3cox1,
                       cut=c(2.5),
                       start="tstart",
                       id="patid",
                       zero=0,
                       episode="tgroup",
                       event="event")

intervalcoxoutput <- coxph(Surv(tstart, years_in_study_cprd, event) ~ gender:strata(tgroup) + startage:strata(tgroup) + startage + eth11Alt*total, data=splitdata)
intervalcoxoutput # gender coeff doesnt differ greatly across followup time. But the age coeff is much larger in first half of followup. What does this mean?

intervalcoxoutput <- coxph(Surv(tstart, years_in_study_cprd, event) ~ gender:strata(tgroup) + startage:strata(tgroup) + eth11Alt:strata(tgroup) + total:strata(tgroup), data=splitdata)
intervalcoxoutput # Although the coeffs vary across followup time, differences for age, number of LTCs and gender are small. Diffs are larger but not consistent for ethnicity. No a priori rationale for the ethnicity effect to depend on follow-up time.



