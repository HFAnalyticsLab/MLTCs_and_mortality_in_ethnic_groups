library(tidyverse)
library(haven)
library(arrow)
library(tableone)
library(nnet)
library(survival)
library(survminer)
library(lubridate)
library(broom)
library(ggplot2)


pats_flags3cox <- readRDS(str_c(processed_RDS_path, 'pats_flags3cox.rds'))

pats_flags3cox <- pats_flags3cox %>%
  mutate(totalcat=ifelse(total>8, 8, total)) %>%
  mutate(totalcat=factor(totalcat)) %>%
  mutate(totalcatg=ifelse(total>3, 3, total)) %>%
  mutate(totalcatg=factor(totalcatg)) %>%
  mutate(totalcapped=ifelse(total>8, 8, total)) %>%
  mutate(startage=2015-yob-50) %>%
  mutate(eth11=factor(eth11, levels=c("White", "Bangladeshi", "Pakistani", "Indian", "Oth_Asian" , "Bl_Afric", "Bl_Carib", "Bl_Other", "Chinese", "Mixed", "Other")))  ## reorder categories for tables
pats_flags3cox$startageg <- cut(pats_flags3cox$startage, 
                                breaks=c(-Inf, 0, Inf),
                                labels=c("18-50y", ">50y"))  
catvars <- c("event")
catTableOverall <- CreateCatTable(vars=catvars, data=pats_flags3cox)
catTableOverall



## check whether assoc between survival and LTCs is approx linear
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcat + cluster(pracid), data=pats_flags3cox)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_suppltable3.csv')))



## model checking
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ total, data=pats_flags3cox)
plotdata1<-pats_flags3cox
plotdata1$resid_mart<-resid(coxoutput, type="martingale", collapse = pats_flags3cox$patid) 


index.plot<- function (var.x=total, var.y=resid_mart){
  var.x<-enquo(var.x)
  var.y<-enquo(var.y)
  ggplot(data=plotdata1, mapping=aes(x=!!var.x, y=!!var.y))+
    geom_point()+
    geom_smooth()+
    theme_bw()+theme(legend.key = element_blank())
}
##smothing method is gam; cannot use loess or lowess because of the large sample size

index.plot(var.x=total) #the smoothed curve is not a straight line suggesting a non linear form is needed. Plot shows larger negative residuals at larger values of total. Try log transform. 

## Nonlinearity kicks in at 8+ LTCs - explore the data
checkhightotal <- plotdata1 %>% filter(total>8) %>% select(patid, gender, startage, total, eth11, pracid, imd2015_10, event, years_in_study_cprd, bodysystem1, bodysystem2, bodysystem4, bodysystem5, bodysystem6, bodysystem7, bodysystem8, bodysystem10, bodysystem11, bodysystem12, bodysystem13, bodysystem15, resid_mart)
vars <- c("gender", "startage", "total", "eth11", "imd2015_10", "event", "resid_mart")
catvars <- c("gender", "total", "eth11", "imd2015_10", "event")
tablehightotal <- CreateTableOne(vars=vars, data=checkhightotal, factorVars=catvars)
tablehightotal


checklowertotal <- plotdata1 %>% filter(total<9) %>% select(patid, gender, startage, total, eth11, pracid, imd2015_10, event, years_in_study_cprd, bodysystem1, bodysystem2, bodysystem4, bodysystem5, bodysystem6, bodysystem7, bodysystem8, bodysystem10, bodysystem11, bodysystem12, bodysystem13, bodysystem15, resid_mart)
vars <- c("gender", "startage", "total", "eth11", "imd2015_10", "event", "resid_mart")
catvars <- c("gender", "total", "eth11", "imd2015_10", "event")
tablelowertotal <- CreateTableOne(vars=vars, data=checklowertotal, factorVars=catvars)
tablelowertotal


#Try Log transform 
pats_flags3cox <- pats_flags3cox %>%
  mutate(totalnonzero=total+0.001) %>%
  mutate(logtotal=log(totalnonzero))
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ logtotal, data=pats_flags3cox)
plotdata1<-pats_flags3cox
plotdata1$resid_mart<-resid(coxoutput, type="martingale", collapse = pats_flags3cox$patid) 

index.plot<- function (var.x=logtotal, var.y=resid_mart){
  var.x<-enquo(var.x)
  var.y<-enquo(var.y)
  ggplot(data=plotdata1, mapping=aes(x=!!var.x, y=!!var.y))+
    geom_point()+
    geom_smooth()+
    theme_bw()+theme(legend.key = element_blank())
}

index.plot(var.x=logtotal) 


#Check total capped at 8 LTCs
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ totalcapped, data=pats_flags3cox)
plotdata1<-pats_flags3cox
plotdata1$resid_mart<-resid(coxoutput, type="martingale", collapse = pats_flags3cox$patid) 

index.plot<- function (var.x=totalcapped, var.y=resid_mart){
  var.x<-enquo(var.x)
  var.y<-enquo(var.y)
  ggplot(data=plotdata1, mapping=aes(x=!!var.x, y=!!var.y))+
    geom_point()+
    geom_smooth(method="gam", formula=y ~ s(x, bs = "cs", k=8))+
    theme_bw()+theme(legend.key = element_blank())
}

index.plot(var.x=totalcapped) ## Some remaining non-linearity though size of martingale residuals smaller than using raw total



## Cox model to show assoc between number of LTCs and mortality
##coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + eth11 + totalcapped + cluster(pracid), data=pats_flags3cox)
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + totalcapped*startage + cluster(pracid), data=pats_flags3cox) ## improvement in goodness of fit when include ethnicity x age and total x age interactions  
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table2.csv')))
write.csv(glance(coxoutput), file=(str_c(outputs_path, 'mai_table2fit.csv')))


## Cox model to test whether the mortality risk for LTCs depends on ethnicity
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + eth11*totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3cox)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3.csv')))
write.csv(glance(coxoutput), file=(str_c(outputs_path, 'mai_table3fit.csv')))
#coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage*totalcapped + cluster(pracid), data=pats_flags3cox) ## 3-way interaction does not improve model fit so do not use


## stratify by ethnicity to get a handle on all the interactions
pats_flags3coxBlAfr <- pats_flags3cox %>% filter(eth11=="Bl_Afric")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxBlAfr)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3BlAfr.csv')))

pats_flags3coxBangl <- pats_flags3cox %>% filter(eth11=="Bangladeshi")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxBangl)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3Bangl.csv')))

pats_flags3coxBlCarib <- pats_flags3cox %>% filter(eth11=="Bl_Carib")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxBlCarib)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3BlCarib.csv')))

pats_flags3coxBlOth <- pats_flags3cox %>% filter(eth11=="Bl_Other")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxBlOth)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3Blother.csv')))

pats_flags3coxIndian <- pats_flags3cox %>% filter(eth11=="Indian")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxIndian)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3Indian.csv')))

pats_flags3coxPakst <- pats_flags3cox %>% filter(eth11=="Pakistani")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxPakst)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3Pakst.csv')))

pats_flags3coxChn <- pats_flags3cox %>% filter(eth11=="Chinese")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxChn)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3Chinese.csv')))

pats_flags3coxOthAsian <- pats_flags3cox %>% filter(eth11=="Oth_Asian")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxOthAsian)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3OthAsian.csv')))

pats_flags3coxMixed <- pats_flags3cox %>% filter(eth11=="Mixed")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxMixed)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3Mixed.csv')))
                 
pats_flags3coxOth <- pats_flags3cox %>% filter(eth11=="Other")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxOth)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3Other.csv')))

pats_flags3coxWhite <- pats_flags3cox %>% filter(eth11=="White")
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + totalcapped + startage*totalcapped + cluster(pracid), data=pats_flags3coxWhite)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_table3White.csv')))


## Check model estimates look same across age groups
#pats_flags3coxyoung <- pats_flags3cox %>% filter(startage<15) ##age 18-64
#pats_flags3coxolder <- pats_flags3cox %>% filter(startage>=15) ##age 65 and older
#coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + totalcapped*startage + cluster(pracid), data=pats_flags3coxyoung) 
#coxoutput
#coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + totalcapped*startage + cluster(pracid), data=pats_flags3coxolder) 
#coxoutput

## Check model estimates look similar for men and women
#pats_flags3coxM <- pats_flags3cox %>% filter(gender=="men")
#pats_flags3coxF <- pats_flags3cox %>% filter(gender=="women")
#coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ eth11*startage + totalcapped*startage + cluster(pracid), data=pats_flags3coxM) 
#coxoutput
#coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ eth11*startage + totalcapped*startage + cluster(pracid), data=pats_flags3coxF) 
#coxoutput



## check contribution of years with 2+ LTCs
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + eth11*totalcapped + startage*totalcapped + yearsMM + cluster(pracid), data=pats_flags3cox)
coxoutput

## check contribution of additional MLTCs
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + eth11*totalcapped + startage*totalcapped + yearsMM + EndMLTC_count + cluster(pracid) , data=pats_flags3cox)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_suppltable5a.csv')))
write.csv(glance(coxoutput), file=(str_c(outputs_path, 'mai_suppltable5afit.csv')))

## check contribution of IMD
## first check if IMD should be included as categorical or if approx linear assoc is an okay fit
pats_flags3cox <- pats_flags3cox %>%
  mutate(imd2015_10cat=factor(imd2015_10))
#coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + imd2015_10cat , data=pats_flags3cox)
#coxoutput
#coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + startage + imd2015_10 , data=pats_flags3cox) # categorical is better fit
#coxoutput
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + eth11*totalcapped + startage*totalcapped + yearsMM + EndMLTC_count + imd2015_10cat  + cluster(pracid), data=pats_flags3cox)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_suppltable5.csv')))



## check whether things look the same for complex MM
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + eth11 + complexMM + cluster(pracid), data=pats_flags3cox)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_suppltable6.csv')))
write.csv(glance(coxoutput), file=(str_c(outputs_path, 'mai_suppltable6fit.csv')))

coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + startage*complexMM + eth11*complexMM + cluster(pracid), data=pats_flags3cox)
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_suppltable7.csv')))
write.csv(glance(coxoutput), file=(str_c(outputs_path, 'mai_suppltable7fit.csv')))



#####################################################################################################
## LOOK AT SPECIFIC CONDITIONS THAT CONTRIBUTE TO HIGH MORTALITY or Look at specific causes of death?
catvars <- c("bodysystem1", "bodysystem2", "bodysystem5", "bodysystem8", "bodysystem13") 
TableStrat <- CreateTableOne(vars=catvars, strata=c("eth11"), data=pats_flags3cox, factorVars=catvars)
TableStrat #number of cancers too small to look at separately

coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + startage*bodysystem2 + eth11*bodysystem2  + cluster(pracid), data=pats_flags3cox) #circulatory system
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_suppltable7circulatory.csv')))
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + startage*bodysystem5 + eth11*bodysystem5 + cluster(pracid), data=pats_flags3cox) #endocrine system
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_suppltable7endocrine.csv')))
coxoutput <- coxph(Surv(years_in_study_cprd, event) ~ gender + eth11*startage + startage*bodysystem8 + eth11*bodysystem8 + cluster(pracid), data=pats_flags3cox) #respiratory
coxoutput
write.csv(tidy(coxoutput), file=(str_c(outputs_path, 'mai_suppltable7respiratory.csv')))


