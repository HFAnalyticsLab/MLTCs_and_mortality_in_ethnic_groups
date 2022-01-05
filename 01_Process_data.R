#### Prep Aurum sample and derive variables


library(aurumpipeline) ## load in the pipeline functions
library(data.table)
library(tidyverse)
library(lubridate)

ls('package:aurumpipeline') ## list of pipeline functions


pats <- opendt(patient_path) ## get all the patient data
prac <- opendt(practice_path) ## practice data

pats <- merge(pats, prac, by = 'pracid') ## join them

### inclusion criteria goes here:
max(pats$regstartdate)
min(pats$regenddate[!is.na(pats$regenddate)])
min(pats$cprd_ddate[!is.na(pats$cprd_ddate)])
min(prac$lcd)
sum(is.na(prac$lcd))
min(2015 - pats$yob)
pats[, .N, by = gender]
pats[, .N, by = acceptable]
aurumLkup::lkup_Region
pats[, .N, by = region][order(region)] ## 134 patients with missing region


###################################################################################
####### copy qof_process function here and edit to allow date of condition to output
obscount <- length(list.files(obs_path, full.names = T))

## get Anna L's codelists### edit existing function
codeloc <- code_path

enddate <- as.Date('2015-01-01')

## get all codelists
codelist <- read_multimorb_codelists(codeloc)

## get defined LTCs
code_cats <- readxl::read_excel(
  cat_path
  , sheet = 'Lists_combined'
  , skip = 1
)

code_cats$Disease <- gsub(' ', '_', code_cats$Disease)
code_cats$Disease[code_cats$Disease == 'End_stage_renal_disease'] <- 'Chronic_Kidney_Disease'
code_cats$Disease[code_cats$Disease == 'Peripheral_Arterial_Disease'] <- 'Peripheral_Vascular_Disease'

codelist <- merge(codelist, code_cats, by.x = 'disease', by.y = 'Disease') %>%
  .[, .(disease, medcodeid, read, Colname)]


codelist[, .N, by = disease]
codelist[, .N, by = Colname][order(N)]

# filter observations by medcode observations only - you don't need to match or join before doing this, 
# just use the variable from second DT as a filter
# we also don't want any data with missing date variables
diag_obs <- data.table::data.table()

for (i in 1:obscount){
  
  temp <- opendt(file.path(obs_path, i)
                 , cols_in = c('patid', 'medcodeid', 'obsdate', 'obstypeid')) %>%
    ## only those in the condition list and not family history and not after the required date
    dplyr::filter(medcodeid %in% codelist$medcodeid & obstypeid != 4) # don't filter by enddate now so can re-use later 
  
  diag_obs <- rbind(temp, diag_obs)
  gc()
  
  print(paste0('Completed ', i, ' of ', obscount, ', found ', nrow(diag_obs), ' observations'))
  
}

# sort the data
data.table::setkey(diag_obs, patid, obsdate)

# select just the date, medcode and patid variables
diag_obs <- diag_obs[, .(patid, medcodeid, obsdate)] %>% unique()

### dates for disease diagnoses, cancer first
cancer <- diag_obs[codelist[Colname == 'cancer'], on = .(medcodeid)][ #restrict to relevant medcodes using a join
  order(obsdate), .SD[.N], by = .(patid, Colname)][ #retain newest cancer record per patient
    obsdate < enddate & obsdate >= (enddate - read)][ #see if newest was in last 5 years
    , .(oldest_cond = min(obsdate), recent_cond = max(obsdate)) ## both dates are the same as only most recent diagnoses kept
    , by = .(patid, Colname, read)]

# other conditions by medcode
other_conds <- diag_obs[codelist[Colname != 'cancer'], on = .(medcodeid), allow.cartesian = TRUE][ #restrict to relevant medcodes using a join
  obsdate < enddate & obsdate >= (enddate - read)][ #restrict to required date range
    , .(mcount = .N, oldest_cond = min(obsdate), recent_cond = max(obsdate))
    , by = .(patid, Colname, read)] #count by patient/condition


### product code to identify athsma/depression/anxiety
coltypes <- NULL
file_pattern <- 'csv'

files <- list.files(codeloc, pattern = file_pattern, full.names = TRUE) #look for drug lists
files <- files[order(files)]

codes <- purrr::map(files, vroom::vroom, delim = ',', col_types = coltypes)
  
## combine prodcodes from each with descriptions matching codelists defined before  
  data <- rbind(data.table::data.table(Colname = 'anxiety_prodcode'
              , category = 'Diagnoses of Anxiety'
              , system = 'Mental Health Disorders'
              , prodcodeid = bit64::as.integer64(codes[[1]]$ProdCodeId)) ## different capitalisation
    
             , data.table::data.table(Colname = 'asthma'
             , category = 'Diagnoses of Asthma'
             , system = 'Diseases of the Respiratory System'
             , prodcodeid = bit64::as.integer64(codes[[2]]$ProdCodeID))
  
            , data.table::data.table(Colname = 'depression_prodcode'
             , category = 'Diagnoses of Depression'
             , system = 'Mental Health Disorders'
             , prodcodeid = bit64::as.integer64(codes[[3]]$ProdCodeId)))
  
  data <- data[!is.na(prodcodeid), ]
  
  data[, .N, by = Colname]
  
  ## asthma needs prodcode in the last year, others need 4
  ## also a readcode whenever, others within a  year
  data$read <- ifelse(data$Colname == 'asthma', 9999999, 365)
  data$Rx <- ifelse(data$Colname == 'asthma', 1, 4)
  
#Read in drug and diagnosis codes 
drugcount <- length(list.files(drug_path, full.names = T))
  
presc <- data.table::data.table()

for (i in 1:drugcount){
    
    temp <- opendt(here::here(drug_path, i)
                   , cols_in = c('patid', 'prodcodeid', 'issuedate', 'issueid')) %>%
      ## only those in the condition list and not family history and not after the required date
      dplyr::filter(prodcodeid %in% data$prodcodeid) # don't filter on enddate so we can use again later
    
    presc <- rbind(temp, presc)
    gc()
    
    print(paste0('Completed ', i, ' of ', drugcount, ', found ', nrow(presc), ' observations'))
    
  }
  
  # sort the data
  data.table::setkey(presc, patid, issuedate)
  
  # select just the date and patid variables
  presc <- presc[, .(patid, prodcodeid, issuedate)] %>% unique()
    
#Process drug issue data to identify conditions by prodcode
drugs <- presc[data, on = .(prodcodeid), allow.cartesian = TRUE][ #restrict to relevant prodcodes using a join
    issuedate < enddate & issuedate >= (enddate - read)][ #restrict to required date range
      , .(pcount = .N, oldest_cond = min(issuedate), recent_cond = max(issuedate))
      , by = .(patid, Colname, read, Rx)][ #count records by patient and condition
      pcount >= Rx] %>% #restrict using Rx field
      .[, .(patid, Colname, read, oldest_cond, recent_cond)] # required columns

## asthma needs both entries to exist to be flagged 
both_asthma_conds <- intersect(drugs$patid[drugs$Colname == 'asthma'] # patids of those with both condition and prescription
          , other_conds$patid[other_conds$Colname == 'asthma'])

rem <- other_conds[other_conds$Colname == 'asthma' & !(other_conds$patid %in% both_asthma_conds), ]

other_conds <- dplyr::anti_join(other_conds, rem, by = c('patid', 'Colname'))

######## Combine for results ######################################
res <- data.table::rbindlist(list(cancer, other_conds
                             , drugs[drugs$Colname != 'asthma'])
                             , use.names = T, fill = T)

## get date patients had 2 different ltcs:
res2 <- res[order(res$patid, res$oldest_cond)]

## combine dep/anx prod and medcodes, get oldest dates where patient has more than 1
res2$Coltocount <- gsub('_prodcode|_medcode', '', res2$Colname)

res2 <- res2 %>% dplyr::group_by(patid, Coltocount) %>%
  dplyr::summarise(oldest_cond = min(oldest_cond)) %>% data.table::setDT()
res2 <- res2[order(res2$patid, res2$oldest_cond)]

res2[, num := seq_len(.N), by = patid]

resmin <- res2[num == 2, ][, .(patid, oldest_cond)]

## remove duplicates where someone has both medcode and prodcode for asthma
## don't have to do this now
res <- res %>% dplyr::group_by(patid, Colname) %>%
  dplyr::summarise(oldest_cond = min(oldest_cond)) %>% data.table::setDT()

## get combined anxiety
anx <- res[Colname %in% c('anxiety_medcode', 'anxiety_prodcode'), ] %>%
  select(patid, oldest_cond) %>%
  dplyr::group_by(patid) %>% 
  dplyr::summarise(oldest_cond = min(oldest_cond)) %>% 
  data.table::setDT()

anx$Colname <- 'anxiety'

## get combined depression
dep <- res[Colname %in% c('depression_medcode', 'depression_prodcode'), ] %>%
  select(patid, oldest_cond) %>%
  dplyr::group_by(patid) %>% 
  dplyr::summarise(oldest_cond = min(oldest_cond)) %>% 
  data.table::setDT()

dep$Colname <- 'depression'

## get anxiety & depression field
both <- rbind(anx, dep) %>%
  select(patid, oldest_cond) %>%
  dplyr::group_by(patid) %>% 
  dplyr::summarise(oldest_cond = min(oldest_cond)) %>% 
  data.table::setDT()

both$Colname = 'anxiety_depression'

res <- rbind(res, anx, dep, both)

col_heads <- unique(res$Colname[order(res$Colname)])

## 
results_dates <- res[ #bind our tables -- use character format for dates 
  , .(patid, Colname, as.character(oldest_cond))] %>% #restrict to required variables 
  reshape2::dcast(., ... ~ Colname, value.var = 'V3', fill = '1800-01-01') ## cast long to wide with disease names as field headers

## dcasting with dates converts them to numeric class so need to change back:
## note if missing 1st Jan 1800 is used , prefer that to NA?
results_dates[, col_heads] <- lapply(results_dates[, col_heads], lubridate::ymd)

results_dates <- results_dates %>% merge(resmin ## add on date of 2 ltcs
                             , by = 'patid', all.x = T)

## add date to name to differentiate when combined with flags
data.table::setnames(results_dates, old = col_heads
                     , new = paste0(names(results_dates[, col_heads]), '_Date')) 
date_names <- names(results_dates)[2:ncol(results_dates)] ## get new names

results <- res[ #bind our tables
  , .(patid, Colname, flag = 1)] %>% #restrict to required variables and add a flag variable in (to indicate the ref exists)
  reshape2::dcast(., ... ~ Colname, value.var = 'flag', fill = 0) ## cast long to wide with disease names as field headers

gc() #clean up memory


## merge on flags to patients data and fill in NAs
pats <- merge(pats, results, by = 'patid', all.x = T)

pats <- merge(pats, results_dates, by = 'patid', all.x = T)

data.table::setnafill(pats, cols = col_heads, fill = 0)
data.table::setnafill(pats, cols = date_names, fill = as.Date('1800-01-01'))

## look at proportion of patients with each condition:
pats[, lapply(.SD, sum), .SDcols = col_heads] # number of patients with each
pats[, lapply(.SD, mean), .SDcols = col_heads] * 100 # proportion

## specify which condition fields are to be included in total count for MLTC
## some are subsets of others
to_use <- col_heads[!grepl('code|anxiety_depression', col_heads)]

pats[, total := rowSums(.SD, na.rm = TRUE), .SDcols = to_use] ## total conditions per patient

pats[, .N, by = total][order(total)] ## distribution of condition num

length(unique(results$patid)) / length(unique(pats$patid)) * 100 ## overall prop of those with a condition

## clean up
rm('results', 'results_dates', 'temp')
gc()

#### add ethnicity info
## get EMIS lookup and categories first
medlkup <- aurumLkup::lkup_202101_EMISMedicalDictionary

ethnic16 <- data.table::data.table(ethnic16 = c(1:17, -99)
                                   , ethnic16label = c('British', 'Irish', 'Other white', 'White & Black Caribbean', 
                                                       'White & Black African', 'White & Asian', 'Other mixed', 'Indian',
                                                       'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'African',
                                                       'Other Black', 'Chinese', 'Other', 'Insufficient information',
                                                       'Insufficient information'))

ethnic5 <- data.table::data.table(ethnic5 = c(1:5)
                                  , ethnic5label = c('White', 'Mixed', 'Asian', 'Black', 'Other'))

## load in opensafely codelist - need to convert from Snomedid to medcodeid
eth_codelist <- readxl::read_excel(
  eth_path
  , col_types = 'text') %>% data.table::setDT()

## make sure ids are right format
eth_codelist$id <- bit64::as.integer64(eth_codelist$id)
eth_codelist$Ethnic5 <- as.numeric(eth_codelist$Ethnic5)
eth_codelist$Ethnic16 <- as.numeric(eth_codelist$Ethnic16)

## merge to get medcodeids and ethnic categories
eth_codelist <- eth_codelist %>% merge(medlkup, by.x = 'id', by.y = 'SnomedCTConceptId') %>%
  merge(ethnic5, by.x = 'Ethnic5', by.y = 'ethnic5', all.x = TRUE) %>%
  merge(ethnic16, by.x = 'Ethnic16', by.y = 'ethnic16', all.x = TRUE) %>%
  .[, .(medcodeid = MedCodeId, ethnic5label, ethnic16label)]

eth_obs <- data.table::data.table()

## load in relevent observations
for (i in 1:obscount){
  
  temp <- opendt(here::here(obs_path, i)
                 , cols_in = c('patid', 'medcodeid', 'obsdate', 'obstypeid')) %>%
    ## only those in the condition list and not family history and not after the required date
    dplyr::filter(medcodeid %in% eth_codelist$medcodeid & obstypeid != 4 & obsdate <= enddate) 
  
  eth_obs <- rbind(temp, eth_obs)
  gc()
  
  print(paste0('Completed ', i, ' of ', obscount, ', found ', nrow(eth_obs), ' observations'))
  
}

eth <- add_ethnicity(eth_obs, eth_codelist, c('ethnic5label', 'ethnic16label'))
eth$ethnic5label[is.na(eth$ethnic5label)] <- 'Uncategorised'
eth$ethnic16label[is.na(eth$ethnic16label)] <- 'Uncategorised'

pats <- merge(pats, eth, by = 'patid', all.x = TRUE)

## fill in missing data
pats$ethnic5label[is.na(pats$ethnic5label)] <- 'No data'
pats$ethnic16label[is.na(pats$ethnic16label)] <- 'No data'

pats[, .N, by = ethnic5label][order(-N)]
pats[, .N, by = ethnic16label][order(-N)]

######################################################################################
### linked datasets
## hes ethnicity 
hes <- data.table::fread(paste0(linked_path, 'hes_patient_20_000239.txt'))

hes[, .N, by = gen_ethnicity][order(gen_ethnicity)]
hes$patid <- as.numeric(hes$patid)

hes$gen_ethnicity[is.na(hes$gen_ethnicity)] <- 'Unknown'
hes$gen_ethnicity[hes$gen_ethnicity == ''] <- 'Unknown'

## convert hes eth to eth5
eth_links <- data.table::data.table(gen_ethnicity = c('Bangladeshi', 'Bl_Afric', 'Bl_Carib', 'Bl_Other'
                                                      , 'Chinese', 'Indian', 'Mixed', 'Oth_Asian'
                                                      , 'Other', 'Pakistani', 'Unknown', 'White')
                                    , update_eth = c('Asian', 'Black', 'Black', 'Black', 'Asian', 'Asian'
                                                       , 'Asian', 'Asian', 'Other', 'Asian', 'No data', 'White')
)
#pats$gen_ethnicity <- NULL


hes <- merge(hes, eth_links, by = 'gen_ethnicity', all.x = T)
pats <- merge(pats, hes[, c('patid', 'gen_ethnicity', 'update_eth')], by = 'patid', all.x = T)
pats[, .N, by = gen_ethnicity][order(gen_ethnicity)]

check <- pats[, .N, by = c('ethnic5label', 'ethnic16label', 'gen_ethnicity', 'update_eth')]

sum(check[check$ethnic5label == 'No data', ]$N) ## how many missing cprd eth we have from HES (including unknowns)
sum(check[check$ethnic5label == 'No data' & check$gen_ethnicity != 'Unknown', ]$N) ## how many missing cprd eth we can replace from hes
check[check$ethnic5label == 'No data' & check$gen_ethnicity != 'Unknown', ]

## conditional replace
pats[, eth5_update := ifelse(ethnic5label == 'No data' & gen_ethnicity != 'Unknown' & !is.na(gen_ethnicity)
                             , update_eth, ethnic5label)]
pats[, .N*100/nrow(pats), by = ethnic5label] ## original proportions
pats[, .N*100/nrow(pats), by = eth5_update] ## updated proportions
pats[, .N*100/nrow(pats), by = gen_ethnicity] ## hes data only proportions
pats[, .N*100/nrow(pats), by = ethnic16label] ## eth 16 proportions (cannot update from HES)

## update HES eth from eth16:


## 2011 census breakdown:
## 86% white
## 7.5% asian
## 3.3% black
## 2.2% mixed
## 1.0% other


## death data
dlink <- data.table::fread(paste0(linked_path, 'death_patient_20_000239.txt'))

dlink$patid <- as.numeric(dlink$patid)
dlink$dod <- as.Date(dlink$dod, format = '%d/%m/%Y')

all_data <- merge(pats, dlink, by = 'patid', all.x = T)
all_data[all_data$cprd_ddate == all_data$dod, c('cprd_ddate', 'dod')] ## where death dates match
all_data[all_data$cprd_ddate != all_data$dod, c('cprd_ddate', 'dod')] ## where they don't
all_data[is.na(all_data$cprd_ddate) & !is.na(all_data$dod), c('cprd_ddate', 'dod')] ## where cprd date is missing - to be updated

## create death date using ONS death where available, otherwise use CPRD
all_data[, ddate := if_else(is.na(dod), cprd_ddate, dod)]
## and end date, min of death, leave cprd or 31/12/2019

all_data[, edate := apply(
                cbind(all_data[, c('ddate', 'regenddate'), with = FALSE]
                      , as.Date('2019-12-31')), 1, FUN = min)]

all_data[is.na(edate), edate := '2019-12-31']
all_data[, edate := as.Date(all_data$edate)]

## clean up names
names(all_data)[2] <- 'pracid'
all_data$pracid.y <- NULL

## ons deprivation data
ons_dep <- data.table::fread(paste0(linked_path, 'patient_imd2015_20_000239.txt'))

ons_dep$patid <- as.numeric(ons_dep$patid)

all_data <- merge(all_data, ons_dep[, c('patid', 'imd2015_10')], by = 'patid', all.x = T)

all_data[, .N, by = imd2015_10][order(imd2015_10)]

## dist of imd
all_data[, .N, by = imd2015_10][order(imd2015_10)]

## check dist of imd by ethnicity
table(all_data$eth5_update, all_data$imd2015_10) %>% 
  prop.table(., 1) %>%
  
  
  {. * 100} %>%
  round(2)

#################################################################################
### number of ltcs at study end date (edate)
### dates for disease diagnoses, cancer first
diag_obs_end <- merge(diag_obs, all_data[, c('patid', 'edate'), with = F] ## merge to get end date for patients
                      , by = 'patid', all.x = T)

cancer_end <- diag_obs_end[codelist[Colname == 'cancer'], on = .(medcodeid)][ #restrict to relevant medcodes using a join
  order(obsdate), .SD[.N], by = .(patid, Colname)][ #retain newest cancer record per patient
    obsdate < edate & obsdate >= (edate - read)][ #see if newest was in last 5 years
      , .(oldest_cond = min(obsdate), recent_cond = max(obsdate)) ## both dates are the same as only most recent diagnoses kept
      , by = .(patid, Colname, read)]

# other conditions by medcode
other_conds_end <- diag_obs_end[codelist[Colname != 'cancer'], on = .(medcodeid), allow.cartesian = TRUE][ #restrict to relevant medcodes using a join
  obsdate < edate & obsdate >= (edate - read)][ #restrict to required date range
    , .(mcount = .N, oldest_cond = min(obsdate), recent_cond = max(obsdate))
    , by = .(patid, Colname, read)] #count by patient/condition

## now for prodcodes
presc_end <- merge(presc, all_data[, c('patid', 'edate'), with = F]
                      , by = 'patid', all.x = T)

#Process drug issue data to identify conditions by prodcode
drugs_end <- presc_end[data, on = .(prodcodeid), allow.cartesian = TRUE][ #restrict to relevant prodcodes using a join
  issuedate < edate & issuedate >= (edate - read)][ #restrict to required date range
    , .(pcount = .N, oldest_cond = min(issuedate), recent_cond = max(issuedate))
    , by = .(patid, Colname, read, Rx)][ #count records by patient and condition
      pcount >= Rx] %>% #restrict using Rx field
  .[, .(patid, Colname, read, oldest_cond, recent_cond)] # required columns

## asthma needs both entries to exist to be flagged 
both_asthma_conds_end <- intersect(drugs_end$patid[drugs$Colname == 'asthma'] # patids of those with both condition and prescription
                               , other_conds_end$patid[other_conds_end$Colname == 'asthma'])

rem_end <- other_conds_end[other_conds_end$Colname == 'asthma' & !(other_conds_end$patid %in% both_asthma_conds_end), ]

other_conds_end <- dplyr::anti_join(other_conds_end, rem_end, by = c('patid', 'Colname'))

######## Combine for results ######################################
res_end <- data.table::rbindlist(list(cancer_end, other_conds_end
                                  , drugs_end[drugs_end$Colname != 'asthma'])
                             , use.names = T, fill = T) %>% .[, .(patid, Colname)] %>% unique()

## remove med/prodcode disctinction:
res_end$Colname <- gsub('_medcode|_prodcode', '', res_end$Colname)
res_end <- unique(res_end)

res_end <- res_end[, .N, by = patid]
names(res_end)[2] <- 'EndMLTC_count'

all_data <- merge(all_data, res_end, by = 'patid', all.x = T)
all_data[is.na(EndMLTC_count), EndMLTC_count := 0]

all_data[, .N, by = EndMLTC_count][order(EndMLTC_count)] ## few higher but afew lower - to be expected I think
all_data[, .N, by = total][order(total)]


### number of ltcs at 3 months prior to study end date (edate)
### dates for disease diagnoses, cancer first
diag_obs_3 <- merge(diag_obs, all_data[, c('patid', 'edate'), with = F] ## merge to get end date for patients
                      , by = 'patid', all.x = T)

diag_obs_3[, edate3 := edate %m-% months(3)] ## get 3 month prior date

cancer_3 <- diag_obs_3[codelist[Colname == 'cancer'], on = .(medcodeid)][ #restrict to relevant medcodes using a join
  order(obsdate), .SD[.N], by = .(patid, Colname)][ #retain newest cancer record per patient
    obsdate < edate3 & obsdate >= (edate3 - read)][ #see if newest was in last 5 years
      , .(oldest_cond = min(obsdate), recent_cond = max(obsdate)) ## both dates are the same as only most recent diagnoses kept
      , by = .(patid, Colname, read)]

# other conditions by medcode
other_conds_3 <- diag_obs_3[codelist[Colname != 'cancer'], on = .(medcodeid), allow.cartesian = TRUE][ #restrict to relevant medcodes using a join
  obsdate < edate3 & obsdate >= (edate3 - read)][ #restrict to required date range
    , .(mcount = .N, oldest_cond = min(obsdate), recent_cond = max(obsdate))
    , by = .(patid, Colname, read)] #count by patient/condition

## now for prodcodes
presc_3 <- merge(presc, all_data[, c('patid', 'edate'), with = F]
                   , by = 'patid', all.x = T)

presc_3[, edate3 := edate %m-% months(3)] ## get 3 month prior date

#Process drug issue data to identify conditions by prodcode
drugs_3 <- presc_3[data, on = .(prodcodeid), allow.cartesian = TRUE][ #restrict to relevant prodcodes using a join
  issuedate < edate3 & issuedate >= (edate3 - read)][ #restrict to required date range
    , .(pcount = .N, oldest_cond = min(issuedate), recent_cond = max(issuedate))
    , by = .(patid, Colname, read, Rx)][ #count records by patient and condition
      pcount >= Rx] %>% #restrict using Rx field
  .[, .(patid, Colname, read, oldest_cond, recent_cond)] # required columns

## asthma needs both entries to exist to be flagged 
both_asthma_conds_end <- intersect(drugs_end$patid[drugs$Colname == 'asthma'] # patids of those with both condition and prescription
                                   , other_conds_end$patid[other_conds_end$Colname == 'asthma'])

rem_end <- other_conds_end[other_conds_end$Colname == 'asthma' & !(other_conds_end$patid %in% both_asthma_conds_end), ]

other_conds_end <- dplyr::anti_join(other_conds_end, rem_end, by = c('patid', 'Colname'))

######## Combine for results ######################################
res_3 <- data.table::rbindlist(list(cancer_3, other_conds_3
                                      , drugs_3[drugs_3$Colname != 'asthma'])
                                 , use.names = T, fill = T) %>% .[, .(patid, Colname)] %>% unique()

res_3$Colname <- gsub('_medcode|_prodcode', '', res_3$Colname)
res_3 <- unique(res_3)

res_3 <- res_3[, .N, by = patid]
names(res_3)[2] <- 'EndMLTC_3_count'

all_data <- merge(all_data, res_3, by = 'patid', all.x = T)
all_data[is.na(EndMLTC_3_count), EndMLTC_3_count := 0]

all_data[, .N, by = EndMLTC_count][order(EndMLTC_count)] ## few higher but afew lower - to be expected I think
all_data[, .N, by = EndMLTC_3_count][order(EndMLTC_3_count)] ## 
all_data[, .N, by = total][order(total)]

rm('anx', 'both', 'cancer', 'cancer_3', 'cancer_end', 'check', 'code_cats', 'codes', 'dep', 'diag_obs'
   , 'diag_obs_3', 'diag_obs_end', 'dlink', 'drugs', 'drugs_3', 'drugs_end', 'eth', 'eth_obs', 'hes'
   , 'ons_dep', 'other_conds', 'other_conds_3', 'other_conds_end', 'pats', 'prac', 'presc', 'presc_3'
   , 'presc_end', 'rem', 'rem_end', 'res', 'res_3', 'res_end', 'res2', 'resmin', 'temp')

gc()

save.image(here::here('Output', 'CodeRun.RData'))
saveRDS(all_data, file = here::here('Output', 'Pats_update.rds'))
