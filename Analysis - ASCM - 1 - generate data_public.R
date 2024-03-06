require(tidyverse); require(magrittr); require(readxl)
require(augsynth); require(ggplot2); require(cowplot)

setwd('C:/Users/schraw/Documents/DPR/data/')

#' Counties with DPR water during some portion of the study.
dpr.counties <- c('068', '114', '165', '208')

load("phewas.cases.v20220518.rdata")
load("phewas.births.v20220518.rdata")

#' Assign DPR exposure status in births and cases files based on estimated date of conception.
#' Individuals whose EDC is later than 02/06/2013 were <= 12 weeks gestational age on 05/01/2013, when we will assume DPR went online.
cases %<>% 
  filter(bpa4 == '888.8') %>% 
  mutate(county = as.factor(county),
         birthyear = ifelse(birthyear == 2013 & birthhalf == 'Fall-Winter', 2013.5, birthyear),
         birthhalf = as.factor(birthhalf))

#' Note no cases in Loving County (#151). Drop it. 
births %<>% 
  filter(county != '151') %>% 
  mutate(county = as.factor(county),
         birthyear = ifelse(birthyear == 2013 & birthhalf == 'Fall-Winter', 2013.5, birthyear),
         birthhalf = as.factor(birthhalf),
         high.school = factor(ifelse(edug == '< High school', 0, 1), labels = c('No', 'Yes')),
         hispanic = factor(ifelse(ethrace == 'Hispanic', 1, 
                                  ifelse(ethrace == 'Unknown', NA, 0)),
                           labels = c('No','Yes')),
         black = factor(ifelse(ethrace == 'Black non-Hispanic', 1, 
                               ifelse(ethrace == 'Unknown', NA, 0)),
                        labels = c('No', "Yes")))

#' Count cases and births by county, year, and half of year.
case.count <- cases %>% 
  count(county, birthyear, .drop = F) %>% 
  rename(n.cases = n) 

birth.count <- births %>% 
  count(county, birthyear, .drop = F) %>%
  rename(n.births = n)

texas.counts <- left_join(birth.count, case.count, by = c('county','birthyear')) %>% 
  mutate(n.cases = ifelse(is.na(n.cases), 0, n.cases),
         dpr = ifelse(county %in% dpr.counties & birthyear %in% c(2013.5,2014:2017), 1, 0),      
         cases.per.10k = n.cases/(n.births/10000))

#' Append RUCC.
counties <- select(births, county, rucc) %>% filter(!duplicated(county))

texas.counts %<>% left_join(counties, by = 'county')

#' Data frames containing information on covariates.
age <- aggregate(age ~ county + birthyear, data = births, median)

education <- births %>% 
  mutate(birthyear = as.factor(birthyear)) %>% 
  count(high.school, county, birthyear, .drop=FALSE) %>% 
  filter(high.school == 'No') %>% 
  rename(n.no.high.school = n) %>% 
  mutate(birthyear = as.numeric(as.character(birthyear)))

hispanic <- births %>% 
  mutate(birthyear = as.factor(birthyear)) %>% 
  count(hispanic, county, birthyear, .drop=FALSE) %>% 
  filter(hispanic == 'Yes') %>% 
  rename(n.hispanic = n) %>% 
  mutate(birthyear = as.numeric(as.character(birthyear)))

black <- births %>% 
  mutate(birthyear = as.factor(birthyear)) %>% 
  count(black, county, birthyear, .drop=FALSE) %>% 
  filter(black == 'Yes') %>% 
  rename(n.black = n) %>% 
  mutate(birthyear = as.numeric(as.character(birthyear)))

#' Join them to the birth/case counts.
texas.counts %<>% 
  left_join(age, by = c('county','birthyear')) %>% 
  left_join(education, by = c('county','birthyear')) %>% 
  left_join(hispanic, by = c('county','birthyear')) %>% 
  left_join(black, by = c('county','birthyear')) %>% 
  mutate(pct.no.hs = (n.no.high.school/n.births)*100,
         pct.hisp = (n.hispanic/n.births)*100,
         pct.black = (n.black/n.births)*100)

small.counties <- texas.counts %>% 
  filter(n.births < 25, !duplicated(county)) %>% 
  pull(county)

texas.counts %<>% filter(!county %in% small.counties)

saveRDS(texas.counts, 
        file = 'C:/Users/schraw/Documents/DPR/data/direct.potable.reuse.data.v20220616.rds')

rm(list = ls())

# Create CHD data ---------------------------------------------------------

load("phewas.cases.v20220518.rdata")
load("phewas.births.v20220518.rdata")

#' Counties with DPR water during some portion of the study.
dpr.counties <- c('068', '114', '165', '208')

#' Assign DPR exposure status in CHD cases based on estimated date of conception.
#' Individuals whose EDC is later than 02/06/2013 were <= 12 weeks gestational age on 05/01/2013, when we will assume DPR went online.
chd <- cases  %>%  
  filter(str_detect(bpa4, '74[567]')) %>% 
  mutate(county = as.factor(county),
         birthyear = ifelse(birthyear == 2013 & birthhalf == 'Fall-Winter', 2013.5, birthyear),
         birthhalf = as.factor(birthhalf))

#' Count cases by county, year, and half of year.
chd.count <- chd %>% 
  count(county, birthyear, .drop = F) %>% 
  rename(n.cases = n) 

#' Note no cases in Loving County (#151). Drop it. 
births %<>% 
  filter(county != '151') %>% 
  mutate(county = as.factor(county),
         birthyear = ifelse(birthyear == 2013 & birthhalf == 'Fall-Winter', 2013.5, birthyear),
         birthhalf = as.factor(birthhalf),
         high.school = factor(ifelse(edug == '< High school', 0, 1), labels = c('No', 'Yes')),
         hispanic = factor(ifelse(ethrace == 'Hispanic', 1, 
                                  ifelse(ethrace == 'Unknown', NA, 0)),
                           labels = c('No','Yes')),
         black = factor(ifelse(ethrace == 'Black non-Hispanic', 1, 
                               ifelse(ethrace == 'Unknown', NA, 0)),
                        labels = c('No', "Yes")))

birth.count <- births %>% 
  count(county, birthyear, .drop = F) %>%
  rename(n.births = n)

chd.data <- left_join(birth.count, chd.count, by = c('county','birthyear')) %>% 
  mutate(n.cases = ifelse(is.na(n.cases), 0, n.cases),
         dpr = ifelse(county %in% dpr.counties & birthyear %in% c(2013.5,2014:2017), 1, 0),      
         cases.per.10k = n.cases/(n.births/10000))

#' Append RUCC.
rucc.mapping <- select(births, county, rucc) %>% filter(!duplicated(county))

chd.data <- chd.data %>% left_join(rucc.mapping, by = 'county')

#' Join data frames containing information on covariates to the birth/case counts.
chd.data <- chd.data %>% 
  left_join(age, by = c('county','birthyear')) %>% 
  left_join(education, by = c('county','birthyear')) %>% 
  left_join(hispanic, by = c('county','birthyear')) %>% 
  left_join(black, by = c('county','birthyear')) %>% 
  mutate(pct.no.hs = (n.no.high.school/n.births)*100,
         pct.hisp = (n.hispanic/n.births)*100,
         pct.black = (n.black/n.births)*100,
         ruccfinal = as.numeric(substr(rucc, 1, 1)))

small.counties <- chd.data %>% 
  filter(n.births < 25, !duplicated(county)) %>% 
  pull(county)

chd.data <- chd.data %>% 
  filter(!county %in% small.counties)

saveRDS(chd.data, 
        file = 'C:/Users/schraw/Documents/DPR/data/direct.potable.reuse.CHD.data.20240129.rds')

rm(list = ls())

# Create NTD data ---------------------------------------------------------

load("phewas.cases.v20220518.rdata")
load("phewas.births.v20220518.rdata")

#' Counties with DPR water during some portion of the study.
dpr.counties <- c('068', '114', '165', '208')

#' Assign DPR exposure status in CHD cases based on estimated date of conception.
#' Individuals whose EDC is later than 02/06/2013 were <= 12 weeks gestational age on 05/01/2013, when we will assume DPR went online.
ntd <- cases  %>%  
  filter(str_detect(bpa4, str_c('740.0', '741', '742.0', sep = '|'))) %>% #' Includes anencephalus, spina bifida, and encephalocele.
  mutate(county = as.factor(county),
         birthyear = ifelse(birthyear == 2013 & birthhalf == 'Fall-Winter', 2013.5, birthyear),
         birthhalf = as.factor(birthhalf))

#' Count cases by county, year, and half of year.
ntd.count <- ntd %>% 
  count(county, birthyear, .drop = F) %>% 
  rename(n.cases = n) 

#' Note no cases in Loving County (#151). Drop it. 
births %<>% 
  filter(county != '151') %>% 
  mutate(county = as.factor(county),
         birthyear = ifelse(birthyear == 2013 & birthhalf == 'Fall-Winter', 2013.5, birthyear),
         birthhalf = as.factor(birthhalf),
         high.school = factor(ifelse(edug == '< High school', 0, 1), labels = c('No', 'Yes')),
         hispanic = factor(ifelse(ethrace == 'Hispanic', 1, 
                                  ifelse(ethrace == 'Unknown', NA, 0)),
                           labels = c('No','Yes')),
         black = factor(ifelse(ethrace == 'Black non-Hispanic', 1, 
                               ifelse(ethrace == 'Unknown', NA, 0)),
                        labels = c('No', "Yes")))

birth.count <- births %>% 
  count(county, birthyear, .drop = F) %>%
  rename(n.births = n)

ntd.data <- left_join(birth.count, ntd.count, by = c('county','birthyear')) %>% 
  mutate(n.cases = ifelse(is.na(n.cases), 0, n.cases),
         dpr = ifelse(county %in% dpr.counties & birthyear %in% c(2013.5,2014:2017), 1, 0),      
         cases.per.10k = n.cases/(n.births/10000))

#' Append RUCC.
rucc.mapping <- select(births, county, rucc) %>% filter(!duplicated(county))

ntd.data <- ntd.data %>% left_join(rucc.mapping, by = 'county')

#' Join data frames containing information on covariates to the birth/case counts.
ntd.data <- ntd.data %>% 
  left_join(age, by = c('county','birthyear')) %>% 
  left_join(education, by = c('county','birthyear')) %>% 
  left_join(hispanic, by = c('county','birthyear')) %>% 
  left_join(black, by = c('county','birthyear')) %>% 
  mutate(pct.no.hs = (n.no.high.school/n.births)*100,
         pct.hisp = (n.hispanic/n.births)*100,
         pct.black = (n.black/n.births)*100,
         ruccfinal = as.numeric(substr(rucc, 1, 1)))

small.counties <- ntd.data %>% 
  filter(n.births < 25, !duplicated(county)) %>% 
  pull(county)

ntd.data <- ntd.data %>% 
  filter(!county %in% small.counties)

saveRDS(ntd.data, 
        file = 'C:/Users/schraw/Documents/DPR/data/direct.potable.reuse.NTD.data.20240129.rds')

rm(list = ls())

