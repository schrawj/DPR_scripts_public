require(tidyverse); require(magrittr); require(tictoc)
require(cowplot); require(readxl)

counts <- readRDS('C:/Users/schraw/Documents/DPR/data/DPR_counts_data_20230504.rds')

# Install/load augsynth ---------------------------------------------------

#' Rtools is required. Can download Rtools installer from CRAN.
#require(devtools)
#install_github('ebenmichael/augsynth')

require(augsynth)

# Evaluate covariate-BD associations --------------------------------------

covs <- c('ruccfinal', 'age', 'pct.no.hs', 'pct.hisp', 'pct.black')

for (i in covs){
  
  plot.data <- counts %>% 
    select(cases.per.10k, all_of(i)) 
  
  names(plot.data) <- c('case.count', 'exposure')
  
  cor.test(plot.data$case.count, plot.data$exposure, method = 'pearson') %>% print()
  
  plot <- ggplot(data = plot.data, aes(x = exposure, y = case.count)) +
    
    geom_point() + 
    
    geom_smooth(method = 'lm') +
    
    geom_smooth() +
    
    labs(x = i)
  
  print(plot)
  
}

# ASCM model --------------------------------------------------------------

counts <- counts %>% 
  mutate(birthyear = as.numeric(as.character(birthyear)),
         county = as.character(county),
         birthtime = ifelse(birthhalf == 'Spring-Summer', birthyear, birthyear + 0.5)) %>% 
  filter(birthyear >= 2003) #' limit analysis period to 10 years prior to DPR start time

#' Include if reporting results from the sensitivity analysis excluding Wichita County, a portion of which was briefly supplied with DPR-treated
#' water from 2014-2015.
#counts <- counts %>% 
#  filter(county != 'Wichita')

#' Ridge augmented SCM model with auxiliary covariates.
asyn <- augsynth(cases.per.10k ~ dpr, unit = county, time = birthtime, data = counts, progfunc = 'none')

covsyn <- augsynth(cases.per.10k ~ dpr | ruccfinal + age + pct.no.hs + pct.hisp + pct.black,
                   county, birthtime, counts, progfunc = 'Ridge', scm = T, fixedeff = T)

#' Same model, with a different approach to choosing lambda (the ridge hyperparameter).
#' min_1se = FALSE selects the lambda that minimizes the cross-validation MSE, rather than
#' the maximal lambda with MSE within one SD of the minimal MSE.
covsyn.min <- augsynth(cases.per.10k ~ dpr | ruccfinal + age + pct.no.hs + pct.hisp + pct.black,
                       county, birthtime, counts, progfunc = 'Ridge', scm = T, fixedeff = T, min_1se = FALSE)

#' A one-way hypothesis test
summary(covsyn, stat_func = function(x) - sum(x))
summary(covsyn.min, stat_func = function(x) - sum(x))

setwd('C:/Users/schraw/Documents/DPR/data/')

saveRDS(asyn,
        'ASCM_crude_model_20230505.rds')

saveRDS(covsyn,
        paste0('ASCM_adjusted_model_', format(Sys.Date(), '%Y%m%d'),'.rds'))

saveRDS(covsyn.min,
        'ASCM_adjusted_model_min_lambda_20230505.rds')

# Plot ASCM results -------------------------------------------------------

asyn <- readRDS('ASCM_crude_model_20230505.rds')
covsyn <- readRDS('ASCM_adjusted_model_20230815.rds')
covsyn.min <- readRDS('ASCM_adjusted_model_min_lambda_20230505.rds')

asyn.plot <- plot(asyn) + 
  
  labs(x = 'Year',
       y = 'Change in birth defects prevalence (per 10,000 live births)') + 
  
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size =  14, face = 'bold'))
asyn.plot

covsyn.plot <- plot(covsyn) + 
  
  labs(x = 'Year',
       y = 'Change in birth defects prevalence (per 10,000 live births)') + 
  
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size =  14, face = 'bold'))
covsyn.plot

svg('C:/Users/schraw/Documents/DPR/R_outputs/ascm.estimates.v20230815.svg', 8, 8)

covsyn.plot

dev.off()

covsyn.min.plot <- plot(covsyn.min) + 
  
  labs(x = 'Year',
       y = 'Change in birth defects prevalence (per 10,000 live births)')
covsyn.min.plot

plot(covsyn, inf_type = 'jackknife+')
plot(covsyn, cv = T)

# Compare observed to simulated data --------------------------------------

setwd('//smb-main.ad.bcm.edu/genepi2/Old_genepi2/Jeremy/PheWAS/DPR/')

model <- readRDS('Datasets/ASCM_adjusted_model_20230815.rds')

model.summary <- summary(model)

att.est <- model.summary$average_att['Estimate'] %>% unlist()

att.p <- model.summary$average_att['p_val'] %>% unlist()

simulated.atts <- readRDS('Datasets/ASCM_placebo_studies_20230816.rds')

att.plot <- ggplot(data = simulated.atts, aes(x = Estimate)) +
  
  geom_histogram(bins = 25, fill = 'gray50', color = 'red') + 
  
  geom_vline(xintercept = att.est, linetype = 'dashed') + 
  
  theme_minimal() +
  
  theme(panel.grid = element_blank()) + 
  
  labs(x = 'Average ATT Estimate (N=1,000 Simulations)',
       y = 'Count')

att.plot

p.plot <- ggplot(data = simulated.atts, aes(x = p_val)) +
  
  geom_histogram(bins = 25, fill = 'gray50', color = 'red') + 
  
  geom_vline(xintercept = att.p, linetype = 'dashed') + 
  
  theme_minimal() +
  
  theme(panel.grid = element_blank()) +
  
  labs(x = 'P-values for Average ATT Estimates (N=1,000 Simulations)',
       y = 'Count')

p.plot

svg('R outputs/ascm.simulations.att.parameters.v20220715.svg', width = 10, height = 8)

plot_grid(att.plot, p.plot, 
          labels = c('A: ATT Estimates', 'B: P-values for ATT Estimates'))

dev.off()

# Iterate over lambda values ----------------------------------------------

counts <- readRDS('C:/Users/schraw/Documents/DPR/data/DPR_counts_data_20230504.rds')

counts <- counts %>% 
  mutate(birthyear = as.numeric(as.character(birthyear)),
         county = as.character(county),
         birthtime = ifelse(birthhalf == 'Spring-Summer', birthyear, birthyear + 0.5)) %>% 
  filter(birthyear >= 2003) #' Limit analysis period to 10 years prior to DPR start time

lambdas <- c(0, 0.5,5,10,50,100,1000,10000, 100000, 1000000, 10000000, 100000000)

models <- tibble()

plots <- vector('list', length(lambdas))

for (i in seq_along(lambdas)){
  
  print(paste('lambda =', lambdas[i]))
  
  covsyn <- augsynth(cases.per.10k ~ dpr | ruccfinal + age + pct.no.hs + pct.hisp + pct.black,
                     county, birthtime, counts, progfunc = 'Ridge', scm = T, lambda = lambdas[i])
  
  sum.cov <- summary(covsyn)
  
  plot <- plot(covsyn) + labs(title = paste0('lambda=',lambdas[i]))
  
  plots[[i]] <- plot
  
  new.model <- tibble(lambda = lambdas[i],
                      
                      scaled.l2.imbalance = covsyn$scaled_l2_imbalance,
                      scaled.covariate.l2.imabalance = covsyn$scaled_covariate_l2_imbalance,
                      mean.bias = mean(sum.cov$bias_est),
                      
                      avg.att.estimate = as.numeric(sum.cov$average_att[1]),
                      att.pval = as.numeric(sum.cov$average_att[5]))
  
  models <- rbind(models, new.model)
  
}

saveRDS(models, 
        'C:/Users/schraw/Documents/DPR/R_outputs/ascm_model_fit_by_lambda_20230714.rds')

names(models) <- c(names(models)[1:4], 'att.estimate', 'att.p.value')

models <- models %>% 
  rename(estimate = `avg.att.estimate$Estimate`,
         att.p.value = `att.pval$p_val`)

write_csv(models,
          'C:/Users/schraw/Documents/DPR/R_outputs/ascm_model_fit_by_lambda_20230620.csv')

svg('C:/Users/schraw/Documents/DPR/R_outputs/ascm_model_fit_by_lambda_20230714.svg', height = 12, width = 9)

plot_grid(plots[[1]], plots[[2]], plots[[3]], 
          plots[[4]], plots[[5]], plots[[6]], 
          plots[[7]], plots[[8]], plots[[9]], 
          plots[[10]], plots[[11]], plots[[12]], 
          ncol = 3)

dev.off()

# Plot annual estimates for simulations and real data ---------------------

setwd('C:/Users/schraw/Documents/DPR/')

model <- readRDS('data/ASCM_adjusted_model_20230815.rds')

model.summary <- summary(model)

simulations <- readRDS('data/ASCM_placebo_studies_annualized_ATT_estimates_20230816.rds')
simulated.average.att <- readRDS('data/ASCM_placebo_studies_20230816.rds')

#' Proportion of simulation studies that returned an ATT at least as extreme as the observed.
table(abs(simulated.average.att$Estimate) > model.summary$average_att$Estimate)

#' Transform data for plotting.
for (i in 1:length(simulations)){
  
  simulations[[i]] <- simulations[[i]]$att
  
  simulations[[i]]$iteration <- i
  
}

simulations <- do.call(rbind.data.frame, simulations)

#' Bind in new covsyn annualized estimates.
model.summary$att <- model.summary$att %>% 
  mutate(iteration = 1001)

plot.data <- bind_rows(model.summary$att, simulations) %>% 
  mutate(iteration = factor(iteration))

plot <- ggplot(data = plot.data, aes(x=Time, y = Estimate, color = iteration, alpha = iteration, size = iteration)) +
  
  geom_line() + 
  
  geom_vline(xintercept = 2013.5, linetype = 'dashed') +
  
  theme_classic() +
  
  labs(x = 'Year', y = 'Change in birth defects prevalence (per 10,000 livebirths)') +
  
  scale_color_manual(values = c(rep('gray75', 1000), 'red')) + 
  
  scale_alpha_manual(values = c(rep(0.1, 1000), 1)) +
  
  scale_size_manual(values = c(rep(1, 1000), 1.5)) +
  
  theme(axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 12, face = 'bold'),
        
        legend.position = 'none')

plot

svg(paste0('R_outputs//ASCM_placebo_study_att_estimates_', format(Sys.Date(), '%Y%m%d'), '.svg'), height = 9, width = 16)

print(plot)

dev.off()

# ASCM models for CHD -----------------------------------------------------

#' CHD case counts and prevalence estimates by county and year.
chd <- readRDS('data/direct.potable.reuse.CHD.data.20240129.rds')

chd <- chd %>% 
  mutate(birthyear = as.numeric(as.character(birthyear))) %>% 
  filter(birthyear >= 2003) #' Kara suggests limiting analysis period to 10 years prior to DPR start time

#' Include if reporting results from the sensitivity analysis excluding Wichita County, a portion of which was briefly supplied with DPR-treated
#' water from 2014-2015.
#chd <- chd %>% 
#  filter(county != 243)

#' Ridge augmented SCM model with auxiliary covariates.
asyn.chd <- augsynth(cases.per.10k ~ dpr | rucc + age + pct.no.hs + pct.hisp + pct.black, 
                     unit = county, time = birthyear, data = chd, progfunc = 'none')

covsyn.chd <- augsynth(cases.per.10k ~ dpr | rucc + age + pct.no.hs + pct.hisp + pct.black,
                       county, birthyear, chd, progfunc = 'Ridge', scm = T, fixedeff = T)

#' Same model, with a different approach to choosing lambda (the ridge hyperparameter).
#' min_1se = FALSE selects the lambda that minimizes the cross-validation MSE, rather than
#' the maximal lambda with MSE within one SD of the minimal MSE.
covsyn.min <- augsynth(cases.per.10k ~ dpr | rucc + age + pct.no.hs + pct.hisp + pct.black,
                       county, birthyear, chd, progfunc = 'Ridge', scm = T, fixedeff = T, min_1se = FALSE)
summary(covsyn.min)

#' Plot the SCM and Ridge ASCM results
asyn.plot <- plot(asyn.chd) + 
  
  labs(x = 'Year',
       y = 'Change in CHD prevalence (per 10,000 live births)') + 
  
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size =  14, face = 'bold'))
asyn.plot

svg('R_outputs/scm.CHD.estimates.v20240208.svg', 8, 8)

asyn.plot

dev.off()

covsyn.plot <- plot(covsyn.chd) + 
  
  labs(x = 'Year',
       y = 'Change in CHD prevalence (per 10,000 live births)') + 
  
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size =  14, face = 'bold'))
covsyn.plot

svg('R_outputs/ascm.CHD.estimates.v20240131.svg', 8, 8)

covsyn.plot

dev.off()

# ASCM models for NTDs ----------------------------------------------------

ntd <- readRDS('data/direct.potable.reuse.NTD.data.20240129.rds')

ntd <- ntd %>% 
  mutate(birthyear = as.numeric(as.character(birthyear))) %>% 
  filter(birthyear >= 2003) #' Limit analysis period to 10 years prior to DPR start time

#' Include if reporting results from the sensitivity analysis excluding Wichita County, a portion of which was briefly supplied with DPR-treated
#' water from 2014-2015.
#ntd <- ntd %>% 
#  filter(county != 243)

#' Ridge augmented SCM model with auxiliary covariates.
asyn.ntd <- augsynth(cases.per.10k ~ dpr | rucc + age + pct.no.hs + pct.hisp + pct.black, 
                     unit = county, time = birthyear, data = ntd, progfunc = 'none')

covsyn.ntd <- augsynth(cases.per.10k ~ dpr | rucc + age + pct.no.hs + pct.hisp + pct.black,
                   county, birthyear, ntd, progfunc = 'Ridge', scm = T, fixedeff = T)

#' Same model, with a different approach to choosing lambda (the ridge hyperparameter).
#' min_1se = FALSE selects the lambda that minimizes the cross-validation MSE, rather than
#' the maximal lambda with MSE within one SD of the minimal MSE.
covsyn.min <- augsynth(cases.per.10k ~ dpr | rucc + age + pct.no.hs + pct.hisp + pct.black,
                       county, birthyear, ntd, progfunc = 'Ridge', scm = T, fixedeff = T, min_1se = FALSE)

#' A one-way hypothesis test
summary(covsyn.ntd, stat_func = function(x) - sum(x))
summary(covsyn.min, stat_func = function(x) - sum(x))

#' Plot the SCM and Ridge ASCM results
asyn.plot <- plot(asyn.ntd) + 
  
  labs(x = 'Year',
       y = 'Change in NTD prevalence (per 10,000 live births)') + 
  
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size =  15, face = 'bold'))
asyn.plot

svg('R_outputs/scm.NTD.estimates.v20240208.svg', 8, 8)

asyn.plot

dev.off()

covsyn.plot <- plot(covsyn.ntd) + 
  
  labs(x = 'Year',
       y = 'Change in NTD prevalence (per 10,000 live births)') + 
  
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size =  14, face = 'bold'))
covsyn.plot

svg('R_outputs/ascm.NTD.estimates.v20240131.svg', 8, 8)

covsyn.plot

dev.off()

rm(list = ls()); gc()

# Run placebo studies -----------------------------------------------------

#' CHD case counts and prevalence estimates by county and year.
counts <- readRDS('data/direct.potable.reuse.CHD.data.20240129.rds')
#counts <- readRDS('data/direct.potable.reuse.NTD.data.20240129.rds')

outcome <- 'CHD'

i <- 1

#' Maximum number of placebo studies to perform.
max <- 1000

#' Vector of the names of treated and untreated counties.
treated.units <- c('Ector','Midland','Howard','Scurry')
placebo.units <- counts %>% pull(county) %>% unique() %>% subset(!. %in% treated.units)

#' Initialize objects to hold placebo study results.
model.info <- data.frame()

annual.att.estimates <- vector('list', length = max)

#' This may take a full workday or so.
while (i < (max + 1) ){
  
  print(i)
  
  #' Set the DPR variable to untreated for all counties, and generate a uniform random variable to use when assigning placebo units as treated.
  my.data <- counts %>% 
    mutate(dpr = 0,
           runif = runif(nrow(.)))
  
  #' Choose four untreated counties for the simulation study.
  treated <- my.data %>% 
    filter(county %in% placebo.units) %>% 
    arrange(runif) %>% 
    slice(1:4) %>% 
    pull(county)
  
  #' Update the DPR variable accordingly.
  my.data <- my.data %>% 
    mutate(dpr = ifelse(county %in% treated & birthyear > 2013, 1, 0))
  
  #' Ridge augmented SCM model for an imaginary outcome with a similar distribution and the same auxiliary covariates.
  #' A sort of sanity check.
  asyn <- augsynth(cases.per.10k ~ dpr | rucc + age + pct.no.hs + pct.hisp + pct.black,
                   county, birthyear, 
                   my.data, 
                   progfunc = 'none', 
                   scm = T,
                   fixedeff = T)
  
  asyn.summary <- summary(asyn)
  
  annual.att.estimates[[i]] <- asyn.summary
  
  new.model <- data.frame(iteration = i,
                          att.estimate = asyn.summary$average_att['Estimate'],
                          att.pval = asyn.summary$average_att['p_val'],
                          l2.imbalance = asyn$l2_imbalance,
                          avg.estimated.bias = mean(asyn.summary$bias_est[,1])
  )
  
  model.info <- rbind(model.info, new.model)
  
  i <- i + 1
  
}

saveRDS(model.info,
        paste0('data/ASCM_placebo_studies_', outcome, '_', format(Sys.Date(), '%Y%m%d'),'.rds'))

saveRDS(annual.att.estimates,
        paste0('data/ASCM_placebo_studies_annualized_ATT_estimates_', outcome, '_', format(Sys.Date(), '%Y%m%d'), '.rds'))

rm(list = ls()); gc()
