########################################################################
# Automated reporting of Bayesian Estimation Superceeds the T test 
# (BEST: Krushke, 2013), a bayesian t test alternative.

# Author: Ian Hussey (ian.hussey@ugent.be)
# see github.com/ianhussey/automatedreporting 

# Thanks to John Kruschke for feedback on how to report results and 
# to Mike Meredith for help with the inner workings of the BEST package.

# License: GPLv3+
# Version: 1.0

# Model to describe data
#   means of both conditions (μ1, μ2), 
#   SDs of both conditions (σ1, σ2), 
#   shared normality parameter (ν).

# Prior distribution
# Krushke (2013) decribes a specific broad/vague default prior,
# HOWEVER THE BEST PACKAGE USED HERE EMPLOYS A DIFFERENT DEFAULT PRIOR TO THAT DESCRIBED IN THE 2013 ARTICLE.
# Kruschke (2016, personal communication) argues that both are equally broad and vague.
# For each sample yi in (y1, y2), 
#   μi = normal(M = mean(yi), SD = sd(yi)*5))
#   σi = gamma(Mo = sd(yi), SD = sd(yi)*5)
#   ν = gamma(M = 30, SD = 30)

# Assumptions of script
# 1. Comparison value (compVal) between conditions = 0
# 2. ROPE is placed on effect size (rather than mean group difference)
# 3. Decision making regarding whether the effect size's HDI includes zero assumes a unimodal plot/single interval, 
# however this should be checked against the plot. 

########################################################################
# Clean the workspace

rm(list=ls())

########################################################################
## Dependencies

library(BEST)
library(dplyr)
library(reshape2)

########################################################################
# Specific data, variables, and parameters of test

# labels
DV_name                 <- "mean RTs in the derivation opportunities task"
condition_a_name        <- "the low condition"
condition_b_name        <- "the high condition"
analysis_file_name      <- "BEST - deriv opps RTs.RData"
output_file_name        <- "BEST output - deriv opps RTs.txt"
ROPE                    <- c(-0.2, 0.2)  # region of practical equivalence (ROPE) for assessing group equality.

# working directory where output will be saved
setwd("~/Git/Derivation study/Analyses/")

# Data acquisition
data_df <- 
  read.csv("~/Git/Derivation study/Data processing/processed data for analysis.csv") %>%
  filter(exclude == FALSE)  # exclude participants who met any of the three mastery criteria

# BEST test
attach(data_df)  # use the input data frame for all tests below

BEST <- BESTmcmc(deriv_opps_rt_mean[condition == "low"],  # SET THE DV AND CONDITION NAMES HERE
                 deriv_opps_rt_mean[condition == "high"],  # SET THE DV AND CONDITION NAMES HERE
                 burnInSteps = 1000,  # Increase this if convergence is insufficient
                 numSavedSteps = 1e+05,  # Increase this or thinsteps if effective sample size is insufficient
                 thinSteps = 1) 

########################################################################
# save to/read from disk

# save analysis to disk 
save(BEST, file = analysis_file_name)  

# Load previously saved analysis from disk
#load(file = analysis_file_name)

########################################################################
# tidy up output

BEST_output_df <- 
  summary(BEST, ROPEeff = ROPE) %>%
  as.data.frame() %>%  # convert to data frame for easier subsetting
  tibble::rownames_to_column() %>%  # convert rowname to column for subsetting
  dplyr::mutate(mode = round(mode, 2),  # round values and rename
                HDIlo = round(HDIlo, 2),
                HDIup = round(HDIup, 2),
                percent_greater_than_zero = round(`%>compVal`, 2),
                percent_in_rope = round(`%InROPE`, 2)) %>%
  dplyr::select(-`%InROPE`, -`%>compVal`)

########################################################################
## MCMC convergence and n.eff assessment

# convert the strings returned by print into a usable data frame. This is a bit hacky but it works.

# NB!! this is dependant on the width of the RStudio console being adequtely wide to print all columns on one row, 
# even though this printing is not shown
n_eff_strings <- 
  capture.output(print(BEST)) %>%  # capture print as variable
  as.data.frame() %>%  # convert to data frame for easier subsetting
  tibble::rownames_to_column() %>% 
  dplyr::filter(rowname > 3) %>%  # trim top and bottom rows
  dplyr::filter(rowname <= 8) %>%
  dplyr::select(-rowname)

colnames(n_eff_strings) <- "strings"

MCMC_checks <-
  reshape2::colsplit(string = n_eff_strings$strings,
                     pattern = "\\s+",  # treat one or more spaces as a column break (uses regular expressions) 
                     names = c("parameter", "mean", "sd", "median", "HDIlo", "HDIup", "Rhat", "n.eff")) %>%
  dplyr::select(parameter, Rhat, n.eff) %>%
  dplyr::mutate(Rhat_sufficient = ifelse(Rhat > 1.05, 0, 1),  # insufficient convergence if less than value
                n_eff_sufficient = ifelse(n.eff <= 10000, 0, 1)) %>%  # insufficient effective sample size if less than value
  dplyr::summarize(Rhat_sufficient = as.logical(min(Rhat_sufficient)),
                   n_eff_sufficient = as.logical(min(n_eff_sufficient)))

if(is.na(MCMC_checks[1,1]) | is.na(MCMC_checks[1,2])) print("************** \n ERROR: the console width is to narrow to print the results correctly! \n **************")

########################################################################
# View results

# full output
BEST_output_df

# plot
plotAll(BEST, ROPEeff = ROPE, showCurve = TRUE)

########################################################################
## extract individual variables for easier printing

MCMC_convergence        <- MCMC_checks$Rhat_sufficient
MCMC_effective_n        <- MCMC_checks$n_eff_sufficient
es_mode                 <- BEST_output_df %>% filter(rowname == "effSz") %>% .$mode  
es_hdi_low              <- BEST_output_df %>% filter(rowname == "effSz") %>% .$HDIlo  
es_hdi_high             <- BEST_output_df %>% filter(rowname == "effSz") %>% .$HDIup 
es_in_rope              <- BEST_output_df %>% filter(rowname == "effSz") %>% .$percent_in_rope
m_condition_a           <- BEST_output_df %>% filter(rowname == "mu1") %>% .$mean
m_condition_a           <- round(m_condition_a, 2)
m_condition_b           <- BEST_output_df %>% filter(rowname == "mu2") %>% .$mean
m_condition_b           <- round(m_condition_b, 2)

########################################################################
# construct strings from output

# MCMC convergence
MCMC_checks_string      <- ifelse(MCMC_convergence == FALSE, 
                                  "The MCMC chains did not converge well. NB 'burnInSteps' SHOULD BE INCREASED AND THE TEST RE-RUN.",
                                  ifelse(MCMC_effective_n == FALSE,
                                         "The effective sample size was insufficient for one or more parameter. NB 'numSavedSteps' OR 'thinSteps' SHOULD BE INCREASED AND THE TEST RE-RUN.",
                                         "The MCMC chains converged well and had an effective sample size (ESS) greater than 10,000 for all parameters."))

# interpret effect size based on Cohen's (1988) guidelines
es_size                 <- ifelse(abs(es_mode) < 0.2, "negligable", 
                                       ifelse(abs(es_mode) < 0.5, "small", 
                                              ifelse(abs(es_mode) < 0.8, "medium", "large"))) 

# assess if >=95% of credible es are inside the ROPE
equality_boolean        <- ifelse(es_in_rope >= 95, 1, 0)

# assess if the 95% HDI includes the zero point
es_hid_includes_zero    <- ifelse((es_hdi_low * es_hdi_high) < 0,  # if the product of the number is negative then one is positive and one is negative, therefore the interval contains zero. Otherwise, it does not.
                                  "included zero",
                                  "did not include zero")

# Assess 3 way decision path based on equality and differences booleans to make a final conclusion
conclusions             <- ifelse(equality_boolean == 1,  # NB even if differences==1 here, effect is still so small as to consider groups equal.
                                  "Given that more than 95% of estimated effect sizes fell within the ROPE, the posterior distribution therefore indicated that the groups were credibly equal. ",
                                  ifelse(es_hid_includes_zero == "did not include zero",
                                         "Given that less than 95% of estimated effect sizes fell within the ROPE and the 95% RDI did not include zero, the posterior distribution therefore indicated that credible differences existed between the groups. ",
                                         "Although the 95% HDI included zero, less than 95% of estimated effect sizes within the ROPE. As such, the posterior distribution indicated that there was great uncertainty about the magnitude of difference between the two conditions, which were neither credibly different nor credibly equal. "))

########################################################################
# combine all output into a natural langauge string

BEST_parameters         <- sprintf("Bayesian analysis (Kruschke, 2013) was used to compare differences in %s between %s and %s. The analysis accommodated the possibility of outliers by using t distributions to describe the data, and allowed for different variances across the groups. Specifically, the model employed 5 parameters to describe the data: the means of both conditions (μ1, μ2), the standard deviations of both conditions (σ1, σ2), and a shared normality parameter (ν). We employed the default prior, which is a noncommittal prior intended to have minimal impact on the posterior distribution. Specifically, for sample yi in (y1, y2), μi = normal(M = mean(yi), SD = sd(yi)*5)), σi = gamma(Mo = sd(yi), SD = sd(yi)*5), ν = gamma(M = 30, SD = 30). The posterior distribution was represented by Markov Chain Monte Carlo (MCMC) simulation methods (see Kruschke, 2013). For decision-making purposes, a region of practical equivalence (ROPE: Kruschke, 2011) for negligible effect size was defined (-0.2 < d < 0.2; Cohen, 1988). ",
                                   DV_name,
                                   condition_a_name, 
                                   condition_b_name)

BEST_text               <- sprintf("%s The posterior distributions showed the modal estimate of the %s was %s for %s and %s for %s. The modal estimated effect size was %s (Cohen, 1988) with a 95%% Highest Density Interval that %s, Mo d = %s, 95%% HDI [%s, %s]. %s %% of estimated effect sizes fell within the ROPE. %s", 
                                   MCMC_checks_string,
                                   DV_name,
                                   m_condition_a,
                                   condition_a_name,
                                   m_condition_b,
                                   condition_b_name,
                                   es_size,
                                   es_hid_includes_zero, 
                                   es_mode, 
                                   es_hdi_low, 
                                   es_hdi_high,
                                   es_in_rope,
                                   conclusions)

########################################################################
# write data to disk

sink(output_file_name)
cat(BEST_parameters)
cat("\n\n")
cat(BEST_text)
sink()


