###################################################################
# Automated reporting of Bayesian Estimation Superceeds the Ttest 
# (BEST: Krushke, 2013), a bayesian t test alternative.
# see http://sumsar.net/blog/2014/02/bayesian-first-aid-two-sample-t-test/

# Author: Ian Hussey (ian.hussey@ugent.be)
# see github.com/ianhussey/automatedreporting 

# Model to describe data
#   means of both conditions (μ1, μ2), 
#   SDs of both conditions (σ1, σ2), 
#   shared normality parameter (ν).

# Prior
# Krushke (2013) decribes a specific broad/vague default prior, 
# HOWEVER THE BEST PACKAGE USED HERE EMPLOYS A DIFFERENT DEFAULT PRIOR TO THAT DESCRIBED IN THE 2013 ARTICLE.
# Kruschke (2016, personal communication) argues that both are equally broad and vague.
# For each sample yi in (y1, y2), 
#   μi = normal(M = mean(yi), SD = sd(yi)*5))
#   σi = gamma(Mo = sd(yi), SD = sd(yi)*5)
#   ν = gamma(M = 30, SD = 30)

# Assumptions of script:
# 1. Comparison value (compVal) = 0
# 2. ROPE is placed on effect size (e.g., rather than mean difference)

###################################################################
# Clean the workspace

rm(list=ls())

###################################################################
## Dependencies

library(BEST)
library(dplyr)

###################################################################
# Specific data, variables, and parameters of test

DV_name                 <- "mean RTs in the derivation opportunities task"
condition_a_name        <- "the low condition"
condition_b_name        <- "the high condition"
analysis_file_name      <- "BEST - deriv opps RTs.RData"
output_file_name        <- "BEST output - deriv opps RTs.txt"
ROPE                    <- c(-0.2, 0.2)  # region of practical equivalence (ROPE) for assessing group equality.

# working directory where output will be saved
setwd("/Users/Ian/Dropbox/Work/Studies/DCC work/DCC Article 2 - Derivation/Experiments 2 - Ian and Sean/OSF files/5 Analyses/")

# Data acquisition and exclusion of failers
data_df <- 
  read.csv("/Users/Ian/Dropbox/Work/Studies/DCC work/DCC Article 2 - Derivation/Experiments 2 - Ian and Sean/OSF files/4 Data processing/processed data for analysis.csv") %>%
  filter(exclude == FALSE)  # exclude participants who met any of the three mastery criteria

# BEST test
attach(data_df)  # use the input data frame for all tests below

BEST <- BESTmcmc(deriv_opps_rt_mean[condition == 1],  # SET THE DV AND CONDITION NAMES HERE
                 deriv_opps_rt_mean[condition == 2])

########################################################################
# save to/read from disk

# save analysis to disk 
save(BEST, file = analysis_file_name)  

# Load previously saved analysis from disk
#load(file = file_name)

########################################################################
# tidy up output

BEST_output_df <- 
  summary(BEST, 
          ROPEeff = ROPE) %>%
  as.data.frame() %>%  # convert to data frame for easier subsetting
  tibble::rownames_to_column() %>%  # convert rowname to column for subsetting
  mutate(mode = round(mode, 2),  # round values and rename awkwardly named ones
         HDIlo = round(HDIlo, 2),
         HDIup = round(HDIup, 2),
         percent_greater_than_zero = round(`%>compVal`, 2),
         percent_in_rope = round(`%InROPE`, 2)) %>%
  select(-`%InROPE`, -`%>compVal`)

########################################################################
# View results

# full output
BEST_output_df

# plot
plotAll(BEST, ROPEeff = ROPE, showCurve = TRUE)

########################################################################
## extract individual variables for easier printing

es_mode                 <- BEST_output_df %>% filter(rowname == "effSz") %>% .$mode  
es_hdi_low              <- BEST_output_df %>% filter(rowname == "effSz") %>% .$HDIlo  
es_hdi_high             <- BEST_output_df %>% filter(rowname == "effSz") %>% .$HDIup  
es_greater_zero         <- BEST_output_df %>% filter(rowname == "effSz") %>% .$percent_greater_than_zero  
es_in_rope              <- BEST_output_df %>% filter(rowname == "effSz") %>% .$percent_in_rope  
m_condition_a           <- BEST_output_df %>% filter(rowname == "mu1") %>% .$mean
m_condition_a           <- round(m_condition_a, 2)
m_condition_b           <- BEST_output_df %>% filter(rowname == "mu2") %>% .$mean
m_condition_b           <- round(m_condition_b, 2)

########################################################################
# construct strings from output

# interpret effect size based on Cohen's (1988) guidelines
es_size                 <- ifelse(abs(es_mode) < 0.2, "negligable", 
                                       ifelse(abs(es_mode) < 0.5, "small", 
                                              ifelse(abs(es_mode) < 0.8, "medium", "large"))) 

# assess if >=95% of credible es are inside the ROPE
equality_boolean        <- ifelse(es_in_rope >= 95, 1, 0)

# assess if the 95% HDI includes the zero point
es_hid_includes_zero    <- ifelse(m_condition_a * m_condition_b < 0,  # if the product of the number is negative then one is positive and one is negative, therefore the interval contains zero. Otherwise, it does not.
                                  "overlapped zero",
                                  "did not overlap zero")

# Assess 3 way decision path based on equality and differences booleans to make a final conclusion
conclusions             <- ifelse(equality_boolean == 1,  # NB even if differences==1 here, effect is still so small as to consider groups equal.
                                  "were credibly equal. ",
                                  ifelse(es_hid_includes_zero == "did not overlap zero",
                                         "were credibly different. ",
                                         "were neither credibly different nor credibly equal. No firm conclusions could therefore be drawn. "))

########################################################################
# combine all output into a natural langauge string
BEST_parameters         <- sprintf("Bayesian BEST tests (Kruschke, 2013) were used to compare differences between %s and %s. In each case, the model employed 5 parameters to describe the data: the means of both conditions (μ1, μ2), the standard deviations of both conditions (σ1, σ2), and a shared normality parameter (ν). We employed the default BEST prior, which is a noncommittal prior intended to have minimal impact on the posterior distribution. Specifically, for sample yi in (y1, y2), μi = normal(M = mean(yi), SD = sd(yi)*5)), σi = gamma(Mo = sd(yi), SD = sd(yi)*5), ν = gamma(M = 30, SD = 30). See Kruschke (2013) for details of the Markov Chain Monte Carlo sampling method. Finally, in order to allow us to assess group equality as well as group differences, a region of practical equivalence (ROPE) was defined (negligible effect size: -0.2 < d < 0.2). ",
                                   condition_a_name, 
                                   condition_b_name)

BEST_text               <- sprintf("The posterior probabilities of a BEST test with %s as DV and condition as IV indicated that %s%% of credible effect sizes were greater than 0, and %s%% were within the ROPE. The most credible effect size was of %s size with a highest density interval that %s, Mode d = %s, 95%% HDI [%s, %s]. We therefore concluded that %s between %s (M = %s) and %s (M = %s) %s", 
                                   DV_name,
                                   es_greater_zero, 
                                   es_in_rope, 
                                   es_size, 
                                   es_hid_includes_zero, 
                                   es_mode, 
                                   es_hdi_low, 
                                   es_hdi_high, 
                                   DV_name, 
                                   condition_a_name, 
                                   m_condition_a, 
                                   condition_b_name, 
                                   m_condition_b, 
                                   conclusions)

########################################################################
# write data to disk

sink(output_file_name)
cat(BEST_parameters)
cat("\n\n")
cat(BEST_text)
sink()

