###################################################################
# Descriptive statistics

# Ian Hussey (ian.hussey@ugent.be)

# output checked against results returned by JASP

###################################################################
# clean the workspace
rm(list=ls())

###################################################################
# dependencies
library(tidyr)
library(dplyr)
library(psych)

###################################################################
# data acquisition
setwd("/Users/Ian/Dropbox/Work/Studies/DCC work/DCC Article 2 - Derivation/Experiments 2 - Ian and Sean/OSF files/5 Analyses/")
data_df <- read.csv("/Users/Ian/Dropbox/Work/Studies/DCC work/DCC Article 2 - Derivation/Experiments 2 - Ian and Sean/OSF files/4 Data processing/processed data for analysis.csv")

###################################################################
# distribution plots for DVs

plot(density(data_df$IAT_test_D1[data_df$condition == 1]))
lines(density(data_df$IAT_test_D1[data_df$condition == 2]))

plot(density(data_df$deriv_opps_rt_mean[data_df$condition == 1]))
lines(density(data_df$deriv_opps_rt_mean[data_df$condition == 2]))

###################################################################
# descriptives

# factor variables
gender_counts                 <- data_df %>% count(gender)
rel_train_criterion_counts    <- data_df %>% count(rel_train_mastery)
rel_test_criterion_counts     <- data_df %>% count(rel_test_mastery)
IAT_test_criterion_counts     <- data_df %>% count(IAT_test_exclude_based_on_fast_trials)
total_exclusions_count        <- data_df %>% count(exclude)

# all ps
descriptives_all_participants <- 
  data_df %>% 
  dplyr::select(age,
                rel_train_n_blocks,
                rel_train_perc_acc,
                rel_test_perc_acc,
                IAT_test_rt_mean,
                IAT_test_perc_acc,
                deriv_opps_rt_mean,
                deriv_opps_perc_acc) %>%
  psych::describe(fast = TRUE,  # subset of descriptive stats
                  ranges = FALSE,
                  trim = 0) %>%
  dplyr::select(-vars, -se)

# by condition
descriptives_by_condition <- 
  data_df %>% 
  dplyr::select(age,
                rel_train_n_blocks,
                rel_train_perc_acc,
                rel_test_perc_acc,
                IAT_test_rt_mean,
                IAT_test_perc_acc,
                deriv_opps_rt_mean,
                deriv_opps_perc_acc) %>%
  psych::describeBy(data_df$condition,
                    fast = TRUE,  # subset of descriptive stats
                    ranges = FALSE,
                    trim = 0)

###################################################################
# write output to disk

sink("descriptive statistics.txt")
cat("\n Gender counts \n")
gender_counts
cat("\n relational training pass/fails \n")
rel_train_criterion_counts
cat("\n relational testing pass/fails \n")
rel_test_criterion_counts
cat("\n IAT test criterion pass/fails \n")
IAT_test_criterion_counts
cat("\n total exclusions \n")
total_exclusions_count
cat("\n")
descriptives_all_participants
cat("\n")
descriptives_by_condition
sink()


