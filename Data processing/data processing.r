########################################################################
# Calculate D1 scores, accuracy and latency summary statistics for the 
# Open Source IRAP (Implicit Relational Assessment Procedure)
########################################################################
# Author: 
# Ian Hussey (ian.hussey@ugent.be)

# Version:
# 1.0

# Notes:
# prolific_ID is used only to pay participants and must be deleted from all data files before 
# raw data is posted online

# To do:
# None.

########################################################################
# Clean workspace
rm(list=ls())

########################################################################
# Dependencies

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

########################################################################
# Data acquisition and cleaning

## Set the working directory
setwd("/Users/Ian/Dropbox/Work/Projects/Hussey & Hughes - Derivation study/OSF Derivation study/Data/")

# Read all files with the .iqdat extension
files <- list.files(pattern = "\\.csv$")  

# Read these files sequentially into a single data frame
input_df <- dplyr::tbl_df(plyr::rbind.fill(lapply(files, data.table::fread, header = TRUE)))  # tbl_df() requires dplyr, rbind.fill() requires plyr, fread requires data.table

# Make some variable names more transparent
cleaned_df <- 
  input_df %>%
  dplyr::select(subject,
                date,
                time,
                blockcode,  # name of block
                blocknum,
                trialnum,
                response,  # for string responses
                correct,
                latency) %>%
  dplyr::rename(participant = subject,
                block_name = blockcode,
                block_n = blocknum,
                trial_n = trialnum,
                string_response = response,
                accuracy = correct,
                rt = latency) %>%
  dplyr::mutate(participant = as.numeric(participant),
                block_n = as.numeric(block_n),
                trial_n = as.numeric(trial_n),
                accuracy = as.numeric(accuracy),
                rt = as.numeric(rt),
                condition = ifelse(participant%%2 == 1, "low", ifelse(participant%%2 == 0, "high", NA)))


########################################################################
# demographics and parameters 

demographics_df <-
  cleaned_df %>%
  dplyr::group_by(participant) %>%
  dplyr::filter(grepl("demographics", block_name)) %>%  # filter rows where the block_name includes string
  dplyr::select(participant, condition, trial_n, string_response) %>%  # select only necessary columns
  tidyr::spread(trial_n, string_response) %>%  # convert rows to columns
  dplyr::rename(age = `1`,  # rename for clarity
                gender = `2`,
                prolific_ID = `3`)  # NB this must be removed from both summary and raw files before data is posted online


########################################################################
# relational training

# select relevant data
rel_train_df <-
  cleaned_df %>%
  dplyr::filter(grepl("Relational_Training_Phase", block_name)) %>%  # filter rows where the block_name includes string
  dplyr::select(participant, accuracy, block_n)

# n blocks
rel_train_n_blocks_df <-
  rel_train_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(rel_train_n_blocks = max(block_n, na.rm = TRUE))

# % accuracy on last block and pass/fail mastery criterion
rel_train_perc_acc_df <-
  rel_train_df %>%
  dplyr::group_by(participant) %>%
  dplyr::filter(block_n == max(block_n, na.rm = TRUE)) %>%  # filter only the last of their blocks (up to 5)
  dplyr::summarize(rel_train_perc_acc = round(sum(accuracy)/n(), 3)*100) %>%
  dplyr::mutate(rel_train_mastery = ifelse(rel_train_perc_acc >= 90, "pass", "fail"))  # 87.5 is 36/40 correct


########################################################################
# relational testing

# select relevant data, % accuracy and pass/fail mastery criterion
rel_test_perc_acc_df <-
  cleaned_df %>%
  dplyr::filter(grepl("Relational_Testing_Phase", block_name)) %>%  # filter rows where the block_name includes string
  dplyr::select(participant, accuracy) %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(rel_test_perc_acc = round(sum(accuracy)/n(), 3)*100) %>%  # % accuracy
  dplyr::mutate(rel_test_mastery = ifelse(rel_test_perc_acc >= 81.25, "pass", "fail"))  # 81.25 is 13/16 correct


########################################################################
# derivation opportunities

# select relevant data
deriv_opps_df <-
  cleaned_df %>%
  dplyr::filter(grepl("Derivation_Opportunities", block_name)) %>%  # filter rows where the block_name includes string
  dplyr::select(participant, rt, accuracy) %>%
  dplyr::filter(rt <= 10000)  # rts less than 10,000 ms only

# mean rt
deriv_opps_mean_rt_df <-
  deriv_opps_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(deriv_opps_rt_mean = mean(rt)) %>%
  dplyr::mutate(deriv_opps_rt_mean = round(deriv_opps_rt_mean, 0))  # rounding for output simplicity is done only after D1 score calculation

# % acc
deriv_opps_perc_acc_df <- 
  deriv_opps_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(deriv_opps_perc_acc = round(sum(accuracy)/n(), 2)*100)


########################################################################
# IAT Test D1 scores (following Greenwald et al., 2003), % accuracy and mean latency

# select relevant data
IAT_test_df <-  
  cleaned_df %>%
  dplyr::filter(block_name == "IAT_test_compatible_block" | block_name == "IAT_test_incompatible_block",  # Test IAT only, and test blocks only
                rt <= 10000)  # rts less than 10,000 only            

# D1 and mean rt
IAT_test_D1_and_mean_rt_df <-
  IAT_test_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(IAT_test_rt_mean_compatible = mean(rt[block_name == "IAT_test_compatible_block"], na.rm = TRUE),
                   IAT_test_rt_mean_incompatible = mean(rt[block_name == "IAT_test_incompatible_block"], na.rm = TRUE),
                   IAT_test_rt_mean = mean(rt),
                   IAT_test_rt_sd = sd(rt)) %>%
  dplyr::mutate(diff = IAT_test_rt_mean_incompatible - IAT_test_rt_mean_compatible, # this is effectively a rowwise() calculation as we have group_by() participant and then summarize()'d. rowwise() not included for brevity.
                IAT_test_D1 = round(diff / IAT_test_rt_sd, 3),
                IAT_test_rt_mean = round(IAT_test_rt_mean, 0)) %>%  # rounding for output simplicity is done only after D1 score calculation
  dplyr::select(participant, 
                IAT_test_D1,
                IAT_test_rt_mean)

# calculate % acc and % fast trials from test block data
IAT_test_df$too_fast_trial <- ifelse(IAT_test_df$rt < 300, 1, 0)  # add new column that records if RT < 300ms.

IAT_test_perc_acc_fast_trials_df <- 
  IAT_test_df %>%
  dplyr::group_by(participant) %>%
  dplyr::summarize(IAT_test_perc_acc = round(sum(accuracy)/n(), 2),
                   IAT_test_percent_fast_trials = sum(too_fast_trial)/n()) %>%  # arbitrary number of test block trials
  dplyr::mutate(IAT_test_exclude_based_on_fast_trials = ifelse(IAT_test_percent_fast_trials < 0.1, "pass", "fail")) %>%  
  dplyr::select(participant,
                IAT_test_perc_acc,
                IAT_test_exclude_based_on_fast_trials)


########################################################################
# Join data frames and assess exclusion criteria

# join dfs
output_df <- 
  plyr::join_all(list(as.data.frame(demographics_df),  # join_all throws a requires input be data.frame error, despite is.data.frame returning TRUE for all members of list. Workaround is to coerce all to DF here. 
                      as.data.frame(rel_train_n_blocks_df),
                      as.data.frame(rel_train_perc_acc_df),
                      as.data.frame(rel_test_perc_acc_df),
                      as.data.frame(deriv_opps_mean_rt_df),
                      as.data.frame(deriv_opps_perc_acc_df),
                      as.data.frame(IAT_test_D1_and_mean_rt_df),
                      as.data.frame(IAT_test_perc_acc_fast_trials_df)),
                 by = "participant",
                 type = "full")

# define IAT outliers (+/- 2.5 SD on lat or acc)
IAT_outlier_cutoffs_df <- 
  output_df %>% 
  dplyr::summarize(accuracy_upper = mean(IAT_test_perc_acc, na.rm = TRUE) + (2.5 * sd(IAT_test_perc_acc, na.rm = TRUE)),
                   accuracy_lower = mean(IAT_test_perc_acc, na.rm = TRUE) - (2.5 * sd(IAT_test_perc_acc, na.rm = TRUE)),
                   latency_upper = mean(IAT_test_rt_mean, na.rm = TRUE) + (2.5 * sd(IAT_test_rt_mean, na.rm = TRUE)),
                   latency_lower = mean(IAT_test_rt_mean, na.rm = TRUE) - (2.5 * sd(IAT_test_rt_mean, na.rm = TRUE)))

# define IAT outliers (add to output_df)
output_df <- 
  output_df %>%
  dplyr::mutate(IAT_outlier = ifelse(IAT_test_perc_acc > IAT_outlier_cutoffs_df$accuracy_upper, TRUE, 
                                     ifelse(IAT_test_perc_acc < IAT_outlier_cutoffs_df$accuracy_lower, TRUE,
                                            ifelse(IAT_test_rt_mean > IAT_outlier_cutoffs_df$latency_upper, TRUE,
                                                   ifelse(IAT_test_rt_mean < IAT_outlier_cutoffs_df$latency_lower, TRUE, FALSE)))))

# Assess if each participant meet any of the four exclusion criteria. Also excludes participants with partial data.
output_df <- 
  output_df %>%
  rowwise() %>%
  dplyr::mutate(exclude = ifelse(rel_train_mastery == "fail", TRUE, 
                                 ifelse(rel_test_mastery == "fail", TRUE,
                                        ifelse(IAT_test_exclude_based_on_fast_trials == "fail", TRUE,
                                               ifelse(IAT_outlier == TRUE, TRUE,
                                                      FALSE)))))


########################################################################
# Write to disk
write.csv(output_df, file = "~/Git/Derivation study/Data processing/processed data for analysis.csv", row.names = FALSE)

