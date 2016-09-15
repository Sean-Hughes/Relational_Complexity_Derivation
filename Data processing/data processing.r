########################################################################
# Calculate D1 scores, accuracy and latency summary statistics for the 
# Open Source IRAP (Implicit Relational Assessment Procedure)
########################################################################
# Author: 
# Ian Hussey (ian.hussey@ugent.be)

# Version:
# 0.1

# Notes:
# prolific_ID is used only to pay participants and must be deleted from all data files before 
# raw data is posted online

# To do:
# Exclude IAT acc and latency >2.5 SD from mean

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
setwd("~/Git/Derivation study/Measures/")

# Create a list of all files in this folder that use the extension ".csv"
files <- list.files(pattern = "\\.iqdat$")  

# Read these files sequentially into a single data frame
input_df <- dplyr::tbl_df(plyr::rbind.fill(lapply(files, data.table::fread, header = TRUE)))  # tbl_df() requires dplyr, rbind.fill() requires plyr, fread requires data.table

# Make some variable names more transparent
cleaned_df <- 
  input_df %>%
  dplyr::select(subject,
                group,  # condition
                date,
                time,
                blockcode,  # name of block
                blocknum,
                trialnum,
                response,  # for string responses
                correct,
                latency,
                expressions.d,
                expressions.percentcorrect) %>%
  dplyr::rename(participant = subject,
                condition = group,
                block_name = blockcode,
                block_n = blocknum,
                trial_n = trialnum,
                string_response = response,
                accuracy = correct,
                rt = latency,
                IAT_D1 = expressions.d,
                IAT_percent_correct = expressions.percentcorrect)
    

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
  dplyr::mutate(rel_test_mastery = ifelse(rel_test_perc_acc >= 87.5, "pass", "fail"))  # 87.5 is 7/8 correct


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
  dplyr::filter(grepl("IAT_test_", block_name),  # IAT Test only and test blocks only
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
                 type = "full") %>%
  rowwise() %>%
  dplyr::mutate(exclude = ifelse(rel_train_mastery == "fail",  # assess if each participant meet any of the three exclusion criteria
                                 TRUE, 
                                 ifelse(rel_test_mastery == "fail", 
                                        TRUE,
                                        ifelse(IAT_test_exclude_based_on_fast_trials == "fail",
                                               TRUE,
                                               FALSE))))

########################################################################
# Write to disk
write.csv(output_df, file = "~/Git/Derivation study/Data processing/processed data for analysis.csv", row.names=FALSE)

