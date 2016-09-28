########################################################################
# Automated reporting of an independent t test and cohen's d
# for use in knittr scripts

# Ian Hussey (ian.hussey@ugent.be)
# see github.com/ianhussey/automatedreporting 

# output checked against results returned by JASP

########################################################################
# Clean workspace
rm(list=ls())

########################################################################
# dependencies 
library(dplyr)
library(effsize)
library(weights)  # for rd(), a round() alternative 

########################################################################
# Specific data, variables, and parameters of test

# labels
DV_name                   <- "IAT D1 scores"
condition_a_name          <- "the low condition"
condition_b_name          <- "the high condition"
condition_a_code_in_data  <- 1
condition_b_code_in_data  <- 2
output_file_name          <- "t test output - IAT D1.txt"

# data acquisition
setwd("~/Git/Derivation study/Analyses/")

# Data acquisition 
data_df <- 
  read.csv("~/Git/Derivation study/Data processing/processed data for analysis.csv") %>%
  filter(exclude == FALSE)  # exclude participants who met any of the three mastery criteria

attach(data_df)  # use the input data frame for all tests below

# t test
t_test <- t.test(formula = IAT_test_D1 ~ condition,    # IV ~ DV adjusted here
                 alternative = "two.sided",
                 paired = FALSE)

# effect size
cohens_d <- cohen.d(IAT_test_D1 ~ condition,           # IV ~ DV adjusted here
                    paired = FALSE)

# descriptive stats by cell
desc_stats <- 
  ez::ezStats(data = data_df,
              dv = .(IAT_test_D1),                     # DV adjusted here
              wid = .(participant),
              between = .(condition),                  # IV adjusted here
              type = 3) %>%
  mutate(Mean = round(Mean, 2),                        # round for later reporting
         SD = round(SD, 2)) %>%
  select(-FLSD)

########################################################################
## extract individual stats

# t test
t_test_est        <- round(t_test$statistic[[1]], 2)
t_test_df         <- round(t_test$parameter[[1]], 2)
t_test_p          <- round(t_test$p.value[[1]], 5)

# effect size
d_est             <- round(cohens_d$estimate[[1]], 2)
d_ci_lower        <- round(cohens_d$conf.int[["inf"]], 2)
d_ci_upper        <- round(cohens_d$conf.int[["sup"]], 2)
d_interpretation  <- cohens_d$magnitude[[1]]

# round p values using APA rules
t_test_p <- ifelse(t_test_p < 0.001, "< .001", 
                   ifelse(t_test_p < 0.01,
                          paste("= ", rd(t_test_p, 3), sep = ""),  # rd() rounds, converts to string, and removes the leading 0.
                          paste("= ", rd(t_test_p, 2), sep = "")))

## descriptives
a_m   <- desc_stats %>% filter(condition == condition_a_code_in_data) %>% .$Mean  # convert df to individual variables
b_m   <- desc_stats %>% filter(condition == condition_b_code_in_data) %>% .$Mean
a_sd  <- desc_stats %>% filter(condition == condition_a_code_in_data) %>% .$SD
b_sd  <- desc_stats %>% filter(condition == condition_b_code_in_data) %>% .$SD
a_n   <- desc_stats %>% filter(condition == condition_a_code_in_data) %>% .$N
b_n   <- desc_stats %>% filter(condition == condition_b_code_in_data) %>% .$N

########################################################################
# convert output to natural langauge

nhst <- ifelse(t_test_p < 0.05, 
               paste("A Welch's independent t test demonstrated significant differences of ", d_interpretation, " effect size in ", DV_name, " between ", sep = ""),
               paste("A Welch's independent t test demonstrated non-significant differences of ", d_interpretation, " effect size between ", sep = ""))

# t test and d
t_test_output <- paste(", t(", t_test_df, ") = ", t_test_est, ", p ", t_test_p, ", d = ", d_est, ", 95% CI [", d_ci_lower, ", ", d_ci_upper, "]. ", sep = "")

# descriptive stats
desc_a <- paste(condition_a_name, " (n = ", a_n, ", M = ", a_m, ", SD = ", a_sd, ")", sep = "")
desc_b <- paste(condition_b_name, " (n = ", b_n, ", M = ", b_m, ", SD = ", b_sd, ")", sep = "")

########################################################################
## combine and write to disk

## final summary
t_test_text <- paste(nhst, 
                     desc_a, 
                     " and ", 
                     desc_b, 
                     t_test_output, 
                     sep = "")

## write to disk
sink(output_file_name)
cat(t_test_text)  # cat() supresses the line number from being printed
sink()

