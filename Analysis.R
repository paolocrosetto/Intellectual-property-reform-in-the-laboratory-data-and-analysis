###
###
###


### Analysis script for the paper "Intellectual porperty reform in the laboratory"

### Libraries
library(tidyverse)                ### the specific R dialect used in this script
library(kableExtra)               ### export beautiful tables
library(huxtable)                 ### export beautiful tables (alternative package)
library(broom)                    ### run efficiently statistical tests and export them to data frames
library(effsize)                  ### computing effect sizes using coehn's d


### Analysis scripts

### 0. data cleaning and preparation for the rest of the analysis
source("Scripts/data_cleaning.R")


### 1. sample demographics
source("Scripts/sample_statistics.R")


### 2. Effects of abolishing intellectual property

# group statistics
source("Scripts/group_points_transition.R")

# group regression
source("Scripts/table_2.R")

# individual-level regression
source("Scripts/individual_points_transition.R")
