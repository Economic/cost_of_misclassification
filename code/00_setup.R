# Title: State misclassification analysis
# Author: Daniel Perez
# Purpose: Estimate cost of misclassification by state

library(tidyverse)
library(here)
library(janitor)
library(openxlsx)
library(labelled)
library(readxl)
library(blsR)
library(data.table)
library(realtalk)

#Load CPS supplement and geographic labels
source("code/01_get_oews_data.R", echo = TRUE)

source("code/02_get_bls_ecec_data.R", echo = TRUE)

source("code/03_clean_ecec_data.R", echo = TRUE)

source('code/04_merge_oews_ecec.R', echo = TRUE)

source('code/05_job_value_comparison.R', echo = TRUE)

source('code/06_excel_export.R', echo = TRUE)


