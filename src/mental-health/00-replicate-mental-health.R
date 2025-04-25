
# LIBRARY ----------------------------------------------------------------------
library(labelled)
library(careless)
library(qualtRics)
library(tidyverse)

# IMPORT -----------------------------------------------------------------------
source(here::here('src/mental-health/import-qualtrics-mental-health.R'))

# CLEAN ------------------------------------------------------------------------
source(here::here('src/mental-health/clean-mental-health.R'))

# DEMOGRAPHICS ------------------------------------------------------------
source(here::here('src/mental-health/demographics-mental-health.R'))

# SUMMARIZE --------------------------------------------------------------------

# Helper Function: Agree and Strongly Agree as Percentage of Responses
function_agree <- function(x){
  ifelse(is.numeric(x), round(sum(x >= 4, na.rm = T) / sum(x >= 0, na.rm = T), 5), 'N/A')
}


source(here::here('src/mental-health/means-mental-health.R'))
source(here::here('src/mental-health/agree-mental-health.R'))
source(here::here('src/mental-health/pre-post-means-mental-health.R'))
source(here::here('src/mental-health/pre-post-agree-mental-health.R'))
source(here::here('src/mental-health/t-tests-mental-health.R'))

# CREATE Codebook --------------------------------------------------------------
#source(here::here('src/create-codebook.R'))
#codebook %>% write_csv(here::here('output/codebook-mental-health.csv'))
means_mental_health
agreed_mental_health
pre_post_means_mental_health
pre_post_agree_mental_health
t_tests_mental_health

  
