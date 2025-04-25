
# LIBRARY ----------------------------------------------------------------------
library(labelled)
library(careless)
library(qualtRics)
library(tidyverse)

# IMPORT -----------------------------------------------------------------------
source(here::here('src/sleep/import-qualtrics-sleep.R'))

# CLEAN ------------------------------------------------------------------------
source(here::here('src/sleep/clean-sleep.R'))

# DEMOGRAPHICS ------------------------------------------------------------
source(here::here('src/sleep/demographics-sleep.R'))

# SUMMARIZE --------------------------------------------------------------------

## Helper Function: Agree and Strongly Agree as Percentage of Responses
function_agree <- function(x){
  ifelse(is.numeric(x), round(sum(x >= 4, na.rm = TRUE) / sum(x >= 0, na.rm = TRUE), 5), 'N/A')
}

## Help Function: Paired T-test
custom_t_test <- 
  function(x){
    x %>% 
      transmute(
        n = n(),
        before_mean = mean(before), 
        before_sd = sd(before),
        after_mean = mean(after),
        after_sd = sd(after),
        diff = before - after,
        mean_diff = mean(diff),
        sd_diff = sd(diff),
        t = abs( (mean_diff - 0) /  (sd_diff /  sqrt(n)) ), 
        df = n - 1,
        p_t = pt(t, df = df, lower.tail = FALSE)) %>% 
      select(-diff) %>% 
      unique()
  }

## Create Tables:
source(here::here('src/sleep/means-sleep.R'))
source(here::here('src/sleep/agree-sleep.R'))
source(here::here('src/sleep/pre-post-means-sleep.R'))
source(here::here('src/sleep/pre-post-agree-sleep.R'))
source(here::here('src/sleep/demographics-scores-sleep.R'))

# VISUALIZE ---------------------------------------------------------------
source(here::here('src/sleep/plot-sleep.R'))
