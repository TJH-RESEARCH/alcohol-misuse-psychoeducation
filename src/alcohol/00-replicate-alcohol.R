
# LIBRARY ----------------------------------------------------------------------
library(labelled)
library(careless)
library(qualtRics)
library(tidyverse)

# IMPORT -----------------------------------------------------------------------
source(here::here('src/alcohol/import-qualtrics-alcohol.R'))

# CLEAN ------------------------------------------------------------------------
source(here::here('src/alcohol/clean-alcohol.R'))

# DEMOGRAPHICS ------------------------------------------------------------
source(here::here('src/alcohol/demographics-alcohol.R'))

# SUMMARIZE --------------------------------------------------------------------

## Helper Function: Agree and Strongly Agree as Percentage of Responses
function_agree <- function(x){
  ifelse(is.numeric(x), round(sum(x >= 4, na.rm = TRUE) / sum(x >= 0, na.rm = TRUE), 5), 'N/A')
}

source(here::here('src/alcohol/t-tests-alcohol.R'))
source(here::here('src/alcohol/agree-alcohol.R'))


# Write Data Set --------------------------------------------------------------
## Using the Qualtrics API, as we do more trainings and collect more data, the analysis will change
## So here let's save a copy of the data that was used in order to enable replication in the future
data_alcohol %>% write_csv(here::here(paste0('output/data-alcohol-',Sys.Date(), '.csv')))
