
# LIBRARY ----------------------------------------------------------------------
library(labelled)
library(careless)
library(qualtRics)
library(tidyverse)

# IMPORT -----------------------------------------------------------------------
source(here::here('scripts/01-import-data.R'))

# CLEAN ------------------------------------------------------------------------
source(here::here('scripts/02-clean.R'))

# DEMOGRAPHICS ------------------------------------------------------------
source(here::here('scripts/03-demographics.R'))

# SUMMARIZE --------------------------------------------------------------------

## Analyze percentage of responses reporting agree or strongly agree to post-training satisfaction, increased knowledge, and intent to use
source(here::here('scripts/04-analysis-agreement.R'))

## Calculates the mean scores for the same items as above (i.e., the post-training items for which the percentage in agreement were calculated)
source(here::here('scripts/05-analysis-means.R'))

## Calculate percentage in agreement with the items asked pre- and post-intervention
source(here::here('scripts/06-pre-post-agreement.R'))

## Run t-tests on pre-post intervention changes
source(here::here('scripts/07-t-tests.R'))


# Write Data Set --------------------------------------------------------------
## We are sing the Qualtrics API, as we do more trainings and collect more data, the analysis will change
## So here let's save a copy of the data that was used in order to enable replication in the future
data_alcohol %>% write_csv(here::here(paste0('data-alcohol-',Sys.Date(), '.csv')))
