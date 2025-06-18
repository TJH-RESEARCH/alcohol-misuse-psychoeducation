# Analysis Code for: Addressing Alcohol Misuse in Public Safety Personnel: Preliminary Evidence from an Alcohol Use and Misuse Training for Police and Fire Personnel

This repository contains the code to replicate the analysis presented in: "Addressing Alcohol Misuse in Public Safety Personnel: Preliminary Evidence from an Alcohol Use and Misuse Training for Police and Fire Personnel."

The code cannot be replicated without the original data, which is not publicly available.

## Description of the analysis

The entire analysis code is contained in `scripts/`. Each part of the analysis can be executed in the correct order by running the `00-main.R` script. That script executes the other scripts, which perform the following:

- `01-import-data.R`: loads the data from the Qualtrics survey API
- `02-clean.R`: cleans the data, including removing participants who did not consent to research, renaming variables, resolving ambiguities in pre-.post-interevntion participant IDs, and creating factor variables. 
- `03-demographics.R`: creates the tables reporting participant demographics
- `04-analysis-agreement.R`: Calculates the percentage of responses reporting agree or strongly agree to post-training satisfaction, increased knowledge, and intent to use. Creates a table reporting these percentages.
- `05-analysis-means.R`: Calculates the mean scores for the same items as above (i.e., the itmes for which the percentage in agreement were calculated)
- `06-pre-post-agreement.R`: Calculate percentage in agreement with the items asked pre- and post-intervention
- `07-t-tests.R`: performs paired t-tests on the pre-/post-intervention items

## Citation

The manuscript is currently in progress. 
