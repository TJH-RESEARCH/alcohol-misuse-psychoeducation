

# Load the API key --------------------------------------------------------
qualtRics::qualtrics_api_credentials(api_key = readr::read_lines(here::here("qualtrics_api_key.txt")), 
                          base_url = "kennesaw.co1.qualtrics.com",
                          install = TRUE, overwrite = TRUE)

# Load the surveys --------------------------------------------------------
survey <- qualtRics::all_surveys() %>%
  filter(name == "Alcohol Misuse")

# Fetch the responses --------------------------------------------------------
raw <- qualtRics::fetch_survey(surveyID = survey$id, 
                    verbose = TRUE, 
                    label = FALSE, 
                    convert = FALSE)


# Write a raw copy --------------------------------------------------------
#raw %>% write_csv(paste0(here::here(), '/output/data-raw-oha-', system_date, '.csv'))



# Save the data to the environment as 'data' ------------------------------
data_alcohol <- raw
rm(raw)
