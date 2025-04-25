

# make names lower case
names(data_mental_health) <- data_mental_health %>% names() %>% str_to_lower()

# Convert non-consent into dummy 1-0
data_mental_health <-
  data_mental_health %>% 
  mutate(
    informed_consent = ifelse(is.na(informed_consent), 0, informed_consent)
  )

# Rename service dummy variables and create a factor variable:
data_mental_health <-
  data_mental_health %>% 
  rename(police = service_1, 
         fire = service_2,
         other_service = service_3) %>% 
  mutate(
    service = case_when(
      police == 1 ~ 'police',
      fire == 1 ~ 'fire',
      .default = NA
    )
  )
  
# Rename Satisfaction Questions
data_mental_health <-
  data_mental_health %>% 
  rename(satisfaction_content = satisfaction_1,
         satisfaction_usefullness = satisfaction_2,
         satisfaction_clear_purpose = satisfaction_3,
         satisfaction_expectations = satisfaction_4,
         satisfaction_overall = satisfaction_5)





# Filter non-consent
#data_mental_health %>% count(informed_consent)

data_mental_health <-
  data_mental_health %>% 
  filter(informed_consent == 1)

# Filter incomplete
#data_mental_health %>% count(progress)

data_mental_health <-
  data_mental_health %>% 
  filter(progress == 100)


# Match before and after responses
#data_mental_health %>% count(after)

data_mental_health <-
  data_mental_health %>% 
  mutate(
    training_name = str_to_lower(training_name),
    training_name = str_replace_all(training_name, "[.'’]", ""),
    training_name = str_replace_all(training_name, " ", "_"),
    year = lubridate::year(startdate),
    month = lubridate::month(startdate),
    day = lubridate::day(startdate),
    id = str_c(training_name, '_', year, '_', month, '_', day),
    ip_short_1 = str_split_fixed(ipaddress, '\\.', n = 4)[,1],
    ip_short_2 = str_split_fixed(ipaddress, '\\.', n = 4)[,2],
    ip_short_3 = str_split_fixed(ipaddress, '\\.', n = 4)[,3],
    ip_short_4 = str_split_fixed(ipaddress, '\\.', n = 4)[,4]
    ) %>% 
  
  # Fix misspellings in names
  mutate(
    id = ifelse(id == 'beyoncé_2024_7_16', 'beyonce_2024_7_16', id),
    id = ifelse(id == 'bon_2024_7_17', 'bon_iver_2024_7_17', id),
    id = ifelse(id == 'bob_iver_2024_4_30', 'bon_iver_2024_4_30', id),
    id = ifelse(id == 'destiny_child_2024_5_1', 'destinys_child_2024_5_1', id),
    id = ifelse(id == 'lodre_2024_7_17', 'lorde_2024_7_17', id),
    
  # Disambiguate some matches by IP address
    id = ifelse(id == 'johnny_cash_2024_7_17', paste0(id, '_', as.numeric(ip_short_1) * 4), id),
    id = ifelse(id == 'snoop_dogg_2024_5_1', paste0(id, '_', as.numeric(ip_short_1) * 4, ip_short_4), id)
  ) 

#data_mental_health %>% count(id) %>% arrange(desc(n)) %>% print(n = 110)


# Join info on matched before-after with main data_mental_healthset
data_mental_health <-
  left_join(
    data_mental_health, 
    data_mental_health %>% 
      count(id) %>% 
      arrange(n) %>% 
      mutate(match_before_after = ifelse(n == 2, 1, 0)) %>% 
      select(id, match_before_after),
    by = c('id' = 'id')
  )

#data_mental_health %>% count(match_before_after, after)

# Some people answered before twice or after twice instead of one of each
## disambiguate with start time
#data_mental_health %>% filter(match_before_after == 1) %>% select(id, after) %>% arrange(id) %>% print(n = 200)

data_mental_health <-
  data_mental_health %>% 
  mutate(
    after = ifelse(id == 'prince_2024_4_30' & startdate == '2024-04-30 09:00:26 EDT', 0, after), 
    after = ifelse(id == 'prince_2024_4_30' & startdate == '2024-04-30 09:28:13 EDT', 1, after), 
    after = ifelse(id == 'dr_dre_2024_5_2' & startdate == '2024-05-02 10:29:35 EDT', 0, after), 
    after = ifelse(id == 'dr_dre_2024_5_2' & startdate == '2024-05-02 11:09:30 EDT', 1, after)
  )




# Clean demographic data

## Change snarky gender response to NA
data_mental_health <-
  data_mental_health %>% 
  mutate(gender = ifelse(gender == 3, NA, gender))

## Label factors
data_mental_health <-
  data_mental_health %>% 
  mutate(
    supervisor = 
      factor(
        supervisor,
        levels = c(0,1),
        labels = c('No', 'Yes')
      ),
    gender = 
      factor(
        gender,
        levels = c(1:3),
        labels = c('Male', 'Female', 'Other')
      )
  )

## Fix birth years
data_mental_health <-
  data_mental_health %>% 
  mutate(
    birth_year = ifelse(birth_year == 'Georgia', NA, birth_year),
    birth_year = ifelse(birth_year == 'Italy', NA, birth_year),
    birth_year = ifelse(birth_year == 81, 1981, birth_year),
    birth_year = ifelse(birth_year == 200, NA, birth_year),
    birth_year = as.numeric(birth_year),
    age_years = year - birth_year
  )

# Age Categories
data_mental_health <-
  data_mental_health %>% 
  mutate(
    age_category = 
      factor(
        case_when(
          age_years > 18 & age_years <= 24 ~ 1,
          age_years > 24 & age_years <= 34 ~ 2,
          age_years > 34 & age_years <= 44 ~ 3,
          age_years > 44 & age_years <= 54 ~ 4,
          age_years > 54 ~ 5,
        ),
        levels = c(1:5),
        ordered = TRUE,
        labels = c(
          '18 to 24 years old',
          '25 to 34 years old',
          '35 to 44 years old',
          '45 to 54 years old',
          '55 years or older'
        )
      )
  )
