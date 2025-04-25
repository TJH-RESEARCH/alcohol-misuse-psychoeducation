

# make variable names lower case
names(data_alcohol) <- data_alcohol %>% names() %>% str_to_lower()

# Convert informed consent response into dummy 1-0
data_alcohol <-
  data_alcohol %>% 
  mutate(
    informed_consent = ifelse(is.na(informed_consent), 0, informed_consent)
  )

# Rename service dummy variables and create a factor variable:
data_alcohol <-
  data_alcohol %>% 
  rename(police = service_1, 
         fire = service_2) %>% 
  mutate(
    service = case_when(
      police == 1 ~ 'police',
      fire == 1 ~ 'fire',
      .default = NA
    )
  )

# Rename Satisfaction Questions
data_alcohol <-
  data_alcohol %>% 
  rename(satisfaction_content = satisfaction_1,
         satisfaction_usefullness = satisfaction_2,
         satisfaction_clear_purpose = satisfaction_3,
         satisfaction_overall = satisfaction_4)

# Filter non-consent
data_alcohol <-
  data_alcohol %>% 
  filter(informed_consent == 1)

# Filter incomplete
data_alcohol <-
  data_alcohol %>% 
  filter(progress > 60)

# Create codes to match before-after responses
data_alcohol <-
  data_alcohol %>% 
  mutate(
    training_name = str_to_lower(training_name),
    training_name = str_replace_all(training_name, "[.'’]", ""),
    training_name = str_replace_all(training_name, " ", "_"),
    year = lubridate::year(startdate),
    month = lubridate::month(startdate),
    day = lubridate::day(startdate),
    hour = lubridate::hour(startdate),
    id = str_c(training_name, '_', year, '_', month, '_', day),
    ip_short_1 = str_split_fixed(ipaddress, '\\.', n = 4)[,1]
    )

# Resolve ambiguous IDs....only radiohead_2024_3_26 is an issue. It can be resolved using IP address
data_alcohol <-
  data_alcohol %>% 
  mutate(
    id = ifelse(id == 'radiohead_2024_3_26', paste0(id, ip_short_1), id)
  )

# Resolve people who took the pre- or post- twice instead of one each (using start time)
## actually, it looks like tim_mcgraw_2024_3_26 was two different people, given the surveys were 3 hours a part.
## I will disambiguate with a timestamp but not change they before/after:
data_alcohol <-
  data_alcohol %>% 
  mutate(id = ifelse(id == 'tim_mcgraw_2024_3_26', paste0(id, hour), id))


# Resolve misspelled IDs
data_alcohol <-
  data_alcohol %>% 
  mutate(
    id = ifelse(id == 'bob_segar_2024_8_7', 'bob_seagar_2024_8_7', id),
    id = ifelse(id == 'jason_alden_2024_3_26', 'jason_aldean_2024_3_26', id),
    id = ifelse(id == 'mary_j_blidge_2024_8_7', 'mary_j_blige_2024_8_7', id),
    id = ifelse(id == 'camila_cabella_2024_8_6', 'camila_cabello_2024_8_6', id),
    id = ifelse(id == 'boddy_brown_2024_8_8', 'bobby_brown_2024_8_8', id)
  )

# Some responses gave service to pre- and not post- vice versa
data_alcohol <-
  data_alcohol %>% 
  mutate(
    service = ifelse(id == 'billie_eilish_2024_8_7', 'fire', service),
    service = ifelse(id == 'bob_seagar_2024_8_7', 'fire', service),
    service = ifelse(id == 'lady_gaga_2024_3_27', 'police', service),
    service = ifelse(id == 'muse_2024_8_8', 'fire', service)
  )


# Join info on matched before-after with main data_alcoholset
data_alcohol <-
  left_join(
    data_alcohol, 
    data_alcohol %>% 
      count(id) %>% 
      arrange(n) %>% 
      mutate(match_before_after = ifelse(n == 2, 1, 0)) %>% 
      select(id, match_before_after),
    by = c('id' = 'id')
  )


# RECODE DEMOGRAPHICS ------------------------------------------------------------
## Label factors
data_alcohol <-
  data_alcohol %>% 
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
      ),
    age = 
      factor(age, 
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


data_alcohol %>% 
  count(id, service) %>% 
  arrange(desc(n)) %>% 
  print(n = 400)
