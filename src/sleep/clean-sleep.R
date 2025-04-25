

# make names lower case
names(data_sleep) <- data_sleep %>% names() %>% str_to_lower()

# Convert non-consent into dummy 1-0
data_sleep <-
  data_sleep %>% 
  mutate(
    informed_consent = ifelse(is.na(informed_consent), 0, informed_consent)
  )

# Rename service dummy variables and create a factor variable:
data_sleep <-
  data_sleep %>% 
  rename(police = service_1, 
         fire = service_2) %>% 
  mutate(
    service = case_when(
      police == 1 ~ 'police',
      fire == 1 ~ 'fire',
      .default = NA
    )
  )

  
#data_sleep %>% count(service)
#data_sleep %>% filter(after == 1) %>% count(service)

# Rename Satisfaction Questions
data_sleep <-
  data_sleep %>% 
  rename(satisfaction_content = satisfaction_1,
         satisfaction_usefullness = satisfaction_2,
         satisfaction_clear_purpose = satisfaction_3,
         satisfaction_overall = satisfaction_4)

# Filter non-consent
data_sleep %>% count(informed_consent)

data_sleep <-
  data_sleep %>% 
  filter(informed_consent == 1)

# Filter incomplete
data_sleep %>% count(progress)

data_sleep <-
  data_sleep %>% 
  filter(progress > 50)


# Match before and after responses
data_sleep %>% count(after)

data_sleep <-
  data_sleep %>% 
  mutate(
    training_name = str_to_lower(training_name),
    training_name = str_replace_all(training_name, "[.'’]", ""),
    training_name = str_replace_all(training_name, " ", "_"),
    year = lubridate::year(startdate),
    month = lubridate::month(startdate),
    day = lubridate::day(startdate),
    id = str_c(training_name, '_', year, '_', month, '_', day),
    ip_short_1 = str_split_fixed(ipaddress, '\\.', n = 4)[,1]
    ) %>% 
  mutate(
    id = ifelse(id == 'alicia_kets_2024_6_4', 'alicia_keys_2024_6_4', id),
    id = ifelse(id == 'jay-_z_2024_3_22', 'jay-z_2024_3_22', id),
    id = ifelse(id == 'king_of_leon_2024_9_17', 'kings_of_leon_2024_9_17', id),
    id = ifelse(id == 'missy_elliot_2024_3_21', 'missy_elliott_2024_3_21', id),
    id = ifelse(id == 'shawn_mendez_2024_1_23', 'shawn_mendes_2024_1_23', id),
    id = ifelse(id == '*nsync_2024_3_5', 'nsync_2024_3_5', id)
  )

data_sleep %>% count(id) %>% arrange(desc(n)) %>% print(n = 800)


data_sleep <-
  data_sleep %>% 
  mutate(
    id = ifelse(
      id == 'johnny_cash_2024_1_25' & startdate == '2024-01-25 08:57:20 EDT',
      'johnny_cash_2024_1_25_a', 
      id
    ),
    id = ifelse(
      id == 'johnny_cash_2024_1_25' & startdate == '2024-01-25 09:28:03 EDT',
      'johnny_cash_2024_1_25_a', 
      id
    ),
    
    id = ifelse(
      id == 'johnny_cash_2024_1_25' & startdate == '2024-01-25 10:07:36 EDT',
      'johnny_cash_2024_1_25_b', 
      id
    ),
    id = ifelse(
      id == 'johnny_cash_2024_1_25' & startdate == '2024-01-25 10:47:47 EDT',
      'johnny_cash_2024_1_25_b', 
      id
    ),
    
    id = ifelse(
      id == 'tim_mcgraw_2024_3_20' & startdate == '2024-03-20 08:59:45 EDT',
      'tim_mcgraw_2024_3_20_a', 
      id
    ),
    id = ifelse(
      id == 'tim_mcgraw_2024_3_20' & startdate == '2024-03-20 09:46:16 EDT',
      'tim_mcgraw_2024_3_20_a', 
      id
    ),
    
    id = ifelse(
      id == 'tim_mcgraw_2024_3_20' & startdate == '2024-03-20 11:05:59 EDT',
      'tim_mcgraw_2024_3_20_b', 
      id
    ),
    id = ifelse(
      id == 'tim_mcgraw_2024_3_20' & startdate == '2024-03-20 11:42:35 EDT',
      'tim_mcgraw_2024_3_20_b', 
      id
    ),
    
    id = ifelse(
      id == 'johnny_cash_2024_3_20' & startdate == '2024-03-20 11:01:36 EDT',
      'johnny_cash_2024_3_20_a', 
      id
    ),
    id = ifelse(
      id == 'johnny_cash_2024_3_20' & startdate == '2024-03-20 11:42:29 EDT',
      'johnny_cash_2024_3_20_a', 
      id
    ),
    
    id = ifelse(
      id == 'johnny_cash_2024_3_20' & startdate == '2024-03-20 14:01:11 EDT',
      'johnny_cash_2024_3_20_b', 
      id
    ),
    id = ifelse(
      id == 'johnny_cash_2024_3_20' & startdate == '2024-03-20 14:54:30 EDT',
      'johnny_cash_2024_3_20_b', 
      id
    ),
    
    id = ifelse(
      id == 'tim_mcgraw_2024_6_4',
      paste0('tim_mcgraw_2024_6_4', ip_short_1), 
      id
    )
    
    
  )

# Cardi B is not possible to disambiguate! Exlucde:
data_sleep <-
  data_sleep %>% 
  filter(id != 'cardi_b_2024_3_22')


# Join info on matched before-after with main data_sleepset
data_sleep <-
  left_join(
    data_sleep, 
    data_sleep %>% 
      count(id) %>% 
      arrange(n) %>% 
      mutate(match_before_after = ifelse(n == 2, 1, 0)) %>% 
      select(id, match_before_after),
    by = c('id' = 'id')
  )



# RECODE DEMOGRAPHICS ------------------------------------------------------------
## Label factors
data_sleep <-
  data_sleep %>% 
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
