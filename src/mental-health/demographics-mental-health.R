

demographics_mental_health <-
  bind_rows(
    data_mental_health %>% 
      filter(after == 0) %>% # After data had no demos
      count(age_category) %>% 
      mutate(perc = n /sum(n)) %>% 
      rename(category = 1) %>% 
      mutate(category = as.character(category)),
    
    data_mental_health %>% 
      filter(after == 0) %>% 
      count(gender) %>% 
      mutate(perc = n /sum(n)) %>% 
      arrange(desc(perc)) %>% 
      rename(category = 1),
    
    data_mental_health %>% 
      filter(after == 0) %>% 
      count(supervisor) %>% 
      mutate(perc = n /sum(n)) %>% 
      arrange(desc(perc)) %>% 
      rename(category = 1),
    
    data_mental_health %>% 
      filter(after == 0) %>% 
      count(service) %>% 
      mutate(perc = n /sum(n)) %>% 
      arrange(desc(perc)) %>% 
      mutate(service = str_to_title(service)) %>% 
      rename(category = 1)
  ) %>% 
  mutate(
    perc = round(perc, digits = 2)
  )

demographics_mental_health %>% 
  write_csv(here::here('output/demographics-mental-health.csv'))
