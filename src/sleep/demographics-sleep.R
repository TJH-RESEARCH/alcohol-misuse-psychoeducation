

demographics_sleep <-
  bind_rows(
    data_sleep %>% 
      filter(after == 0) %>% # After data had no demos
      count(age) %>% 
      mutate(perc = n /sum(n)) %>% 
      rename(category = 1) %>% 
      mutate(category = as.character(category)),
    
    data_sleep %>% 
      filter(after == 0) %>% 
      count(gender) %>% 
      mutate(perc = n /sum(n)) %>% 
      arrange(desc(perc)) %>% 
      rename(category = 1),
    
    data_sleep %>% 
      filter(after == 0) %>% 
      count(supervisor) %>% 
      mutate(perc = n /sum(n)) %>% 
      arrange(desc(perc)) %>% 
      rename(category = 1),
    
    data_sleep %>% 
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

demographics_sleep %>% 
  write_csv(here::here('output/demographics-sleep.csv'))
