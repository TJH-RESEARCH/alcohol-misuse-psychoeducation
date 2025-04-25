

demographics_alcohol <-
  bind_rows(
    data_alcohol %>% 
      filter(after == 0) %>% # After data had no demos
      count(age) %>% 
      mutate(perc = n /sum(n)) %>% 
      rename(category = 1) %>% 
      mutate(category = as.character(category),
             variable = 'Age') %>% 
      select(variable, everything()),
    
    data_alcohol %>% 
      filter(after == 0) %>% 
      count(gender) %>% 
      mutate(perc = n /sum(n)) %>% 
      arrange(desc(perc)) %>% 
      rename(category = 1) %>% 
      mutate(variable = 'Gender') %>% 
      select(variable, everything()),
    
    data_alcohol %>% 
      filter(after == 0) %>% 
      count(supervisor) %>% 
      mutate(perc = n /sum(n)) %>% 
      arrange(desc(perc)) %>% 
      rename(category = 1) %>% 
      mutate(variable = 'Supervisor') %>% 
      select(variable, everything()),
    
    data_alcohol %>% 
      filter(after == 0) %>% 
      count(service) %>% 
      mutate(perc = n /sum(n)) %>% 
      arrange(desc(perc)) %>% 
      mutate(service = str_to_title(service)) %>% 
      rename(category = 1) %>% 
      mutate(variable = 'Service') %>% 
      select(variable, everything()),
  ) %>% 
  mutate(
    perc = round(perc, digits = 2)
  )

demographics_alcohol %>% 
  write_csv(here::here(paste0('output/demographics-alcohol-', Sys.Date(), '.csv')))
