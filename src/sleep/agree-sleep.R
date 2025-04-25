
# Create Quantitative Analysis Table: Agreed

agreed_sleep <-
  bind_rows(
    data_sleep %>% 
      filter(after == 1) %>% 
      group_by(service) %>% 
      select(
         #know_effects,
         #know_hygiene,
         know_post, 
         intent_to_use, 
         satisfaction_content,
         satisfaction_usefullness,
         satisfaction_clear_purpose,
         satisfaction_overall) %>% 
      transmute(across(everything(), function_agree)) %>% 
      unique() %>% 
      filter(!is.na(service)) %>% 
      select(service, everything()),
    
    data_sleep %>% 
      filter(after == 1) %>% 
      select(
         #know_effects,
         #know_hygiene,
         know_post, 
         intent_to_use, 
         satisfaction_content,
         satisfaction_usefullness,
         satisfaction_clear_purpose,
         satisfaction_overall) %>% 
      transmute(across(everything(), function_agree)) %>% 
      unique() %>% 
      mutate(service = 'total') %>% 
      select(service, everything()),
    
    
  ) %>% 
    bind_cols(
      bind_rows(
        data_sleep %>%
          filter(after == 1) %>% 
          count(service),
        data_sleep %>% filter(after == 1) %>% filter(!is.na(service)) %>% count() %>% mutate(service = 'total')
      ) %>% 
        filter(!is.na(service)) %>% 
        select(n)
    ) %>% 
    select(service, n, everything())

agreed_sleep



# Write results to file
agreed_sleep %>% 
  pivot_longer(-service) %>% 
  pivot_wider(id_cols = name,
              names_from = service, 
              values_from = value) %>% 
  mutate(name = c(
    'n',
    'Post-Training Knowledge',
    'Intent to Use the Information',
    'Satisfaction: Content',
    'Satisfaction: Usefulness',
    'Satisfaction: Clarity of Purpose',
    'Satisfaction: Overall'
  )) %>% 
  select(name, total, fire, police) %>% 
  rename(Item = name, 
         `Total` = total,
         Fire = fire, 
         Police = police) %>% 
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) %>%
  write_csv(here::here('output/agreed_sleep.csv'))


