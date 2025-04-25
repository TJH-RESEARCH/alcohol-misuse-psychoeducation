


# Create Quantitative Analysis Table: Agreed

agreed_mental_health <-
  bind_rows(
    data_mental_health %>% 
      filter(after == 1) %>% 
      group_by(service) %>% 
      select(
          #know_differences,
          #know_selfcare,
          #now_resources,
          #know_support,
          know_post, 
          intent_to_use, 
          satisfaction_content,
          satisfaction_usefullness,
          satisfaction_clear_purpose,
          satisfaction_expectations,
          satisfaction_overall) %>% 
      transmute(across(everything(), function_agree)) %>% 
      unique() %>% 
      filter(!is.na(service)) %>% 
      select(service, everything()),
    
    data_mental_health %>% 
      filter(after == 1) %>% 
      select(
         #know_differences,
         #know_selfcare,
         #know_resources,
         #know_support,
         know_post,
         intent_to_use, 
         satisfaction_content,
         satisfaction_usefullness,
         satisfaction_clear_purpose,
         satisfaction_expectations,
         satisfaction_overall) %>% 
      transmute(across(everything(), function_agree)) %>% 
      unique() %>% 
      mutate(service = 'total') %>% 
      select(service, everything()),
    
    
  ) %>% 
    bind_cols(
      bind_rows(
        data_mental_health %>%
          filter(after == 1) %>% 
          count(service),
        data_mental_health %>% 
          filter(after == 1) %>% 
          filter(!is.na(service)) %>% 
          count() %>% 
          mutate(service = 'total')
      ) %>% 
        filter(!is.na(service)) %>% 
        select(n)
    ) %>% 
    select(service, n, everything())

agreed_mental_health


# Write results to file
agreed_mental_health %>% 
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
    'Satisfaction: Met Expectations',
    'Satisfaction: Overall'
  )) %>% 
  select(name, total, fire, police) %>% 
  rename(Item = name, 
         `Total Sample` = total,
         Fire = fire, 
         Police = police) %>% 
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) %>%
  write_csv(here::here('output/agreed_mental_health.csv'))


data_mental_health %>% 
  count(know_post)
