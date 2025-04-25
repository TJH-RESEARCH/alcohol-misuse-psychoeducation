

# Create Quantitative Analysis Table: Agreed

agreed_alcohol <-
  bind_rows(
    data_alcohol %>% 
      filter(after == 1) %>% 
      group_by(service) %>% 
      select(
         #know_signs, 
         #know_reduction, 
         #know_effects, 
         know_post_insights,
         know_post_resources,
         know_post_comfort,
         intent_to_use, 
         satisfaction_content,
         satisfaction_usefullness,
         satisfaction_clear_purpose,
         satisfaction_overall) %>% 
      transmute(across(everything(), function_agree)) %>% 
      unique() %>% 
      filter(!is.na(service)) %>% 
      select(service, everything()),
    
    data_alcohol %>% 
      filter(after == 1) %>% 
      select(
         #know_signs, 
         #know_reduction, 
         #know_effects, 
         know_post_insights,
         know_post_resources,
         know_post_comfort, 
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
  arrange(service) %>% # arrange alphabetically by service
    bind_cols(
      bind_rows(
        data_alcohol %>%
          filter(after == 1 & !is.na(service)) %>% 
          count(service),
        data_alcohol %>% filter(after == 1) %>% filter(!is.na(service)) %>% count() %>% mutate(service = 'total')
      ) %>% 
        arrange(service) %>%  # arrange alphabetically by service to match above
        select(n)
    ) %>% 
    select(service, n, everything())

agreed_alcohol



# Write results to file
agreed_alcohol %>% 
  pivot_longer(-service) %>% 
  pivot_wider(id_cols = name,
              names_from = service, 
              values_from = value) %>% 
  mutate(name = c(
    'n',
    'Improved Knowledge: Insights',
    'Improved Knowledge: Resources',
    'Improved Knowledge: Comfort',
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
  write_csv(here::here(paste0('output/agreed-alcohol-', Sys.Date(), '.csv')))


