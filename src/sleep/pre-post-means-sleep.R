
pre_post_means_sleep <-
  bind_rows(
    # Total Sample
    data_sleep %>% 
      group_by(after) %>% 
      select(know_effects,
             know_hygiene) %>% 
      transmute(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
      unique() %>% 
      mutate(service = 'total'),
    
    # Grouped by Service
    data_sleep %>% 
      group_by(after, service) %>% 
      select(know_effects,
             know_hygiene) %>% 
      transmute(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
      unique() %>% 
      filter(!is.na(service)) %>% 
      select(service, everything())
  ) %>% 
  mutate(after = ifelse(after == 1, 'post', 'pre')) %>% 
  pivot_wider(names_from = after, values_from = c(know_effects,
                                                  know_hygiene))


