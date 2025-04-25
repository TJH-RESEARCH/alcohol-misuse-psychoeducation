
pre_post_means_alcohol <-
  bind_rows(
    # Total Sample
    data_alcohol %>% 
      group_by(after) %>% 
      select(know_signs, 
             know_reduction, 
             know_effects) %>% 
      transmute(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
      unique() %>% 
      mutate(service = 'total'),
    
    # Grouped by Service
    data_alcohol %>% 
      group_by(after, service) %>% 
      select(know_signs, 
             know_reduction, 
             know_effects) %>% 
      transmute(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
      unique() %>% 
      filter(!is.na(service)) %>% 
      select(service, everything())
  ) %>% 
  mutate(after = ifelse(after == 1, 'post', 'pre')) %>% 
  pivot_wider(names_from = after, values_from = c(know_signs, 
                                                  know_reduction, 
                                                  know_effects))

pre_post_means_alcohol %>% view()


