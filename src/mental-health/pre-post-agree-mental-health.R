
pre_post_agree_mental_health <-
  bind_rows(
  # Total Sample
  data_mental_health %>% 
    filter(match_before_after == 1) %>% 
    group_by(after) %>% 
    select(know_differences,
           know_selfcare,
           know_resources,
           know_support) %>% 
    transmute(across(everything(), function_agree)) %>% 
    unique() %>% 
    mutate(service = 'total'),
  
  # Grouped by Service
  data_mental_health %>% 
    filter(match_before_after == 1) %>% 
    group_by(after, service) %>% 
    select(know_differences,
           know_selfcare,
           know_resources,
           know_support) %>% 
    transmute(across(everything(), function_agree)) %>% 
    unique() %>% 
    filter(!is.na(service)) %>% 
    select(service, everything())
  ) %>% 
  mutate(after = ifelse(after == 1, 'post', 'pre')) %>% 
  pivot_wider(names_from = after, values_from = c(know_differences,
                                                  know_selfcare,
                                                  know_resources,
                                                  know_support))

pre_post_agree_mental_health


