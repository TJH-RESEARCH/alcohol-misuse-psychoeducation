
pre_post_means_mental_health <-

  bind_cols(
      bind_rows(
      # Total Sample
      data_mental_health %>% 
        filter(match_before_after == 1) %>% 
        group_by(after) %>% 
        select(know_differences,
               know_selfcare,
               know_resources,
               know_support) %>% 
        transmute(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
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
        transmute(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
        unique() %>% 
        filter(!is.na(service)) %>% 
        select(service, everything())
    ) %>% 
    mutate(after = ifelse(after == 1, 'post', 'pre')) %>% 
    pivot_wider(names_from = after, values_from = c(know_differences,
                                                    know_selfcare,
                                                    know_resources,
                                                    know_support)) %>% 
      rename_with(~ paste0(.x, '_mean'), starts_with('know')),
  
  
  bind_rows(
    # Total Sample
    data_mental_health %>% 
      filter(match_before_after == 1) %>% 
      group_by(after) %>% 
      select(know_differences,
             know_selfcare,
             know_resources,
             know_support) %>% 
      transmute(across(everything(), ~ sd(.x, na.rm = TRUE))) %>% 
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
      transmute(across(everything(), ~ sd(.x, na.rm = TRUE))) %>% 
      unique() %>%
      filter(!is.na(service)) %>% 
      select(service, everything())
  ) %>% 
    mutate(after = ifelse(after == 1, 'post', 'pre')) %>% 
    pivot_wider(names_from = after, values_from = c(know_differences,
                                                    know_selfcare,
                                                    know_resources,
                                                    know_support)) %>% 
    select(-service) %>% 
    rename_with(~ paste0(.x, '_sd'), starts_with('know'))
  
  ) %>% 
    select(
      service, 
      starts_with('know_differences_pre'),
      starts_with('know_differences_post'),
      starts_with('know_selfcare_pre'),
      starts_with('know_selfcare_post'),
      starts_with('know_resources_pre'),
      starts_with('know_resources_post'),
      starts_with('know_suport_pre'),
      starts_with('know_suport_post')
    )



# Reshape -----------------------------------------------------------------
pre_post_means_mental_health %>% 
  pivot_longer(cols = -service) %>% 
  pivot_wider(names_from = service, values_from = value, id_cols = name) %>% 
  mutate(stat = ifelse(str_detect(name, 'mean'), 'mean', 'sd'),
         post = ifelse(str_detect(name, 'post'), 'post', 'pre'),
         name = str_remove(name, 'know_'),
         name = str_remove(name, '_pre_mean'),
         name = str_remove(name, '_post_mean'),
         name = str_remove(name, '_pre_sd'),
         name = str_remove(name, '_post_sd')
         ) %>% 
  pivot_wider(names_from = c(stat, post), 
              values_from = c(total, fire, police),
              id_cols = name
              )
  

         