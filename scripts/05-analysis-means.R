

# Summarize
means_alcohol <-
  bind_cols(
    bind_rows(
      data_alcohol %>% 
        filter(after == 1) %>%
        summarize(
          n = n(),
          #know_signs = mean(know_signs, na.rm = TRUE), 
          #know_reduction = mean(know_reduction, na.rm = TRUE), 
          #know_effects = mean(know_effects, na.rm = TRUE), 
          know_post_insights = mean(know_post_insights, na.rm = TRUE), 
          know_post_resources = mean(know_post_resources, na.rm = TRUE), 
          know_post_comfort = mean(know_post_comfort, na.rm = TRUE), 
          intent_to_use = mean(intent_to_use, na.rm = TRUE),
          satisfaction_content = mean(satisfaction_content, na.rm = TRUE),
          satisfaction_usefullness = mean(satisfaction_usefullness, na.rm = TRUE),
          satisfaction_clear_purpose = mean(satisfaction_clear_purpose, na.rm = TRUE),
          satisfaction_overall = mean(satisfaction_overall, na.rm = TRUE)
        ) %>% 
        mutate(service = 'total') %>% 
        select(service, everything()),
      
      data_alcohol %>% 
        filter(after == 1) %>%
        filter(!is.na(service)) %>% 
        group_by(service) %>% 
        summarize(
          n = n(),
          #know_signs = mean(know_signs, na.rm = TRUE), 
          #know_reduction = mean(know_reduction, na.rm = TRUE), 
          #know_effects = mean(know_effects, na.rm = TRUE), 
          know_post_insights = mean(know_post_insights, na.rm = TRUE), 
          know_post_resources = mean(know_post_resources, na.rm = TRUE), 
          know_post_comfort = mean(know_post_comfort, na.rm = TRUE), 
          intent_to_use = mean(intent_to_use, na.rm = TRUE),
          satisfaction_content = mean(satisfaction_content, na.rm = TRUE),
          satisfaction_usefullness = mean(satisfaction_usefullness, na.rm = TRUE),
          satisfaction_clear_purpose = mean(satisfaction_clear_purpose, na.rm = TRUE),
          satisfaction_overall = mean(satisfaction_overall, na.rm = TRUE)
        )
    ) %>% 
      pivot_longer(-service) %>% 
      pivot_wider(names_from = service, values_from = value) %>% 
      rename(total_mean = total, fire_mean = fire, police_mean = police),
    
    
    bind_rows(
      data_alcohol %>% 
        filter(after == 1) %>%
        summarize(
          n = n(),
          #know_signs = mean(know_signs, na.rm = TRUE), 
          #know_reduction = mean(know_reduction, na.rm = TRUE), 
          #know_effects = mean(know_effects, na.rm = TRUE), 
          know_post_insights = mean(know_post_insights, na.rm = TRUE), 
          know_post_resources = mean(know_post_resources, na.rm = TRUE), 
          know_post_comfort = mean(know_post_comfort, na.rm = TRUE), 
          intent_to_use = sd(intent_to_use, na.rm = TRUE),
          satisfaction_content = sd(satisfaction_content, na.rm = TRUE),
          satisfaction_usefullness = sd(satisfaction_usefullness, na.rm = TRUE),
          satisfaction_clear_purpose = sd(satisfaction_clear_purpose, na.rm = TRUE),
          satisfaction_overall = sd(satisfaction_overall, na.rm = TRUE)
        ) %>% 
        mutate(service = 'total') %>% 
        select(service, everything()),
      
      data_alcohol %>% 
        filter(after == 1) %>%
        filter(!is.na(service)) %>% 
        group_by(service) %>% 
        summarize(
          n = n(),
          #know_signs = mean(know_signs, na.rm = TRUE), 
          #know_reduction = mean(know_reduction, na.rm = TRUE), 
          #know_effects = mean(know_effects, na.rm = TRUE), 
          know_post_insights = mean(know_post_insights, na.rm = TRUE), 
          know_post_resources = mean(know_post_resources, na.rm = TRUE), 
          know_post_comfort = mean(know_post_comfort, na.rm = TRUE), 
          intent_to_use = sd(intent_to_use),
          satisfaction_content = sd(satisfaction_content, na.rm = TRUE),
          satisfaction_usefullness = sd(satisfaction_usefullness, na.rm = TRUE),
          satisfaction_clear_purpose = sd(satisfaction_clear_purpose, na.rm = TRUE),
          satisfaction_overall = sd(satisfaction_overall, na.rm = TRUE)
        )
    ) %>% 
      pivot_longer(-service) %>% 
      pivot_wider(names_from = service, values_from = value) %>% 
      select(total, fire, police) %>% 
      rename(total_sd = total, fire_sd = fire, police_sd = police)
    
  ) %>% 
    select(name, total_mean, total_sd, fire_mean, fire_sd, police_mean, police_sd)

means_alcohol
