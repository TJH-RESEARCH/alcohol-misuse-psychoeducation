
sleep_effects <-
  bind_rows(
    # Total: effects - Age
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, age, know_effects) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = c(know_effects,age),
                  values_fn = list) %>% 
      rename(before = know_effects_0, after = know_effects_1, age = age_0) %>% 
      select(-age_1) %>% 
      unnest(c(before, after, age)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      group_by(age) %>% 
      custom_t_test() %>% 
      mutate(item = 'effects',
             age = as.character(age)) %>% 
      rename(category = 1) %>% 
      slice(c(3,2,4,1,5)),
    
    
    
    # Total: effects - Gender
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, gender, know_effects) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = c(know_effects, gender),
                  values_fn = list) %>% 
      rename(before = know_effects_0, after = know_effects_1, gender = gender_0) %>% 
      select(-gender_1) %>% 
      unnest(c(before, after, gender)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      group_by(gender) %>% 
      custom_t_test() %>% 
      mutate(item = 'effects') %>% 
      rename(category = 1),
    
    
    
    
    
    # Total: effects - supervisor
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, supervisor, know_effects) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = c(know_effects, supervisor),
                  values_fn = list) %>% 
      rename(before = know_effects_0, after = know_effects_1, supervisor = supervisor_0) %>% 
      select(-supervisor_1) %>% 
      unnest(c(before, after, supervisor)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      group_by(supervisor) %>% 
      custom_t_test() %>% 
      mutate(item = 'effects') %>% 
      rename(category = 1),
    
    
    
    # Total: effects - service
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, service, know_effects) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = c(know_effects, service),
                  values_fn = list) %>% 
      rename(before = know_effects_0, after = know_effects_1, service = service_0) %>% 
      select(-service_1) %>% 
      unnest(c(before, after, service)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      group_by(service) %>% 
      custom_t_test() %>% 
      mutate(item = 'effects') %>% 
      rename(category = 1)
  )
    
    
sleep_hygiene <-  
  bind_rows(
    # Total: hygiene - Age
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, age, know_hygiene) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = c(know_hygiene,age),
                  values_fn = list) %>% 
      rename(before = know_hygiene_0, after = know_hygiene_1, age = age_0) %>% 
      select(-age_1) %>% 
      unnest(c(before, after, age)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      group_by(age) %>% 
      custom_t_test() %>% 
      mutate(item = 'hygiene',
             age = as.character(age)) %>% 
      rename(category = 1) %>% 
      slice(c(3,2,4,1,5)),
    
    
    
    # Total: hygiene - Gender
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, gender, know_hygiene) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = c(know_hygiene, gender),
                  values_fn = list) %>% 
      rename(before = know_hygiene_0, after = know_hygiene_1, gender = gender_0) %>% 
      select(-gender_1) %>% 
      unnest(c(before, after, gender)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      group_by(gender) %>% 
      custom_t_test() %>% 
      mutate(item = 'hygiene') %>% 
      rename(category = 1),
    
    
    
    
    
    # Total: hygiene - supervisor
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, supervisor, know_hygiene) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = c(know_hygiene, supervisor),
                  values_fn = list) %>% 
      rename(before = know_hygiene_0, after = know_hygiene_1, supervisor = supervisor_0) %>% 
      select(-supervisor_1) %>% 
      unnest(c(before, after, supervisor)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      group_by(supervisor) %>% 
      custom_t_test() %>% 
      mutate(item = 'hygiene') %>% 
      rename(category = 1),
    
    
    
    # Total: hygiene - service
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, service, know_hygiene) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = c(know_hygiene, service),
                  values_fn = list) %>%
      rename(before = know_hygiene_0, after = know_hygiene_1, service = service_0) %>% 
      select(-service_1) %>% 
      unnest(c(before, after, service)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      group_by(service) %>% 
      custom_t_test() %>% 
      mutate(item = 'hygiene') %>% 
      rename(category = 1)
  )

# Create Table
sleep_demos <-
  bind_cols(
    sleep_effects %>% 
      select(category, n, mean_diff, sd_diff, t) %>% 
      rename(mean_diff_effects = mean_diff, 
             sd_diff_effects = sd_diff,
             t_effects = t),
    sleep_hygiene %>% 
      ungroup() %>% 
      select(mean_diff, sd_diff, t) %>% 
      rename(mean_diff_hygiene = mean_diff, 
             sd_diff_hygiene = sd_diff, 
             t_hygiene = t),
  ) %>% 
    mutate(mean_diff_effects = abs(mean_diff_effects),
           mean_diff_hygiene = abs(mean_diff_hygiene))

sleep_demos %>% write_csv(here::here('output/demographics-scores-sleep.csv'))


