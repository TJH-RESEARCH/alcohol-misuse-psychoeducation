
t_tests_sleep <-
  bind_rows(
    # How knowledgeable are you on how sleep affects the body?
    
    ## total sample
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, know_effects) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_effects,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'effects', service = 'total'),
    
    ## police only
    data_sleep %>%
      filter(match_before_after == 1 & police == 1) %>% 
      select(id, after, know_effects) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_effects,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'effects', service = 'police'),
    
    ## Fire only
    data_sleep %>%
      filter(match_before_after == 1 & fire == 1) %>% 
      select(id, after, know_effects) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_effects,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'effects', service = 'fire'),
    
    
    # How knowledgeable would you rate yourself on sleep hygiene?
    
    ## total 
    data_sleep %>%
      filter(match_before_after == 1) %>% 
      select(id, after, know_hygiene) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_hygiene,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'hygiene', service = 'total'),
    
    data_sleep %>%
      filter(match_before_after == 1 & police == 1) %>% 
      select(id, after, know_hygiene) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_hygiene,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'hygiene', service = 'police'),
    
    data_sleep %>%
      filter(match_before_after == 1 & fire == 1) %>% 
      select(id, after, know_hygiene) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_hygiene,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'hygiene', service = 'fire')
    
  ) %>% 
  select(item, service, everything()) %>% 
  pivot_wider(id_cols = item,
              names_from = service,
              values_from = -c(item, service))

t_tests_sleep

t_tests_sleep %>% 
  select(item, 
         mean_diff_total, t_total, p_t_total,
         mean_diff_fire, t_fire, p_t_fire,
         mean_diff_police, t_police, p_t_police) %>% 
  write_csv(here::here('output/t-test-sleep.csv'))

t_tests_sleep %>% glimpse()

effects <-
  t_tests_sleep %>% 
  transmute(
    item,
    d_total = abs(mean_diff_total) / sd_diff_total,
    d_police = abs(mean_diff_police) / sd_diff_police,
    d_fire = abs(mean_diff_fire) / sd_diff_fire)











