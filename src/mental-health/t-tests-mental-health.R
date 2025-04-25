custom_t_test <- 
  function(x){
    x %>% 
      transmute(
        n = n(),
        before_mean = mean(before), 
        before_sd = sd(before),
        after_mean = mean(after),
        after_sd = sd(after),
        diff = before - after,
        mean_diff = mean(diff),
        sd_diff = sd(diff),
        t = abs( (mean_diff - 0) /  (sd_diff /  sqrt(nrow(.))) ), 
        df = n - 1,
        p_t = pt(t, df = df, lower.tail = FALSE)) %>% 
      select(-diff) %>% 
      unique()
    
  }


t_tests_mental_health <-
  bind_rows(
    # How would you rate your understanding of the differences between mental health,
    # mental health challenges, and mental health disorders?
    data_mental_health %>%
      filter(match_before_after == 1) %>% 
      select(id, after, know_differences) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_differences) %>% 
      rename(before = `0`, after = `1`) %>% 
      custom_t_test() %>% 
      mutate(item = 'differences', service = 'total'),
    
    data_mental_health %>%
      filter(match_before_after == 1 & police == 1) %>% 
      select(id, after, know_differences) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_differences) %>%
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'differences', service = 'police'),
    
    data_mental_health %>%
      filter(match_before_after == 1 & fire == 1) %>% 
      select(id, after, know_differences) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_differences) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'differences', service = 'fire'),
    
    
    # How would you rate your familiarity with self-care and its application?
    data_mental_health %>%
      filter(match_before_after == 1) %>% 
      select(id, after, know_selfcare) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_selfcare) %>% 
      rename(before = `0`, after = `1`) %>% 
      custom_t_test() %>% 
      mutate(item = 'selfcare', service = 'total'),
    
    data_mental_health %>%
      filter(match_before_after == 1 & police == 1) %>% 
      select(id, after, know_selfcare) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_selfcare) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'selfcare', service = 'police'),
    
    data_mental_health %>%
      filter(match_before_after == 1 & fire == 1) %>% 
      select(id, after, know_selfcare) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_selfcare) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'selfcare', service = 'fire'),
    
    
    # How would you rate your knowledge of the availability of 
    # mental health resources?
    data_mental_health %>%
      filter(match_before_after == 1) %>% 
      select(id, after, know_resources) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_resources) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'resources', service = 'total'),
    
    data_mental_health %>%
      filter(match_before_after == 1 & police == 1) %>% 
      select(id, after, know_resources) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_resources) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'resources', service = 'police'),
    
    data_mental_health %>%
      filter(match_before_after == 1 & fire == 1) %>% 
      select(id, after, know_resources) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_resources) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'resources', service = 'fire'),
    
    # If I were to experience a mental health challenge, I would know where to
    # go for support or how to help myself.
    data_mental_health %>%
      filter(match_before_after == 1) %>% 
      select(id, after, know_support) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_support) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'support', service = 'total'),
    
    
    data_mental_health %>%
      filter(match_before_after == 1 & police == 1) %>% 
      select(id, after, know_support) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_support) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'support', service = 'police'),
    
    
    data_mental_health %>%
      filter(match_before_after == 1 & fire == 1) %>% 
      select(id, after, know_support) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_support) %>% 
      rename(before = `0`, after = `1`) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'support', service = 'fire')
    
  ) %>% 
  select(item, service, everything()) %>% 
  pivot_wider(id_cols = item,
              names_from = service,
              values_from = -c(item, service))

t_tests_mental_health

t_tests_mental_health %>% 
  select(item, 
         mean_diff_total, t_total, p_t_total,
         mean_diff_fire, t_fire, p_t_fire,
         mean_diff_police, t_police, p_t_police) %>% 
  write_csv(here::here('output/t-test-mental-health.csv'))
