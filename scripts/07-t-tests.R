custom_t_test <- 
  function(x){
    x %>% 
      transmute(
        n = n(),
        before_mean = mean(before), 
        before_sd = sd(before),
        after_mean = mean(after),
        after_sd = sd(after),
        diff = after - before,
        mean_diff = mean(diff),
        sd_diff = sd(diff),
        d = mean_diff / sd_diff,
        t = (mean_diff - 0) /  (sd_diff /  sqrt(nrow(.))), 
        df = n - 1,
        p_t = pt(t, df = df, lower.tail = FALSE)) %>% 
      select(-diff) %>% 
      unique()
  }


t_tests_alcohol <-
  bind_rows(
    # I can recognize warning signs for alcohol misuse in someone else or myself.
    
    ## Total sample
    data_alcohol %>%
      filter(match_before_after == 1) %>% 
      select(id, after, know_signs) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_signs,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'signs', service = 'total'), # Label the item and sample
    
    ## Police
    data_alcohol %>%
      filter(match_before_after == 1 & police == 1) %>% 
      select(id, after, know_signs) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_signs,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'signs', service = 'police'), # Label the item and sample
    
    ## Fire
    data_alcohol %>%
      filter(match_before_after == 1 & fire == 1) %>% 
      select(id, after, know_signs) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_signs,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'signs', service = 'fire'), # Label the item and sample
    
    
    # I am aware of strategies to reduce my own drinking if needed. 
    
    ## Total sample
    data_alcohol %>%
      filter(match_before_after == 1) %>% 
      select(id, after, know_reduction) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_reduction,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'reduction', service = 'total'), # Label the item and sample
    
    ## Police
    data_alcohol %>%
      filter(match_before_after == 1 & police == 1) %>% 
      select(id, after, know_reduction) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_reduction,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'reduction', service = 'police'), # Label the item and sample
    
    ## Fire
    data_alcohol %>%
      filter(match_before_after == 1 & fire == 1) %>% 
      select(id, after, know_reduction) %>% 
      pivot_wider(id_cols = id, 
                  names_from = after, 
                  values_from = know_reduction,
                  values_fn = list) %>% 
      rename(before = `0`, after = `1`) %>% 
      unnest(c(before, after)) %>% 
      filter(!is.na(before) & !is.na(after)) %>% 
      custom_t_test() %>% 
      mutate(item = 'reduction', service = 'fire'), # Label the item and sample
    
    
    # I am aware of the health effects alcohol has on my body.
    
    ## Total sample
    data_alcohol %>%
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
      mutate(item = 'effects', service = 'total'), # Label the item and sample
    
    ## Police
    data_alcohol %>%
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
      mutate(item = 'effects', service = 'police'), # Label the item and sample
    
    ## Fire
    data_alcohol %>%
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
      mutate(item = 'effects', service = 'fire') # Label the item and sample
    
    
    
  ) %>% 
  select(item, service, everything()) %>% 
  pivot_wider(id_cols = item,
              names_from = service,
              values_from = -c(item, service))

t_tests_alcohol

t_tests_alcohol %>% 
  select(item, 
         d_total, t_total, p_t_total,
         d_fire, t_fire, p_t_fire,
         d_police, t_police, p_t_police) %>% 
  write_csv(here::here(paste0('output/t-test-alcohol-', Sys.Date(),'.csv')))
